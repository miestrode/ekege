use std::{collections::BTreeMap, mem};

use cranelift::{
    codegen::{
        self,
        ir::{self, BlockArg},
    },
    module::{DataDescription, FuncId, Linkage, Module},
    prelude::{
        Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder, isa::CallConv,
        settings, types,
    },
};
use cranelift_jit::{JITBuilder, JITModule};

use crate::{
    database::DatabaseId,
    rule::{FlatMapTermPattern, FlatMapTermPatternInput, FlatQuery, QueryVariable},
};

type PlanNodeIndex = usize;

#[derive(Debug)]
pub(crate) enum Expression {
    NaturalJoin {
        lookup_node_index: PlanNodeIndex,
        expand_node_index: PlanNodeIndex,
    },
    FlatMapTermPattern {
        pattern: FlatMapTermPattern,
    },
}

impl Expression {
    pub(crate) fn assert_ids_are_local(&self, plan: &QueryPlan, database_id: DatabaseId) {
        match self {
            Expression::NaturalJoin {
                lookup_node_index: node_index_a,
                expand_node_index: node_index_b,
            } => {
                plan.node(*node_index_a)
                    .assert_ids_are_local(plan, database_id);
                plan.node(*node_index_b)
                    .assert_ids_are_local(plan, database_id);
            }
            Expression::FlatMapTermPattern { pattern } => {
                pattern.assert_ids_are_local(database_id);
            }
        }
    }
}

#[derive(Debug)]
pub(crate) struct PlanNode {
    expression: Expression,
    outputs: BTreeMap<QueryVariable, usize>,
    parent_index: PlanNodeIndex,
}

impl PlanNode {
    pub(crate) fn assert_ids_are_local(&self, plan: &QueryPlan, database_id: DatabaseId) {
        self.expression.assert_ids_are_local(plan, database_id);
    }
}

#[derive(Debug)]
pub(crate) struct QueryPlan {
    nodes: Vec<PlanNode>,
}

impl QueryPlan {
    pub(crate) fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub(crate) fn node(&self, index: PlanNodeIndex) -> &PlanNode {
        &self.nodes[index]
    }

    pub(crate) fn node_mut(&mut self, index: PlanNodeIndex) -> &mut PlanNode {
        &mut self.nodes[index]
    }

    fn add_node(&mut self, expression: Expression) -> PlanNodeIndex {
        let index = self.nodes.len();

        self.nodes.push(PlanNode {
            outputs: match &expression {
                Expression::NaturalJoin {
                    lookup_node_index,
                    expand_node_index,
                } => self
                    .node(*lookup_node_index)
                    .outputs
                    .iter()
                    .chain(self.node(*expand_node_index).outputs.iter())
                    .map(|(variable, index)| (*variable, *index))
                    .collect(),
                Expression::FlatMapTermPattern { pattern } => pattern
                    .inputs
                    .iter()
                    .filter_map(|input| {
                        if let FlatMapTermPatternInput::QueryVariable(variable) = input {
                            Some(*variable)
                        } else {
                            None
                        }
                    })
                    .zip(0..)
                    .collect(),
            },
            expression,
            parent_index: index, // Marker. TODO: Replace this
        });

        index
    }

    fn set_node_parent(&mut self, node_index: PlanNodeIndex, parent_index: PlanNodeIndex) {
        self.node_mut(node_index).parent_index = parent_index;
    }

    pub(crate) fn add_natural_join(
        &mut self,
        node_index_a: PlanNodeIndex,
        node_index_b: PlanNodeIndex,
    ) -> PlanNodeIndex {
        let index = self.add_node(Expression::NaturalJoin {
            lookup_node_index: node_index_a,
            expand_node_index: node_index_b,
        });

        self.set_node_parent(node_index_a, index);
        self.set_node_parent(node_index_b, index);

        index
    }

    pub(crate) fn add_flat_map_term_pattern(
        &mut self,
        flat_map_term_pattern: FlatMapTermPattern,
    ) -> PlanNodeIndex {
        self.add_node(Expression::FlatMapTermPattern {
            pattern: flat_map_term_pattern,
        })
    }

    pub(crate) fn add_flat_query(&mut self, query: FlatQuery) -> Option<PlanNodeIndex> {
        let mut map_term_patterns = query.map_term_patterns.into_iter();

        let mut flat_query_index = self.add_flat_map_term_pattern(map_term_patterns.next()?);

        for pattern in map_term_patterns {
            let pattern_index = self.add_flat_map_term_pattern(pattern);

            flat_query_index = self.add_natural_join(flat_query_index, pattern_index);
        }

        Some(flat_query_index)
    }

    fn last_node_index(&self) -> usize {
        self.nodes.len() - 1
    }

    pub(crate) fn assert_ids_are_local(&self, database_id: DatabaseId) {
        for plan_node in &self.nodes {
            plan_node.assert_ids_are_local(self, database_id);
        }
    }
}

pub(crate) struct Jit {
    builder_context: FunctionBuilderContext,
    codegen_context: codegen::Context,
    data_description: DataDescription,
    module: JITModule,
}

impl Default for Jit {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift::native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift::module::default_libcall_names());

        let module = JITModule::new(builder);

        Self {
            builder_context: FunctionBuilderContext::new(),
            codegen_context: module.make_context(),
            data_description: DataDescription::new(),
            module,
        }
    }
}

impl Jit {
    pub(crate) fn generate_query_plan(mut self, plan: &QueryPlan) -> impl Module {
        self.produce_join_expression(plan.last_node_index(), plan);
        self.module
            .finalize_definitions()
            .expect("all needed functions defined");

        self.module
    }

    fn produce_join_expression(&mut self, node_index: PlanNodeIndex, plan: &QueryPlan) -> FuncId {
        let current_function = self
            .module
            .declare_function(
                &format!("produce_{node_index}"),
                Linkage::Local,
                &ir::Signature::new(CallConv::Fast),
            )
            .unwrap();

        let pointer_type = self.module.target_config().pointer_type();

        let node = plan.node(node_index);

        match &node.expression {
            Expression::NaturalJoin {
                lookup_node_index,
                expand_node_index,
            } => {
                let produce_lookup = self.produce_join_expression(*lookup_node_index, plan);
                let produce_expand = self.produce_join_expression(*expand_node_index, plan);

                let mut function_builder =
                    FunctionBuilder::new(&mut self.codegen_context.func, &mut self.builder_context);
                let entry = function_builder.create_block();
                function_builder.append_block_params_for_function_params(entry);
                function_builder.switch_to_block(entry);
                function_builder.seal_block(entry);

                let produce_lookup = self
                    .module
                    .declare_func_in_func(produce_lookup, function_builder.func);
                function_builder.ins().call(produce_lookup, &[]);

                let produce_expand = self
                    .module
                    .declare_func_in_func(produce_expand, function_builder.func);
                function_builder.ins().call(produce_expand, &[]);

                function_builder.ins().return_(&[]);
                function_builder.finalize();
            }
            Expression::FlatMapTermPattern { pattern } => {
                let consume = self.consume_join_expression(node.parent_index, node_index, plan);

                let mut function_builder =
                    FunctionBuilder::new(&mut self.codegen_context.func, &mut self.builder_context);
                let entry = function_builder.create_block();
                function_builder.append_block_params_for_function_params(entry);
                function_builder.switch_to_block(entry);
                function_builder.seal_block(entry);

                let current_base = function_builder.ins().iconst(pointer_type, 0);
                let current_length = function_builder.ins().iconst(types::I32, 0);

                let loop_condition = function_builder.create_block();
                function_builder.ins().jump(
                    loop_condition,
                    &[
                        BlockArg::Value(current_base),
                        BlockArg::Value(current_length),
                    ],
                );

                let current_base =
                    function_builder.append_block_param(loop_condition, pointer_type);
                let current_length =
                    function_builder.append_block_param(loop_condition, types::I32);
                function_builder.switch_to_block(loop_condition);

                let loop_body = function_builder.create_block();
                let loop_end = function_builder.create_block();
                function_builder.ins().brif(
                    current_length,
                    loop_body,
                    &[
                        BlockArg::Value(current_base),
                        BlockArg::Value(current_length),
                    ],
                    loop_end,
                    &[],
                );

                let current_base = function_builder.append_block_param(loop_body, pointer_type);
                let current_length = function_builder.append_block_param(loop_body, types::I32);
                function_builder.switch_to_block(loop_body);

                // Arguments start after the hash, which is 64 bits
                let offsets = 2..pattern.inputs.len() + 2;
                let arguments = offsets
                    .map(|offset| {
                        function_builder
                            .ins()
                            .iadd_imm(current_base, (offset * mem::size_of::<u32>()) as i64)
                    })
                    .collect::<Vec<_>>();

                let consume = self
                    .module
                    .declare_func_in_func(consume, function_builder.func);
                function_builder.ins().call(consume, &arguments);

                let u32s_in_map_term = 2 + pattern.inputs.len() + 1;
                let new_base = function_builder.ins().iadd_imm(
                    current_base,
                    (u32s_in_map_term * mem::size_of::<u32>()) as i64,
                );
                let new_length = function_builder.ins().iadd_imm(current_length, -1);
                function_builder.ins().jump(
                    loop_condition,
                    &[BlockArg::Value(new_base), BlockArg::Value(new_length)],
                );
                function_builder.seal_block(loop_condition);
                function_builder.seal_block(loop_body);

                function_builder.switch_to_block(loop_end);
                function_builder.seal_block(loop_end);

                function_builder.ins().return_(&[]);
                function_builder.finalize();
            }
        }

        println!("{}", self.codegen_context.func.display());

``        self.module
            .define_function(current_function, &mut self.codegen_context)
            .expect("error defining function");

        self.module.clear_context(&mut self.codegen_context);

        current_function
    }

    fn consume_join_expression(
        &mut self,
        node_index: PlanNodeIndex,
        coming_from: PlanNodeIndex,
        plan: &QueryPlan,
    ) -> FuncId {
        let &PlanNode {
            expression:
                Expression::NaturalJoin {
                    lookup_node_index, ..
                },
            parent_index,
            ..
        } = plan.node(node_index)
        else {
            unreachable!()
        };

        let is_lookup_side = coming_from == lookup_node_index;

        let current_function = if is_lookup_side {
            let current_function = self
                .module
                .declare_function(
                    &format!("consume_lookup_{node_index}"),
                    Linkage::Local,
                    &ir::Signature::new(CallConv::Fast),
                )
                .unwrap();

            let mut function_builder =
                FunctionBuilder::new(&mut self.codegen_context.func, &mut self.builder_context);

            let entry = function_builder.create_block();
            function_builder.append_block_params_for_function_params(entry);
            function_builder.switch_to_block(entry);
            function_builder.seal_block(entry);

            let pointer_type = self.module.target_config().pointer_type();

            function_builder.ins().return_(&[]);
            function_builder.finalize();

            current_function
        } else {
            let current_function = self
                .module
                .declare_function(
                    &format!("consume_expand_{node_index}"),
                    Linkage::Local,
                    &self.codegen_context.func.signature,
                )
                .unwrap();

            let mut function_builder =
                FunctionBuilder::new(&mut self.codegen_context.func, &mut self.builder_context);

            let entry = function_builder.create_block();
            function_builder.append_block_params_for_function_params(entry);
            function_builder.switch_to_block(entry);
            function_builder.seal_block(entry);

            let pointer_type = self.module.target_config().pointer_type();

            function_builder.ins().return_(&[]);
            function_builder.finalize();

            current_function
        };

        println!("{}", self.codegen_context.func.display());

        self.module
            .define_function(current_function, &mut self.codegen_context)
            .expect("error defining function");

        self.module.clear_context(&mut self.codegen_context);

        current_function
    }
}
