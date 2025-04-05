use cranelift::{
    codegen,
    module::{DataDescription, FuncId, Linkage, Module},
    prelude::{
        Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder, settings, types,
    },
};
use cranelift_jit::{JITBuilder, JITModule};

use crate::{
    database::DatabaseId,
    rule::{FlatMapTermPattern, FlatQuery},
};

type PlanNodeIndex = usize;

#[derive(Debug)]
pub(crate) enum JoinExpression {
    NaturalJoin {
        node_index_a: PlanNodeIndex,
        node_index_b: PlanNodeIndex,
    },
    FlatMapTermPattern(FlatMapTermPattern),
}

impl JoinExpression {
    pub(crate) fn assert_ids_are_local(&self, plan: &QueryPlan, database_id: DatabaseId) {
        match self {
            JoinExpression::NaturalJoin {
                node_index_a,
                node_index_b,
            } => {
                plan.node(*node_index_a)
                    .assert_ids_are_local(plan, database_id);
                plan.node(*node_index_b)
                    .assert_ids_are_local(plan, database_id);
            }
            JoinExpression::FlatMapTermPattern(pattern) => {
                pattern.assert_ids_are_local(database_id);
            }
        }
    }
}

#[derive(Debug)]
struct PlanNode {
    expression: JoinExpression,
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

    fn add_node(&mut self, expression: JoinExpression) -> PlanNodeIndex {
        let index = self.nodes.len();

        self.nodes.push(PlanNode {
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
        let index = self.add_node(JoinExpression::NaturalJoin {
            node_index_a,
            node_index_b,
        });

        self.set_node_parent(node_index_a, index);
        self.set_node_parent(node_index_b, index);

        index
    }

    pub(crate) fn add_flat_map_term_pattern(
        &mut self,
        flat_map_term_pattern: FlatMapTermPattern,
    ) -> PlanNodeIndex {
        self.add_node(JoinExpression::FlatMapTermPattern(flat_map_term_pattern))
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

    pub(crate) fn assert_ids_are_local(&self, database_id: DatabaseId) {
        for plan_node in &self.nodes {
            plan_node.assert_ids_are_local(self, database_id);
        }
    }
}

// pub struct Jit {
//     builder_context: FunctionBuilderContext,
//     codegen_context: codegen::Context,
//     data_description: DataDescription,
//     module: JITModule,
// }

// impl Default for Jit {
//     fn default() -> Self {
//         let mut flag_builder = settings::builder();
//         flag_builder.set("use_colocated_libcalls", "false").unwrap();
//         flag_builder.set("is_pic", "false").unwrap();
//         let isa_builder = cranelift::native::builder().unwrap_or_else(|msg| {
//             panic!("host machine is not supported: {}", msg);
//         });
//         let isa = isa_builder
//             .finish(settings::Flags::new(flag_builder))
//             .unwrap();
//         let mut builder = JITBuilder::with_isa(isa, cranelift::module::default_libcall_names());

//         builder.symbol("get_map_iterator_start", todo!());

//         let module = JITModule::new(builder);

//         Self {
//             builder_context: FunctionBuilderContext::new(),
//             codegen_context: module.make_context(),
//             data_description: DataDescription::new(),
//             module,
//         }
//     }
// }

// impl Jit {
//     fn generate_query_plan(&mut self, plan: &QueryPlan) {
//         self.produce_join_expression(0, plan);
//     }

//     fn produce_join_expression(&mut self, node_index: PlanNodeIndex, plan: &QueryPlan) -> FuncId {
//         let function_id = self
//             .module
//             .declare_function(
//                 &format!("produce_{node_index}"),
//                 Linkage::Local,
//                 &self.codegen_context.func.signature,
//             )
//             .unwrap();

//         let mut function_builder =
//             FunctionBuilder::new(&mut self.codegen_context.func, &mut self.builder_context);

//         let entry = function_builder.create_block();
//         function_builder.append_block_params_for_function_params(entry);
//         function_builder.switch_to_block(entry);
//         function_builder.seal_block(entry);

//         let pointer_type = self.module.target_config().pointer_type();

//         let node = plan.node(node_index).expect("valid node index");

//         match &node.expression {
//             JoinExpression::NaturalJoin {
//                 node_index_a: expression_a,
//                 node_index_b: expression_b,
//             } => {
//                 self.produce_join_expression(*expression_a, plan);
//                 self.produce_join_expression(*expression_b, plan);
//             }
//             JoinExpression::FlatMapTermPattern(flat_map_term_pattern) => {
//                 // let map_id = function_builder
//                 //     .ins()
//                 //     .iconst(types::I32, flat_map_term_pattern.map_id.inner() as i64);
//                 // let call = function_builder
//                 //     .ins()
//                 //     .call(get_map_iterator_start, &[map_id]);
//                 let start_pointer = function_builder.ins().iconst(pointer_type, 0);
//                 let length = function_builder.ins().iconst(types::I32, 120);

//                 let loop_start = function_builder.create_block();
//                 function_builder.append_block_param(loop_start, pointer_type);
//                 function_builder.append_block_param(loop_start, types::I32);

//                 function_builder
//                     .ins()
//                     .jump(loop_start, &[start_pointer, length]);
//                 function_builder.seal_block(function_builder.current_block().unwrap());
//                 function_builder.switch_to_block(loop_start);

//                 function_builder.ins().call();

//                 self.consume_join_expression(node.parent_index, plan, function_builder);
//             }
//         }

//         function_id
//     }

//     fn consume_join_expression(
//         &mut self,
//         node_index: PlanNodeIndex,
//         plan: &QueryPlan,
//         function_builder: &mut FunctionBuilder,
//     ) {
//         let node = plan.node(node_index).expect("valid node index");

//         match &node.expression {
//             JoinExpression::NaturalJoin {
//                 node_index_a,
//                 node_index_b,
//             } => {
//                 self.consume_join_expression(node.parent_index, plan, function_builder);
//             }
//             JoinExpression::FlatMapTermPattern(_) => unreachable!(),
//         }
//     }
// }
