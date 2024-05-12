use ekege::{
    database::Database,
    id::Id,
    map::map,
    query::{MapPattern, MapPatternArgument, SimpleQuery},
    term::map_term,
};

fn main() {
    let mut database = Database::new();

    let boolean = database.new_type();

    let x = database.new_constant(boolean);

    let or = database.insert_map(map! { (boolean, boolean) -> boolean });
    let and = database.insert_map(map! { (boolean, boolean) -> boolean });

    database.insert_map_term(map_term! { and(x, or(x, x)) });

    for substitution in database.search(SimpleQuery::new(vec![
        MapPattern::new(
            or,
            vec![
                MapPatternArgument::Variable(Id(1)),
                MapPatternArgument::Variable(Id(1)),
                MapPatternArgument::Variable(Id(0)),
            ],
        ),
        MapPattern::new(
            and,
            vec![
                MapPatternArgument::Variable(Id(3)),
                MapPatternArgument::Variable(Id(0)),
                MapPatternArgument::Variable(Id(2)),
            ],
        ),
    ])) {
        println!("{substitution:?}");
    }
}
