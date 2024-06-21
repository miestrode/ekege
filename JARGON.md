# Jargon

To express somewhat complicated ideas, some specialized terminology is used. As standard terminology doesn't yet exist, below is a summary of the jargon used in library code:

## Database

Canonical term ID
: Used for classifying equivalence of map members, in the form of map terms

Type
: Barrier to enforce certain requirements on map member arguments

Maps
: Uninterpreted function with a signature (e.g. `or` has signature `(bool, bool) -> bool`. The type of flat terms such as `or(x, y)` would be `bool`)

Flat term
: An uninterpreted function application with canonical term ID inputs (e.g. `or(x, y)`)

Tree term
: An uninterpreted function application with inputs being other tree terms, or canonical term IDs (e.g. `or(x, and(y, z))`)

Map member
: Tuple of canonical term ID inputs for which a map is defined (e.g. if flat term `xor(x, y)` exists in the database, `(x, y)` is a member of map `xor`)

Flat map term
: Tuple of canonical term ID inputs defining a map member, and an output canonical term ID such that the relevant flat term is of the output canonical term ID (e.g. if `eq(x, y)` has canonical term ID `z`, we say `(x, y, z)` is a map term in `eq`)

Tree map term
: Tuple of tree map terms and canonical term ID inputs defining a map member, and an output canonical term ID such that the relevant flat term is of the output canonical term ID (e.g. if `eq(x, y)` has canonical term ID `z`, we say `(x, y, z)` is a map term in `eq`)

Reversed flat map term
: Like a map term, but where the output canonical term ID is first (e.g. if `eq(x, y)` has canonical term ID `z`, we say `(z, x, y)` is a reversed map term in `eq`)

Database
: Stores all currently known map terms, and a union-find over term IDs, producing canonical term IDs

## Rules

Query variable
: Variable to be substituted in a query or a rule. The query restricts substitutions and the rule payloads are ran for each substitution

Previously created flat term index
: An index into the flat terms created by flat rule payloads. Index 0 is the first flat term, 1 is the second, etc. Used to refer to the flat terms in subsequent flat rule payloads

Term creation flat rule payload
: A flat rule payload consisting of a flat term with some of the canonical term IDs substituted for query variables, or previously created flat term indices. The payload will create the flat term if it didn't exist

Union flat rule payload
: A flat rule payload consisting of two items, each being a canonical term ID, query variable, or previously created flat term index. The payload will unify the two items after resolving their canonical term ID

Flat map term patterns
: Like a flat map term, but where some canonical term IDs are replaced with query variables

Flat rule payload
: Either a term creation flat rule payload or a union flat rule payload

Flat query
: A tuple of flat map term patterns

Flat rule
: A flat query and a tuple of flat rule payloads

Tree map term patterns
: Like a tree map term, but where some canonical term IDs are replaced with query variables

Term creation tree rule payload
: A tree rule payload consisting of a tree map term pattern. The payload will create parts of the tree term if they didn't exist

Union tree rule payload
: A tree rule payload consisting of two items, each being a canonical term ID, query variable, or tree map term pattern. The payload will unify the two items after resolving their canonical term ID

Tree rule payload
: Either a term creation tree rule payload or a union tree rule payload

Tree query
: A tuple of tree map term patterns

Tree rule
: A tree query and a tuple of tree rule payloads
