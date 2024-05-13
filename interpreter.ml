type id = string

type expression =
| Id of id
| Int of int
| Add of expression * expression
| FuncApp of id

type statement =
| Expr of expression
| Assign of id * expression
| FuncDecl of id * expression

type program =
| Program of statement list

(* TODO: Should be hashmap instead of list *)
type value_storage = (id * int) list
type function_storage = (id * expression) list

let rec lookup (id: id) (storage: (id * 'a) list): 'a =
  match storage with
  | [] -> raise Not_found
  | (id', element)::storage' ->
    if id = id'
    then element
    else lookup id storage'

let rec interpretExpression
  (expression: expression)
  (valueStorage: value_storage)
  (functionStorage: function_storage)
: int =
  match expression with
  | Id id -> lookup id valueStorage
  | Int value -> value
  | Add (expression1, expression2) ->
    let value1 = interpretExpression expression1 valueStorage functionStorage
    and value2 = interpretExpression expression2 valueStorage functionStorage
    in value1 + value2
  | FuncApp id ->
    let expression' = lookup id functionStorage
    in interpretExpression expression' valueStorage functionStorage

let interpretStatement
  (statement: statement)
  (valueStorage: value_storage)
  (functionStorage: function_storage)
: (int * value_storage * function_storage) =
  match statement with
  | Expr expression ->
    let value = interpretExpression expression valueStorage functionStorage
    in (value, valueStorage, functionStorage)
  | Assign (id, expression) ->
    let value = interpretExpression expression valueStorage functionStorage
    in (value, (id, value)::valueStorage, functionStorage)
  | FuncDecl (id, expression) -> (0, valueStorage, (id, expression)::functionStorage)

let rec interpretStatements
  (statements: statement list)
  (valueStorage: value_storage)
  (functionStorage: function_storage)
: (int * value_storage * function_storage) =
  match statements with
  | [] -> (0, valueStorage, functionStorage) (* TODO: Reconsider how to handle this case *)
  | statement'::[] -> interpretStatement statement' valueStorage functionStorage
  | statement'::statements' ->
    let (_, valueStorage', functionStorage') = interpretStatement statement' valueStorage functionStorage
    in interpretStatements statements' valueStorage' functionStorage'

let interpretProgram(program: program): int =
  match program with
  | Program statements ->
    let (value, _, _) = interpretStatements statements [] []
    in value

let program = Program [
  Expr (Int 5);
  Expr (Add (Int 5, Int 4));
  Assign ("x", Int 2);
  Assign ("y", Int 3);
  Expr (Add (Id "x", Id "y"));
  FuncDecl ("f", Int 6);
  Expr (Add (FuncApp "f", (Int 5)));
]

let result = interpretProgram program
let () =
  print_int(result);
  print_newline();
