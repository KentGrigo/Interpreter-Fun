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

module Id = struct type t = id let compare x1 x2 = Stdlib.compare x1 x2 end
module IdMap = Map.Make(Id)
type value_storage = int IdMap.t
type function_storage = expression IdMap.t

let rec interpretExpression
  (expression: expression)
  (valueStorage: value_storage)
  (functionStorage: function_storage)
: int =
  match expression with
  | Id id -> IdMap.find id valueStorage
  | Int value -> value
  | Add (expression1, expression2) ->
    let value1 = interpretExpression expression1 valueStorage functionStorage
    and value2 = interpretExpression expression2 valueStorage functionStorage
    in value1 + value2
  | FuncApp id ->
    let expression' = IdMap.find id functionStorage
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
    in (value, IdMap.add id value valueStorage, functionStorage)
  | FuncDecl (id, expression) -> (0, valueStorage, IdMap.add id expression functionStorage)

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
    let (value, _, _) = interpretStatements statements IdMap.empty IdMap.empty
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
