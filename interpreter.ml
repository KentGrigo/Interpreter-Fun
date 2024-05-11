/*
expr :=
| ID
| INT
| ID = expr
| expr + expr
*/
 
alias ValueStorage = Map<(ID, Int)>
alias FunctionStorage = Map<(ID, Expr)>
 
data Expr =
| ID of string
| Int of int
| Assign of ID * Expr
| Add of Expr * Expr
| Statement of [Expr]
| FuncDecl of ID * Expr
| FuncApp of ID
 
Statement [
  (Assign (ID "x") (Int 2))
  (Assign (ID "y") (Int 3))
  (Add (ID "x") (ID "y"))
]
 
fun interpret (expr: Expr): Int =
    let (val, _, _) = interpretExpr(expr, [], [])
    val
 
fun interpretStatement(
    exprs: [Expr],
    valueStorage: ValueStorage,
    functionStorage: FunctionStorage,
): (Int, ValueStorage, FunctionStorage) =
    match exprs with
    | [] => throw
    | expr'::[] => interpretExpr(expr', storage)
    | expr'::expr's =>
      let (val, storage') = interpretExpr(expr', storage)
      interpretStatement(expr's, storage')
 
fun interpretExpr(
    expr: Expr,
    valueStorage: ValueStorage,
    functionStorage: FunctionStorage,
): (Int, ValueStorage, FunctionStorage) =
  match expr with
  | ID id => (storage[id], valueStorage, functionStorage) // TODO: might not exist
  | Int val => (val, valueStorage, functionStorage)
  | Assign (id, expr') =>
    let (val, valueStorage') = interpretExpr(expr, valueStorage, functionStorage)
    valueStorage'[id] = val
    (val, valueStorage', functionStorage)
  | Add (expr'1, expr'2) =>
    let (val1, valueStorage', functionStorage') = interpretExpr(expr'1, valueStorage, functionStorage)
    let (val2, valueStorage'', functionStorage'') = interpretExpr(expr'2, valueStorage', functionStorage')
    let val' = val1 + val2
    (val', valueStorage'', functionStorage'')
  | Statement expr's =>
    interpretStatement(expr's, storage)
  | FuncDecl (id, expr') =>
    functionStorage[id] = expr'
    (0, valueStorage, functionStorage)
  | FuncApp id =>
    let expr' = functionStorage[id]
    interpretExpr(expr', valueStorage, functionStorage)
 