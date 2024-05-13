let test01() =
  let program = Program [
    Expr (Int 5);
  ]
  in interpretProgram program = 5

let test02() =
  let program = Program [
    Expr (Add (Int 5, Int 4));
  ]
  in interpretProgram program = 9

let test03() =
  let program = Program [
    Assign ("x", Int 2);
    Assign ("y", Int 3);
    Expr (Add (Id "x", Id "y"));
  ]
  in interpretProgram program = 5

let test04() =
  let program = Program [
    FuncDecl ("f", Int 6);
    Expr (FuncApp "f");
  ]
  in interpretProgram program = 6
