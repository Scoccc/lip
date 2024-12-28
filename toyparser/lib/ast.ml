type ast =
    Const of int
  |  Hex of int
  | Neg of ast
  | Add of ast * ast
  | Sub of ast * ast
  | Div of ast * ast
  | Mul of ast * ast
  
