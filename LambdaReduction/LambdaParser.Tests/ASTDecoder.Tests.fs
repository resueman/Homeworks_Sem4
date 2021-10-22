module ASTDecoder.Tests

    open FsUnit
    open NUnit.Framework
    open AST
    open AstDecoder

    let I = Abs("x", Var "x")
    let Ix = I
    let Iy = Abs("y", Var "y") 
    let K = Abs("x", Abs("y", Var "x"))
    let DZ'Z = Abs("d", Abs("z", Var "z"))
    let X = Var "x"
    let XY = App(Var "x", Var "y")
    let S = Abs("x", Abs("y", Abs("z", App(App(Var "x", Var "z"), App(Var "y", Var "z")))))

    [<Test>]
    let ``Should parse variable correctly`` () =
        buildLambdaExpressionFromAST (Var "x") |> should equal "x"
        buildLambdaExpressionFromAST XY |> should equal "(x y)"
        buildLambdaExpressionFromAST (App(App(App(App(App(Var "a", Var "b"), Var "c"), Var "d"), Var "e"), Var "f")) |> should equal "(((((a b) c) d) e) f)"
        buildLambdaExpressionFromAST (App(App(Var "a", App(Var "b", App(Var "c", App(Var "d", Var "e")))), Var "f")) |> should equal "((a (b (c (d e)))) f)"
        buildLambdaExpressionFromAST (App(XY, XY)) |> should equal "((x y) (x y))"
        buildLambdaExpressionFromAST (Abs("x", App(Var "x", Iy))) |> should equal "(\x.(x (\y.y)))"
        buildLambdaExpressionFromAST (App(Ix, Iy)) |> should equal "((\x.x) (\y.y))"
        buildLambdaExpressionFromAST I |> should equal "(\x.x)"
        buildLambdaExpressionFromAST (Abs("x", App(App(Var "x", Var "y"), Var "z"))) |> should equal "(\x.((x y) z))"
        buildLambdaExpressionFromAST (Abs("x", App(Var "x", App(Var "y", Var "z")))) |> should equal "(\x.(x (y z)))"
        buildLambdaExpressionFromAST K |> should equal "(\x.(\y.x))"
        buildLambdaExpressionFromAST (Abs("x", (Abs("y", (Abs("z", Var "x")))))) |> should equal "(\x.(\y.(\z.x)))"
        buildLambdaExpressionFromAST (Abs("x", App(App(Abs("y", Var "z"), Var "k"), Var "d"))) |> should equal "(\x.(((\y.z) k) d))"
        buildLambdaExpressionFromAST (Abs("x", Abs("y", App(Var "x", Var "y")))) |> should equal "(\x.(\y.(x y)))"
        buildLambdaExpressionFromAST (App(K, DZ'Z)) |> should equal "((\x.(\y.x)) (\d.(\z.z)))" 
        buildLambdaExpressionFromAST (App(App(S, K), K)) |> should equal "(((\x.(\y.(\z.((x z) (y z))))) (\x.(\y.x))) (\x.(\y.x)))"
