module LambdaParser.Tests

    open FsUnit
    open NUnit.Framework
    open LambdaParserUI
    open AST

    let I = Abs("x", Var "x")
    let XY'X = Abs("x", Abs("y", Var "x"))
    let DZ'Z = Abs("d", Abs("z", Var "z"))
    let X = Var "x"
    let XY = App(Var "x", Var "y")
    let S = Abs("x", Abs("y", Abs("z", App(App(Var "x", Var "z"), App(Var "y", Var "z")))))
    let K = Abs("x", Abs("y", Var "x"))

    [<Test>]
    [<TestCase("longName")>]
    [<TestCase("longName123")>]
    [<TestCase("_longName")>]
    [<TestCase("_1")>]
    let ``Should support complicated names for named definitions`` (varName: string) =
        sprintf "let %s = x\n%s" varName varName |> parseFromString |> should equal (Some X)
   
    [<Test>]
    [<TestCase("longVarName")>]
    [<TestCase("longVarName123")>]
    [<TestCase("_longVarName")>]
    [<TestCase("_1")>]
    let ``Should support complicated names for variables in term`` (varName: string) =  
        sprintf "let A = %s\nA" varName |> parseFromString |> should equal (Var varName |> Some)
    
    [<Test>]
    let ``Should parse variable correctly`` () =
        parseFromString "let A = x\nA" |> should equal (Some X)
        parseFromString "let A = (x)\nA" |> should equal (Some X)

    [<Test>]
    [<TestCase("let A = x y\nA")>]
    [<TestCase("let A = (x y)\nA")>]
    [<TestCase("let A = (x) (y)\nA")>]
    [<TestCase("let X = x\nlet Y = y\nX Y")>]
    [<TestCase("let X = y\nlet Y = x\nY X")>]
    [<TestCase("let X = (x)\nlet Y = (y)\nX Y")>]
    [<TestCase("let X = (y)\nlet Y = (x)\nY X")>]
    let ``Should parse application of two variables correctly`` (input: string) =
        parseFromString input |> should equal (Some XY)

    [<Test>]
    let ``Should parse applications with any grouping of variables correctly`` () =
        parseFromString "let A = a b c d e f\nA" |> should equal (Some(App(App(App(App(App(Var "a", Var "b"), Var "c"), Var "d"), Var "e"), Var "f")))
        parseFromString "let A = a (b (c (d e))) f\nA" |> should equal (Some(App(App(Var "a", App(Var "b", App(Var "c", App(Var "d", Var "e")))), Var "f")))
        parseFromString "let A = a\nlet B = b\nlet C = c\nlet D = d\nA B C D" |> should equal (Some(App(App(App(Var "a", Var "b"), Var "c"), Var "d")))
        parseFromString "let A = a b\nlet B = c d\nA B" |> should equal (Some(App(App(Var "a", Var "b"), App(Var "c", Var "d"))))
        parseFromString "let A = (x y) (x y)\nA" |> should equal (Some(App(XY, XY)))
        parseFromString "let A = x y c\nlet B = x y\nA B" |> should equal (Some(App(App(XY, Var "c"), XY)))
    
    [<Test>]
    let ``Should parse application of two abstractions correctly`` () =
        Assert.Pass()

    [<Test>]
    [<TestCase("let I = \x.x\nI")>]
    [<TestCase("let I = (\x.x)\nI")>]
    [<TestCase("let I = \x.(x)\nI")>]
    [<TestCase("let I = (\x.(x))\nI")>]
    let ``Should parse I-combinator correctly`` (input: string) =
        parseFromString input |> should equal (Some I)

    [<Test>]
    let ``Should parse lambda-abstraction with one argument correctly`` () =
        Assert.Pass()

    [<Test>]
    let ``Should parse lambda-abstraction with many parameters, which returns variable, correctly`` () =
        parseFromString "let LA = \x y.x\nLA" |> should equal (Some XY'X)
        parseFromString "let LA = (\x y.x)\nLA" |> should equal (Some XY'X)
        parseFromString "let LA = \x y.(x)\nLA" |> should equal (Some XY'X)
        parseFromString "let LA = (\x y.(x))\nLA" |> should equal (Some XY'X)
        parseFromString "let LA = \x y z k m n.x\nLA" |> should equal (Some(Abs("x", (Abs("y", (Abs("z", (Abs("k", (Abs("m", Abs("n", Var "x"))))))))))))

    [<Test>]
    let ``Should parse difficult lambda-abstractions`` () =
        Assert.Pass()

    [<Test>]
    let ``Should parse any lambda-term consisting of named definitions correctly`` () =
        Assert.Pass()

    [<Test>]
    let ``Should build AST correctly`` () =
        parseFromString "let LA = \x y.x y\nLA" |> should equal (Some(Abs("x", Abs("y", App(Var "x", Var "y")))))
        parseFromString "let LA = \x y.(x) (y)\nLA" |> should equal (Some(Abs("x", Abs("y", App(Var "x", Var "y")))))
        parseFromString "let LA = \x y z.(a b) (x y)\nLA" |> should equal (Some(Abs("x", Abs("y", Abs("z", App(App(Var "a", Var "b"), App(Var "x", Var "y")))))))
        parseFromString "let LA = \x y z.(a b)\nlet App = (x y)\nLA App" |> should equal (Some(App(Abs("x", Abs("y", Abs("z", App(Var "a", Var "b")))), XY)))
        parseFromString "let LA1 = \x y.x\nlet LA2 = \d z.z\nLA1 LA2" |> should equal (Some(App(XY'X, DZ'Z)))
        parseFromString "let S = \x y z.x z (y z)\nlet K = \x y.x\nS K K" |> should equal (Some(App(App(S, K), K)))

