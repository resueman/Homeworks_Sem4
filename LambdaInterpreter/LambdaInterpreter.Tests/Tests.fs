module LambdaInterpreter.Tests

open NUnit.Framework
open FsUnit
open LambdaInterpreter

module Tests =

    let S = Abs("x", Abs("y", Abs("z", App(App(Var "x", Var "z"), App(Var "y", Var "z")))))
    let K = Abs("x", Abs("y", Var "x"))
    let I = Abs("z", Var "z")

    let substitutions =
        [
            TestCaseData(App(Abs("a", Var "a"), Var "b")).Returns(Var "b")
            TestCaseData(App(Abs("a", Var "x"), Var "b")).Returns(Var "x")
            TestCaseData(App(Abs("a", Abs("b", Abs("c", Abs("d", Var "a")))), Var "b")).Returns(Abs("b", Abs("c", Abs("d", Var "b"))))
            TestCaseData(App(Abs("a", Abs("b", Abs("c", Abs("d", Var "a")))), Var "m")).Returns(Abs("b", Abs("c", Abs("d", Var "m"))))
            TestCaseData(App(Abs("a", Abs("b", Abs("c", Abs("d", Var "b")))), Var "b")).Returns(Abs("b", Abs("c", Abs("d", Var "b"))))
            TestCaseData(App(Abs("a", Abs("b", Abs("c", Abs("d", Var "c")))), Var "b")).Returns(Abs("b", Abs("c", Abs("d", Var "c"))))
            TestCaseData(App(Abs("a", Abs("b", Abs("c", Abs("d", Var "x")))), Var "m")).Returns(Abs("b", Abs("c", Abs("d", Var "x"))))
            
            TestCaseData(Abs("z", App(Abs("y", Var "z"), App(Abs("x", Abs("y", Var "x")), Var "z")))).Returns(I) // 4 -> 5
            TestCaseData(Abs("z", App(App(Abs("x", Abs("y", Var "x")), Var "z"), App(Abs("x", Abs("y", Var "x")), Var "z")))).Returns(I) // 3 -> 5
            TestCaseData(App(Abs("y", Abs("z", App(App(Abs("x", Abs("y", Var "x")), Var "z"), App(Var "y", Var "z")))), Abs("x", Abs("y", Var "x")))).Returns(I) // 2 -> 5
            TestCaseData(App(App(S, K), K)).Returns(I) // 1 -> 5
        ]

    [<Test>]
    [<TestCaseSource("substitutions")>]
    let ``Substitution should work correctly`` expression =
        reduce expression
