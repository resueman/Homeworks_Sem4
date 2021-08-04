module LambdaInterpreter.Tests

open NUnit.Framework
open FsUnit
open LambdaInterpreter

module Tests =

    let S = Abs("x", Abs("y", Abs("z", App(App(Var "x", Var "z"), App(Var "y", Var "z")))))
    let K = Abs("x", Abs("y", Var "x"))
    let I = Abs("z", Var "z")
    let triplet = Abs("x", App(Var "x", App(Var "x", Var "x")))
    let w = Abs("b", App(Var "b", Var "b"))
    let W = App(w, w)

    let variableSubstitutionsCases =
        [
            TestCaseData(App(I, Var "b")).Returns(Var "b")
            TestCaseData(App(Abs("a", Var "x"), Var "b")).Returns(Var "x")
            TestCaseData(App(Abs("a", Abs("b", Abs("c", Abs("d", Var "a")))), Var "m")).Returns(Abs("b", Abs("c", Abs("d", Var "m"))))
            TestCaseData(App(Abs("a", Abs("b", Abs("c", Abs("d", Var "x")))), Var "m")).Returns(Abs("b", Abs("c", Abs("d", Var "x"))))
        ]

    let normalFormAchievedOnlyByNormalReductionCases =
        [ 
            TestCaseData(App(Abs("x", Var "y"), App(triplet, triplet))).Returns(Var "y")
        ]

    let reductionToNormalFormCases =
        [
            TestCaseData(Abs("z", App(Abs("y", Var "z"), App(Abs("x", Abs("y", Var "x")), Var "z")))).Returns(I) // 4 -> 5
            TestCaseData(Abs("z", App(App(Abs("x", Abs("y", Var "x")), Var "z"), App(Abs("x", Abs("y", Var "x")), Var "z")))).Returns(I) // 3 -> 5
            TestCaseData(App(Abs("y", Abs("z", App(App(Abs("x", Abs("y", Var "x")), Var "z"), App(Var "y", Var "z")))), Abs("x", Abs("y", Var "x")))).Returns(I) // 2 -> 5
            TestCaseData(App(App(S, K), K)).Returns(I) // 1 -> 5
        ]

    let abstractionSubstitutionCases = 
        [
            TestCaseData(App(I, I)).Returns(I)
        ]
    
    let renamingCases = 
        [
            TestCaseData(App(Abs("a", Abs("b", Abs("c", Abs("d", Var "a")))), Var "b")).Returns(Abs("b'", Abs("c", Abs("d", Var "b"))))
            TestCaseData(App(Abs("a", Abs("b", Abs("c", Abs("d", Var "b")))), Var "b")).Returns(Abs("b", Abs("c", Abs("d", Var "b"))))
            TestCaseData(App(Abs("a", Abs("b", Abs("c", Abs("d", Var "a")))), Var "c")).Returns(Abs("b", Abs("c'", Abs("d", Var "c"))))
            TestCaseData(App(App(App(Abs("x", Abs("y", Abs("z", App(App(Var "x", Var "y"), Var "z")))), App(Var "y", Var "z")), Var "x"), Var "z")).Returns(App(App(App(Var "y", Var "z"), Var "x"), Var "z"))
        ]

    let noNormalFormCases = 
        [
            TestCaseData(W).Returns(W)
            TestCaseData(App(App(Abs("a", W), Var "b"), App(Abs("c", App(Var "c", Var "b")), I))).Returns(App(W, Var "b"))
            TestCaseData(App(triplet, triplet)).Returns(App(triplet, triplet))
        ] 

    [<Test>]
    [<TestCaseSource("variableSubstitutionsCases")>]
    let ``Should substitute variable correctly`` expression =
        reduce expression
    
    [<TestCaseSource("reductionToNormalFormCases")>]   
    let ``Should reduce to normal form`` expression =
        reduce expression

    [<TestCaseSource("normalFormAchievedOnlyByNormalReductionCases")>]
    let ``Should reduce using normal reducing strategy`` expression =
        reduce expression
    
    [<TestCaseSource("abstractionSubstitutionCases")>]
    let ``Should substitute abstraction correctly`` expression =
        reduce expression

    [<TestCaseSource("renamingCases")>]
    let ``Should rename when it needed`` expression =
        reduce expression
      
    [<TestCaseSource("noNormalFormCases")>]
    let ``Should return term even if normal form doesn't exist`` expression =
        reduce expression
