namespace ArithmeticParseTreeTests

/// <summary>Contains tests for checking the correctness of 
/// processing an arithmetic parse tree</summary>
module Tests =
    open System
    open FsUnit
    open NUnit.Framework
    open ArithmeticParseTree
    open ArithmeticParseTree.ParseTree

    [<Test>]
    let ``Should divide correctly`` () =
        Division (Number 15, Number 5) |> evaluate |> should equal 3

    [<Test>]
    let ``Should throw exception on division by zero`` () =
        (fun () -> evaluate >> ignore <| Division (Number 15, Number 0)) |> should throw typeof<DivideByZeroException>

    [<Test>]
    let ``Should multiply correctly`` () =
        Multiplication (Number -1, Number 13) |> evaluate |> should equal -13

    [<Test>]
    let ``Should add correctly`` () =
        Addition (Number 4, Number 8) |> evaluate |> should equal 12

    [<Test>]
    let ``Should subtract correctly`` () =
        Subtraction (Number 15, Number 7) |> evaluate |> should equal 8 

    [<TestCaseSource("complexExpressions")>]
    let ``Should evaluate the value of complex expressions`` expression =
        evaluate expression

    let complexExpressions = 
        [
            TestCaseData(Subtraction(Addition(Number 5, Number 7), Division(Number 6, Number 3))).Returns(10)
            TestCaseData(Subtraction(Subtraction(Subtraction(Subtraction(Number 10, Number 2), Number 3), Number 4), Number 0)).Returns(1)
            TestCaseData(Subtraction(Number -1, Number -2)).Returns(1)
            TestCaseData(Subtraction(Number 0, Subtraction(Number 4, Subtraction(Number 3, Subtraction(Number 2, Number 10))))).Returns(7)
            TestCaseData(Subtraction(Subtraction(Number 5, Subtraction(Number 10, Number 5)), Subtraction(Subtraction(Number 3, Number 2), Number 10))).Returns(9)
            TestCaseData(Subtraction(Subtraction(Number 4, Number 3), Number 1)).Returns(0)
            TestCaseData(Subtraction(Subtraction(Number 4, Subtraction(Number -1, Number -1)), Number 5)).Returns(-1)
            TestCaseData(Subtraction(Subtraction(Number 5, Number 15), Subtraction(Number 16, Number 5))).Returns(-21)            
            TestCaseData(Division(Number 121, Subtraction(Number -4, Number 7))).Returns(-11)
            TestCaseData(Addition(Addition(Number 4, Multiplication(Number -1, Number -11)), Number 5)).Returns(20)
            TestCaseData(Division(Subtraction(Number 5, Number 15), Addition(Number -1, Number 6))).Returns(-2)
            TestCaseData(Subtraction(Division(Addition(Subtraction(Number 10, Number 2), Number 3), Number 11), Number 9)).Returns(-8)
            TestCaseData(Multiplication(Number -1, Number -2)).Returns(2)
            TestCaseData(Subtraction(Number 0, Multiplication(Number 4, Subtraction(Number 13, Division(Number 12, Number 3))))).Returns(-36)
            TestCaseData(Multiplication(Division(Number 15, Subtraction(Number 10, Number 5)), Division(Addition(Number 13, Number 2), Number -3))).Returns(-15)
        ]

