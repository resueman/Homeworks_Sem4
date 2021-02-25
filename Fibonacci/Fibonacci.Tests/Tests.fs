namespace Fibonacci.Tests

module FibonacciTests =
    open Fibonacci
    open NUnit.Framework
    open FsUnit

    [<TestCase(0, 0)>]
    [<TestCase(1, 1)>]
    [<TestCase(2, 1)>]
    [<TestCase(3, 2)>]
    [<TestCase(4, 3)>]
    [<TestCase(5, 5)>]
    [<TestCase(6, 8)>]
    [<TestCase(7, 13)>]
    [<TestCase(8, 21)>]
    [<TestCase(9, 34)>]
    let ``Should return correct value on non-negative integers`` (number:int, expected:int) =
        match number |> bigint |> fibonacci with 
        | Some(value) -> value |> should be (equal (expected |> bigint))
        | None -> failwith $"Expected {expected} but was None"

    [<Test>]
    let ``Should support big integer Fibonacci`` () =
        match fibonacci 200I with 
        | Some(value) -> value |> should be (equal 280571172992510140037611932413038677189525I)
        | None -> failwith $"Expected value but was None"

    [<Test>]
    let ``Shouldn't fail on very big integer`` () =
        fibonacci 300000I |> should not' (be equal 0)

    [<Test>]
    let ``Should return None on negative integers`` ([<Random(-1000, -1, 100)>] number:int) = 
        number |> bigint |> fibonacci |> should be (equal None)