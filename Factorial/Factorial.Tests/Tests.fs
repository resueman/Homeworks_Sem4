namespace Factorial.Tests

module FactorialTests =
    open Factorial
    open NUnit.Framework
    open FsUnit

    [<TestCase(0, 1)>]
    [<TestCase(1, 1)>]
    [<TestCase(2, 2)>]
    [<TestCase(3, 6)>]
    [<TestCase(4, 24)>]
    [<TestCase(5, 120)>]
    [<TestCase(6, 720)>]
    [<TestCase(7, 5040)>]
    let ``Should return correct value on non-negative integers`` (number:int, expected:int) =
        match number |> bigint |> factorial with 
        | Some(value) -> value |> should be (equal (expected |> bigint))
        | None -> failwith $"Expected {expected} but was None"

    [<Test>]
    let ``Should support big integer factorial`` () =
        match factorial 20I with 
        | Some(value) -> value |> should be (equal 2432902008176640000I)
        | None -> failwith $"Expected value but was None"

    [<Test>]
    let ``Shouldn't fail on very big integer`` () =
        match factorial 300000I with 
        | Some(value) -> value |> should not' (be equal 0)
        | None -> failwith $"Expected value but was None"

    [<Test>]
    let ``Should return None on negative integers`` ([<Random(-1000, -1, 100)>] number:int) = 
        number |> bigint |> factorial |> should be (equal None)
