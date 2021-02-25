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
    let ``Should return correct value on non-negative integers`` (number:bigint, expected:bigint) =
        match factorial number with 
        | Some(value) -> value |> should be (equal expected)
        | None -> failwith $"Expected {expected} but was None"

    [<Test>]
    let ``Should support big integer factorial`` () =
        factorial 20I |> should be (equal 2432902008176640000I)

    [<Test>]
    let ``Shouldn't fail on very big integer`` () =
        factorial 30000I |> should not' (be equal 0)

    [<Test>]
    let ``Should return None on negative integers`` ([<Random(-1000, -1, 100)>] number:bigint) = 
        factorial number |> should be (equal None)
