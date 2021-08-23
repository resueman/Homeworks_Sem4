/// Contains methods for testing the correctness of workflow,
/// which performs calculations with numbers, given as strings
module WorkflowWithStrings.Tests

open NUnit.Framework
open FsUnit
open WorkflowWithStrings

[<TestCase(5, 8)>]
[<TestCase(0, -5)>]
[<TestCase(-5, -98)>]
[<TestCase(556, 768)>]
[<TestCase(1, -8)>]
let ``Should return value in case of numbers given as strings`` (x: int, y: int) =
    let calculate = CalculateBuilder()
    let result = calculate {
            let! x = x.ToString()
            let! y = y.ToString()
            let z = x + y
            return z
        }
    result |> should equal (x + y |> Some)

[<TestCase("tyvf", "hjvk")>]
[<TestCase("y", "m")>]
[<TestCase("1", "x")>]
[<TestCase("x", "1")>]
let ``Should return None in case of incorrect input`` (x: string, y: string) =
    let calculate = CalculateBuilder()
    let result = calculate {
        let! x = x
        let! y = y
        let z = x + y
        return z
        }
    result |> should equal None
