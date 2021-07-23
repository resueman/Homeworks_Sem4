/// Contains tests that check if rounding workflow,
/// performing mathematical calculations with a specified precision,
/// works correctly
module RoundingWorkflow.Tests

open NUnit.Framework
open FsUnit
open RoundingWorkflow

[<Test>]
let ``Should return value with expected precision`` () =
    let rounding digits = RoundingBuilder digits
    let value = rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    value |> should (equalWithin 0.001) (0.048 |> Some)

[<Test>]
let ``Should round with expected precision on each step`` () =
    let rounding digits = RoundingBuilder digits
    let value = rounding 6 {
        let! a = 2.0 / 3.0
        let! b = 1.3333336
        return a * b
    }
    value |> should (equalWithin 0.000001) (0.888890 |> Some)
