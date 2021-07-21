module RoundingWorkflow.Tests

open NUnit.Framework
open FsUnit
open RoundingWorkflow

[<Test>]
let ``Should return expected value`` () =
    let rounding digits = RoundingBuilder digits
    let value = rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }

    value |> should equal (0.048 |> Some)
