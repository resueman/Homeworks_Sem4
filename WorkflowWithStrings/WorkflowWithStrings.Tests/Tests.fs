module WorkflowWithStrings.Tests

open NUnit.Framework
open FsUnit
open WorkflowWithStrings

[<Test>]
let ``Should return value in case of correct input`` () =
    let calculate = CalculateBuilder()
    let result = calculate {
            let! x = "1"
            let! y = "2"
            let z = x + y
            return z
        }
    result |> should equal (Some 3)

[<Test>]
let ``Should return None in case of incorrect input`` () =
    let calculate = CalculateBuilder()
    let result = calculate {
        let! x = "2"
        let! y = "m"
        let z = x + y
        return z
        }
    result |> should equal None

