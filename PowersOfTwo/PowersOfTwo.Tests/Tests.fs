namespace PowersOfTwo.Tests

module PowersOfTwoTests =
    open PowersOfTwo
    open NUnit.Framework
    open FsUnit

    [<Test>]
    let ``Should return list with 1 when m and n are zeroes`` () =
        match generateListOfPowers 0 0 with
        | Some(value) -> value |> should equal [1]
        | None -> failwith "Expected [1] but was None"

    [<Test>]
    let ``Should works correctly when n less than m`` () =
        let n = 2
        let m = 3
        match generateListOfPowers n m with
        | Some(value) -> value |> should equal [4; 8; 16; 32]
        | None -> failwith "Expected [4; 8; 16; 32] but was None"

    [<Test>]
    let ``Should works correctly when m less than n`` () =
        let n  = 4
        let m = 2
        match generateListOfPowers n m with
        | Some(value) -> value |> should equal [16; 32; 64]
        | None -> failwith "Expected [16; 32; 64] but was None"

    [<Test>]
    let ``Should return None when n is negative`` () =
        generateListOfPowers -6 8 |> should equal None

    [<Test>]
    let ``Should return None when m is negative`` () =
        generateListOfPowers 6 -1 |> should equal None

    [<Test>]
    let ``Should works correctly when n equal to zero`` () =
        match generateListOfPowers 0 3 with
        | Some(value) -> value |> should equal [1; 2; 4; 8]
        | None -> failwith "Expected [1; 2; 4; 8] but was None"

    [<Test>]
    let ``Should works correctly when m equal to zero`` () =
        match generateListOfPowers 4 0 with
        | Some(value) -> value |> should equal [16]
        | None -> failwith "Expected [16] but was None"
