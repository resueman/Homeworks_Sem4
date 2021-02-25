namespace FindElement.Tests

module FindElementTests =
    open FindElement
    open NUnit.Framework
    open FsUnit
    
    [<TestCase(7, 0)>]
    [<TestCase(5, 1)>]
    [<TestCase(76, 2)>]
    [<TestCase(9, 3)>]
    [<TestCase(98, 4)>]
    let ``Should return correct zero-based position in list with length more than 3`` (element:int, expected:int) =
        let list = [7; 5; 76; 9; 98]
        let position = find list element
        match position with
        | Some(value) -> value |> should be (equal expected)
        | None -> failwith $"Expected {position} but was {position}"

    [<Test>]
    let ``Should return first occurence of element`` () =
        let list = [5; 5; 5; 5]
        let expected = 0
        let position = find list 5
        match position with
        | Some(value) -> value |> should be (equal expected)
        | None -> failwith $"Expected {expected} but was {position}"

    [<Test>]
    let ``Should works correct for list of one element`` ([<Random(-788, 100, 1000)>] missing:int) =
        let list = [-789]
        match find list -789 with
        | Some(value) -> value |> should be (equal 0)
        | None -> failwith $"Expected 0 but was None"

        find list missing |> should be (equal None)
       
    [<Test>]
    let ``Should return None for missing elements`` ([<Random(11, 1000, 100)>] missing:int) =
        let list = [7; 5; 9; 10]
        find list missing |> should be (equal None)
       
    [<Test>]
    let ``Shouldn't fail on empty list`` ([<Random(-1000, 1000, 100)>] missing:int) =
        let list = []
        find list missing |> should be (equal None)
