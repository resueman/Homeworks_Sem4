namespace MapForTrees.Tests

/// <summary>Contains tests for checking the correctness of 
/// map function for binary trees</summary>
module Tests =
    open NUnit.Framework
    open FsUnit
    open MapForTrees
    open MapForTrees.MapForTrees

    [<Test>]
    let ``Should map correctly`` () =
        let tree = Node(16, Node(90, Node(35, Empty, Empty), Empty), Empty)
        let func = (fun x -> x * 2)
        let expected = Node(32, Node(180, Node(70, Empty, Empty), Empty), Empty)
        map tree func |> should equal expected

    [<Test>]
    let ``Should map empty correctly`` () = 
        map Empty (fun x -> x) |> should equal Empty
