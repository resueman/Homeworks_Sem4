/// Contains tests for binary tree
module BinaryTree.Tests

    open NUnit.Framework
    open FsUnit
    open BinaryTree
    open System.Collections
    open System.Collections.Generic
    open System

    [<Test>]
    let ``Should add single value`` () =
        let tree = BinaryTree()
        tree.Add 7
        tree.Contains 7 |> should equal true

    [<Test>]
    let ``Should add several values`` () =
        let tree = BinaryTree()
        [11 .. 20] |> List.map (fun v -> tree.Add v) |> ignore
        [1 .. 10] |> List.iter (fun v -> (tree.Contains (v + 10) |> should equal true))

    [<Test>]
    let ``Should not add value if it is already exists in case of single value in a tree`` () =
        let tree = BinaryTree()
        tree.Add 7
        tree.Contains 7 |> should equal true
        tree.Add 7 

        let mutable enumerator = (tree :> IEnumerable).GetEnumerator()
        enumerator.MoveNext() |> ignore
        enumerator.Current |> should equal 7

        enumerator.MoveNext() |> ignore
        (fun () -> enumerator.Current |> ignore) |> should 
            (throwWithMessage "Enumeration has either not started or has already finished.") 
            typeof<InvalidOperationException>

    [<Test>]
    let ``Should not add value if it is already exists in case of several values in a tree`` () =
        let tree = BinaryTree()
        [1 .. 10] |> List.map (fun v -> tree.Add v) |> ignore
        [1 .. 10] |> List.map (fun v -> tree.Add v) |> ignore

        let mutable enumerator = (tree :> IEnumerable).GetEnumerator()
        for i in 1 .. 10 do
            enumerator.MoveNext() |> ignore
            enumerator.Current |> should equal i
        
        enumerator.MoveNext() |> ignore
        (fun () -> enumerator.Current |> ignore) |> should 
            (throwWithMessage "Enumeration has either not started or has already finished.") 
            typeof<InvalidOperationException>

    [<Test>]
    let ``Should delete single value`` () =
        let tree = BinaryTree()
        tree.Add 7
        tree.Contains 7 |> should equal true
        tree.Remove 7
        tree.Contains 7 |> should equal false

    [<Test>]
    let ``Should delete several values`` () =
        let tree = BinaryTree()
        [1 .. 10] |> List.map (fun v -> tree.Add v) |> ignore
        [10; 3; 6; 9; 4] |> List.map (fun v -> tree.Remove v) |> ignore
        tree |> should equivalent [1; 2; 5; 7; 8]

    [<Test>]
    let ``Should not fail when not existing value from empty tree correctly`` () =
        let tree = BinaryTree()
        tree.Remove 7
        tree.Contains 7 |> should equal false

    [<Test>]
    let ``Should delete not existing value from not empty tree correctly`` () =
        let tree = BinaryTree()
        [1 .. 10] |> List.map (fun v -> tree.Add v) |> ignore
        tree.Remove 11
        tree.Remove 12
        tree.Contains 11 |> should equal false
        tree.Contains 12 |> should equal false
       
    [<Test>]
    let ``Enumerator should work correctly`` () =
        let tree = BinaryTree()
        [1 .. 10] |> List.map (fun v -> tree.Add v) |> ignore
        
        let mutable enumerator = (tree :> IEnumerable).GetEnumerator()
        let returned = List<obj>()
        for i in 1 .. 10 do
            enumerator.MoveNext() |> ignore
            returned.Add enumerator.Current

        returned |> should equivalent [1..10]

    [<Test>]
    let ``Several enumerators should work correctly`` () =
        let tree = BinaryTree()
        [1 .. 6] |> List.map (fun v -> tree.Add v) |> ignore
        
        let mutable enumerator1 = (tree :> IEnumerable).GetEnumerator()
        let mutable enumerator2 = (tree :> IEnumerable).GetEnumerator()
        let returned1 = List<obj>()
        let returned2 = List<obj>()

        for i in 1 .. 5 do
            enumerator1.MoveNext() |> ignore
        for i in 1 .. 6 do
            enumerator2.MoveNext() |> ignore

        enumerator1.Current |> should not' (be equal enumerator2.Current)
