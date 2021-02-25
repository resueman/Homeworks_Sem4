namespace ReverseList.Tests

module ReverseListTests =
    open ReverseList
    open NUnit.Framework
    open FsUnit
    open FsCheck
    open FsCheck.NUnit
    
    [<Property>]
    let ``Should return the same list of integers after double reverse`` (ls:list<int>) = reverse >> reverse <| ls = ls
    
    Check.QuickThrowOnFailure ``Should return the same list of integers after double reverse``

    [<Property>]
    let ``Should return the same list of string arrays after double reverse`` (ls:list<array<string>>) = reverse >> reverse <| ls = ls
    
    Check.QuickThrowOnFailure ``Should return the same list of string arrays after double reverse``

    [<Property>]
    let ``Should return the same list of strings after double reverse`` (ls:list<string>) = reverse >> reverse <| ls = ls
     
    Check.QuickThrowOnFailure ``Should return the same list of strings after double reverse``

    [<Property>]
    let ``Should return the same list of integers as library function`` (ls:list<int>) = reverse <| ls = List.rev ls
    
    Check.QuickThrowOnFailure ``Should return the same list of integers as library function``

    [<Property>]
    let ``Should return the same list of string arrays as library function`` (ls:list<array<string>>) = reverse <| ls = List.rev ls
    
    Check.QuickThrowOnFailure ``Should return the same list of string arrays as library function``

    [<Property>]
    let ``Should return the same list of strings as library function`` (ls:list<string>) = reverse <| ls = List.rev ls
     
    Check.QuickThrowOnFailure ``Should return the same list of strings as library function``

    [<Test>]
    let ``Should return empty list when reverse empty list`` () =
        reverse [] |> should equal []

    [<Test>]
    let ``Should return same list consisting of one element`` () =
        reverse [6] |> should equal [6]

    [<Test>]
    let ``Should reverse list with even number of elements`` () =
        reverse [8; 9; 0; 34; -1; 81] |> should equal [81; -1; 34; 0; 9; 8]

    [<Test>]
    let ``Should reverse list with odd number of elements`` () =
        reverse [45; 89; 32; -5; -82] |> should equal [-82; -5; 32; 89; 45]

    [<Test>]
    let ``Should reverse list of strings`` () = 
        reverse ["dog"; "cat"; "fish"; "elephant"; "bird"] |> should equal ["bird"; "elephant"; "fish"; "cat"; "dog"]

    [<Test>]
    let ``Should reverse list of tuples`` () =
        reverse [(9, 87, 90); (8, 9, 0); (7, 9, 65); (89, 8, 1)] |> should equal [(89, 8, 1); (7, 9, 65); (8, 9, 0); (9, 87, 90)]
