namespace EvenNumbersCounter.Tests

module Tests =
    open NUnit.Framework
    open FsCheck
    open EvenNumbersCounter.EvenNumbersCounters

    let ``Map counter should be equal as function to filter counter`` (list: list<int>) = 
        mapCounter list = filterCounter list
    
    Check.QuickThrowOnFailure ``Map counter should be equal as function to filter counter``

    let ``Filter counter should be equal as function to fold counter`` (list: list<int>) =
        filterCounter list = foldCounter 0 list

    Check.QuickThrowOnFailure ``Filter counter should be equal as function to fold counter``

    [<TestCaseSource("testCases")>]
    let ``Map counter should return expected value`` list =
        mapCounter list

    [<TestCaseSource("testCases")>]
    let ``Filter counter should return expected value`` list =
        filterCounter list

    [<TestCaseSource("testCases")>]
    let ``Fold counter should return expected value`` list =
        foldCounter 0 list

    let testCases = 
        [
            TestCaseData(List.empty<int>).Returns(0)
            TestCaseData([1]).Returns(0)
            TestCaseData([1; 1; 1; 3; 7; 19; 21]).Returns(0)
            TestCaseData([2]).Returns(1)
            TestCaseData([24; 65; 13; 9; 12; 64]).Returns(3)
            TestCaseData([2; 4; 6; 8; 16]).Returns(5)
            TestCaseData([1; 2; 3; 4; 5; 6; 7; 8; 9; 10]).Returns(5)
            TestCaseData(Seq.initInfinite(fun x -> x) |> Seq.filter(fun x -> x % 2 = 1) |> Seq.take 10000 |> Seq.toList).Returns(0)
            TestCaseData(Seq.initInfinite(fun x -> x) |> Seq.filter(fun x -> x % 2 = 0) |> Seq.take 10000 |> Seq.toList).Returns(10000)
        ]
