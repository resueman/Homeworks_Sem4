/// Contains tests for functions that map list tail
module PointFree2.Tests

    open NUnit.Framework
    open FsCheck
    open MapTailFunctions

    [<Test>]
    let ``MapTail functions should be equivalent`` () =
        Check.QuickThrowOnFailure (fun mapper list -> 
            if List.isEmpty list then
                true
            else 
                let a = f mapper list
                let b = f' mapper list
                let c = f'' mapper list
                a = b && b = c)
