/// checks if multiply map functions are equivalent
module PointFree.Tests

open NUnit.Framework
open PointFree
open FsCheck
open MultiplyMap

[<Test>]
let ``Multiply map functions should return same result`` () =
    Check.QuickThrowOnFailure (fun list x -> 
        let a = nonPointFreeFunc1 list x
        let b = nonPointFreeFunc2 list x
        let c = nonPointFreeFunc3 list x
        let d = pointFreeFunc list x
        a = b && b = c && c = d)
