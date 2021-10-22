/// Contains functions that checks if all implementations of lazy works correctly
module Lazy.Tests

    open NUnit.Framework
    open FsUnit
    open Lazy
    open System.Threading
    open System.Threading.Tasks

    [<Test>]
    let ``Tread unsafe lazy should call calculation once`` () =
        let mutable counter = 0
        let lazyInstance = LazyFactory.CreateLazy(fun () -> counter <- counter + 1)
        for _ in 1 .. 10 do
            lazyInstance.Get ()
            counter |> should equal 1

    [<Test>]
    let ``Barrier concurrent lazy should call calculation once in single-thread scenario`` () =
        let mutable counter = 0
        let lazyInstance = LazyFactory.CreateBarrierConcurrentLazy(fun () -> counter <- counter + 1)
        for _ in 1 .. 10 do
            lazyInstance.Get ()
            counter |> should equal 1

    [<Test>]
    let ``Lock-free concurrent lazy should return result of first calculation in single-thread scenario`` () =
        let mutable counter = 0
        let lazyInstance = LazyFactory.CreateLockFreeConcurrentLazy(fun () -> counter <- counter + 1)
        for _ in 1 .. 10 do
            lazyInstance.Get ()
            counter |> should equal 1

    [<Test>]
    let ``Barrier concurrent lazy should call calculation once in multithreaded-thread scenario`` () =
        let mutable counter = 0
        let lazyInstance = LazyFactory.CreateBarrierConcurrentLazy(fun () -> Interlocked.Increment &counter)
        Parallel.For(1, 30, (fun _ -> lazyInstance.Get () |> ignore)) |> ignore
        counter |> should equal 1           

    [<Test>]
    let ``Lock-free concurrent lazy should return result of first calculation in multithreaded-thread scenario`` () =
        let mutable counter = 0
        let lazyInstance = LazyFactory.CreateLockFreeConcurrentLazy(fun () -> Interlocked.Increment &counter)
        Parallel.For(1, 30, (fun _ -> lazyInstance.Get () |> ignore)) |> ignore
        counter |> should equal 1
