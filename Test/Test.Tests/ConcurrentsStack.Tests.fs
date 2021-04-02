// Contains tests for thread safe stack
module Test.Tests

    open NUnit.Framework
    open ConcurrentStack
    open FsUnit
    open System.Threading.Tasks
    open System.Threading

    [<Test>]
    let ``One Pop one TryPop test`` () =
        let stack = ConcurrentStack<int> ()
        stack.Push 1
        stack.TryPop() |> should equal (Some(1))

    [<Test>]
    let ``TryPop should return None on Empty stack test`` () =
        let stack = ConcurrentStack<int> ()
        stack.TryPop() |> should equal None

    [<Test>]
    let ``Multiple Push and TryPop test`` () =
        let stack = ConcurrentStack<int> ()
        stack.Push 1
        stack.Push 2
        stack.TryPop() |> should equal (Some(2))
        stack.Push 3
        stack.TryPop() |> should equal (Some(3))
        stack.TryPop() |> should equal (Some(1))
        stack.TryPop() |> should equal None
        stack.TryPop() |> should equal None

    [<Test>]
    let ``Concurrent stack is actually thread safe test`` () =
        let stack = ConcurrentStack<int> ()
        let resetEvent = new ManualResetEvent false
        let tasks = [|
            Task.Run (fun () -> 
                resetEvent.WaitOne() |> ignore 
                stack.Push 1);

            Task.Run (fun () -> 
                resetEvent.WaitOne() |> ignore
                stack.Push 2);

            Task.Run (fun () -> 
                resetEvent.WaitOne() |> ignore
                stack.Push 3)|]
        
        resetEvent.Set () |> ignore
        
        Task.WaitAll tasks

        stack.TryPop() |> should not' (equal None)
        stack.TryPop() |> should not' (equal None)
        stack.TryPop() |> should not' (equal None)
        stack.TryPop() |> should equal None
