// Contains thread safe stack
module ConcurrentStack

open System

// Implementation of thread safe stack
type 'a ConcurrentStack() = 
    let mutable elements = []
    let mutable locker = Object()

    // Adds value to the top of stack
    member s.Push (value: 'a) =
        lock locker (fun () ->
        elements <- value :: elements)

    // If stack contains any elements, removes top element and returns it;
    // otherwise returns None
    member s.TryPop () =
        lock locker (fun () ->
        match elements with
        | [] -> None
        | top :: tail ->
            elements <- tail
            Some(top))       

