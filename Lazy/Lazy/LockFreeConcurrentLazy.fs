namespace Lazy

    open System.Threading

    /// Returns a lazily initialized value with a guarantee of correct work in a multithreading program
    /// The first call causes the calculation and returns the result, next calls return the same object as the first call
    type LockFreeConcurrentLazy<'a>(supplier: unit -> 'a) = 
        let mutable instance = None

        interface ILazy<'a> with 
            member this.Get () =
                if instance.IsNone then
                    let oldValue = instance
                    let newValue = supplier () |> Some
                    Interlocked.CompareExchange(&instance, newValue, oldValue) |> ignore

                instance.Value
    