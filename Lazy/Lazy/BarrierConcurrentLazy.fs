namespace Lazy

    /// Returns a lazily initialized value with a guarantee of correct work in a multithreading program
    /// The first call causes the calculation and returns the result, next calls return the same object as the first call
    type BarrierConcurrentLazy<'a>(supplier: unit -> 'a) = 
        [<VolatileField>]
        let mutable instance = None
        let lockObj = obj()

        interface ILazy<'a> with 
            member this.Get () =
                if instance.IsNone then                
                    lock lockObj (fun () -> 
                        if instance.IsNone then 
                            let value = supplier() |> Some
                            instance <- value)
                instance.Value
