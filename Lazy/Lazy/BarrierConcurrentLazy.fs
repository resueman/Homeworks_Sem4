namespace Lazy

    type BarrierConcurrentLazy<'a>(supplier: unit -> 'a) = 
        [<VolatileField>]
        let mutable instance = None
        let lockObj = obj()

        interface ILazy<'a> with 
            member this.Get () =
                lock lockObj (fun () -> 
                    if instance.IsNone then 
                        let value = () |> supplier |> Some
                        instance <- value)

                instance.Value
