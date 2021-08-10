namespace Lazy

    open System.Threading

    type LockFreeConcurrentLazy<'a>(supplier: unit -> 'a) = 
        let mutable instance = None

        interface ILazy<'a> with 
            member this.Get () =
                if instance.IsNone then
                    let newValue = () |> supplier |> Some
                    Interlocked.CompareExchange(ref instance, newValue, None) |> ignore

                instance.Value
    