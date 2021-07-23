namespace Lazy

open System.Threading

module Lazy =

    type ILazy<'a> =
        abstract member Get: unit -> 'a

    type Lazy<'a>(supplier: unit -> 'a) = 
        let mutable instance = None

        interface ILazy<'a> with
            member this.Get () =
                if instance.IsNone then
                    instance <- (supplier () |> Some)
                    
                instance.Value

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

    type LockFreeConcurrentLazy<'a>(supplier: unit -> 'a) = 
        let mutable instance = None

        interface ILazy<'a> with 
            member this.Get () =
                if instance.IsNone then
                    let newValue = () |> supplier |> Some
                    Interlocked.CompareExchange(ref instance, newValue, None) |> ignore

                instance.Value
    
