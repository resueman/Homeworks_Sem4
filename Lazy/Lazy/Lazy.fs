namespace Lazy 

    type Lazy<'a>(supplier: unit -> 'a) = 
        let mutable instance = None

        interface ILazy<'a> with
            member this.Get () =
                if instance.IsNone then
                    instance <- (supplier () |> Some)
                
                instance.Value
