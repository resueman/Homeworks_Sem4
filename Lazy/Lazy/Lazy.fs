namespace Lazy 

    /// Returns a lazily initialized value without a guarantee of correct work in a multithreading program.
    /// The first call causes the calculation and returns the result, next calls return the same object as the first call
    type Lazy<'a>(supplier: unit -> 'a) = 
        let mutable instance = None

        interface ILazy<'a> with
            member this.Get () =
                if instance.IsNone then
                    instance <- (supplier () |> Some)
                
                instance.Value
