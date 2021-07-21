namespace RoundingWorkflow

module RoundingWorkflow =
    open System

    type RoundingBuilder(digits: int) =
        member this.Bind(x: float , f) = 
            Math.Round (x, digits) |> f

        member this.Return (x:float) = 
            Math.Round (x, digits) |> Some
