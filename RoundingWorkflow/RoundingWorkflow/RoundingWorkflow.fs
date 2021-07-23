namespace RoundingWorkflow

/// Contains workflow that performs mathematical calculations 
/// with a specified precision (number of digits after the dot)
module RoundingWorkflow =
    open System

    /// Workflow that performs mathematical calculations 
    /// with a specified precision (number of digits after the dot)
    type RoundingBuilder(digits: int) =
        member this.Bind(x: float, f) = 
            Math.Round (x, digits) |> f

        member this.Return (x: float) = 
            Math.Round (x, digits) |> Some
