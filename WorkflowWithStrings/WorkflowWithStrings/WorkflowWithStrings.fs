namespace WorkflowWithStrings

/// Contains workflow performing calculations with numbers, given as strings
module WorkflowWithStrings =

    /// Active pattern for parsing string to int
    let (|Int|_|) str =
        match System.Int32.TryParse(str:string) with
        | true, int -> Some(int)
        | _ -> None

    /// Workflow performing calculations with numbers, given as strings
    type CalculateBuilder() =
        member this.Bind(x, f) =
            match x with
            | Int i -> f i
            | _ -> None
        member this.Return x = Some x
