namespace WorkflowWithStrings

module WorkflowWithStrings =

    let (|Int|_|) str =
        match System.Int32.TryParse(str:string) with
        | true, int -> Some(int)
        | _ -> None

    type CalculateBuilder() =
        member this.Bind(x, f) =
            match x with
            | Int i -> f i
            | _ -> None
        member this.Return x = Some x
