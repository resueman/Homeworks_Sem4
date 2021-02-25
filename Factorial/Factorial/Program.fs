open System
open Factorial

[<EntryPoint>]
let main _ = 
    printf "Enter the number of which will calculate factorial: "
    match Console.ReadLine() |> bigint.TryParse with
    | true, number ->
        let factorial = factorial number 
        match factorial with
        | Some(value)
            -> printf "%A! = %A" number value
        | None
            -> printf "Input number must be non-negative"
    | _ -> printf "Input must be a non-negative integer number"
    0
