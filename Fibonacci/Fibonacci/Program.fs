open System
open Fibonacci

[<EntryPoint>]
let main _ = 
    printf "Enter number of Fibonacchi you want to get: "
    match Console.ReadLine() |> bigint.TryParse with
    | true, number ->
        let result = fibonacci number
        match result with
        | None -> printf "Number of Fibonacci must be non-negative number"
        | Some(fibonacci) -> printf "%A-th Fibonacci is %A" number fibonacci
    | _ -> printf "Number of Fibonacchi must be non-negative integer number"    
    0
