open System

let rec fibonacci n =
    let rec calculate previous current i = 
        match i with
        | i when i = n -> previous + current
        | _ -> calculate current (previous + current) (i + 1)            

    match n with
    | 0 -> Some(0)
    | 1 -> Some(1)
    | someNumber when someNumber <= 0 -> None
    | _ -> Some(calculate 0 1 2)

[<EntryPoint>]
let main _ = 
    printf "Enter number of Fibonacchi you want to get: "
    match Console.ReadLine() |> Int32.TryParse with
    | true, number ->
        let result = fibonacci number
        match result with
        | None -> printf "Number of Fibonacci must be non-negative number"
        | Some(fibonacci) -> printf "%d-th Fibonacci is %d" number fibonacci
    | _ -> printf "Number of Fibonacchi must be non-negative integer number"    
    0
