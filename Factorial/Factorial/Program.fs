open System

let factorial n =
    let rec loop i acc =
        if i > n then 
            Some(acc)
        else 
            loop (i + 1) (acc * i)

    if n < 0 then 
        None
    else 
        loop 1 1

[<EntryPoint>]
let main _ = 
    printf "Enter the number of which will calculate factorial: "
    match Console.ReadLine() |> Int32.TryParse with
    | true, number ->
        let factorial = factorial number 
        match factorial with
        | Some(value)
            -> printf "%d! = %d" number value
        | None
            -> printf "Input number must be non-negative"
    | _ -> printf "Input must be a non-negative integer number"
    0