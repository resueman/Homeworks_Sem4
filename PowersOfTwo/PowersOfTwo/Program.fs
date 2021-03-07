open System
open PowersOfTwo

let handleInput _ =
    match Console.ReadLine() |> Int32.TryParse with
    | (true, value) when value >= 0 -> Some(value)
    | _ -> None

let rec inputLoop name =
    match handleInput () with 
    | Some(value) -> 
        value
    | None -> 
        printfn "%s must be integer positive number" name
        printfn ""
        printf "Enter %s: " name
        inputLoop name

[<EntryPoint>]
let main _ = 
    printfn "This program generates list of [2^N; 2^(N + 1); ...; 2^(N + M)]"
    printf "Enter N: "
    let n = inputLoop "N"
    printf "Enter M: "
    let m = inputLoop "M"
    let powers = generateListOfPowers n m
    printfn "Powers: %A" powers
    0
