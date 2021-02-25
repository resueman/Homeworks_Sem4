open System
open FindElement

[<EntryPoint>]
let main _ = 
    printfn "Enter a sequence separating elements with a space:"
    let items = Console.ReadLine().Split[|' '|]
    printf "Enter element which position in sequence you want to get: "
    let soughtFor = Console.ReadLine()
    match find items soughtFor with
    | Some(position) -> printf "Position of %s is %d" soughtFor position
    | None -> printf "Element %s missing in sequence" soughtFor
    0
