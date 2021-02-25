open System
open ReverseList

[<EntryPoint>]
let main _ =
    printfn "Enter items separating them with space:"
    let items = Console.ReadLine().Split[|' '|]
    printfn "Reversed items:"
    items |> reverse |> printfn "%A"
    0
