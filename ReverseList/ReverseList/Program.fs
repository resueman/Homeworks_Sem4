open System

let rec reverse list = Seq.fold (fun acc x -> x :: acc) [] list

[<EntryPoint>]
let main _ =
    printfn "Enter items separating them with space:"
    let items = Console.ReadLine().Split[|' '|]
    printfn "Reversed items:"
    reverse items |> printfn "%A"
    0
