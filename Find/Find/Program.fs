open System

let find sequence element = 
    let rec loop index sequence =
        match sequence with
        | seq when Seq.isEmpty seq -> None
        | seq when Seq.head seq <> element -> loop (index + 1) (Seq.tail seq)
        | _ -> Some(index)
    loop 0 sequence

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
