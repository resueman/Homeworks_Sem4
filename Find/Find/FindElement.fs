module FindElement

let find sequence element = 
    let rec loop index sequence =
        match sequence with
        | seq when Seq.isEmpty seq -> None
        | seq when Seq.head seq <> element -> loop (index + 1) (Seq.tail seq)
        | _ -> Some(index)
    loop 0 sequence
