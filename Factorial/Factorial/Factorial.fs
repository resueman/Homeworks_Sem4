module Factorial

let factorial n =
    let rec loop i acc =
        match n with 
        | n when n < i -> Some(acc)
        | _ -> loop (i + 1I) (acc * i)

    match n with 
    | n when n < 0I -> None
    | _ -> loop 1I 1I
