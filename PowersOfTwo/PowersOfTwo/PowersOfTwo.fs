module PowersOfTwo

let raiseNumberToNthPower powerBase power=
    let rec loop i value =
        match i with
        | i when i = power -> value
        | _ -> loop (i + 1) (value * powerBase)

    loop 0 1

let generateListOfPowers n m =
    let rec loop n powers =
        let newPower = 2 * List.last powers
        match n with 
        | 0 -> powers
        | n when (List.length powers) = n -> powers @ [newPower]
        | _ -> loop n (powers @ [newPower])            
    
    if n < 0 || m < 0 then None
    else Some(loop m [raiseNumberToNthPower 2 n]) 
