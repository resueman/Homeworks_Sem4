module PowersOfTwo

let generateListOfPowers n m =
    let rec loop n powers =
        let newPower = List.head powers / 2
        match n with 
        | 0 -> powers
        | n when (List.length powers) = n -> newPower :: powers 
        | _ -> loop n (newPower :: powers)            
    
    if n < 0 || m < 0 then None
    else Some(loop m [pown 2 (n + m)]) 
