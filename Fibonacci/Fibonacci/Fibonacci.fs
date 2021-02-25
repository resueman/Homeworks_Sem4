module Fibonacci

let rec fibonacci n =
    let rec calculate previous current i = 
        match i with
        | i when i = n -> previous + current
        | _ -> calculate current (previous + current) (i + 1I)            

    if n = 0I then Some(0I)
    elif n = 1I then Some(1I)
    elif n <= 0I then None
    else Some(calculate 0I 1I 2I)
