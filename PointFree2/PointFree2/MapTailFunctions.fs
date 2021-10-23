namespace PointFree2

/// Contains equivalent functions, that take a function and a list, 
/// and return mapped tail of given list
module MapTailFunctions =
    
    let f g l = List.map g (List.tail l)
     
    let f' g = List.tail >> List.map g

    let f'' g l = ((>>) List.tail) << List.map <| g l // let f'' = ((>>) List.tail) << List.map, без указания g и l ругается
