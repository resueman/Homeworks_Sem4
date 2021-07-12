namespace PointFree

/// contains equivalent function that take a list and a number and
/// return the list obtained by multiplying the given list by the given number
module MultiplyMap =

    let nonPointFreeFunc1 x l = List.map (fun y -> y * x) l

    let nonPointFreeFunc2 x = List.map (fun y -> y * x)
    
    let nonPointFreeFunc3 x = List.map ((*) x)
    
    let pointFreeFunc = (*) >> List.map
