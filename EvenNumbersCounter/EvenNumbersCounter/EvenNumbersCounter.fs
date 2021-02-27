namespace EvenNumbersCounter

module EvenNumbersCounters =
    let mapCounter =
        List.map (fun x -> if x % 2 = 0 then 1 else 0) >> List.sum

    let filterCounter = 
        List.filter (fun x -> x % 2 = 0) >> List.length

    let foldCounter =
        List.fold (fun acc x -> if x % 2 = 0 then acc + 1 else acc)
