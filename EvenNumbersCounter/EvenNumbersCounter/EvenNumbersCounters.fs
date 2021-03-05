namespace EvenNumbersCounter

/// <summary>Contains even numbers counters that has same functionality, but distinct implementation</summary>
module EvenNumbersCounters =
    
    /// <summary>
    /// Counts number of even numbers in a list using map function
    /// </summary>
    /// <returns>Number of even numbers in a list</returns>
    let mapCounter =
        List.map (fun x -> if x % 2 = 0 then 1 else 0) >> List.sum

    /// <summary>
    /// Counts number of even numbers in a list using filter function
    /// </summary>
    /// <returns>Number of even numbers in a list</returns>
    let filterCounter = 
        List.filter (fun x -> x % 2 = 0) >> List.length
    
    /// <summary>
    /// Counts number of even numbers in a list using fold function
    /// </summary>
    /// <returns>Number of even numbers in a list</returns>
    let foldCounter =
        List.fold (fun acc x -> if x % 2 = 0 then acc + 1 else acc) 0
