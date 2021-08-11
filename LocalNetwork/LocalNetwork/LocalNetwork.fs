namespace LocalNetwork

    open System.Collections.Generic

    /// Describes local network
    type LocalNetwork(adjacencyMatrix: Dictionary<IComputer, IComputer list>) = 

        let computers = 
            let list1 = Dictionary.ValueCollection adjacencyMatrix |> Seq.concat |> Seq.toList
            let list2 = Dictionary.KeyCollection adjacencyMatrix |> Seq.toList
            (list1 @ list2) |> List.distinct

        let tryToInfectNeighbours computer = 
            if adjacencyMatrix.ContainsKey computer then
                adjacencyMatrix.[computer] |> List.iter (fun x -> x.TryToGetInfected ())

        member this.MakeStep () =
            computers |> List.filter (fun c -> c.IsInfected) |> List.iter tryToInfectNeighbours

        member this.IsInTheFinalState () = 
            computers |> List.filter (fun x -> x.OS.ProbabilityOfInfection <> 0.0 && not x.IsInfected) |> List.isEmpty
