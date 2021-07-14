module Computer
    open System
    open System.Collections.Generic

    type IOS = 
        abstract member ProbabilityOfInfection: float

    type IComputer =
        abstract member OS: IOS
        abstract member IsInfected: bool
        abstract member TryToGetInfected: unit -> unit

    type LocalNetwork(adjacencyMatrix: Dictionary<IComputer, IComputer list>) = 

        let computers = Seq.toList adjacencyMatrix.Keys

        let tryToInfectNeighbours computer = 
            adjacencyMatrix.[computer] |> List.iter (fun x -> x.TryToGetInfected ())

        member this.MakeStep () = 
            computers |> List.iter tryToInfectNeighbours

        member this.IsInTheFinalState () = 
            computers |> List.filter (fun x -> x.OS.ProbabilityOfInfection = 0.0 || x.IsInfected) |> List.isEmpty
