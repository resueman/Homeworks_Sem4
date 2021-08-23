/// Contains functions that checks if simulation of computers infection in local network goes as expected
module LocalNetwork.Tests

    open NUnit.Framework
    open LocalNetwork
    open System.Collections.Generic
    open FsUnit

    type OS(probability: float) =
        interface IOS with
            member this.ProbabilityOfInfection = probability

    type Computer(probability: float) =
        let mutable isInfected = false

        interface IComputer with
            member this.OS = OS(probability) :> IOS
            member this.IsInfected with get() = isInfected
            member this.TryToGetInfected () = 
                if probability <> 0.0 then
                    isInfected <- true

    type InfectedComputer() =
        interface IComputer with
            member this.OS = OS(0.0) :> IOS
            member this.IsInfected = true
            member this.TryToGetInfected () = ()

    let makeNetwork counter probability =
        let adjacencyMatrix = Dictionary<IComputer, IComputer list>()
        let rec build computer step computers = 
            let right = Computer(probability) :> IComputer
            let bottom = Computer(probability) :> IComputer
            let adj = [bottom; right]
            adjacencyMatrix.Add(computer, adj)
            adjacencyMatrix.Add(bottom, [])
            if step >= counter then
                adjacencyMatrix, (bottom::right::computers) 
            else 
                build right (step + 2) (bottom::right::computers) 

        let infectedComputer = InfectedComputer():>IComputer
        let adjMatrix, computers = build infectedComputer 3 [infectedComputer]
        LocalNetwork(adjMatrix), computers
 
        // a - b - c - d -e   -> a g b h c i d j e
        // |   |   |   |  
        // g   h   i   j  

    let getInfectionList computers = 
        computers |> List.map (fun c -> c :> IComputer) |> List.map (fun c -> c.IsInfected) 
        
    [<Test>]
    let ``State of network shouldn't change if all computers are protected`` () =
        let network, computers = makeNetwork 9 0.0
        network.IsInTheFinalState () |> should equal true
        getInfectionList computers |> should equal [false; false; false; false; false; false; false; false; true]

    [<Test>]
    let ``Virus should behave as BFS if probability of infection is 1`` () =
        let network, computers = makeNetwork 9 1.0
        
        network.IsInTheFinalState () |> should equal false
        getInfectionList computers |> should equal [false; false; false; false; false; false; false; false; true]
        network.MakeStep ()

        network.IsInTheFinalState () |> should equal false
        getInfectionList computers |> should equal [false; false; false; false; false; false; true; true; true]
        network.MakeStep ()
        
        network.IsInTheFinalState () |> should equal false
        getInfectionList computers |> should equal [false; false; false; false; true; true; true; true; true]
        network.MakeStep ()
        
        network.IsInTheFinalState () |> should equal false
        getInfectionList computers |> should equal [false; false; true; true; true; true; true; true; true]
        network.MakeStep ()
        
        getInfectionList computers |> should equal [true; true; true; true; true; true; true; true; true]
        network.IsInTheFinalState () |> should equal true

    [<Test>]
    let ``Network should behave as expected when network is already in final state`` () =
        let network, computers = makeNetwork 9 1.0
        for _ in 1 .. 4 do
            network.MakeStep ()

        for _ in 1 .. 10 do
            getInfectionList computers |> should equal [true; true; true; true; true; true; true; true; true]
            network.IsInTheFinalState () |> should equal true
            network.MakeStep ()
        
        let network, computers = makeNetwork 9 0.0
        for _ in 1 .. 10 do
            network.IsInTheFinalState () |> should equal true
            getInfectionList computers |> should equal [false; false; false; false; false; false; false; false; true]
            network.MakeStep ()
