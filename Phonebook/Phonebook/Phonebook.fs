module Phonebook

    open System.IO
    open Commands
    open System.Text.Json
    open System.Text.RegularExpressions

    type Record = { Name: string; Phone: string }

    let doAdd record database =
        if List.filter ((=) record) database <> [] then
            database
        else 
            record::database

    let doFindNames phone = 
        List.filter (fun x -> x.Phone = phone) >> List.map (fun x -> x.Name)

    let doFindPhones name = 
        List.filter (fun x -> x.Name = name) >> List.map (fun x -> x.Phone)

    let writeToFile path database = 
        let getStreamWriter = 
            if not <| File.Exists path then 
                let fs = File.Create path
                new StreamWriter(fs)
            else 
                new StreamWriter(path, false)

        use writer = getStreamWriter
        let json = JsonSerializer.Serialize<Record list> database
        writer.Write json

    let readFromFile path =
        if File.Exists path |> not then
            printf "File %s doesn't exist" path
            None
        else
            use reader = new StreamReader(path)
            let json = reader.ReadToEnd ()
            JsonSerializer.Deserialize<Record list> json |> Some

    let formatRecord record = 
        sprintf "%s -- %s" record.Phone record.Name

    let doPrintAll = 
        List.map formatRecord >> List.iter (printf "%s\n")

    //////////////////////////////////////////////////////////////////

    let add input database = 
        let regex = new Regex(@$"{addCommand}\s+(.+)\s+(.+)")
        let inputMatch = regex.Match input
        if inputMatch.Success then
            let record = {Phone = inputMatch.Groups.[1].Value; Name = inputMatch.Groups.[2].Value}
            database |> doAdd record
        else
            printfn "Incorrect number of arguments for '%s' command\n" addCommand
            database

    let findNames input database = 
        let regex = new Regex(@$"{findNamesCommand}\s+(.+)")
        let inputMatch = regex.Match input
        if inputMatch.Success then
            let phone = inputMatch.Groups.[1].Value
            database |> doFindNames phone |> Some
        else
            printfn "Incorrect number of arguments for '%s' command\n" findNamesCommand
            None

    let findPhones input database = 
        let regex = new Regex(@$"{findPhonesCommand}\s+(.+)")
        let inputMatch = regex.Match input
        if inputMatch.Success then
            let name = inputMatch.Groups.[1].Value
            database |> doFindPhones name |> Some
        else
            printfn "Incorrect number of arguments for '%s' command\n" findPhonesCommand
            None

    let save input database = 
        let regex = new Regex(@$"{saveCommand}\s+(.+)")
        let inputMatch = regex.Match input
        if inputMatch.Success then
            let filename = inputMatch.Groups.[1].Value
            database |> writeToFile filename
        else
            printfn "Incorrect number of arguments for '%s' command\n" saveCommand

    let restore input oldDatabase =
        let regex = new Regex(@$"{restoreCommand}\s+(.+)")
        let inputMatch = regex.Match input
        let newDatabase = 
            if inputMatch.Success then
                let filename = inputMatch.Groups.[1].Value
                readFromFile filename
            else
                printfn "Incorrect number of arguments for '%s' command\n" restoreCommand
                None

        match newDatabase with
        | Some(db) -> db
        | None -> oldDatabase

    let printAll database = 
        match database with
        | [] -> printfn "Database is empty\n"
        | _ -> doPrintAll database