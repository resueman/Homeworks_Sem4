module Phonebook

    open System.IO
    open System.Text.Json

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
            None
        else
            use reader = new StreamReader(path)
            let json = reader.ReadToEnd ()
            JsonSerializer.Deserialize<Record seq> json |> Seq.toList |> Some

    let formatRecord record = 
        sprintf "%s -- %s" record.Phone record.Name

    let doPrintAll = 
        List.map formatRecord >> List.iter (printfn "%s")
