/// contains functions that allows to add, find, save, restore records
module Phonebook

    open System.IO
    open System.Text.Json

    /// describes form of records that can be stored in phonebook 
    type Record = { Name: string; Phone: string }

    /// adds record to phonebook
    let doAdd record database =
        if List.filter ((=) record) database <> [] then
            database
        else 
            record::database

    /// finds names by phone
    let doFindNames phone = 
        List.filter (fun x -> x.Phone = phone) >> List.map (fun x -> x.Name)

    /// finds phones by name
    let doFindPhones name = 
        List.filter (fun x -> x.Name = name) >> List.map (fun x -> x.Phone)

    /// writes all records from phonebook to file
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

    /// reads data from file; returns it as deserialized list of records
    let readFromFile path =
        if File.Exists path |> not then
            None
        else
            use reader = new StreamReader(path)
            let json = reader.ReadToEnd ()
            JsonSerializer.Deserialize<Record seq> json |> Seq.toList |> Some

    /// formats record
    let formatRecord record = 
        sprintf "%s -- %s" record.Phone record.Name

    /// prints all records
    let doPrintAll = 
        List.map formatRecord >> List.iter (printfn "%s")
