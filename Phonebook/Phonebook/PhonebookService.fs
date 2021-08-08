﻿module PhonebookService

    open Commands
    open Phonebook
    open System.Text.RegularExpressions

    let add input database = 
        let regex = new Regex(@$"^\s*{addCommand}\s+(.+)\s+(.+)\s*$")
        let inputMatch = regex.Match input
        if inputMatch.Success then
            let record = {Phone = inputMatch.Groups.[1].Value; Name = inputMatch.Groups.[2].Value}
            let newDatabase = database |> doAdd record 
            printfn "Success!"
            newDatabase
        else
            printfn "Incorrect number of arguments for '%s' command" addCommand
            database

    let findNames input database = 
        let regex = new Regex(@$"^\s*{findNamesCommand}\s+(.+)\s*$")
        let inputMatch = regex.Match input
        if inputMatch.Success then
            let phone = inputMatch.Groups.[1].Value
            database |> doFindNames phone |> Some
        else
            printfn "Incorrect number of arguments for '%s' command" findNamesCommand
            None

    let findPhones input database = 
        let regex = new Regex(@$"^\s*{findPhonesCommand}\s+(.+)\s*$")
        let inputMatch = regex.Match input
        if inputMatch.Success then
            let name = inputMatch.Groups.[1].Value
            database |> doFindPhones name |> Some
        else
            printfn "Incorrect number of arguments for '%s' command" findPhonesCommand
            None

    let save input database = 
        let regex = new Regex(@$"^\s*{saveCommand}\s+(.+)\s*$")
        let inputMatch = regex.Match input
        if inputMatch.Success then
            let filename = inputMatch.Groups.[1].Value
            database |> writeToFile filename
            printfn "Phonebook successfully saved to '%s'" filename
        else
            printfn "Incorrect number of arguments for '%s' command" saveCommand

    let restore input oldDatabase =
        let regex = new Regex(@$"^\s*{restoreCommand}\s+(.+)\s*$")
        let inputMatch = regex.Match input
        let newDatabase = 
            if not inputMatch.Success then
                printfn "Incorrect number of arguments for '%s' command" restoreCommand
                None
            else
                let filename = inputMatch.Groups.[1].Value
                let database = readFromFile filename
                match database with
                | None -> printfn "File %s doesn't exist" filename
                          None
                | _ -> printfn "Database successfully restored from '%s'" filename
                       database

        match newDatabase with
        | Some db -> db
        | None -> oldDatabase

    let printAll database = 
        match database with
        | [] -> printfn "Database is empty"
        | _ -> doPrintAll database
