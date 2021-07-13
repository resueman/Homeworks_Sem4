open System
open Commands
open Phonebook

let printMenu = 
    printf "Hello, it's a phonebook!\n"
    printf "Please, enter command:\n"
    printf "%s\n" exitCommand
    printf "%s <phone> <name>\n" addCommand
    printf "%s <name>\n" findPhonesCommand
    printf "%s <phone>\n" findNamesCommand
    printf "%s\n" printAllCommand
    printf "%s <filename>\n" saveCommand
    printf "%s <filename>\n\n" restoreCommand    
    
let printFields fields =
    match fields with
    | Some(values) -> values |> List.map (sprintf "%s\n") |> List.iter (printf "%s")
    | None -> printfn "Fields not found\n\n"

let rec loop database = 
    let input = Console.ReadLine()
    let command = input.Trim().Split(' ').[0];
    match command with
    | x when x = exitCommand -> database
    | x when x = addCommand -> add input database |> loop
    | x when x = findNamesCommand -> findNames input database |> printFields
                                     loop database    
    | x when x = findPhonesCommand -> findPhones input database |> printFields
                                      loop database    
    | x when x = printAllCommand -> printAll database 
                                    loop database
    | x when x = saveCommand -> save input database
                                loop database
    | x when x = restoreCommand -> restore input database |> loop
    | _ -> printfn "No such a command, try again\n\n"
           loop database

[<EntryPoint>]
let main argv =
    loop List.Empty |> ignore
    0