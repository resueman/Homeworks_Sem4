open System
open Commands
open PhonebookService

let printMenu = 
    printfn "Hello, it's a phonebook!"
    printfn "Please, enter command:"
    printfn "%s" exitCommand
    printfn "%s <phone> <name>" addCommand
    printfn "%s <name>" findPhonesCommand
    printfn "%s <phone>" findNamesCommand
    printfn "%s" printAllCommand
    printfn "%s <filename>" saveCommand
    printfn "%s <filename>" restoreCommand    
    
let printFields fields =
    match fields with
    | Some([]) -> printfn "No records"
    | Some(values) -> values |> List.map (sprintf "%s") |> List.iter (printfn "%s")
    | None -> printfn "Records not found"

let rec loop database = 
    printfn ""
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
    | _ -> printfn "No such a command, try again"
           loop database

[<EntryPoint>]
let main _ =
    loop List.Empty |> ignore
    0