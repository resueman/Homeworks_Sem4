open System

let printMenu = 
    printfn "Welcome to lambda-reductor!"
    printfn "This program receives set of named definitions with lambda-expressions and lambda expression consisting of named definitions"
    printfn "Then program parses lambda expression and reduces it to normal form if it possible using beta-reduction with normal strategy"
    printfn "Example:"
    printfn "Input:"
    printfn "let S = \x y z.x z (y z)"
    printfn "let K = \x y.x"
    printfn "S K K"
    printfn "Output: \x.x"

let printOptions =
    printfn "Please, choose option:"
    printfn "1 <string> -- Enter input as string"
    printfn "2 <filename> -- Get input from file"

let rec loop = 
    printOptions
    //let input = Console.ReadLine()

[<EntryPoint>]
let main argv =
    printMenu
    loop
    0