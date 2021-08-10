open System
open System.Text.RegularExpressions
open LambdaParser.LambdaParserUI
open LambdaInterpreter.LambdaInterpreter
open AstDecoder

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

let processAST option = 
    match option with
    | Some term -> term |> reduce |> buildLambdaExpressionFromAST |> printfn "Reduced: %s"
    | None -> printfn "Unable to parse input"

let rec loop () = 
    printOptions
    let input = Console.ReadLine()
    let regex = Regex("^\s*([1 2])\s+(.+)\s*$")
    let inputMatch = regex.Match input
    if inputMatch.Success then
        if inputMatch.Groups.[1].Value = "1" then
            parseFromString inputMatch.Groups.[2].Value |> processAST
        else 
            parseFromFile inputMatch.Groups.[2].Value |> processAST
    else
        printfn "incorrect request"
        loop ()

[<EntryPoint>]
let main _ =
    printMenu
    loop ()
    0