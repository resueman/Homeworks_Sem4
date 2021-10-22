namespace LambdaParser

/// provides the user with an interface to interact with the lambda-interpreter
module LambdaParserUI = 
    open AST
    open LambdaParser
    open FParsec
    open System
    open System.IO

    /// returns abstract syntax tree build from lambda expression and named definitions 
    let getAST expList =
        let performSubstitutions predefinedAST term = 
            let rec substitute term = 
                match term with
                | Term.App(l, r) -> Term.App(substitute l, substitute r)
                | Term.Var(name) -> (List.find (fun (n, t) -> n = name) predefinedAST) |> snd
                | _ -> term
            
            substitute term
        
        let rec get predefinedAST expList =
            match expList with
            | [] -> None
            | h::t -> match buildIntermediateRepresentation h with
                      | Success(NamedT(name, term), _, _) when name = "" -> buildAST term |> performSubstitutions predefinedAST |> Some
                      | Success(NamedT(name, term), _, _) -> let namedAST = name, buildAST term
                                                             get (namedAST::predefinedAST) t
                      | _ -> get predefinedAST t
        
        get [] expList

    /// parse string with named definitions and lambda-expression from given string
    /// returns option with abstract syntax tree, if expressions were in correct form; otherwise returns None
    let parseFromString (inputString: string) = 
        let splitted = inputString.Split([| "+"; |], StringSplitOptions.None) |> Array.toList
        getAST splitted

    /// parse string with named definitions and lambda-expression from file
    /// returns option with abstract syntax tree, if expressions were in correct form; otherwise returns None
    let parseFromFile (filename: string) = 
        let strings = seq {
            use reader = new StreamReader(filename)
            while not reader.EndOfStream do
                yield reader.ReadLine ()
        }
        Seq.toList strings |> getAST
