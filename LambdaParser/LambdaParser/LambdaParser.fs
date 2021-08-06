namespace LambdaParser

module LambdaParser =
    open FParsec

    /// AST
    type Term = 
        | Var of string
        | Abs of string * Term
        | App of Term * Term

    and T =  
        T of Primary * T'
    and Primary =
        | Var of string
        | Abs of string * T
        | Brackets of T
    and T' = 
        | App of Primary * T'
        | Epsilon

    let parse input = 
        let t, tRef = createParserForwardedToRef()
        let name = many1Satisfy isLetter
        let brackets = (pchar '(') >>. t .>> (pchar ')') |>> Brackets 
        
        let abstraction = (attempt ((pchar '\\') >>. name .>> (pchar '.') .>>. t |>> Abs)) // "\x.x"
                            <|> ((pchar '\\') >>. name .>>. t |>> Abs) // "\x y.x"
                            <|> (attempt ((pchar ' ') >>. name .>> (pchar '.') .>>. t |>> Abs)) // " x.x"
                            <|> ((pchar ' ') >>. name .>>. t |>> Abs) // " y z.x"

        let variable = name |>> Var <|> (spaces >>. name |>> Var)

        let primary = brackets <|> abstraction <|> variable                

        let t', t'Ref = createParserForwardedToRef()
        let application = (pchar ' ') >>. primary .>>. t' |>> App
        t'Ref := application <|> (preturn T'.Epsilon)

        tRef := primary .>>. t' |>> T

        let result = input |> run t
        printf "%A\n" result |> ignore
