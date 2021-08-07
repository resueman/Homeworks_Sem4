namespace LambdaParser

/// contains methods to parse string with lambda-expression and to represent result as abstract syntax tree  
module LambdaParser =
    open FParsec

    /// abstract syntax tree that represents lambda-expression
    type Term = 
        | Var of string
        | Abs of string * Term
        | App of Term * Term
    
    /// intermediate representation of lambda-expression
    type NamedT = 
        NamedT of string * T
    and T =  
        T of Primary * T'
    and Primary =
        | Var of string
        | Abs of string * T
        | Brackets of T
    and T' = 
        | App of Primary * T'
        | Epsilon

    /// builds abstract syntax tree from intermediate representstion, which was built by input string with lambda expression
    let rec buildAST exp =
        let buildPrimary primary = 
            match primary with
            | Var name -> Term.Var(name)
            | Brackets t -> buildAST t
            | Abs (name, t) -> Term.Abs(name, buildAST t)

        let rec buildT' t' acc =
            match t' with
            | Epsilon -> acc
            | App(p, r) -> Term.App(acc, buildPrimary p) |> buildT' r 

        let buildT (T(primary, t')) = buildT' t' (buildPrimary primary)
            
        buildT exp

    /// builds intermediate representation of lambda-expression by input string with lambda-expression
    let buildIntermediateRepresentation input = 
        let t, tRef = createParserForwardedToRef()
        
        let name = many1Satisfy isLetter
        let variable = name |>> Var
        let brackets = (pchar '(') >>. t .>> (pchar ')') |>> Brackets 
        let abstraction = attempt ((pchar '\\') >>. name .>> (pchar '.') .>>. t |>> Abs) // "\x.x"
                            <|> ((pchar '\\') >>. name .>>. t |>> Abs) // "\x y.x"
                            <|> (attempt ((pchar ' ') >>. name .>> (pchar '.') .>>. t |>> Abs)) // " x.x"
                            <|> ((pchar ' ') >>. name .>>. t |>> Abs) // " y z.x"

        let primary = brackets <|> abstraction <|> variable                

        let t', t'Ref = createParserForwardedToRef()
        let application = (pchar ' ') >>. primary .>>. t' |>> App
        t'Ref := application <|> (preturn T'.Epsilon)

        tRef :=  primary .>>. t' |>> T
        let nameOfTerm =  pstring("let") .>> spaces >>. name .>> spaces .>> pchar('=') .>> spaces

        let namedTerm = (nameOfTerm <|> preturn "") .>>. t |>> NamedT

        input |> run namedTerm
