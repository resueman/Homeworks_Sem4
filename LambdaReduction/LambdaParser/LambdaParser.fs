namespace LambdaParser

/// contains methods to parse string with lambda-expression and to represent result as abstract syntax tree  
module LambdaParser =
    
    open AST
    open FParsec
    
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
        
        let identifier =  
            let isIdentifierFirstChar c = isLetter c || c = '_'
            let isIdentifierChar c = isLetter c || isDigit c || c = '_'
            many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

        let variable = identifier |>> Var
        let brackets = (pchar '(') >>. t .>> (pchar ')') |>> Brackets 
        let abstraction = attempt ((pchar '\\') >>. identifier .>> (pchar '.') .>>. t |>> Abs) // "\x.x"
                            <|> ((pchar '\\') >>. identifier .>>. t |>> Abs) // "\x y.x"
                            <|> (attempt ((pchar ' ') >>. identifier .>> (pchar '.') .>>. t |>> Abs)) // " x.x"
                            <|> ((pchar ' ') >>. identifier .>>. t |>> Abs) // " y z.x"

        let primary = brackets <|> abstraction <|> variable                

        let t', t'Ref = createParserForwardedToRef()
        let application = (pchar ' ') >>. primary .>>. t' |>> App
        t'Ref := application <|> (preturn T'.Epsilon)

        tRef :=  primary .>>. t' |>> T
        let nameOfTerm =  pstring("let") .>> spaces >>. identifier .>> spaces .>> pchar('=') .>> spaces

        let namedTerm = (nameOfTerm <|> preturn "") .>>. t |>> NamedT

        input |> run namedTerm
