module AstDecoder
    
    open AST

    /// builds lambda-expression from AST
    let buildLambdaExpressionFromAST term = 
        let rec build term = 
            match term with
            | Term.Var name -> sprintf "%s" name
            | Term.Abs (var, inner) -> sprintf "(\%s.%s)" var (build inner) 
            | Term.App (l, r) -> sprintf "(%s %s)" (build l) (build r)
        build term