namespace LambdaInterpreter

module LambdaInterpreter =
    
    type Term = 
        | Var of string
        | Abs of string * Term
        | App of Term * Term  

    /// substitutes term to absBody instead of absVarName
    let substitute substitutedTerm absVarName absBody =
        let rec replace replacedName term =
            match term with
            | Var name when name = replacedName -> substitutedTerm
            | Var _ -> term
            | Abs(v, t) -> Abs(v, replace replacedName t)
            | App(l, r) -> App(replace replacedName l, replace replacedName r)

        replace absVarName absBody
        
    let rec reduceLeft left = 
        match left with
        | App(l, r) -> 
            match reduceLeft l with
            | Abs(absVarName, absBody) -> substitute r absVarName absBody
            | _ -> l
        | _ -> left

    let rec reduce term =
        match term with
        | Var(name) -> term
        | Abs(var, term) -> Abs(var, reduce term)
        | App(left, right) ->
            match reduceLeft left with
            | Abs(absVarName, absBody) -> reduce <| substitute right absVarName absBody // substitute right to absBody
            | _ -> App (reduce left, reduce right)
            