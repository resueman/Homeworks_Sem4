module AST 
    
    /// describes lambda-term
    type Term = 
        | Var of string
        | Abs of string * Term
        | App of Term * Term  
