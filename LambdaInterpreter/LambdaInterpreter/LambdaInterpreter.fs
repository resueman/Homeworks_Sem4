namespace LambdaInterpreter

/// contains types that help to describe lambda expressions and methods that allow to reduce them
module LambdaInterpreter =
    
    /// describes lambda-term
    type Term = 
        | Var of string
        | Abs of string * Term
        | App of Term * Term  

    /// describes all vars of particular term, devides them to bounded and free
    type TermVars = {Free: string list; Bounded: string list}

    /// substitutes term to absBody instead of absVarName, returns pair of term in which we substituted value and value that indicates if term has normal form
    /// if after substitution term wasn't reduced, we return false and term before substitution; otherwise we return true and term after substitution
    let substitute substitutedTerm absVarName absBody =
        /// returns clasterized to free and bounded variables names of term
        let getVars term = 
            let rec get term free bounded = 
                match term with
                | Var name -> let freeVars = List.filter (fun x -> List.contains x bounded |> not) (name::free |> List.distinct)
                              { Free = freeVars; Bounded = bounded }
                | Abs (name, innerTerm) -> get innerTerm free (name::bounded |> List.distinct)
                | App (l, r) -> let leftVars = get l free bounded
                                let rightVars = get r free bounded
                                let boundedVars = leftVars.Bounded @ rightVars.Bounded |> List.distinct
                                let freeVars = leftVars.Free @ rightVars.Free 
                                               |> List.distinct 
                                               |> List.filter (fun x -> List.contains x boundedVars |> not)
                                { Free = freeVars; Bounded = boundedVars }
            get term [] []

        /// contains clasterized to free and bounded variables names of abstraction body, where another term will be substituted
        let absBodyVars = getVars absBody

        /// renames term-destination variables that after substitution may become bounded, which will lead to incorrect reduction
        let renameIfNeeded term =
            let substitutedTermVars = getVars substitutedTerm
            let renamingVars = List.filter (fun x -> List.contains x substitutedTermVars.Free) absBodyVars.Bounded
            let replacements = List.map (fun x -> x + "\'") renamingVars |> List.zip renamingVars
            
            let getReplacement name = 
                List.find (fun x -> fst x = name) replacements |> snd
            
            let shouldBeReplaced name = 
                List.contains name (List.map fst replacements)
            
            let rec doRenaming term = 
                match term with
                | Var name when shouldBeReplaced name -> getReplacement name |> Var
                | App(l, r) -> App(doRenaming l, doRenaming r)
                | Abs(var, innerTerm) when shouldBeReplaced var -> Abs(getReplacement var, doRenaming innerTerm)
                | Abs(name, innerTerm) -> Abs(name, doRenaming innerTerm)
                | _ -> term

            if renamingVars = [] || List.contains absVarName absBodyVars.Free |> not then 
                term
            else 
                doRenaming term

        /// performs substitution of term to abstraction inner term instead of abstraction bounded variable
        let rec performSubstitution replacedName term =
            if List.contains absVarName absBodyVars.Free |> not then
                term
            else
                match term with
                | Var name when name = replacedName -> substitutedTerm
                | Var _ -> term
                | Abs(v, t) -> Abs(v, performSubstitution replacedName t)
                | App(l, r) -> App(performSubstitution replacedName l, performSubstitution replacedName r) 

        /// counts term nodes
        let countNodes term = 
            let rec count term = 
                match term with
                | Var _ -> 1
                | Abs (x, inner) -> 2 + (count inner)
                | App (l, r) -> 1 + (count l) + (count r)
            
            count term

        let newAbsBody = renameIfNeeded absBody |> performSubstitution absVarName        
        if 1 + countNodes absBody + countNodes substitutedTerm <= countNodes newAbsBody then
            false, App(Abs(absVarName, absBody), substitutedTerm) 
        else 
            true, newAbsBody
    
    /// reduces leftmost term
    let rec reduceLeft left = 
        match left with
        | App(l, r) -> 
            match reduceLeft l with
            | Abs(absVarName, absBody) -> substitute r absVarName absBody |> snd
            | _ -> l                                                     
        | _ -> left
    
    /// reduses term to normal form, if it exists, using normal strategy; otherwise reduses to normal form those parts of term that have it
    let rec reduce term =
        match term with
        | Var _ -> term
        | Abs(var, term) -> Abs(var, reduce term)
        | App(left, right) ->
            match reduceLeft left with              
            | Abs(absVarName, absBody) -> let result = substitute right absVarName absBody
                                          match fst result with
                                          | true -> snd result |> reduce
                                          | false -> snd result
            | _ -> App (reduce left, reduce right)
            