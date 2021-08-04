namespace LambdaInterpreter

module LambdaInterpreter =
    
    type Term = 
        | Var of string
        | Abs of string * Term
        | App of Term * Term  

    type TermVars = {Free: string list; Bounded: string list}

    /// substitutes term to absBody instead of absVarName
    let substitute substitutedTerm absVarName absBody =
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

        let renameIfNeeded term =
            let absBodyVars = getVars absBody
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

        let rec performSubstitution replacedName term =
            match term with
            | Var name when name = replacedName -> substitutedTerm
            | Var _ -> term
            | Abs(v, t) -> Abs(v, performSubstitution replacedName t)
            | App(l, r) -> App(performSubstitution replacedName l, performSubstitution replacedName r) 

        let countNodes term = 
            let rec count term = 
                match term with
                | Var _ -> 1
                | Abs (x, inner) -> 2 + (count inner)
                | App (l, r) -> 1 + (count l) + (count r)
            
            count term

        let newTerm = renameIfNeeded absBody |> performSubstitution absVarName
        
        if 1 + countNodes absBody + countNodes substitutedTerm <= countNodes newTerm then
            false, App(Abs(absVarName, absBody), substitutedTerm) 
        else 
            true, newTerm

    ///////
    let rec reduceLeft left = 
        match left with
        | App(l, r) -> 
            match reduceLeft l with
            | Abs(absVarName, absBody) -> substitute r absVarName absBody |> snd ////// 
            | _ -> l                                                     
        | _ -> left

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
            