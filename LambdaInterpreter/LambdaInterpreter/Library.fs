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

        let rename toRename term =
            let replacements = List.map (fun x -> x + "\'") toRename |> List.zip toRename
            let getReplacement name = List.find (fun x -> fst x = name) replacements |> snd
            let shouldBeReplaced name = List.contains name (List.map fst replacements)
            let rec doRenaming term = 
                match term with
                | Var name when shouldBeReplaced name -> getReplacement name |> Var
                | App(l, r) -> App(doRenaming l, doRenaming r)
                | Abs(var, innerTerm) when shouldBeReplaced var -> Abs(getReplacement var, doRenaming innerTerm)
                | Abs(name, innerTerm) -> Abs(name, doRenaming innerTerm)
                | _ -> term

            if toRename = [] then 
                term
            else 
                doRenaming term

        let rec replace replacedName term =
            match term with
            | Var name when name = replacedName -> substitutedTerm
            | Var _ -> term
            | Abs(v, t) -> Abs(v, replace replacedName t)
            | App(l, r) -> App(replace replacedName l, replace replacedName r)

        let absBodyVars = getVars absBody
        let substitutedTermVars = getVars substitutedTerm
        let renamingVars = List.filter (fun x -> List.contains x substitutedTermVars.Free) absBodyVars.Bounded
        let renamedTerm = rename renamingVars absBody
        replace absVarName renamedTerm

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
            