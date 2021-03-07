namespace LambdaInterpreter

module LambdaInterpreter =
    
    type Variable = 
        | Var of string
    and LambdaTerm = 
        | Variable of Variable
        | LambdaAbstarction of Variable * LambdaTerm
        | Applique of LambdaTerm * LambdaTerm 
 
    let substitute leftTerm rightTerm = 
        leftTerm // 

    let rec reduce term =
        match term with
        | Variable _ -> term
        | LambdaAbstarction (variable, term) -> LambdaAbstarction(variable, reduce term)
        | Applique (left, right) ->
            match left with
            | LambdaAbstarction (_, _) -> substitute left right |> reduce
            | _ -> Applique(reduce left, reduce right)

    //  ((λa.(λb.b b) (λb.b b)) b) ((λc.(c b)) (λa.a))

    //let term = Applique(
    //               Applique(   
    //                   Applique(
    //                       LambdaAbstarction(Var("a"), LambdaAbstarction(Var("b"), Applique(Variable(Var("b")), Variable(Var("b"))))), 
    //                       LambdaAbstarction(Var("b"), Applique(Variable(Var("b")), Variable(Var("b"))))), 
    //                   Variable(Var("b"))),
    //               Applique(
    //                   LambdaAbstarction(Var("c"), Applique(Variable(Var("c")), Variable(Var("b")))), 
    //                   LambdaAbstarction(Var("a"), Variable(Var("a")))))

    //let reduced = reduce term