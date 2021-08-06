module LambdaParser.Tests
    open NUnit.Framework

    //let expressions = ["x"; "x y z k m n l"; "(x) (y)"; "(x)"; "(x y) (z l)"; "(x (y z))"; 
    //                   "\x.x"; "\x.x y"; "\x.x y z"; "\x y.x"; "\x y z.x y z"; 
    //                   "\x y.x y z (z k) (\x y.y u x)"; "(\x.x)";] 

    //let parse exp= 
    //    printfn "Expression: %s" exp
    //    LambdaParser.LambdaParser.parse exp
    //    printf "Next\n\n\n"

    //List.iter parse expressions


    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let Test1 () =
        Assert.Pass()
