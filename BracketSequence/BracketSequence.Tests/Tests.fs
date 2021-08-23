/// contains tests for bracket sequence balance checker
module BracketSequence.Tests

open NUnit.Framework
open BracketSequence.BracketBalanceChecker
open FsUnit

let correspond opening closing = 
    getClosing opening = Some(closing)

[<Test>]
[<Combinatorial>]
let ``Bracket balance checker for sequence without nested brackets should works correctly`` 
        ([<Values('(', '[', '{')>] opening: char, [<Values(')', ']', '}')>] closing: char, [<Values('(', '[', '{', ')', ']', '}')>] any: char) =
    
    checkBalance [any] |> should equal false
    checkBalance [any;'f'] |> should equal false
    checkBalance ['f'; any;] |> should equal false
    checkBalance [opening; closing] |> should equal (correspond opening closing)
    checkBalance [opening; closing; any] |> should equal false
    checkBalance [any; opening; closing] |> should equal false

    checkBalance [opening; any; opening; closing; opening; closing] |> should equal (correspond opening closing && closing = any)
    checkBalance [opening; closing; opening; any; opening; closing] |> should equal (correspond opening closing && closing = any)
    checkBalance [opening; closing; opening; closing; opening; any] |> should equal (correspond opening closing && closing = any)
    
    checkBalance [any; closing; opening; closing; opening; closing] |> should equal (correspond opening closing && opening = any)
    checkBalance [opening; closing; any; closing; opening; closing] |> should equal (correspond opening closing && opening = any)
    checkBalance [opening; closing; opening; closing; any; closing] |> should equal (correspond opening closing && opening = any)

    checkBalance ['{'; '}'; '{'; '}'; '('; ')'] |> should equal true
    checkBalance ['('; ')'; '{'; '}'; '['; ']'] |> should equal true
    
[<Test>]
[<Combinatorial>]
let ``Bracket balance checker for sequence with nested brackets should works correctly`` ([<Values('(', '[', '{')>] opening: char, [<Values(')', ']', '}')>] closing: char) =     
    checkBalance ['('; '{'; '['; ']'; '('; ')'; '}'; ')'] |> should equal true
    checkBalance ['('; '{'; '['; '{'; '}'; ']'; '}'; ')'] |> should equal true
    checkBalance ['('; '{'; '['; ']'; '('; '['; ']'; ')'; '}'; ')'] |> should equal true
    checkBalance ['{'; '}'; '('; '{'; '['; ']'; '('; ')'; '}'; ')'; '['; ']'] |> should equal true
    checkBalance [opening; opening; opening; closing; closing; closing] |> should equal (correspond opening closing)
    checkBalance [opening; opening; opening; closing; closing;] |> should equal false
    checkBalance [opening; opening; closing; closing; closing] |> should equal false


