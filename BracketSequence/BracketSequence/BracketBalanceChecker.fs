namespace BracketSequence

/// checks if bracket sequence is correct
module BracketBalanceChecker =
    let getClosing bracket = 
        match bracket with
        | '(' -> Some(')')
        | '[' -> Some(']')
        | '{' -> Some('}')
        | _ -> None

    let checkBalance brakets = 
        let rec check bracketSeq expectedClosingSeq = 
            match bracketSeq with
            | [] -> List.isEmpty expectedClosingSeq
            | _ ->
                    let braket = bracketSeq.Head
                    let expectedClosing = List.tryHead expectedClosingSeq
                    match getClosing braket with
                    | Some(closing) -> closing::expectedClosingSeq |> check bracketSeq.Tail 
                    | None when Some(braket) = expectedClosing -> check bracketSeq.Tail expectedClosingSeq.Tail
                    | _ -> false
        check brakets []
