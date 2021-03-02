namespace ArithmeticParseTree

module ParseTree =
    open System

    let rec evaluate (n: Expression) =
        match n with
        | Number (value) -> value
        | Addition (left, right) -> evaluate left + evaluate right
        | Subtraction (left, right) -> evaluate left - evaluate right 
        | Multiplication (left, right) -> evaluate left * evaluate right
        | Division (_, right) when evaluate right = 0 -> raise (new DivideByZeroException("Division by zero"))
        | Division (left, right) -> evaluate left / evaluate right
