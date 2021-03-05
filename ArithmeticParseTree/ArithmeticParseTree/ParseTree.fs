namespace ArithmeticParseTree

/// <summary>
/// Contains method for evaluating arithmetic parse tree
/// </summary>
module ParseTree =
    open System
    
    /// <summary>Сalculates the value of an expression represented as binary tree</summary>
    /// <param name="expression">Arithmetic expression represented as binary tree</param>
    /// <returns>Result of tree processing, equal to the value of the expression</returns>
    let rec evaluate expression =
        match expression with
        | Number (value) -> value
        | Addition (left, right) -> evaluate left + evaluate right
        | Subtraction (left, right) -> evaluate left - evaluate right 
        | Multiplication (left, right) -> evaluate left * evaluate right
        | Division (_, right) when evaluate right = 0 -> raise (new DivideByZeroException("Division by zero"))
        | Division (left, right) -> evaluate left / evaluate right
