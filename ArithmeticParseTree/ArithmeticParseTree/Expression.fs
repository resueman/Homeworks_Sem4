namespace ArithmeticParseTree

    /// <summary>
    /// Represents arithmetic parse tree
    /// </summary>
    type Expression = 
        | Number of int
        | Addition of Expression * Expression
        | Subtraction of Expression * Expression
        | Multiplication of Expression * Expression
        | Division of Expression * Expression
