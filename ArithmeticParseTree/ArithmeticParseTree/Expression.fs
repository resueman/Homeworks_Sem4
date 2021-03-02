namespace ArithmeticParseTree

type Expression = 
    | Number of int
    | Addition of Expression * Expression
    | Subtraction of Expression * Expression
    | Multiplication of Expression * Expression
    | Division of Expression * Expression
