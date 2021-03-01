namespace ArithmeticParseTree

module ArithmeticParseTree =
    open System

    type Node = 
        | Operand of int
        | Addition of Node * Node
        | Subtraction of Node * Node
        | Multiplication of Node * Node
        | Division of Node * Node

    let rec eval (n: Node) =
        match n with
        | Operand(value) -> value
        | Addition(left, right) -> eval left + eval right
        | Subtraction(left, right) -> eval left - eval right 
        | Multiplication(left, right) -> eval left * eval right
        | Division (_, right) when eval right = 0 -> raise (new DivideByZeroException("Division by zero"))
        | Division (left, right) -> eval left / eval right
       
    let tree = Subtraction(Addition(Operand(5), Operand(7)), Division(Operand(6), Operand(0))) // 10
