open System.IO

// Prints to file sides of square with specified length, that consist of "*" 
let printSquare n =
    let rec printSquareRecursive n row column (writer: StreamWriter) =
        match (row, column) with
        | _ when column = n -> 
            writer.WriteLine ""
        | _ when row = n ->
            writer.WriteLine ""
            printSquareRecursive n 0 (column + 1) writer 
        | _ when row = 0 || row = n - 1 || column = 0 || column = n - 1 ->
            writer.Write "*"
            printSquareRecursive n (row + 1) column writer
        | _ -> 
            writer.Write " "
            printSquareRecursive n (row + 1) column writer

    let path = $"./{n}.txt"
    use writer = new StreamWriter (path)  
    if n <= 0 then
        writer.Write("Length of square side must be positive number")
    else
        printSquareRecursive n 0 0 writer

[<EntryPoint>]
let main argv =
    printSquare 5
    0