module ReverseList

let rec reverse sequence = Seq.fold (fun reveresed x -> x :: reveresed) [] sequence

