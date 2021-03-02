namespace MapForTrees

module MapForTrees =

    type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Empty
   
    let rec map tree f =
        match tree with
        | Empty -> Empty
        | Node (node, left, right) -> Node(f node, map left f, map right f)
