namespace MapForTrees

    /// <summary>Represents binary tree</summary>
    type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Empty
