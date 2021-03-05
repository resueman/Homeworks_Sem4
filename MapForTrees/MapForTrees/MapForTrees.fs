namespace MapForTrees

/// <summary>Contains map function for binary tree</summary>
module MapForTrees =
   
    /// <summary>Function that applying to each node of given binary tree</summary>
    /// <param name="tree">Tree to whose nodes the map function will be applied</param>
    /// <param name="f">Function that is applying to binary tree nodes</param>
    /// <returns>Binary tree, the result of map function applying to each node of given tree</returns>
    let rec map tree f =
        match tree with
        | Empty -> Empty
        | Node (node, left, right) -> Node(f node, map left f, map right f)
