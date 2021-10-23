namespace BinaryTree

open System.Collections
open System.Collections.Generic

/// Contains implemetation of binary tree
module BinaryTree =
   
    /// Describes tree
    type Tree = 
        | Node of int * Tree * Tree
        | Empty

    /// Binary tree implementation
    type BinaryTree () =
        let mutable tree = Empty

        /// Adds value to binary tree if it doesn't exist
        member this.Add valueToAdd =
            let rec addToSubtree subtree =
                match subtree with
                | Empty -> Node(valueToAdd, Empty, Empty)
                | Node(rootValue, left, right) -> 
                    if rootValue = valueToAdd then
                        Node(rootValue, left, right)
                    elif valueToAdd > rootValue then 
                        Node(rootValue, left, addToSubtree right)
                    else
                        Node(rootValue, addToSubtree left, right)
            
            tree <- addToSubtree tree
        
        /// Checks if binary tree contains value. 
        /// Returns true if binary tree contains value; otherwise returns false 
        member this.Contains valueToSearch =
            let rec containsInSubtree subtree =
                match subtree with 
                | Empty -> false
                | Node(root, left, right) -> 
                    if valueToSearch = root then 
                        true
                    elif valueToSearch > root then
                        containsInSubtree right
                    else 
                        containsInSubtree left
            
            containsInSubtree tree

        /// Removes value from binary tree, if this value exists
        member this.Remove (valueToRemove: int) = 
            let rec removeFromSubtree (subtree: Tree) =
                match subtree with
                | Node(rootValue, left, right) when rootValue <> valueToRemove ->
                    if valueToRemove > rootValue then 
                        Node(rootValue, left, removeFromSubtree right) 
                    else 
                        Node(rootValue, removeFromSubtree left, right)
                | Node(_, Empty, Empty) -> Empty
                | Node(_, left, Empty) -> left
                | Node(_, Empty, right) -> right
                | Node(_, _, right) -> 
                    match this.FindAndRemoveMinFromTree right with
                    | Some (min, newRight) -> Node(min, newRight, right)
                    | None -> Empty
                | _ -> Empty
            
            tree <- removeFromSubtree tree

        /// Finds and removes min value from a tree
        member private this.FindAndRemoveMinFromTree tree =
            let rec findMin subtree =
                match subtree with
                | Node(root, Empty, _) -> Some root
                | Node(_, left, _) -> findMin left 
                | Empty -> None

            let rec removeMin subtree =
                match subtree with
                | Node(_, Empty, right) -> right
                | Node(value, left, right) -> Node(value, removeMin left, right)
                | Empty -> Empty

            let min = findMin tree
            match min with
            | None -> None
            | Some value -> 
                let treeWithoutMin = removeMin tree
                (value, treeWithoutMin) |> Some

        interface IEnumerable with 
            /// Returns enumerator
            override this.GetEnumerator () =
                let rec accumulateTreeValues subtree (acc : List<int>) =
                    match subtree with
                    | Node(value, left, right) -> 
                        accumulateTreeValues left acc
                        acc.Add(value)
                        accumulateTreeValues right acc
                    | Empty -> ()

                let values = List<int>()
                accumulateTreeValues tree values
                values.GetEnumerator() :> IEnumerator 
