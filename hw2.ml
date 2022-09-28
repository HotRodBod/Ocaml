(*
Name: Rodney Wotton
Pledge: I pledge my honor that I have abided by the Stevens Honor System. - Rodney Wotton
*)

(*cannot have an empty leaf, leaves  hold values of type int, nodes  hold values of type char.  *)
type dTree = 
|Node of (char*dTree*dTree)
|Leaf of int

(* defining tLeft and tRight in order to test functions in terminal*)
let tLeft = Node('w', Node('x', Leaf(2), Leaf(5)), Leaf(8))
let tRight = Node('w', Node('x', Leaf(2), Leaf(5)), Node('y', Leaf(7), Leaf(5)))

(* adds 1 for every leaf reached. If a node is reached, the function is recursed and adds 1 for each time it reaches a node until it reaches the last leaf*)
let rec dTree_height t =
    match t with
    |Leaf(a) -> 1
    |Node(a,b,c) ->
        if (dTree_height b) > (dTree_height c)
        then 1 + dTree_height b
        else 1 + dTree_height c

(*recurses through the left and right leaves/nodes of each node and adds 1 for each leaf or node found*)
let rec dTree_size t =
    match t with
    |Leaf(a) -> 1
    |Node(a,b,c) -> (dTree_size b) + (dTree_size c) + 1

(*returns a list with the paths of all of its leaves *)
let rec dTree_paths t =
    let zero lst =
        0::lst in
    let one lst = 
        1::lst in
    match t with
    |Leaf(a)->[[]]
    |Node(a,b,c) -> (List.map zero (dTree_paths b)) @ (List.map one (dTree_paths c)) 

(*returns the booleans true or false depending if the leaves have the same depth *)
let rec dTree_is_perfect t = 
    match t with 
    |Leaf(a) -> true
    |Node(a,b,c) ->
        if (dTree_height b) != (dTree_height c)
        then false
        else (dTree_is_perfect b) && (dTree_is_perfect c)

(*Takes an int->int, g, and a char->char, f, and applies f to every node and g to every leaf. 
When a leaf is reached, a new leaf is created, and the function is then applied to the new leaf, a.*)
(*dTree_map Char.uppercase_ascii (fun x -> x+1) tLeft;;*)
let rec dTree_map f g t = 
    match t with
    |Leaf(a) -> Leaf(g a)
    |Node(a,b,c) -> Node((f a), (dTree_map f g b), (dTree_map f g c))
(*Takes a list and makes it into a tree. After recursing, and reaching an 
empty list, it creates a tail leaf to the tree with value 0. *)
let rec list_to_tree l = 
    match l with
    |[] -> Leaf(0)
    |x::t -> Node(x, list_to_tree t, list_to_tree t)
(*A leaf's path is inputed into the fuction and the number it will now be replaced with.
The helper function, goes through the path and finds the leaf, the replace_leaf_at function 
replaces that leaf with the new number.*)
let rec replace_leaf_at t f = 
    let rec leafmealone t a b = 
        match t with 
        |Leaf(a) -> Leaf(b)
        |Node(x,y,z) ->
            match a with 
            |[] -> Leaf(b)
            |w::tail when w = 0 -> Node(x, (leafmealone y tail b), z)
            |w::tail when w = 1 -> Node(x, y, leafmealone z tail b)
            |w::tail -> failwith "Item of path is not a 0 or 1"
    in match f with 
    |[] -> t
    |(a,b)::tl -> replace_leaf_at (leafmealone t a b) tl

(*a pair-encoding of a boolean function and
returns its tree-encoding *)
let bf_to_dTree (a,b) = 
    replace_leaf_at (list_to_tree a) b 
