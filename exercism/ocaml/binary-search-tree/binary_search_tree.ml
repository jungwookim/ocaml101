open Base

type bst = Empty | Node of bst * int * bst

let empty = Empty

let value t =
  match t with
  | Empty -> Result.Error "empty"
  | Node (_, v, _) -> Result.Ok v

let left t =
  match t with
  | Empty -> Result.Error "empty"
  | Node (l, _, _) -> Result.Ok l

let right t =
  match t with
  | Empty -> Result.Error "empty"
  | Node (_, _, r) -> Result.Ok r

let rec insert n t =
  match t with
  | Empty -> Node (Empty, n, Empty)
  | Node (l, v, r) -> if n = v then t
                      else if n < v then Node (insert n l, v, r)
                      else Node(l, v, insert n r)

let rec to_list t =
  match t with
  | Empty -> []
  | Node (l, v, r) -> to_list l @ [v] @ to_list r
