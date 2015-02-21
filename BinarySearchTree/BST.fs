// Antropov Igor 21.02.2015
// Binary Search Tree


type tree = Nil | Node of tree * int * tree

let rec insert tree value =
  match tree with
  | Nil -> Node (Nil, value, Nil)
  | Node (l, c, r) -> 
    if c = value then Node (l, value, r) 
      elif c > value then Node (insert l value, c, r) 
        else Node (l, c, insert r value)
  
let rec printСLR l =
  match l with
  | Nil -> printf ""
  | Node (l, c, r) -> printf "%i " c; printСLR l; printСLR r

let rec printLRC l =
  match l with
  | Nil -> printf ""
  | Node (l, c, r) -> printLRC l; printLRC r; printf "%i " c

let rec printLCR l =
  match l with
  | Nil -> printf ""
  | Node (l, c, r) -> printLCR l; printf "%i " c; printLCR r

let rec right tree =
  match tree with
  | Nil -> Nil
  | Node (l, c, r) -> r

let rec left tree =
  match tree with
  | Nil -> Nil
  | Node (l, c, r) -> l

let rec center tree =
  match tree with
  | Nil -> 0
  | Node (l, c, r) -> c

let rec find_most_left tree =
  match tree with
  | Nil -> 0
  | Node (l, c, r) -> if l = Nil then c else find_most_left l

let rec move_left_subtree tree =
  match tree with
  | Nil -> Nil
  | Node (l, c, r) -> 
    if left l = left Nil then Node (right l, c, r)
                    else Node (move_left_subtree l, c, r)

let rec remove tree value =
  match tree with
  | Nil -> Nil
  | Node (l, c, r) -> 
    if c < value then Node (remove l value, c, r)
      elif c > value then Node (l, c, remove r value)
      elif c = value && l = Nil && r = Nil then Nil
        elif c = value && l <> Nil && r = Nil then l
          elif c = value && l = Nil && r <> Nil then r
            elif left r = left Nil 
              then Node (l, center r, right r)
                else Node (l, find_most_left r, move_left_subtree r)


[<EntryPoint>]
let main args =
    0 
