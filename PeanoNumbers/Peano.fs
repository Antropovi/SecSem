//Antropov Igor 21.02.2015
//Peano numbers

type Peano = Zero | S of Peano

let Suc p = S p

let minus1 p = 
 match p with
 | Zero -> Zero
 | S p -> p

let rec plus a b =
 match a with
 | Zero -> b
 | S a -> S (plus a b)

let rec minus a b =
  match a,b with
  | Zero, _ -> Zero
  | S a, Zero -> S a
  | S a, S b -> minus a b

let rec mult a b =
  match a with
  | Zero -> Zero
  | S a -> plus b (mult a b)

let rec pow a b = 
  match a, b with 
  | _ , Zero -> S(Zero)
  | Zero, S b -> Zero
  | S a, S b -> mult (S a) (pow (S a) b)

let rec toInt p = 
  match p with 
  | Zero -> 0
  | S p -> toInt (p) + 1 

let intToString p =
  p.ToString ()

let print p = p |> toInt
                |> intToString
                |> printf "%s/n"

[<EntryPoint>]
let main argv = 
  0