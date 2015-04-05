// Антропов Игорь 04.04.2015
// Задания 35 и 36
open NUnit.Framework
open System.IO
open System
 
type som = Oper of int | Num of string

let prior a =
  match a with
  | Oper(1) | Oper(0) -> 2
  | Oper(2) | Oper(3) | Oper (7) -> 3
  | Oper(4) -> 4
  | Oper(5) | Oper(6) -> 1
  | _ -> 0


let pop stk =
 match stk with
 |[] -> []
 |_:: tl -> tl

let pow a b =
  let mutable res = 1
  for i in 1..b do
    res <- res*a
  res

let someToInt a =
  match a with
  | None -> 1
  | Some(x) -> x

let strToInt (a : string) (map : Map<string, int>) = 
  try 
    Convert.ToInt32(a)
  with 
    | :? FormatException -> Map.find a map


let calc (str : string) (map : Map<string, int>) =

  let mutable result = []
  let mutable stack = [Oper(9)]
  let mutable list = [] 
  let mutable temp = ""
 
  let s = str

  for i in 0..s.Length - 1 do
    match s.[i] with
    | '+' -> if s.[i-1] = ')' then list <- list @ [Oper(0)] else list <- list @ [Num(temp); Oper(0)]; temp <- ""
    | '-' -> if s.[i-1] = ')' then list <- list @ [Oper(1)] 
                              elif s.[i-1] = '(' then 
                                                  temp <- temp + "-"
                                                 else
                                                  list <- list @ [Num(temp); Oper(1)]; temp <- ""  
    | '*' -> if s.[i-1] = ')' then list <- list @ [Oper(2)] else list <- list @ [Num(temp); Oper(2)]; temp <- ""  
    | '/' -> if s.[i-1] = ')' then list <- list @ [Oper(3)] else list <- list @ [Num(temp); Oper(3)]; temp <- ""
    | '%' -> if s.[i-1] = ')' then list <- list @ [Oper(7)] else list <- list @ [Num(temp); Oper(7)]; temp <- ""  
    | '^' -> if s.[i-1] = ')' then list <- list @ [Oper(4)] else list <- list @ [Num(temp); Oper(4)]; temp <- ""  
    | '(' -> list <-            list @ [Oper(5)]; temp <- ""
    | ')' -> list <- list @ [Num(temp); Oper(6)]; temp <- ""
    |  x -> temp <- temp + x.ToString()
  if list.[list.Length - 1] <> Oper(6) then list <- list @ [Num(temp)]


  for i in 0..list.Length - 1 do
    match list.[i] with
    | Num(x)  -> result <- result @ [Num(x)]  
    | Oper(6) -> while stack.Head <> Oper(5) do 
                    result <- result @ [stack.Head]
                    stack <- pop(stack)
                 stack <- pop(stack)           
    | Oper(5) -> stack <- [Oper(5)] @ stack                 
    | Oper(x) -> while prior(Oper(x)) <= prior (stack.Head) do
                   result <- result @ [stack.Head]
                   stack <- pop(stack) 
                 stack <- [Oper(x)] @ stack 


  for i in 0..stack.Length - 1 do
    result <- result @ [stack.[i]]

  let mutable templist = []

  let mutable flag = true

  for i in 0 .. result.Length - 1 do
    if flag
      then 
        match result.[i] with
        | Num(x)  -> templist <- strToInt x map :: templist
        | Oper(0) -> templist <- (templist.Item(1) + templist.Item(0)) :: pop(pop(templist))
        | Oper(1) -> templist <- (templist.Item(1) - templist.Item(0)) :: pop(pop(templist))
        | Oper(2) -> templist <- (templist.Item(1) * templist.Item(0)) :: pop(pop(templist))
        | Oper(3) -> 
          if templist.Item(0) = 0 then
                                    flag <- false
                                  else
                                    templist <- (templist.Item(1) / templist.Item(0)) :: pop(pop(templist))
        | Oper(7) ->
          if templist.Item(0) = 0 then
                                    flag <- false
                                  else        
                                    templist <- (templist.Item(1) % templist.Item(0)) :: pop(pop(templist))
        | Oper(4) -> templist <- pow (templist.Item(1))  (templist.Item(0)) :: pop(pop(templist))
        | _ -> ()

  if flag then Some(templist.Head) else None 


[<Test>]
let `` Test 1 - Calculation `` () =
  Assert.AreEqual(Some(1013), calc "23-34+(42-26)^2*4" Map.empty) 

[<Test>]
let `` Test 2 - Calculation `` () =
  Assert.AreEqual(Some(-8), calc "2*(-4)" Map.empty)

[<Test>]
let `` Test 3 - Calculation `` () =
  Assert.AreEqual(Some(-2), calc "(-2)-(-3)*(-1)/(-4)" Map.empty)

[<Test>]
let `` Test 4 - Calculation `` () =
  Assert.AreEqual(Some(3), calc "3+4*2/(1-5)^2" Map.empty)

[<Test>]
let `` Test 5 - Calculation `` () =
  Assert.AreEqual(None, calc "5/0" Map.empty)

[<Test>]
let `` Test 6 - Calculation `` () =
  Assert.AreEqual(Some(-64), calc "(-4)^x" (Map.ofList [("x", 3)]))

[<Test>]
let `` Test 7 - Calculation `` () =
  Assert.AreEqual(Some(-1116), calc "y%x*z" (Map.ofList [("x", 24); ("y", 42); ("z", -62)]))

[<Test>]
let `` Test 8 - Calculation `` () =
  Assert.AreEqual(None, calc "x%y" (Map.ofList [("x", 24); ("y", 0)]))


[<EntryPoint>]
let main argv = 

    0 // return an integer exit code
