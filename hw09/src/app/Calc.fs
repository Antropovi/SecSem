module Calc

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


let calc (str : string) (value1 : double) =

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
  
  if list.Length < 1  || list.[list.Length - 1] <> Oper(6)  then list <- list @ [Num(temp)]


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


  for i in 0 .. result.Length - 1 do
        match result.[i] with
        | Num(x)  -> if x = "x" then templist <- value1 :: templist else templist <- Convert.ToDouble(x) :: templist
        | Oper(0) -> templist <- (templist.Item(1) + templist.Item(0)) :: pop(pop(templist))
        | Oper(1) -> templist <- (templist.Item(1) - templist.Item(0)) :: pop(pop(templist))
        | Oper(2) -> templist <- (templist.Item(1) * templist.Item(0)) :: pop(pop(templist))
        | Oper(3) -> templist <- (templist.Item(1) / templist.Item(0)) :: pop(pop(templist))
        | Oper(7) -> templist <- (templist.Item(1) % templist.Item(0)) :: pop(pop(templist))
        | Oper(4) -> templist <- Math.Pow (templist.Item(1), templist.Item(0)) :: pop(pop(templist))
        | _ -> ()
  templist.Head