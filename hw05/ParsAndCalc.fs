// Антропов Игорь
// Задания 37 и 38

open NUnit.Framework
open System.IO
open System
 
type som = Oper of int | Num of int

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


let parsing () =

  use f = new StreamReader("Input.txt")
  let mutable result = []
  let mutable stack = [Oper(9)]
  let mutable list = [] 
  let mutable temp = 0
 
  let mutable neg = 1
  let s = f.ReadToEnd()

  for i in 0..s.Length - 1 do
    match s.[i] with
    | '+' -> if s.[i-1] = ')' then list <- list @ [Oper(0)] 
                              else list <- list @ [Num(neg*temp); Oper(0)]; temp <- 0; neg <- 1
    | '-' -> if s.[i-1] = ')' then list <- list @ [Oper(1)] 
                              elif s.[i-1] = '(' then 
                                                  neg <- -1
                                                 else
                                                  list <- list @ [Num(neg*temp); Oper(1)]; temp <- 0; neg <- 1
    | '*' -> if s.[i-1] = ')' then list <- list @ [Oper(2)] 
                              else list <- list @ [Num(neg*temp); Oper(2)]; temp <- 0; neg <- 1  
    | '/' -> if s.[i-1] = ')' then list <- list @ [Oper(3)] 
                              else list <- list @ [Num(neg*temp); Oper(3)]; temp <- 0; neg <- 1
    | '%' -> if s.[i-1] = ')' then list <- list @ [Oper(7)] 
                              else list <- list @ [Num(neg*temp); Oper(7)]; temp <- 0; neg <- 1  
    | '^' -> if s.[i-1] = ')' then list <- list @ [Oper(4)] 
                              else list <- list @ [Num(neg*temp); Oper(4)]; temp <- 0; neg <- 1    
    | '(' -> list <-            list @ [Oper(5)]; temp <- 0; neg <- 1
    | ')' -> list <- list @ [Num(neg*temp); Oper(6)]; temp <- 0; neg <- 1
    | _ -> temp <- temp*10 + Convert.ToInt32(s.[i].ToString())
  if list.[list.Length - 1] <> Oper(6) then list <- list @ [Num(neg*temp)]


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

  use t = new StreamWriter("Result1.txt")

  for i in 0 .. result.Length - 1 do
    match result.[i] with
    | Num(x)  -> t.WriteLine(x)
    | Oper(0) -> t.WriteLine("+")
    | Oper(1) -> t.WriteLine("-")
    | Oper(2) -> t.WriteLine("*")
    | Oper(3) -> t.WriteLine("/")
    | Oper(7) -> t.WriteLine("%")
    | Oper(4) -> t.WriteLine("^")
    | _ -> ()


let pow a b =
  let mutable res = 1
  for i in 1..b do
    res <- res*a
  res

let calculation () = 
  let mutable list : int list = []
  use f = new StreamReader("Result1.txt")
  use t = new StreamWriter("Result2.txt")
  let mutable str = f.ReadLine()
  while str <> null do
    match str with 
    | "+" -> list <- (list.Item(1) + list.Item(0)) :: pop(pop(list))
    | "-" -> list <- (list.Item(1) - list.Item(0)) :: pop(pop(list))
    | "*" -> list <- (list.Item(1) * list.Item(0)) :: pop(pop(list))
    | "/" -> list <- (list.Item(1) / list.Item(0)) :: pop(pop(list))
    | "%" -> list <- (list.Item(1) % list.Item(0)) :: pop(pop(list))
    | "^" -> list <- pow (list.Item(1))  (list.Item(0)) :: pop(pop(list)) 
    | " " -> ()
    |  _  -> 
      let value = Convert.ToInt32(str)
      list <- value :: list
    str <- f.ReadLine()

  t.WriteLine(list.Head)



[<Test>]
let ``Test1 - parsing and calculations`` () = 
  let t = new StreamWriter("Input.txt")
  t.Write("5-2+(4-1*4)^0") 
  t.Dispose()
  parsing()
  calculation()
  use temp1 = new StreamReader("Result1.txt")
  use temp2 = new StreamReader("Result2.txt")
  Assert.AreEqual("4", temp2.ReadLine())
  Assert.AreEqual("5\r\n2\r\n-\r\n4\r\n1\r\n4\r\n*\r\n-\r\n0\r\n^\r\n+\r\n", temp1.ReadToEnd())


[<Test>]
let ``Test2 - parsing and calculations`` () = 
  let t = new StreamWriter("Input.txt")
  t.Write("(-2)-(-3)*(-1)/(-4)") 
  t.Dispose()
  parsing()
  calculation()
  use temp1 = new StreamReader("Result1.txt")
  use temp2 = new StreamReader("Result2.txt")
  Assert.AreEqual("-2", temp2.ReadLine())
  Assert.AreEqual("-2\r\n-3\r\n-1\r\n*\r\n-4\r\n/\r\n-\r\n", temp1.ReadToEnd())


[<Test>]
let ``Test3 - parsing and calculations`` () = 
  let t = new StreamWriter("Input.txt")
  t.Write("23-34+(42-26)^2*4") 
  t.Dispose()
  parsing()
  calculation()
  use temp1 = new StreamReader("Result1.txt")
  use temp2 = new StreamReader("Result2.txt")
  Assert.AreEqual("1013", temp2.ReadLine())
  Assert.AreEqual("23\r\n34\r\n-\r\n42\r\n26\r\n-\r\n2\r\n^\r\n4\r\n*\r\n+\r\n", temp1.ReadToEnd())


[<EntryPoint>]
let main argv = 
    0 
