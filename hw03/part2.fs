//Антропов Игорь 171гр
//Задания 27, 28, 29, 33, 34


open NUnit.Framework


type IList<'A when 'A : equality> =
  interface 
    abstract AddHead   : 'A -> unit
    abstract AddTail   : 'A -> unit
    abstract AddByNum  : 'A -> int -> unit
    abstract DellHead  : unit -> unit
    abstract DellTail  : unit -> unit
    abstract DellByNum : int -> unit
    abstract FindByVal : ('A -> bool) -> Option<'A>
    abstract Head      : Option<'A> 
    abstract Append    : IList<'A> -> unit
    abstract IsNotEmpty   : unit -> bool
  end


  type List<'A> = Null | Node of 'A * List<'A> 


  type ADTList<'A when 'A : equality> (L: List<'A>) =
    class
      let mutable list = L 

      interface IList<'A> with 
        member this.AddHead elem = list <- Node (elem, list)

        member this.IsNotEmpty () =  list <> Null


        member this.AddTail elem = 
          let rec pushTo list =
            match list with
            | Null -> Node (elem, Null)
            | Node (a, b) -> Node (a,pushTo b)
          list <- pushTo list

        member this.AddByNum elem num = 
          let rec func list count = 
            match list with 
            | Null -> Node (elem, Null)
            | Node (a, b) -> 
              if count = num then Node (elem, list) else Node (a, func b (count+1) )
          list <- func list 0

        member this.DellHead () = 
          match list with
          | Null -> ()
          | Node (a,b) -> list <- b 

        member this.DellTail () = 
          let rec func list =
            match list with
            | Null -> Null
            | Node (a, Node( _ , Null)) -> Node (a, Null)
            | Node (a, b) -> Node (a, func b)
          list <- func list

        member this.DellByNum num = 
          let rec func list count =
            match list with
            | Null -> Null
            | Node (a, b) -> if count = num then b else Node (a, func b (count+1)) 
          list <- func list 0

        member this.FindByVal f = 
          let rec func list = 
            match list with
            | Node (a, b) -> if (f a) then Some a else func b
            | Null -> None
          func list

        member this.Head = 
          match list with
          | Null -> None
          | Node (a, _) -> Some a


        member this.Append (l : IList<'A>) = 
          let rec func list = 
            match list with 
            | Node (a, b) -> Node(a, func b)
            | Null ->
              if l.IsNotEmpty() then
               match l.Head with               
                | Some x -> l.DellHead(); Node (x, func list)
                | None -> Null
              else Null
          list <- func list

      override this.ToString() =
        let rec func list =
          match list with
          | Null -> "]"
          | Node (a, b) -> a.ToString() + "; " + func b
        "[" + func list
    end






  type ArrayList<'A when 'A : equality> (A : 'A[]) =
    class
      let mutable array = A

      interface IList<'A> with 

        member this.IsNotEmpty () = array.Length <> 0 

        member this.AddHead elem = array <- Array.append [|elem|] array

        member this.AddTail elem = array <- Array.append array [|elem|]

        member this.AddByNum elem num =  
          if array.Length = num - 1 then array <- Array.append array [|elem|]
          else        
            array <- Array.append 
                     (Array.append (Array.sub array 0 num) [|elem|])
                     (Array.sub array num (array.Length - num) )

        member this.DellHead () = array <- Array.sub array 1 (array.Length - 1)

        member this.DellTail () = array <- Array.sub array 0 (array.Length - 1)

        member this.DellByNum num = 
          array <- Array.append 
                   (Array.sub array 0 num ) 
                   (Array.sub array (num + 1) (array.Length - num - 1))

        member this.FindByVal func = Array.tryFind func array

        member this.Head = Some array.[0]

        member this.Append l = 
          let rec func () =
            if l.IsNotEmpty() then
             match l.Head with               
              | Some x -> array <- Array.append array [|x|]; l.DellHead(); func() 
              | None -> ()
            else ()
          func ()

      override this.ToString() =
        let mutable result = "["
        for i in array do 
          result <- result + i.ToString() + "; "
        result <- result + "]"
        result
    end

//Проверка реализации списка абстрактного типа данных

let list = new ADTList<string> (Null)  :> IList<string>

[<TestCase("c", Result = "[c; ]")>]
[<TestCase("b", Result = "[b; c; ]")>]
[<TestCase("a", Result = "[a; b; c; ]")>]
let ``Test 01 - ADTList: Add to Head`` elem =
    list.AddHead(elem)
    list.ToString()

[<TestCase("d", Result = "[a; b; c; d; ]")>]
[<TestCase("e", Result = "[a; b; c; d; e; ]")>]
let ``Test 02 - ADTList: Add to Tail`` elem = 
  list.AddTail(elem) 
  list.ToString()


[<TestCase("z", 0, Result = "[z; a; b; c; d; e; ]")>]
[<TestCase("f", 3, Result = "[z; a; b; f; c; d; e; ]")>]
[<TestCase("q", 8, Result = "[z; a; b; f; c; d; e; q; ]")>]
let ``Test 03 - ADTList: Add by num`` (elem : string) (num : int) = 
    list.AddByNum elem num
    list.ToString()

[<TestCase(Result = "[a; b; f; c; d; e; q; ]")>]
let ``Test 04 - ADTList: Delete Head`` () =
    list.DellHead()
    list.ToString()

[<TestCase(Result = "[a; b; f; c; d; e; ]")>]
let ``Test 05 - ADTList: Delete Tail`` () =
    list.DellTail()
    list.ToString()

[<TestCase(2, Result = "[a; b; c; d; e; ]")>]
let ``Test 06 - ADTList: Delete By num`` num =
    list.DellByNum(num)
    list.ToString()

      
let list1 = new ADTList<string> (Null)  :> IList<string>
list1.AddHead("z")
list1.AddHead("y")
list1.AddHead("x")

[<Test>]
let `` Test 07 - ADTList: Find by val`` () =
    Assert.AreEqual(Some "y", list1.FindByVal(fun x -> x = "y" ))
    Assert.AreEqual(Some "z", list1.FindByVal(fun x -> x > "y"))
    Assert.AreEqual(None, list1.FindByVal(fun x -> x = "a"))


[<Test>]
let ``Test 08 - ADTList: Append 2 lists`` () =
  list.Append list1
  Assert.AreEqual("[a; b; c; d; e; x; y; z; ]", list.ToString())
  let listArr = new ArrayList<string> ([||])  :> IList<string>
  listArr.AddHead("3")
  listArr.AddHead("2")
  listArr.AddHead("1")
  list.Append listArr
  Assert.AreEqual("[a; b; c; d; e; x; y; z; 1; 2; 3; ]", list.ToString())


//Проверка реализации списка абстрактного типа данных

let array = new ArrayList<string> ([||])  :> IList<string>

[<TestCase("c", Result = "[c; ]")>]
[<TestCase("b", Result = "[b; c; ]")>]
[<TestCase("a", Result = "[a; b; c; ]")>]
let ``Test 01 - ArrayList: Add to Head`` elem =
  array.AddHead(elem)
  array.ToString()

[<TestCase("d", Result = "[a; b; c; d; ]")>]
[<TestCase("e", Result = "[a; b; c; d; e; ]")>]
let ``Test 02 - ArrayList: Add to Tail`` elem = 
  array.AddTail(elem) 
  array.ToString()

[<TestCase("z", 0, Result = "[z; a; b; c; d; e; ]")>]
[<TestCase("f", 3, Result = "[z; a; b; f; c; d; e; ]")>]
[<TestCase("q", 8, Result = "[z; a; b; f; c; d; e; q; ]")>]
let ``Test 03 - ArrayList: Add by num`` (elem : string) (num : int) = 
  array.AddByNum elem num
  array.ToString()

[<TestCase(Result = "[a; b; f; c; d; e; q; ]")>]
let ``Test 04 - ArrayList: Delete Head`` () =
  array.DellHead()
  array.ToString()

[<TestCase(Result = "[a; b; f; c; d; e; ]")>]
let ``Test 05 - ArrayList: Delete Tail`` () =
  array.DellTail()
  array.ToString()

[<TestCase(2, Result = "[a; b; c; d; e; ]")>]
[<TestCase(0, Result = "[b; c; d; e; ]")>]
[<TestCase(3, Result = "[b; c; d; ]")>]
let ``Test 06 - ArrayList: Delete By num`` num =
  array.DellByNum(num)
  array.ToString()

      
let array1 = new ArrayList<string> ([||])  :> IList<string>
array1.AddHead("z")
array1.AddHead("y")
array1.AddHead("x")

[<Test>]
let `` Test 07 - ArrayList: Find by val`` () =
    Assert.AreEqual(Some "y", array1.FindByVal(fun x -> x = "y" ))
    Assert.AreEqual(Some "z", array1.FindByVal(fun x -> x > "y"))
    Assert.AreEqual(None, array1.FindByVal(fun x -> x = "a"))


[<Test>]
let ``Test 08 - ArrayList: Append 2 lists`` () =
  array.Append array1
  Assert.AreEqual("[b; c; d; x; y; z; ]", array.ToString())
  let listADT = new ArrayList<string> ([||])  :> IList<string>
  listADT.AddHead("3")
  listADT.AddHead("2")
  listADT.AddHead("1")
  array.Append listADT
  Assert.AreEqual("[b; c; d; x; y; z; 1; 2; 3; ]", array.ToString())



[<EntryPoint>]
let main argv =
    0