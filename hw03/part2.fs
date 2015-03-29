

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
  end


  type List<'A> = Null | Node of 'A * List<'A> 


  type ADTList<'A when 'A : equality> (L: List<'A>) =
    class
      let mutable list = L 

      interface IList<'A> with 
        member this.AddHead elem = list <- Node (elem, list)

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
            | Null -> failwith "Нет такого элемента"
            | Node (a, b) -> if (f a) then Some a else func b
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
              match l.Head with               
              | Some x -> l.DellHead(); Node (x, func list)
              | None -> Null
          list <- func list
    end






  type ArrayList<'A when 'A : equality> (A : 'A[]) =
    class
      let mutable array = A

      interface IList<'A> with 
        member this.AddHead elem = array <- Array.append [|elem|] array

        member this.AddTail elem = array <- Array.append array [|elem|]

        member this.AddByNum elem num =          
         array <- Array.append 
                   (Array.append (Array.sub array 0 num) [|elem|])
                   (Array.sub array num (array.Length - num + 1) )

        member this.DellHead () = array <- Array.sub array 1 (array.Length - 1)
        member this.DellTail () = array <- Array.sub array 0 (array.Length - 1)

        member this.DellByNum num = 
          array <- Array.append 
                   (Array.sub array 0 (num - 1)) 
                   (Array.sub array (num + 1) (array.Length - num))

        member this.FindByVal func = Array.tryFind func array

        member this.Head = Some array.[0]

        member this.Append l = 
          let rec func () =
            match l.Head with               
            | Some x -> array <- Array.append array [|x|]; l.DellHead(); func() 
            | None -> ()
          func ()
    end



[<EntryPoint>]
let main argv =
    0