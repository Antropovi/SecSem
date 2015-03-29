
//#20
type IGraph<'A> =
  interface 
    abstract IsPassable : 'A -> 'A -> bool
    abstract GetSize : unit -> int
    abstract GetVals : unit -> 'A array
  end

//#21
type GraphMatrix<'A when 'A : equality> (values : 'A array, edges : bool [,]) = 
  class
    let size = values.Length
    let vert = values 
    interface IGraph<'A> with
      member this.GetSize () = size
      member this.GetVals () = vert
      member this.IsPassable a b = 
        let temp1 = Array.findIndex (fun x -> x = a) values
        let temp2 = Array.findIndex (fun x -> x = b) values
        edges.[temp1,temp2]
  end

//#22
type ListGraph<'A when 'A : equality>(values : 'A array, edges : list<'A> array) = 
  class
    let size = values.Length
    let vert = values
    interface IGraph<'A> with
      member this.GetVals () = vert
      member this.GetSize () = size
      member this.IsPassable a b = 
        let temp1 = Array.findIndex (fun x -> x = a) values
        List.exists (fun x -> x = b) edges.[temp1]
  end


let rec dfs (graph : IGraph<'A>) (visited : bool array) value dir = 
  if dir = "In" then 
    let index = Array.findIndex (fun x -> x = value) (graph.GetVals ())
    for i in 0 .. (graph.GetSize ()) - 1 do
      if not visited.[i] && graph.IsPassable value (graph.GetVals ()).[i]  
        then visited.[i] <- true 
             dfs graph visited (graph.GetVals ()).[i] dir
  else 
    let index = Array.findIndex (fun x -> x = value) (graph.GetVals ())
    for i in 0 .. (graph.GetSize ()) - 1 do
      if not visited.[i] && graph.IsPassable (graph.GetVals ()).[i] value 
        then visited.[i] <- true 
             dfs graph visited (graph.GetVals ()).[i] dir                       
  visited |> ignore


let visitedToVals (visited : bool array) (values : 'A array) =
  let mutable result = []
  for i in 0 .. visited.Length - 1 do 
    if visited.[i] then result <- List.append result [values.[i]]
  result

//#23
let passableTo (graph : IGraph<'A>) value = 
  let mutable result = []
  let size = graph.GetSize()
  let visited = Array.create size false
  dfs graph visited value "In"
  visitedToVals visited (graph.GetVals())


//#24
let passableFrom (graph : IGraph<'A>) value = 
  let mutable result = []
  let size = graph.GetSize()
  let visited = Array.create size false
  dfs graph visited value "Out"
  visitedToVals visited (graph.GetVals())



//#25
type ILabelGraph<'A, 'B> =
  interface 
    inherit  IGraph<'A>
    abstract GetLabels : unit -> 'B array
  end



[<EntryPoint>]
let main argv = 
    0 // return an integer exit code
