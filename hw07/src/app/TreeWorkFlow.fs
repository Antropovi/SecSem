//Антропов Игорь 171рг
//Время выполнения 3 часа

module TreeWorkFlow

open NUnit.Framework



type Tree<'A> = Nil | Node of Tree<'A> * 'A * Tree<'A>


type TreeBilder<'A, 'B>(f: 'A-> 'B -> 'A , value) =

    member this.Insert tree value = 
      let rec insert tree value = 
       match tree with           
       | Nil -> Node (Nil, value, Nil)
       | Node (l, c, r) -> 
          if c = value then Node (l, value, r) 
            elif c > value then Node (insert l value, c, r) 
              else Node (l, c, insert r value)
      insert tree value
            

    member this.Bind (x, rest) = 
      match x with
      | Nil -> value
      | Node(l, c, r) -> 
          let c = f value c
          rest (rest c l) r


    member this.For(x, f) =
      let mutable tree = Nil
      for i in x do 
        tree <- this.Insert tree i
      f tree

    member this.Delay(f) = f()


    member this.Combine (a, b) =
      let mutable tree = b
      let rec Fold f a tree =  
       match tree with
       | Nil   -> a
       | Node (l, c, r) -> 
         let c = f a c 
         Fold f (Fold f c l) r
      tree <- Fold this.Insert tree a 
      tree

    member this.Return x = x


let rec Fold f value tree =
  TreeBilder(f, value){
    let! temp = tree
    return Fold f temp  
  }


let insert tree value = 
      let rec insert tree value = 
       match tree with           
       | Nil -> Node (Nil, value, Nil)
       | Node (l, c, r) -> 
          if c = value then Node (l, value, r) 
            elif c > value then Node (insert l value, c, r) 
              else Node (l, c, insert r value)
      insert tree value
      
       
let rec Filter f value tree =
  TreeBilder((fun t arg -> if (f arg) then insert t arg else t), value){
        let! temp = tree
        return Filter f tree
    }


let rec Map f value tree =
  TreeBilder((fun t arg -> insert t (f arg)), value){
        let! temp = tree
        return Map f tree
    }