//Антропов Игорь 171гр
//Время выполнения 1 час + 1 ччас на ошибку из 2 скобок

module Max

open System.Threading
open Integral
open Calc

let maxInRange (arr : int []) l r =
  let mutable res = None
  for i in l .. r do
    match res with 
    | None -> res <- Some (arr.[i])
    | Some(x) -> if x < arr.[i] then res <- Some(arr.[i])
  res


let maxOnes threadNumber arraySize : int =
  let rnd = new System.Random(0)  
  let arr = Array.init arraySize (fun i -> rnd.Next(0, arraySize))
  let res = ref 0
  let step = arraySize / threadNumber
  let threadArray = Array.init threadNumber (fun i ->
      new Thread(ThreadStart(fun _ ->
          let threadRes = maxInRange arr (i * step) ((i+1) * step - 1)
          match threadRes with
          | None -> ()
          | Some(x) -> if x > res.Value then res := x
        ))
    )
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  res.Value

let duration s f = 
  let timer = new System.Diagnostics.Stopwatch()
  timer.Start()
  let returnValue = f()
  printfn "Task: %s\t\t\tElapsed Time: %i" s timer.ElapsedMilliseconds
  returnValue

// 1 ~ 5299   2 ~ 4631  4 ~ 4263  8 ~ 3951  16 ~ 4318



[<EntryPoint>]
let main argv = 
//  let temp = integral "1" 0.0 10.0 0.0001 8
//  printf "%A\t\t" temp


(*
  let timer = new System.Diagnostics.Stopwatch()
  timer.Start()
  let temp = maxOnes 8 100000000
  printfn "Max: %i\t Time; %i" temp timer.ElapsedMilliseconds
  *)

  0 // return an integer exit code
