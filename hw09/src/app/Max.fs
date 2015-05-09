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


let maxOnes threadNumber (array : int []) =
//  let rnd = new System.Random(0)  
//  let arr = Array.init arraySize (fun i -> rnd.Next(0, arraySize))
  let res = ref 0
  let step = array.Length / threadNumber
  let threadArray = Array.init threadNumber (fun i ->
      new Thread(ThreadStart(fun _ ->
          let threadRes = maxInRange array (i * step) ((i+1) * step - 1)
          match threadRes with
          | None -> ()
          | Some(x) -> if x > res.Value then res := x
        ))
    )
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  if (threadNumber * step - 1) < array.Length - 1 then
    let temp = maxInRange array (threadNumber * step - 1) (array.Length - 1)
    match temp with
     | None -> ()
     | Some(x) -> if x > res.Value then res := x
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
(*
  let timer = new System.Diagnostics.Stopwatch()
  timer.Start()
  let temp = integral "x" 0.0 10.0 0.0001 1                        // 50.001    131
  printfn "Answ: %A\t Time; %i" temp timer.ElapsedMilliseconds
  timer.Reset()
  timer.Start()
  let temp = integral "x" 0.0 10.0 0.0001 2                        // 50.001  46
  printfn "Answ: %A\t Time; %i" temp timer.ElapsedMilliseconds
  timer.Reset()
  timer.Start()
  let temp = integral "x" 0.0 10.0 0.0001 4                        // 50.00175 41
  printfn "Answ: %A\t Time; %i" temp timer.ElapsedMilliseconds
  timer.Reset()
  timer.Start()
  let temp = integral "x" 0.0 10.0 0.0001 8                        // 50.003875 43
  printfn "Answ: %A\t Time; %i" temp timer.ElapsedMilliseconds
  timer.Reset()
  timer.Start()
  let temp = integral "x" 0.0 10.0 0.0001 9                        // 50.00444444   47
  printfn "Answ: %A\t Time; %i" temp timer.ElapsedMilliseconds
  timer.Reset()
  timer.Start()
  let temp = integral "1" 0.0 10.0 0.0001 1                        // 10.0001  141
  printfn "Answ: %A\t Time; %i" temp timer.ElapsedMilliseconds
  timer.Reset()

  let rnd = new System.Random(0)  
  let arr = Array.init arraySize (fun i -> rnd.Next(0, arraySize))
  let timer = new System.Diagnostics.Stopwatch()
  timer.Start()

  let temp = maxOnes 8 arr 
  printfn "Max: %i\t Time; %i" temp timer.ElapsedMilliseconds
  *)

  0 // return an integer exit code
