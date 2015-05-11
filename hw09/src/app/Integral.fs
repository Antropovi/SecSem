module Integral

open Calc
open System.Threading
open System

let intInRange func from _to step =
  let mutable res : double = 0.0
  let mutable i = from
  while i < _to do
    if i+step < _to then
      res <- res + (((calc func i) + (calc func (i+step)))* step / 2.0) 
  //  res <- res + (((calc func i) + (calc func i+step))* step / 2.0) - 2 скобки - минус 1 час
                    else
      res <- res + (((calc func i) + (calc func _to))*step / 2.0)
    i <- i + step
  res
  // 0-1 1-2 2-3 3-4 4-5                        5-6 6-7 7-8 8-9 9-10
  // 0 - 1.25  2.5  3.75 5 6.25 7.50 8.75 10
let integral func from _to step (threadNum : int) =
  let temp : double = (_to - from) / Convert.ToDouble(threadNum)
  let res : double ref = ref 0.0
  let threadArray = Array.init threadNum (fun i ->
      new Thread(ThreadStart(fun _ ->
        let threadRes = intInRange func (Convert.ToDouble(i) * temp) (Convert.ToDouble((i+1)) * temp) step
        res := res.Value + threadRes
        ))
     )
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  res.Value