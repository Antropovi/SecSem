//Антропов Игорь 171рг
//Время выполнения 2 часа

module Rings

open NUnit.Framework

type CalcInRing(n) =
  member this.Bind (x, f) =
    let temp = x % n
    if temp < 0 then f (temp + n) else f temp
  member this.ReturnFrom(x) =  
    let temp = x % n
    if temp < 0 then (temp + n) else temp
  member this.Combine (a, b) = 
    let temp = (a + b) % n
    if temp < 0 then temp + n else temp
  member this.Delay f = f ()

let ring n = new CalcInRing(n)

