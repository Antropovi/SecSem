module ringtest

open NUnit.Framework
open Rings


[<TestCase(3 , Result = 2 ) >]
[<TestCase(7 , Result = 0 ) >]
[<TestCase(5 , Result = 4 ) >]
[<TestCase(1 , Result = 0 ) >]
let ``Test 1 of rings``(n) =
  ring(n){
    let! a = 3 * 4
    let! b = 10 / 5
    return! a
    return! b
  }

[<TestCase(3 , Result = 2 ) >]
[<TestCase(7 , Result = 0 ) >]
[<TestCase(11 , Result = 5 ) >]
let ``Test 2 of rings`` (n) =
  ring(n){
    let! a = -6 * 3
    return! a - 10
  }