module test

open NUnit.Framework
open FSharp.Calc


[<Test>]
let ``Test 1 of calc`` () = 
  // simulation of pressing 5 + 6 = 
  // exp 11
  accVal("5")
  Assert.AreEqual("5", returnVal())
  equal()
  Assert.AreEqual("5", returnVal())
  operSet("+")
  accVal("6")
  Assert.AreEqual("6", returnVal())
  equal()
  setFlag()
  Assert.AreEqual("11", returnVal())

  clearAll()


[<Test>]
let ``Test 2 of calc`` () = 
  // simulation of pressing 0 cos * 5 =
  // exp 5
  accVal("0")
  Assert.AreEqual("0", returnVal())
  equal()
  unaryOper("cos")
  Assert.AreEqual("1", returnVal())
  equal()
  Assert.AreEqual("1", returnVal())
  operSet("*")
  accVal("5")
  Assert.AreEqual("5", returnVal())

  clearAll()

