module testIntegrals

open Integral
open NUnit.Framework


[<Test>]
let ``Testing of integrals `` () =
  Assert.Less((integral "x" 0.0 10.0 0.0001 1) - 50.0, 1.0)
  Assert.Less((integral "x" 0.0 10.0 0.0001 2) - 50.0, 1.0)
  Assert.Less((integral "x" 0.0 10.0 0.0001 4) - 50.0, 1.0)
  Assert.Less((integral "x" 0.0 10.0 0.0001 8) - 50.0, 1.0)
  Assert.Less((integral "1" 0.0 10.0 0.0001 2) - 10.0, 1.0)
