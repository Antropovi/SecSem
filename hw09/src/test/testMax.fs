module testMax

open Max 
open NUnit.Framework



[<TestCase(1, 10, Result = 9)>]
[<TestCase(4, 10, Result = 9)>]
[<TestCase(2, 100, Result = 99)>]
[<TestCase(4, 10000, Result = 9999)>]
[<TestCase(100, 1000000, Result = 999999)>]
[<TestCase(9, 100000000, Result = 99999998)>]
let `` Test of finding max of int array`` threadNumber arraySize =
  maxOnes threadNumber arraySize 