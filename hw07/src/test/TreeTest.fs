module TreeTest

open TreeWorkFlow
open NUnit.Framework

let f a b = a

[<Test>]
let``Test 1: for `` () =
  let tree = Node(Node(Nil, 1, Nil), 2, Node(Nil, 4, Nil))
  let temp = 
    TreeBilder(f, 0){
    for i in [2; 1; 4] do
    return i
  } 
  Assert.AreEqual(tree, temp)

[<Test>]
let ``Test 2: fold `` ()=
  let temp = 
    TreeBilder(f, 0){
    for i in [5; 2; 4; 3] do
    return i
    } 
  let sum = Fold (+) 0 temp
  Assert.AreEqual(14, sum)
  let max = Fold (fun x y -> if x > y then x else y) 0 temp
  Assert.AreEqual(5, max)

[<Test>]
let ``Test 3: combine`` () =
  let temp = 
    TreeBilder(f, 0){
    for i in [5; 2; 4; 3] do
    for j in [1; 6; 8; 7] do
    return i 
    return j
    } 
  let res = Node (Nil, 1, Node (Node (Node (Nil, 2, Node (Node (Nil, 3, Nil), 4, Nil)),
                                           5, Nil), 6, Node(Node(Nil, 7, Nil), 8, Nil)))
  Assert.AreEqual(res, temp)