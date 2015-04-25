module FSharp.Calc
open System


let mutable value1 = ""
let mutable value2 = ""
let mutable operation = ""
let mutable flag = true


let setFlag () = flag <- true


let returnVal ()  =  if flag then value1 
                             else value2
         
                             
let accVal (temp:string) =
  if flag then value1 <- (value1 + temp) 
          else value2 <- (value2 + temp) 


let operSet (oper:string) = 
  flag <- false
  operation <- oper
  

let equal () = 
  match operation with
  | "+"   -> value1 <- Convert.ToString((Convert.ToDouble(value1) + Convert.ToDouble(value2))) 
             operation <- ""; flag <- true 
  | "-"   -> value1 <- Convert.ToString((Convert.ToDouble(value1) - Convert.ToDouble(value2))) 
             operation <- ""; flag <- true 
  | "*"   -> value1 <- Convert.ToString((Convert.ToDouble(value1) * Convert.ToDouble(value2))) 
             operation <- ""; flag <- true 
  | "/"   -> value1 <- Convert.ToString((Convert.ToDouble(value1) / Convert.ToDouble(value2))) 
             operation <- ""; flag <- true 
  | "^n"  -> value1 <- Convert.ToString(Math.Pow(Convert.ToDouble(value1), Convert.ToDouble(value2))) 
             operation <- ""; flag <- true 
  | "log" -> value1 <- Convert.ToString(Math.Log(Convert.ToDouble(value1), Convert.ToDouble(value2))) 
             operation <- ""; flag <- true 

  | _    -> ()
  value2 <- ""


let unaryOper (oper:string) =
  match oper with
  | "+/-"  -> value1 <- Convert.ToString((-1.0) * Convert.ToDouble(value1))
  | "sin"  -> value1 <- Convert.ToString(Math.Sin(Convert.ToDouble(value1)))
  | "cos"  -> value1 <- Convert.ToString(Math.Cos(Convert.ToDouble(value1)))
  | "tan"  -> value1 <- Convert.ToString(Math.Tan(Convert.ToDouble(value1)))
  | "asin" -> value1 <- Convert.ToString(Math.Asin(Convert.ToDouble(value1)))
  | "acos" -> value1 <- Convert.ToString(Math.Acos(Convert.ToDouble(value1)))
  | "atan" -> value1 <- Convert.ToString(Math.Atan(Convert.ToDouble(value1)))
  | "^2"   -> value1 <- Convert.ToString((Convert.ToDouble(value1) * Convert.ToDouble(value1)))
  | "^3"   -> value1 <- Convert.ToString(Math.Pow(Convert.ToDouble(value1), 3.0))
  | "ln"  -> value1 <- Convert.ToString(Math.Log(Convert.ToDouble(value1)))
  | "10^x" -> value1 <- Convert.ToString(Math.Pow(10.0, Convert.ToDouble(value1)))
  | _      -> ()


let clearAll () = 
  value1 <- ""
  value2 <- ""
  flag <- true
