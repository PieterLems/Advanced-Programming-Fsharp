module Main
open System
open BasicFunctions
open EasyFunctions
open AverageFunctions
open HardFunctions
open Tutorial


[<EntryPoint>]
let main argv =
  BasicFunctions()
  printfn "\n"
  EasyFunctions()
  printfn "\n"
  AverageFunctions()
  printfn "\n"
  HardFunctions()
  printfn "\n"
  Tutorial()
  0
