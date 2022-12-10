open System

open System.Diagnostics
open Microsoft.FSharp.Core
let inFile = "day01/input-01.txt" 


let nextSlice input  =
    input
    |> List.findIndex (fun item -> item.Equals(""))
    |> fun x -> input[..x]
    
let splitBy v list =
    let sumIt list = 
      if list = [] then []
      else [List.sum list]
    
    let rec loop groupSoFar list = seq { 
      match list with
      | [] -> yield! sumIt groupSoFar
      | head::tail when head = v ->
          yield! sumIt groupSoFar
          yield! loop [] tail
      | head::tail ->
          yield! loop (groupSoFar @ [head |> int]) tail }
    loop [] list |> List.ofSeq
    
let findMax list =
    list
    |> Seq.mapi (fun i v -> i, v)
    |> Seq.maxBy snd

let printMax file = 
    utils.Input.readAllLines file
            |> List.ofArray
            |> splitBy ""
            |> findMax
            |> printfn "Max calories %A" 

let sumTop file n = 
    utils.Input.readAllLines file
            |> List.ofArray
            |> splitBy ""
            |> List.sortDescending
            |> List.take n
            |> List.sum
            |> printfn "Sum of top %d: %d" n 
            
printMax inFile
sumTop inFile 3