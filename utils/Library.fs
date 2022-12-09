namespace utils

open System.IO

module Input =
    let rootPath = "/home/martin/src/AdventOfCode2022"
    let readLines file =
        File.ReadAllLines $"{rootPath}/{file}"
        
module ListUtils =
    let splitBy v list =
       
        let rec loop groupSoFar list = seq { 
          match list with
          | [] -> yield! groupSoFar
          | head::tail when head = v ->
              yield! groupSoFar
              yield! loop [] tail
          | head::tail ->
              yield! loop (groupSoFar @ [head]) tail }
        loop [] list |> List.ofSeq