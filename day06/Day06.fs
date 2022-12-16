
open System.Drawing

let findMarkerStart windowSize list =
    let isMarker window =        
        let list = window
                |> Seq.toList
        
        list
        |> List.distinct
        |> List.length = list.Length 

    let offsetSize = windowSize - 1    
    let rec slideWindow size (list: char list) index =
        match list[index..(index + offsetSize)] with
        | window when window |> isMarker -> index
        | _ -> slideWindow size list (index + 1)
        
    slideWindow windowSize list 0
    |> fun x -> x + windowSize

let charsUntilPacketMarkerEnd (input: string) =
    input
    |> Seq.toList
    |> findMarkerStart 4
let charsUntilMessageMarkerEnd (input: string) =
    input
    |> Seq.toList
    |> findMarkerStart 14

let inFile = "day06/input.txt"
let part1 file =
    file
    |> utils.Input.readAllLines
    |> fun x -> x[0]
    |> charsUntilPacketMarkerEnd
    |> printfn "Chars until end of packet marker: %d"
    
let part2 file =
    file
    |> utils.Input.readAllLines
    |> fun x -> x[0]
    |> charsUntilMessageMarkerEnd
    |> printfn "Chars until end of message marker: %d"

part1 inFile
part2 inFile