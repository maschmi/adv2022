open System.Text
open Commands
let inFile = "day05/input.txt"

let getSepLine file separator =
    utils.Input.readLines file
    |> Seq.findIndex separator


let crateMover9000 from count (bay: string list list) : string list =
    bay[from-1][0..count-1] |> List.rev
    
let crateMover9001 from count (bay: string list list) : string list =
    bay[from-1][0..count-1]


let execute crane storageBay command =
    let rec move f d newFrom newDest (newBay: string list list) idx (bay: string list list) = [
        match bay with
        | [] -> yield! newBay
        | _::tail when idx = f - 1  ->
            yield! move f d newFrom newDest (newBay @ [newFrom]) (idx + 1) tail
        | _::tail when idx = d - 1  ->
            yield! move f d newFrom newDest (newBay @ [newDest]) (idx + 1) tail
        | head::tail ->
            yield! move f d newFrom newDest (newBay @ [head]) (idx + 1) tail
    ]
    
    let doMove m f d (bay: string list list) =
        let moving = bay |> crane f m
        let newFrom = bay[f-1][m..]
        let newDest = moving @ bay[d-1] 
        bay
        |> move f d newFrom newDest ([]: string list list) 0
    
   
    match command with
    | Move(count, from, dest) ->
        doMove count from dest storageBay
    | _ -> failwith "Unknown command"
   
    


let rec getTops tops (bay: string list list) = [
    match bay with
    | [] -> yield! tops
    | head::tail ->
        yield! getTops (tops @ [head[0][1]]) tail
]


    
    
let rec performCommands crane bay commands = 
    match commands with
    | [] -> bay
    | head::tail ->
        let bayAfter = execute crane bay head
        performCommands crane bayAfter tail
        



let unloadShip crane file =
    let loadingBay = LoadingBay.readLoadingBay file
    file
    |> Commands.parseCommands
    |> List.ofSeq
    |> performCommands crane loadingBay
    |> getTops []
    |> fun chars -> string(List.fold (fun (sb: StringBuilder) (c: char) -> sb.Append(c)) (StringBuilder()) chars) 
    |> printfn "Word is: %s"
    
let part1 file
    = unloadShip crateMover9000 file
    
let part2 file
    = unloadShip crateMover9001 file
    
part1 inFile
part2 inFile