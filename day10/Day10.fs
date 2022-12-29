// For more information see https://aka.ms/fsharp-console-apps


let cyclesOfInterest = [20; 60; 100; 140; 180; 220]

let (maxCycles: int) = 6 * 40 

type Command =
    | NOOP of int
    | ADDX of int*int
    
type RegisterValue = { cycle: int; value: int }
let noop = NOOP (1)
let addx x = ADDX(2, x)

let parseCommand (line: string) =
    let cmd = line.Split " "
    match cmd[0] with
    | "noop" ->  noop
    | "addx" ->  addx (cmd[1] |> int)
    | _ -> failwith "unknown oppcode"
  
let rec executeCommand (registerInCycle: RegisterValue) command: RegisterValue list= [
    let cycle = registerInCycle.cycle
    let value = registerInCycle.value
    match command with
    | NOOP(leftCycles) when leftCycles = 1 -> yield { cycle = cycle + 1; value =  value }
    | ADDX(leftCycles, x) when leftCycles = 1 -> yield { cycle = cycle + 1; value =  value + x }
    | NOOP(leftCycles) ->
        let noopCycle = {  cycle = cycle + 1; value =  value } 
        yield  noopCycle
        yield! executeCommand { cycle = cycle + 1; value =  value } (NOOP(leftCycles - 1))
    | ADDX(leftCycles,x) ->
        let noopCycle = {  cycle = cycle + 1; value =  value } 
        yield  noopCycle
        yield! executeCommand { cycle = cycle + 1; value =  value } (ADDX(leftCycles - 1, x))
    ]

    
let readCommands commandSeq =
    let evaluateCommand command (registerByCycle: RegisterValue list): RegisterValue list  =
        parseCommand command
        |> executeCommand (registerByCycle |> List.last)
        |> fun x -> registerByCycle @ x
    
    let rec nextCommand (commands: string seq) (registerByCycle: RegisterValue list) =
        let highestCycle = registerByCycle |> List.last |> fun x -> x.cycle
        match commands with
        | _ when (commands |> Seq.isEmpty) -> registerByCycle
        | cmds when highestCycle <= maxCycles -> nextCommand (cmds |> Seq.tail) (evaluateCommand (cmds |> Seq.head) registerByCycle)
        | _ when highestCycle > maxCycles -> registerByCycle
        | _ -> failwith "todo"
        
    nextCommand commandSeq [ { cycle = 1; value = 1 } ]


let calculatePixel sprite pixelPos  =
    match sprite |> List.contains pixelPos with
    | true -> "#"
    | false -> "."

let calcSprite registerValue =
     [registerValue - 1, registerValue, registerValue + 1]

let getPixelPos cycle =
    cycle % 40

let rec extractValueDuringDefinedCycles (definedCycles: int list) (values: RegisterValue list) (input: RegisterValue list) =
    match definedCycles with
    |  [] -> values
    |  head::tail ->
        //we want to know the value DURING the cycle so we substract one, otherwise we will get it AFTER the cycles
        let value = match input |> List.tryItem (head - 1) with
                    | Some(v) -> v
                    | None -> { cycle = head; value = 1 }
        extractValueDuringDefinedCycles tail (value::values) input

let calculateSignalStrength input =
    input.cycle * input.value
    
let part1 file =
    utils.Input.readLines file
    |> readCommands
    |> extractValueDuringDefinedCycles cyclesOfInterest []
    |> List.map calculateSignalStrength
    |> List.sum
    |> printfn "Total sum of all signal strengths:  %A"

part1 "day10/input.txt"