module Commands


type Command =
    | Move of count: int * from: int * dest: int
    | NOP

let getSepLine separator file =
    utils.Input.readLines file
    |> Seq.findIndex (fun elem -> elem = separator)

let readCommands infile start =
    infile
    |> utils.Input.readLines
    |> Seq.skip start

let tokenenize (line: string) =
    line.Split " "

let rec createCommands (command: Command) (tokens: string list) = 
    let getArgument (t: string list) = (t[0] |> int)
    
    let getMoveParameters (cmd: Command) =
        match cmd with
        | Move (c,f,d) -> c,f,d
        | _ -> failwith "Not a move command"
        
    match tokens with
    | [] ->  command
    | head::tail when head = "move" ->
        let cmd = Command.Move ((tail |> getArgument), 0, 0) 
        createCommands cmd tail[1..]
    | head::tail when head = "from" ->
        let count, _, dest = getMoveParameters command
        let cmd = Command.Move (count, (tail |> getArgument), dest) 
        createCommands cmd tail[1..]
    | head::tail when head = "to" ->
        let count, from, _ = getMoveParameters command
        let cmd = Command.Move (count, from, (tail |> getArgument)) 
        createCommands cmd tail[1..]
    | _ -> failwith "Unknown command"

let toCommands (line: string) =
    let convert = createCommands Command.NOP
    
    line
    |> tokenenize
    |> List.ofSeq
    |> convert
    
    
let parseCommands infile =
    let getCommands = readCommands infile
    let skipEmptyLine index = index + 1
    
    infile
    |> getSepLine ""
    |> skipEmptyLine
    |> getCommands
    |> Seq.map toCommands

