module Commands

//this represents the commands available
type Command =
    | Move of count: int * from: int * dest: int
    | NOP

//#region file operations
let private getSepLineIndex separator file =
    utils.Input.readLines file |> Seq.findIndex (fun elem -> elem = separator)

let private readCommands infile start =
    infile |> utils.Input.readLines |> Seq.skip start
//#endregion

//#region command parsing

//tokenizes the line by splitting at whitespaces
let private tokenize (line: string) = line.Split " " |> List.ofSeq

//parsed the tokens in a line into a command
let rec private createCommandFromTokens (command: Command) (tokens: string list) =
    let convertToInt (t: string list) = (t[0] |> int)

    let getMoveParameters (cmd: Command) =
        match cmd with
        | Move (c, f, d) -> c, f, d
        | _ -> failwith "Not a move command"

    match tokens with
    | [] -> command
    | head :: tail when head = "move" ->
        let cmd = Command.Move((tail |> convertToInt), 0, 0)
        createCommandFromTokens cmd tail[1..]
    | head :: tail when head = "from" ->
        let count, _, dest = getMoveParameters command
        let cmd = Command.Move(count, (tail |> convertToInt), dest)
        createCommandFromTokens cmd tail[1..]
    | head :: tail when head = "to" ->
        let count, from, _ = getMoveParameters command
        let cmd = Command.Move(count, from, (tail |> convertToInt))
        createCommandFromTokens cmd tail[1..]
    | _ -> failwith "Unknown command"

let private toCommands (line: string) =
    let convertToCommand = createCommandFromTokens Command.NOP

    line |> tokenize |> convertToCommand
//#endregion

// main entry point to parse the given file
let parseCommands infile =
    let getCommands = readCommands infile
    let skipEmptyLine index = index + 1

    infile
    |> getSepLineIndex ""
    |> skipEmptyLine
    |> getCommands
    |> Seq.map toCommands
