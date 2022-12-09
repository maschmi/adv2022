
let inFile = "day02/input.txt" 

// A, X = rock
// B, Y = paper
// C, Z = scissors

// rock beats scissors
// paper beats rock
// scissors beats paper

let drawScore = 3
let wonScore = 6

type Symbols =
    | Rock
    | Paper
    | Scissors
    
type Outcome =
    | Win
    | Lose
    | Draw
    
type Rock () = 
    static let value = 1
    static member score opponent =
        match opponent with
        | Symbols.Rock ->  drawScore + value
        | Symbols.Scissors -> wonScore + value
        | Symbols.Paper -> value
    
    static member needed outcome =
        match outcome with
        | Outcome.Win -> Symbols.Paper
        | Outcome.Lose -> Symbols.Scissors
        | Outcome.Draw -> Symbols.Rock

type Paper () =
    static let value = 2
    static member score opponent =
        match opponent with
        | Symbols.Paper ->  drawScore + value
        | Symbols.Rock -> wonScore + value
        | Symbols.Scissors -> value
        
    static member needed outcome =
        match outcome with
        | Outcome.Win -> Symbols.Scissors
        | Outcome.Lose -> Symbols.Rock
        | Outcome.Draw -> Symbols.Paper
    

type Scissors () =
    static let value = 3
    static member score opponent =
        match opponent with
        | Symbols.Scissors ->  drawScore + value
        | Symbols.Paper -> wonScore + value
        | Symbols.Rock -> value
    
    static member needed outcome =
        match outcome with
        | Outcome.Win -> Symbols.Rock
        | Outcome.Lose -> Symbols.Paper
        | Outcome.Draw -> Symbols.Scissors
    
let convertSymbol element =
    match element with
    | "A" | "X" -> Symbols.Rock
    | "B" | "Y" -> Symbols.Paper
    | "C" |"Z" -> Symbols.Scissors
    | _ -> failwith "no spock"

let convertOutcome element =
    match element with
    | "X" -> Outcome.Lose
    | "Y" -> Outcome.Draw
    | "Z" -> Outcome.Win
    | _ -> failwith "don't run away"

let convertSymbols codes =
    let a,b = codes
    (convertSymbol a, convertSymbol b)

let convertSymbolAndOutcome codes =
    let a,b = codes
    (convertSymbol a, convertOutcome b)
    
let splitLine convert (line: string) =
    match line.Split() with
    | [| a; b |] -> convert (a,b)
    | _ -> failwith "size must be 2"

let rec calcScore game =
    let (a,b) = game
    match b with
    | Symbols.Rock ->  Rock.score a
    | Symbols.Paper -> Paper.score a
    | Symbols.Scissors -> Scissors.score a

let calculateNeededSymbols game =
    let (a,b) = game
    match a with
    | Symbols.Rock -> a, Rock.needed b
    | Symbols.Paper -> a, Paper.needed b
    | Symbols.Scissors -> a, Scissors.needed b
    
let part1Split = splitLine convertSymbols

let part1 inFile = 
    utils.Input.readLines inFile
        |> List.ofArray
        |> List.map part1Split  
        |> List.map calcScore
        |> List.sum
        |> printfn "Total score part1: %d"

let part2Split = splitLine convertSymbolAndOutcome
let part2 inFile =
    utils.Input.readLines inFile
        |> List.ofArray
        |> List.map part2Split 
        |> List.map calculateNeededSymbols
        |> List.map calcScore
        |> List.sum
        |> printfn "Total score part2: %d"

part2 inFile