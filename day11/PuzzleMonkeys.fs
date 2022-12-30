module day11.PuzzleMonkeys
//this time it will be to cumbersome to parse the input file, this will be done manually
open day11.Monkey

let testMonkey0Operation old = old * 17u
let testMonkey0Decision value =
    match value % 3u with
    | 0u -> 3
    | _ -> 6
    
let testMonkey1Operation old = old + 2u
let testMonkey1Decision value =
    match value % 13u with
    | 0u -> 3
    | _ -> 0
    
let testMonkey2Operation old = old + 1u
let testMonkey2Decision value =
    match value % 2u with
    | 0u -> 0
    | _ -> 1
    
let testMonkey3Operation old = old + 7u
let testMonkey3Decision value =
    match value % 11u with
    | 0u -> 6
    | _ -> 7

let testMonkey4Operation old = old * old
let testMonkey4Decision value =
    match value % 19u with
    | 0u -> 2
    | _ -> 5

let testMonkey5Operation old = old + 8u
let testMonkey5Decision value =
    match value % 17u with
    | 0u -> 2
    | _ -> 1

let testMonkey6Operation old = old * 2u
let testMonkey6Decision value =
    match value % 5u with
    | 0u -> 4
    | _ -> 7

let testMonkey7Operation old = old + 6u
let testMonkey7Decision value =
    match value % 7u with
    | 0u -> 4
    | _ -> 5

let toUnitList input =
    input |> List.map (fun x -> x |> uint)
let monkey0 = Monkey([59;65;86;56;74;57;56] |> toUnitList, testMonkey0Operation, testMonkey0Decision, 0u)
let monkey1 = Monkey([63;83;50;63;56] |> toUnitList, testMonkey1Operation, testMonkey1Decision, 0u)
let monkey2 = Monkey([93;79;74;55] |> toUnitList, testMonkey2Operation, testMonkey2Decision, 0u)
let monkey3 = Monkey([86;61;67;88;94;69;56;91] |> toUnitList, testMonkey3Operation, testMonkey3Decision, 0u)
let monkey4 = Monkey([76;50;51] |> toUnitList, testMonkey4Operation, testMonkey4Decision, 0u)
let monkey5 = Monkey([77;76] |> toUnitList, testMonkey5Operation, testMonkey5Decision, 0u)
let monkey6 = Monkey([74u], testMonkey6Operation, testMonkey6Decision, 0u)
let monkey7 = Monkey([67;85;52;86;91;95] |> toUnitList, testMonkey7Operation, testMonkey7Decision, 0u)

let monkeys = [monkey0; monkey1; monkey2; monkey3; monkey4; monkey5; monkey6; monkey7]