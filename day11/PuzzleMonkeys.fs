module day11.PuzzleMonkeys
//this time it will be to cumbersome to parse the input file, this will be done manually
open day11.Monkey
open Microsoft.FSharp.Core.Operators.Checked

let testMonkey0Operation old = old * 17UL
let testMonkey0Decision divBy value =
    match value % divBy with
    | 0UL -> 3
    | _ -> 6
    
let testMonkey1Operation old = old + 2UL
let testMonkey1Decision divBy value =
    match value % divBy with
    | 0UL -> 3
    | _ -> 0
    
let testMonkey2Operation old = old + 1UL
let testMonkey2Decision divBy value =
    match value % divBy with
    | 0UL -> 0
    | _ -> 1
    
let testMonkey3Operation old = old + 7UL
let testMonkey3Decision divBy value =
    match value % divBy with
    | 0UL -> 6
    | _ -> 7

let testMonkey4Operation old = old * old
let testMonkey4Decision divBy value =
    match value % divBy with
    | 0UL -> 2
    | _ -> 5

let testMonkey5Operation old = old + 8UL
let testMonkey5Decision divBy value =
    match value % divBy with
    | 0UL -> 2
    | _ -> 1

let testMonkey6Operation old = old * 2UL
let testMonkey6Decision divBy value =
    match value % divBy with
    | 0UL -> 4
    | _ -> 7

let testMonkey7Operation old = old + 6UL
let testMonkey7Decision divBy value =
    match value % divBy with
    | 0UL -> 4
    | _ -> 5

let toUnitList input =
    input |> List.map (fun x -> x |> uint64)
let monkey0 = Monkey([59;65;86;56;74;57;56] |> toUnitList, testMonkey0Operation, 3UL, testMonkey0Decision, 0UL)
let monkey1 = Monkey([63;83;50;63;56] |> toUnitList, testMonkey1Operation, 13UL, testMonkey1Decision, 0UL)
let monkey2 = Monkey([93;79;74;55] |> toUnitList, testMonkey2Operation, 2UL, testMonkey2Decision, 0UL)
let monkey3 = Monkey([86;61;67;88;94;69;56;91] |> toUnitList, testMonkey3Operation, 11UL, testMonkey3Decision, 0UL)
let monkey4 = Monkey([76;50;51] |> toUnitList, testMonkey4Operation, 19UL, testMonkey4Decision, 0UL)
let monkey5 = Monkey([77;76] |> toUnitList, testMonkey5Operation, 17UL, testMonkey5Decision, 0UL)
let monkey6 = Monkey([74UL], testMonkey6Operation, 5UL, testMonkey6Decision, 0UL)
let monkey7 = Monkey([86;85;52;86;91;95] |> toUnitList, testMonkey7Operation, 7UL, testMonkey7Decision, 0UL)

let monkeys = [monkey0; monkey1; monkey2; monkey3; monkey4; monkey5; monkey6; monkey7]