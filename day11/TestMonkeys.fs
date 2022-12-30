module day11.TestMonkeys
//this time it will be to cumbersome to parse the input file, this will be done manually
open day11.Monkey

let testMonkey0Operation (old: uint) = old * uint 19u
let testMonkey0Decision value =
    match value % 23u with
    | 0u -> 2
    | _ -> 3
    
let testMonkey1Operation old = old + 6u
let testMonkey1Decision value =
    match value % 19u with
    | 0u -> 2
    | _ -> 0
    
let testMonkey2Operation old = old * old
let testMonkey2Decision value =
    match value % 13u with
    | 0u -> 1
    | _ -> 3
    
let testMonkey3Operation old = old + 3u
let testMonkey3Decision value =
    match value % 17u with
    | 0u -> 0
    | _ -> 1

let toUnitList input =
    input |> List.map (fun x -> x |> uint)
let monkey0 = Monkey([79;98] |> toUnitList, testMonkey0Operation, testMonkey0Decision, 0u)
let monkey1 = Monkey([54;65;75;74] |> toUnitList, testMonkey1Operation, testMonkey1Decision, 0u)
let monkey2 = Monkey([79;60;97] |> toUnitList, testMonkey2Operation, testMonkey2Decision, 0u)
let monkey3 = Monkey([74] |> toUnitList, testMonkey3Operation, testMonkey3Decision, 0u)

let monkeys = [monkey0; monkey1; monkey2; monkey3]