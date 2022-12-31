
module day11.TestMonkeys
//this time it will be to cumbersome to parse the input file, this will be done manually
open day11.Monkey
open Microsoft.FSharp.Core.Operators.Checked

let testMonkey0Operation (old: uint64) = old * 19UL
let testMonkey0Decision divBy value =
    match value % divBy with
    | 0UL -> 2
    | _ -> 3
    
let testMonkey1Operation old = old + 6UL
let testMonkey1Decision divBy value =
    match value % divBy with
    | 0UL -> 2
    | _ -> 0
    
let testMonkey2Operation old = old * old
let testMonkey2Decision divBy value =
    match value % divBy with
    | 0UL -> 1
    | _ -> 3
    
let testMonkey3Operation old = old + 3UL
let testMonkey3Decision divBy value =
    match value % divBy with
    | 0UL -> 0
    | _ -> 1

let toUnitList input =
    input |> List.map (fun x -> x |> uint64)
let monkey0 = Monkey([79;98] |> toUnitList, testMonkey0Operation, 23UL, testMonkey0Decision, 0UL)
let monkey1 = Monkey([54;65;75;74] |> toUnitList, testMonkey1Operation, 19UL, testMonkey1Decision, 0UL)
let monkey2 = Monkey([79;60;97] |> toUnitList, testMonkey2Operation, 13UL, testMonkey2Decision, 0UL)
let monkey3 = Monkey([74] |> toUnitList, testMonkey3Operation, 17UL, testMonkey3Decision, 0UL)

let monkeys = [monkey0; monkey1; monkey2; monkey3]