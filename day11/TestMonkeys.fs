module day11.TestMonkeys
//this time it will be to cumbersome to parse the input file, this will be done manually
open day11.Monkey

let testMonkey0Operation old = old * 19
let testMonkey0Decision value =
    match value % 23 with
    | 0 -> 2
    | _ -> 3
    
let testMonkey1Operation old = old + 6
let testMonkey1Decision value =
    match value % 19 with
    | 0 -> 2
    | _ -> 0
    
let testMonkey2Operation old = old * old
let testMonkey2Decision value =
    match value % 13 with
    | 0 -> 1
    | _ -> 3
    
let testMonkey3Operation old = old + 3
let testMonkey3Decision value =
    match value % 17 with
    | 0 -> 0
    | _ -> 1

let monkey0 = Monkey([79;98], testMonkey0Operation, testMonkey0Decision, 0)
let monkey1 = Monkey([54;65;75;74], testMonkey1Operation, testMonkey1Decision, 0)
let monkey2 = Monkey([79;60;97], testMonkey2Operation, testMonkey2Decision, 0)
let monkey3 = Monkey([74], testMonkey3Operation, testMonkey3Decision, 0)

let monkeys = [monkey0; monkey1; monkey2; monkey3]