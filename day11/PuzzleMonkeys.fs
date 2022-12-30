module day11.PuzzleMonkeys
//this time it will be to cumbersome to parse the input file, this will be done manually
open day11.Monkey

let testMonkey0Operation old = old * 17
let testMonkey0Decision value =
    match value % 3 with
    | 0 -> 3
    | _ -> 6
    
let testMonkey1Operation old = old + 2
let testMonkey1Decision value =
    match value % 13 with
    | 0 -> 3
    | _ -> 0
    
let testMonkey2Operation old = old + 1
let testMonkey2Decision value =
    match value % 2 with
    | 0 -> 0
    | _ -> 1
    
let testMonkey3Operation old = old + 7
let testMonkey3Decision value =
    match value % 11 with
    | 0 -> 6
    | _ -> 7

let testMonkey4Operation old = old * old
let testMonkey4Decision value =
    match value % 19 with
    | 0 -> 2
    | _ -> 5

let testMonkey5Operation old = old + 8
let testMonkey5Decision value =
    match value % 17 with
    | 0 -> 2
    | _ -> 1

let testMonkey6Operation old = old * 2
let testMonkey6Decision value =
    match value % 5 with
    | 0 -> 4
    | _ -> 7

let testMonkey7Operation old = old + 6
let testMonkey7Decision value =
    match value % 7 with
    | 0 -> 4
    | _ -> 5


let monkey0 = Monkey([59;65;86;56;74;57;56], testMonkey0Operation, testMonkey0Decision, 0)
let monkey1 = Monkey([63;83;50;63;56], testMonkey1Operation, testMonkey1Decision, 0)
let monkey2 = Monkey([93;79;74;55], testMonkey2Operation, testMonkey2Decision, 0)
let monkey3 = Monkey([86;61;67;88;94;69;56;91], testMonkey3Operation, testMonkey3Decision, 0)

let monkey4 = Monkey([76;50;51], testMonkey4Operation, testMonkey4Decision, 0)
let monkey5 = Monkey([77;76], testMonkey5Operation, testMonkey5Decision, 0)
let monkey6 = Monkey([74], testMonkey6Operation, testMonkey6Decision, 0)
let monkey7 = Monkey([67;85;52;86;91;95], testMonkey7Operation, testMonkey7Decision, 0)

let monkeys = [monkey0; monkey1; monkey2; monkey3; monkey4; monkey5; monkey6; monkey7]