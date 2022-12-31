open day11
open day11.Monkey
open Microsoft.FSharp.Core.Operators.Checked


let worryLevelReductionByFactor reductionValue worryLevel=
    (worryLevel |> float) / (reductionValue |> float)
    |> floor
    |> uint64
    
let reduceWorryLevelByFactor3 = worryLevelReductionByFactor 3

let finishMonkey(monkeyToFinish: int * Monkey)  monkeys =
    let number, monkey = monkeyToFinish
    monkeys |> List.updateAt number (Monkey([], monkey.operation, monkey.divisibleBy, monkey.decision, monkey.inspectedItems))
    
let updateMonkey monkeys (monkeyToUpdate: int * Monkey) =
    let number, monkey = monkeyToUpdate
    monkeys |> List.updateAt number monkey


let rec playMonkey worryLevelReducer number items (monkeys: Monkey list) =
    let monkey = monkeys[number]
    match items with
    | [] -> monkeys |> finishMonkey (number, monkey)                                      
    | head::tail ->
        head
        |> monkey.operation
        |> worryLevelReducer
        |> fun newWorryLevel -> let throwToMonkey = newWorryLevel |> monkey.throwTo
                                (number, monkey.updateAfterInspection())
                                        |> updateMonkey monkeys
                                        |> fun afterInspection -> 
                                            (throwToMonkey, monkeys[throwToMonkey].catch(newWorryLevel))
                                            |> updateMonkey afterInspection
        |> playMonkey worryLevelReducer number tail
        
let rec playRound worryLevelReducer monkeyNumber (monkeys: Monkey list) =
    match monkeys |> List.length  with
    | x when monkeyNumber > x - 1 -> monkeys
    | _ ->
        playMonkey worryLevelReducer monkeyNumber monkeys[monkeyNumber].items monkeys
        |> playRound worryLevelReducer (monkeyNumber + 1)
        
    

let printMonkey index (monkey: Monkey) =
    printfn("-----")
    index |> printfn("Monkey %d")
    monkey.items |> printfn("Items: %A")
    monkey.inspectedItems |> printfn("Count: %A")
    printfn("-----")
    
let printMonkeys (monkeys: Monkey list) =
    monkeys
    |> List.mapi printMonkey
    |> ignore
    monkeys
    

let rec playPart1 roundsToPlay monkeys =
    match roundsToPlay with
    | [] -> monkeys
    | _::tail -> playRound reduceWorryLevelByFactor3 0 monkeys    
                 |> playPart1 tail

let normalizeWorryLevel gcd worryLevel =
    match worryLevel % gcd with
    | 0UL -> gcd
    | x -> x
                    
let rec playPart2 roundsToPlay gcd monkeys =
    let normalizeWorryLevelByGCD = normalizeWorryLevel gcd
    match roundsToPlay with
    | [] -> monkeys    
    | head::tail when [10000] |> List.contains head ->
                head |> printfn("== After round %d ==")
                playRound normalizeWorryLevelByGCD 0 monkeys
                |> printMonkeys
                |> playPart2 tail gcd
    | _::tail ->
                playRound normalizeWorryLevelByGCD 0 monkeys    
                |> playPart2 tail gcd

let getGCD (monkeys: Monkey list) =
    monkeys
    |> List.map (fun m -> m.divisibleBy)
    |> List.reduce (fun a b -> a * b)

let calculateMonkeyBusiness (monkeys: Monkey list) =
    monkeys    
    |> List.sortByDescending (fun monkey -> monkey.inspectedItems)
    |> List.take 2
    |> List.map (fun m -> m.inspectedItems)
    |> List.reduce ( fun a b -> a * b)
   

(*
playPart1 [1..20] PuzzleMonkeys.monkeys
|> calculateMonkeyBusiness
|> printfn "Total monkey business %d"
*)

playPart2 [1..10000] (TestMonkeys.monkeys |> getGCD) TestMonkeys.monkeys
|> calculateMonkeyBusiness 
|> printfn "Total monkey business %d - expected for test 2713310158"
        
//too low:     13937224624    