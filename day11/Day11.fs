open day11
open day11.Monkey

let worryLevelReductionByFactor reductionValue worryLevel=
    (worryLevel |> float) / (reductionValue |> float)
    |> floor
    |> int
    
let reduceWorryLevelByFactor3 = worryLevelReductionByFactor 3

let finishMonkey(monkeyToFinish: int * Monkey)  monkeys =
    let number, monkey = monkeyToFinish
    monkeys |> List.updateAt number (Monkey([], monkey.operation, monkey.decision, monkey.inspectedItems))
    
let updateMonkey monkeys (monkeyToUpdate: int * Monkey) =
    let number, monkey = monkeyToUpdate
    monkeys |> List.updateAt number monkey


let rec playMonkey number items (monkeys: Monkey list) =
    let monkey = monkeys[number]
    match items with
    | [] -> monkeys |> finishMonkey (number, monkey)                                      
    | head::tail ->
        head
        |> monkey.operation
        |> reduceWorryLevelByFactor3
        |> fun newWorryLevel -> let throwToMonkey = newWorryLevel |> monkey.decision
                                (number, monkey.updateAfterInspection())
                                        |> updateMonkey monkeys
                                        |> fun afterInspection -> 
                                            (throwToMonkey, monkeys[throwToMonkey].catch(newWorryLevel))
                                            |> updateMonkey afterInspection
                                        
                                 
        |> playMonkey number tail
        
let rec playRound monkeyNumber (monkeys: Monkey list) =
    match monkeys |> List.length  with
    | x when monkeyNumber > x - 1 -> monkeys
    | _ ->
        playMonkey monkeyNumber monkeys[monkeyNumber].items monkeys
        |> playRound (monkeyNumber + 1)
        
    

let printMonkey index (monkey: Monkey) =
    printfn("-----")
    index |> printfn("Monkey %d")
    monkey.items |> printfn("Items: %A")
    monkey.inspectedItems |> printfn("Count: %d")
    printfn("-----")
    
let printMonkeys monkeys =
    monkeys
    |> List.mapi printMonkey
    |> ignore
    
let rounds = [1..20]
let rec play roundsToPlay monkeys =
    match roundsToPlay with
    | [] -> monkeys    
    | _::tail -> playRound 0 monkeys    
                 |> play tail
                    

let calculateMonkeyBusiness (monkeys: Monkey list) =
    monkeys    
    |> List.sortByDescending (fun monkey -> monkey.inspectedItems)
    |> List.take 2
    |> List.map (fun m -> m.inspectedItems)
    |> List.reduce ( fun a b -> a * b)
    

play rounds PuzzleMonkeys.monkeys
|> calculateMonkeyBusiness
|> printfn "Total monkey business %d"
        
         