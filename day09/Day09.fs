// For more information see https://aka.ms/fsharp-console-apps

type Coordinates = int*int


let rec createInBetween (currentHead: Coordinates) calcFunction (inBetweens: Coordinates list) (steps: int list)  = 
    match steps with
    | [] -> inBetweens
    | _::tail ->
        let newHead = calcFunction currentHead
        createInBetween newHead calcFunction (inBetweens @ [newHead]) tail
    

let calcVisitedHeadPositions old (line: string) = 
    let command = line.Split " "
    let steps = [1..command[1] |> int]     
    match command[0] with
    | "R" -> steps |> createInBetween old (fun coord -> Coordinates(fst coord + 1, snd coord)) []
    | "L" -> steps |> createInBetween old (fun coord -> Coordinates(fst coord - 1, snd coord)) []
    | "D" -> steps |> createInBetween old (fun coord -> Coordinates(fst coord, snd coord - 1)) []
    | "U" -> steps |> createInBetween old (fun coord -> Coordinates(fst coord, snd coord + 1)) []
    | _ -> failwith "unknown move"

    
    
let isAdjacent tail head =
    let headX, headY = head
    match tail with
    //same
    | x,y when x = headX && y = headY -> true
    //left or right
    | x,y when x = headX - 1 && y = headY -> true
    | x,y when x = headX + 1 && y = headY -> true
    //up or down
    | x,y when x = headX && y = headY - 1 -> true
    | x,y when x = headX && y = headY + 1 -> true
    //diagonal down
    | x,y when x = headX - 1 && y = headY - 1 -> true
    | x,y when x = headX + 1 && y = headY - 1 -> true
    //diagonal up
    | x,y when x = headX - 1 && y = headY + 1 -> true
    | x,y when x = headX + 1 && y = headY + 1 -> true
    | _ -> false
    
    

let moveTail tail head =
    let headX, headY = head
    let moveCol t =
        match t with
        // left -> move right
        | x,y when x < headX -> (x + 1, y)
        // right -> move left
        | x,y when x > headX -> (x - 1, y)
        | _ -> failwith "not same row"
        
    let moveRow t =
        match t with
        // below -> move up
        | x,y when y < headY -> (x, y + 1)
        // above -> move down
        | x,y when y > headY -> (x, y - 1)
        | _ -> failwith "not same column"
        
    let moveDiagonal t =
        match t with
        //lower left -> move right and up
        | x,y when x < headX && y < headY -> (x + 1, y + 1)
        //lower right -> move left and up
        | x,y when x > headX && y < headY -> (x - 1, y + 1)
        //upper left -> move right and down
        | x,y when x < headX && y > headY -> (x + 1, y - 1)
        //upper right -> move left and down
        | x,y when x > headX && y > headY -> (x - 1, y - 1)
        | _ -> failwith "not diagonal"
    
    //same row
    match tail with
    | x,_ when x = headX -> moveRow tail
    //same col
    | _,y when y = headY -> moveCol tail
    //diagonal
    | _ -> moveDiagonal tail
    
let rec moveTailStepByStep (heads: Coordinates list) (currentTail: Coordinates) (tailPositions: Coordinates list) =
    match heads with
    | [] -> tailPositions
    | head::tail ->
        let newTail = match isAdjacent currentTail head with 
                      | true -> currentTail                                   
                      | false -> moveTail currentTail head
        moveTailStepByStep tail newTail (newTail::tailPositions)

let rec simulate knotCount (precedingKnots: Coordinates list)   =
    match knotCount with
    // we reached the end, return the result 
    | x when x = 0 -> precedingKnots
    | _ ->
        //take the precedingKnots positions, reverse them as we did use new::[old] and we want to start at the beginning
        //every knot starts at (0,0)
        let knotPositions = moveTailStepByStep (precedingKnots |> List.rev) (Coordinates(0,0)) []
        simulate (knotCount - 1) knotPositions 

let calculateTailPositions knotCount line =    
    let rec move currentHead currentTailPos line (positions: Coordinates list) = 
        match line with
        | [] -> positions
        | head::tail ->
            let newHeads = head |> calcVisitedHeadPositions currentHead
            let visitedTailPositions = moveTailStepByStep newHeads currentTailPos []
            move (newHeads |> List.last) visitedTailPositions[0] tail (visitedTailPositions @ positions)
    
    let start = Coordinates(0,0)
    
    // we simulate thew first two knots as the initial value, so we remove the first two 
    let knotsToSimulate = knotCount - 2
    
    //calculate first pair of (like in part1)
    move start start line []
    //calculate remaining knots positions
    |> simulate knotsToSimulate
    
let calculateTwoKnotsRope movements =
    calculateTailPositions 2 movements

let calculateTenKnotsRope movements =
    calculateTailPositions 10 movements

let calculate rope file =
    file
    |> utils.Input.readLines
    |> Seq.toList
    |> rope
    |> List.distinct
    |> List.length
    |> printfn "Total position the tail was on: %A"


let part1 file =
    let calculator = calculate calculateTwoKnotsRope
    file |> calculator
    
let part2 file =
    let calculator = calculate calculateTenKnotsRope
    file |> calculator

part1 "day09/input.txt"
part2 "day09/input.txt"