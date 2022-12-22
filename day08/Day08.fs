// For more information see https://aka.ms/fsharp-console-apps
let inFile = "day08/input.txt"

// extracts the row and splits it at the position of the tree
let horizontalLine (trees: int[,]) idx =
    let x,y = idx
    trees[y..y,*]
    |> Seq.cast<int>
    |> Seq.toList
    |> fun row -> row[0..x-1], row[x+1..]        
    

// extracts the column and splits it at the position of the tree
let verticalLine (trees: int[,]) (idx: int*int) =
    let x,y = idx
    trees[*,x..x]
    |> Seq.cast<int>
    |> Seq.toList
    |> fun col -> col[0..y-1], col[y+1..]

type Coordinates = int*int

let maxSize matrix =
    Coordinates (Array2D.length2 matrix - 1, Array2D.length1 matrix - 1)
    
let checkVisible (trees: seq<seq<int>>) =
    
    let rec hasLineOfSight tree (lineOfSight: int list * int list)  =
        let rec partMatch line =
            match line with
            | [] -> true
            // we can abort as soon as we find a tree higher then the invesitgated one on the line of sight
            | head::_ when head >= tree -> false
            | _::tail -> partMatch tail
       
        //we need to match left/right, top/down sperately
        match partMatch (fst lineOfSight) with
        | true -> true
        //reverse the second part as we look von outside to in
        | false -> partMatch (snd lineOfSight |> List.rev)
        
    let isBorder (dims: int*int ) (idx: int*int) =
        let maxX, maxY = dims
        
        match idx with
        | x,y when x = 0 || y = 0  -> true
        | x,y when x = maxX || y = maxY -> true
        | _ -> false
        
    let isVisible (trees: int[,]) (dims: int*int) (idx: int*int) =
        match isBorder dims idx with
        | true -> true
        | false -> match hasLineOfSight (trees[snd idx,fst idx]) (horizontalLine trees idx) with
                   | true -> true
                   | false -> hasLineOfSight (trees[snd idx,fst idx]) (verticalLine trees idx)

    let rec count (trees: int[,]) (dims: Coordinates) idx counter =
        let maxX, maxY = dims
        
        let visibleTree = match isVisible trees dims idx with
                          | true -> 1
                          | false -> 0
                          
        match idx with
        | x,_ when x = maxX -> match idx with
                               | _,y when y = maxY -> counter + 1 //we need to add one as we are not iteration over (maxX, maxY)
                               | _,y -> count trees dims (0, y + 1) (counter + visibleTree)
        | x,y -> count trees dims (x+1,y) (counter + visibleTree)
    
    
    let idx = Coordinates (0,0)
    let trarray = array2D trees
    //we use x,y but they use y,x
    let dims = trarray |> maxSize
    count trarray dims idx 0

let parseLine (line: string) =
    line
    |> Seq.toArray
    |> Seq.map (fun e -> e.ToString() |> int )
    

let part1 file =
    file
    |> utils.Input.readLines
    |> Seq.map parseLine  
    |> checkVisible
    |> printfn "Number of total visible trees: %A"
    
    
let calculateScenicScores (trees: seq<seq<int>>) : int list =
    let getX coords =
        fst coords
    
    let getY coords =
        snd coords
    
    let rec countTreesVisible (tree: int) (trees: int list) counter =
        match trees with
        | [] -> counter
        | head::_ when head >= tree -> counter + 1
        | _::tail -> countTreesVisible tree tail (counter + 1) 
    
    let calculateDimension getLine (tree: int) (treeMatrix: int[,]) (idx: Coordinates) =
        let l, r = getLine treeMatrix idx
        //this time we look outwards, so we reverse the first part of the tuple (left or upwards)
        let countLeft = countTreesVisible tree (l |> List.rev) 0
        let countRight = countTreesVisible tree r 0
        countLeft * countRight
    
    let calculateVertical (tree: int) (treeMatrix: int[,]) (idx: Coordinates) =
        calculateDimension verticalLine tree treeMatrix idx
        
    let calculateHorizontal (tree: int) (treeMatrix: int[,]) (idx: Coordinates) =
        calculateDimension horizontalLine tree treeMatrix idx
         
        
    let calculateTree (tree: int) (treeMatrix: int[,]) (idx: Coordinates) =
        (calculateHorizontal tree treeMatrix idx) *  (calculateVertical tree treeMatrix idx)
        
    
    let rec calculateAll (treeMatrix: int[,]) (dims: Coordinates) (idx: Coordinates) : int list  = [
            
        match idx with
        | x,_ when x = (dims |> getX) ->
            match idx with
            | x,y when y = (dims |> getY) ->               
                yield calculateTree treeMatrix[y,x] treeMatrix idx 
            | x,y ->
                yield calculateTree treeMatrix[y,x] treeMatrix idx
                yield! calculateAll treeMatrix dims (0,y+1) 
        | x,y ->
            yield calculateTree treeMatrix[y,x] treeMatrix idx
            yield! calculateAll treeMatrix dims (x+1,y)
    ]
    
    
    let treeMatrix = array2D trees
    let dims = treeMatrix |> maxSize
    let idx = Coordinates(0,0)
    
    calculateAll treeMatrix dims idx
    
    
let part2 file =
    file
    |> utils.Input.readLines
    |> Seq.map parseLine
    |> calculateScenicScores
    |> List.max
    |> printfn "Maximal scenic score is: %A"

//part1 inFile    
part2 "day08/input.txt"