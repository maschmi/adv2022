module LoadingBay 
let getLoadingBay file =
    let separator = ""
    utils.Input.readLines file     
    |> Seq.takeWhile (fun x -> x <> separator) 


let transpose (input: string list list) =
       
    let rec removeEmpty current line  = [
        match line with
        | [] -> yield! current
        | head::tail when head = "" ->            
            yield! removeEmpty current tail
        | head::tail ->
            yield!  head::current
            yield! removeEmpty current tail
    ] 
    
    let toListOfList (matrix: string[,]) = [
        for r in matrix.GetLowerBound(0)..matrix.GetUpperBound(0)
            do yield matrix[r,*]
            |> List.ofArray
            |> removeEmpty []
    ]
        
        
    let w = input[0].Length //width of the storage room
    let h = input.Length // max current height, may include empty spots
    input
    |> fun x -> fun row col -> x[row][col]
    |> Array2D.init h w
    |> fun x -> fun row col -> x[col,row]
    |> Array2D.init w h
    |> toListOfList
    
   
    
let parseLbLine (line: string) =
    let concat (input: char list) =
        input
        |> List.map (fun c -> c.ToString().Trim())
        |> String.concat ""
        
    line 
    |> Seq.toList
    |> List.chunkBySize 4
    |> List.map concat
    

let parseLoadingBay loadingBay  =
    loadingBay
    |> List.map parseLbLine

let removeLastEntry l =
    l
    |> List.removeAt (l.Length - 1)    

let readLoadingBay file =
    getLoadingBay file
    |> Seq.toList 
    |> List.map parseLbLine
    |> removeLastEntry
    |> transpose