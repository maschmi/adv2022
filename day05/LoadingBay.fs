module LoadingBay

//#region file operations
let private getLoadingBay file =
    let separator = ""
    utils.Input.readLines file |> Seq.takeWhile (fun x -> x <> separator)
//#endregion


//#region parsing operations
let transpose (input: string list list) =
    //we do not care about empty positions
    let rec removeEmpty current line =
        [ match line with
          | [] -> yield! current
          | head :: tail when head = "" -> yield! removeEmpty current tail
          | head :: tail ->
              yield! head :: current
              yield! removeEmpty current tail ]

    //convert the Array2D to a list of list without empty positions
    let removeEmptyPositions (matrix: string[,]) =
        [ for r in matrix.GetLowerBound(0) .. matrix.GetUpperBound(0) do
              yield matrix[r, *] |> List.ofArray |> removeEmpty [] ]


    let w = input[0].Length //width of the storage room
    let h = input.Length // max current height, may include empty spots

    input
    //function to parse the list of list into an Array2D
    |> fun x -> fun row col -> x[row][col]
    |> Array2D.init h w
    //function to transpose the Array2D
    |> fun x -> fun row col -> x[col, row]
    |> Array2D.init w h
    //back to a list of list as the positions have different lengths
    |> removeEmptyPositions


let private parseLbLine (line: string) =
    let concat (input: char list) =
        input |> List.map (fun c -> c.ToString().Trim()) |> String.concat ""
    
    line
    //convert the string to a list of chars
    |> Seq.toList
    //each storage position has a width fo 4 chars
    |> List.chunkBySize 4
    //convert the char list to a string without whitespaces
    |> List.map concat

let private parseLoadingBay loadingBay =
    let removeLastEntry l = l |> List.removeAt (l.Length - 1)
    loadingBay |> List.map parseLbLine |> removeLastEntry
//#endregion

let readLoadingBay file =
    getLoadingBay file
    |> Seq.toList
    |> parseLoadingBay
    |> transpose
