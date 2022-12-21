let inFile = "day07/input.txt"

type FileRep = { Name: string; Path: string list; Size: int}



let evaluateCdCommand (files: FileRep list * string list) arg =
    let f, path = files
    match arg with
    | ".." -> (f, path[1..])
    | a -> (f, a::path)
    

let parseCommand (files: FileRep list * string list) (line: string) =
    let parseChangeDir = evaluateCdCommand files 
    let tokens = line.Split " "
    let cmd = tokens[1]
    match cmd with
    | "cd" -> parseChangeDir tokens[2]
    | _ -> files

let parseFile (files: FileRep list * string list) (line: string) =
    let file = line.Split " "
    let size = file[0] |> int
    let fileList, currentPath = files
    let newFile = { Name = file[1]; Path = currentPath; Size = size }
    (newFile::fileList, currentPath)


let parseLine (files: FileRep list * string list)  (line: string)  =
    match line with
    | line when line.StartsWith "$" -> parseCommand files line
    | line when line.StartsWith "dir" -> files 
    | line -> parseFile files line
 
let rec parse (files: FileRep list * string list) (input: string list)  = 
    match input with 
    | [] -> files
    | head::tail ->
        let newDirs = parseLine files head
        parse newDirs tail

let calculateDirSizes (files: FileRep list) =
    let changeFunction (fileSize: int) (value: int option) =
        match value with
        | Some s -> Some (s + fileSize)
        | None -> Some fileSize
        
    let rec calcDirs (file: FileRep) (path: string list) (dirs: Map<string,int>)=
        match path with
        | [] -> dirs
        | _::tail ->
            let f = changeFunction file.Size
            let dirPath = path |> List.rev |> List.reduce (fun a b -> a + "/" + b)
            calcDirs file tail (dirs.Change (dirPath,f))    
    
    let rec calc (files: FileRep list) (dirs: Map<string,int>) =
        match files with
        | [] -> dirs
        | head::tail ->
            calcDirs head head.Path dirs
            |> calc tail
    
    calc files Map.empty

let getDirSizes file =
    let fileParser = parse ([], [])
    let getFileList input =
        let files, _ = input
        files
    
    file
    |> utils.Input.readLines
    |> Seq.toList
    |> fileParser
    |> getFileList
    |> calculateDirSizes     
let part1 file =
    
    let filterBelow sizes =
        sizes
        |> Seq.filter (fun e -> e <= 100000)
    
    file
    |> getDirSizes
    |> fun x -> x.Values
    |> filterBelow
    |> Seq.sum
    |> printfn "Total size of directories of size <= 100000 %A"
 

let part2 file =        
    let dirSizes = file |> getDirSizes
           
    let totalDiskSpace = 70000000
    let neededSpace = 30000000   
    let freeDiskSpace = totalDiskSpace - dirSizes["/"]
    let missingSpace = neededSpace - freeDiskSpace
    
    dirSizes
    |> Map.toList
    |> List.sortBy (fun tup -> snd tup)
    |> List.filter (fun tup -> snd tup >= missingSpace)
    |> fun l -> l[0]
    |> printfn "Directory to delete to gain enough space to update  %A" 

part1 inFile    
part2 inFile