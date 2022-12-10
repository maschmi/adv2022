let inFile = "day03/input.txt"

let splitHalf (line: string) =
    let length = line.Length
    let mid = length / 2
    match length % 2 with
    | 0 -> line[0..mid - 1], line[mid..]
    | _ -> failwith "not even number of chars"
     
let stringToList (input: string) =
    List.ofSeq input

let findDups (compartement: string) (head: char)=
    compartement.Contains head

let findBadges (backpacks: string*string) (head: char) =
    let bp1, bp2 = backpacks
    bp1.Contains head && bp2.Contains head
let rec search whatToSearch items  =
        match items with
        | [] -> failwith "no duplicate found"
        | (head: char)::_ when whatToSearch head -> head     
        | _::tail -> search whatToSearch tail
    

let rec findDuplicateItems (compartments: string * string) =
    let comp1, comp2 = compartments
    let findDup = search (findDups comp2)

    comp1
    |> stringToList
    |> findDup
    
let isLowercase ascii =
    ascii
    |> int
    |> (fun code -> code >= 97 && code <= 122)

let isUppercase ascii =
    ascii
    |> int
    |> (fun code -> code >= 65 && code <= 90)
    
let toPriority (item: char) =
    let lowerCaseOffset = 97 - 1
    let upperCaseOffset = 65 - 1 - 26
    match item  with
    | item when isLowercase item -> (item |> int) - lowerCaseOffset
    | item when isUppercase item -> (item |> int) - upperCaseOffset
    | _ -> failwith "no char"
    
let part1 file = 
    utils.Input.readLines file
        |> Seq.map splitHalf
        |> Seq.map findDuplicateItems
        |> Seq.map toPriority
        |> Seq.sum
        |> printfn "Summed priorities of misplaces items: %d"

let backpacks (group: 'a[]) =
    match group.Length with
    | 3 -> group[0], group[1], group[2]
    | _ -> failwith "not a group of three"
    
let searchBadge (bp: string*string*string) =
    let bp1, bp2, bp3 = bp
    let findMe = findBadges (bp2,bp3) |> search
   
    bp1
    |> stringToList
    |> findMe
    
let findBadge (group: string[]) =
    group
    |> backpacks
    |> searchBadge 
        
let part2 file =
    utils.Input.readLines file
    |> Seq.chunkBySize 3
    |> Seq.map findBadge
    |> Seq.map toPriority
    |> Seq.sum
    |> printfn "Sum of badges %d"

part1 inFile
part2 inFile