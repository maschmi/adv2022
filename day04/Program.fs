// For more information see https://aka.ms/fsharp-console-apps
let inFile = "day04/input.txt"

let splitGroups (line: string) =
    let groups = line.Split(",")
    match groups.Length with
    | 2 -> groups[0],groups[1]
    | _ -> failwith "not two groups"

let toSectors min max =
    (min, max)

let getSectors (group: string) =
    let sectors = group.Split("-")
    match sectors.Length with
    | 2 -> toSectors (sectors[0] |> int) (sectors[1] |> int)
    | _ -> failwith "no start / no end"

let getTeamSectors team =
    let one, two = team
    (one |> getSectors, two |> getSectors)

let rec includes list1 list2 =
    let contains list element = list |> List.contains element
    let revList2 = List.rev list2
    contains list1 list2[0] && contains list1 revList2[0]         

let includesCompleteSectorList (team: (int*int)*(int*int)) =
    let m1, m2 = team
    let sec1 = [ fst(m1) .. snd(m1) ]
    let sec2 = [ fst(m2) .. snd(m2) ]
    
    match sec1 with
    | sec when sec |> includes sec2  -> 1
    | sec when sec2 |> includes sec -> 1
    | _ -> 0

let rec includesAny list1 list2 =
    match list1 with
    | [] -> 0
    | head::_ when list2 |> List.contains head -> 1
    | _::tail -> includesAny tail list2

let includesAnySectors (team: (int*int)*(int*int)) =
    let m1, m2 = team
    let sec1 = [ fst(m1) .. snd(m1) ]
    let sec2 = [ fst(m2) .. snd(m2) ]
    sec1 |> includesAny sec2
    

let convertPart1 input =
    input
    |> splitGroups
    |> getTeamSectors
    |> includesCompleteSectorList
   
let part1 file =
    utils.Input.readLines file
    |> Seq.map convertPart1
    |> Seq.sum
    |> printfn "Team count with total overlapping sectors: %d"
    
    
//part1 inFile

let convertPart2 input =
    input
    |> splitGroups
    |> getTeamSectors
    |> includesAnySectors
    
let part2 file =
    utils.Input.readLines file
    |> Seq.map convertPart2
    |> Seq.sum
    |> printfn "Team count with any overlapping sectors: %d"
    
part2 inFile