module day11.Monkey

[<Struct>]
type Monkey =
        val items: uint64 list
        val operation: uint64 -> uint64
        val divisibleBy: uint64
        val decision: uint64 -> uint64 -> int
        val inspectedItems: uint64
        new (itemList: uint64 list, op: uint64 -> uint64, divBy: uint64, test: uint64 -> uint64 -> int, count: uint64) = {
            items = itemList
            operation = op
            divisibleBy = divBy
            decision = test
            inspectedItems = count
        }
        
        member this.throwTo(worryLevel: uint64) =
            worryLevel |> this.decision this.divisibleBy
        
        member this.catch(item: uint64) = Monkey(this.items @ [item], this.operation, this.divisibleBy, this.decision, this.inspectedItems)
        member this.updateAfterInspection() = Monkey(this.items |> List.tail, this.operation, this.divisibleBy, this.decision, this.inspectedItems + uint64 1)
        
