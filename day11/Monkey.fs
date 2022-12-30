module day11.Monkey

[<Struct>]
type Monkey =
        val items: uint list
        val operation: uint -> uint
        val decision: uint -> int
        val inspectedItems: uint
        new (itemList: uint list, op: uint -> uint, test: uint -> int, count: uint) = {
            items = itemList
            operation = op
            decision = test
            inspectedItems = count
        }
        member this.catch(item: uint) = Monkey(this.items @ [item], this.operation, this.decision, this.inspectedItems)
        member this.updateAfterInspection() = Monkey(this.items |> List.tail, this.operation, this.decision, this.inspectedItems + uint 1)
        
