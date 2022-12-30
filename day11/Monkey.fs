module day11.Monkey

[<Struct>]
type Monkey =
        val items: int list
        val operation: int -> int
        val decision: int -> int
        val inspectedItems: int
        new (itemList: int list, op: int -> int, test: int -> int, count: int) = {
            items = itemList
            operation = op
            decision = test
            inspectedItems = count
        }
        member this.catch(item: int) = Monkey(this.items @ [item], this.operation, this.decision, this.inspectedItems)
        member this.updateAfterInspection() = Monkey(this.items |> List.tail, this.operation, this.decision, this.inspectedItems + 1)
        
