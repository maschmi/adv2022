module testing
open Day06
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.That(charsUntilPacketMarkerEnd "mjqjpqmgbljsphdztnvjfqwrcgsmlb", Is.EqualTo 7)

[<Test>]
let Test2 () =
    Assert.That(charsUntilPacketMarkerEnd "bvwbjplbgvbhsrlpgdmjqwftvncz", Is.EqualTo 5)

[<Test>]
let Test3 () =
    Assert.That(charsUntilPacketMarkerEnd "nppdvjthqldpwncqszvftbrmjlhg", Is.EqualTo 6)
 
[<Test>]
let Test4 () =
    Assert.That(charsUntilPacketMarkerEnd "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", Is.EqualTo 10)
    
[<Test>]
let Test5 () =
    Assert.That(charsUntilPacketMarkerEnd "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", Is.EqualTo 11)
    
[<Test>]
let Test6 () =
    Assert.That(charsUntilMessageMarkerEnd "mjqjpqmgbljsphdztnvjfqwrcgsmlb", Is.EqualTo 19)

[<Test>]
let Test7 () =
    Assert.That(charsUntilMessageMarkerEnd "nppdvjthqldpwncqszvftbrmjlhg", Is.EqualTo 23)
    
[<Test>]
let Test8 () =
    Assert.That(charsUntilMessageMarkerEnd "bvwbjplbgvbhsrlpgdmjqwftvncz", Is.EqualTo 23)
    
[<Test>]
let Test9 () =
    Assert.That(charsUntilMessageMarkerEnd "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", Is.EqualTo 29)
    
[<Test>]
let Test10 () =
    Assert.That(charsUntilMessageMarkerEnd "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", Is.EqualTo 26)