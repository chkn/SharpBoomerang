[<NUnit.Framework.TestFixture>]
module SharpBoomerang.Tests.IsoTests

open System
open System.Linq
open NUnit.Framework

open SharpBoomerang

type DU =
    | DUStr of string
    | DUFloat of float
    | DUList of DU seq
    | DUMap of Map<string,int>


let write value (ch : #IChannel<_>) = ch.Write(value); ch
let assertRead (expected : 't) (str : string) (ch : IChannel<'t>) =
    ch.Read(fun v -> Assert.AreEqual(expected, v, str))
    ch

let testInt v1 v2 iso =
    let pipe = Channel.pipe() |> write 6
    let map = pipe
              |> Channel.map iso
              |> assertRead v1 "#1"
              |> write 2
    Assert.AreEqual(v2, pipe.Value.Value, "#2")
    map
    |> assertRead 2 "#3"
    |> ignore

let testStr v1 v2 iso =
    let pipe = Channel.pipe() |> write "Hello, World!"
    let map = pipe
              |> Channel.map iso
              |> assertRead v1 "#1"
              |> write "So long!"
    Assert.AreEqual(v2, pipe.Value.Value, "#2")

// Numerical:
let [<Test>] AddExplicit() = ((fun i -> i + 1), (fun i -> i - 1)) |> testInt 7 1
let [<Test>] AddImplicit() = Iso.ofFn (fun i -> i + 1) |> testInt 7 1

let [<Test>] SubExplicit() = ((fun i -> i - 1), (fun i -> i + 1)) |> testInt 5 3
let [<Test>] SubImplicit() = Iso.ofFn (fun i -> i - 1) |> testInt 5 3

let [<Test>] MulExplicit() = ((fun i -> i * 2), (fun i -> i / 2)) |> testInt 12 1
let [<Test>] MulImplicit() = Iso.ofFn (fun i -> i * 2) |> testInt 12 1

let [<Test>] DivExplicit() = ((fun i -> i / 2), (fun i -> i * 2)) |> testInt 3 4
let [<Test>] DivImplicit() = Iso.ofFn (fun i -> i / 2) |> testInt 3 4

// String:
let [<Test>] ToUpperExplicit() = ((fun (s : string) -> s.ToUpper()), (fun (s : string) -> s.ToLower())) |> testStr "HELLO, WORLD!" "so long!"
let [<Test>] ToUpperImplicit() = Iso.ofFn (fun (s : string) -> s.ToUpper()) |> testStr "HELLO, WORLD!" "so long!"

let [<Test>] ToLowerExplicit() = ((fun (s : string) -> s.ToLower()), (fun (s : string) -> s.ToUpper())) |> testStr "hello, world!" "SO LONG!"
let [<Test>] ToLowerImplicit() = Iso.ofFn (fun (s : string) -> s.ToLower()) |> testStr "hello, world!" "SO LONG!"

let [<Test>] ToUpperInvExplicit() = ((fun (s : string) -> s.ToUpperInvariant()), (fun (s : string) -> s.ToLowerInvariant())) |> testStr "HELLO, WORLD!" "so long!"
let [<Test>] ToUpperInvImplicit() = Iso.ofFn (fun (s : string) -> s.ToUpperInvariant()) |> testStr "HELLO, WORLD!" "so long!"

let [<Test>] ToLowerInvExplicit() = ((fun (s : string) -> s.ToLowerInvariant()), (fun (s : string) -> s.ToUpperInvariant())) |> testStr "hello, world!" "SO LONG!"
let [<Test>] ToLowerInvImplicit() = Iso.ofFn (fun (s : string) -> s.ToLowerInvariant()) |> testStr "hello, world!" "SO LONG!"

[<Test>]
let JoinImplicit() =
    let pipe = Channel.pipe() |> write [| "Hello"; "World" |]
    let map = pipe
              |> Channel.map(Iso.ofFn (fun a -> String.Join(", ", a)))
              |> assertRead "Hello, World" "#1"
              |> write "Hey, There"
    Assert.AreEqual([| "Hey"; "There" |], pipe.Value.Value, "#2")
    map
    |> assertRead "Hey, There" "#3"
    |> ignore

[<Test>]
let ArrayMapImplicit() =
    let pipe = Channel.pipe() |> write [| "a"; "b"; "c" |]
    let map = pipe
              |> Channel.map(Iso.ofFn (fun a -> Array.map (fun (s : string) -> s.ToUpperInvariant()) a))
              |> assertRead [| "A"; "B"; "C" |] "#1"
              |> write [| "X"; "Y"; "Z" |]
    Assert.AreEqual([| "x"; "y"; "z" |], pipe.Value.Value, "#2")
    map
    |> assertRead [| "X"; "Y"; "Z" |] "#3"
    |> ignore

[<Test>]
let MapOfSeqImplicit() =
    let pipe = Channel.pipe() |> write (seq { yield ("one", 1); yield ("two", 2); yield ("three", 3) })
    let map = pipe
              |> Channel.map(Iso.ofFn (fun s -> DUMap(Map.ofSeq s)))
              |> assertRead (Map.ofSeq [ ("one", 1); ("two", 2); ("three", 3) ] |> DUMap) "#1"
              |> write (Map.ofSeq [ ("four", 4); ("five", 5); ("six", 6) ] |> DUMap)
    // F# map impl stores keys sorted
    Assert.That(Enumerable.SequenceEqual([ ("five", 5); ("four", 4); ("six", 6) ], pipe.Value.Value), "#2")
    map
    |> assertRead (Map.ofSeq [ ("four", 4); ("five", 5); ("six", 6) ] |> DUMap) "#4"
    |> ignore

[<Test>]
let MapOfSeqPipeImplicit() =
    let pipe = Channel.pipe() |> write (seq { yield ("one", 1); yield ("two", 2); yield ("three", 3) })
    let map = pipe
              |> Channel.map(Iso.ofFn (fun s -> s |> Map.ofSeq |> DUMap))
              |> assertRead (Map.ofSeq [ ("one", 1); ("two", 2); ("three", 3) ] |> DUMap) "#1"
              |> write (Map.ofSeq [ ("four", 4); ("five", 5); ("six", 6) ] |> DUMap)
    // F# map impl stores keys sorted
    Assert.That(Enumerable.SequenceEqual([ ("five", 5); ("four", 4); ("six", 6) ], pipe.Value.Value), "#2")
    map
    |> assertRead (Map.ofSeq [ ("four", 4); ("five", 5); ("six", 6) ] |> DUMap) "#4"
    |> ignore

[<Test>]
let StrIntArrayMapImplicit() =
    let pipe = Channel.pipe() |> write [| 1; 2; 3 |]
    let map = pipe
              |> Channel.map(Iso.ofFn (fun a -> String.Join("", Array.map (fun i -> i.ToString()) a)))
              |> assertRead "123" "#1"
              |> write "456"
    Assert.AreEqual([| 4; 5; 6 |], pipe.Value.Value, "#2")
    map
    |> assertRead "456" "#3"
    |> ignore

[<Test>]
let FunctionImplicit() =
    let pipe = Channel.pipe() |> write "up"
    let map = pipe
              |> Channel.map(Iso.ofFn (function | "up" -> "down" | "left" -> "right" | other -> other))
              |> assertRead "down" "#1"
              |> write "right"
    Assert.AreEqual("left", pipe.Value.Value, "#2")
    pipe.Write "foo"
    map
    |> assertRead "foo" "#3"
    |> ignore

[<Test>]
let FunctionImplicitDifferentTypes() =
    let pipe = Channel.pipe() |> write "one"
    let map = pipe
              |> Channel.map(Iso.ofFn (function | "one" -> 1 | "two" -> 2 | _ -> failwith "nope"))
              |> assertRead 1 "#1"
              |> write 2
    Assert.AreEqual("two", pipe.Value.Value, "#2")
    map
    |> assertRead 2 "#3"
    |> ignore

[<Test>]
let UnionCaseImplicit() =
    let pipe = Channel.pipe() |> write "hello"
    let map = pipe
              |> Channel.map(Iso.ofFn (fun s -> DUStr s))
              |> assertRead (DUStr("hello")) "#1"
              |> write (DUStr("bye"))
    Assert.AreEqual("bye", pipe.Value.Value, "#2")
    map
    |> assertRead (DUStr("bye")) "#3"
    |> ignore

[<Test>]
let UnionCaseImplicitPipeRight() =
    let pipe = Channel.pipe() |> write "hello"
    let map = pipe
              |> Channel.map(Iso.ofFn (fun s -> s |> DUStr))
              |> assertRead (DUStr("hello")) "#1"
              |> write (DUStr("bye"))
    Assert.AreEqual("bye", pipe.Value.Value, "#2")
    map
    |> assertRead (DUStr("bye")) "#3"
    |> ignore

[<Test>]
let TupleImplicit1() =
    let pipe = Channel.pipe() |> write ("a", "B")
    let map = pipe
              |> Channel.map(Iso.ofFn (fun (a, b) -> (a.ToUpperInvariant(), b.ToLowerInvariant())))
              |> assertRead ("A", "b") "#1"
              |> write ("C", "d")
    Assert.AreEqual(("c", "D"), pipe.Value.Value, "#2")
    map
    |> assertRead ("C", "d") "#3"
    |> ignore

[<Test>]
let TupleImplicit2() =
    let pipe = Channel.pipe() |> write (1, 2, "a")
    let map = pipe
              |> Channel.map(Iso.ofFn (fun (a, b, c) -> (a.ToString(), b.ToString(), c.ToUpperInvariant())))
              |> assertRead ("1", "2", "A") "#1"
              |> write ("3", "4", "B")
    Assert.AreEqual((3, 4, "b"), pipe.Value.Value, "#2")
    map
    |> assertRead ("3", "4", "B") "#3"
    |> ignore