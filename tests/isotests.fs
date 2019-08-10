[<NUnit.Framework.TestFixture>]
module SharpBoomerang.Tests.Iso

open System
open System.Linq

open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open SharpBoomerang

[<AutoOpen>]
module private Properties =
    let inline forValues val1 val2 (a, b) =
        "functions are inverses" @| lazy
            (b (a val1)) = val1 && (a (b val2)) = val2

    let inline forSeqValues val1 val2 (a, b) =
        "functions are inverses" @| lazy
            Enumerable.SequenceEqual((b (a val1)), val1) &&
            (a (b val2)) = val2

    let forSpecificValues lst (a, b) =
        "functions are inverses" @| lazy
            List.forall(fun (val1, val2) -> (a val1) = val2 && (b val2) = val1) lst

// Numerical:

[<Property>]
let ``explicit Iso of int addition`` a b =
    ((fun i -> i + 1), (fun i -> i - 1))
    |> forValues a b

[<Property>]
let ``implicit Iso of int addition`` a b =
    Iso.ofFn (fun i -> i + 1)
    |> forValues a b

[<Property>]
let ``explicit Iso of int subtraction`` a b =
    ((fun i -> i - 1), (fun i -> i + 1))
    |> forValues a b

[<Property>]
let ``implicit Iso of int subtraction`` a b =
    Iso.ofFn (fun i -> i - 1)
    |> forValues a b

type DivisibleByTwo =
    static member Int32() =
        Arb.Default.Int32()
        |> Arb.mapFilter ((*) 2) (fun i -> i % 2 = 0)

[<Property(Arbitrary=[| typeof<DivisibleByTwo> |])>]
let ``explicit Iso of int multiplication`` a b =
    ((fun i -> i * 2), (fun i -> i / 2))
    |> forValues a b

[<Property(Arbitrary=[| typeof<DivisibleByTwo> |])>]
let ``implicit Iso of int multiplication`` a b =
    Iso.ofFn (fun i -> i * 2)
    |> forValues a b

[<Property(Arbitrary=[| typeof<DivisibleByTwo> |])>]
let ``explicit Iso of int division`` a b =
    ((fun i -> i / 2), (fun i -> i * 2))
    |> forValues a b

[<Property(Arbitrary=[| typeof<DivisibleByTwo> |])>]
let ``implicit Iso of int division`` a b =
    Iso.ofFn (fun i -> i / 2)
    |> forValues a b

// String:

[<Property>]
let ``explicit Iso of String.ToUpper`` (NonNull a) (NonNull b) =
    ((fun (s : string) -> s.ToUpper()), (fun (s : string) -> s.ToLower()))
    |> forValues ((a : string).ToLower()) ((b : string).ToUpper())

[<Property>]
let ``implicit Iso of String.ToUpper`` (NonNull a) (NonNull b) =
    Iso.ofFn (fun (s : string) -> s.ToUpper())
    |> forValues ((a : string).ToLower()) ((b : string).ToUpper())

[<Property>]
let ``explicit Iso of String.ToLower`` (NonNull a) (NonNull b) =
    ((fun (s : string) -> s.ToLower()), (fun (s : string) -> s.ToUpper()))
    |> forValues ((a : string).ToUpper()) ((b : string).ToLower())

[<Property>]
let ``implicit Iso of String.ToLower`` (NonNull a) (NonNull b) =
    Iso.ofFn (fun (s : string) -> s.ToLower())
    |> forValues ((a : string).ToUpper()) ((b : string).ToLower())

[<Property>]
let ``explicit Iso of String.ToUpperInvariant`` (NonNull a) (NonNull b) =
    ((fun (s : string) -> s.ToUpperInvariant()), (fun (s : string) -> s.ToLowerInvariant()))
    |> forValues ((a : string).ToLowerInvariant()) ((b : string).ToUpperInvariant())

[<Property>]
let ``implicit Iso of String.ToUpperInvariant`` (NonNull a) (NonNull b) =
    Iso.ofFn (fun (s : string) -> s.ToUpperInvariant())
    |> forValues ((a : string).ToLowerInvariant()) ((b : string).ToUpperInvariant())

[<Property>]
let ``explicit Iso of String.ToLowerInvariant`` (NonNull a) (NonNull b) =
    ((fun (s : string) -> s.ToLowerInvariant()), (fun (s : string) -> s.ToUpperInvariant()))
    |> forValues ((a : string).ToUpperInvariant()) ((b : string).ToLowerInvariant())

[<Property>]
let ``implicit Iso of String.ToLowerInvariant`` (NonNull a) (NonNull b) =
    Iso.ofFn (fun (s : string) -> s.ToLowerInvariant())
    |> forValues ((a : string).ToUpperInvariant()) ((b : string).ToLowerInvariant())

[<Property>]
let ``implicit Iso of String.Join`` (NonEmptyArray a) (NonNull b) =
    Iso.ofFn (fun a -> String.Join(", ", (a : NonNull<string> array)))
    |> forValues a b

// Array:

[<Property>]
let ``implicit Iso of Array.map uncurried`` a b =
    Iso.ofFn (fun a -> Array.map (fun i -> i + 1) a)
    |> forValues a b

[<Property>]
let ``implicit Iso of Array.map curried`` a b =
    Iso.ofFn (Array.map (fun i -> i + 1))
    |> forValues a b

[<Property>]
let ``implicit Iso of Array.map and String.Join`` (a : PositiveInt array) b =
    Iso.ofFn (fun a -> String.Join("", Array.map (fun (PositiveInt i) -> i.ToString()) a))
    |> forValues a b

// Map:

type MapCase = MapCase of Map<string,int>

[<Property>]
let ``implicit Iso of Map.ofList`` a (b : MapCase) =
    Iso.ofFn (fun s -> MapCase(Map.ofList s))
    |> forValues [a] b

[<Property>]
let ``implicit Iso of Map.ofSeq`` (a : (string * int)) (b : MapCase) =
    Iso.ofFn (fun (s : seq<_>) -> MapCase(Map.ofSeq s))
    |> forSeqValues ([a] :> _) b

[<Property>]
let ``implicit Iso of Map.ofSeq piped`` (a : (string * int)) (b : MapCase) =
    Iso.ofFn (fun (s : seq<_>) -> s |> Map.ofSeq |> MapCase)
    |> forSeqValues ([a] :> _) b

// Functions:

[<Property>]
let ``implicit Iso of function mapping same types`` () =
    Iso.ofFn (function | "up" -> "down" | "left" -> "right" | other -> other)
    |> forSpecificValues [
        "up", "down"
        "left", "right"
        "foo", "foo"
    ]

[<Property>]
let ``implicit Iso of function mapping different types`` () =
    Iso.ofFn (function | "one" -> 1 | "two" -> 2 | _ -> failwith "nope")
    |> forSpecificValues [
        "one", 1
        "two", 2
    ]

// Tuples:

[<Property>]
let ``implicit Iso of homogeneous tuple`` a b =
    Iso.ofFn (fun (a, b) -> a + 1, b - 1)
    |> forValues a b

[<Property>]
let ``implicit Iso of heterogeneous tuple`` a b =
    Iso.ofFn (fun (a, b, c : string) -> a + 1, b - 1, String(c.Reverse().ToArray()))
    |> forValues a b

// Discriminated Unions:

type DU =
    | DUStr of string
    | DUFloat of float
    | DUFloatStr of float * string
    | DUList of DU seq
    | DUMap of Map<string,int>

[<Property>]
let ``implicit Iso of union single arg`` a b =
    Iso.ofFn (fun s -> DUStr s)
    |> forValues a b

[<Property>]
let ``implicit Iso of union single arg piped`` a b =
    Iso.ofFn (fun s -> s |> DUStr)
    |> forValues a b

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

[<Property>]
let ``implicit Iso of union two args`` a b =
    Iso.ofFn(fun (f, s) -> DUFloatStr(f, s))
    |> forValues a b

type SingleArg = SingleArg of string

[<Property>]
let ``implicit Iso of decomposing union single arg`` a b =
    Iso.ofFn(fun (SingleArg s) -> s)
    |> forValues a b

type TwoArgs = TwoArgs of string * int

[<Property>]
let ``implicit Iso of decomposing union two args`` a b =
    Iso.ofFn(fun (TwoArgs(s, i)) -> (s, i))
    |> forValues a b

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
let FunctionImplicitDifferentTypes1() =
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
let FunctionImplicitDifferentTypes2() =
    let pipe = Channel.pipe() |> write (DUStr("one"))
    let map = pipe
              |> Channel.map(Iso.ofFn (function | DUStr("one") -> DUFloat(1.0) | DUStr("two") -> DUFloat(2.0) | _ -> raise(Exception("nope"))))
              |> assertRead (DUFloat(1.0)) "#1"
              |> write (DUFloat(2.0))
    Assert.AreEqual(DUStr("two"), pipe.Value.Value, "#2")
    map
    |> assertRead (DUFloat(2.0)) "#3"
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

[<Test>]
let UnionCaseTupleImplicit1() =
    let pipe = Channel.pipe() |> write (1.0, "one")
    let map = pipe
              |> Channel.map(Iso.ofFn(fun (f, s) -> DUFloatStr(f, s)))
              |> assertRead (DUFloatStr(1.0, "one")) "#1"
              |> write (DUFloatStr(2.0, "two"))
    Assert.AreEqual((2.0, "two"), pipe.Value.Value, "#2")
    map
    |> assertRead (DUFloatStr(2.0, "two")) "#3"
    |> ignore

