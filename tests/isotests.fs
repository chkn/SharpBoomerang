namespace SharpBoomerang.Tests

open System
open NUnit.Framework

open SharpBoomerang

type DU =
    | DUStr of string
    | DUFloat of float
    | DUList of DU seq

[<TestFixture>]
type IsoTests() =

    let write value (ch : #IChannel<_>) = ch.Write(value); ch
    let assertRead (expected : 't) (str : string) (ch : IChannel<'t>) =
        ch.Read(fun v -> Assert.AreEqual(expected, v, str))
        ch

    let testNum v1 v2 iso =
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
    [<Test>] member x.AddExplicit() = ((fun i -> i + 1), (fun i -> i - 1)) |> testNum 7 1
    [<Test>] member x.AddImplicit() = Iso.ofFn (fun i -> i + 1) |> testNum 7 1

    [<Test>] member x.SubExplicit() = ((fun i -> i - 1), (fun i -> i + 1)) |> testNum 5 3
    [<Test>] member x.SubImplicit() = Iso.ofFn (fun i -> i - 1) |> testNum 5 3

    [<Test>] member x.MulExplicit() = ((fun i -> i * 2), (fun i -> i / 2)) |> testNum 12 1
    [<Test>] member x.MulImplicit() = Iso.ofFn (fun i -> i * 2) |> testNum 12 1

    [<Test>] member x.DivExplicit() = ((fun i -> i / 2), (fun i -> i * 2)) |> testNum 3 4
    [<Test>] member x.DivImplicit() = Iso.ofFn (fun i -> i / 2) |> testNum 3 4

    // String:
    [<Test>] member x.ToUpperExplicit() = ((fun (s : string) -> s.ToUpper()), (fun (s : string) -> s.ToLower())) |> testStr "HELLO, WORLD!" "so long!"
    [<Test>] member x.ToUpperImplicit() = Iso.ofFn (fun (s : string) -> s.ToUpper()) |> testStr "HELLO, WORLD!" "so long!"

    [<Test>] member x.ToLowerExplicit() = ((fun (s : string) -> s.ToLower()), (fun (s : string) -> s.ToUpper())) |> testStr "hello, world!" "SO LONG!"
    [<Test>] member x.ToLowerImplicit() = Iso.ofFn (fun (s : string) -> s.ToLower()) |> testStr "hello, world!" "SO LONG!"

    [<Test>] member x.ToUpperInvExplicit() = ((fun (s : string) -> s.ToUpperInvariant()), (fun (s : string) -> s.ToLowerInvariant())) |> testStr "HELLO, WORLD!" "so long!"
    [<Test>] member x.ToUpperInvImplicit() = Iso.ofFn (fun (s : string) -> s.ToUpperInvariant()) |> testStr "HELLO, WORLD!" "so long!"

    [<Test>] member x.ToLowerInvExplicit() = ((fun (s : string) -> s.ToLowerInvariant()), (fun (s : string) -> s.ToUpperInvariant())) |> testStr "hello, world!" "SO LONG!"
    [<Test>] member x.ToLowerInvImplicit() = Iso.ofFn (fun (s : string) -> s.ToLowerInvariant()) |> testStr "hello, world!" "SO LONG!"

    [<Test>]
    member x.JoinImplicit() =
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
    member x.ArrayMapImplicit() =
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
    member x.StrIntArrayMapImplicit() =
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
    member x.FunctionImplicit() =
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
    member x.FunctionImplicitDifferentTypes() =
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
    member x.UnionCaseImplicit() =
        let pipe = Channel.pipe() |> write "hello"
        let map = pipe
                  |> Channel.map(Iso.ofFn (fun s -> DUStr s))
                  |> assertRead (DUStr("hello")) "#1"
                  |> write (DUStr("bye"))
        Assert.AreEqual("bye", pipe.Value.Value, "#2")
        map
        |> assertRead (DUStr("bye")) "#3"
        |> ignore