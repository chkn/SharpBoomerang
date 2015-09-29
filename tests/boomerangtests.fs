namespace SharpBoomerang.Tests

open System
open NUnit.Framework

open SharpBoomerang
open SharpBoomerang.Combinators

[<TestFixture>]
type BoomerangTests() =

    let assertParsePrint b expected input =
        let holder = Channel.pipe()
        b holder |> parseFromStr input
        Assert.AreEqual(expected :> obj, holder.Value.Value)
        Assert.AreEqual(input, b holder |> printToStr)

    [<Test>]
    member __.Char() = assertParsePrint bchr 'h' "h"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "<end of stream> at position 1")>]
    member __.CharEndOfStr() = (bchr %'h' >> bend) |> parseFromStr "hello"

    [<Test>]
    member __.Lit() = assertParsePrint (blit <+^ %"hello") "hello" "hello"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "<character> at position 2")>]
    member __.LitExpectedCharacter() = assertParsePrint (blit <+^ %"hello") "hello" "he"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "'e' in 'hello' at position 2")>]
    member __.LitExpectedE() = assertParsePrint (blit <+^ %"hello") "hello" "holla"

    [<Test>]
    member __.NStr() = assertParsePrint (bnstr %5) "hello" "hello"

    [<Test>]
    member __.IntToNStr() = ()
