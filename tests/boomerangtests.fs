namespace SharpBoomerang.Tests

open System
open NUnit.Framework

open SharpBoomerang
open SharpBoomerang.Combinators

[<TestFixture>]
type BoomerangTests() =

    let testSuccess b input =
        b |> parseFromStr input
        Assert.AreEqual(input, b |> printToStr)

    let testCh b expected input =
        let holder = Channel.pipe()
        b holder |> parseFromStr input
        Assert.AreEqual(expected :> obj, holder.Value.Value)
        Assert.AreEqual(input, b holder |> printToStr)

    [<Test>]
    member __.Char() = testCh bchr 'h' "h"

    [<Test>]
    member __.Digit() = testCh bdigit '5' "5"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "digit at position 0")>]
    member __.DigitExpected() = testCh bdigit 'X' "X"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "<end of stream> at position 1")>]
    member __.CharEndOfStr() = (bchr %'h' >> bend) |> parseFromStr "hello"

    [<Test>]
    member __.Lit() = testCh (blit <+. %"hello") "hello" "hello"

    [<Test>]
    member __.LitMult() = testSuccess (blit %"foo" @* %3) "foofoofoo"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "\"foo\" at position 6")>]
    member __.LitMultExpected() = testSuccess (blit %"foo" @* %3) "foofoo"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "\"hello\" at position 0")>]
    member __.LitExpectedCharacter() = testCh (blit <+. %"hello") "hello" "he"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "\"hello\" at position 0")>]
    member __.LitExpectedE() = testCh (blit <+. %"hello") "hello" "holla"

    [<Test>]
    member __.NStr() = testCh (bnstr %5) "hello" "hello"

    [<Test>]
    member __.NGOneOrMore() = testCh (+(bchr %'z') >>. bdigit) '1' "z1"

    [<Test>]
    member __.NGOneOrMoreCh() = testCh (+.bdigit .>> blit %"4") [| '1'; '2'; '3' |] "1234"

    [<Test>]
    member __.ChPropOneOrMore() =
        testCh (!+.(bchr) >>| ((fun chrs -> String(Seq.toArray chrs)), (fun str -> str :> char seq))) "hello" "hello"

    [<Test>]
    member __.PInt() = testCh bpint 123 "123"

    [<Test>]
    member __.ChrPIntChr() =
        testCh (bchr %'a' >>. bpint .>> bchr %'b') 100 "a100b"

    [<Test>]
    member __.PIntToNStr() =
        testCh (bpint |>>. bnstr) "hello" "5hello"