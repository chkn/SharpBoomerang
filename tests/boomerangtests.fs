namespace SharpBoomerang.Tests

open System
open NUnit.Framework

open SharpBoomerang
open SharpBoomerang.Combinators

type Title =
    | Mr
    | Ms
    | Dr

type Name = {
    Title : Title option;
    First : string;
    Last  : string;
    }

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
    member __.LitCh() = testCh (blit <?? %"hello") "hello" "hello"

    [<Test>]
    member __.LitTimes() = testSuccess (%3 |> btimes (blit %"foo")) "foofoofoo"

    [<Test>]
    member __.LitNList() = testCh (%3 |> bnlist bdigit) [| '1'; '2'; '3' |] "123"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "\"foo\" at position 6")>]
    member __.LitMultExpected() = testSuccess (%3 |> btimes (blit %"foo")) "foofoo"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "\"hello\" at position 0")>]
    member __.LitExpectedCharacter() = testSuccess (blit %"hello") "he"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "\"hello\" at position 0")>]
    member __.LitExpectedE() = testSuccess (blit %"hello") "holla"

    [<Test>]
    member __.NStr() = testCh (bnstr %5) "hello" "hello"

    [<Test>]
    member __.NGOneOrMore() = testCh (+(bchr %'z') >>. bdigit) '1' "z1"

    [<Test>]
    member __.NGOneOrMoreCh() = testCh (+.bdigit .>> blit %"4") [| '1'; '2'; '3' |] "1234"

    [<Test>]
    member __.ChPropOneOrMore() =
        testCh (!+.bchr .>>% ((fun chrs -> String(Seq.toArray chrs)), (fun str -> str :> char seq))) "hello" "hello"

    [<Test>]
    member __.ChrSList() =
        testCh (bslist bchr (blit %",") .>>% ((fun chrs -> String(Seq.toArray chrs)), (fun str -> str :> char seq))) "hello" "h,e,l,l,o"

    [<Test>]
    member __.PInt() = testCh bpint 123 "123"

    [<Test>]
    member __.ChrPIntChr() =
        testCh (bchr %'a' >>. bpint .>> bchr %'b') 100 "a100b"

    [<Test>]
    member __.PIntToNStr() =
        testCh (bpint |>>. bnstr) "hello" "5hello"

    [<Test>]
    member __.PIntToMult() =
        testSuccess (bpint ??> %3 |>> btimes (blit %"foo")) "3foofoofoo"

    [<Test>]
    member __.Int() =
        testCh bint 123 "123"
        testCh bint -123 "-123"

    [<Test>]
    member __.PFloat() =
        testCh bpfloat 123 "123"
        testCh bpfloat 12.345 "12.345"
 
    [<Test>]
    member __.Float() =
        testCh bfloat 123 "123"
        testCh bfloat 12.345 "12.345"
        testCh bfloat -123 "-123"
        testCh bfloat -12.345 "-12.345"

    [<Test>]
    member __.ChrPFloatChr() =
        testCh (bchr %'a' >>. bpfloat .>> bchr %'b') 123 "a123b"
        testCh (bchr %'a' >>. bpfloat .>> bchr %'b') 12.345 "a12.345b"

    [<Test>]
    member __.NGAtEnd() =
        testCh (+.bdigit) [| '5'; '5'; '5' |] "555"

    [<Test>]
    member __.NGAtEndExplicit() =
        testCh (+.bdigit .>> bend) [| '5'; '5'; '5' |] "555"

    [<Test>]
    member __.Str() =
        testCh (bstr .>> blit %"1") "nirb" "nirb1"
 
    [<Test>]
    member __.DU() = testCh (bdu<Title>) Ms "Ms"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "\"Ms\", \"Dr\", OR \"Mr\" at position 0")>]
    member __.DUExpected() = testCh (bdu<Title>) Ms "Bs"

    [<Test>]
    member __.ManualDU() = testCh ((blit %"Mr" >>% Mr) <.> (blit %"Ms" >>% Ms) <.> (blit %"Dr" >>% Dr)) Dr "Dr"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "\"Ms\", \"Mr\", OR \"Dr\" at position 0")>]
    member __.ManualDUExpected() = testCh ((blit %"Mr" >>% Mr) <.> (blit %"Ms" >>% Ms) <.> (blit %"Dr" >>% Dr)) Dr "Bs"

    [<Test>]
    member __.LitMap() =
        testCh (blit %"five" >>% 5) 5 "five"

    [<Test; ExpectedException(typeof<Expected>, ExpectedMessage = "5 but got 6 at position 0")>]
    member __.LitMapExpected() =
        let b = blit %"five" >>% 5
        b %6 |> printToStr |> ignore

    [<Test>]
    member __.Tuple2Ints() = testCh (bdigit .>>. bdigit) ('1', '2') "12"

    [<Test>]
    member __.Tuple3Ints() =
        let bdigits : Boomerang<(char * char * char)> = bdigit .>>. bdigit .>>. bdigit
        testCh bdigits ('1', '2', '3') "123"

    [<Test>]
    member __.Name() =
        let btitle = ~~~(bdu<Title> .>> ~~(blit %".") %true .>> !+(bws %' '))
        let bname = btitle .>>. bstr .>> !+(bws %' ') .>>. bstr .>>% ((fun (t, f, l) -> { Title = t; First = f; Last = l }), (fun n -> (n.Title, n.First, n.Last)))
        let parseName = StringInputChannel >> parser bname
        let name = parseName "Dr. Albert Einstein"
        Assert.AreEqual(Dr, name.Title.Value)
        Assert.AreEqual("Albert", name.First)
        Assert.AreEqual("Einstein", name.Last)

    [<Test>]
    member __.NCharsFirstSuccessfulRecovery() =
        let bjchr = bnstr %2 // parse 2 characters
                    .>>% Iso.ofFn (function
                                   | "\\b" -> '\b'
                                   | "\\f" -> '\u000C'
                                   | "\\n" -> '\n'
                                   | "\\r" -> '\r'
                                   | "\\t" -> '\t'
                                   | _ -> failwith "Not an escape sequence")
                    <.> bchr
        let bjstr = +.bjchr .>>% ((fun chrs -> String(Seq.toArray chrs)), (fun str -> str.ToCharArray() :> char seq))
        let print = stringPrinter bjstr
        Assert.AreEqual("Hello\\nWorld", print "Hello\nWorld")

    [<Test>]
    member __.FirstSuccessfulRecorvery() =
        let bjchr =
            (blit %"\\b" >>% '\b')     <.>
            (blit %"\\f" >>% '\u000C') <.>
            (blit %"\\n" >>% '\n')     <.>
            (blit %"\\r" >>% '\r')     <.>
            (blit %"\\t" >>% '\t')     <.>
            (blit %"\\\"" >>% '"')     <.>
            bchr
        let bjstr =
            // Define an Iso for converting a character sequence to a string
            let charsToStr : Iso<_,_> = ((fun chrs -> String(Seq.toArray chrs)),
                                         (fun str  -> str.ToCharArray() :> char seq))
            // Boomerang a quoted JSON string
            blit %"\"" >>. (+.bjchr .>>% charsToStr) .>> blit %"\"" .>>% Iso.ofFn(fun str -> DUStr str)
        let bjnum = bfloat .>>% Iso.ofFn(fun num -> DUFloat num)
        let rec bjlist jlist =
            blit %"[" >>. bslist bjson (bws0 >> blit %"," >> bws0) .>> blit %"]" .>>% Iso.ofFn(fun seq -> DUList seq) <| jlist
        and bjson =
            bjstr <.>
            bjnum <.>
            bjlist
        let print = stringPrinter bjson
        Assert.AreEqual("[5,6,\"Hello\\nWorld\"]", print(DUList([| DUFloat(5.0); DUFloat(6.0); DUStr("Hello\nWorld") |])))

    [<Test>]
    member __.TupleList() =
        let bpair = bstr .>>. bpint
        testCh (bslist bpair (bws %' ')) [| ("a", 1); ("b", 2); ("c", 3) |] "a1 b2 c3"

    [<Test>]
    member __.GreedyRepeatRecursive() =
        let rec blst lst = !+.brec .>>% Iso.ofFn(fun l -> DUList l) <| lst
        and brec =
            blst    <.>
            (bstr   .>>% Iso.ofFn(fun s -> DUStr s)) <.>
            (bfloat .>>% Iso.ofFn(fun n -> DUFloat n))
        let print = stringPrinter brec
        Assert.AreEqual("aaa456bbb321", print(DUList([| DUStr("aaa"); DUFloat(456.0); DUStr("bbb"); DUFloat(321.0) |])))

    [<Test>]
    member __.SListRecursive() =
        let rec blst lst = bslist brec (blit %",") .>>% Iso.ofFn(fun l -> DUList l) <| lst
        and brec =
            blst    <.>
            (bstr   .>>% Iso.ofFn(fun s -> DUStr s)) <.>
            (bfloat .>>% Iso.ofFn(fun n -> DUFloat n))
        let print = stringPrinter brec
        Assert.AreEqual("aaa,456,bbb,321", print(DUList([| DUStr("aaa"); DUFloat(456.0); DUStr("bbb"); DUFloat(321.0) |])))
