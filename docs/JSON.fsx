(*** hide ***)
#load @"../SharpBoomerang.fsx"
open System
open SharpBoomerang
open SharpBoomerang.Combinators

(**
Parsing JSON
============

FParsec has JSON parsing as one of its [tutorials][0]. I adapted the code from
the tutorial and ported it to SharpBoomerang. This document will be fleshed out
with more discussion in the future.

[0]: http://www.quanttec.com/fparsec/tutorial.html#parsing-json
*)

type Json =
    | JNull
    | JBool   of bool
    | JString of string
    | JNumber of float

    | JList   of Json seq
    | JObject of Map<string, Json>

/// Boomerangs a JNull
let bjnull = blit %"null" >>% JNull

/// Boomerangs a JBool
let bjbool = (blit %"true" >>% JBool true) <.>
             (blit %"false" >>% JBool false)

/// Boomerangs a (possibly escaped) character. Doesn't handle Unicode escapes.
let bjchr =
    // Define a boomerang to parse any character that isn't a backslash
    let bchrnoslash = Channel.expectFn "no slash" (fun c -> c <> '\\') >> bchr

    // Boomerang a possibly-escaped character with the first-successful parse operator, <.>
    (blit %"\\b" >>% '\b') <.>
    (blit %"\\f" >>% '\f') <.>
    (blit %"\\n" >>% '\n') <.>
    (blit %"\\r" >>% '\r') <.>
    (blit %"\\t" >>% '\t') <.>
    (blit %"\\\"" >>% '"') <.>
    // Any character that isn't a slash simply yields itself
    bchrnoslash            <.>
    // Any other character prefixed by a slash simply yields that character
    //  This rule must come after the previous one to prevent a slash from always being printed.
    (blit %"\\" >>. bchr)

/// Boomerangs a JString
let bjstr =
    // Define an Iso for converting a character sequence to a string
    let charsToStr : Iso<_,_> = ((fun chrs -> String(Seq.toArray chrs)),
                                 (fun str  -> str.ToCharArray() :> char seq))
    // Boomerang a quoted JSON string
    blit %"\"" >>. (+.bjchr .>>% charsToStr) .>> blit %"\"" .>>% Iso.ofFn(fun str -> JString str)

/// Boomerangs a JNumber. Doesn't handle exponent notation
let bjnum = bfloat .>>% Iso.ofFn(fun num -> JNumber num)

/// Boomerangs a JList
// Note that we need to make the arg explicit here to make f# happy :/
let rec bjlist jlist =
    blit %"[" >>. bslist bjson (blit %",") .>> blit %"]" .>>% Iso.ofFn(fun seq -> JList seq) <| jlist

/// Boomerangs a JObject
// Again note that we need to make the arg explicit here to make f# happy :/
and bjobj jobj =
    // Boomerang a key-value pair
    let bkv = bjstr .>> bws0 .>> blit %":" .>>. bjson .>>% ((fun (JString k, v) -> (k, v)), (fun (k, v) -> (JString k, v)))

    // Boomerang the object
    blit %"{" >> bws0 >>. bslist bkv (bws0 >> blit %"," >> bws0) .>> bws0 .>> blit %"}" .>>% Iso.ofFn(fun seq -> seq |> Map.ofSeq |> JObject) <| jobj

/// Boomerangs a Json
and bjson =
    bws0    >>. // optional whitespace
    (bjnull <.>
     bjbool <.>
     bjstr  <.>
     bjnum  <.>
     bjlist <.>
     bjobj) .>>
    bws0 // optionally ending in whitespace


let parseJson = StringInputChannel >> parser bjson
let printJson = stringPrinter bjson

(**
Now we can parse/print some JSON:
*)

let json = parseJson """
{
    "str": "value",
    "num": 10.5,
    "bln": true,
    "nul": null,
    "lst": [4, 5, "a", "b"]
}
"""

printJson json
