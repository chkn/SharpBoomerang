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

//[<Property>]
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

// Other types:

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

[<Property>]
let ``implicit Iso of homogeneous tuple`` a b =
    Iso.ofFn (fun (a, b) -> a + 1, b - 1)
    |> forValues a b

[<Property>]
let ``implicit Iso of heterogeneous tuple`` a b =
    Iso.ofFn (fun (a, b, c : string) -> a + 1, b - 1, String(c.Reverse().ToArray()))
    |> forValues a b
