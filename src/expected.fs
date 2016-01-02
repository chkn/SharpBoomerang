namespace SharpBoomerang

open System

type Position = {
    Row : int option
    Col : int
    } with
    override this.ToString() =
        match this.Row with
        | Some r -> sprintf "%d:%d" r (this.Col)
        | _ -> sprintf "position %d" (this.Col)

/// An exception thrown for parse errors
type Expected(what : string, where : Position option, inner : Exception) =
    inherit Exception(what + (match where with | Some w -> sprintf " at %O" w | _ -> ""), inner)
    new(wrapped : Exception, where) = Expected(wrapped.Message, where, wrapped)
    member __.Expected = what
    member __.Position = where

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Expected =

    /// Raises an `Expected` exception
    let inline expected what = raise(Expected(what, None, null))

    /// Raises an `Expected` exception
    let inline expectedAt where what = raise(Expected(what, Some where, null))

    /// Constructs an `Expected` exception from a list of exceptions.
    ///  If the list contains `Expected` exceptions at the same `Position`, their messages are
    ///  coalesced into a more human-readable message.
    let ofList(lst : exn list) =
        let rec filterEx (e : exn) =
            match e with
            | :? AggregateException as ae -> ae.InnerExceptions |> Seq.collect filterEx |> List.ofSeq
            | :? Expected as e when e.InnerException <> null -> e.InnerException |> filterEx
            | _ -> [e]
        let lst = lst |> List.collect filterEx
        let agg = AggregateException(lst)
        let rec iter dupes leftover (l : exn list) =
            match dupes, l with
            | [], (:? Expected as e) :: tl -> iter [e] leftover tl
            | (d :: _), (:? Expected as e) :: tl when e.Position = d.Position -> iter (e :: dupes) leftover tl
            | _, (e :: tl) -> iter dupes (e :: leftover) tl
            | _, [] -> dupes, leftover
        match iter [] [] lst with
        | [], l -> Expected(String.Join(" OR ", l |> List.map (fun e -> e.Message)), None, agg)
        | dupes, leftover ->
            let last, dupes, pos =
                match dupes, leftover with
                | (d :: dtl), [] -> (sprintf ", OR %s" d.Expected), dtl, d.Position
                | _, _  -> "", dupes, None
            let dupeStr = String.Join(", ", dupes |> List.map (fun e -> e.Expected)) +
                          (match pos, dupes with
                           | None, (d :: _) when d.Position.IsSome -> sprintf " at %O" d.Position.Value
                           | _, _ -> "")
            Expected(String.Join(" OR ", dupeStr :: (leftover |> List.map (fun e -> e.ToString()))) + last, pos, agg)