﻿namespace SharpBoomerang

open System
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open SharpBoomerang.Expected

[<AllowNullLiteral>]
type IMark =
    inherit IDisposable
    /// Seeks back to the point represented by this mark
    abstract Rewind : unit -> unit

[<Sealed>]
type NullMark private () =
    static let inst = new NullMark() :> IMark
    static member Instance = inst
    interface IMark with
        member __.Rewind()  = ()
        member __.Dispose() = ()

/// Represents a bidirectional data source with Read and Write operations.
type IChannel<'t> =
    /// Reads this `IChannel` and calls the passed function with the result.
    abstract Read : ('t -> unit) -> unit

    /// Writes a`'t` into this `IChannel`.
    /// If this instance represents a read-only channel, then this method must do nothing.
    abstract Write : 't -> unit

    /// Obtains an `IMark` for this `IChannel`'s current state.
    abstract Mark : unit -> IMark

type channel<'t> = IChannel<'t>

/// A channel for piping between combinators.
/// If the channel is read and there is no value available, the callback
/// will be saved and called on the next `Write` operation.
[<Sealed>]
type PipeChannel<'t>() =
    let mutable value = None
    let mutable valueCallback = Unchecked.defaultof<Action<'t>>
    member __.Value = value
    member __.Read ret =
        match value with
        | Some v -> ret v
        | _ -> valueCallback <- Delegate.Combine(valueCallback, Action<'t>(ret)) :?> _
    member __.Write v =
        value <- Some v
        if valueCallback <> null then
            valueCallback.Invoke(v)
            valueCallback <- null
    member __.Mark() =
            let mark = value
            { new IMark with
                 member __.Rewind() = value <- mark
                 member __.Dispose() = () }
    interface IChannel<'t> with
        member this.Read ret = this.Read ret
        member this.Write v  = this.Write v
        member this.Mark()   = this.Mark()

/// A channel for directly accessing a mutable data source.
[<Sealed>]
type ExprChannel<'t>(expr : Expr<'t>) =
    static let rec getExpr = function
    | Value(value, _) -> value
    | PropertyGet(None, pi, _) -> pi.GetValue(null, null)
    | PropertyGet(Some(expr), pi, _) -> pi.GetValue(getExpr expr, null)
    | u -> failwithf "Unsupported Expr: %A" u
    static let setExpr v = function
    | ValueWithName(_, _, nm) -> failwithf "Cannot mutate this binding: %s" nm
    | Value(_, _) -> () // do nothing so behavior matches ConstChannel
    | PropertyGet(None, pi, _) -> pi.SetValue(null, v, null)
    | PropertyGet(Some(expr), pi, _) -> pi.SetValue(getExpr expr, v, null)
    | u -> failwithf "Unsupported Expr: %A" u
    let read() = getExpr expr :?> 't
    member __.Read ret = read() |> ret
    member __.Write v  = setExpr v expr
    member this.Mark() =
        let value = read()
        { new IMark with
             member __.Rewind() = this.Write(value)
             member __.Dispose() = () }
    interface IChannel<'t> with
        member this.Read ret = this.Read ret
        member this.Write v  = this.Write v
        member this.Mark()   = this.Mark()

/// A channel for decomposing a sequence into its elements
type DecomposeChannel<'t>(ch : IChannel<'t seq>, shouldFlush : List<'t> -> bool) =
    let buf = List<'t>()
    let mutable i = 0
    let mutable marks = 0
    let rec read ret tryFillBuffer =
        if i < buf.Count then
            let v = buf.[i]
            if marks <= 0 then
                buf.RemoveRange(0, i + 1)
                i <- 0
            else
                i <- i + 1
            ret v
        elif tryFillBuffer then
            ch.Read(fun v ->
                buf.AddRange(v)
                read ret false
            )
    new(ch : IChannel<'t array>, shouldFlush : List<'t> -> bool) =
        DecomposeChannel(
            {
                new IChannel<'t seq> with
                    member __.Read ret = ch.Read(ret)
                    member __.Write v  = ch.Write(v :?> 't array)
                    member __.Mark()   = ch.Mark()
            }, shouldFlush)
    member __.Count = buf.Count - i
    member __.Flush() =
        let arry = buf.ToArray()
        buf.Clear()
        i <- 0
        ch.Write(arry)
    member this.Write v  =
        buf.Add(v)
        if shouldFlush buf then
            this.Flush()
    member this.Read ret = read ret true
    member this.Mark() =
        let mark = i
        let submark = if i >= buf.Count then ch.Mark() else null
        marks <- marks + 1
        { new IMark with
             member __.Rewind()  =
                i <- mark
                if submark <> null then
                    buf.RemoveRange(i, buf.Count - i)
                    submark.Rewind()
             member __.Dispose() =
                marks <- marks - 1
                if submark <> null then
                    submark.Dispose()
        }
    interface IChannel<'t> with
        member this.Read ret = this.Read ret
        member this.Write v  = this.Write v
        member this.Mark()   = this.Mark()

[<RequireQualifiedAccess>]
module Channel =
    // Channel creation:

    /// Creates a PipeChannel, generally used for piping between combinators.
    let inline pipe() = PipeChannel<_>()

    /// Creates a PipeChannel with the given initial value.
    let inline pipeWith value =
        let ch = PipeChannel<_>()
        ch.Write(value)
        ch

    /// Returns a channel that reads/writes to the given mutable destination.
    let inline ofExpr e = ExprChannel(e) :> IChannel<_>

    /// Returns a read-only channel that is initialized with a constant value.
    ///  Any writes to the returned channel are ignored.
    let ofValue v =
        {
            new IChannel<_> with
                member __.Read ret = ret v
                member __.Write _  = ()
                member __.Mark()   = NullMark.Instance
        }

    // Channel ops:

    /// Returns a channel that wraps the given channel as read-only.
    ///  Any writes to the returned channel are ignored.
    let toReadOnly (ch : IChannel<_>) =
        {
            new IChannel<_> with
                member __.Read ret = ch.Read(ret)
                member __.Write _  = ()
                member __.Mark()   = ch.Mark()
        }

    /// Tries to read from the given channel and then resets the channel
    ///  to its state prior to the read.
    let ensureReadable (ch : IChannel<_>) =
        use mark = ch.Mark()
        try
            ch.Read(ignore)
        finally
            mark.Rewind()

    /// Reads a number from the first channel, and then collects that number of values from second channel
    let collect (cnt : int channel) (ch : IChannel<'t>) =
        {
            new IChannel<'t array> with
                member __.Write v =
                    cnt.Write(v.Length)
                    for item in v do
                        ch.Write(item)
                member __.Read ret =
                    cnt.Read(fun n ->
                        let input = Array.zeroCreate<'t>(n)
                        if n <= 0 then
                            ret input
                        else
                            let index = ref 0
                            let rec readOne() =
                                ch.Read(fun v -> 
                                    let mutable i = !index
                                    input.[i] <- v
                                    i <- i + 1
                                    if i < n then
                                        index := i
                                        readOne()
                                    else
                                        ret input
                                )
                            readOne()
                    )
                member __.Mark() = ch.Mark()
        }

    let inline decomposeSeq shouldFlush (ch : IChannel<'t seq>) = DecomposeChannel(ch, shouldFlush)
    let inline decomposeArray shouldFlush (ch : IChannel<'t array>) = DecomposeChannel(ch, shouldFlush)

    type private ExpectChannel<'t>(ch : 't channel, check : ('t -> unit) -> 't -> unit) =
        interface IChannel<'t> with
            member __.Read ret = ch.Read(check ret)
            member __.Write nv = nv |> check ch.Write
            member __.Mark()   = ch.Mark()

    /// Returns a channel that expects a certain value to be read or written
    ///  to/from the given channel. If a different value is read or written,
    ///  throws an exception. The value to be expected is read from a channel.
    let expect (exp : IChannel<_>) (ch : IChannel<_>) =
        let check ret value = exp.Read(fun v -> if value = v then ret v else sprintf "%A" v |> expected)
        ExpectChannel(ch, check) :> IChannel<_>

    /// Returns a channel that expects a certain value to be read or written
    ///  to/from the given channel. If a different value is read or written,
    ///  throws an exception with the given description. The expected value is determined by a function.
    let expectFn (desc : string) (check : 't -> bool) (ch : IChannel<_>) =
        let check ret value = if check value then ret value else expected desc
        ExpectChannel(ch, check) :> IChannel<_>

    /// Maps one channel type onto another using an Iso
    let map (f1, f2) (ch : IChannel<_>) =
        {
            new IChannel<_> with
                member __.Read ret = ch.Read(fun v -> f1 v |> ret)
                member __.Write v  = ch.Write(f2 v)
                member __.Mark()   = ch.Mark()
        }

    /// Maps one channel into 2 separate channels using 3 functions.
    ///  The returned channels exhibit the following behavior:
    ///    - Both channels must be written before the underlying channel is written.
    ///    - Both channels read from the underlying channel; a read from one of the returned
    ///       channels will affect the value of the next read from the other.
    let map2 f1 f2 f3 (ch : IChannel<_>) =
        let mutable holder1 = None
        let mutable holder2 = None
        let doWrite() =
            match holder1, holder2 with
            | Some(v1), Some(v2) -> ch.Write(f3 v1 v2)
            | _ -> ()
        {
            new IChannel<_> with
                member __.Read ret = ch.Read(fun v -> f1 v |> ret)
                member __.Write v = 
                    holder1 <- Some v
                    doWrite()
                member __.Mark() = ch.Mark()
        },
        {
            new IChannel<_> with
                member __.Read ret = ch.Read(fun v -> f2 v |> ret)
                member __.Write v =
                    holder2 <- Some v
                    doWrite()
                member __.Mark() = ch.Mark()
        }

    /// Maps the given channel into a quoted string
    let quote<'t
     #if !NETFX_CORE
     when 't :> IConvertible
     #endif
     > (ch : IChannel<'t>) =
        ch
        |> map ((fun v -> sprintf "\"%O\"" v), (fun str -> Convert.ChangeType(str.Trim('"'), typeof<'t>) |> unbox))