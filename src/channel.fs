namespace SharpBoomerang

open System
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

/// Represents a bidirectional data source with Read and Write operations.
type IChannel<'t> =
    /// Reads this `IChannel` and calls the passed function with the result.
    abstract Read : ('t -> unit) -> unit

    /// Writes a`'t` into this `IChannel`.
    /// If this instance represents a read-only channel, then this method must do nothing.
    abstract Write : 't -> unit

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
        | None   ->
            valueCallback <- Delegate.Combine(valueCallback, Action<'t>(ret)) :?> _
    member __.Write v =
        value <- Some v
        if valueCallback <> null then
            valueCallback.Invoke(v)
            valueCallback <- null
    interface IChannel<'t> with
        member this.Read ret = this.Read ret
        member this.Write v = this.Write v

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
    interface IChannel<'t> with
        member __.Read ret = getExpr expr :?> 't |> ret
        member __.Write v  = setExpr v expr

/// A channel for decomposing a sequence into its elements
type DecomposeChannel<'t>(ch : IChannel<'t seq>, shouldFlush : List<'t> -> bool) =
    let buf = List<'t>()
    let dequeue() =
        if buf.Count = 0 then None else
        let result = buf.[0]
        buf.RemoveAt(0)
        Some result
    member __.Count = buf.Count
    member __.Flush() =
        let arry = buf.ToArray()
        buf.Clear()
        ch.Write(arry)
    interface IChannel<'t> with
        member this.Write v  =
            buf.Add(v)
            if shouldFlush buf then
                this.Flush()
        member this.Read ret =
            // It is important to requeue the value if the read fails.
            let guardedRet result =
                try ret result with _ ->
                    buf.Insert(0, result)
                    reraise()
            match dequeue() with
            | Some result -> guardedRet result
            | _ -> ch.Read(fun v ->
                    buf.AddRange(v)
                    match dequeue() with
                    | Some result -> guardedRet result
                    | _ -> ()
                   )


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
        }

    // Channel ops:

    /// Returns a channel that wraps the given channel as read-only.
    ///  Any writes to the returned channel are ignored.
    let toReadOnly (ch : IChannel<_>) =
        {
            new IChannel<_> with
                member __.Read ret = ch.Read(ret)
                member __.Write _  = ()
        }

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
        }

    let decompose shouldFlush ch = DecomposeChannel(ch, shouldFlush)

    type private ExpectChannel<'t>(ch : 't channel, check : ('t -> unit) -> 't -> unit) =
        interface IChannel<'t> with
            member __.Read ret = ch.Read(check ret)
            member __.Write nv = nv |> check ch.Write

    /// Returns a channel that expects a certain value to be read or written
    ///  to/from the given channel. If a different value is read or written,
    ///  throws an exception. The value to be expected is read from a channel.
    let expect (expected : IChannel<_>) (ch : IChannel<_>) =
        let check ret value = expected.Read(fun v -> if value = v then ret v else failwithf "%A" v)
        ExpectChannel(ch, check) :> IChannel<_>

    /// Returns a channel that expects a certain value to be read or written
    ///  to/from the given channel. If a different value is read or written,
    ///  throws an exception with the given description. The expected value is determined by a function.
    let expectFn (desc : string) (check : 't -> bool) (ch : IChannel<_>) =
        let check ret value = if check value then ret value else failwithf "%s" desc
        ExpectChannel(ch, check) :> IChannel<_>

    /// Maps one channel type onto another using an Iso
    let map (f1, f2) (ch : IChannel<_>) =
        {
            new IChannel<_> with
                member __.Read ret = ch.Read(fun v -> f1 v |> ret)
                member __.Write v = ch.Write(f2 v)
        }

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
        },
        {
            new IChannel<_> with
                member __.Read ret = ch.Read(fun v -> f2 v |> ret)
                member __.Write v =
                    holder2 <- Some v
                    doWrite()
        }

    /// Maps the given channel into a quoted string
    let quote<'t
     #if !NETFX_CORE
     when 't :> IConvertible
     #endif
     > (ch : IChannel<'t>) =
        ch
        |> map ((fun v -> sprintf "\"%O\"" v), (fun str -> Convert.ChangeType(str.Trim('"'), typeof<'t>) |> unbox))