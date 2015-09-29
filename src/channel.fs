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
    /// If this instance represents a read-only channel, then this method must be a no-op.
    abstract Write : 't -> unit

type channel<'t> = IChannel<'t>

/// A channel for piping between combinators.
/// If the channel is read and there is no value available, the callback
/// will be saved and called on the next `Write` operation.
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
        match valueCallback with
        | null     -> ()
        | callback ->
            callback.Invoke(v)
            valueCallback <- null
    interface IChannel<'t> with
        member this.Read ret = this.Read ret
        member this.Write v = this.Write v

/// A channel for directly accessing a mutable data source.
type private ExprChannel<'t>(expr : Expr<'t>) =
    static let rec getExpr = function
    | Value(value, _) -> value
    | PropertyGet(None, pi, _) -> pi.GetValue(null, null)
    | PropertyGet(Some(expr), pi, _) -> pi.GetValue(getExpr expr, null)
    | u -> failwithf "Unsupported Expr: %A" u
    static let setExpr v = function
    | ValueWithName(_, _, nm) -> failwithf "Cannot mutate this binding: %s" nm
    | Value(value, _) -> () // do nothing so behavior matches ConstChannel
    | PropertyGet(None, pi, _) -> pi.SetValue(null, v, null)
    | PropertyGet(Some(expr), pi, _) -> pi.SetValue(getExpr expr, v, null)
    | u -> failwithf "Unsupported Expr: %A" u
    interface IChannel<'t> with
        member __.Read ret = getExpr expr :?> 't |> ret
        member __.Write v  = setExpr v expr

/// Collects values from the given wrapped channel until the given function returns Some value,
///  which becomes the value of this channel.
type private CollectorChannel<'a,'b when 'b :> IEnumerable<'a>>(ch : 'a channel, fn : List<'a> -> 'b option) =
    interface IChannel<'b> with
        member __.Write v = for item in v do ch.Write(item)
        member __.Read ret =
            let buf = List<'a>()
            let rec readOne() =
                ch.Read(fun v ->
                    buf.Add(v)
                    match fn buf with
                    | Some result -> ret result
                    | _ -> readOne()
                )
            readOne()

module Channel =
    // Channel creation:

    /// Creates a channel for piping between combinators.
    let inline pipe() = PipeChannel<_>()

    /// Creates a channel for directly accessing a mutable data source.
    let ofExpr e = ExprChannel(e) :> IChannel<_>

    /// Creates a read-only channel that is initialized with a constant value
    let ofValue v =
        {
            new IChannel<_> with
                member __.Read ret = ret v
                member __.Write _  = ()
        }

    // Channel ops:

    /// Maps one channel type onto another using an Iso
    let map iso (ch : IChannel<_>) =
        {
            new IChannel<_> with
                member __.Read ret = ch.Read(fun v -> ret((fst iso) v))
                member __.Write v = ch.Write((snd iso) v)
        }

    let collect fn ch = CollectorChannel(ch, fn) :> IChannel<_>
