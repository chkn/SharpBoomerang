namespace SharpBoomerang

open System
open System.IO
open System.Text
open Microsoft.FSharp.Quotations

/// An exception thrown for parse errors.
type Expected(str : string) =
    inherit Exception(str)
    new(what, where) = Expected(sprintf "%A at position %i" what where)
    new(lst : exn list) = Expected(String.Join(" OR ", lst |> List.map (fun e -> e.Message)))

type ContextType =
    | Unknown = 0
    | Parsing = 1
    | Printing = 2

[<AbstractClass>]
type Context() =
    let mutable finished = false
    let finishedChanged = new Event<EventHandler, EventArgs>()

    /// Indicates to the Context that there is no more to parse/print
    ///  Implementation must be able to be called more than once
    abstract Finished : bool with get, set
    default x.Finished with get() = finished
                        and set v =
                            if finished <> v then
                                if not v then invalidArg "value" "Cannot set Finished to False once it is True"
                                finished <- v
                                finishedChanged.Trigger(x, EventArgs.Empty)
    [<CLIEvent>]
    member x.FinishedChanged = finishedChanged.Publish

    /// Should be used by subclasses when a parse error occurs
    member x.Expected what = raise(Expected(what, x.Index))

    /// The current index in the input (when parsing) or output (when printing)
    abstract Index : int with get, set

    /// A `ContextType` indicating whether this `Context` does parsing or printing
    abstract Type : ContextType with get

    /// Parse or print the character from/into the given channel
    abstract Char : IChannel<char> -> Context

    abstract Dispose : unit -> unit
    default x.Dispose() = x.Finished <- true
    interface IDisposable with
        member x.Dispose() = x.Dispose()

type Boomerang = Context -> Context
type Boomerang<'t> = IChannel<'t> -> Context -> Context

/// Supplies one channel when parsing and another when printing
type ConditionalChannel<'t>(parseCh : IChannel<'t>, printCh : IChannel<'t>, ctx : Context, alwaysWriteToPrint : bool) =
    interface IChannel<'t> with
        member x.Read ret =
            match ctx.Type with
            | ContextType.Parsing -> parseCh.Read ret
            | ContextType.Printing -> printCh.Read ret
            | other -> failwithf "Unsupported ContextType: %O" other
        member x.Write v =
            match ctx.Type with
            | ContextType.Parsing -> parseCh.Write v; if alwaysWriteToPrint then printCh.Write v
            | ContextType.Printing -> printCh.Write v
            | other -> failwithf "Unsupported ContextType: %O" other

// onFailOrFinished will be called if the next parser fails or there is no next parser. The argument
//  indicates that this will be the last call of the function.
//  if there is a next parser, and it fails, it will be tried repeatedly for each successive character
//  unless it returns false.
type UntilSuccessContext(wrapped : Context, onFailOrFinished : bool -> bool) as this =
    inherit Context()
    let onFinishedChanged _ _ =
        onFailOrFinished true |> ignore
        this.Dispose()
    let onFinishedChangedDel = EventHandler(onFinishedChanged)
    do
        wrapped.FinishedChanged.AddHandler(onFinishedChangedDel)
    let rec iter (f : unit -> Context) =
        let mark = this.Index
        try
            let ctx = f()
            // if we succeeded then call one last time
            try onFailOrFinished true |> ignore with _ -> ()
            // Dispose ourselves to remove the handler
            this.Dispose()
            ctx
        with _ ->
            this.Index <- mark
            if not(onFailOrFinished(this.Finished)) || this.Finished then
                // restore wrapped context
                wrapped
            else
                iter f
    override x.Type = wrapped.Type
    override x.Index with get() = wrapped.Index and set v = wrapped.Index <- v
    override x.Finished with get() = wrapped.Finished and set v = wrapped.Finished <- v
    override x.Char c = iter (fun () -> wrapped.Char c)
    override x.Dispose() =
        // Don't call base and set Finished = true in this case
        wrapped.FinishedChanged.RemoveHandler(onFinishedChangedDel)

type ParseContext(str : string) =
    inherit Context()
    let mutable index = 0
    override __.Index with get() = index and set v = index <- v
    override __.Type = ContextType.Parsing
    override this.Char c =
        if index >= str.Length then
            this.Finished <- true
            expected "character"
        else
            let result = str.[index]
            index <- index + 1
            c.Write(result)
        this :> Context

type PrintContext(writer : StringBuilder) =
    inherit Context()
    override __.Type = ContextType.Printing
    override __.Index with get() = writer.Length and set v = writer.Length <- v
    override this.Char c =
        c.Read(fun v -> ignore(writer.Append(v)))
        this :> Context

/// This provides a nice, fluent API for C# people, and gives the canonical
///  implementations for the F# operators defined after
type Context with

    member this.Lit (ch : IChannel<string>) =
        ch.Read(fun str ->
            let sindex = ref 0
            this.Char

/// Provides conveniences for using SharpBoomerang from the F# language.
module FSharp =

    let parseFromStr str b =
        use parser = new ParseContext(str) :> Context
        parser |> b |> ignore
    let printToStr b =
        let buf = StringBuilder()
        use printer = new PrintContext(buf) :> Context
        printer |> b |> ignore
        buf.ToString ()

    /// Constant channel (cannot be written to)
    let inline (~%) v = Channel.ofValue v

    /// Constant channel whose value is determined by a function
    let inline (~%%) fn = { new IChannel<_> with
                             member x.Read ret = fn ret
                             member x.Write v = () }

    /// Pipes the channel from the left boomerang into the right one
    let inline (|>>) (l : Boomerang<_>) (r : Boomerang<_>) =
        let pipe = PipeChannel()
        l pipe >> r pipe
    (*
    /// Maps the value of the channel from the left boomerang using an Iso
    let inline (>|) (l : Boomerang<_>) (iso : Iso<_,_>) (ctx : Context) =
        let pipe = PipeChannel()
        let nctx = l pipe ctx
    *)

    // First tries the left boomerang and then the right
    let inline (<|>) (l : Boomerang) (r : Boomerang) (ctx : Context) =
        let mark = ctx.Index
        try l ctx with e1 ->
            ctx.Index <- mark
            try r ctx with e2 ->
                raise(Expected([e1; e2]))

    // First tries the left boomerang and then the right, propagating the channel
    let inline (<^>) (l : Boomerang<_>) (r : Boomerang<_>) (ch : IChannel<_>) (ctx : Context) =
        let mark = ctx.Index
        try l ch ctx with e1 ->
            ctx.Index <- mark
            try r ch ctx with e2 ->
                raise(Expected([e1; e2]))

    // Optional operator.
    let inline (~~) (l : Boomerang) (ch : IChannel<bool>) (ctx : Context) =
        match ctx.Type with
        | ContextType.Parsing ->
            let mark = ctx.Index
            try
                let nctx = l ctx
                ch.Write(true)
                nctx
            with _ ->
                ctx.Index <- mark
                ch.Write(false)
                ctx
        | ContextType.Printing ->
            ch.Read(function
            | true  -> l ctx |> ignore
            | false -> ())
            ctx
        | other -> failwithf "Unsupported ContextType: %O" other

    // Channel-propagating optional operator.
    let inline (~~~) (l : Boomerang<'t>) (ch : IChannel<'t option>) (ctx : Context) =
        match ctx.Type with
        | ContextType.Parsing ->
            let pipe = PipeChannel<'t>()
            let mark = ctx.Index
            try
                let nctx = l pipe ctx
                ch.Write(pipe.Value)
                nctx
            with _ ->
                ctx.Index <- mark
                ch.Write(None)
                ctx
        | ContextType.Printing ->
            ch.Read(function
            | Some v -> l %v ctx |> ignore
            | None   -> ())
            ctx
        | other -> failwithf "Unsupported ContextType: %O" other

    // Conservative one or more operator. Continues to match the given boomerang until it fails
    //   or the next one succeeds.
    let inline (~+) (l : Boomerang) (ctx : Context) =
        new UntilSuccessContext(l ctx, fun last -> if last then false else ignore(l ctx); true) :> Context

    // Greedy one or more operator. Continues to match the given boomerang until it fails.
    //  When printing, prints one.
    let inline (~+.) (l : Boomerang) (ctx : Context) =
        let mutable mark = 0
        let mutable nctx = l ctx
        try
            if nctx.Type = ContextType.Parsing then
                while true do
                    mark <- nctx.Index
                    nctx <- l nctx
            nctx
        with _ ->
            nctx.Index <- mark
            nctx

    // Pipes one channel into the boomerang when parsing and the other when printing.
    //  When parsing, any writes to the parse channel will also be written to the print channel.
    let inline (<?+^) (l : Boomerang<_>) parseCh printCh ctx =
        l (ConditionalChannel(parseCh, printCh, ctx, true)) ctx

    // Pipes the channel from the left boomerang into the quoted variable
    let inline (^->) (l : Boomerang<_>) (r : Expr<_>) = ExprChannel(r) |> l

    let inline blit c (ctx : Context) = ctx.Lit c
    let inline bint e (ctx : Context) = ctx.Int e
    let inline bstr s (ctx : Context) = ctx.Str s
    let inline bnstr e n (ctx : Context) = ctx.NStr e n
    let inline bws s (ctx : Context) = ctx.Ws s

    type BoomerangBuilder() =
        member x.Bind(b : Boomerang<_>, f : Boomerang<_>) = fun ch -> b ch >> f ch
        member x.Bind(b : Boomerang, f : unit -> Boomerang) = b >> f()
        member x.Bind(b : Boomerang, f : unit -> Boomerang<_>) = fun ch -> b >> f() ch

        member x.Zero() = fun (ctx : Context) -> ctx
        member x.Return(v) = x.Zero()
        member x.ReturnFrom(v) = v

    let boomerang = BoomerangBuilder()