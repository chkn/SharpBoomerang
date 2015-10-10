namespace SharpBoomerang

open System
open System.IO
open System.Text
open System.Collections.Generic
open Microsoft.FSharp.Quotations

[<AllowNullLiteral>]
type IMark =
    inherit IDisposable
    /// Seeks back to the point represented by this mark
    abstract Rewind : unit -> unit

/// A channel of characters that can be rewound to an earlier point
type ICharChannel =
    inherit IChannel<char>
    abstract Index : int
    abstract EndOfStream : bool
    abstract Mark : unit -> IMark

/// An exception thrown for parse errors
type Expected(str : string, inner : Exception) =
    inherit Exception(str, inner)
    new(wrapped : Exception, where) = Expected(sprintf "%s at position %i" wrapped.Message where, wrapped)
    new(what : string, where : int) = Expected(sprintf "%s at position %i" what where, null)
    new(inner : Exception, what, where) = Expected(sprintf "%s at position %i" what where, inner)
    new(lst : exn list) = Expected(String.Join(" OR ", lst |> List.map (fun e -> e.Message)), AggregateException(lst))

// Can't use a DU for Context because we need a special subclass for parsing until success
type ContextType =
    | Parsing = 1uy
    | Printing = 2uy

type Context(contextType : ContextType, channel : ICharChannel) =
    member this.Type = contextType
    member this.Channel = channel
    member this.Expect(bln, what : string) = if not bln then raise(Expected(what, channel.Index))
    abstract member Connect : IChannel<'t> * IChannel<'t> -> Context
    default this.Connect(l, r) =
        let index = channel.Index
        try
            match contextType with
            | ContextType.Parsing  -> l.Read(fun v -> r.Write(v))
            | ContextType.Printing -> r.Read(fun v -> l.Write(v))
            | _ -> failwith "Unknown context type"
            this
        // Catch and wrap vanilla System.Exception to enable `failwith` (e.g. in channel.fs)
        //  - we assume any more specific exception is unexpected and don't catch it.
        with e when e.GetType() = typeof<Exception> ->
            raise(Expected(e, index))

type UntilSuccessContext(wrapped : Context, fn : Context -> Context, onFinish : unit -> unit) =
    inherit Context(wrapped.Type, wrapped.Channel)
    new(wrapped, fn) = UntilSuccessContext(wrapped, fn, fun () -> ())
    override this.Connect(l, r) =
        let mutable ctx = wrapped
        let mutable finished = false
        while not finished do
            use mark = this.Channel.Mark()
            try
                base.Connect(l, r) |> ignore
                finished <- true
            with _ ->
                mark.Rewind()
                ctx <- fn ctx
        onFinish()
        ctx

type Boomerang = Context -> Context
type Boomerang<'t> = 't channel -> Boomerang

type StringInputChannel(str : string) =
    let mutable index = 0
    interface ICharChannel with
        member __.Index = index
        member __.EndOfStream = index >= str.Length
        member __.Write v = ()
        member __.Read ret =
            if index >= str.Length then raise(Expected("<character>", index))
            let i = index
            index <- index + 1
            ret str.[i]
        member __.Mark() =
            let mark = index
            { new IMark with
                 member __.Rewind() = index <- mark
                 member __.Dispose() = () }

type StringOutputChannel(writer : StringBuilder) =
    interface ICharChannel with
        member __.Index = writer.Length
        member __.EndOfStream = false
        member __.Write v = writer.Append(v) |> ignore
        member __.Read ret = ()
        member __.Mark() =
            let mark = writer.Length
            { new IMark with
                 member __.Rewind() = writer.Length <- mark
                 member __.Dispose() = () }

module Combinators =

    [<Literal>]
    let Parsing = ContextType.Parsing

    [<Literal>]
    let Printing = ContextType.Printing

    let parseFromStr str (b : Boomerang) =
        Context(Parsing, StringInputChannel(str)) |> b |> ignore
    let printToStr (b : Boomerang) =
        let buf = StringBuilder()
        Context(Printing, StringOutputChannel(buf)) |> b |> ignore
        buf.ToString ()

    // Channel operators:

    /// Constant channel (cannot be written to)
    let inline (~%) v = Channel.ofValue v

    /// Constant channel whose value is determined by a function
    let inline (~%%) fn = { new IChannel<_> with
                             member x.Read ret = fn ret
                             member x.Write v = () }

    /// Adapter. Connects the left and right channels
    ///  depending on whether the given Context is Parsing or Printing.
    let inline (<->) (l : ICharChannel -> #IChannel<'t>) (r : 't channel) (ctx : Context) =
        ctx.Connect(l ctx.Channel, r)

    // Parsing operators:

    // (>>): Sequential between two closed-over Boomerangs, e.g. blit %'foo' >> blit %'bar' matches foobar

    /// Pipes the channel from the left boomerang into the right one
    let inline (|>>) (l : Boomerang<_>) (r : Boomerang<_>) =
        let pipe = PipeChannel()
        l pipe >> r pipe

    /// Pipes the channel from the left boomerang into the right one, propagating another channel
    let inline (|>>.) (l : Boomerang<_>) (r : IChannel<_> -> Boomerang<_>) ch =
        let pipe = PipeChannel()
        l pipe >> r pipe ch

    /// Sequentially calls left and right, propagating the left channel
    let inline (.>>) (l : Boomerang<_>) (r : Boomerang) ch = l ch >> r

    /// Sequentially calls left and right, propagating the right channel
    let inline (>>.) (l : Boomerang) (r : Boomerang<_>) ch = l >> r ch

    /// Maps the value of the channel from the left boomerang using an Iso
    let inline (>>|) (l : Boomerang<_>) (f1, f2) ch = l (Channel.map (f2, f1) ch)

    /// First tries the left boomerang and then the right
    let inline (<|>) (l : Boomerang) (r : Boomerang) (ctx : Context) =
        use mark = ctx.Channel.Mark()
        try l ctx with e1 ->
            mark.Rewind()
            try r ctx with e2 ->
                raise(Expected([e1; e2]))

    /// First tries the left boomerang and then the right, propagating the channel
    let inline (<.>) (l : Boomerang<_>) (r : Boomerang<_>) (ch : IChannel<_>) (ctx : Context) =
        use mark = ctx.Channel.Mark()
        try l ch ctx with e1 ->
            mark.Rewind()
            try r ch ctx with e2 ->
                raise(Expected([e1; e2]))

    /// Optional operator.
    let (~~) (l : Boomerang) (matched : IChannel<bool>) (ctx : Context) =
        match ctx.Type with
        | Parsing ->
            use mark = ctx.Channel.Mark()
            try
                let nctx = l ctx
                matched.Write(true)
                nctx
            with _ ->
                mark.Rewind()
                matched.Write(false)
                ctx
        | Printing ->
            matched.Read(function
            | true  -> l ctx |> ignore
            | false -> ())
            ctx
        | _ -> failwith "Unknown context type"

    /// Channel-propagating optional operator.
    let (~~~) (l : Boomerang<'t>) (matched : IChannel<'t option>) (ctx : Context) =
        match ctx.Type with
        | Parsing ->
            let pipe = PipeChannel<'t>()
            use mark = ctx.Channel.Mark()
            try
                let nctx = l pipe ctx
                matched.Write(pipe.Value)
                nctx
            with _ ->
                mark.Rewind()
                matched.Write(None)
                ctx
        | Printing ->
            matched.Read(function
            | Some v -> l %v ctx |> ignore
            | None   -> ())
            ctx
        | _ -> failwith "Unknown context type"

    /// Non-greedy one or more operator. Continues to match the given boomerang until
    ///  it fails or the next one succeeds. When printing, the boomerang is called once.
    let inline (~+) (l : Boomerang) (ctx : Context) = UntilSuccessContext(l ctx, l) :> Context

    /// Channel-propagating non-greedy one or more operator. Continues to match the given
    ///  boomerang until it fails or the next one succeeds.
    let inline (~+.) (l : Boomerang<'t>) (matched : IChannel<'t seq>) ctx =
        let collector = Channel.decompose (fun _ -> false) matched
        let b = l collector
        UntilSuccessContext(b ctx, b, fun () -> collector.Flush()) :> Context

    /// Greedy one or more operator. Continues to match the given boomerang until it fails.
    ///  When printing, prints one.
    let (!+) (l : Boomerang) (ctx : Context) =
        let mutable nctx = l ctx
        let mutable mark = Unchecked.defaultof<IMark>
        try
            if nctx.Type = Parsing then
                while true do
                    if mark <> null then mark.Dispose()
                    mark <- nctx.Channel.Mark()
                    nctx <- l nctx
        with _ ->
            mark.Rewind()
        if mark <> null then mark.Dispose()
        nctx

    /// Channel-propagating greedy one or more operator. Continues to match the given boomerang
    ///  until it fails.
    let (!+.) (l : Boomerang<'t>) (matched : IChannel<'t seq>) (ctx : Context) =
        let collector = matched |> Channel.decompose (fun _ -> false)
        let mutable nctx = l collector ctx
        let mutable mark = Unchecked.defaultof<IMark>
        try
            while collector.Count > 0 do
                if mark <> null then mark.Dispose()
                mark <- ctx.Channel.Mark()
                nctx <- l collector nctx
        with _ ->
            mark.Rewind()
        if mark <> null then mark.Dispose()
        if ctx.Type = Parsing then
            collector.Flush()
        nctx

    /// Multiplication operator. Repeats the boomerang a given number of times
    let ( @* ) (l : Boomerang) (cnt : int channel) (ctx : Context) =
        let nctx = ref ctx
        cnt.Read(fun n ->
            for __ = 1 to n do
                nctx := l !nctx
        )
        !nctx

    /// Channel-propagating multiplication operator. Repeats the boomerang a given number of times
    //let ( @*. ) (l : Boomerang<'t>) (cnt : int channel) (ch : IChannel<'t array>) (ctx : Context) =
    //    Channel.decompose cnt 


    /// Pipes the first channel when parsing, second when printing.
    ///  Writes go to both channels.
    let (<+.) (l : Boomerang<_>) (parseCh : IChannel<_>) (printCh : IChannel<_>) (ctx : Context) =
        l { new IChannel<'t> with
            member __.Write v  = parseCh.Write v; printCh.Write v
            member __.Read ret =
                match ctx.Type with
                | Parsing  -> parseCh.Read ret
                | Printing -> printCh.Read ret
                | _ -> failwith "Unknown context type"
        } ctx


    /// Pipes the channel from the left boomerang into the quoted variable
    let inline (.->) (l : Boomerang<_>) (r : Expr<_>) = Channel.ofExpr r |> l

    /// Customizes the error message
    let (<?>) (l : Boomerang) (expected : string channel) (ctx : Context) =
        let index = ctx.Channel.Index
        try
            l ctx
        with e ->
            expected.Read(fun s -> raise(Expected(e, s, index)))
            ctx

    /// Customizes the error message and propagates the channel
    let (<?.>) (l : Boomerang<_>) (expected : string channel) (ch : IChannel<_>) (ctx : Context) =
        let index = ctx.Channel.Index
        try
            l ch ctx
        with e ->
            expected.Read(fun s -> raise(Expected(e, s, index)))
            ctx

    // Boomerangs:

    /// A boomerang to expect the end of the input when parsing
    let inline bend (ctx : Context) =
        if ctx.Type = Parsing then
            ctx.Expect(ctx.Channel.EndOfStream, "<end of stream>")
        ctx

    /// Boomerangs a single character
    let bchr chr = id <-> chr

    /// Boomerangs a digit (0 - 9) as a System.Char
    let bdigit digit = Channel.expectFn "digit" Char.IsDigit <-> digit

    /// Boomerangs a whitespace character
    let bws ws = Channel.expectFn "whitespace" Char.IsWhiteSpace <-> ws

    /// Boomerangs a given number of characters into an array
    let bnchrs n chrs = Channel.collect n <-> chrs

    /// Boomerangs a string of a given length
    let bnstr n = n |> bnchrs >>| ((fun chrs -> String(chrs)), (fun s -> s.ToCharArray()))

    /// Boomerangs a literal string
    let blit (str : string channel) =
        let n = str |> Channel.map (Iso.oneWay (fun s -> s.Length)) |> Channel.toReadOnly
        str |> Channel.expect str |> bnstr n <?> (str |> Channel.map (Iso.oneWay (fun s -> "\"" + s + "\"")))

    /// Greedily boomerangs a positive integer (sequence of digits)
    let bpint : Boomerang<int> =
        let rec decomp i = seq {
            let m, r = Math.DivRem(i, 10)
            yield char (r + 48)
            if m > 0 then
                yield! decomp m
        }
        !+.bdigit >>| (Seq.fold (fun i d -> i * 10 + (int d - 48)) 0, decomp >> Seq.rev)
 
    //let inline bstr (ch : string channel) = Channel.collect (fun chr ->
  