namespace SharpBoomerang

open System
open System.IO
open System.Text
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

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
    abstract Expect : (unit -> bool) * string -> unit
    default this.Expect(bln, what) = if not(bln()) then raise(Expected(what, channel.Index))
    abstract Connect : IChannel<'t> * IChannel<'t> -> Context
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
    abstract Dispose : unit -> unit
    default this.Dispose() = ()
    interface IDisposable with
        member this.Dispose() = this.Dispose()

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
        using (new Context(Parsing, StringInputChannel(str)) |> b) ignore

    let printToStr (b : Boomerang) =
        let buf = StringBuilder()
        using (new Context(Printing, StringOutputChannel(buf)) |> b) ignore
        buf.ToString()

    let parser (b : Boomerang<_>) inputCh =
        let ch = Channel.pipe()
        using (new Context(Parsing, inputCh) |> b ch) ignore
        ch.Value.Value

    let printer (b : Boomerang<_>) outputCh value =
        let ch = Channel.ofValue value
        using (new Context(Printing, outputCh) |> b ch) ignore

    let stringPrinter b value =
        let buf = StringBuilder()
        let out = StringOutputChannel(buf)
        printer b out value
        buf.ToString()

    // Channel operators:

    /// Constant channel (cannot be written to)
    let inline (~%) v = Channel.ofValue v

    /// Read-only channel whose value is determined by a function
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

    /// Yields the given value on a channel if the left boomerang matches
    let (>>%) (l : Boomerang) value (ch : IChannel<_>) (ctx : Context) =
        match ctx.Type with
        | Parsing ->
            let nctx = l ctx
            ch.Write(value)
            nctx
        | Printing ->
            ch.Read(fun v -> ctx.Expect((fun _ -> v = value), sprintf "%A but got %A" value v))
            l ctx
        | _ -> failwith "Unknown context type"

    /// Maps the value of the channel from the left boomerang using an `Iso`
    let inline (.>>%) (l : Boomerang<_>) (f1, f2) ch = l (Channel.map (f2, f1) ch)

    // KLUDGE: Nasty, but gets the job done
    //  Technique thanks to http://stackoverflow.com/a/2812306/578190
    type __TupleBoomerang =
        static member Append(l : Boomerang<_>, r : Boomerang<_>, ch : IChannel<_>) =
            let lch, rch = Channel.map2 (fun (a, _) -> a) (fun (_, b) -> b) (fun a b -> (a, b)) ch
            l lch >> r rch
        static member Append(l : Boomerang<(_ * _)>, r : Boomerang<_>, ch : IChannel<_>) =
            let lch, rch = Channel.map2 (fun (a, b, _) -> (a, b)) (fun (_, _, c) -> c) (fun (a, b) c -> (a, b, c)) ch
            l lch >> r rch

    let inline __tupleBoomerangAppend_< ^t, ^a, ^b, ^c, ^d when (^t or ^a) : (static member Append : ^a * ^b * ^c -> ^d)> a b c =
        ((^t or ^a) : (static member Append : ^a * ^b * ^c -> ^d) (a, b, c))

    // Sequentially calls left and right, combining the channels into a tuple channel
    let inline (.>>.) l r ch = __tupleBoomerangAppend_<__TupleBoomerang, _, _, _, _> l r ch

    /// First tries the left boomerang and then the right
    let inline (<|>) (l : Boomerang) (r : Boomerang) (ctx : Context) =
        use mark = ctx.Channel.Mark()
        try l ctx with e1 ->
            mark.Rewind()
            try r ctx with e2 ->
                raise(Expected([e1; e2]))

    /// Tries each boomerang in order (plural of <|> operator)
    let bchoice (lst : Boomerang list) (ctx : Context) =
        let mutable exns = []
        use mark = ctx.Channel.Mark()
        let rec iter = function
        | hd :: tl ->
            try hd ctx with e ->
                exns <- e :: exns
                mark.Rewind()
                iter tl
        | [] ->
            raise(Expected(exns))
        iter lst

    /// First tries the left boomerang and then the right, propagating the channel
    let inline (<.>) (l : Boomerang<_>) (r : Boomerang<_>) (ch : IChannel<_>) (ctx : Context) =
        use mark = ctx.Channel.Mark()
        try l ch ctx with e1 ->
            mark.Rewind()
            try r ch ctx with e2 ->
                raise(Expected([e1; e2]))

    /// Tries each boomerang in order, propagating the channel (plural of <.> operator)
    let bfirst (lst : Boomerang<_> list) (ch : IChannel<_>) (ctx : Context) =
        let mutable exns = []
        use mark = ctx.Channel.Mark()
        let rec iter = function
        | hd :: tl ->
            try hd ch ctx with e ->
                exns <- e :: exns
                mark.Rewind()
                iter tl
        | [] ->
            raise(Expected(exns))
        iter lst

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

    type private NonGreedyRepeatContext(wrapped : Context, fn : Context -> Context, onFinish : unit -> unit) =
        inherit Context(wrapped.Type, wrapped.Channel)
        new(wrapped, fn) = new NonGreedyRepeatContext(wrapped, fn, fun _ -> ())
        override this.Expect(bln, what) =
            let mutable ctx = wrapped
            let mutable finished = bln()
            while not finished do
                ctx <- fn ctx
                finished <- bln()
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
        override this.Dispose() =
            // Make sure we capture any more at the end
            let mutable ctx = wrapped
            try
                // FIXME: protect against infinite loop
                while true do
                    ctx <- fn ctx
            with _ -> ()
            onFinish()
            wrapped.Dispose()

    /// Non-greedy one or more operator. Continues to match the given boomerang until
    ///  it fails or the next one succeeds. When printing, the boomerang is called once.
    let (~+) (l : Boomerang) (ctx : Context) = new NonGreedyRepeatContext(l ctx, l) :> Context

    /// Channel-propagating non-greedy one or more operator. Continues to match the given
    ///  boomerang until it fails or the next one succeeds.
    let (~+.) (l : Boomerang<'t>) (matched : IChannel<'t seq>) (ctx : Context) =
        let collector = Channel.decompose (fun _ -> false) matched
        let b = l collector
        match ctx.Type with
        | Parsing  ->
            new NonGreedyRepeatContext(b ctx, b, fun _ -> collector.Flush()) :> Context
        | Printing ->
            let mutable nctx = b ctx
            while collector.Count > 0 do
                nctx <- b nctx
            nctx
        | _ -> failwith "Unknown context type"

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

    /// Left conditional operator. Reads the first channel when parsing, second when printing.
    ///  Writes go to both channels.
    let (<??) (l : Boomerang<_>) (parseCh : IChannel<_>) (printCh : IChannel<_>) (ctx : Context) =
        l {
            new IChannel<'t> with
                member __.Write v  = parseCh.Write v; printCh.Write v
                member __.Read ret =
                    match ctx.Type with
                    | Parsing  -> parseCh.Read ret
                    | Printing -> printCh.Read ret
                    | _ -> failwith "Unknown context type"
        } ctx

    /// Right conditional operator. Reads come from first channel, writes go to second channel
    let (??>) (l : Boomerang<_>) (readCh : IChannel<_>) (writeCh : IChannel<_>) (ctx : Context) =
        l {
            new IChannel<'t> with
                member __.Write v  = writeCh.Write v
                member __.Read ret = readCh.Read(fun v -> writeCh.Write(v); ret v)
        } ctx

    /// Pipes the channel from the left boomerang into the quoted variable
    let inline (.->) (l : Boomerang<_>) (r : Expr<_>) = l (Channel.ofExpr r)

    /// Customizes the error message
    let (<?>) (l : Boomerang) (expected : string channel) (ctx : Context) =
        let index = ctx.Channel.Index
        try
            l ctx
        with e ->
            expected.Read(fun s -> raise(Expected(e, s, index)))
            ctx

    /// Customizes the error message and propagates the channel
    let (<.?>) (l : Boomerang<_>) (expected : string channel) (ch : IChannel<_>) (ctx : Context) =
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
            ctx.Expect((fun _ -> ctx.Channel.EndOfStream), "<end of stream>")
        ctx

    /// Boomerangs a single character
    let bchr chr = id <-> chr

    /// Boomerangs a digit (0 - 9) as a System.Char
    let bdigit digit = Channel.expectFn "digit" Char.IsDigit <-> digit

    /// Boomerangs a whitespace character
    let bws ws = Channel.expectFn "whitespace" Char.IsWhiteSpace <-> ws

    /// Boomerangs a given number of characters into an array
    let bnchrs n chrs = Channel.collect n <-> chrs

    /// Boomerangs the given boomerang a given number of times
    let btimes (l : Boomerang) (times : int channel) (ctx : Context) =
        let nctx = ref ctx
        times.Read(fun n ->
            for __ = 1 to n do
                nctx := l !nctx
        )
        !nctx

    /// Boomerangs the given boomerang a given number of times into an array
    //let barray (l : Boomerang<'t>) (times : int channel) (ch : IChannel<'t array>) (ctx : Context) =
    //    ch |> Channel.decompose (fun lst -> lst

    /// Boomerangs a string of a given length
    let bnstr n =
        n
        |> bnchrs
        .>>% ((fun chrs -> String(chrs)), (fun s -> s.ToCharArray()))

    /// Boomerangs a literal string
    let blit (str : string channel) =
        let n = str |> Channel.map (Iso.oneWay (fun s -> s.Length)) |> Channel.toReadOnly
        str
        |> Channel.expect str
        |> bnstr n <?> Channel.quote str

    /// Boomerangs a positive integer (greedy sequence of digits)
    let bpint : Boomerang<int> =
        let rec decomp i = seq {
            yield char ((i % 10) + 48)
            let m = i / 10
            if m > 0 then
                yield! decomp m
        }
        !+.bdigit .>>% (Seq.fold (fun i d -> i * 10 + (int d - 48)) 0, decomp >> Seq.rev)

    /// Boomerangs a possibly-negative integer (greedy sequence of digits)
    let bint : Boomerang<int> =
        let negate = fun i -> -i
        bpint <.> (blit %"-" >>. bpint .>>% (negate, negate))

    /// Boomerangs a positive floating point number in the decimal format
    // FIXME: This needs some love
    let bpfloat : Boomerang<float> =
        !+.(Channel.expectFn "floating point number" (fun c -> Char.IsDigit(c) || c = '.') >> bchr)
        .>>% ((fun chrs -> Double.Parse(String(Seq.toArray chrs))), (fun f -> f.ToString().ToCharArray() :> char seq))

    /// Boomerangs a possibly-negative floating point number
    let bfloat : Boomerang<float> =
        let negate = fun i -> -i
        bpfloat <.> (blit %"-" >>. bpfloat .>>% (negate, negate))
 
    /// Boomerangs an arbitrary length string (non-greedy sequence of characters)
    let bstr : Boomerang<string> =
        +.bchr .>>% ((fun chrs -> String(Seq.toArray chrs)), (fun str -> str.ToCharArray() :> char seq))

    /// Boomerangs a discriminated union case that takes no arguments
    let bdu<'a> : Boomerang<'a> =
        let cases = FSharpType.GetUnionCases typeof<'a>
        let toString (x : 'a) = fst(FSharpValue.GetUnionFields(x, typeof<'a>)).Name
        let fromString s =
            cases
            |> Array.pick (fun case ->
                if case.Name = s then
                    Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
                else
                    None)
        cases
        |> Array.map (fun case -> blit <?? (Channel.ofValue case.Name))
        |> List.ofArray
        |> bfirst
        .>>% (fromString, toString)