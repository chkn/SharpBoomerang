namespace SharpBoomerang

open System
open System.IO
open System.Text
open Microsoft.FSharp.Quotations

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
type Expected(str : string) =
    inherit Exception(str)
    new(what, where) = Expected(sprintf "%s at position %i" what where)
    new(lst : exn list) = Expected(String.Join(" OR ", lst |> List.map (fun e -> e.Message)))

type Context =
    | Parsing of ICharChannel
    | Printing of ICharChannel
    member this.Channel = match this with | Parsing ch -> ch | Printing ch -> ch
    member this.Expect(bln, what) = if not bln then raise(Expected(what, this.Channel.Index))
    member this.Connect(l : IChannel<_>, r : IChannel<_>) =
        match this with
        | Parsing _  -> l.Read(fun v -> r.Write(v))
        | Printing _ -> r.Read(fun v -> l.Write(v))

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

type SplitterChannel<'t>(parseCh : IChannel<'t>, printCh : IChannel<'t>, ctx : Context) =
    interface IChannel<'t> with
        member __.Write v  = parseCh.Write v; printCh.Write v
        member __.Read ret =
            match ctx with
            | Parsing _  -> parseCh.Read ret
            | Printing _ -> printCh.Read ret

module Combinators =

    let parseFromStr str (b : Boomerang) =
        StringInputChannel(str) :> ICharChannel |> Parsing |> b |> ignore
    let printToStr (b : Boomerang) =
        let buf = StringBuilder()
        StringOutputChannel(buf) :> ICharChannel |> Printing |> b |> ignore
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
    let (-->) (l : ICharChannel -> #IChannel<'t>) (r : 't channel) (ctx : Context) = ctx.Connect(l ctx.Channel, r)

    let (==>) (l : ('a -> ICharChannel -> #IChannel<'b>) * ('b -> 'a)) (inp : IChannel<'a>) (r : 'b channel) (ctx : Context) =
        match ctx with
        | Parsing _   -> inp.Read(fun v -> ctx |> ((fst l) v) --> r)
        | Printing ch -> r.Read(fun v -> inp.Write((snd l) v); ch.Write(v))

    // Parsing operators:

    /// Pipes the channel from the left boomerang into the right one
    let inline (|>>) (l : Boomerang<_>) (r : Boomerang<_>) =
        let pipe = PipeChannel()
        l pipe >> r pipe

    /// Maps the value of the channel from the left boomerang using an Iso
    let inline (>>|) (l : Boomerang<_>) (iso : Iso<_,_>) (r : Boomerang<_>) =
        let pipe = PipeChannel() :> IChannel<_>
        l pipe >> r (Channel.map iso pipe)

    /// First tries the left boomerang and then the right
    let inline (<|>) (l : Boomerang) (r : Boomerang) (ctx : Context) =
        use mark = ctx.Channel.Mark()
        try l ctx with e1 ->
            mark.Rewind()
            try r ctx with e2 ->
                raise(Expected([e1; e2]))

    /// First tries the left boomerang and then the right, propagating the channel
    let inline (<^>) (l : Boomerang<_>) (r : Boomerang<_>) (ch : IChannel<_>) (ctx : Context) =
        use mark = ctx.Channel.Mark()
        try l ch ctx with e1 ->
            mark.Rewind()
            try r ch ctx with e2 ->
                raise(Expected([e1; e2]))
    (*
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
    *)
    /// Splitter. Reads go to first channel when parsing, second when printing.
    ///  Writes go to both channels.
    let inline (<+^) (l : Boomerang<_>) parseCh printCh ctx =
        l (SplitterChannel(parseCh, printCh, ctx)) ctx

    /// Pipes the channel from the left boomerang into the quoted variable
    let inline (^->) (l : Boomerang<_>) (r : Expr<_>) = Channel.ofExpr r |> l

    // Boomerangs:

    /// Boomerangs a single character
    let bchr (ch : char channel) = id ==> ch

    /// A boomerang to expect the end of the input when parsing
    let bend = function | Parsing ch as ctx -> ctx.Expect(ch.EndOfStream, "<end of stream>"); ctx | ctx -> ctx

    //let bnstr (n : int channel) ch =
    //    n --> Channel.collect (fun inp -> if inp.Count = n then Some(String(inp.ToArray())) else None) ==> ch
    (*
    /// Boomerangs a literal string
    let blit (ch : string channel) (ctx : Context) =
        ch.Read(fun str ->
            Channel.collect (fun inp ->
                let i = inp.Count - 1
                ctx.Expect(inp.[i] = str.[i], sprintf "'%c' in '%s'" str.[i] str)
                if inp.Count = str.Length then Some(str) else None)
            <-> ch <| ctx |> ignore
        )
        ctx

    /// Boomerangs a string of specified length
    let bnstr (cnt : int channel) (ch : string channel) (ctx : Context) =
        cnt.Read(fun n -> Channel.collect (fun inp -> if inp.Count = n then Some(String(inp.ToArray())) else None) <-> ch <| ctx |> ignore)
        ctx
    *)
    /// Boomerangs a positive integer
    //let bpint (ch : int channel) = Channel.collect (fun inp -> 
 
    //let inline bint ch = Channel.expect (Char.IsDigit) >> Channel.map ((fun v -> int v - 48), (fun i -> char(i + 48))) <-> ch
    //let inline bstr (ch : string channel) = Channel.collect (fun chr ->
    (*
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
    *)