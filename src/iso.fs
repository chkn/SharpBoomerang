namespace SharpBoomerang

open System
open System.Linq
open System.Linq.Expressions
open System.Reflection
open System.Text.RegularExpressions
open System.Collections.Generic

open Microsoft.FSharp.Math
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

(*
The following isomorphism type, Iso, is borrowed from,
and thus compatible with, Aether (https://github.com/xyncro/aether).
*)

/// Total, one-to-one isomorphism of a <> b
type Iso<'a,'b> = ('a -> 'b) * ('b -> 'a)

[<AbstractClass; Sealed>]
type Iso private () =

    /// Represents an Iso that only supports a single direction transformation
    static member oneWay(fn : 'a -> 'b) : Iso<_,_> = fn, (fun _ -> failwith "Reverse transformation not supported")

    /// Given a simple function, `'a -> 'b`, attempts to derive the inverse implicitly
    // This method is a work in progress. We will add cases as they come up.
    static member ofFn([<ReflectedDefinition(true)>] expr : Expr<('a -> 'b)>) : Iso<_,_> =
        match expr with
        | WithValue(:? ('a -> 'b) as fn, _, Lambda(arg, body)) ->
            let rec visitAll exprs a = seq { for e in exprs do yield visit e a }
            and visit e (a : Map<Var,LExpr>) : LExpr =

                // Call inversion
                //  Given a chain of calls, A(B(C(d))) = d'
                //    where A, B, and C are functions and d is some argument expression,
                //  the inverse of that is C'(B'(A'(d'))) = d,
                //    where A', B', and C' are the inverse functions of A, B, and C respectively.
                let rec (|Invert|_|) = function
                    | SpecificCall <@@ (|>) @@> (_, _, [Invert(e,_); _]) as skipped -> Some (e, Some skipped)
                    | SpecificCall <@@ (|>) @@> (_, _, [e; _]) as skipped -> Some (e, Some skipped)

                    // 1 argument
                    | Call(_, _, [Invert(e,_)]) as skipped -> Some (e, Some skipped)
                    | Call(_, _, [e]) as skipped -> Some (e, Some skipped)
                    | NewUnionCase(_, [Invert(e,_)]) as skipped -> Some (e, Some skipped)
                    | NewUnionCase(_, [e]) as skipped -> Some (e, Some skipped)

                    // 2 arguments
                    | Call(_, _, [_; Invert(e,_)]) as skipped -> Some (e, Some skipped)
                    | Call(_, _, [_; e]) as skipped -> Some (e, Some skipped)
                    | NewUnionCase(_, [_; Invert(e,_)]) as skipped -> Some (e, Some skipped)
                    | NewUnionCase(_, [_; e]) as skipped -> Some (e, Some skipped)

                    | e -> Some (e, None)

                let replace expr inExpr withLExpr =
                    match inExpr with
                    | Some(e) ->
                        let var, newExpr = match expr with
                                           | Var(v) -> v, e
                                           | _ ->
                                                let var = Var("_", expr.Type)
                                                let varExpr = Expr.Var(var)
                                                var, e |> Expr.replace (function | x when x = expr -> Some varExpr | _ -> None)
                        let a = a.Add(var, withLExpr)
                        visit newExpr a
                    | _ -> withLExpr

                match e with
                // Values:
                | Value(v, _) -> LExpr.Constant(v) :> _
                | Var(v) -> a.[v]
                | Coerce(e, t) -> LExpr.Retype(visit e a, t)
                | NewObject(ctor, args) -> LExpr.New(ctor, visitAll args a) :> _

                // DU:
                | NewUnionCase(case, [Invert(arg, inner)]) ->
                    // The inverse of creating a union with one arg is decomposing the case into the arg
                    let p = visit arg a
                    let prop = case.GetFields().Single()
                    LExpr.Property(LExpr.Retype(p, prop.DeclaringType), prop) |> replace arg inner

                // Tuple:
                // FIXME: Generalize this to other expressions than NewTuple
                | TupleDecompose(t, args, (NewTuple(exprs) as nt)) when typeof<'b> = nt.Type ->
                    let inpts, a = Seq.zip args exprs
                                |> Seq.map (fun (v, e) -> v, LExpr.Parameter(e.Type, v.Name))
                                |> Seq.mapFold (fun a t -> snd t, a |> Map.add (fst t) (snd t :> LExpr)) a
                    let outps = args |> List.map (fun v -> LExpr.Parameter(v.Type, v.Name))
                    let objs = outps |> List.map (fun p -> LExpr.Convert(p, typeof<obj>) :> LExpr)
                    let makeTuple = typeinfoof<FSharpValue>.GetMethod("MakeTuple")
                    let getTupleFields = typeinfoof<FSharpValue>.GetMethod("GetTupleFields")
                    let body =
                        [
                            LExpr.AssignAll(inpts, LExpr.Call(getTupleFields, visit t a)) |> Seq.singleton;
                            Seq.zip outps exprs |> Seq.map (fun (p, e) -> LExpr.Assign(p, visit e a) :> LExpr);
                            LExpr.Convert(LExpr.Call(makeTuple, LExpr.NewArrayInit(typeof<obj>, objs), LExpr.Constant(t.Type)), t.Type) :> LExpr |> Seq.singleton
                        ]
                        |> Seq.concat
                        |> Seq.toList
                    for v in args do
                        a.Remove(v) |> ignore
                    LExpr.Block(t.Type, Seq.append inpts outps, body) :> _

                | Let(v, body, following) ->
                    let p = LExpr.Parameter(v.Type, v.Name)
                    let a = a.Add(v, p)
                    let res = LExpr.Block([| p |], LExpr.Assign(p, visit body a), visit following a)
                    res :> _

                // Exceptions:
                | SpecificCall <@@ raise @@> (_, _, [arg]) -> LExpr.Throw(visit arg a, e.Type) :> _
                | SpecificCall <@@ failwith @@> (_, _, [arg]) -> LExpr.Throw(LExpr.New(typeinfoof<Exception>.GetConstructor([| typeof<string> |]), visit arg a), e.Type) :> _

                // Control flow:
                | Lambda(v, e) ->
                    // We swap the argument and return types
                    let p = LExpr.Parameter(e.Type, v.Name)
                    let a = a.Add(v, p)
                    LExpr.FSharpFunc(p, visit e a) :> _

                | SpecificCall <@@ (|>) @@> (_, _, [Invert(arg, inner); Lambda(v, body)]) ->
                    visit body (a.Add(v, visit arg a)) |> replace arg inner

                | IfThenElse(SpecificCall <@@ (=) @@> (_, _, [Var(v) as arg1; arg2]), thn, els) ->
                    // Swap the if and the then
                    let ifTrue = visit arg2 a
                    LExpr.Condition(LExpr.Equal(visit arg1 a, visit thn a), ifTrue, LExpr.Retype(visit els a, ifTrue.Type)) :> _

                // Delegate ops:
                | NewDelegate(t, args, body) ->
                    let delegateType, orderedargs, argexprs =
                        let ti = t.GetTypeInfo()
                        let forward() = t, args, args |> List.map (fun a -> LExpr.Parameter(a.Type, a.Name))
                        if ti.IsGenericType then
                            // For some delegate types, we need to compute the inverse of them as well
                            let gt = t.GetGenericTypeDefinition()
                            let reverse() =
                                let oa = List.rev args
                                let ga = ti.GenericTypeArguments
                                gt.MakeGenericType(Array.rev ga), oa,
                                oa |> List.map (fun a -> LExpr.Parameter((if a.Type = ga.[0] then ga.[1] else a.Type), a.Name))

                            if gt = typedefof<Func<_,_>> then reverse()
                            else forward()
                        else forward()
                    let a = List.zip orderedargs (argexprs |> List.map (fun p -> p :> LExpr)) |> Map.ofList
                    LExpr.Lambda(delegateType, visit body a, argexprs) :> _

                // Numerical ops:
                | SpecificCall <@@ (+) @@> (_, _, [arg1; arg2]) -> LExpr.Subtract((visit arg1 a), (visit arg2 a)) :> _
                | SpecificCall <@@ (-) @@> (_, _, [arg1; arg2]) -> LExpr.Add((visit arg1 a), (visit arg2 a)) :> _
                | SpecificCall <@@ (*) @@> (_, _, [arg1; arg2]) -> LExpr.Divide((visit arg1 a), (visit arg2 a)) :> _
                | SpecificCall <@@ (/) @@> (_, _, [arg1; arg2]) -> LExpr.Multiply((visit arg1 a), (visit arg2 a)) :> _

                // Object ops:
                | Call(Some t, mi, []) when mi.Name = "ToString" && isConvertible t.Type ->
                    LExpr.Convert(LExpr.Call(typeinfoof<Convert>.GetMethod("ChangeType", [| typeof<obj>; typeof<Type> |]),
                                             LExpr.Convert(visit t a, typeof<obj>), LExpr.Constant(t.Type)), t.Type) :> _

                // String ops:
                | Call(Some t, mi, args) when t.Type = typeof<string> && (mi.Name |> startsEither "ToUpper" "ToLower") ->
                    let name = (if mi.Name |> starts "ToUpper" then "ToLower" else "ToUpper") + mi.Name.Substring(7)
                    LExpr.Call((visit t a), typeinfoof<string>.GetMethod(name, mi.GetParameters() |> Array.map (fun p -> p.ParameterType))) :> _
                | SpecificCall <@@ String.Join @@> (_, _, [sep; Invert(strs, inner)]) ->
                    let sepexpr = visit sep a
                    let b = visit strs a
                    // We need to check if we're joining on the empty string,
                    //  becuase in that case, string.Split is *not* the inverse
                    LExpr.Condition(LExpr.Equal(sepexpr, LExpr.Constant("")),
                        // Array.map (fun c -> c.ToString()) (b.ToCharArray())
                        LExpr.Call(getGenericMethod <@@ Array.map @@> [| typeof<char>; typeof<string> |],
                            LExpr.Constant(fun (c : char) -> c.ToString()), LExpr.Call(b, typeinfoof<string>.GetMethod("ToCharArray", [||]))),
                        // str.Split([| sepexpr |], StringSplitOptions.None)
                        LExpr.Call(b, typeinfoof<string>.GetMethod("Split", [| typeof<string[]>; typeof<StringSplitOptions> |]),
                            LExpr.NewArrayInit(typeof<string>, sepexpr), LExpr.Constant(StringSplitOptions.None))) |> replace strs inner

                // Array ops:
                #if !NETFX_CORE
                | SpecificCall <@@ Array.ConvertAll @@> (_, _, [Invert(array, inner); converter])
                #endif
                | SpecificCall <@@ Array.map @@> (_, _, [converter; Invert(array, inner)]) ->
                    let arrayexpr  = visit array a
                    let mutable ce = visit converter a
                    #if !NETFX_CORE
                    if ce.Type.GetGenericTypeDefinition() = typedefof<Converter<_,_>> then
                        ce <- LExpr.Call(typedefof<FSharpFunc<_,_>>.MakeGenericType(ce.Type.GetGenericArguments()).GetMethod("FromConverter"), ce)
                    #endif
                    LExpr.Call(
                        ce.Type.GenericTypeArguments |> getGenericMethod <@@ Array.map @@>,
                        ce, arrayexpr) |> replace array inner

                // Easily invertible ops in the collections modules:
                | SpecificCall <@@ Map.ofSeq @@> (_, _, [Invert(arg, inner)]) as call ->
                    LExpr.Call(call.Type.GenericTypeArguments |> getGenericMethod <@@ Map.toSeq @@>, visit arg a) |> replace arg inner

                | other -> failwithf "Unsupported expr: %A" other
            let b = LExpr.Parameter(typeof<'b>)
            let del = LExpr.Lambda<Func<'b,'a>>(visit body (Map.ofList [(arg, b :> LExpr)]), b).Compile()
            fn, del.Invoke
        | other -> failwithf "Cannot determine inverse for expr: %A" other
