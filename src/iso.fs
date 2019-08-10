namespace SharpBoomerang

open System
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

/// Convenience methods for creating an `Iso`
type Iso =

    /// Represents an Iso that only supports a single direction transformation
    static member oneWay(fn : 'a -> 'b) : Iso<_,_> = fn, (fun _ -> failwith "Reverse transformation not supported")

    /// Given a simple function, `'a -> 'b`, attempts to derive the inverse implicitly
    // This method is a work in progress. We will add cases as they come up.
    static member ofFn([<ReflectedDefinition(true)>] expr : Expr<('a -> 'b)>) : Iso<_,_> =
        // Call inversion
        //  Given a chain of calls, A(B(C(d))) = d'
        //    where A, B, and C are functions and d is some argument expression,
        //  the inverse of that is C'(B'(A'(d'))) = d,
        //    where A', B', and C' are the inverse functions of A, B, and C respectively.
        // To achieve that, this function translates A(B(C(d))) -> [C; B; A; d] 
        let rec invert lst = function
        | SpecificCall <@@ (|>) @@> (_, _, [arg; _])

        // 1 argument
        | Call(_, _, [arg])
        | NewUnionCase(_, [arg])

        // 2 arguments
        | Call(_, _, [_; arg])
        | NewUnionCase(_, [_; arg]) as expr -> expr :: invert arg

        | expr -> [expr]

        match expr with
        | WithValue(:? ('a -> 'b) as fn, _, expr) ->
            let rec visitAll exprs a = seq { for e in exprs do yield visit e a }
            and visit e (a : Map<Var,LExpr>) : LExpr =
                match e with
                // Values:
                | Var(v) -> a.[v]
                | Value(v, _) -> LExpr.Constant(v) :> _
                | Coerce(e, t) -> LExpr.Retype(visit e a, t)

                // Strictly speaking, the inverse of NewObject for an arbitrary type is
                //  undefined, but we need this here for cases like raise(Exception(...))
                | NewObject(ctor, args) -> LExpr.New(ctor, visitAll args a) :> _

                // DU:
                | NewUnionCase(case, [Invert(arg, inner)]) ->
                    // The inverse of creating a union with one arg is decomposing the case into the arg
                    let prop = Array.exactlyOne (case.GetFields())
                    LExpr.Property(LExpr.Retype(visit arg a, prop.DeclaringType), prop)
                    |> replace arg inner
                | NewUnionCase(case, args) ->
                    // For our purposes, the inverse of creating a union case with more than one arg is decomposing into a tuple
                    let props = case.GetFields()
                    let propGets = (args, props)
                                   ||> Seq.map2 (fun arg prop -> LExpr.Property(visit arg a, prop) :> LExpr)
                                   |> Seq.toArray
                    let types = propGets |> Array.map (fun p -> p.Type)
                    LExpr.Tuple(FSharpType.MakeTupleType(types), propGets)

                // Tuple:
                | NewTuple(args) as nt ->
                    // For our purposes, the inverse of creating a tuple is creating a tuple of the inverse of our args
                    let inverseArgs = visitAll args a |> Seq.toArray
                    let types = inverseArgs |> Array.map (fun a -> a.Type)
                    LExpr.Tuple(FSharpType.MakeTupleType(types), inverseArgs)

                // Decomposition:
                | UnionDecompose(t, args, (TupleCompose(_, _, exprs) as nt))
                | TupleDecompose(t, args, (TupleCompose(_, _, exprs) as nt)) ->
                    let inpts, a = (args, exprs)
                                   ||> Seq.map2 (fun v e -> v, LExpr.Parameter(e.Type, v.Name))
                                   |> Seq.mapFold (fun a (v, p) -> p, a |> Map.add v (p :> LExpr)) a
                    LExpr.Block(t.Type, inpts,
                        LExpr.AssignFromTuple(inpts, visit t a),
                        visit nt a
                    ) :> _

                | Let(v, body, following) ->
                    let p = LExpr.Parameter(v.Type, v.Name)
                    let a = a.Add(v, p)
                    LExpr.Block([| p |], LExpr.Assign(p, visit body a), visit following a) :> _

                // Exceptions:
                | SpecificCall <@@ raise @@> (_, _, [arg]) -> LExpr.Throw(visit arg a, e.Type) :> _
                | SpecificCall <@@ failwith @@> (_, _, [arg]) -> LExpr.Throw(LExpr.New(typeinfoof<Exception>.GetConstructor([| typeof<string> |]), visit arg a), e.Type) :> _

                // Control flow:
                | Lambda(v, e) ->
                    // We swap the argument and return types
                    let p = LExpr.Parameter(e.Type, v.Name)
                    LExpr.FSharpFunc(p, visit e (a.Add(v, p))) :> _

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
                                let ga = ti.GetGenericArguments()
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
                            LExpr.NewArrayInit(typeof<string>, sepexpr), LExpr.Constant(StringSplitOptions.None))
                    )
                    |> replace strs inner

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
                        ce.Type.GetTypeInfo().GetGenericArguments() |> getGenericMethod <@@ Array.map @@>,
                        ce, arrayexpr)
                    |> replace array inner

                // Easily invertible ops in the collections modules:
                | SpecificCall <@@ Map.ofSeq @@> (_, _, [Invert(arg, inner)]) as call ->
                    LExpr.Call(call.Type.GetTypeInfo().GetGenericArguments()
                               |> getGenericMethod <@@ Map.toSeq @@>, visit arg a
                    )
                    |> replace arg inner

                | other -> failwithf "Unsupported expr: %A" other

            // But first, we need to find the outer lambda expr
            let rec outerVisit expr args =
                match expr with
                | Let(arg, body, following) ->
                    args
                    |> Map.add arg (visit body args)
                    |> outerVisit following
                | Lambda(arg, body) ->
                    let newArg  = LExpr.Parameter(typeof<'b>)
                    let newBody =
                        args
                        |> Map.add arg (newArg :> _)
                        |> visit body
                    let del = LExpr.Lambda<Func<'b,'a>>(newBody, newArg).Compile()
                    fn, del.Invoke
                | other -> failwithf "Cannot determine inverse for expr: %A" other

            outerVisit expr Map.empty
        | other -> failwithf "Impossible expr: %A" other

/// A module of predefined `Iso`s
module Isos =

    /// An `Iso` mapping an option to a default value if None
    let defaultTo defaultValue : Iso<_,_> = (function | Some(value) -> value | _ -> defaultValue), Some
