namespace SharpBoomerang

open System
open System.Linq
open System.Linq.Expressions
open System.Reflection
open System.Text.RegularExpressions
open System.Collections.Generic

open Microsoft.FSharp.Math
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
    static member ofFn([<ReflectedDefinition(true)>] expr : Expr<('a -> 'b)>) : Iso<_,_> =
        match expr with
        | WithValue(:? ('a -> 'b) as fn, _, Lambda(arg, body)) ->
            let rec visitAll exprs a b = seq { for e in exprs do yield visit e a b }
            and visit e (a : Dictionary<Var,ParameterExpression>) b : LExpr =
                match e with
                // Values:
                | Value(v, _) -> LExpr.Constant(v) :> _
                | Var(v) when v = arg -> b :> _
                | Var(v) -> a.[v] :> _

                // | Let(v, body, following) -> Expression.Ass
                | Lambda(v, e) ->
                    // We swap the argument and return types
                    let p = LExpr.Parameter(e.Type, v.Name)
                    let d = toDict [v] [p]
                    LExpr.FSharpFunc(p, visit e d b) :> _

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
                    LExpr.Lambda(delegateType, visit body (toDict orderedargs argexprs) b, argexprs) :> _

                // Numerical ops:
                | SpecificCall <@@ (+) @@> (_, _, [arg1; arg2]) -> LExpr.Subtract((visit arg1 a b), (visit arg2 a b)) :> _
                | SpecificCall <@@ (-) @@> (_, _, [arg1; arg2]) -> LExpr.Add((visit arg1 a b), (visit arg2 a b)) :> _
                | SpecificCall <@@ (*) @@> (_, _, [arg1; arg2]) -> LExpr.Divide((visit arg1 a b), (visit arg2 a b)) :> _
                | SpecificCall <@@ (/) @@> (_, _, [arg1; arg2]) -> LExpr.Multiply((visit arg1 a b), (visit arg2 a b)) :> _

                // Object ops:
                | Call(Some t, mi, []) when mi.Name = "ToString" && isConvertible t.Type ->
                    LExpr.Convert(LExpr.Call(typeinfoof<Convert>.GetMethod("ChangeType", [| typeof<obj>; typeof<Type> |]),
                                             LExpr.Convert(visit t a b, typeof<obj>), LExpr.Constant(t.Type)), t.Type) :> _

                // String ops:
                | Call(Some t, mi, args) when t.Type = typeof<string> && (mi.Name |> startsEither "ToUpper" "ToLower") ->
                    let name = (if mi.Name |> starts "ToUpper" then "ToLower" else "ToUpper") + mi.Name.Substring(7)
                    LExpr.Call((visit t a b), typeinfoof<string>.GetMethod(name, mi.GetParameters() |> Array.map (fun p -> p.ParameterType))) :> _
                | SpecificCall <@@ String.Join @@> (_, _, [sep; strs]) ->
                    let sepexpr = visit sep a b
                    // We need to check if we're joining on the empty string,
                    //  becuase in that case, string.Split is *not* the inverse
                    let results = LExpr.Condition(LExpr.Equal(sepexpr, LExpr.Constant("")),
                                    // Array.map (fun c -> c.ToString()) (b.ToCharArray())
                                    LExpr.Call(getGenericMethod <@@ Array.map @@> [| typeof<char>; typeof<string> |],
                                        LExpr.Constant(fun (c : char) -> c.ToString()), LExpr.Call(b, typeinfoof<string>.GetMethod("ToCharArray", [||]))),
                                    // str.Split([| sepexpr |], StringSplitOptions.None)
                                    LExpr.Call(b, typeinfoof<string>.GetMethod("Split", [| typeof<string[]>; typeof<StringSplitOptions> |]),
                                        LExpr.NewArrayInit(typeof<string>, sepexpr), LExpr.Constant(StringSplitOptions.None))) :> LExpr
                    (visit strs a results)

                // Array ops:
                #if !NETFX_CORE
                | SpecificCall <@@ Array.ConvertAll @@> (_, _, [array; converter])
                #endif
                | SpecificCall <@@ Array.map @@> (_, _, [converter; array]) ->
                    let arrayexpr  = visit array a b
                    let mutable ce = visit converter a b
                    #if !NETFX_CORE
                    if ce.Type.GetGenericTypeDefinition() = typedefof<Converter<_,_>> then
                        ce <- LExpr.Call(typedefof<FSharpFunc<_,_>>.MakeGenericType(ce.Type.GetGenericArguments()).GetMethod("FromConverter"), ce)
                    #endif
                    LExpr.Call(
                        ce.Type.GenericTypeArguments |> getGenericMethod <@@ Array.map @@>,
                        ce, arrayexpr) :> _
                
                | other -> failwithf "Unsupported expr: %A" other
            let b = LExpr.Parameter(typeof<'b>)
            let del = LExpr.Lambda<Func<'b,'a>>(visit body (Dictionary<_,_>()) b, b).Compile()
            fn, del.Invoke
        | other -> failwithf "Cannot determine inverse for expr: %A" other
