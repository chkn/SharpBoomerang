namespace SharpBoomerang

open System
open System.Linq
open System.Linq.Expressions
open System.Reflection
open System.Text.RegularExpressions
open System.Collections.Generic

open Microsoft.FSharp.Math
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

/// Convenience for Linq expression trees
type private LExpr = Expression

/// Helper functions
[<AutoOpen>]
module private Helpers = 
    let inline starts start (str : string) =
        str.StartsWith(start, StringComparison.Ordinal)

    let inline startsEither start1 start2 (str : string) =
        str |> starts start1 || str |> starts start2

    let inline toDict keys values =
        (List.zip keys values).ToDictionary(fst, snd)

    type Expression with
        static member Lambda(e:Expression<_>) = e

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

                // Delegate ops:
                | NewDelegate(t, args, body) ->
                    let delegateType, orderedargs, argexprs =
                        let forward() = t, args, args |> List.map (fun a -> LExpr.Parameter(a.Type, a.Name))
                        if t.IsGenericType then
                            // For some delegate types, we need to compute the inverse of them as well
                            let gt = t.GetGenericTypeDefinition()
                            let reverse() =
                                let ga = t.GetGenericArguments()
                                let oa = List.rev args
                                gt.MakeGenericType(Array.rev ga), oa,
                                oa |> List.map (fun a -> LExpr.Parameter((if a.Type = ga.[0] then ga.[1] else a.Type), a.Name))

                            if gt = typedefof<Func<_,_>> then reverse() else
                            if gt = typedefof<Converter<_,_>> then reverse()
                            else forward()
                        else forward()
                    LExpr.Lambda(delegateType, visit body (toDict orderedargs argexprs) b, argexprs) :> _

                // Numerical ops:
                | SpecificCall <@@ (+) @@> (_, _, [arg1; arg2]) -> LExpr.Subtract((visit arg1 a b), (visit arg2 a b)) :> _
                | SpecificCall <@@ (-) @@> (_, _, [arg1; arg2]) -> LExpr.Add((visit arg1 a b), (visit arg2 a b)) :> _
                | SpecificCall <@@ (*) @@> (_, _, [arg1; arg2]) -> LExpr.Divide((visit arg1 a b), (visit arg2 a b)) :> _
                | SpecificCall <@@ (/) @@> (_, _, [arg1; arg2]) -> LExpr.Multiply((visit arg1 a b), (visit arg2 a b)) :> _

                // Object ops:
                | Call(Some t, mi, []) when mi.Name = "ToString" && typeof<IConvertible>.IsAssignableFrom(t.Type) ->
                    LExpr.Convert(LExpr.Call(typeof<Convert>.GetMethod("ChangeType", [| typeof<obj>; typeof<Type> |]), visit t a b, LExpr.Constant(t.Type)), t.Type) :> _

                // String ops:
                | Call(Some t, mi, args) when t.Type = typeof<string> && (mi.Name |> startsEither "ToUpper" "ToLower") ->
                    let name = (if mi.Name |> starts "ToUpper" then "ToLower" else "ToUpper") + mi.Name.Substring(7)
                    LExpr.Call((visit t a b), typeof<string>.GetMethod(name, mi.GetParameters() |> Array.map (fun p -> p.ParameterType))) :> _
                | SpecificCall <@@ String.Join @@> (_, _, [sep; strs]) ->
                    let sepexpr = visit sep a b
                    // We need to check if we're joining on the empty string,
                    //  becuase in that case, string.Split is *not* the inverse
                    let results = LExpr.Condition(LExpr.Equal(sepexpr, LExpr.Constant("")),
                                    // Array.ConvertAll(b.ToCharArray(), (fun c -> c.ToString()))
                                    LExpr.Call(typeof<Array>.GetMethod("ConvertAll").MakeGenericMethod(typeof<char>, typeof<string>),
                                        LExpr.Call(b, typeof<string>.GetMethod("ToCharArray", Type.EmptyTypes)), LExpr.Lambda<Converter<char,string>>(fun c -> c.ToString())),
                                    // str.Split([| sepexpr |], StringSplitOptions.None)
                                    LExpr.Call(b, typeof<string>.GetMethod("Split", [| typeof<string[]>; typeof<StringSplitOptions> |]),
                                        LExpr.NewArrayInit(typeof<string>, sepexpr), LExpr.Constant(StringSplitOptions.None))) :> LExpr
                    (visit strs a results)

                // Array ops:
                | SpecificCall <@@ Array.ConvertAll @@> (_, _, [array; converter]) ->
                    let arrayexpr = visit array a b
                    let converterexpr = visit converter a b
                    LExpr.Call(typeof<Array>.GetMethod("ConvertAll")
                        .MakeGenericMethod(converterexpr.Type.GetGenericArguments()), arrayexpr, converterexpr) :> _

                | other -> failwithf "Unsupported expr: %A" other
            let b = LExpr.Parameter(typeof<'b>)
            let del = LExpr.Lambda<Converter<'b,'a>>(visit body (Dictionary<_,_>()) b, b).Compile()
            fn, FuncConvert.ToFSharpFunc(del)
        | other -> failwithf "Cannot determine inverse for expr: %A" other
