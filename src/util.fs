namespace SharpBoomerang

open System
open System.Reflection
open System.Collections
open System.Collections.Generic

open System.Linq
open System.Linq.Expressions

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

/// Convenience for Linq expression trees
type internal LExpr = Expression

/// Helper functions
[<AutoOpen>]
module internal Helpers = 
    let inline starts start (str : string) =
        str.StartsWith(start, StringComparison.Ordinal)

    let inline startsEither start1 start2 (str : string) =
           str |> starts start1
        || str |> starts start2

    let inline toDict keys values =
        (List.zip keys values).ToDictionary(fst, snd)

    // Expr helpers:

    // Functions of the form: fun (a, b, c, ...) -> ...
    // | TupleDecompose(tuple : Expr, args : Var list, body : Expr)
    let rec (|TupleDecompose|_|) = function
    | Let(v, TupleGet(t, _), TupleDecompose(t', args, body)) when t = t' -> Some (t, v :: args, body)
    | Let(v, TupleGet(t, _), body) -> Some (t, [v], body)
    | _ -> None

    // Reflection helpers:

    let rec getMethod = function
    | Lambda(_, e)  -> getMethod e
    | Let(_, _, e)  -> getMethod e
    | Call(_, m, _) -> m
    | other -> failwithf "Unsupported Expr: %A" other

    let getGenericMethod expr types =
        (getMethod expr).GetGenericMethodDefinition().MakeGenericMethod(types)

    let argsMatch argTypes (m : MethodBase) =
        let methodTypes = m.GetParameters() |> Array.map (fun p -> p.ParameterType)
        argTypes = methodTypes

    #if NETFX_CORE
    type TypeInfo with
        member ti.GetMethod(name) = ti.GetDeclaredMethods(name) |> Seq.exactlyOne
        member ti.GetMethod(name, argTypes) =
            ti.GetDeclaredMethods(name) |> Seq.find (argsMatch argTypes)
        member ti.GetConstructor(argTypes) =
            ti.DeclaredConstructors |> Seq.find (argsMatch argTypes)
    #else
    type Type with
        member t.GetTypeInfo() = t
        member t.GenericTypeArguments = t.GetGenericArguments()
        member t.DeclaredConstructors = t.GetConstructors()
    #endif

    let inline typeinfoof<'t> = typeof<'t>.GetTypeInfo()

    let inline isConvertible t =
    #if NETFX_CORE
           t = typeof<int>
        || t = typeof<Int64>
        || t = typeof<double>
        || t = typeof<float>
        || t = typeof<bool>
        || t = typeof<decimal>
        || t = typeof<byte>
        || t = typeof<Int16>
    #else
        typeof<IConvertible>.IsAssignableFrom(t)
    #endif

    // Expression tree helpers:

    type ExpressionFunc<'t,'r>(p, e) =
        inherit FSharpFunc<'t,'r>()
        let func = lazy(LExpr.Lambda<Func<'t,'r>>(e, [| p |]).Compile())
        override __.Invoke(arg) = func.Force().Invoke(arg)

    type Expression with
        /// Lambda from F# func, quoted automatically by the compiler
        ///  See http://stackoverflow.com/a/23146624/578190
        static member Lambda(e:Expression<_>) = e

        /// Expression representing an FSharpFunc
        static member FSharpFunc(p : ParameterExpression, e : Expression) =
            let funcType = typedefof<ExpressionFunc<_,_>>.MakeGenericType(p.Type, e.Type)
            let ctor = funcType.GetTypeInfo().DeclaredConstructors |> Seq.exactlyOne
            LExpr.New(ctor, LExpr.Constant(p), LExpr.Constant(e))

        /// Conversion that retypes certain expressions
        static member Retype(e : Expression, t : Type) : Expression =
            match e.NodeType with
            | ExpressionType.Default -> LExpr.Default(t) :> _
            | _ ->
                match e with
                | :? UnaryExpression as unary ->
                    match e.NodeType with
                    | ExpressionType.Throw -> LExpr.Throw(unary.Operand, t) :> _
                    | ExpressionType.Convert -> LExpr.Convert(unary.Operand, t) :> _
                    | _ -> LExpr.Convert(e, t) :> _
                | _ -> LExpr.Convert(e, t) :> _

        static member AssignAll(ps : ParameterExpression seq, array : Expression) : Expression =
            let var = LExpr.Variable(array.Type)
            let body = Seq.append (LExpr.Assign(var, array) :> LExpr |> Seq.singleton)
                       <| Seq.mapi (fun i p ->
                            LExpr.Assign(p, LExpr.Convert(LExpr.ArrayAccess(var, [| LExpr.Constant(i) :> LExpr |]), p.Type)) :> LExpr) ps
            LExpr.Block([| var |], body) :> _