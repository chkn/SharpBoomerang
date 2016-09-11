[<AutoOpen>]
module internal SharpBoomerang.Util

open System
open System.Reflection
open System.Collections
open System.Collections.Generic

open System.Linq
open System.Linq.Expressions

open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

/// Convenience for Linq expression trees
type internal LExpr = Expression

// String helpers:

let inline starts start (str : string) = str.StartsWith(start, StringComparison.Ordinal)
let inline startsEither start1 start2 str = starts start1 str || starts start2 str

// Expr helpers:

module Expr =
    // http://www.fssnip.net/1i
    let rec replace f q = 
        let q = defaultArg (f q) q
        match q with
        | ExprShape.ShapeCombination(a, args) -> 
            let nargs = args |> List.map (replace f)
            ExprShape.RebuildShapeCombination(a, nargs)
        | ExprShape.ShapeLambda(v, body)  -> 
            Expr.Lambda(v, replace f body)
        | ExprShape.ShapeVar(v) ->
            Expr.Var(v)

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

// Functions of the form: fun (a, b, c, ...) -> ...
// | TupleDecompose(tuple : Expr, args : Var list, body : Expr)
let rec (|TupleDecompose|_|) = function
| Let(v, TupleGet(t, _), TupleDecompose(t', args, body)) when t = t' -> Some (t, v :: args, body)
| Let(v, TupleGet(t, _), body) -> Some (t, [v], body)
| _ -> None

// Matches constructs that logically take a tuple, e.g. tupling, DU creation, method call
// | TupleCompose(target : Expr option, constructor : MethodBase, args : Expr list)
let (|TupleCompose|_|) = function
| NewTuple(args) as nt ->
    match FSharpValue.PreComputeTupleConstructorInfo(nt.Type) with
    | ctor, None -> Some (None, ctor :> MethodBase, args)
    | _, _ -> failwith "Large tuple support not implemented"
| NewUnionCase(case, args) -> Some (None, FSharpValue.PreComputeUnionConstructorInfo(case, true) :> _, args)
| Call(targ, mthd, args) -> Some (targ, mthd :> _, args)
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
    member ti.GetMethod(name, argTypes) = ti.GetDeclaredMethods(name) |> Seq.find (argsMatch argTypes)
    member ti.GetProperty(name) = ti.GetDeclaredProperty(name)
    member ti.GetConstructor(argTypes) = ti.DeclaredConstructors |> Seq.find (argsMatch argTypes)
    member ti.GetGenericArguments() = ti.GenericTypeArguments
    member ti.GetConstructors() = ti.DeclaredConstructors
#else
type Type with
    member t.GetTypeInfo() = t
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
        let ctor = funcType.GetTypeInfo().GetConstructors() |> Seq.exactlyOne
        LExpr.New(ctor, LExpr.Constant(p), LExpr.Constant(e))

    /// Expression representing the creation of a Tuple
    static member Tuple(tupleType : Type, items : Expression seq) : Expression =
        match FSharpValue.PreComputeTupleConstructorInfo(tupleType) with
        | ci, None -> LExpr.New(ci, items) :> _
        | _, _ -> failwith "Large tuple support not implemented"

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

    /// Expression that represents a Tuple's fields as obj[]
    static member GetTupleFields(tuple : Expression) : Expression =
        let getTupleFields = typeinfoof<FSharpValue>.GetMethod("GetTupleFields")
        LExpr.Call(getTupleFields, tuple) :> _

    /// Expression that represents a discriminated union's fields as obj[]
    static member GetUnionFields(case : Expression) : Expression =
        let getCaseFields = getMethod <@@ FSharpValue.GetUnionFields(null, typeof<obj>, true) @@>
        let tuple = LExpr.Call(getCaseFields, case, LExpr.Constant(case.Type), LExpr.Constant(Some(true))) :> LExpr
        let item2 = typeinfoof<Tuple<UnionCaseInfo,obj[]>>.GetProperty("Item2")
        LExpr.Property(tuple, item2) :> _

    static member AssignFromArray(ps : ParameterExpression seq, array : Expression) : Expression =
        let var = LExpr.Variable(array.Type)
        let body = Seq.append (LExpr.Assign(var, array) :> LExpr |> Seq.singleton)
                    <| Seq.mapi (fun i p ->
                        LExpr.Assign(p, LExpr.Convert(LExpr.ArrayAccess(var, [| LExpr.Constant(i) :> LExpr |]), p.Type)) :> LExpr) ps
        LExpr.Block([| var |], body) :> _

    static member AssignFromTuple(ps : ParameterExpression seq, tuple : Expression) : Expression =
        let fields = Expression.GetTupleFields(tuple)
        LExpr.AssignFromArray(ps, fields)

