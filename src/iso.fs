(**
Isomorphisms
============

Some crazy Greek mathematical thing. See the [Wikipedia page][0] for the gory details.

[0]: https://en.wikipedia.org/wiki/Isomorphism

TL;DR it's a function, `'a -> 'b`, and its inverse, `'b -> 'a`
*)

namespace SharpBoomerang

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Quotations.DerivedPatterns


/// Total isomorphism of a <> b
type Iso<'a,'b> = ('a -> 'b) * ('b -> 'a)

[<AbstractClass; Sealed>]
type Iso private () =

    /// Given a simple function, `'a -> 'b`, attempts to derive the inverse implicitly
    [<CompiledName("FromFunction")>]
    static member ofFn([<ReflectedDefinition(true)>] expr : Expr<('a -> 'b)>) : Iso<_,_> =
        match expr with
        | WithValue(v, e) ->
            let rec invert = function
            | ShapeVar var -> Expr.Var var
            | ShapeLambda (var, expr) -> Expr.Lambda (var, invert expr)
            | ShapeCombination(shapeComboObject, exprList) ->
                RebuildShapeCombination(shapeComboObject, List.map invert exprList)
            //(v, invert
