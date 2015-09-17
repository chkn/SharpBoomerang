SharpBoomerang
==============

Parser and unparser (pretty printer) from the same grammar.
Inspired by the [boomerang package for Haskell][0].

[0]: https://hackage.haskell.org/package/boomerang

Pros
----

- Written in F# 4.0
- In F#, you can define the grammar in a functional style with combinators!
- C# love coming soon!

Cons
----

- Still pretty rough, especially from C#
- Must put entire text to parse into a `System.String`
- Not many tests yet
- Not yet optimized for performance
- Not yet a complete parsing framework

Docs
----

An introduction in F# can be found in `Introduction.fsx`.

