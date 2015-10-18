SharpBoomerang
==============

Parser and unparser (pretty printer) from the same grammar. Somewhat akin to the [boomerang package for Haskell][0], or the [Boomerang language][1].

[0]: https://hackage.haskell.org/package/boomerang
[1]: http://www.seas.upenn.edu/~harmony/

Features
--------

- Written in F# 4.0
- In F#, you can define the grammar in a functional style with combinators!

Considerations
--------------

- Still pretty rough, especially from C#
- Not many tests yet
- Not yet optimized for performance
- Not yet a complete parsing framework

Dragons!
--------

If you're using Mono, you'll need to build and install F# 4.0 from source, as Mono doesn't package it yet. Thankfully, this is fairly straightforward:

1. Clone `https://github.com/fsharp/fsharp`
2. From the checkout, do the standard, `./configure && make`
3. Install it into your system Mono with `sudo make install`

Also note that running `Introduction.fsx` in FSI currently crashes Mono.

Docs
----

An introduction in F# can be found in `Introduction.fsx`. The HTML docs can be built by running `build.fsx`.

Contributions, Feedback, etc.
-----------------------------
All welcome! Feel free to file PRs or Issues. Also consider poking me on Twitter @chknofthescene, as I sometimes miss GitHub notifications.