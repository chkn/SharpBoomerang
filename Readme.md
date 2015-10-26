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

Requirements
------------

### Visual Studio

You'll need Visual Studio 2015 -- earlier versions will not work. Just load SharpBoomerang.sln and go!

### Mono

If you're using Mono, you'll need to build and install F# 4.0 from source, as Mono doesn't package it yet. Thankfully, this is fairly straightforward:

1. Clone `https://github.com/fsharp/fsharp`
2. From the checkout, do the standard, `./configure && make`
3. Install it into your system Mono with `sudo make install`

After that you can load SharpBoomerang.sln in Xamarin Studio. The highlighting and code sense won't recognize F# 4.0 features, but compiling, running the tests, F# interactive, etc. should all work.

Note that running `Introduction.fsx` in F# interactive currently crashes Mono.

Docs
----

An introduction in F# can be found in `Introduction.fsx`. The HTML docs can be built by running `build.fsx`.

Tests
-----

The NUnit tests are under the `tests` directory. The tests should be runnable from the Unit Tests pad in Xamarin Studio or the Test Explorer in Visual Studio.


Contributions, Feedback, etc.
-----------------------------

All welcome! Feel free to file PRs or Issues. Also consider poking me on Twitter @chknofthescene, as I sometimes miss GitHub notifications.