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

You'll need Visual Studio 2015 -- earlier versions will not work. Just load `SharpBoomerang.sln` and go!

### Xamarin Studio

If you are using Mono, you'll need version 4.3.1 or newer. Just load `SharpBoomerang.sln` in Xamarin Studio and go!

Docs
----

An introduction in F# can be found in `docs/Introduction.fsx`. The HTML docs can be built by running `build.fsx`.

Tests
-----

The NUnit tests are under the `tests` directory. The tests should be runnable from the Unit Tests pad in Xamarin Studio or the Test Explorer in Visual Studio.


Contributions, Feedback, etc.
-----------------------------

All welcome! Feel free to file PRs or Issues. Also consider poking me on Twitter @chknofthescene, as I sometimes miss GitHub notifications.