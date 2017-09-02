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

[2]: http://www.mono-project.com/download/beta/

Docs
----

Documentation can be found in the `docs` directory, or [online][3]. The HTML docs can be built like this:

    [Windows] build.cmd docs
    [Mac] build.fsx docs

[3]: http://chkn.github.io/SharpBoomerang/docs/Introduction.html

### TL;DR

For those who are impatient, here are some samples that are more fully discussed in the docs.

    [hide]
    #load @"SharpBoomerang.fsx"
    open SharpBoomerang
    open SharpBoomerang.Combinators

#### Hello World

    let bhello =
        blit %"Hello" >>       // boomerang the literal string, "Hello"
        ~~(blit %",") %true >> // optionally parse a comma (",") -- "%true" means it will always be printed
        !+(bws %' ') >>        // parse one or more whitespace characters -- it will print one space
        blit %"World!"         // boomerang the literal string, "World!"

#### Parse a Name

    type Title =
        | Mr
        | Ms
        | Dr

    type Name = {
        Title : Title option;
        First : string;
        Last  : string;
    }

    let bspace = !+(bws %' ')
    let btitle = ~~~(bdu<Title> .>> ~~(blit %".") %true .>> bspace)
    let bname = btitle .>>. bstr .>> bspace .>>. bstr .>>% ((fun (t, f, l) -> { Title = t; First = f; Last = l }), (fun n -> (n.Title, n.First, n.Last)))

#### Parse JSON

See the full document [here][4].

[4]: http://chkn.github.io/SharpBoomerang/examples/JSON.html


Tests
-----

The NUnit tests are under the `tests` directory. The tests should be runnable from the Unit Tests pad in Xamarin Studio or the Test Explorer in Visual Studio.

Building the NuGet Package
--------------------------

Sometimes it's helpful to build the NuGet package locally. To do this:

    [Windows] build.cmd nupkg
    [Mac] build.fsx nupkg

If you have not already built a Release build, the build will run as part of this command. Note that you'll need to have previously restored the NuGet packages otherwise it will fail.


Contributions, Feedback, etc.
-----------------------------

All welcome! Feel free to file PRs or Issues. Also consider poking me on Twitter @chknofthescene, as I sometimes miss GitHub notifications.