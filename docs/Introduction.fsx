(**
# Introduction

Hey there! This document will give you a quick crash course in SharpBoomerang.

SharpBoomerang is a library for specifying a grammar for both a parser and a
pretty printer simultaneously. Every construct in SharpBoomerang is designed
to be bidirectional.

## Follow Along

Open this file in Xamarin Studio or Visual Studio and use Ctrl+Enter (XS) or
Alt+Enter (VS) to execute the following code snippets in F# interactive.

The first thing we need to do is to load the source files we'll be using:
*)
#load @"../src/util.fs"
#load @"../src/iso.fs"
#load @"../src/channel.fs"
#load @"../src/boomerang.fs"

(**
Then, open the SharpBoomerang namespaces to bring its functions into the global scope:
*)
open SharpBoomerang
open SharpBoomerang.Combinators

(**

## Hello World

Okay, now that we're set up, here's how you'd write a boomerang for "Hello World!":
*)

let bhello = blit %"Hello World!"

(**
By convention, boomerangs start with the letter "b." `blit` is the boomerang for
parsing/printing a literal value. To be more specific, `blit` reads from its channel and
then either parses expecting to find the given string, or prints the string. The percent
sign (%) prefix operator creates a read-only channel that holds a constant value.

On a side note, instead of "parsing/printing," we'll just say "parsing" from now on,
but all constructs can do either.

As a simple example, let's see how we can parse "Hello World!" using that boomerang:
*)


bhello |> parseFromStr "Hello World!"

(**
Run that annnddd... nothing happens. However, if you try changing the "Hello World!" string
passed to `parseFromStr`, you'll get an exception because the parse failed.

Just for completeness, here's the printer for that:
*)

bhello |> printToStr // prints "Hello World!"

(**

## Parsing a Name

Cool, so let's actually do something interesting. Let's make a simple parser to
parse two strings separated by whitespace, possibly with a title beforehand.

### Parsing a Choice

We've already used the `blit` boomerang to parse a literal string. We can use
multiple `blit`s separated by the first-successful-parse operator, `<|>`, to
try to parse a few different titles:
*)

let btitle1 =
    (blit %"Mr") <|>
    (blit %"Ms") <|>
    (blit %"Dr")

(**
Let's try that out:
*)

// should work:
btitle1 |> parseFromStr "Dr"

// uncomment, should fail:
//btitle1 |> parseFromStr "Bs"

(**
Play with the strings passed to `parseFromStr` above and you'll see that it correctly
only accepts "Mr", "Ms", or "Dr".

Now let's try the printer:
*)

btitle1 |> printToStr

(**
Well that always prints "Mr" no matter what. To see why this is, we need to talk a
little more about channels.

### Channels

Generally, boomerangs write to the given channel when parsing, and read from the channel
when printing. So what's happening is that the first `blit` boomerang reads the string
"Mr" from its channel and prints it. Then, because it succeeded, the `<|>` operator
does not execute any of the other boomerangs.

What we really want to happen is to have some sort of variable to hold the actual
title that was parsed. Then, when printing, that variable should be read to print
the correct title again. To do this, we'll use the channel-propagating first-successful-parse
operator, `<.>`. That operator behaves the same as `<|>`, except that it exposes
the successful parse on a channel that you pass to the resulting function. You'll note
that simply changing `<|>` to `<.>` in our `btitle1` boomerang yields a compiler error.
Passing a channel (with %"..") to our `blit` partially applied it, consuming the
channel argument. What we really want is to only pass that constant when parsing,
but to print the value propagated by the `<.>` operator. We can do this with the
`<??` operator:
*)

let btitle2 =
    (blit <?? %"Mr") <.>
    (blit <?? %"Ms") <.>
    (blit <?? %"Dr")

(**
You'll note that the type of `btitle2` is now `IChannel<string> -> Context -> Context`,
when before it was just `Context -> Context`. This is because we now have
a channel that will expose the successfully parsed string.

We want our title to be optional, so we'll define a `string option` mutable binding
to hold the value, and define a `btitle3` to optionally parse a title into that binding:
*)

let mutable title = None
let btitle3 = ~~~btitle2 .-> <@ title @>

(**
The `~~~` prefix operator takes a boomerang exposing an `IChannel<'t>` and makes it optional,
exposing an `IChannel<'t option>`. Then, the result of that is piped, via the `.->` operator,
into a mutable binding. Remember, everything in SharpBoomerang is bidirectional, which is
why a quoted expression, `<@ title @>` is used to indicate the binding -- when parsing, the
result will be written there, and when printing, that value will be read.

Let's give it a try and see if it works:
*)

btitle3 |> parseFromStr "Dr"
title // is now "Dr"

btitle3 |> printToStr // now prints "Dr"

title <- Some "Mr"
btitle3 |> printToStr // now prints "Mr"

btitle3 |> parseFromStr "Bs"
title // is now None

(**
Our title will be followed by an optional dot (.) and then some whitespace.
 Let's define a boomerang that will match that, but only if `title` is `Some`:
*)

let bdotwsiftitle = ~~(~~(blit %".") %true >> !+(bws %' ')) %%(fun ret -> ret title.IsSome)

(**
The `~~` prefix operator indicates that the boomerang is optional to parse, and exposes an
`IChannel<bool>` to determine if it is printed. For the "." after the title, it will be optional
to parse, but always printed, so we can pass a constant `true`. For the overall boomerang, we only
want to print it if `title.IsSome`.

The `bws` boomerang matches any single character of whitespace (' ', '\t', '\r', '\n').
In our case, we don't care what character was actually parsed and we always want to print a single space,
so we just pass a read-only channel with that. The `!+` prefix operator indicates that the boomerang can
be greedily matched one or more times, though when printing, it will only print once.

Okay, so let's put this all together into a full parser for our definition of a name:
*)

let mutable firstName = ""
let mutable lastName = ""

let bname1 = btitle3 >> bdotwsiftitle >> (bstr .-> <@ firstName @>) >> !+(bws %' ') >> (bstr .-> <@ lastName @>)

bname1 |> parseFromStr "Mr. Elmer Fudd"

bname1 |> printToStr

(**
Play with the name passed in to `parseFromStr`, and you'll see that this pretty much works:
the different components are parsed out into the different mutable bindings and then those
values are also used to pretty print.

### Data Structures

Parsing into a bunch of mutable variables is less than ideal. It'd be nice if we could use
some immutable data structures like this:
*)

type Title =
    | Mr
    | Ms
    | Dr

type Name = {
    Title : Title option;
    First : string;
    Last  : string;
    }

(**
Let's start again and define boomerangs to parse our title using these data structures.
As our title is now a discriminated union, and none of its cases take any arguments, we
can now use the `bdu` built-in boomerang to quickly parse that:
*)

let btitle = ~~~(bdu<Title> .>> ~~(blit %".") %true .>> !+(bws %' '))

let bname = btitle .>>. bstr .>> !+(bws %' ') .>>. bstr .>>% ((fun (t, f, l) -> { Title = t; First = f; Last = l }), (fun n -> (n.Title, n.First, n.Last)))

let parseName = StringInputChannel >> parser bname

parseName "Alex Corrado"

