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

bhello |> printToStr // -> "Hello World!"

(**

## Parsing a Name

Cool, so let's actually do something interesting. Let's make a simple parser to
parse a first and last name separated by whitespace, possibly with a title beforehand.

Side note: In the real world, the format of names varies wildly. This is not meant to be a
comprehensive parser, only a simple example.

We'll start by defining some data structures to hold our name:
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
### Parsing a Choice

Let's define a boomerang to parse `Title`. We've already used the `blit` boomerang to parse a
literal string. We can use multiple `blit`s separated by the first-successful-parse operator,
`<|>`, to try to parse a few different titles:
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
Uh oh! That always prints "Mr" no matter what. To see why this is, we need to talk a
little more about channels.

### Channels

Generally, boomerangs write to their given channel when parsing, and read from the channel
when printing. So what's happening is that the first `blit` boomerang reads the string
"Mr" from its channel and prints it. Then, because it succeeded, the `<|>` operator
does not execute any of the other boomerangs.

What we really want is to expose a channel from our title boomerang to which the parsed result
will be written and from which the value to print will be read.

To do this, we'll use the channel-propagating first-successful-parse operator, `<.>`. That operator
behaves the same as `<|>`, except that it exposes the successful parse on a channel that you pass
to the resulting function. You'll notice that many of the operators in SharpBoomerang come in pairs
like this.

Simply changing `<|>` to `<.>` in our `btitle1` boomerang yields a compiler error. Passing a channel
(with %"..") to our `blit` partially applied it, consuming the channel argument. What we really want
is to only pass that constant when parsing, but to print the value propagated by the `<.>` operator.
We can do this with the `<??` operator:
*)

let btitle2 =
    (blit <?? %"Mr") <.>
    (blit <?? %"Ms") <.>
    (blit <?? %"Dr")

(**
Up to now, we've been dealing with boomerangs of type `Boomerang`. For these we've used the `parseFromStr`
and `printToStr` functions. However, you'll note that the type of `btitle2` is now effectively
`Boomerang<string>`. This means that it now exposes an `IChannel<string>` or `string channel`. This
enables us to read the title that was parsed and write the title to be printed. We do this using the
`parser` and `stringPrinter` functions like so:
*)

let parseTitle2 = StringInputChannel >> parser btitle2
parseTitle2 "Ms" // -> "Ms"

let printTitle2 = stringPrinter btitle2
printTitle2 "Ms" // -> "Ms"

(**
Ok, so now we have a couple functions: `parseTitle2` that takes a string and parses it into a title,
returning the resulting string, and `printTitle2` that takes a title string and prints it. This is
actually pretty useless. Instead of dealing with the title as a string, it would be much more useful
to deal with our `Title` discriminated union. We can do that by using the `>>%` operator to yield a
union case if the `blit` boomerang matches, like so:
*)

let btitle3 =
    (blit %"Mr" >>% Mr) <.>
    (blit %"Ms" >>% Ms) <.>
    (blit %"Dr" >>% Dr)

let parseTitle3 = StringInputChannel >> parser btitle3
parseTitle3 "Ms" // -> Ms

let printTitle3 = stringPrinter btitle3
printTitle3 Ms // -> "Ms"

(**
Note that we removed the `<??` operator because we no longer care about the string value-- the
`>>%` operator only cares about the success or failure of the preceeding boomerang.

Now the type of `btitle3` is `Boomerang<Title>` and this is looking much more useful! However,
this is kinda boilerplate-- parsing a discriminated union where none of the cases have arguments
is pretty common, and we have a built-in boomerang for that: `bdu<Title>`.

### Operators and Combinators

The real power of SharpBoomerang is in combining the built-in boomerangs with operators to create
new boomerangs, and then combining those, and so on. Hopefully, at this point you have an idea of
how this basically works. You've seen the built-in combinator, `blit`, for literal strings. We've
also just introduced `bdu<'a>` for discriminated unions. There are more of these, including `bws`
for parsing whitespace, and `bstr` for parsing a non-greedy arbitrary string of characters.

Putting these all together, let's "cut to the chase" and define the full boomerangs for parsing
our name:
*)

let bspace = !+(bws %' ') // one or more whitespace character (when printing, prints single space: ' ')
let btitle = ~~~(bdu<Title> .>> ~~(blit %".") %true .>> bspace)
let bname = btitle .>>. bstr .>> bspace .>>. bstr .>>% ((fun (t, f, l) -> { Title = t; First = f; Last = l }), (fun n -> (n.Title, n.First, n.Last)))

let parseName = StringInputChannel >> parser bname
let printName = stringPrinter bname

let einstein = parseName "Dr. Albert Einstein"
printName einstein


(**
Breaking it down, the `.>>` operator sequentially applies the left and then the right boomerang,
propagating the channel from the left. When we say "propagating," we mean exposing that channel
in the resulting function. As a rule of thumb, operators with dots (.) expose the channel(s)
from the boomerang(s) that are adjacent to the dot(s). For instance, the `.>>.` operator exposes
the channels from both boomerangs as a tuple. The `.>>%` maps the channel from the left
boomerang using an `Iso`. See `ChannelsAndIsos.fsx` for more about that.

Next, the `~~` operator is the optional operator. The boomerang to which it is applied becomes
optional to parse, and the exposed boolean channel determines whether that boomerang is printed.
Finally, `~~~` is the channel-propagating optional operator, which turns a `Boomerang<'a>` into
`Boomerang<'a option>`.
*)



