(**
The easiest way to follow along is to just open this file in Xamarin Studio or
Visual Studio and use Ctrl+Enter (XS) or Alt+Enter (VS) to execute the following code
snippets in F# interactive.

The first thing we need to do is to load the SharpBoomerang library
 (change the path here if this isn't where it is):
*)
#r "bin/Debug/SharpBoomerang.dll"

(**
Then, open the SharpBoomerang namespaces to bring its functions into the global scope:
*)
open SharpBoomerang
open SharpBoomerang.FSharp

(**

Hello World
-----------

Okay, now that we're set up, here's how you'd write a boomerang for "Hello World!":
*)

let bhello = blit %"Hello World!"

(**
`blit` is the boomerang for parsing/printing a literal value. To be more specific,
`blit` reads from its channel and then either parses expecting to find the given string,
or prints the string. The percent sign prefix operator creates a `ConstChannel` that
holds a constant value.

On a side note, since it's a drag to always say "parsing/printing," we'll just say "parsing"
from now on, but all constructs can do either.

Granted, this example is a bit boring, but let's see how we can parse "Hello World!"
using that boomerang:
*)


bhello |> parseFromStr "Hello World!"

(**
Run that annnddd... nothing happens. But if you try changing the "Hello World!" string
passed to `parseFromStr` you'll get an exception because the parse failed.

Just for completeness, here's the printer for that:
*)

bhello |> printToStr // prints "Hello World!"

(**

Parsing a Name
--------------

Cool, so let's actually do something interesting. Let's make a simple parser to
parse two strings separated by whitespace, possibly with a title beforehand.

We've already used the `blit` boomerang to parse a literal string. We can use
multiple `blit`s separated by the first-successful-parse operator, `<|>` to
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
little more about channels. When parsing, boomerangs write the parsed value into
the given channel, and when printing, boomerangs read the value to print from their
given channel. So what's happening is that the first `blit` boomerang reads the string
"Mr" from its channel and prints it. Then, because it succeeded, the `<|>` operator
does not execute any of the other boomerangs.

What we really want to happen is to have some sort of variable to hold the actual
title that was parsed. Then, when printing, that variable should be read to print
the correct title again. To do this, we'll use the channel-propagating first-successful-
parse operator, `<^>`. That operator behaves the same as `<|>`, except that it exposes
the successful parse on a channel that you pass to the resulting function. You'll note
that simply changing `<|>` to `<^>` in our `btitle1` boomerang yields a compiler error.
Passing the `ConstChannel` (with %"..") to our `blit` partially applied it, consuming
the channel argument. What we really want is to only pass that constant when parsing,
but to print the value propagated by the `<^>` operator. We can do this with the
`<?+^` operator:
*)

let btitle2 =
    (blit <?+^ %"Mr") <^>
    (blit <?+^ %"Ms") <^>
    (blit <?+^ %"Dr")

(**
You'll note that the type of `btitle2` is now `IChannel<string> -> Context -> Context`
while the type of `btitle1` boomerang was just `Context -> Context`. This is because
we now have a channel that will expose the successfully parsed string.

We want our title to be optional, we'll define a `string option` mutable binding
to hold the value, and a new boomerang, `btitle`, that will optionally parse a title
into that binding:
*)

let mutable title = None
let btitle = ~~~btitle2 ^-> <@ title @>

(**
The `~~~` prefix operator takes a boomerang exposing an `IChannel<'t>` and makes it optional,
exposing an `IChannel<'t option>`. Then, the result of that is piped, via the `^->` operator
into a mutable binding. Remember, everything in SharpBoomerang is bidirectional, which is
why a quoted expression, `<@ title @>` is used to indicate the binding-- when parsing, the
result will be written there, and when printing, that value will be read.

Let's give it a try and see if it works:
*)

btitle |> parseFromStr "Dr"
title // is now "Dr"

btitle |> printToStr // now prints "Dr"

title <- Some "Mr"
btitle |> printToStr // now prints "Mr"

btitle |> parseFromStr "Bs"
title // is now None

(**
Our title will be followed by an optional dot (.) and then some whitespace,
so let's define a couple helper boomerangs for those:
*)

let boptdot = ~~(blit %".") %true
let bmanyws = +.(bws %" ")

(**
....
*)

let bdotSpaceIfTitle = ~~(boptdot >> bmanyws) %%(fun r -> r title.IsSome)

(**
Let's break that down from the inside out: The `bws` boomerang matches any single character
of whitespace (' ', '\t', '\r', '\n'). In our case, we don't care what character was actually
parsed and we always want to print a single space, so we just pass a `ConstChannel` with that.
The `+.` prefix operator indicates that the boomerang can be greedily matched one or more times,
though when printing, it will only print once. So `(blit %"." >> +.(bws %" "))` composes a boomerang
that matches a "." followed by one or more whitespace characters. Got it?

Next, the `~~` prefix operator indicates that the boomerang is optional to parse, and exposes an
`IChannel<bool> to determine if it is printed-- this value is supplied by a function that returns true
if our `title` binding has a value.

Okay, so let's put this all together into a full parser for our definition of a name:
*)

let mutable firstName = ""
let mutable lastName = ""

let bname = btitle >> bdotSpaceIfTitle >> (bstr ^-> <@ firstName @>) >> bmanyws >> (bstr ^-> <@ lastName @>)

bname |> parseFromStr "Mr. Miguel de Icaza"

bname |> printToStr

(**
Play with the name passed in to `parseFromStr`, and you'll see that this pretty much works:
the different components are parsed out into the different mutable bindings and then those
values are also used to pretty print.

Now having a bunch of mutable variables is less than ideal. It'd be nice if we could use
some immutable data structures like this:
*)

type Title =
    | None
    | Mr
    | Ms
    | Dr

type Name = {
    //Title : Title;
    First : string;
    Last  : string;
    }

let bname2 = boomerang {
    //let! title = btitle2
    let! first = bstr

    return first
}

bname2 %"hello" |> printToStr