(**
The easiest way to follow along is to just open this file in Xamarin Studio or
Visual Studio and use Ctrl+Enter (XS) or Alt+Enter (VS) to execute the following code
snippets in F# interactive.

The first thing we need to do is to load the SharpBoomerang source code:
*)
#load "boomerang.fs"

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
//btitle1 |> parseFromStr "Fu"

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

We can save that into a mutible destination easily:
*)

let mutable title = ""
let btitle = btitle2 ^-> <@ title @>

btitle |> parseFromStr "Dr"
title // is now "Dr"

btitle |> printToStr // now prints "Dr"

title <- "Mr"
btitle |> printToStr // now prints "Mr"

(**
Our title will be followed by an optional dot (.) and then some whitespace,
so let's define a boomerang for that:
*)

let bdotws = ~~(blit %".") >> +.(bws %" ")

(**
The `~~` prefix operator indicates that the boomerang is optional to parse but
should always be printed. If we didn't want the dot when pretty printing, we could
use the `~~~` operator instead, which still makes the boomerang optional to parse
but also will not print it.

The `bws` boomerang matches any single character of whitespace (' ', '\t', '\r', '\n').
In our case, we don't care what character was actually parsed and we always want to print
a single space, so we just pass a `ConstChannel` with that. The `+.` prefix operator indicates
that the boomerang can be greedily matched one or more times, though when printing, it will
only print once.

Okay, so let's put this all together into a full parser for our definition of a name:
*)

let mutable firstName = ""
let mutable lastName = ""

let bname = btitle >> bdotws >> (bstr ^-> <@ firstName @>) >> bws %" " >> (bstr ^-> <@ lastName @>)

bname |> parseFromStr "Dr.   B. Corrado"

bname |> printToStr
