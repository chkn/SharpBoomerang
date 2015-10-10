(**
Channels and Isomorphisms
=========================

**isomorphism** _n._
Some crazy Greek mathematical thing. See the [Wikipedia page][0] for the gory details.

TL;DR it's a function, `'a -> 'b`, and its inverse, `'b -> 'a`

[0]: https://en.wikipedia.org/wiki/Isomorphism

Say what?
---------

In SharpBoomerang, isomorphisms are represented by this type:

    type Iso<'a,'b> = ('a -> 'b) * ('b -> 'a)

That type and other functions for working with it is defined in iso.fs:
*)
#load @"../src/iso.fs"

(**
Channels are defined in channel.fs. We'll talk more about those later.
*)
#load @"../src/channel.fs"

(**
We'll also want to open a couple namespaces:
*)
open System
open SharpBoomerang

(**
Let's take numerical addition, whose inverse is simply subtraction. 
Here's an isomorphism representing adding one to a number (and subtracting it):
*)

let add1 = (fun n -> n + 1), (fun n -> n - 1)

(**
See, that's really simple, right? Well sometimes SharpBoomerang can make it even simpler
by inferring the inverse function. Thus, we can simplify the definition further and only
define the `'a -> 'b` function:
*)

let add1_2 = Iso.ofFn (fun n -> n + 1)


(**
Channels
--------

By themselves, isos aren't that useful, but they become very powerful when you
combine them with channels. Here's a more complex example to whet our appetite:
*)

let raw = Channel.pipeWith "abc" // create a channel that initially contains "abc"
let map = raw |> Channel.map(Iso.ofFn (fun str -> str.ToUpperInvariant()))

raw.Read(printfn "%A") // prints "abc"
map.Read(printfn "%A") // prints "ABC"

map.Write("DEF")

raw.Read(printfn "%A") // prints "def"
map.Read(printfn "%A") // prints "DEF"

(**
Channels and isos are the powerful combination SharpBoomerang uses to achieve its
bidirectionality.
*)
