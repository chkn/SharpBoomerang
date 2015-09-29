
#load @"src/iso.fs"
#load @"src/channel.fs"
#load @"src/boomerang.fs"

open SharpBoomerang
open SharpBoomerang.Combinators

let ch = channel {
    return 5
}

ch.Read(printfn "%A")

let ch2 = channel {
    let! bar = ch
    return bar + 1
}

ch2.Read(printfn "%A")
