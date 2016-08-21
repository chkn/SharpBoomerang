#!/usr/bin/env fsharpi --exec

open System
open System.IO
open System.Diagnostics
open System.ComponentModel
let inline (/) p1 p2 = Path.Combine(p1, p2)
let inline ``./`` p  = __SOURCE_DIRECTORY__ / p

/// The version of the nuget package (bump this on each release)
let Version = "0.0.1-pre1"

// Paths of interest:

/// The directory containing all the docs fsx files
let docs = ``./`` "docs"

/// The output directory into which the docs html files will be generated
let output = ``./`` "docs-html"

/// The path to the template used in the docs html generation
let template = ``./`` "docs" / "tools" / "template.html"

/// The path to the nuspec for creating the nuget package
let nuspec = ``./`` "SharpBoomerang.nuspec"

/// The path in which nuget packages will be installed
let packages = ``./`` "packages"

/// Project info for the docs
let projInfo =
  [
    "page-description", "SharpBoomerang documentation"
    "page-author", "Alex Corrado"
    "github-link", "https://github.com/chkn/SharpBoomerang"
    "nuget-link", "https://www.nuget.org/packages/SharpBoomerang"
    "project-name", "SharpBoomerang"
  ]

let quote (s : string) = "\"" + s.Replace("\"", "\\\"") + "\""
let run prog args =
    try
        let proc = ProcessStartInfo(prog, String.Join (" ", Array.map quote args), UseShellExecute = false)
                   |> Process.Start
        proc.WaitForExit()
        if proc.ExitCode <> 0 then
            failwith "Failed."
    with :? Win32Exception ->
        printfn "Failed to run %s. Check that you are in a Visual Studio command prompt." prog
        reraise()

// Tools
let git = run "git"
let nuget = run "nuget"
let msbuild = run "msbuild"

// Setup: Ensure F# formatting is available
let FSharpFormattingPath = packages / "FSharp.Formatting"
if not(Directory.Exists(FSharpFormattingPath)) then
    nuget [| "install"; "FSharp.Formatting"; "-ExcludeVersion"; "-OutputDirectory"; packages |]
//FIXME: hardcoded path
#load  @"packages/FSharp.Formatting/FSharp.Formatting.fsx"

// Tasks
let makeBuild() =
    msbuild [| "/p:Configuration=Release" |]

let makeDocs()  =
    let fsi = FSharp.Literate.FsiEvaluator()
    //fsi.EvaluationFailed.Add(fun e -> printf "%O" e)
    FSharp.Literate.Literate.ProcessDirectory(docs, template, output, replacements = projInfo, fsiEvaluator = fsi)
    FSharp.Literate.Literate.ProcessMarkdown(``./`` "Readme.md", template, output / "index.html", replacements = projInfo)

    // Manually copy some files needed by the template
    output / "content" |> Directory.CreateDirectory |> ignore
    let copyContent src = File.Copy(src, output / "content" / Path.GetFileName(src), true)
    FSharpFormattingPath / "styles" / "style.css" |> copyContent
    FSharpFormattingPath / "styles" / "tips.js"   |> copyContent

let makePages() =
    makeDocs()
    git [| "checkout"; "gh-pages" |]
    let rec loop path =
        for f in Directory.EnumerateFileSystemEntries(path) do
            match f with
            | "." | ".." | "" -> ()
            | _ ->
                let p = f.Replace(Path.GetFileName(output), "")
                if Directory.Exists(f) then
                    if not(Directory.Exists(p)) then Directory.CreateDirectory(p) |> ignore
                    loop f
                else
                    printfn "Copying: %s to %s" f p
                    File.Copy(f, p, true)
                    git [| "add"; p |]
    loop output
    git [| "status" |]
    printfn "If this looks correct, type `git commit` and then `git push origin gh-pages` to push live."


let makeNupkg() =
    if not(Directory.Exists(``./`` "bin" / "Release")) then makeBuild()
    nuget [| "pack"; nuspec; "-Properties"; "version=" + Version |]

let makeClean() =
    let rmrf p = try Directory.Delete(p, true) with :? DirectoryNotFoundException -> ()
    rmrf (``./`` "bin")
    rmrf (``./`` "obj")
    rmrf (``./`` "docs-html")
    Directory.EnumerateFiles(__SOURCE_DIRECTORY__, "*.nupkg")
    |> Seq.iter (fun file -> File.Delete(file))

match fsi.CommandLineArgs with
| [| _; "clean" |] -> makeClean()
| [| _; "build" |] -> makeBuild()
| [| _; "docs"  |] -> makeDocs()
| [| _; "pages" |] -> makePages()
| [| _; "nupkg" |] -> makeNupkg()
| [| _; "all"   |]
| [| _ |] -> makeDocs(); makeNupkg()
| _ -> printf "Invalid options.\n"
