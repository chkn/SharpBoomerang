#!/usr/bin/env fsharpi --exec

#load @"packages/FSharp.Formatting.2.10.3/FSharp.Formatting.fsx"
open FSharp.Literate
open System.IO

// Paths
let top = __SOURCE_DIRECTORY__
let fsharpFormatting = Path.Combine(top, "packages", "FSharp.Formatting.2.10.3")
let docs = Path.Combine(top, "docs")
let template = Path.Combine(fsharpFormatting, "literate", "templates", "template-project.html")
let output = Path.Combine(top, "docs-html")

let projInfo =
  [
    "page-description", "SharpBoomerang documentation"
    "page-author", "Alex Corrado"
    "github-link", "https://github.com/chkn/SharpBoomerang"
    "project-name", "SharpBoomerang"
  ]

Literate.ProcessDirectory(docs, template, output, replacements = projInfo)

// Manually copy some files needed by the template
Directory.CreateDirectory(Path.Combine(output, "content"))
let copyContent src =
    let dest = Path.Combine(output, "content", Path.GetFileName(src))
    File.Copy(src, dest)

Path.Combine(fsharpFormatting, "styles", "style.css") |> copyContent
Path.Combine(fsharpFormatting, "styles", "tips.js") |> copyContent

