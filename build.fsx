#load ".paket/load/netstandard2.1/Fluid.Core.fsx"
#r "packages/FSharp.Formatting/lib/netstandard2.1/FSharp.Formatting.Common.dll"
#r "packages/FSharp.Formatting/lib/netstandard2.1/FSharp.Formatting.Markdown.dll"
open System
open System.IO
open FSharp.Formatting.Markdown
open Fluid
open Microsoft.FSharp.Reflection

// --------------------------------------------------------------------------------------
// Data
// --------------------------------------------------------------------------------------

type Employment = 
  { role : string; institution : string; start : int; 
    range : string; label : string; kind : string }
type Funding = 
  { grant : string; institution : string; start : int; 
    range : string; label : string }
type Award = 
  { award : string; venue : string; year : int; label : string }
type Data = 
  { experience : Employment list 
    funding : Funding list 
    awards : Award list }

// --------------------------------------------------------------------------------------
// Logging
// --------------------------------------------------------------------------------------

let cprint clr fmt =
  Printf.kprintf (fun s ->
    let old = Console.ForegroundColor
    Console.ForegroundColor <- clr
    Console.WriteLine(s)
    Console.ForegroundColor <- old) fmt

// --------------------------------------------------------------------------------------
// Parsing
// --------------------------------------------------------------------------------------

let formatSpans spans = Markdown.ToHtml(MarkdownDocument([Span(spans, None)], dict [])).Trim()

let rec getSection sec pars = 
  match pars with 
  | Heading(1, [ DirectLink(_, s, _, _) ], _)::ListBlock(_, lis, _)::_ when s = sec -> lis
  | p::pars -> getSection sec pars
  | [] -> failwith $"getSection: Section {sec} not found"

let (|ColSplit|_|) (s:string) = 
  match s.Split([|':'|],StringSplitOptions.TrimEntries) with [| s1; s2 |] -> Some(s1, s2) | _ -> None

let parseItems defProp items = 
  [ for item in items do
      match item with 
      | [Span(b, _); ListBlock(_, kvps, _)] ->
          [ yield defProp, formatSpans b
            for kvp in kvps do
              match kvp with 
              | [ Span(Literal(ColSplit(k, lit), _)::lits, _) ] 
              | [ Paragraph(Literal(ColSplit(k, lit), _)::lits, _) ] -> 
                  yield k, formatSpans (Literal(lit, None)::lits) 
              | _ -> failwith $"parseItems: Unexpected item: {kvp}" ] |> dict
      | _ -> failwith $"parseItems: Unexpected structure: {item}" ]


let readRecords<'T> defProp sec pars = 
  let convert (s:string) ty =
    if ty = typeof<string> then box s
    elif ty = typeof<int> then box (Int32.Parse s)
    else failwith $"convert: Cannot convert to {ty.Name}"
  let items = getSection sec pars
  let data = parseItems defProp items
  let flds = FSharpType.GetRecordFields(typeof<'T>)
  [ for rcd in data ->
      let get fn = if rcd.ContainsKey(fn) then rcd.[fn] else ""
      let vals = [| for f in flds -> convert (get f.Name) f.PropertyType |]
      unbox<'T>(FSharpValue.MakeRecord(typeof<'T>, vals)) ]


// --------------------------------------------------------------------------------------
// Processing
// --------------------------------------------------------------------------------------

let (@) p1 p2 = Path.Combine(p1, p2)
let all = __SOURCE_DIRECTORY__ @ "data/all.md"

let doit () =

  let doc = Markdown.Parse(File.ReadAllText(all))
  let model = 
    { experience = readRecords<Employment> "role" "#experience" doc.Paragraphs
      funding = readRecords<Funding> "grant" "#funding" doc.Paragraphs
      awards = readRecords<Award> "award" "#awards" doc.Paragraphs}
  let options = TemplateOptions()

  let rec allTypes ty = seq {
    if FSharpType.IsRecord(ty) then 
      yield ty
      for fld in FSharpType.GetRecordFields(ty) do
        yield! allTypes fld.PropertyType
    if ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<list<_>> then
      yield! allTypes (ty.GetGenericArguments().[0]) }

  for t in allTypes (model.GetType()) do
    options.MemberAccessStrategy.Register(t)

  let parser = FluidParser()
  Directory.CreateDirectory(__SOURCE_DIRECTORY__ @ "output") |> ignore
  for f in Directory.GetFiles(__SOURCE_DIRECTORY__ @ "sources") do  
    let src = File.ReadAllText(f)
    let templ = parser.Parse(src) 
    let ctx = new TemplateContext(model, options)
    let out = templ.Render(ctx)
    File.WriteAllText(__SOURCE_DIRECTORY__ @ "output" @ Path.GetFileName(f), out)

doit ()

// --------------------------------------------------------------------------------------
// Watcher
// --------------------------------------------------------------------------------------

let watch dir op = 
  let rec update () =
    try
      cprint ConsoleColor.White "[INFO] Updating documents"
      op()
      cprint ConsoleColor.White "[INFO] Update completed"
    with 
    | e when e.Message.Contains("process cannot access the file") ->
      System.Threading.Thread.Sleep(100)
      cprint ConsoleColor.White "[INFO] Retrying..."
      update()
    | e -> 
      cprint ConsoleColor.Red "[ERROR] %s" e.Message

  let fsw = new FileSystemWatcher(dir)
  fsw.NotifyFilter <- NotifyFilters.LastWrite
  fsw.Changed.Add(fun e -> update())
  fsw.EnableRaisingEvents <- true

watch (__SOURCE_DIRECTORY__ @ "sources") doit
watch (__SOURCE_DIRECTORY__ @ "data") doit