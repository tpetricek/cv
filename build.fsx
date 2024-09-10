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

type Service = 
  { role : string; venue : string; start : int; range : string }

type Admin =
  { role : string; institution : string; start : int; 
    range : string; label : string }

type Supervision =
  { person : string; role : string; institution : string; 
    start : int; range : string; label : string }

type Teaching = 
  { course : string; institution : string; year : int; 
    range : string; label : string }

type Advising = 
  { title : string; student : string; institution : string; 
    year : string; kind : string }

type Visit = 
  { institution : string; date : string; year : int; label : string  }

type Invited = 
  { venue : string; date : string; year : int; title : string 
    kind : string; location : string }

type Membership = 
  { organization : string; range : string; start : int; label : string  }

type Project = 
  { title : string; range : string; start : int; institution : string; 
    label : string; role : string  }

type Talk = 
  { title : string; venue : string; year : int; link : string; }

type PC = { venue : string; year : int }
type PhD = { institution : string; year : int }
type Grant = { name : string; country : string }

type Reviewing = 
  { pc : PC list 
    phd : PhD list
    reviewer : string list
    grant : Grant list }

type Data = 
  { experience : Employment list 
    funding : Funding list 
    awards : Award list 
    reviewing : Reviewing
    service : Service list
    admin : Admin list
    supervision : Supervision list
    teaching : Teaching list
    advising : Advising list
    visits : Visit list
    invited : Invited list
    projects : Project list
    memberships : Membership list
    talks : Talk list
    }

// --------------------------------------------------------------------------------------
// Logging
// --------------------------------------------------------------------------------------

let cprint clr fmt =
  Printf.kprintf (fun s ->
    let old = Console.ForegroundColor
    Console.ForegroundColor <- clr
    Console.WriteLine($"[{DateTime.Now.ToLongTimeString()}] {s}")
    Console.ForegroundColor <- old) fmt

// --------------------------------------------------------------------------------------
// Parsing
// --------------------------------------------------------------------------------------

let formatSpans spans = Markdown.ToHtml(MarkdownDocument([Span(spans, None)], dict [])).Trim()

let rec getSection sec pars = 
  match pars with 
  | Heading(_, [ DirectLink(_, s, _, _) ], _)::ListBlock(_, lis, _)::_ when s = sec -> lis
  | p::pars -> getSection sec pars
  | [] -> failwith $"getSection: Section {sec} not found"

let (|ColSplit|_|) (s:string) = 
  let i = s.IndexOf(':')
  if i > 0 then Some(s.Substring(0, i).Trim(), s.Substring(i+1).Trim())
  else None

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

let makeRecord<'T> () = 
  let convert (s:string) ty =
    if ty = typeof<string> then box s
    elif ty = typeof<int> then box (Int32.Parse s)
    else failwith $"convert: Cannot convert to {ty.Name}"
  let flds = FSharpType.GetRecordFields(typeof<'T>)
  fun (rcd:System.Collections.Generic.IDictionary<_, _>) ->
    let get fn = if rcd.ContainsKey(fn) then rcd.[fn] else ""
    let vals = [| for f in flds -> convert (get f.Name) f.PropertyType |]
    unbox<'T>(FSharpValue.MakeRecord(typeof<'T>, vals))

let readRecords<'T> defProp sec pars = 
  let items = getSection sec pars
  let data = parseItems defProp items
  let maker = makeRecord<'T>()
  [ for rcd in data -> maker rcd ]

let readSimpleList sec pars =
  let items = getSection sec pars
  [ for item in items ->
      match item with 
      | [Span(spans, _)] -> formatSpans spans
      | _ -> failwith $"readSimpleList: Unexpected item: {item}" ]

let readInlineRecords<'T> (ps:string list) sec pars = 
  let items = readSimpleList sec pars
  let maker = makeRecord<'T>()
  [ for s in items ->
      match s.Split([|','|], StringSplitOptions.TrimEntries) with 
      | kvps when kvps.Length = ps.Length -> Seq.zip ps kvps |> dict |> maker
      | _ -> failwith $"readKvpList: Cannot split item: {s}. Expected {ps.Length} items." ]

// --------------------------------------------------------------------------------------
// Processing
// --------------------------------------------------------------------------------------

let (@) p1 p2 = Path.Combine(p1, p2)
let data = __SOURCE_DIRECTORY__ @ "data"

let doit () =
  let pars = 
    [ for f in Directory.GetFiles(data) do
        yield! Markdown.Parse(File.ReadAllText(f)).Paragraphs ]
  let model = 
    { experience = readRecords<Employment> "role" "#experience" pars
      funding = readRecords<Funding> "grant" "#funding" pars
      awards = readRecords<Award> "award" "#awards" pars
      reviewing = 
        { reviewer = readSimpleList "#reviewer" pars
          phd = readInlineRecords<PhD> ["institution"; "year"] "#phd" pars
          grant = readInlineRecords<Grant> ["name"; "country"] "#grant" pars
          pc = readInlineRecords<PC> ["venue"; "year"] "#pc" pars }
      service = readRecords<Service> "role" "#service" pars
      admin = readRecords<Admin> "role" "#admin" pars
      supervision = readRecords<Supervision> "person" "#supervision" pars
      teaching = readRecords<Teaching> "course" "#teaching" pars
      advising = readRecords<Advising> "title" "#advising" pars
      visits = readRecords<Visit> "institution" "#visits" pars
      invited = readRecords<Invited> "venue" "#invited" pars
      projects = readRecords<Project> "title" "#projects" pars
      memberships = readRecords<Membership> "organization" "#memberships" pars
      talks = readRecords<Talk> "title" "#talks" pars
    }
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
  Directory.CreateDirectory(__SOURCE_DIRECTORY__ @ "docs") |> ignore
  for f in Directory.GetFiles(__SOURCE_DIRECTORY__ @ "sources") do  
    let src = File.ReadAllText(f)
    let templ = parser.Parse(src) 
    let ctx = new TemplateContext(model, options)
    let out = templ.Render(ctx)
    File.WriteAllText(__SOURCE_DIRECTORY__ @ "docs" @ Path.GetFileName(f), out)

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
  fsw

let f1 = watch (__SOURCE_DIRECTORY__ @ "sources") doit
let f2 = watch (__SOURCE_DIRECTORY__ @ "data") doit