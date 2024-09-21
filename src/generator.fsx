#load "../.paket/load/netstandard2.1/Fluid.Core.fsx"
#r "../packages/FSharp.Formatting/lib/netstandard2.1/FSharp.Formatting.Common.dll"
#r "../packages/FSharp.Formatting/lib/netstandard2.1/FSharp.Formatting.Markdown.dll"
#r "../packages/Microsoft.Extensions.FileProviders.Physical/lib/netstandard2.0/Microsoft.Extensions.FileProviders.Physical.dll"
open System
open System.IO
open FSharp.Formatting.Markdown
open Fluid
open Microsoft.FSharp.Reflection
open Microsoft.Extensions.FileProviders

let (@) p1 p2 = Path.Combine(p1, p2)
let data = __SOURCE_DIRECTORY__ @ ".." @ "data"

// --------------------------------------------------------------------------------------
// Data
// --------------------------------------------------------------------------------------

type OptionInt = 
  { hasvalue : bool; value : int }

type Output = 
  { output : string }

type Employment = 
  { role : string; institution : string; start : int; 
    range : string; label : string; kind : string }

type Funding = 
  { grant : string; institution : string; start : int; 
    range : string; label : string; role : string; outputs : seq<Output> }

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

type Year = 
  { year : int; semester : string }
type Teaching = 
  { course : string; institution : string; year : int; 
    range : string; label : string; years : seq<Year> }

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
    label : string; role : string; outputs : seq<Output> }

type Talk = 
  { title : string; venue : string; year : int; link : string; }

type Publication =
  { title : string; venue : string; year : int; authors : string; ``type`` : string; ``if`` : string
    hdl : string; arxiv : string; url : string; doi: string; isbn: string; comment: string; impact: string
    citations : seq<Publication>; wos : OptionInt; gscholar : OptionInt; scopus : OptionInt }

type PC = { venue : string; year : int; highlight : string }
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
    publications : Publication list }

type Content = 
  { content : string; title : string }

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
          [ yield defProp, [formatSpans b]
            for kvp in kvps do
              match kvp with 
              | [ Span([Literal(ColSplit(k, ""), _)], _); ListBlock(_, pars, _) ] -> 
                  let vals = pars |> List.map (function
                      | [Span(lits, _) | Paragraph(lits, _)] -> formatSpans lits
                      | v -> failwith $"parseItems: Unexpected value: {v}")
                  yield k, vals
              | [ Span(Literal(ColSplit(k, lit), _)::lits, _) ] 
              | [ Paragraph(Literal(ColSplit(k, lit), _)::lits, _) ] -> 
                  yield k, [formatSpans (Literal(lit, None)::lits)]
              | _ -> failwith $"parseItems: Unexpected item: {kvp}" ] |> dict
      | _ -> failwith $"parseItems: Unexpected structure: {item}" ]

let readYears (s:string) = 
  [ for y in s.Split([| ',' |], StringSplitOptions.TrimEntries) ->
      let (|AcadYear|) (s:string) =
        match s.Split('/') with [| y1; y2 |] -> int y1 | _ -> failwith $"Wrong academic year: {s}"
      match y.Split(' ') with 
      | [| _; AcadYear year |] 
      | [| AcadYear year |] -> { year = year; semester = y }
      | _ -> failwith $"Wrong semester: {y}" ]

let rec makeRecord<'T> () = 
  let convert (ss:string list) ty =
    if ty = typeof<seq<Output>> then
      box [ for s in ss -> { output = s } ] 
    else
      let s = List.exactlyOne ss
      if ty = typeof<string> then box s
      elif ty = typeof<OptionInt> then 
        if s = "" then box { hasvalue = false; value = 0 } 
        else box { hasvalue = true; value = Int32.Parse s }
      elif ty = typeof<int> then box (Int32.Parse s)
      elif ty = typeof<seq<Year>> then 
        if s = "" then box List.empty<Year> else box (readYears s)
      elif ty = typeof<seq<Publication>> then 
        if s = "" then box List.empty<Publication> else box (readCitations s)
      else failwith $"convert: Cannot convert to {ty.Name}"
  let flds = FSharpType.GetRecordFields(typeof<'T>)
  fun (rcd:System.Collections.Generic.IDictionary<_, _>) ->
    let get fn = if rcd.ContainsKey(fn) then rcd.[fn] else [""]
    let vals = [| for f in flds -> convert (get f.Name) f.PropertyType |]
    unbox<'T>(FSharpValue.MakeRecord(typeof<'T>, vals))

and readRecords<'T> defProp sec pars = 
  let items = getSection sec pars
  let data = parseItems defProp items
  let maker = makeRecord<'T>()
  [ for rcd in data -> maker rcd ]

and readCitations (key:string) =
  let pars = Markdown.Parse(File.ReadAllText(data @ "citations" @ key + ".md")).Paragraphs
  readRecords<Publication> "title" "#citations" pars

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
      | kvps when kvps.Length <= ps.Length -> 
          let kvps = Array.append kvps (Array.create (ps.Length - kvps.Length) "") |> Array.map (fun v -> [v])
          Seq.zip ps kvps |> dict |> maker
      | _ -> failwith $"readKvpList: Cannot split item: {s}. Expected {ps.Length} items." ]

// --------------------------------------------------------------------------------------
// Processing
// --------------------------------------------------------------------------------------

let parser = FluidParser()
let options = TemplateOptions()
options.FileProvider <- new PhysicalFileProvider(__SOURCE_DIRECTORY__ @ ".." @ "templates");

let rec allTypes seen (ty:System.Type) = seq {
  if Set.contains ty.FullName seen then () else
  let seen = Set.add ty.FullName seen
  if FSharpType.IsRecord(ty) then 
    yield ty
    for fld in FSharpType.GetRecordFields(ty) do
      yield! allTypes seen fld.PropertyType
  if (ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<seq<_>>) ||
      (ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<list<_>>) then
    yield! allTypes seen (ty.GetGenericArguments().[0]) }

for t in allTypes Set.empty typeof<Data> do
  options.MemberAccessStrategy.Register(t)
options.MemberAccessStrategy.Register(typeof<Content>)


let processFile f outf model =
  let src = File.ReadAllText(f)
  let templ = parser.Parse(src) 
  let ctx = new TemplateContext(model, options)
  let out = templ.Render(ctx)
  File.WriteAllText(outf, out)


let doit () =
  cprint ConsoleColor.DarkGray "Updating documents"
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
          pc = readInlineRecords<PC> ["venue"; "year"; "highlight"] "#pc" pars }
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
      publications = readRecords<Publication> "title" "#pubs" pars
    }

  let layout = __SOURCE_DIRECTORY__ @ ".." @ "layouts" @ "index.html"
  let sources = __SOURCE_DIRECTORY__ @ ".." @ "sources"
  let rec collectDirs dir = seq {
    yield dir
    for sub in Directory.GetDirectories(dir) do yield! collectDirs sub }
  let dirs = collectDirs sources

  for sourceDir in dirs do
    Directory.CreateDirectory(sourceDir.Replace("sources", "docs")) |> ignore
    for f in Directory.GetFiles(sourceDir, "*.md") do  
      let outf = f.Replace("sources", "docs").Replace(".md", ".html")
      let src = File.ReadAllText(f)
      let title = Markdown.Parse(src).Paragraphs |> List.pick (function Heading(1, spans, _) -> Some(formatSpans spans) | _ -> None)
      let html = Markdown.ToHtml(src) 
      processFile layout outf { content = html; title = title }
      cprint ConsoleColor.DarkGray "Processed: %s" outf

    for f in Directory.GetFiles(sourceDir, "*.html") do  
      let outf = f.Replace("sources", "docs")
      processFile f outf model
      cprint ConsoleColor.DarkGray "Processed: %s" outf
  cprint ConsoleColor.Green "Update completed"

doit ()

// --------------------------------------------------------------------------------------
// Watcher
// --------------------------------------------------------------------------------------

let watch dir op = 
  let rec update () =
    try
      op()
    with 
    | e when e.Message.Contains("process cannot access the file") ->
      System.Threading.Thread.Sleep(100)
      cprint ConsoleColor.DarkYellow "Cannot access a file, retrying..."
      update()
    | e -> 
      cprint ConsoleColor.Red "Error occurred: %s" e.Message

  let fsw = new FileSystemWatcher(dir)
  fsw.IncludeSubdirectories <- true
  fsw.NotifyFilter <- NotifyFilters.LastWrite
  fsw.Changed.Add(fun e -> update())
  fsw.EnableRaisingEvents <- true
  fsw

let start () =
  let f1 = watch (__SOURCE_DIRECTORY__ @ ".." @ "templates") doit
  let f2 = watch (__SOURCE_DIRECTORY__ @ ".." @ "sources") doit
  let f3 = watch (__SOURCE_DIRECTORY__ @ ".." @ "data") doit

  cprint ConsoleColor.Green "Waiting for edits, press any key to stop"
  Console.ReadKey() |> ignore