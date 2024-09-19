#load "parsec.fs"
open Parsec
open System

// --------------------------------------------------------------------------------------
// Bibtex parser
// --------------------------------------------------------------------------------------

let stringWithout nc = 
  zeroOrMore (pred (fun c -> c <> nc)) |> map (fun c -> new System.String(Array.ofSeq c))

let qvalue = char '"' <*>> stringWithout '"' <<*> char '"' 
let bvalue = char '{' <*>> stringWithNestedBrackets <<*> char '}' 
let bbvalue = char '{' <*>> char '{' <*>> stringWithNestedBrackets <<*> char '}' <<*> char '}' 

let stringOrUrlOrNumber = 
  oneOrMore (letter <|> number <|> char '_' <|> char ':' <|> char '/' <|> char '-') 
  |> map (fun ch -> new System.String(Array.ofList ch))

let value = bbvalue <|> bvalue <|> qvalue <|> stringOrUrlOrNumber

let ident = 
  oneOrMore (letter <|> char '_' <|> char '-') 
  |> map (fun ch -> new System.String(Array.ofList ch))

let prop = 
  anySpace <*>> ident <<*> anySpace <<*> char '=' <<*> 
    anySpace <*> value <<*> anySpace <<*> zeroOrMore (char ',')

let entry = 
  char '@' <*>> string <<*> char '{' 
    <<*> stringWithout ',' <<*> char ',' <*> oneOrMore prop 
      <<*> anySpace <<*> char '}' <<*> anySpace

let anyString = zeroOrMore any |> map (fun ch -> new System.String(Array.ofList ch))
let p = zeroOrMore entry <*> anyString

// --------------------------------------------------------------------------------------
// Formatting and preprocessing
// --------------------------------------------------------------------------------------

let special = 
  [ "Acm", "ACM"
    "Peerj", "PeerJ"
    "Ieee", "IEEE"
    "On", "on"
    "Of", "of"
    "-pacmpl", " (PACMPL)"
    "Sigplan", "SIGPLAN"
    "Emsoft", "EMSOFT"
    "Ecoop", "ECOOP"
    "The", "the"]

let reorderAuthors (authors:string) = 
  [ for a in authors.Replace('\r', ' ').Replace('\n',' ') .Split(" and ") do
      let parts = a.Split([| ',' |], StringSplitOptions.TrimEntries)
      (String.concat " " parts.[1..]) + " " + parts.[0] ] |> String.concat ", "

let normalizeJournal (journal:string) = 
  let journal, acc = 
    if journal.EndsWith(")") && journal.IndexOf(" (") > 0 then 
      let i = journal.IndexOf(" (")
      journal.Substring(0, i), journal.Substring(i)
    else journal, ""
  let parts = journal.Replace("\\&", "&").Split([| ' ' |], StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
  let res = [ for p in parts -> p.[0].ToString().ToUpper() + p.Substring(1).ToLower() ] |> String.concat " "
  let njournal = special |> Seq.fold (fun (s:string) (w,r) -> s.Replace(w, r)) res
  njournal + acc

// --------------------------------------------------------------------------------------
// Reading and formatting
// --------------------------------------------------------------------------------------

type Publication = 
  { title : string; venue : string; year : int; authors : string; doi : string; isbn : string }

let readPublication (typ:string, props:list<string*string>) = 
  let props = props |> List.map (fun (k, v) -> k.ToLower(), v) |> Map.ofSeq
  let typ = typ.ToLower()
  { title = props.["title"].Replace('\r', ' ').Replace('\n', ' ').Replace("  ", " ")
    authors = props.["author"] |> reorderAuthors
    year = props.["year"] |> int
    doi = defaultArg (props.TryFind "doi") ""
    isbn = if props.ContainsKey("doi") then "" else (defaultArg (props.TryFind "isbn") "")
    venue = 
      [ if typ = "article" then 
          yield props.["journal"] |> normalizeJournal
        elif typ = "inproceedings" then
          yield props.["booktitle"] |> normalizeJournal 
        else 
          failwith $"typ = {typ}" 
        match props.TryFind "volume" with 
        | Some vol -> yield $"vol. {vol}"
        | _ -> ()
        match props.TryFind "article-number", props.TryFind "number" with 
        | Some num, _ | _, Some num -> yield $"no. {num}"
        | _ -> ()
        match props.TryFind "pages" with 
        | Some pp -> yield $"pp. {pp}"
        | _ -> () ] |> String.concat ", " }

let writePublication pub = 
  [ yield $"* {pub.title}"
    yield $"  - authors: {pub.authors}"
    yield $"  - year: {pub.year}"
    if pub.doi <> "" then yield $"  - doi: {pub.doi}"
    if pub.isbn <> "" then yield $"  - isbn: {pub.isbn}"
    yield $"  - venue: {pub.venue}" 
    yield "" ]

// --------------------------------------------------------------------------------------
// Do it!
// --------------------------------------------------------------------------------------

open System.IO

let (@) p1 p2 = Path.Combine(p1, p2)
let source = __SOURCE_DIRECTORY__ @ ".." @ "data" @ "citations"

let run () = 
  for f in Directory.GetFiles(source, "*.bib") do
    let bibtex = File.ReadAllText(f).Trim()
    let parsed = run p bibtex |> Option.get |> fst
    let read = List.map readPublication parsed
    let lines = read |> List.collect writePublication
    let linesAll = "# [Citations](#citations)" :: "" :: lines
    let outf = Path.ChangeExtension(f, "md")
    File.WriteAllLines(outf, linesAll)
