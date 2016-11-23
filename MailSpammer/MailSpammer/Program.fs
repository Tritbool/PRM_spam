// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open FSCL.Compiler
open FSCL.Language
open FSCL.Runtime
open Fake.FileHelper

(* types *)
type dico = string list
type mail = string
type vector = int list


(* file management *)
let readFile path split =
    let text = File.ReadAllText(path).Replace(',',' ').Replace('.',' ').Replace(':',' ').Replace(';',' ').Replace('!',' ').Replace('?',' ').Replace('(',' ').Replace(')',' ').Replace('{',' ').Replace('}',' ').Replace('[',' ').Replace(']',' ').Replace('=',' ').Replace('-',' ').Replace('*',' ').Replace('+',' ').Replace('\\',' ').Replace('_',' ').Replace('<',' ').Replace('>',' ').Replace('"',' ').Replace('@',' ')
    if split then
        let t = text.Split(' ') 
        t
    else 
       [|text|]

//Read all files    
let readTheFiles path =
    let files = Directory.GetFiles(path)
    files(* |> Array.map(fun(f) -> f, readFile f) *)

let rec write (sw:StreamWriter)(l:string list) =
    match l with
        []->sw.WriteLine(" ")
        |hd::tl->sw.Write(hd); write sw tl

let writeToFile (l:string list) =
    use sw = new StreamWriter("test.res",true) in
        write sw l

let writeCSV (l:string list)=
    use sw = new StreamWriter("offers.csv",true) in
    write sw l


(* miscellaneous *)
let arrayToList (a: 'e array) =
    Array.toList a


let splitText (s:string) =
    let text = s.Replace(',',' ').Replace('.',' ').Replace(':',' ').Replace(';',' ').Replace('!',' ').Replace('?',' ').Replace('(',' ').Replace(')',' ').Replace('{',' ').Replace('}',' ').Replace('[',' ').Replace(']',' ').Replace('=',' ').Replace('-',' ').Replace('*',' ').Replace('+',' ').Replace('\\',' ').Replace('_',' ').Replace('<',' ').Replace('>',' ').Replace('"',' ').Replace('@',' ')
    text.Split(' ')


let rec printList l =
    match l with 
        []->printf "\n"
        |hd::tl-> printf "%s\n" hd; printList tl;;

(*CALCULS*)
let square x = x*x

//Calcul scalaire

[<ReflectedDefinition;Kernel>]
let scalaireVect(a:int32[], b:int32[], c:int32[], wi:WorkItemInfo) =
    let gid = wi.GlobalID(0)
    c.[gid] <- a.[gid] * b.[gid]

    c
  
let rec somme (v:vector) =
    match v with
    []->0
    |hd::tl-> hd+(somme tl)

let scalaire (v1:vector)(v2:vector) = 
    let ws = WorkSize(int64 v1.Length)
    let vs = Array.zeroCreate v1.Length
    let a = List.toArray v1
    let b = List.toArray v2
    <@ scalaireVect(a,b,vs, ws) @>.Run() |> ignore
    somme (arrayToList vs)

let scalaireVectShape (v1:vector)(v2:vector) = 
    let ws = WorkSize(int64 v1.Length)
    let vs:int array = Array.zeroCreate v1.Length
    let a = List.toArray v1
    let b = List.toArray v2
    <@ scalaireVect(a,b,vs, ws) @>.Run() |> ignore
    arrayToList vs

//Calcul norme

let norme (v:vector) =
    sqrt(float(somme (scalaireVectShape v v)))

//Calcul vecteur ecart
let angle (v1:vector) (v2:vector)=
    //calcul norme V1
    let nV1 = norme v1
    
    //calcul norme V2
    let nV2 = norme v2

    //calcul norme v1*v2 (scalaire)
    let nScal = nV1*nV2

    //calcul scalaire V1 V2
    let scal = float (scalaire v1 v2) 

    //calcul de l'ecart
    let ecart = scal/nScal

    ecart


(* Parsing d'occurences*)

let rec occ (l:string list)(w:string) =
    match l with
    []->0
    |hd::tl-> if(hd.ToLower().Contains (w.ToLower()))then
                 1+(occ tl w)
              else
                 (occ tl w)


let occurences (m:mail)(w:string) = 
    let l = arrayToList(splitText m)
    (occ l w)

(* Utilitaires Dico *)
let createDico path =
    let d:dico = arrayToList(readFile path true)
    d


(* Vectorisation *)
let createVector (m:mail) (d:dico) =
    let presence = Array.zeroCreate d.Length
    for i in 0..d.Length-1 do
        presence.[i]<- (occurences m d.[i])
    presence

(* Traitement des mails *)

//On parse le mail pour ne récupérer que le nécessaire, à savoir le contenu qui se trouve entre les balises de content text/plain et text/html.
let sortMail (d:dico)(path:string)(vo:vector) =
    let f = readFile path false
    let subject=[|"Subject"|]
    let ss = [|"text/plain"|]
    let m = f.[0].Split(subject, StringSplitOptions.RemoveEmptyEntries)
    let ss = [|"text/html"|]
    let m = m.[m.Length-1].Split(ss,StringSplitOptions.RemoveEmptyEntries)
    let m = m.[0]
    let v = createVector m d
    let cosT = angle (Array.toList v ) vo
    let cosTs = cosT.ToString()
    if(cosT > 0.70)then
        writeToFile (path::":"::[cosTs])
        path
    else
        ""

let sortMails (d:dico)(vo:vector)= 
    let mails = readTheFiles "EML"
    let mutable ok =[] 
    for i=0 to mails.Length-1 do
        let res = sortMail d mails.[i] vo
        if not (res.Equals("")) then
            ok <- res::ok
    ok

let sortOffers (d:dico) (path:string) =
    let f = readFile path false
    let subject=[|"Subject"|]
    let ss = [|"text/plain"|]
    let m = f.[0].Split(subject, StringSplitOptions.RemoveEmptyEntries)
    let ss = [|"text/html"|]
    let m = m.[m.Length-1].Split(ss,StringSplitOptions.RemoveEmptyEntries)
    let m = m.[0]
    let v = createVector m d
    let l:string[] = Array.create v.Length ""
    for i=0 to v.Length-1 do
        if(i <> v.Length-1)then
            l.[i] <- String.Concat((string v.[i]),", ")
        else
            l.[i] <- (string v.[i])
  
    writeCSV (arrayToList l)


let createModelVector (d:dico) = 
    let mails = readTheFiles "EML_offers"
    for i=0 to mails.Length-1 do
        sortOffers d mails.[i]

let loadRefVector =
    let voi = readFile "opti.csv" true
    let voii =voi |> Array.map(fun x -> int x)
    Array.toList voii
         

// Methode unique d'initialisation du parsing
let initialize (dicoPath:string) =
    let d = createDico dicoPath
    //createModelVector d
    let vo = loadRefVector
    let ok = sortMails d vo
    Copy "EML_ok" ok




[<EntryPoint>]
let main argv = 
    initialize "dico.txt";  //Console.ReadKey true;  
    0