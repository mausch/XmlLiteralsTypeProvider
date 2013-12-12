module Helpers

// helpers mostly copied from FsFormlets

open System
open System.Collections.Generic
open System.Xml
open System.Xml.Linq

let inline (!) s = XName.Get(s)

/// <summary>
/// Matches a <see cref="XElement"/>
/// </summary>
/// <param name="n"></param>
let (|Tag|_|) (n: XNode) = 
    match n with
    | :? XElement as e -> Some e
    | _ -> None

/// <summary>
/// Matches a <see cref="XText"/>
/// </summary>
/// <param name="n"></param>
let (|Text|_|) (n: XNode) = 
    match n with
    | :? XText as e -> Some e
    | _ -> None

let (|EmptyText|_|) (n: XNode) =
    match n with
    | Text t ->
        if String.IsNullOrWhiteSpace t.Value
            then Some t
            else None
    | _ -> None

/// Gets attributes of an element as a tuple list
let getAttr (e: XElement) =
    e.Attributes() 
    |> Seq.map (fun a -> a.Name.LocalName,a.Value)
    |> Seq.toList

/// <summary>
/// Matches a <see cref="XElement"/>, splitting name, attributes and children
/// </summary>
/// <param name="n"></param>
let (|TagA|_|) (n : XNode) =
    match n with
    | Tag t -> 
        let name = t.Name.LocalName
        let attr = getAttr t
        let children = t.Nodes() |> Seq.toList
        Some(name,attr,children)
    | _ -> None

let inline (=.) x y =
    // attributes must be ordered due to a bug in XNode.DeepEquals
    // see http://connect.microsoft.com/VisualStudio/feedback/details/400469/xnode-deepequals-incorrect-result
    let rec orderAttributes =
        function
        | TagA(name, attr, children) -> 
            let attr = Seq.sortBy fst attr |> Seq.map (fun (n,v) -> box <| XAttribute(!n, v)) |> Seq.toList
            let children = List.map (orderAttributes >> box) children
            let content = [|yield! attr; yield! children|]
            XElement(!name, content) :> XNode
        | x -> x
    let xx = orderAttributes x
    let yy = orderAttributes y
    XNode.DeepEquals(xx, yy)

let xnodeEqualityComparer = 
    { new IEqualityComparer<XNode> with
        member x.Equals(a,b) = a =. b
        member x.GetHashCode a = a.GetHashCode() }

let xnodeListEqualityComparer = 
    { new IEqualityComparer<XNode list> with
        member x.Equals(a,b) = 
            a.Length = b.Length && List.forall2 (fun x y -> x =. y) a b
        member x.GetHashCode a = a.GetHashCode() }

type XElement with
    static member Create(name, ?attr: (string * string) list, ?children: XNode list) =
        let attr = defaultArg attr []
        let children = defaultArg children []
        let attributes = Seq.map (fun (n,v) -> box (XAttribute(!n, v))) attr
        let childNodes = Seq.map box children
        let content = [|yield! attributes; yield! childNodes|]
        XElement(!name, content)

type XNode with
    static member Parse(x: string) =
        let x = sprintf "<r>%s</r>" x
        let xdoc = XDocument.Parse x
        xdoc.Document.Root.Nodes() |> Seq.toList

    /// Remove empty/whitespace text nodes
    member x.Trim() =
        match x with
        | TagA(name,attr,children) ->
            let cc = children |> List.choose (fun c -> c.Trim())
            Some (XElement.Create(name, attr, cc) :> XNode)
        | EmptyText t -> None
        | _ -> Some x

open Fuchu

type Assert =
//    static member ListExists(element, list) =
//        if not (List.exists ((=) element) list)
//            then failtestf "Expected element not found: %A\nActual: %A" element list
    static member XmlEqual(x: XNode, y: XNode) = 
        Assert.XmlEqual([x], [y])
//        if not (xnodeEqualityComparer.Equals(x,y))
//            then failtestf "Expected: %A\nActual: %A" x y
    static member XmlEqual(x: XNode list, y: XNode list) =
        let x = x |> List.choose (fun a -> a.Trim())
        let y = y |> List.choose (fun a -> a.Trim())
        if not (xnodeListEqualityComparer.Equals(x,y))
            then failtestf "Expected: %A\nActual: %A" x y
    static member inline XmlEqual(x: XNode, y: XNode list) = Assert.XmlEqual([x],y)
    static member inline XmlEqual(x: XNode list, y: XNode) = Assert.XmlEqual(x,[y])
    static member inline XmlEqual(x: string, y: XNode) = Assert.XmlEqual(XNode.Parse x, [y])
    static member inline XmlEqual(x: XNode, y: string) = Assert.XmlEqual([x], XNode.Parse y)
    static member inline XmlEqual(x: string, y: XNode list) = Assert.XmlEqual(XNode.Parse x, y)
    static member inline XmlEqual(x: XNode list, y: string) = Assert.XmlEqual(x, XNode.Parse y)
    static member inline XmlEqual(x: string, y: string) = Assert.XmlEqual(XNode.Parse x, XNode.Parse y)
