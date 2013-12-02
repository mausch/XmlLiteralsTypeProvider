namespace Samples.FSharp.ShareInfoProvider

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Runtime.Serialization
open System.Xml
open System.Xml.Linq


module Impl =
    type Expr with
        static member Sequentials s = Seq.reduce (fun a b -> Expr.Sequential(a,b)) s
        static member ValueT (v: 'a) : 'a Expr = Expr.Cast(Expr.Value v)

    type ProvidedTypeDefinition with
        member internal x.DefineStaticParametersAndAdd(parameters, instantiationFunction) =
            let builder n p =
                let r = instantiationFunction n p
                x.AddMember r
                r
            x.DefineStaticParameters(parameters, builder)

    [<Literal>]
    let xmlns = "http://www.example.com/HtmlTypeProvider"

    let xnamespace = XNamespace.Get xmlns
    let textElemName = xnamespace + "text"

    let stringToStream (s: string) = 
        let ms = new MemoryStream()
        let writer = new StreamWriter(ms)
        writer.Write s
        writer.Flush()
        ms.Position <- 0L
        ms :> Stream

    let loadXml html =
        let xnsmgr = XmlNamespaceManager(NameTable())
        xnsmgr.AddNamespace("x", xmlns)
        let settings = XmlReaderSettings()
        let xctx = XmlParserContext(null, xnsmgr, null, XmlSpace.Default)
        use ms = stringToStream html
        let reader = XmlReader.Create(ms, settings, xctx)
        let xelem = XElement.Load reader
        xelem

    let getTextSplices (x: XElement) =
        if x = null then nullArg "x"
        x.Descendants(textElemName)
        |> Seq.map (fun e -> 
                        let nameAttr = e.Attribute(XName.Get "name")
                        if nameAttr = null then 
                            failwithf "Element %s is missing the required 'name' attribute" e.Name.LocalName
                        nameAttr.Value)
        |> Seq.distinct

    // In-place text replace
    let replaceText (name: string) (value: string) (template: XElement) =
        let textElems = 
            template.Descendants(textElemName)
            |> Seq.where (fun e -> 
                            let nameAttr = e.Attribute(XName.Get "name")
                            nameAttr <> null && nameAttr.Value = name)
            |> Seq.toList
        for e in textElems do
            e.ReplaceWith(XText(value))

    let getFields (ty: Type) =
        ty.GetFields(BindingFlags.Instance ||| BindingFlags.NonPublic) |> Array.toSeq

    let replaceTextByField (this: 'a) template (f: FieldInfo) =
        let value = f.GetValue this
        replaceText f.Name (unbox value) template


open Impl

[<TypeProvider>]
type public HtmlProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    // Get the assembly and namespace used to house the provided types
    let thisAssembly =  Assembly.GetExecutingAssembly()
    let rootNamespace = "Samples.ShareInfo.TPTest"

    let htmlTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, "Html", Some typeof<obj>, IsErased = false)
    let buildType (assembly: ProvidedAssembly) typeName (args: obj[]) =
        let html = args.[0] :?> string
        let xelem = loadXml html
        let ty = ProvidedTypeDefinition(typeName, Some typeof<obj>, IsErased = false)
        let templateField = ProvidedField("__template", typeof<string>)
        templateField.SetFieldAttributes FieldAttributes.InitOnly
        ty.AddMember templateField
        let texts = getTextSplices xelem |> Seq.distinct |> Seq.toList
        let fields = texts |> Seq.map (fun s -> ProvidedField(s, typeof<string>)) |> Seq.toList
        for f in fields do
            f.SetFieldAttributes FieldAttributes.InitOnly
        ty.AddMembers fields
        let ctorBody (args: Expr list) : Expr = 
            let this = args.[0]
            let setTemplate = Expr.FieldSet(this, templateField, Expr.Value html)
            let setFields = Seq.zip fields (Seq.skip 1 args) |> Seq.map (fun (f,a) -> Expr.FieldSet(this, f, a))
            Expr.Sequentials [yield setTemplate; yield! setFields]
        let ctorParams = texts |> Seq.map (fun s -> ProvidedParameter(s, typeof<string>)) |> Seq.toList
        ty.AddMember(ProvidedConstructor(ctorParams, InvokeCode = ctorBody))
        let render (this: Expr) : XElement Expr =
            <@
                let templateHtml: string = (%%Expr.FieldGet(this, templateField))
                let template = loadXml templateHtml
                let thisType = (%(Expr.ValueT (ty :> Type)))
                let reflectedFields = getFields thisType
                //let thisObj : obj = (%%(Expr.Coerce(this, ty)) : obj)
                //let thisObj : obj = box %%this
                //reflectedFields |> Seq.iter (replaceTextByField (%%this) template)
                template
            @>
        let methods = ProvidedMethod("Render", [], typeof<XElement>, InvokeCode = fun args -> render args.[0] :> _)
        ty.AddMember methods
        ty

    let providedAssemblyName = System.IO.Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".dll")
    let providedAssembly = new ProvidedAssembly(providedAssemblyName)

    do 
        htmlTy.DefineStaticParametersAndAdd([ProvidedStaticParameter("html", typeof<string>)], buildType providedAssembly)
        providedAssembly.AddTypes [htmlTy]
        htmlTy.AddMember(ProvidedConstructor(parameters = [], InvokeCode = fun args -> <@@ obj() @@>))
        this.AddNamespace(rootNamespace, [htmlTy])
        

[<TypeProviderAssembly>]
do ()