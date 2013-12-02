namespace Samples.FSharp.ShareInfoProvider

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

    let replaceText (name: string) (value: string) (template: XElement) =
        let textElems = 
            template.Descendants(textElemName)
            |> Seq.choose (fun e -> 
                            let nameAttr = e.Attribute(XName.Get "name")
                            if nameAttr = null || nameAttr.Value <> name then
                                None
                            else
                                Some e)
            |> Seq.toList |> List.toSeq
        for e in textElems do
            e.ReplaceWith(XText(value))
        
        template        

[<TypeProvider>]
type public HtmlProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    // Get the assembly and namespace used to house the provided types
    let thisAssembly =  Assembly.GetExecutingAssembly()
    let rootNamespace = "Samples.ShareInfo.TPTest"
    let baseTy = typeof<obj>

    let htmlTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, "Html", Some baseTy, IsErased = false)
    let buildType (assembly: ProvidedAssembly) typeName (args: obj[]) =
        let html = args.[0] :?> string
        let xelem = Impl.loadXml html
        let ty = ProvidedTypeDefinition(typeName, Some baseTy, IsErased = false)
        let templateField = ProvidedField("__template", typeof<string>)
        templateField.SetFieldAttributes FieldAttributes.InitOnly
        ty.AddMember templateField
        let texts = Impl.getTextSplices xelem |> Seq.distinct |> Seq.toList
        let fields = texts |> Seq.map (fun s -> ProvidedField(s, typeof<string>)) |> Seq.toList
        for f in fields do
            f.SetFieldAttributes FieldAttributes.InitOnly
        ty.AddMembers fields
        let ctorBody (args: Expr list) : Expr = 
            let this = args.[0]
            let setTemplate this = Expr.FieldSet(this, templateField, Expr.Value html)
            let setFields this =
                Seq.zip fields (Seq.skip 1 args)
                |> Seq.map (fun (f,a) -> Expr.FieldSet(this, f, a))
                |> Seq.fold (fun b e -> Expr.Sequential(b,e)) (setTemplate this)
            setFields this
        let ctorParams = texts |> Seq.map (fun s -> ProvidedParameter(s, typeof<string>)) |> Seq.toList
        let ctor  = ProvidedConstructor(ctorParams, InvokeCode = ctorBody)
        ty.AddMember ctor
        let render (this: Expr) : XElement Expr =
            let templateExpr = 
                <@
                    let templateHtml: string = (%%Expr.FieldGet(this, templateField))
                    let template = Impl.loadXml templateHtml
                    Impl.replaceText "title" (%%(Expr.FieldGet(this, fields.[0]))) template
                @>
            templateExpr
//            let replace (t: XElement Expr) (f: ProvidedField) =
//                <@@
//                    let () =
//                        let textElems = (%t).Descendants(Impl.textElemName) |> Seq.toList |> List.toSeq
//                        // for i in xxx do () // for loops don't seem to be supported in code generation
//                        let e = textElems.GetEnumerator()
//                        while (e.MoveNext()) do
//                            let nameAttr = e.Current.Attribute(XName.Get "name")
//                            if nameAttr = null then failwith "Name attribute not found"
//                            let name = nameAttr.Value
//                            let newValue: string = (%%Expr.FieldGet(this, f))
//                            e.Current.ReplaceWith(XText(newValue))
//                    ()
//                @@>
//            let replaceExprs = Seq.map (replace templateExpr) fields
//            let replaceExprs2 = Seq.fold (fun a b -> Expr.Sequential(a,b)) <@@ () @@> replaceExprs
//            Expr.Cast<XElement>(Expr.Sequential(replaceExprs2, templateExpr))
            //unbox 0
                
//
//                let textElems = template.Descendants(Impl.textElemName) |> Seq.toList |> List.toSeq
//                // for i in xxx do () // for loops don't seem to be supported in code generation
//                let e = textElems.GetEnumerator()
//                while (e.MoveNext()) do
//                    let nameAttr = e.Current.Attribute(XName.Get "name")
//                    if nameAttr = null then failwith "Name attribute not found"
//                    let name = nameAttr.Value
//                    let newValue: string = (%%Expr.FieldGet(this, fields |> List.find (fun f -> f.Name = (%name))))
//                    e.Current.ReplaceWith(XText(newValue))
//                template
//            @>
        let methods = ProvidedMethod("Render", [], typeof<XElement>, InvokeCode = fun args -> render args.[0] :> _)
        ty.AddMember methods
        htmlTy.AddMember ty
        ty

    let providedAssemblyName = System.IO.Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".dll")
    let providedAssembly = new ProvidedAssembly(providedAssemblyName)

    do 
        htmlTy.DefineStaticParameters([ProvidedStaticParameter("html", typeof<string>)], buildType providedAssembly)
        providedAssembly.AddTypes [htmlTy]
        htmlTy.AddMember(ProvidedConstructor(parameters = [], InvokeCode = fun args -> <@@ obj() @@>))

//        System.AppDomain.CurrentDomain.add_AssemblyResolve(fun _ args ->
//        let name = System.Reflection.AssemblyName(args.Name)
//        let existingAssembly = 
//            System.AppDomain.CurrentDomain.GetAssemblies()
//            |> Seq.tryFind(fun a -> System.Reflection.AssemblyName.ReferenceMatchesDefinition(name, a.GetName()))
//        match existingAssembly with
//        | Some a -> a
//        | None -> null
//        )

        this.AddNamespace(rootNamespace, [htmlTy])
        

[<TypeProviderAssembly>]
do ()