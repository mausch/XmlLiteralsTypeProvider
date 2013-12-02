namespace XmlLiteralsImpl

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

    let internal createProvidedInstanceMethod methodName parameters (body: (Expr * Expr list) -> 'a Expr) =
        ProvidedMethod(methodName, parameters, typeof<'a>, InvokeCode = fun args -> body (args.[0], Seq.skip 1 args |> Seq.toList) :> _) :> MemberInfo

    [<Literal>]
    let xmlns = "http://www.example.com/XmlLiteralsTypeProvider"

    let xnamespace = XNamespace.Get xmlns
    let textElemName = xnamespace + "text"

    let stringToStream (s: string) = 
        let ms = new MemoryStream()
        let writer = new StreamWriter(ms)
        writer.Write s
        writer.Flush()
        ms.Position <- 0L
        ms :> Stream

    let loadXml xml =
        let xnsmgr = XmlNamespaceManager(NameTable())
        xnsmgr.AddNamespace("x", xmlns)
        let settings = XmlReaderSettings()
        let xctx = XmlParserContext(null, xnsmgr, null, XmlSpace.Default)
        use ms = stringToStream xml
        let reader = XmlReader.Create(ms, settings, xctx)
        let xelem = XElement.Load(reader, LoadOptions.SetLineInfo)
        xelem

    let getNameOrFail (e: XElement) =
        let nameAttr = e.Attribute(XName.Get "name")
        if nameAttr = null then 
            let li = e :> IXmlLineInfo 
            let lineNumberInfo = 
                if li.HasLineInfo()
                    then sprintf " (line %d, position %d)" li.LineNumber li.LinePosition
                    else ""
            failwithf "Element %s%s is missing the required 'name' attribute" e.Name.LocalName lineNumberInfo
        nameAttr.Value

    let getTextSplices (x: XElement) =
        x.Descendants(textElemName)
        |> Seq.map getNameOrFail
        |> Seq.distinct

    // In-place text replace
    let replaceText name (newText: string) (template: XElement) =
        let textElems = 
            template.Descendants(textElemName)
            |> Seq.where (fun e -> getNameOrFail e = name)
            |> Seq.toList
        for e in textElems do
            e.ReplaceWith(XText(newText))

    let getFields (ty: Type) =
        ty.GetFields(BindingFlags.Instance ||| BindingFlags.NonPublic) |> Array.toSeq

    let replaceTextByField this template (f: FieldInfo) =
        let value = f.GetValue this
        replaceText f.Name (unbox value) template

    let internal buildType typeName (args: obj[]) =
        let xml = args.[0] :?> string
        let xelem = loadXml xml
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
            let setTemplate = Expr.FieldSet(this, templateField, Expr.Value xml)
            let setFields = Seq.zip fields (Seq.skip 1 args) |> Seq.map (fun (f,a) -> Expr.FieldSet(this, f, a))
            Expr.Sequentials [yield setTemplate; yield! setFields]
        let ctorParams = texts |> Seq.map (fun s -> ProvidedParameter(s, typeof<string>)) |> Seq.toList
        ty.AddMember(ProvidedConstructor(ctorParams, InvokeCode = ctorBody))

        let render (this: Expr) =
            <@
                let templateXml: string = %%Expr.FieldGet(this, templateField)
                let template = loadXml templateXml
                let thisType = %(Expr.ValueT (ty :> Type))
                let reflectedFields = getFields thisType
                let thisObj: obj = %%(Expr.Coerce(this, typeof<obj>))
                reflectedFields |> Seq.iter (replaceTextByField thisObj template)
                template
            @>
        let renderMethod = createProvidedInstanceMethod "Render" [] (fun (this,_) -> render this)
        ty.AddMember renderMethod
        ty

    // Get the assembly and namespace used to house the provided types
    let thisAssembly =  Assembly.GetExecutingAssembly()
    let rootNamespace = "XmlLiterals"

    let internal xmlTy = 
        let t = ProvidedTypeDefinition(thisAssembly, rootNamespace, "Xml", Some typeof<obj>, IsErased = false)
        t.DefineStaticParametersAndAdd([ProvidedStaticParameter("xml", typeof<string>)], buildType)
        t.AddMember(ProvidedConstructor(parameters = [], InvokeCode = fun args -> <@@ obj() @@>))
        t

    // this isn't used apparently, but it throws a compile-time error if removed:
    // FSC : error FS2014: A problem occurred writing the binary 'obj\Debug\ConsoleApplication2.exe': Error in pass3 for type FSharpLib, error: One of your modules
    // expects the type 'FSharpLib.Template' to be defined within the module being emitted.  You may be missing an input file
    let providedAssemblyName = System.IO.Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".dll")
    let providedAssembly = 
        let a = new ProvidedAssembly(providedAssemblyName)
        a.AddTypes [xmlTy]
        a

[<TypeProvider>]
type XmlLiteralsProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
    do this.AddNamespace(Impl.rootNamespace, [Impl.xmlTy])

[<TypeProviderAssembly>]
do ()