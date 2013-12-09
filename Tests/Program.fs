module Tests

[<Literal>]
let textHoles = """
<html>
<head>
	<title><x:text name="title"/></title>
</head>
<body>
    <h1><x:text name="title"/></h1>
    <p><x:text name="mainText"/></p>
</body>
</html>
"""

type TextHolesTemplate = XmlLiterals.Xml<textHoles>

open Fuchu

let tests = 
    TestList [
        testCase "Text holes" <| fun _ ->
            
            let a = TextHolesTemplate(title = "Hello world", mainText = "Bye world")
            let rendered = a.Render()
            printfn "%A" rendered
    ]

[<EntryPoint>]
let main args = run tests