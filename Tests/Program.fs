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

//type TextHolesTemplate = XmlLiterals.Xml<textHoles>
type TextHolesTemplate = XmlLiterals.Xml<"sample.html">


open Fuchu
open Helpers

let tests = 
    TestList [
        test "Text holes" {
            
            let a = TextHolesTemplate(title = "Hello world", mainText = "Bye world")
            let rendered = a.Render()
            let expected = """
<html>
<head>
    <title>Hello world</title>
</head>
<body>
    <h1>Hello world</h1>
    <p>Bye world</p>
</body>
</html>
"""
            Assert.XmlEqual(expected, rendered)
            //printfn "%A" rendered
        }
            
    ]

[<EntryPoint>]
let main args = run tests
