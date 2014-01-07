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
type TextHolesFileTemplate = XmlLiterals.XmlFile<"sample.html">

[<Literal>]
let textHoles2 = """
<html>
<head>
    <title><x:text name="title"/></title>
</head>
<body>
    <h1><x:text name="title"/></h1>
</body>
</html>
"""

type TextHolesTemplate2 = XmlLiterals.Xml<textHoles2>

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

        test "Text holes 2" {
            let a = TextHolesTemplate2(title = "Hello world")
            let rendered = a.Render()
            let expected = """
<html>
<head>
    <title>Hello world</title>
</head>
<body>
    <h1>Hello world</h1>
</body>
</html>
"""
            Assert.XmlEqual(expected, rendered)
        }

        test "Text holes from file" {
            let a = TextHolesFileTemplate(title = "Hi everyone", mainText = "Bye everyone")
            let rendered = a.Render()
            let expected = """
<html lang="en">
<head>
    <meta charset="utf-8" />
    <title>Hi everyone</title>
</head>
<body>
    <h1>Hi everyone</h1>
    <p>Bye everyone</p>
</body>
</html>
"""
            Assert.XmlEqual(expected, rendered)
        }
            
    ]

[<EntryPoint>]
let main args = run tests