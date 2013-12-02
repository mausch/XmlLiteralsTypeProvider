module FSharpLib

[<Literal>]
let testHtml = """
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

type Template = XmlLiterals.Xml<testHtml>

let a = Template(title = "Hello world", mainText = "Bye world")
let rendered = a.Render()
printfn "%A" rendered
