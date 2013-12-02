module FSharpLib

[<Literal>]
let testHtml = """
<html>
<head>
	<title><x:text name="title"/></title>
</head>
<body>
	<h1><x:text name="h1"/></h1>
</body>
</html>
"""

type Template = XmlLiterals.Xml<testHtml>

let a = Template(title = "Hello world", h1 = "Bye world")
let rendered = a.Render()
printfn "%A" rendered
