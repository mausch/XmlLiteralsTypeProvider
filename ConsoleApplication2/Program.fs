module FSharpLib

[<Literal>]
let testHtml = """
<html>
<head>
	<title><x:text name="title"/></title>
</head>
<body>
	<h1><x:text name="title"/></h1>
</body>
</html>
"""

type Template = Samples.ShareInfo.TPTest.Xml<testHtml>

let a = Template(title = "Hello world")

printfn "%A" (a.Render())
