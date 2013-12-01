module FSharpLib

[<Literal>]
let testHtml = """
<html>
<body>
	<h1><x:text name="title"/></h1>
</body>
</html>
"""

type Template = Samples.ShareInfo.TPTest.Html<testHtml>

let a = Template(title = "Hello world")

printfn "%A" (a.Render())
