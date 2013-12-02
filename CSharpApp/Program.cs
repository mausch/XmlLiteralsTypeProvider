using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpApp {
    class Program {
        static void Main(string[] args) {
            var template = new FSharpLib.Template(title: "Hello world", mainText: "Bye world");
            Console.WriteLine(template.Render().ToString());
        }
    }
}
