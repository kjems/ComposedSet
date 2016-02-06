namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("ComposedSet")>]
[<assembly: AssemblyProductAttribute("ComposedSet")>]
[<assembly: AssemblyDescriptionAttribute("A collection that decomposes it's elements into smaller chunks which is then stored in an array. Each element is then represented by indices into the array of parts.")>]
[<assembly: AssemblyVersionAttribute("0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1"
