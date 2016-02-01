namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("ComposedSet")>]
[<assembly: AssemblyProductAttribute("ComposedSet")>]
[<assembly: AssemblyDescriptionAttribute("A collection that decomposes it's elements into smaller chunks which is then stored in an array. Each element is then represented by indices into the array of parts.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
