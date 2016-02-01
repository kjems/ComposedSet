namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("ComposedSet")>]
[<assembly: AssemblyProductAttribute("ComposedSet")>]
[<assembly: AssemblyDescriptionAttribute("A collection that decomposes it's elements into smaller chuncks which is then stored in an array. Each element is then represented by indicies into the array of parts. Many operation on the collection can be done by looking at the indicies.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
