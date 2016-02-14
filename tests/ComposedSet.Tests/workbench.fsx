#I __SOURCE_DIRECTORY__
#r "System.Core.dll"
#r "System.dll"
#r "../../src/ComposedSet/bin/Debug/ComposedSet.dll"
#r "../../tests/ComposedSetOfStrings/bin/Debug/ComposedSetOfStrings.exe"
open ComposedSet
open ComposedSetOfStrings

let a = ComposedSet.decompose "B."
let b = ComposedSet.decompose "."

let end_trimmed = ComposedSet.trimend a b
let start_trimmed = ComposedSet.trimstart a end_trimmed
let c = ComposedSet.concat end_trimmed start_trimmed
List.map ComposedSet.compose [a; b; end_trimmed; start_trimmed; c]

