module ComposedSet.FsCheck
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open ComposedSet
open ComposedSetOfStrings

type Decomposable = string
let randomSequence length xs = Gen.arrayOfLength length (Gen.elements xs)
let randomSequenceOfChars =
    let randomSequenceOfChars' length =
        gen{ 
            let chars = "ABCDEF./ ".ToCharArray()        
            let! size = Gen.choose (0, length)
            let body = randomSequence size chars
            return! Gen.map (Decomposable : char[] -> Decomposable) body
        }
    Gen.sized randomSequenceOfChars'

type DecomposableGenerators = static member Decomposable() = randomSequenceOfChars |> Arb.fromGen

[<TestFixture>]
module Tests =

    [<Property(Arbitrary =[| typeof<DecomposableGenerators> |])>]
    let hashcode (validStrA:Decomposable) (validStrB:Decomposable) = 
        let a = ComposedSet.decompose validStrA
        let b = ComposedSet.decompose validStrB
        let equalHash  = (ComposedSet.calchash a = ComposedSet.calchash b)
        let equalValue = (a = b)
        // Two difference values could have equal hash, but I expect from the hash function that to be extremly unlikly
        equalHash = equalValue

    [<Property(Arbitrary =[| typeof<DecomposableGenerators> |])>]
    let equals (validStrA : Decomposable) (validStrB : Decomposable) =
        let a = ComposedSet.decompose validStrA
        let b = ComposedSet.decompose validStrB            
        (validStrA = validStrB) = (a = b)

    [<Property(Arbitrary =[| typeof<DecomposableGenerators> |])>]
    let trim (validStrA : Decomposable) (validStrB : Decomposable) =
        let a = ComposedSet.decompose validStrA
        let b = ComposedSet.decompose validStrB        
        let end_trimmed = ComposedSet.trimend a b
        let start_trimmed = ComposedSet.trimstart a end_trimmed
        let conc = ComposedSet.concat end_trimmed start_trimmed
        (ComposedSet.compose a) = (ComposedSet.compose conc)

    [<Property(Arbitrary =[| typeof<DecomposableGenerators> |])>]
    let concat (validStrA : Decomposable) (validStrB : Decomposable) (validStrC : Decomposable) =
        let (++) = ComposedSet.concat
        let a = ComposedSet.decompose validStrA
        let b = ComposedSet.decompose validStrB
        let c = ComposedSet.decompose validStrC
        (ComposedSet.compose (a ++ b ++ c)) = (validStrA + validStrB + validStrC)


let config = { Config.Quick with MaxTest = 1000; Arbitrary =[ typeof<DecomposableGenerators> ] }
Check.One(config, Tests.hashcode)
Check.One(config, Tests.equals)
Check.One(config, Tests.trim)
Check.One(config, Tests.concat)