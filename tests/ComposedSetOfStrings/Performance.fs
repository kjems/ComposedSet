module ComposedSetOfStrings.PerformanceTest
open ComposedSet
open ComposedSetOfStrings

let str_a  = "A.B.C.D"
let str_a2 = "A.B.C.D"
let str_b  = "B/A.C/A.C.D"
let str_c  = "C.D"
let str_d  = "A.B"
let str_e  = "A.B.C.D.E.F"

let fs_a   = ComposedSet.decompose str_a
let fs_a2  = ComposedSet.decompose str_a2
let fs_b   = ComposedSet.decompose str_b
let fs_c   = ComposedSet.decompose str_c
let fs_d   = ComposedSet.decompose str_d
let fs_e   = ComposedSet.decompose str_e

    
let profile name iterations f =
    let w = new System.Diagnostics.Stopwatch()
    w.Start()
    for i in 0..iterations do f()
    w.Stop()
    System.GC.Collect()
    System.Threading.Thread.Sleep(100)
    printfn "%dms:\t %s" w.ElapsedMilliseconds name

let iterations = 500000

profile "Startswith" iterations (fun () -> 
    ComposedSet.startswith fs_b fs_a  |> ignore
    ComposedSet.startswith fs_e fs_a  |> ignore
    ComposedSet.startswith fs_b fs_d  |> ignore)

profile "Endswith" iterations (fun () -> 
    ComposedSet.endswith fs_b fs_a  |> ignore
    ComposedSet.endswith fs_b fs_c  |> ignore)
    
profile "Equals" iterations (fun () -> 
    ComposedSet.equals fs_b fs_a  |> ignore
    ComposedSet.equals fs_a fs_a2 |> ignore
    ComposedSet.equals fs_a fs_e  |> ignore)

profile "Concat" iterations (fun () -> 
    ComposedSet.concat fs_b fs_a  |> ignore
    ComposedSet.concat fs_a fs_a2 |> ignore
    ComposedSet.concat fs_a fs_e  |> ignore)

profile "TrimEnd" iterations (fun () -> 
    ComposedSet.trimend fs_b fs_c |> ignore)
    
let str_bigtext = System.IO.File.ReadAllText("..\..\data\gc.cpp.txt")

let mutable fs_bigtext   = ComposedSet.decompose ""
profile "Decompose Large Text 1st time" 1 (fun () -> (fs_bigtext   <- ComposedSet.decompose str_bigtext))      
profile "Decompose Large Text 2nd time" 1 (fun () -> (fs_bigtext   <- ComposedSet.decompose str_bigtext))    
  
let mutable fs_composed_bigtext   = ""
profile "Compose Large Text" 4 (fun () -> (fs_composed_bigtext   <- ComposedSet.compose fs_bigtext))
   
let lines = str_bigtext.Split('\n')
profile "Decompose Line by Line" 1 (fun () -> Array.map ComposedSet.decompose lines |> ignore)

//printfn "%s" (ComposedSet.getParts())
printfn "Decompose => Compose Equal original: %b" ((ComposedSet.compose fs_bigtext).Equals(str_bigtext))