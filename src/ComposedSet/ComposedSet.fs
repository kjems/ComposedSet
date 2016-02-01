module ComposedSet.FSharp.ComposedSet
    open Common
    type Indices = int array
    type Decomposed<'T> = private {indices : Indices; hash : int}
                 
    let composer (parts : ResizeArray<'T>) (stitch : 'T array -> 'T) =
        fun decomposed -> decomposed.indices |> Array.map (fun i -> parts.[i]) |> stitch
                
    let decomposer (parts : ResizeArray<'T>) (split : 'T -> 'T array) =
        let partIndex = Perf.memoize (fun p -> parts.Add(p); parts.Count - 1)
        Perf.memoize (fun composed ->
            let indices' = [|for part in split composed do yield partIndex part|]
            {indices = indices'; hash = Array.calchash indices'})

    let isempty    cs    = Array.isEmpty    cs.indices
    let calchash   cs    = Array.calchash   cs.indices
    let startswith xs ys = Array.startsWith xs.indices ys.indices
    let endswith   xs ys = Array.endsWith   xs.indices ys.indices
    let equals     xs ys = if xs.hash = ys.hash then Array.forall2 (=) xs.indices ys.indices else false
    let concat     xs ys = 
        let indices' = Array.append xs.indices ys.indices
        {indices = indices'; hash = Array.calchash indices'}
    let (++)             = concat
    let trimend    xs ys = 
        if endswith xs ys then 
            let indices' = Array.sub xs.indices 0 (Array.length xs.indices - Array.length ys.indices)
            {indices = indices'; hash = Array.calchash indices'}
        else xs
