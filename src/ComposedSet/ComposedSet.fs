namespace ComposedSet

/// <summary>
/// A collection that decomposes elements into smaller chunks stored in a shared array.
/// Each element is represented by indices into the array of parts, enabling fast
/// prefix/suffix matching and reduced memory for datasets with repeating substrings.
/// </summary>
module ComposedSet =
    open Common
    
    /// <summary>Array of indices into the parts pool.</summary>
    type Indices = int array
    
    /// <summary>
    /// A decomposed element represented as indices into a shared parts pool.
    /// The hash is pre-computed for fast equality checks.
    /// </summary>
    type Decomposed<'T> = private {indices : Indices; hash : int}
    
    /// <summary>
    /// Creates a composer function that reconstructs the original element from its decomposed form.
    /// </summary>
    /// <param name="parts">The shared parts pool.</param>
    /// <param name="stitch">Function to combine parts back into the original type.</param>
    /// <returns>A function that composes a Decomposed value back to its original form.</returns>
    let composer (parts : ResizeArray<'T>) (stitch : 'T array -> 'T) =
        fun decomposed -> decomposed.indices |> Array.map (fun i -> parts.[i]) |> stitch
    
    /// <summary>
    /// Creates a decomposer function that splits elements into parts and stores them in a shared pool.
    /// Results are memoized for efficiency.
    /// </summary>
    /// <param name="parts">The shared parts pool (will be mutated as new parts are added).</param>
    /// <param name="split">Function to split an element into its constituent parts.</param>
    /// <returns>A memoized function that decomposes values into index arrays.</returns>
    let decomposer (parts : ResizeArray<'T>) (split : 'T -> 'T array) =
        let partIndex = Perf.memoize (fun p -> parts.Add(p); parts.Count - 1)
        Perf.memoize (fun composed ->
            let indices' = [|for part in split composed do yield partIndex part|]
            {indices = indices'; hash = Array.calchash indices'})

    /// <summary>Returns true if the decomposed element has no parts.</summary>
    let isempty cs = Array.isEmpty cs.indices
    
    /// <summary>Computes the hash of the indices array.</summary>
    let calchash cs = Array.calchash cs.indices
    
    /// <summary>Returns true if xs starts with the same indices as ys.</summary>
    let startswith xs ys = Array.startsWith xs.indices ys.indices
    
    /// <summary>Returns true if xs ends with the same indices as ys.</summary>
    let endswith xs ys = Array.endsWith xs.indices ys.indices
    
    /// <summary>
    /// Checks equality using pre-computed hash for fast short-circuit.
    /// Only compares indices if hashes match.
    /// </summary>
    let equals xs ys = 
        if xs.hash = ys.hash then Array.forall2 (=) xs.indices ys.indices 
        else false
    
    /// <summary>Concatenates two decomposed elements by appending their indices.</summary>
    let concat xs ys = 
        let indices' = Array.append xs.indices ys.indices
        {indices = indices'; hash = Array.calchash indices'}
    
    /// <summary>
    /// Removes the suffix ys from xs if xs ends with ys.
    /// Returns xs unchanged if it doesn't end with ys.
    /// </summary>
    let trimend xs ys = 
        if endswith xs ys then 
            let indices' = Array.sub xs.indices 0 (Array.length xs.indices - Array.length ys.indices)
            {indices = indices'; hash = Array.calchash indices'}
        else xs
    
    /// <summary>
    /// Removes the prefix ys from xs if xs starts with ys.
    /// Returns xs unchanged if it doesn't start with ys.
    /// </summary>
    let trimstart xs ys = 
        if startswith xs ys then 
            let indices' = Array.sub xs.indices (Array.length ys.indices) (Array.length xs.indices - Array.length ys.indices)
            {indices = indices'; hash = Array.calchash indices'}
        else xs

    /// <summary>
    /// Checks if xs contains ys as a contiguous subsequence.
    /// Useful for finding patterns within decomposed paths/strings.
    /// </summary>
    let contains xs ys = Array.contains xs.indices ys.indices
    
    /// <summary>
    /// Returns the index where ys first appears in xs, or -1 if not found.
    /// </summary>
    let indexOf xs ys = Array.indexOf xs.indices ys.indices
