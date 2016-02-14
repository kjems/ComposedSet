module Common
open FSharp.Core.Operators.NonStructuralComparison

module Perf =
    let memoize f =
        let cache = new System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        fun x -> match cache.TryGetValue(x) with
                 | true, c -> c
                 | _       -> let res = f x 
                              cache.Add(x, res)
                              res

module String =
    let inline build (xs : 'a array) = 
        let sb = System.Text.StringBuilder(xs.Length)
        for i = 0 to xs.Length - 1 do sb.Append(xs.[i]) |> ignore
        sb.ToString()
        
module List =
    let calchash = List.fold (fun h x -> h * 997 + x) 13
    
    let inline startsWith xs ys = 
        let rec startsWith' xs ys =
            match xs, ys with
            | [],[] | _, [] -> true
            | x::xs, y::ys when x = y -> startsWith' xs ys
            | _ -> false
        startsWith' xs ys

    let rec inline endsWith xs ys =
        startsWith (List.rev xs) (List.rev ys)

    let inline sub xs startIndex count =
        let rec sub xs c i acc = 
            match c,i with
            | c,_ when c >= count      -> List.rev acc
            | _,i when i <  startIndex -> sub xs c (i+1) acc
            | _,i when i >= startIndex -> 
                match xs with
                | []    -> List.rev acc
                | x::xs -> sub xs (c+1) (i+1) (x::acc)
            | _ -> []  // should not happen
        sub xs 0 0 []


module Array =
    let calchash = Array.fold (fun h x -> h * 997 + x) 13

    let inline startsWith xs ys = 
        let xs_length = Array.length xs
        let ys_length = Array.length ys
        if ys_length > 0 && xs_length >= ys_length then
            let shortest = ys_length
            let mutable i = 0
            let mutable equal = true
            while i < shortest do
                if xs.[i] = ys.[i] then
                    i <- i + 1
                else
                    i <- shortest
                    equal <- false
            equal
        else
            false

    let inline endsWith xs ys = 
        let xs_length = Array.length xs
        let ys_length = Array.length ys
        if ys_length > 0 && xs_length >= ys_length then
            let shortest = ys_length
            let mutable i = 1
            let mutable equal = true
            while i <= shortest do
                if xs.[xs_length - i] = ys.[ys_length - i] then
                    i <- i + 1
                else
                    i <- shortest + 1
                    equal <- false
            equal
        else
            false

