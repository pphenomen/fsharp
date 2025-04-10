// Repunit Divisibility
// Problem 129

let isCoprimeWith10 x = x % 2 <> 0 && x % 5 <> 0

let findA n =
    let rec loop k current =
        match current with
        | 0 -> k
        | _ -> loop (k + 1) ((current * 10 + 1) % n)
    loop 1 1

let findMinN limit =
    Seq.initInfinite id |> Seq.filter isCoprimeWith10 |> Seq.find (fun n -> findA n > limit)

[<EntryPoint>]
let main args =
    printfn "A(n)>10 = %d" (findMinN 10)
    printfn "A(n)>1000000 = %d" (findMinN 1000000)

    0