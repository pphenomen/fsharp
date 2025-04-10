// Repunit Divisibility
// Problem 129

let isCoprimeWith10 x = x % 2 <> 0 && x % 5 <> 0

let findA n =
    let rec loop k current =
        if current = 0 then k
        else loop (k + 1) ((current * 10 + 1) % n)
    loop 1 1