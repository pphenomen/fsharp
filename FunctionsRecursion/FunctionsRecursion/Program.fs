printfn("Hello World!")

let solveQuadratic a b c =
    let discriminant = b*b - 4.0*a*c
    if discriminant > 0.0 then
        let x1 = (-b + sqrt discriminant) / (2.0*a)
        let x2 = (-b - sqrt discriminant) / (2.0*a)
        Some(x1, x2)
    elif discriminant = 0.0 then
        let x = -b / (2.0*a)
        Some(x, x)
    else 
        None

let res = solveQuadratic 1.0 2.0 -3.0
match res with
    | Some(x1, x2) -> printf $"Решение: {x1}, {x2}" 
    | None -> printfn "Нет вещественных корней"