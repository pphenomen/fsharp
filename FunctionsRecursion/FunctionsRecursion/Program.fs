printfn("Hello World!")

let solveQuadratic a b c =
    let discriminant = b*b - 4.0*a*c
    if discriminant > 0.0 then
        let x1 = (-b + sqrt discriminant) / (2.0*a)
        let x2 = (-b - sqrt discriminant) / (2.0*a)
        x1, x2
    elif discriminant = 0.0 then
        let x = -b / (2.0*a)
        x, x
    else 
        nan, nan