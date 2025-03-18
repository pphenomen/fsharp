open System

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
    | Some(x1, x2) -> printfn $"Решение: {x1}, {x2}"
    | Some(x1, x2) when x1 = x2 -> printfn $"Уравнение имеет один корень: {x1}"
    | None -> printfn "Нет вещественных корней"

let circleSquare r : float =
    let pi = 3.14159
    pi * r * r

let cylinderVolume h r : float =
    h * circleSquare r

let curryCylinderVolume h = 
    fun r -> cylinderVolume h r

[<EntryPoint>]
let main argv =
    printf "Введите высоту: "
    let h = Console.ReadLine() |> float

    printf "Введите радиус: "
    let r = Console.ReadLine() |> float

    let curriedVolume = curryCylinderVolume h
    let vol = curriedVolume r
    printfn "Объем цилиндра: %f" vol

    0