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

let cylinderVolume h S : float =
    h * S

let rec digitalSum num : int =
    if num = 0 then 0
    else (num % 10) + (digitalSum (num / 10))

[<EntryPoint>]
let main argv =
    System.Console.WriteLine "Введите радиус: "
    let r = Console.ReadLine() |> float

    System.Console.WriteLine "Введите высоту: "
    let h = System.Console.ReadLine() |> float

    let k = r |> circleSquare |> cylinderVolume h 
    System.Console.WriteLine(k) 

    let uprec = digitalSum 525
    Console.WriteLine($"Рекурсия вверх: {uprec}")

    0