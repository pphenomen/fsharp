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

let tailDigitalSum num : int =
    let rec digitalSubSum num currentSum = 
        if num = 0 then currentSum
        else
            let currentNum = num / 10
            let digital = num % 10
            let accumulator = currentSum + digital
            digitalSubSum currentNum accumulator
    digitalSubSum num 0

let rec factorial num : int =
    if num <= 1 then 1
    else num * factorial(num - 1)

let tailFactorial num : int = 
    let rec helper num accumulator =
        if num <= 1 then accumulator
        else accumulator * num |> helper(num - 1)
    helper num 1

let rec digitFold (num : int) (func : int -> int -> int) (initial : int) =
    if num = 0 then initial
    else
        let digit = num % 10
        let newInitial = func initial digit
        digitFold (num / 10) func newInitial

[<EntryPoint>]
let main argv =
    let sumDigits = digitFold 12345 (+) 0
    Console.WriteLine($"Сумма чисел 12345 = {sumDigits}")

    let multiplyDigits = digitFold 12345 (*) 1
    Console.WriteLine($"Произведение чисел 12345 = {multiplyDigits}")

    let maxDigit = digitFold 12345 (max) Int32.MinValue
    Console.WriteLine($"Максимальное число из 12345 = {maxDigit}")

    let minDigit = digitFold 12345 (min) Int32.MaxValue
    Console.WriteLine($"Минимальное число из 12345 = {minDigit}")

    0