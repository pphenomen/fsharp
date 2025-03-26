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

let digitFoldWithSum num =
    digitFold num (fun acc digit -> acc + digit) 0

let digitFoldWithMultiply num =
    digitFold num (fun acc digit -> acc * digit) 1

let digitFoldWithMin num =
    digitFold num (fun acc digit -> min acc digit) Int32.MaxValue

let digitFoldWithMax num =
    digitFold num (fun acc digit -> max acc digit) 0

let digitFoldWithCondition (num: int) (func: int -> int -> int) (initial: int) (condition: int -> bool) =
    let rec helper num acc = 
        if num = 0 then acc
        else
            let digit = num % 10
            let newAcc = if condition digit then func acc digit else acc
            helper (num / 10) newAcc
    helper num initial

let sumEvenDigitsCondition num =
    digitFoldWithCondition num (fun acc digit -> acc + digit) 0 (fun digit -> digit % 2 = 0)

let sumDigitsGreaterThreeCondition num =
    digitFoldWithCondition num (fun acc digit -> acc + digit) 0 (fun digit -> digit > 3)

let minDigitsCondition num =
    digitFoldWithCondition num (fun acc digit -> min acc digit) Int32.MaxValue (fun digit -> true)

let choiseLP input =
    match input with
    | "F#"| "Prolog" -> "Подлиза"
    | "Ruby" -> "Мало было?"
    | _ -> "Главное, что ты счастлив"

[<EntryPoint>]
let main argv =
    Console.WriteLine("Какой у тебя любимый язык программирования?")
    let input = Console.ReadLine()
    Console.WriteLine(choiseLP input)

    0