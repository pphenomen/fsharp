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
let resultOutput () =
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

let curryChoiseLP () =
    let input = Console.ReadLine()
    let processing = choiseLP input
    let output = Console.WriteLine processing
    output

let superposChoiseLP () =
    (Console.ReadLine >> choiseLP >> Console.WriteLine)()

let rec gcd a b = 
    if b = 0 then a
    else gcd b (a % b)

let coprimeDigits (num: int) (func: int -> int -> int) (initial: int) =
    let rec helper current acc =
        if current = 0 then acc
        else
            let digit = current % 10
            let newAcc = 
                if digit <> 0 && gcd num digit = 1 then func acc digit
                else acc
            helper (current / 10) newAcc
    helper num initial

let sumCoprimeDigits num = coprimeDigits num (+) 0
let multiplyCoprimeDigits num = coprimeDigits num (*) 1
let minCoprimeDigits num = coprimeDigits num (min) Int32.MaxValue
let maxCoprimeDigits num = coprimeDigits num (max) Int32.MinValue
let countCoprimeDigits num = coprimeDigits num (fun acc _ -> acc + 1) 0 

let eulerPhi num =
    let rec helper current acc =
        if current = 0 then acc
        else
            let newAcc =
                if gcd num current = 1 then acc + 1
                else acc
            helper (current - 1) newAcc
    helper (num - 1) 0

let coprimeWithCondition (num: int) (condition: int -> bool) (func: int -> int -> int) (initial: int) = 
    let rec helper current acc =
        if current = 0 then acc
        else
            let digit = current % 10
            let newAcc =
                if digit <> 0 && gcd num digit = 1 && condition digit then func acc digit
                else acc
            helper (current / 10) newAcc
    helper num initial

let sumCoprimeWithCondition num = coprimeWithCondition num (fun digit -> digit % 2 = 0) (+) 0
let multiplyCoprimeWithCondition num = coprimeWithCondition num (fun digit -> digit > 3) (*) 1
let minCoprimeWithCondition num = coprimeWithCondition num (fun digit -> digit <> 2) (min) Int32.MaxValue 
let maxCoprimeWithCondition num = coprimeWithCondition num (fun digit -> digit % 5 <> 0) (max) Int32.MinValue
let countCoprimeWithCondition num = coprimeWithCondition num (fun digit -> digit < 4) (fun acc _ -> acc + 1) 0 

[<EntryPoint>]
let main argv =
    let num = 125
    Console.WriteLine($"Сумма взаимно простых с {num} (чётные) = {sumCoprimeWithCondition num}")
    Console.WriteLine($"Произведение взаимно простых с {num} (больше 3) = {multiplyCoprimeWithCondition num}")
    Console.WriteLine($"Минимум из взаимно простых с {num} (не равных 2) = {minCoprimeWithCondition num}")
    Console.WriteLine($"Максимум из взаимно простых с {num} (не делящихся на 5) = {maxCoprimeWithCondition num}")
    Console.WriteLine($"Количество взаимно простых с {num} (меньше 4) = {countCoprimeWithCondition num}")

    0
