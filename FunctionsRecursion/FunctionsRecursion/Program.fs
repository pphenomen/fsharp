open System

// 1
printfn("Hello World!")

// 2 
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

let resultOutput () =
    match solveQuadratic 1.0 2.0 1.0 with
        | Some(x1, x2) when x1 <> x2 -> printfn $"Решение: {x1}, {x2}"
        | Some(x, _) -> printfn $"Уравнение имеет один корень: {x}"
        | None -> printfn "Нет вещественных корней"

// 3
let circleSquare r : float =
    let pi = 3.14159
    pi * r * r

let cylinderVolume h S : float =
    h * S

let pipelineCylinderVolume () =
    let r = Console.ReadLine() |> float
    let h = Console.ReadLine() |> float

    r |> circleSquare |> cylinderVolume h |> printfn "Объем цилиндра: %f"

let superposCylinderVolume () =
    let r = Console.ReadLine() |> float
    let h = Console.ReadLine() |> float

    let superpos = circleSquare >> cylinderVolume h
    superpos r |> printfn "Объем цилиндра: %f"

let curryCylinderVolume () =
    let r = Console.ReadLine() |> float
    let h = Console.ReadLine() |> float
    
    (cylinderVolume h) (circleSquare r) |> printfn "Объем цилиндра: %f"

// 4
let rec digitalSum num : int =
    if num = 0 then 0
    else (num % 10) + (digitalSum (num / 10))

// 5
let tailDigitalSum num : int =
    let rec digitalSubSum num currentSum = 
        if num = 0 then currentSum
        else
            let currentNum = num / 10
            let digital = num % 10
            let accumulator = currentSum + digital
            digitalSubSum currentNum accumulator
    digitalSubSum num 0

// 6
let rec factorial num : int =
    if num <= 1 then 1
    else num * factorial(num - 1)

let tailFactorial num : int = 
    let rec helper num accumulator =
        if num <= 1 then accumulator
        else accumulator * num |> helper(num - 1)
    helper num 1

let func (arg: bool) =
    match arg with
    | true -> tailDigitalSum
    | false -> tailFactorial

// 7
let rec digitFold (num : int) (func : int -> int -> int) (initial : int) =
    if num = 0 then initial
    else
        let digit = num % 10
        let newInitial = func initial digit
        digitFold (num / 10) func newInitial

// 8
let digitFoldWithSum num =
    digitFold num (fun acc digit -> acc + digit) 0

let digitFoldWithMultiply num =
    digitFold num (fun acc digit -> acc * digit) 1

let digitFoldWithMin num =
    digitFold num (fun acc digit -> min acc digit) Int32.MaxValue

let digitFoldWithMax num =
    digitFold num (fun acc digit -> max acc digit) 0

// 9
let digitFoldWithCondition (num: int) (func: int -> int -> int) (initial: int) (condition: int -> bool) =
    let rec helper num acc = 
        if num = 0 then acc
        else
            let digit = num % 10
            let newAcc = if condition digit then func acc digit else acc
            helper (num / 10) newAcc
    helper num initial

// 10
let sumEvenDigitsCondition num = digitFoldWithCondition num (fun acc digit -> acc + digit) 0 (fun digit -> digit % 2 = 0)
let sumDigitsGreaterThreeCondition num = digitFoldWithCondition num (fun acc digit -> acc + digit) 0 (fun digit -> digit > 3)
let minDigitsCondition num = digitFoldWithCondition num (fun acc digit -> min acc digit) Int32.MaxValue (fun digit -> true)

// 11
let choiseLP input =
    match input with
    | "F#"| "Prolog" -> "Подлиза"
    | "Ruby" -> "Мало было?"
    | _ -> "Главное, что ты счастлив"

// 12
let curryChoiseLP () =
    let input = Console.ReadLine()
    let processing = choiseLP input
    let output = Console.WriteLine processing
    output

let superposChoiseLP () =
    (Console.ReadLine >> choiseLP >> Console.WriteLine)()

// 13
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

// 14
let eulerPhi num =
    let rec helper current acc =
        if current = 0 then acc
        else
            let newAcc =
                if gcd num current = 1 then acc + 1
                else acc
            helper (current - 1) newAcc
    helper (num - 1) 0

// 15
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

// ЛР: найти количество делителей числа 
let rec countDivisors num i =
    match i with
    | i when (num % i = 0 && i < num) -> 1 + countDivisors num (i + 1)
    | i when (i >= num) -> 0
    | _ -> 0 + countDivisors num (i + 1)
    
let tailCountDivisors num =
    let rec helper num i acc =
        match i with
        | _ when i >= num -> acc
        | _ when num % i = 0 -> helper num (i + 1) (acc + 1)
        | _ -> helper num (i + 1) acc
    helper num 1 0

let countDivisorsTest () =
    System.Console.WriteLine(countDivisors 10 1)
    System.Console.WriteLine(tailCountDivisors 10)

[<EntryPoint>]
let main argv =
    countDivisorsTest()

    0
