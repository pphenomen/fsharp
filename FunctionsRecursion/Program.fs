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
let rec digitalSum num =
    match num with
    | _ when num = 0 -> 0
    | _ -> (num % 10) + (digitalSum (num / 10))

// 5
let tailDigitalSum num =
    let rec loop num acc =
        match num with 
        | 0 -> acc
        | _ -> loop (num / 10) (acc + (num % 10))
    loop num 0

// 6
let rec factorial num =
    match num with
    | 0 | 1 -> 1
    | _ -> num * factorial(num - 1)

let tailFactorial num =
    let rec loop num acc = 
        match num with
        | 0 | 1 -> acc
        | _ -> loop (num - 1) (acc * num)
    loop num 1

let func (arg: bool) =
    match arg with
    | true -> tailDigitalSum
    | false -> tailFactorial

// 7
let rec reduce (num: int) (func: int->int->int) (acc: int) =
    match num with
    | 0 -> acc
    | _ -> 
        let digit = num % 10
        let newAcc = func acc digit
        let currentDigit = num / 10
        reduce currentDigit func newAcc

// 8
let reduceTest () =
    Console.WriteLine(reduce 1234 (fun acc digit -> acc + digit) 0)
    Console.WriteLine(reduce 1234 (fun acc digit -> acc * digit) 1)
    Console.WriteLine(reduce 1234 (fun acc digit -> if digit < acc then digit else acc) 10)
    Console.WriteLine(reduce 1234 (fun acc digit -> if digit > acc then digit else acc) 0)

// 9
let rec filterReduce (num: int) (func: int->int->int) (acc: int) (condition: int->bool) =
    match num with
    | 0 -> acc
    | _ -> 
        let digit = num % 10
        let newAcc = 
            match condition digit with
            | true -> func acc digit 
            | false -> acc
        let currentDigit = num / 10
        filterReduce currentDigit func newAcc condition

// 10
let filterReduceTest () = 
    Console.WriteLine(filterReduce 12345 (fun acc digit -> acc + digit) 0 (fun digit -> digit % 2 = 0))
    Console.WriteLine(filterReduce 12345 (fun acc digit -> acc * digit) 1 (fun digit -> digit <> 1))
    Console.WriteLine(filterReduce 12345 (fun acc digit -> acc + 1) 0 (fun digit -> digit > 3))
    Console.WriteLine(filterReduce 12345 (fun acc digit -> if digit < acc then digit else acc) 10 (fun digit -> true))

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
    match b with
    | 0 -> a
    | _ -> gcd b (a%b)

let coprimeDigits (num: int) (func: int -> int -> int) (acc: int) =
    let rec loop current acc =
        match current with
        | 0 -> acc
        | _ ->
            let digit = current % 10
            let newAcc =
                match digit with
                | 0 -> acc
                | digit when gcd num digit = 1 -> func acc digit
                | _ -> acc
            loop (current / 10) newAcc
    loop num acc

let coprimeDigitsTest () =
    Console.WriteLine(coprimeDigits 12345 (fun acc digit -> acc + digit) 0)
    Console.WriteLine(coprimeDigits 12345 (fun acc digit -> acc * digit) 1)
    Console.WriteLine(coprimeDigits 12345 (fun acc digit -> if digit < acc then digit else acc) 10)
    Console.WriteLine(coprimeDigits 12345 (fun acc digit -> if digit > acc then digit else acc) 0)
    Console.WriteLine(coprimeDigits 12345 (fun acc digit -> acc + 1) 0)

let eulerPhi num =
    let rec loop current acc = 
        match current with
        | 0 -> acc
        | _ -> 
            let newAcc =
                match gcd num current with
                | 1 -> acc + 1
                | _ -> acc
            loop (current - 1) newAcc
    loop (num - 1) 0

// 15
let coprimeWithCondition (num: int) (condition: int -> bool) (func: int -> int -> int) (initial: int) = 
    let rec loop current acc =
        match current with
        | 0 -> acc 
        | _ -> 
            let digit = current % 10
            let newAcc =
                if gcd num digit = 1 && condition digit then func acc digit
                else acc
            loop (current / 10) newAcc
    loop num initial

let coprimeWithConditionTest () =
    Console.WriteLine(coprimeWithCondition 12345 (fun digit -> digit % 2 = 0) (fun acc digit -> digit + acc) 0)
    Console.WriteLine(coprimeWithCondition 12345 (fun digit -> digit > 3) (fun acc digit -> digit * acc) 1)
    Console.WriteLine(coprimeWithCondition 12345 (fun digit -> digit <> 1) (fun acc digit -> if digit < acc then digit else acc) 10)
    Console.WriteLine(coprimeWithCondition 12345 (fun digit -> digit % 5 <> 0) (fun acc digit -> if digit > acc then digit else acc) 0)
    Console.WriteLine(coprimeWithCondition 12345 (fun digit -> digit < 4) (fun acc digit -> acc + 1) 0)

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

    0
