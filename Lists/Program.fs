open System

// 1
let rec readList n = 
    if n=0 then []
    else
    let Head = Convert.ToInt32(Console.ReadLine())
    let Tail = readList (n-1)
    Head::Tail

// 2
let rec writeList = function
    [] ->   let z = Console.ReadKey()
            0
    | (head : int)::tail -> 
                       Console.WriteLine(head)
                       writeList tail

// 3
let rec accCond list (f : int -> int -> int) p acc = 
    match list with
    | [] -> acc
    | h::t ->
                let newAcc = f acc h
                if p h then accCond t f p newAcc
                else accCond t f p acc

// 4
let accCondTest () =
    Console.WriteLine(accCond [1;2;3;4;5] (fun digit acc -> digit + acc) (fun digit -> digit % 2 = 0) 0)
    Console.WriteLine(accCond [1;2;3;4;5] (fun acc digit -> if digit < acc then digit else acc) (fun digit -> digit <> 1) 10)
    Console.WriteLine(accCond [1;2;3;4;5] (fun digit acc -> acc + 1) (fun digit -> digit % 2 <> 0) 1)

// 5
let max2 x y = if x > y then x else y

let listMax list = 
    match list with 
    |[] -> 0
    | h::t -> accCond list max2 (fun x -> true) h

let rec frequency list elem count =
        match list with
        |[] -> count
        | h::t -> 
                        let count1 = count + 1
                        if h = elem then frequency t elem count1 
                        else frequency t elem count

let rec freqList list mainList curList = 
        match list with
        | [] -> curList
        | h::t -> 
                    let freqElem = frequency mainList h 0
                    let newList = curList @ [freqElem]
                    freqList t mainList newList

let pos list el = 
    let rec pos1 list el num = 
        match list with
            |[] -> 0
            |h::t ->    if (h = el) then num
                        else 
                            let num1 = num + 1
                            pos1 t el num1
    pos1 list el 1

let getIn list pos = 
    let rec getIn1 list num curNum = 
        match list with 
            |[] -> 0
            |h::t -> if num = curNum then h
                     else 
                            let newNum = curNum + 1
                            getIn1 t num newNum
    getIn1 list pos 1

let f7 list = 
    let fL = freqList list list []
    (listMax fL) |> (pos fL) |> (getIn list)

// 6
type 'string btree = 
    Node of 'string * 'string btree * 'string btree
    | Nil

let prefix root left right = (root(); left(); right())
let infix root left right = (left(); root(); right())
let postfix root left right = (left(); right(); root())

let rec printTree tree =
    match tree with
    | Nil -> ()
    | Node(value, left, right) ->
        printfn "%s" value
        printTree left
        printTree right 

let stringTree = Node("A", Node("B", Node("D", Nil, Nil), Node("E", Nil, Nil)), Node("C", Nil, Node("F", Nil, Nil)))

// 7
let mostFrequency list =
    list |> List.groupBy id |> List.maxBy (fun (_, maxList) -> List.length maxList) |> fst

// 8
let countQuadsOfElements list =
    list |> List.filter (fun quadEl -> List.exists(fun el -> el * el = quadEl) list) |> List.length

[<EntryPoint>]
let main argv = 
    Console.WriteLine(countQuadsOfElements [2; 3; 4; 9; 5])

    0