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

// 9
let sumDigits num =
    let rec loop num acc =
        match num with
        | 0 -> acc
        | _ -> loop(num/10) (acc + (num%10))
    loop num 0

let countDivisors num =
    let rec loop num div acc =
        match div with
        | _ when div >= num -> acc
        | _ when num % div = 0 -> loop num (div + 1) (acc + 1)
        | _ -> loop num (div + 1) acc
    loop num 1 0

let sortList1 list1 = List.sortDescending(list1)
let sortList2 list2 = List.sortBy(fun el -> (sumDigits el)) list2
let sortList3 list3 = List.sortByDescending(fun el -> (countDivisors el)) list3

let listTuples list1 list2 list3 = List.zip3 (sortList1 list1) (sortList2 list2) (sortList3 list3)

let listTuplesTest () = 
    Console.WriteLine(sumDigits 12345)
    Console.WriteLine(countDivisors 4)
    Console.WriteLine(sortList1 [1;5;3;4;2])
    Console.WriteLine(sortList2 [19;18;17;16;15])
    Console.WriteLine(sortList3 [4;8;16;32;64])
    Console.WriteLine(listTuples [1;5;3;4;2] [19;18;17;16;15] [4;8;16;32;64])

// 10
let rec readData n =
    match n with
    | 0 -> []
    | _ -> 
        let head = Console.ReadLine()
        let tail = readData (n - 1)
        head::tail

let printList list =
    list |> List.iter (fun (el : string) -> Console.WriteLine(el))

let readDataSorted () = 
    let n = Console.ReadLine() |> int
    let sorted = readData n |> List.sortBy String.length
    printList(sorted)

// 11
let beforeLastMin list =
    let minEl = List.min list
    let lastMin = list |> List.findIndexBack (fun el -> el = minEl)
    list |> List.take lastMin 

let beforeLastMinChurch list =
    let rec loop curList minEl lastMin acc index =
        match curList with
        | [] -> List.rev acc
        | head::tail ->
            if head = minEl then 
                loop tail minEl (index + 1) acc (index + 1)
            else
                if index < lastMin then
                    loop tail minEl lastMin (head::acc) (index + 1)
                else
                    loop tail minEl lastMin acc (index + 1)

    let minEl = 
        let rec findMin list minVal =
            match list with
            | [] -> minVal
            | head::tail -> 
                if head < minVal then findMin tail head
                else findMin tail minVal
        findMin list 100

    let lastMinIndex =
        let rec findLastMin list minEl index acc =
            match list with
            | [] -> acc
            | head::tail ->
                if head = minEl then
                    findLastMin tail minEl (index + 1) index
                else
                    findLastMin tail minEl (index + 1) acc
        findLastMin list minEl 0 (-1)

    loop list minEl lastMinIndex [] 0

// 12
let shiftRight list =
    match list with
    | [] -> []
    | _ -> 
        let last = List.last list
        last::(list |> List.take (List.length list - 1))

let rec shiftRightChurch list acc =
    match list with
    | [] -> List.rev acc
    | head::tail ->
        if tail = [] then
            List.rev (head::acc)
        else
            shiftRightChurch tail (head::acc)

let shiftRightChurchFinal list =
    match list with
    | [] -> []
    | _ -> 
        let last = List.last list
        shiftRightChurch (list |> List.take (List.length list - 1)) [last]

// 13
let maxInRange list a b =
    let maxEl = List.max list
    maxEl >= a && maxEl <= b

let rec findMax list maxVal =
    match list with
    | [] -> maxVal
    | head::tail ->
        if head > maxVal then
            findMax tail head
        else
            findMax tail maxVal

let maxInRangeChurch list a b =
    let maxEl = findMax list -1
    maxEl >= a && maxEl <= b

// 14
let evenOddIndexes list =
    let even = list |> List.mapi (fun ind el -> (ind, el)) |> List.filter (fun (ind, _) -> ind % 2 = 0) |> List.map snd
    let odd = list |> List.mapi (fun ind el -> (ind, el)) |> List.filter (fun (ind, _) -> ind % 2 <> 0) |> List.map snd
    even @ odd

let evenOddIndexesChurch list =
    let rec loop list evenAcc oddAcc index = 
        match list with
        | [] -> List.rev evenAcc @ List.rev oddAcc
        | head::tail ->
            if index % 2 = 0 then
                loop tail (head::evenAcc) oddAcc (index + 1)
            else
                loop tail evenAcc (head::oddAcc) (index + 1)
    loop list [] [] 0

// 15
let rec isPrime num =
    let rec check divisor =
        match divisor >= num with
        | true -> true
        | false -> 
            match num % divisor with
            | 0 -> false
            | _ -> check (divisor + 1)
    if num < 2 then false else check 2

let getPrimeDivisors num =
    [2 .. num] |> List.filter (fun el -> num % el = 0 && isPrime el)

let uniquePrimeDivisors list =
    list |> List.collect getPrimeDivisors |> List.distinct

let rec getPrimeDivisorsChurch num divisor acc =
    if divisor > num then acc
    else 
        let newAcc = if num % divisor = 0 && isPrime divisor then acc @ [divisor] else acc
        getPrimeDivisorsChurch num (divisor + 1) newAcc

let rec uniquePrimeDivisorsChurch list =
    match list with
    | [] -> []
    | head :: tail -> 
        let divisors = getPrimeDivisorsChurch head 2 []
        let newList = uniquePrimeDivisorsChurch tail
        let allDivisors = divisors @ newList

        let rec removeDuplicates lst unique =
            match lst with
            | [] -> unique
            | h :: t -> 
                let updatedUnique = if List.contains h unique then unique else unique @ [h]
                removeDuplicates t updatedUnique

        removeDuplicates allDivisors []

// 16
let count list value = 
    list |> List.filter ((=) value) |> List.length
let processList list = 
    list |> List.filter (fun x -> x >= 0 && x < 100 && count list x > 2) |> List.map (fun x -> x * x) |> List.distinct

let rec count2 list value =
    match list with
    | [] -> 0
    | head :: tail ->
        let newCount = if head = value then 1 else 0
        newCount + count2 tail value

let rec filterList list acc =
    match list with
    | [] -> acc
    | head :: tail ->
        if head >= 0 && head < 100 && count2 list head > 2 then
            filterList tail (acc @ [head * head])
        else
            filterList tail acc

let rec removeDuplicates2 list acc =
    match list with
    | [] -> acc
    | head :: tail ->
        if List.contains head acc then
            removeDuplicates2 tail acc
        else
            removeDuplicates2 tail (acc @ [head])

let processListChurch list =
    let filteredList = filterList list []
    removeDuplicates2 filteredList []

[<EntryPoint>]
let main argv =
    Console.WriteLine(processList [1;2;2;2;3;4;5;5;5;5;-2;-2;-2;101;102])

    let churchList = readList 15
    printf "\n"
    let result = processListChurch churchList
    writeList result

    0