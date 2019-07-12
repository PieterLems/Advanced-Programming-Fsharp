module Tutorial

open System



let matchnumber x = 
    match x with
    | 10 -> printfn " x is 10"
    | 8 -> printfn " x is 8"
    | _ -> printfn " x is something else"

let input1 = Some(99)
let input2 = None

let rec checkInput input=
    match input with
    | Some i -> printfn " input is an int=%d " i
    | None -> printfn " input is invalid"


let options1 (x:int) : string = 
    match x with
    | 1 -> "x = 1"
    | 2 -> "x = 2"
    | _ -> "x is something else "

let time = System.DateTime.Now.Month
let convertMonth (time : int) : string =
    match time with
    | 1 -> "January"
    | 2 -> "February"
    | 3 -> "March"
    | 4 -> "April"
    | 5 -> "May"
    | 6 -> "June"
    | 7 -> "Juli"
    | 8 -> "August"
    | 9 -> "September"
    | 10 -> "October"
    | 11 -> "November"
    | 12 -> "December"
    | _ -> "Incorrect input"

    
let seasonClock month =
    match month with
    | "December" | "January" | "February" -> "Winter"
    | "March" | "April" -> "Spring"
    | "May" | "June" | "Juli" | "August" -> "Summer"
    | "September" | "October" | "November" -> "Autumn"
    | _ -> "Incorrect month"

let safeGuardAge x =
    match x with
    | (n1,n2) when n1>n2 -> printfn "your older than %d"  n2
    | (n1,n2) when n1<n2 -> printfn "your younger than %d" n2
    | _ -> printfn "Your 18"


let enListing (l : List<'a>) =
    match l with
    | [] -> "List is empty"
    | [a] -> sprintf "the only value is %A" a
    | [a;_] -> sprintf "There are 2 values in the list , the first is %A" a
    | _ -> "There are more than 2 values in the list" 

let enListingHead (l:List<'a>) =
    match l with
    | [] -> "Empty"
    | head :: _ -> sprintf "the first value is %A" head

let rec recList (l: List<'a>) = 
    match l with
    | [] -> 0
    | _ :: tail ->  1 + recList tail

let rec applyToAll f list=
    match list with 
    | [] -> []
    | head::tail -> f head :: applyToAll f tail

let Tutorial()=
    printfn "Tutorial"
