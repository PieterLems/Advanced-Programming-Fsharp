module HardFunctions

//let filter = <a>(predicate:(x:a) => boolean) => (l:List<a>):List<a>
//that inserts in the output list only the elements for which predicate returns true

let rec filter (f : 'a -> 'b) (l:List<'a>) : List<'a> =
    match l with 
    | x::xs when f x -> x::(filter f xs)
    | _::xs -> filter f xs
    | [] -> []

//let filterFold = <a>(predicate:(x:a) => boolean) => (l:List<a>):List<a>
//that implements filter only using fold

let filterFold (f : 'a -> bool) (l : List<'a>) : List<'a> =
  l |> List.fold 
          (fun filteredList x -> 
            if f x then 
              x :: filteredList 
            else
              filteredList) [] |> List.rev

//let map = <a, b>(f:(x:a) => b) => (l:List<a>):List<b>
//that applies the function f to all the elements of l and returns a list containing the results.


//let mapFold = <a, b>(f:(x:a) => b) => (l:List<a>):List<b>
//that implements map only using fold
let mapFold (f : 'a -> 'b) (l : List<'a>) : List<'b> =
  l |> List.fold (fun mappedList x -> mappedList @ [f x]) []

//let fold = <s, a>(f:s(state:s) => (x:a) => s) => (init:s) => (l:List<a>):
//that applies a function f to elements in the same position from l, threading an accumulator argu-ment of type s through the computation.

//let apply = <a, b>(f:(x:a) => b) => (x: a):b
// that applies function f to element x.

//let curry = <a, b, c>(f:Tuple<a, b> => c) => (x:a) => (y:b):c
//that applies function ‘f‘ using as input elements ‘x‘ and ‘y‘ stored as a tuple.




//let flatten = <a>(l:List<List<a>>):List<a>
//that takes a list of lists and places all their elements it into a single one. Use fold to implement this function.


let HardFunctions()=
  printfn "HardFunctions"