module EasyFunctions

//let last = <a>(l:List<a>):a
//that returns the last element of a list.
let rec last (l : List<'a>) : Option<'a> =
  match l with
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> last xs


//let rev = <a>(l:List<a>):List<a>
//that creates a list with the elements of l in reverse order.
let rec rev (l:List<'a>) : List<'a> = 
  match l with
  | [] -> []
  | x :: xs -> (rev xs) @ [x]


//let append = <a>(l1:List<a>List<a>) => (l2:List<a>):
//that adds all the elements of l2 after those in l1.
let rec append (l1: List<'a>) (l2: List<'a>) : List<'a> = 
  match l1,l2 with
  | [],l2 -> l2
  | x :: xs,l2 -> x :: (append xs l2)

//let nth = <a>(n:number) => (l:List<a>): a
//that returns the element in position n in l.
let rec nth (n : int) (l : List<'a>) : Option<'a> =
  if n > l.Length then
    None
  elif n = 0 then
    Some l.Head
  else
    nth (n - 1) l.Tail

//let palindrome = <a>(l:List<a>):boolean
//that checks if a list is palindrome. A list is palindrome if it is equal to its inverse.
let rec palindrome (l:List<'a>): bool =
  rev(l) = l


let EasyFunctions() =
  let l1 = [1;4;5;6;7;8;]
  let l2 = [2;5;6;7;8;9;]
  let l3 = [1;2;3;4;3;2;1]

  printfn "%A" (last l1)
  printfn "%A" (rev l1)
  printfn "%A" (append l1 l2)
  printfn "%A" (nth 3 l1)
  printfn "%A" (palindrome l2)