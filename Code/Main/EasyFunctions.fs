module EasyFunctions

//let last = <a>(l:List<a>):a
//that returns the last element of a list.
let rec last (l:List<'a>) : Option<'a> =
  match l with
  | [] -> None
  | [x] -> Some x
  | _ :: tail -> last tail

//let append = <a>(l1:List<a>List<a>) => (l2:List<a>):
//that adds all the elements of l2 after those in l1.
let rec append (l1 : List<'a>) (l2:List<'a>) : List<'a> =
  match l1,l2 with
  | [],l2 -> l2
  | head::tail,l2 -> head:: (append tail l2)

//let rev = <a>(l:List<a>):List<a>
//that creates a list with the elements of l in reverse order.
let rec rev (l:List<'a>) : List<'a> = 
  match l with
  | [] -> []
  | head::tail ->  (append (rev tail) [head])  //alternatively call append (rev xs) [x]
 
//let nth = <a>(n:number) => (l:List<a>): a
//that returns the element in position n in l.
let rec nth (n:int) (l:List<'a>) : Option<'a> =
  if n < 0 then
    None
  elif n =0 then
    Some l.Head
  else
      (nth (n-1) l.Tail)

//let palindrome = <a>(l:List<a>):boolean
//that checks if a list is palindrome. A list is palindrome if it is equal to its inverse.
let rec palindrome (l:List<'a>) : bool = 
  (rev l) = l

//let compress = <a>(l:List<a>):List<a>
//that removes consecutive occurrences of the same element in the list. For example compress
//[a;a;a;a;b;b;c;c;b] = [a;b;c;b].
let rec compress (l:List<'a>) : List<'a> =
  match l with 
  | [] -> []
  | [x] -> [x]
  | head :: head1 :: tail -> 
      if head = head1 then
        compress (head :: tail)
      else 
        head :: (compress (head1 :: tail))

//let caesarCypher = (l:List<string>List<string>) => (shift:number):
//The Caesar’s cypher take a text, represented as a list of characters (note that Typescript does
//not support the type char so you can use a list of string with only one character), and shifts all
//the letters (so only if the character is an alphabetical character) in it up by the number of position
//specified by shift. If the letter goes past z it restarts from a. You can assume that all the text is in lower-case letter.
//For instance:
//shift("c")(5) = h
//shift("y")(5) = d
//The ASCII code for a specific character in a string can be obtained by using the method charCodeAt
//that takes as input the position of the character of the string you want to get the ASCII code of.
//For instance:
//"Caesar".charCodeAt(2) = 101

let EasyFunctions() =
  printfn "%A" "EasyFunctions"
  let list = [1;2;3;4;5;6]
  let list2 = [1;2;3;4;5;6]
  printfn "%A" (last list)
  printfn "%A" (rev list)
  printfn "%A" (append list list2)
  printfn "%A" (nth 3 list)




