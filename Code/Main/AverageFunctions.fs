module AverageFunctions

//let splitAt = <a>(i:number) => (l:List<a>):Tuple<List<a>, List<a>>
//that splits the list into two lists, the first one containing all the elements of l from position 0
//to position i included, and the second one containing all the remaining elements.
//the two resulting lists are returned in a tuple. For example split 3 [3;5;4;-1;2;2] = fst: [3;5;4;-1],snd: [2;2] .
let rec splitAt (i:int) (l:List<'a>) : List<'a> * List<'a> = 
  match l with
  | [] -> [],[]
  | head :: tail ->
    if i = 0 then
      [],head :: tail
    else
      let left,right = splitAt (i - 1) tail
      head :: left,right
  
//let merge = <a>(l1:List<a>) => (l2:List<a>):List<a>
//that merges together two sorted lists into a single sorted list
let rec merge (l1:List<'a>) (l2:List<'a>) : List<'a> =
  match l1,l2 with 
  | [],l -> l
  | l,[] -> l
  | head1::tail1,head2::tail2 -> 
    if head1<=head2 then
      head1 :: (merge tail1 (head2::tail2))
    else
      head2 :: (merge (head1::tail1) tail2)

//let mergeSort = <a>(l:List<a>):List<a>
//implement the Merge Sort algorithm. The Merge Sort returns the list itself when the list has
//only one element. Otherwise it splits l in two lists, one containing the elements between position 0
//and l.length / 2 (Typescript does the floating point division so use Math.floor on the result to
//convert it to the result of the integer division), the other containing the elements from l.length
//1/ 2 + 1 until the end. Merge Sort is called recursively on the two lists. After this step use the
//function merge above to merge the two sorted lists
let rec mergeSort (l : List<'a>) : List<'a> =
  match l with 
  | [] -> []
  | [x] -> [x]
  | _ -> 
    let m = l.Length / 2
    let l1,l2 = splitAt m l
    let sorted1 = mergeSort l1
    let sorted2 = mergeSort l2
    (merge sorted1 sorted2)


let foo (x :int -> string ) (y:int) : string = x y
let (f : int-> string )= foo(fun (x:int) -> string x)


// let map (f : 'a -> 'b)  (l : List<'a>) : List<'b> = l 
// let (x: List<int> -> List<string>) = map (fun (x : int) -> (string x + "1")

let curry (f : 'a * 'b -> 'c) : 'a -> 'b -> 'c = 
let add (x : int, y : int) : int =  x * y 
let (t : int-> int ) = curry add 5

//let eval = (expr1:Expr):Expr
//The function allows to evaluate arithmetic expressions. An arithmetic expression is defined as
//a discriminated union with the following cases:
//An atomic value. This is just a number
//The sum of two expressions.
//The difference of two expressions.
//The product of two expressions.
//The division of two expressions.
//The function eval for an atomic value simply returns the atomic value. For the other cases
//it simply evaluates recursively the arguments of the operator and then combine the two results
//together. For example, evaluating the sum of two expressions expr1 + expr2 recursively evaluates
//expr1 and expr2 and then sum ups the two atomic values together. The other operators behave analogously

//let eval = (expr1:Expr) => (stack:List<Tuple<string, number>>):Expr
//Extend the previous version of the function eval by supporting also variables. Variables are an-
//other case of the union. Evaluating a variable means looking it up in the stack. A stack is a list
//of tuple mapping a variable name to its value. If the lookup is successful eval returns the value of
//the variable, otherwise it throws an error.

//let run = (program: List<Statement>) =>(stack: List<Tuple<string, number>>): List<Tuple<string, number>>
//The function interprets a program from a small imperative language that can consists of the 
//following statements, expressed as a discriminated union.
//-Variable assignment. 
//A variable assignment contains two arguments: the name of the variable,
//and an expression that is used to assign a value to it. First the expression must be evaluated/
//2and then, if the variable already exists in stack, its value is updated, otherwise we add the
///variable name and the value in stack.
//-Print to output.
//This data structure contains an expression, which is evaluated and then
//prints the value on the console.
//The evaluation stops when the list of statements is empty.

let AverageFunctions()=
  printfn  "AverageFunctions"
  let list1 : List<'a> = [1;2;3;4;5;6;9]
  let list2 : List<'a> = [3;4;5;1;6;8]
  printfn "%A" (splitAt 3 list1)
  printfn "%A" (merge list1 list2)
  printfn "%A" (mergeSort list2)