module AverageFunctions

//let splitAt = <a>(i:number) => (l:List<a>):Tuple<List<a>, List<a>>
//that splits the list into two lists, the first one containing all the elements of l from position 0
//to position i included, and the second one containing all the remaining elements.
//the two resulting lists are returned in a tuple. For example split 3 [3;5;4;-1;2;2] = fst: [3;5;4;-1],snd: [2;2] .
 
let rec splitAt (i : int) (l : List<'a>) : List<'a> * List<'a> =
  match l with
  | [] -> [],[]
  | x :: xs ->
      if i = 0 then
        [],x :: xs
      else
        let left,right = splitAt (i - 1) xs
        x :: left,right

//let merge = <a>(l1:List<a>) => (l2:List<a>):List<a>
//that merges together two sorted lists into a single sorted list

//let mergeSort = <a>(l:List<a>):List<a>
//implement the Merge Sort algorithm. The Merge Sort returns the list itself when the list has
//only one element. Otherwise it splits l in two lists, one containing the elements between position 0
//and l.length / 2 (Typescript does the floating point division so use Math.floor on the result to
//convert it to the result of the integer division), the other containing the elements from l.length
//1/ 2 + 1 until the end. Merge Sort is called recursively on the two lists. After this step use the
//function merge above to merge the two sorted lists