module Answers
//BASIC FUNCTIONS
let rec allNumbers (n : int) : string =
  if n < 0 then
    ""
  elif n = 0 then
    string n
  else
    (allNumbers (n - 1)) + " " + (string n)

let rec allNumbersRev (n : int) : string =
  if n < 0 then
    ""
  elif n = 0 then
    string n
  else
    (string n) + " " + (allNumbersRev (n - 1))

let rec allNumbersRange (lower : int) (upper : int) : string =
  if upper < lower then
    ""
  elif lower = upper then
    string lower
  else
    (string lower) + " " + (allNumbersRange (lower + 1) upper)

let rec allNumbersRangeRev (lower : int) (upper : int) : string =
  if upper < lower then
    ""
  elif lower = upper then
    string lower
  else
    (string upper) + " " + (allNumbersRangeRev lower (upper - 1))

let rec allEvenRange (lower : int) (upper : int) : string =
  if upper < lower then
    ""
  elif lower  = upper && lower % 2 = 0 then
    string lower
  else
    if lower % 2 = 0 then 
      (string lower) + " " + (allEvenRange (lower + 1) upper) 
    else 
      allEvenRange (lower + 1) upper

let rec drawLine (length : int) : string =
  if length = 0 then "" else "*" + (drawLine (length - 1))

let rec drawSymbol (symbol : char) (length : int) : string =
  if length = 0 then "" else (string symbol) + (drawSymbol symbol (length - 1))

let rec toBinary (n : int) : string =
  if n = 0 then ""
  else digit 
    let digit = n % 2
    (toBinary (n / 2)) + (string digit)

let rec toBase (n : int) (_base : int) : string =
  if n = 0 then ""
  else
    let digit = n % _base
    (toBase (n / _base) _base) + (string digit)

//EASYFUNCTIONS

let rec last (l : List<'a>) : Option<'a> =
  match l with
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> last xs

let rec append (l1 : List<'a>) (l2 : List<'a>) : List<'a> =
  match l1,l2 with
  | [],l2 -> l2
  | x :: xs,l2 -> x :: (append xs l2)

let rec nth (n : int) (l : List<'a>) : Option<'a> =
  if n < 0 then
    None
  elif n = 0 then
    Some l.Head
  else
    nth (n - 1) l.Tail

let rec rev (l : List<'a>) : List<'a> = 
  match l with
  | [] -> []
  | x :: xs -> (rev xs) @ [x] //alternatively call append (rev xs) [x]

let palindrome (l : List<'a>) : bool =
  (rev l) = l

let rec compress (l : List<'a>) : List<'a> =
  match l with
  | [] -> []
  | [x] -> [x]
  | x :: y :: xs ->
      if x = y then
        compress (y :: xs)
      else
        x :: (compress (y :: xs))

let rec caesarCypher (l : List<char>) (shift : int) : List<char> =
  let shiftChar (c : char) (shift : int) =
    let charCode = int c
    if (charCode >= 97 && charCode <= 122) then
      (((int c) - 97) + shift) % 26 + 97 |> char
    else
      c

  match l with
  | [] -> []
  | x :: xs -> (shiftChar x shift) :: (caesarCypher xs shift)

let rec splitAt (i : int) (l : List<'a>) : List<'a> * List<'a> =
  match l with
  | [] -> [],[]
  | x :: xs ->
      if i = 0 then
        [],x :: xs
      else
        let left,right = splitAt (i - 1) xs
        x :: left,right

let rec merge (l1 : List<'a>) (l2 : List<'a>) : List<'a> =
  match l1,l2 with
  | [],l
  | l,[] -> l
  | x :: xs,y :: ys ->
      if x <= y then
        x :: (merge xs (y :: ys))
      else
        y :: (merge (x :: xs) ys)

let rec mergeSort (l : List<'a>) : List<'a> =
  match l with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let m = l.Length / 2
    let l1,l2 = splitAt m l
    let sortedL1 = mergeSort l1
    let sortedL2 = mergeSort l2
    merge sortedL1 sortedL2





//UNIT4

//This is the O(n^2) version, because for each element of the list
//you append its mapped counterpart at the end, thus it is n * O(n) = O(n^2)
let mapFold (f : 'a -> 'b) (l : List<'a>) : List<'b> =
  l |> List.fold (fun mappedList x -> mappedList @ [f x]) []

//This is the O(n) version, because for each element of the list
//you add its mapped counterpart in front, which costs O(1), thus you
//need n * O(1) = O(n) to map the list, and then O(n) again to reverse it
//and get the elements in the correct order, so O(n) + O(n) = O(n)
let mapFoldFast (f : 'a -> 'b) (l : List<'a>) : List<'b> =
  l |> List.fold (fun mappedList x -> (f x) :: mappedList) [] |> List.rev

//For the same reason as above, this is the O(n) version.
let filterFold (f : 'a -> bool) (l : List<'a>) : List<'a> =
  l |> List.fold 
          (fun filteredList x -> 
            if f x then 
              x :: filteredList 
            else
              filteredList) [] |> List.rev

let flatten (l : List<List<'a>>) : List<'a> =
  l |> List.fold (fun flattenedList l -> flattenedList @ l) []

let rec map2 (f : 'a -> 'b -> 'c) (l1 : List<'a>) (l2 : List<'b>) : Option<List<'c>> =
    match l1,l2 with
    | [],[] -> Some []
    | [],_
    | _,[] -> None
    | x :: xs,y :: ys ->
      let lopt = map2 f xs ys
      match lopt with
      | None -> None
      | Some l -> Some((f x y) :: l)

let rec fold2 (f : 'state -> 'a -> 'b -> 'state) (init : 'state) 
  (l1 : List<'a>) (l2 : List<'b>) : Option<'state> =

  match l1,l2 with
  | [],[] -> Some init
  | [],_
  | _,[] -> None
  | x :: xs,y :: ys ->
      fold2 f (f init x y) xs ys

//recursive version      
let rec zip (l1 : List<'a>) (l2 : List<'b>) : Option<List<'a * 'b>> =
  match l1,l2 with
  | [],[] -> Some []
  | [],_
  | _,[] -> None
  | x :: xs,y :: ys ->
      let lopt = zip xs ys
      match lopt with
      | None -> None
      | Some l -> Some((x,y) :: l)

//fold version
let zipFold (l1 : List<'a>) (l2 : List<'b>) : Option<List<'a * 'b>> =
  let folded =
    fold2 (fun pairs x y -> (x,y) :: pairs) [] l1 l2
  match folded with
  | None -> None
  | Some l -> Some (l |> List.rev)

let rec map2Safe (f : 'a -> 'b -> 'c) (l1 : List<'a>) (l2 : List<'b>) : 
  List<Option<'c>> =

  match l1,l2 with
  | [],[] -> []
  | l1,[] -> l1 |> List.map (fun _ -> None)
  | [],l2 -> l2 |> List.map (fun _ -> None)
  | x :: xs,y :: ys ->
      (Some (f x y)) :: (map2Safe f xs ys)



//UNIT 2
open System

let r = Random()

let clamp min max value =
  if value < min then
    min
  elif value > max then
    max
  else
    value

let minCoord = -50.0
let maxCoord = 50.0

let randomCoord min max =
  min + (r.NextDouble() * (max - min))

type Point2D =
  {
    Position : (float * float)
  }
  with
    static member Create(x : float,y : float) = { Position = (x,y) }
    static member CreateRandom(min : float,max : float) = Point2D.Create(randomCoord min max,randomCoord min max)
    member this.X = fst this.Position
    member this.Y = snd this.Position
    member this.Distance(point : Point2D) =
      Math.Sqrt((this.X - this.X) * (this.X - this.X) + (this.Y - this.Y) * (this.Y - this.Y))
      


type Blob =
  {
    Position : Point2D
    Size     : int
  }
  with
    static member Create() = { Position = Point2D.CreateRandom(minCoord,maxCoord); Size = r.Next(1,6)}
    member this.Speed = (float this.Size) / 10.0
    member this.Up = { this with Position = Point2D.Create(this.Position.X, clamp minCoord maxCoord (this.Position.Y + this.Speed)) }
    member this.Down = { this with Position = Point2D.Create(this.Position.X, clamp minCoord maxCoord (this.Position.Y - this.Speed)) }
    member this.Left = { this with Position = Point2D.Create(clamp minCoord maxCoord (this.Position.X - this.Speed), this.Position.Y) }
    member this.Right = { this with Position = Point2D.Create(clamp minCoord maxCoord (this.Position.X + this.Speed), this.Position.Y) }
    member this.Move() =
      let choice = r.Next(0,4)
      if choice = 0 then
        this.Up
      elif choice = 1 then
        this.Down
      elif choice = 2 then
        this.Left
      elif choice = 3 then
        this.Right
      else
        this

type World =
  {
    Blob1     : Blob
    Blob2     : Blob
    Ticks     : int
  }
  with
    static member Create(ticks : int) =
      {
        Blob1 = Blob.Create()
        Blob2 = Blob.Create()
        Ticks = ticks
      }
    member this.Run() =
      if this.Ticks <= 0 then
        printfn "%A" this
        this
      else
        printfn "%A" this
        {
          Blob1 = this.Blob1.Move()
          Blob2 = this.Blob2.Move()
          Ticks = this.Ticks - 1
        }.Run()

let test() =
  let w = World.Create(1000)
  w.Run()

        


let testUnit2_3() =
  let l1 = [-1;0;2;3;5;6]
  let l2 = [0;1;2;5;7;23]
  let rl = [0;1;1;2;3;3;3;2;3;4;4;4;5;0;0;0;0;0]
  let ul = [5;0;-1;3;2;3;-25;5]
  let aeneid = "Arma virumque cano, Troiae qui primum ab oris"
  printfn "%A" (last l1)
  printfn "%A" (append l1 l2)
  printfn "%A" (rev l1)
  printfn "%A" (compress rl)
  printfn "%A" (
    caesarCypher (
      aeneid.ToLower().ToCharArray() |> 
      Array.toList) 3 |> 
      List.map string |> 
      List.fold (+) "")
  printfn "%A" (splitAt 6 rl)
  printfn "%A" (merge l1 l2)
  printfn "%A" (mergeSort ul)


let testUnit4() =
  let l = [4;5;1;2;3;25;256]
  let l1 = "axsdsdy".ToCharArray() |> Array.toList |> List.map(fun x -> string x)
  let cl = [[3;1;4];[5;0;1];[2];[4;5];[1;3;2]]
  printfn "%A" (mapFoldFast (fun x -> x + 1) l)
  printfn "%A" (filterFold (fun x -> x % 2 = 0) l)
  printfn "%A" (flatten cl)
  printfn "%A" (map2 (fun x y -> (string x) + y) l l1)
  printfn "%A" (fold2 (fun s x y -> s + (string x) + y) "" l l1)
  printfn "%A" (zip l l1)
  printfn "%A" (zipFold l l1)
  printfn "%A" (map2Safe (fun x y -> x :: y) l cl)