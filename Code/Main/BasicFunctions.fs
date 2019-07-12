module BasicFunctions

//let allNumber = (n:number):string
//that returns a string containing all numbers from 0 to n. Separate the numbers with a whitespace.
let rec allNumber (n:int) : string =
    if n = 0 then string n else string n + " " + allNumber(n-1)

//let allNumberRev = (n:number):string
//that returns a string containing all numbers from n to 0. Separate the numbers with a white space.
let rec allNumbersRev (n:int) : string =
    if n=0 then string n else allNumbersRev(n-1) + " " + string n

//let allNumberRange = (lower:number) => (upper:number):string
//that returns a string containing all numbers between lower and upper. Separate the numbers with a white space
let rec allNumberRange (lower : int) (upper : int) : string =
    if lower = upper then string lower else string lower + " " + (allNumberRange(lower + 1) upper)

//let allNumberRangeRev = (lower:number) => (upper:number):string
//that returns a string containing all numbers between lower and upper in reverse order. Separate the numbers with a white space.
let rec allNumberRangeRev (lower : int) (upper: int) : string =
    if lower = upper then string lower else string upper + " " + (allNumberRangeRev lower (upper - 1) )

//let allEvenRange = (lower:number) => (upper:number):string
//that returns a string containing all even numbers between lower and upper. Separate the numbers with a white space.
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

//let drawLine = (length:number):string
//that returns a string containing length asterisks.
let rec drawLine (lenght : int ) : string =
    if lenght = 0 then 
        ""
    else 
        "*" + (drawLine(lenght-1))

//let drawSymbols = (symbol:string) => (length:number):string
//that returns a string containing length repetitions of symbol.
let rec drawSymbols (symbol:string) (lenght : int) : string =
    if lenght = 0 then "" else string symbol + (drawSymbols symbol (lenght-1)) 

//let toBinary = (n:number):string
//that returns a string containing the binary representation of the input number (it must be positive).
//The binary representation is obtained using the following procedure:
//1. Add to the end of the string the remainder of the division between the current number and 2.
//2. Repeat the previous step until the number is 0. In this case simply don’t add anything.
let rec toBinary (n:int) : string =
    if n = 0 then 
        " "
    else 
        let binaryN = n%2
        string binaryN + (toBinary (n/2))


//let toBase = (n:number) => (base:number):string
//that returns a string containing the representation of the input number in an arbitrary base (the
//number must be positive). The algorithm is the same as above except you must take the remainder of n divided by base
let rec toBase (n:int) (_base:int) : string =
    if n = 0 then 
        " "
    else 
        let binaryN = n%2
        string binaryN + (toBase (n/_base) _base)



let BasicFunctions()=
    printfn "BasicFunctions"
    printfn "%A"(allNumberRange 10 15)
    printfn "%A"(allNumberRangeRev 10 15)
    printfn "%A"(allEvenRange 10 15)
    printfn "%A"(drawLine 10)
    printfn "%A"(drawSymbols "#" 10)
    printfn "%A"(toBinary 10)
    printfn "%A"(toBase 10 10)
 