module BasicFunctions

//let allNumber = (n:number):string
//that returns a string containing all numbers from 0 to n. Separate the numbers with a whitespace.

let rec allNumberRev (n : int) : string =
    if n <0 then
        ""
    elif n = 0 then
        string n
    else
        (string n ) + " " + (allNumberRev (n-1))


//let allNumberRev = (n:number):string
//that returns a string containing all numbers from n to 0. Separate the numbers with a white space.

let rec allNumber (n : int ) : string =
    if n < 0 then
        ""
    elif n = 0 then 
        string n 
    else 
       (allNumber (n-1)) + " " + string n


//let allNumberRange = (lower:number) => (upper:number):string
//that returns a string containing all numbers between lower and upper. Separate the numbers with a white space
let rec allNumberRange (lower :int) (upper: int  ) : string = 
    if lower > upper then
        ""
    elif lower = upper then
        string lower
    else
        (string lower ) + " " + (allNumberRange lower ( upper - 1))

//let allNumberRangeRev = (lower:number) => (upper:number):string
//that returns a string containing all numbers between lower and upper in reverse order. Separate the numbers with a white space.


// EX3 oefententa


//let allEvenRange = (lowernumber) => (upper:number):string
//that returns a string containing all even numbers between lower and upper. Separate the numbers with a white space.


//let drawLine = (length:number):string
//that returns a string containing length asterisks.

//let toBinary = (n:number):string
//that returns a string containing the binary representation of the input number (it must be positive).
//The binary representation is obtained using the following procedure:
//1. Add to the end of the string the remainder of the division between the current number and 2.
//2. Repeat the previous step until the number is 0. In this case simply don’t add anything.

//let toBase = (n:number) => (base:number):string
//that returns a string containing the representation of the input number in an arbitrary base (the
//number must be positive). The algorithm is the same as above except you must take the remainder of n divided by base


//let filter = <a, b>(p: (x: a) => bool) => (l:List<a>) => List<a>
//let x: = filter((x: number) => x > 5)

//let rec filter (p :: x : a bool) (l:List<'a>) : List<'a> 
//let x: List<'b> -> List<'b> = filter((x : int) : x >5)
