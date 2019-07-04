module Tutorial

let doFunc()=
  let rec factorial x = 
    if x < 1 then 1
    else x * factorial (x-1)
  printfn "factorial 5 : %i" (factorial 5)

let Tutorial()=
  doFunc()