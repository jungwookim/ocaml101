let inc x = x + 1

let square x = x * x

let rec sum l =
    match l with
    | [] -> 0
    | h :: t -> h + sum t
    
let square_of_sum n =
    let l = List.init n inc in
    let s = sum l in
    square s
let sum_of_squares n =
    let l = List.init n inc in
    let squares = List.map square l in
    sum squares

let difference_of_squares n =
    square_of_sum n - sum_of_squares n
