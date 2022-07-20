let prep_leap_year y = [y mod 4 = 0; y mod 100 = 0; y mod 400 = 0]
let leap_year y =
    match prep_leap_year y with
    | [_; false; true] -> true
    | [true; false; _] -> true
    | [true; _; true] -> true
    | _ -> false
