let raindrop n =
    match (n mod 3 = 0, n mod 5 = 0, n mod 7 = 0) with
    | (true, true, true) -> "PlingPlangPlong"
    | (true, false, true) -> "PlingPlong"
    | (true, false, false) -> "Pling"
    | (true, true, false) -> "PlingPlang"
    | (false, true, true) -> "PlangPlong"
    | (false, true, false) -> "Plang"
    | (false, false, true) -> "Plong"
    | (false, false, false) -> string_of_int n
