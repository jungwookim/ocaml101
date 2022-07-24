type nucleotide = A | C | G | T

let is_equal s1 s2 = s1 = s2
let equality_result n1 n2 =
  List.map2 is_equal n1 n2

let rec count_matched_element l e =
  match l with
  | [] -> 0
  | h :: t -> count_matched_element t e + if h = e then 1 else 0

let hamming_distance n1 n2 = 
  if List.length n1 = List.length n2 then
  let equality_list = equality_result n1 n2 in
  let result = count_matched_element equality_list false in
  Base.Result.Ok result
  else if List.length n1 = 0 then Base.Result.Error "left strand must not be empty"
  else if List.length n2 = 0 then Base.Result.Error "right strand must not be empty"
  else Base.Result.Error "left and right strands must be of equal length"
