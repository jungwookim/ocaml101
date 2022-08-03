let take_first_of_string s =
  match String.length s with
  | 0 -> ""
  | _ -> String.sub s 0 1 |> String.uppercase_ascii

let acronym s = 
  let string_list = Base.String.split_on_chars s ~on:[' '; '_'; '-'] in
  let first_string_list = List.map take_first_of_string string_list in
  List.fold_left String.cat "" first_string_list
