let take_first_of_string s =
  match String.length s with
  | 0 -> ""
  | _ -> String.sub s 0 1 |> String.uppercase_ascii

let acronym s = 
  Base.String.split_on_chars s ~on:[' '; '_'; '-']
  |> List.map take_first_of_string
  |> List.fold_left String.cat ""
