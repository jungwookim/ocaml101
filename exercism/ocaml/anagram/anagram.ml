let string_sort s =
    String.uppercase_ascii s
    |> String.to_seq
    |> List.of_seq
    |> List.sort Char.compare
    |> List.to_seq
    |> String.of_seq

let is_anagram s1 s2 =
    if s1 = s2 then false
    else (string_sort s1) = (string_sort s2)

let anagrams str str_list =
    let upper_str_list = List.map String.uppercase_ascii str_list in
    let no_dup_str_list = List.fold_left (fun a b -> if a = b then a else [a]) [] upper_str_list in
    List.filter (fun s -> is_anagram s str) str_list
