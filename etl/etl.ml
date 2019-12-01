let transform data =
  List.fold_left (fun acc (number, char_list) ->
    (List.map (fun char ->
      (Char.lowercase_ascii char, number)
    ) char_list) @ acc
  ) [] data
  |> List.sort (fun (a, _) (b, _) -> Char.compare a b)