let raindrop number =
  [(3, "Pling"); (5, "Plang"); (7, "Plong");]
  |> List.fold_left
    (fun acc (i, sound) -> if (number mod i = 0) then acc ^ sound else acc) ""
  |> function
    | "" -> string_of_int number
    | res -> res
