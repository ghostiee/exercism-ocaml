type nucleotide = A | C | G | T

let hamming_distance dna_a dna_b =
  let (len_a, len_b) = (List.length dna_a, List.length dna_b) in
    if len_a != len_b then
        match (len_a, len_b) with
        | (0, _) -> Error "left strand must not be empty"
        | (_, 0) -> Error "right strand must not be empty"
        | _ -> Error "left and right strands must be of equal length"
    else
      Ok (List.fold_left2
        (fun acc a b -> if a = b then acc else acc + 1) 0 dna_a dna_b
      )