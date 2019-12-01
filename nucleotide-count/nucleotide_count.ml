open Base

let empty = Map.empty (module Char)

let is_valid_dna = function
| 'A' -> Ok 'A'
| 'C' -> Ok 'C'
| 'G' -> Ok 'G'
| 'T' -> Ok 'T'
| x -> Error x

let count_nucleotide s c =
  match is_valid_dna c with
  | Error c -> Error c
  | Ok c -> let f acc i = match (acc, is_valid_dna i) with
    | (Error e, _) -> Error e
    | (_, Error i) -> Error i
    | (Ok a, Ok d) -> if (Char.equal d c) then Ok (a + 1) else Ok a
    in
      String.fold s ~init:(Ok 0) ~f

let count_nucleotides s =
  let f acc i = match (acc, is_valid_dna i) with
  | (Error e, _) -> Error e
  | (_, Error i) -> Error i
  | (Ok a, Ok d) -> Ok (
    Map.update a d ~f: (function None -> 1 | Some v -> v + 1)
  )
  in
    String.fold s ~init:(Ok empty) ~f
