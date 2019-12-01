open Base
open Result.Monad_infix

let empty = Map.empty (module Char)

let is_valid_nucleotide = function
| 'A' | 'C' | 'G' | 'T' -> Ok ()
| x -> Error x

let fold_nucleotide s ~f ~init =
  String.fold s ~init:init ~f: (fun acc dna ->
    is_valid_nucleotide dna >>= fun _ ->
    acc >>= fun a ->
      Ok (f a dna)
  )

let count_nucleotide s c =
  is_valid_nucleotide c >>= fun _ ->
  fold_nucleotide s ~init:(Ok 0) ~f:(fun acc dna ->
    if (Char.equal dna c) then acc + 1 else acc
  )

let count_nucleotides s =
  fold_nucleotide s ~init:(Ok empty) ~f:(fun acc dna ->
    Map.update acc dna ~f: (function None -> 1 | Some v -> v + 1)
  )
