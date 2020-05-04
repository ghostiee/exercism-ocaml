type base = int

let (>>=) m f =
  match m with
  | None -> None
  | Some(x) -> f x

let return x = Some(x)

let rec to_number number position from = function
  | [] -> number
  | h::t -> to_number (number + h * position) (from * position) from t 

let rec to_target result target number =
  match (number / target, number mod target) with
  | (quotients, rem) when quotients >= target -> to_target (rem::result) target quotients 
  | (quotients, rem) when quotients = 0 -> rem::result
  | (quotients, rem) -> quotients::rem::result 

let validate_from_target from target =
  match (from, target) with
  | (from, target) when from <= 1 || target <= 1 -> None
  | _ -> Some(from, target)

let rec validate_digits from = function
  | [] -> Some([])
  | h::_ when h < 0 || h >= from -> None
  | h::t -> validate_digits from t

let convert_bases ~from ~digits ~target =
  validate_from_target from target >>=
  fun _ -> validate_digits from digits >>=
  fun _ -> to_number 0 1 from (List.rev digits)
    |> to_target [] target
    |> return
