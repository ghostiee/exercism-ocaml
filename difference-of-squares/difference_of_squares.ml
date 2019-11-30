let square_of_sum n =
  let s = n * (1 + n) / 2 in s * s

let sum_of_squares n =
  n * (n + 1) * (2 * n + 1) / 6

let difference_of_squares n =
  square_of_sum n - sum_of_squares n
