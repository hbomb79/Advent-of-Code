module M = CCMap.Make (String)

let word_digit_mapping =
  M.of_list
    [ "zero", "0"
    ; "one", "1"
    ; "two", "2"
    ; "three", "3"
    ; "four", "4"
    ; "five", "5"
    ; "six", "6"
    ; "seven", "7"
    ; "eight", "8"
    ; "nine", "9"
    ]
;;

let digit_regex =
  Str.regexp "\\(one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|[0-9]\\)"
;;

(*
   Performs two regular expression searches: one front-to-back, and the other back-to-front. Both
   searches are looking for the first number they can find (either a word representing a number from
   0-9 [e.g. two], or a literal 0-9 character). The matched substring (and it's starting index) are
   returned to the caller in a tuple

   'Failure' raised if one or more numbers could not be found
*)
let extract_numbers line =
  let matcher = Advent.Searcher.find_match digit_regex line in
  match matcher Forwards, matcher Backwards with
  | Some (firstIndex, firstSubStr), Some (lastIndex, _) when firstIndex = lastIndex ->
    firstSubStr, firstSubStr
  | Some (_, firstSubStr), Some (_, lastSubStr) -> firstSubStr, lastSubStr
  | _ -> Failure "Invalid matches found" |> raise
;;

(*
   If the given string matches a known number/word in our mapping, then it's digit
   representation is returned (i.e. 'one' -> '1'). If no mapping result is found, then the
   given input is returned as is
*)
let parse_match str =
  match M.find_opt str word_digit_mapping with
  | Some x -> x
  | _ -> str
;;

(*
   Extracts two numeric values from the given line using `extract_digits` The digits are
   represented as strings; they are concatenated together and the result is parsed to an int
   before being returned.
*)
let extract line =
  let first, last = extract_numbers line in
  int_of_string (parse_match first ^ parse_match last)
;;

(* Entry point

   Reads the puzzle input from a file and iterates over each line, reducing each to their
   respective numeric value - this value is accumulated over all iterations until the final result
   is achieved
*)
let () =
  let input = Advent.Strings.read_lines "./inputs/day01.txt" in
  let predicate acc line = acc + extract line in
  print_endline (string_of_int (List.fold_left predicate 0 input))
;;
