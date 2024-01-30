(* Block text

   Please implement the function `block_text` of type `string ->
   string` which, given

   - a string `s` consisting only of capital English characters A-Z
   - a nonnegative integer `min_width`
   - a positive integer `max_width`

   returns a string with the same characters as `s` but separated into
   lines with the following properties:

   - each line has length at most `max_width`
   - each line has the same length except possibly the last line
   - the last line has length at least `min_width`

   If the above three properties are not possible to satisfy, then
   every line except for the last line of the returned string should
   have length `max_width` (in other words, ignore the last
   condition).

   If there are multiple ways to satisfy the above three properties,
   the choice with the longest lines (besides the last line) should be
   used.

   Hint: Take a look at the following functions in the OCaml
   documentation:

   - `String.sub`
   - `String.length`

   Examples:
   let _ = assert (block_text "ABCDEFGHIJ" 0 3 = "ABC\nDEF\nGHI\nJ")
   let _ = assert (block_text "ABCDEFGHIJ" 2 3 = "AB\nCD\nEF\nGH\nIJ")
   let _ = assert (block_text "ABCDEFGHIJ" 0 4 = "ABCD\nEFGH\nIJ")
   let _ = assert (block_text "ABDFEFGHIJ" 3 4 = "ABCD\nEFGH\nIJ")

 *)

  let block_text (s : string) (min_width : int) (max_width : int) : string =
    let rec counter s new_string min_width counter_max_width=
    let first_chunk s counter_max_width =
      String.sub s 0 counter_max_width
    in 
    if (String.length s <= max_width) then new_string^"\n"^s
    else if (String.length new_string) < 1 then counter (String.sub s (counter_max_width) (String.length s  - counter_max_width)) ((first_chunk s counter_max_width)) min_width counter_max_width
    else counter (String.sub s (counter_max_width) (String.length s  - counter_max_width)) ((new_string^"\n"^first_chunk s counter_max_width)) min_width counter_max_width
    in
    let rec new_max_width s temp_max_width min_width = 
      if temp_max_width < min_width then max_width
      else if ((String.length s) mod temp_max_width) = 0 then temp_max_width
      else if ((String.length s) mod temp_max_width) >= min_width then temp_max_width
      else new_max_width s (temp_max_width - 1) min_width
    in 
    if (String.length s) < max_width then s
    else if (min_width = 0) then counter s "" 1 max_width
    else if (String.length s) mod max_width < min_width then counter s "" min_width (new_max_width s max_width min_width) 
    else counter s "" min_width max_width

    