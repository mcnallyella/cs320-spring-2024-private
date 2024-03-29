(* Icy Hot

   Implement the function `reduce` which given

     l : a list of `temp`s

   returns a new list of `temp`s gotten by the following reduction rule:

   If `Hot i` and `Icy i` are adjacent (in particular, they must be
   carrying the same value) in any order, then they cancel out and are
   removed from the list.

   This rule should be carried out until it not possible to reduce the
   list any further.

   Examples:
   let _ = assert (reduce [Hot 0;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Icy 1;Hot 1;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Hot 1;Icy 2;Hot 2;Hot 0;Icy 1] = [Hot 0;Hot 1;Hot 0;Icy 1])

*)

type temp
  = Hot of int
  | Icy of int

let int_of_temp t =
  match t with 
  | Hot i -> i
  | Icy i -> i

let char_of_temp t =
  match t with 
  | Hot i -> "h"
  | Icy i -> "s"

let reduce (l : temp list) : temp list =
  (* loops through list and deletes any matching values that are next to each other *)
  let rec loop_list (list : temp list) : temp list =
    match list with
    | [] -> list
    | [head] -> list 
    | head1::head2::tail -> if (int_of_temp head1) = (int_of_temp head2) && (char_of_temp head1) != (char_of_temp head2) then loop_list tail else head1::loop_list (head2::tail) 
  in 
  (* loops through list again and again until no matching values are next to each other *)
  let rec loop_loop list =
    let new_list = loop_list list
    in 
    if list = new_list then new_list
    else loop_loop new_list
  in 
  loop_loop l
