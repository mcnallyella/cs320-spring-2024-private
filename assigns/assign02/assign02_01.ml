(* Grouping Integers and Strings

   Implement a function `convert` which given

     l : a list of `int_or_string`s

   returns a list of `int_list_or_string_list`s such that adjacent
   `int`s and `string`s are grouped together.

   Example:
   let test_in = [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"]
   let test_out = [IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["c"]]
   let _ = assert (convert test_in = test_out)

*)

type int_list_or_string_list
  = IntList of int list
  | StringList of string list

type int_or_string
  = Int of int
  | String of string

  let convert (l : int_or_string list) : int_list_or_string_list list =
    let rec loop_list (list : int_or_string list) (int_list : int list) (string_list : string list)  (new_list : int_list_or_string_list list) : int_list_or_string_list list =
      match list with 
      | Int i::tail -> 
        if List.length string_list != 0 then 
          loop_list tail (i::int_list) [] (StringList (List.rev string_list)::new_list)
        else 
          loop_list tail (i::int_list) string_list new_list
      | String s::tail -> 
        if List.length int_list != 0 then 
          loop_list tail [] (s::string_list) (IntList (List.rev int_list)::new_list)
        else 
          loop_list tail int_list (s::string_list) new_list
      | [] -> 
        (match string_list, int_list with 
        | [], [] -> new_list 
        | _ -> 
          let with_strings = if string_list = [] then new_list else StringList (List.rev string_list) :: new_list in
          if int_list = [] then with_strings else IntList (List.rev int_list) :: with_strings)
    in
    List.rev (loop_list l [] [] [])
  
  
  
(* let is_type t =
  match t with 
  | Int i -> "i"
  | String s -> "s"
  | _ -> "list" *)

  
  
  (* loops through list and deletes any matching values that are next to each other
  let rec loop_list (list : int_or_string list) (new_list : int_list_or_string_list list) : int_list_or_string_list list =
    match list with
    | [] -> new_list
    | String i -> (StringList [i])::new_list
    | Int i -> (IntList [i])::new_list
  in 
  let rec combine_lists list =
    match list with
    | [] -> list
    | [head] -> list 
    | head1::head2::tail -> 
      if (is_type head1) = (is_type head2) then (head2::head1)::loop_list tail
      else [head1]::loop_list (head2::tail) 
    in combine_lists (loop_list l [])
 *)




  
  
  
  
  
  
  (* let rec loop_list list =
    match list with
    | [] -> list
    | [head] -> list 
    | head1::head2::tail -> 
      if (is_type head1) = (is_type head2) then [head1,head2]::loop_list tail
      else if (is_type head1) = "list" && (is_type x::head1) = (is_type head2) then (head2::head1)::loop_list tail
      else [head1]::loop_list (head2::tail) 
  in 
  loop_list l *)
  
  
  
  
  
  
  
  
  (* let rec separate l (ints, strings) =

    match l with
    | [] -> (ints, strings)
    | Int i :: xs -> separate xs (i :: ints, strings)
    | String s :: xs -> separate xs (ints, s :: strings)
  in
  let (integers, strings) = separate l ([], []) in
  [IntList (List.rev integers); StringList (List.rev strings)] *)
