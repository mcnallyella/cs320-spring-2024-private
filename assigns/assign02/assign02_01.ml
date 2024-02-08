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
      (* add int to int list and add string list to new list *)
      | Int i::tail -> 
        if (List.length string_list) != 0 then 
          loop_list tail (i::int_list) [] (StringList (List.rev string_list)::new_list)
        else 
          loop_list tail (i::int_list) string_list new_list
      (* add string to string list and add int list to new list *)
      | String s::tail -> 
        if (List.length int_list) != 0 then 
          loop_list tail [] (s::string_list) (IntList (List.rev int_list)::new_list)
        else 
          loop_list tail int_list (s::string_list) new_list
      (* combine string list and int list into new list *)
      | [] -> 
        (match string_list, int_list with 
        | [], [] -> new_list 
        | _ -> if string_list = [] then new_list else StringList (List.rev string_list) :: new_list 
        | _ -> if int_list = [] then new_list else IntList (List.rev int_list) :: new_list)
    in
    List.rev (loop_list l [] [] [])