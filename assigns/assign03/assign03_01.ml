(* Concatenation Lists

   A `concatlist` is a list whose constructors are based on
   concatenation instead of cons.  It has a constructor for the empty
   list (Nil), a single element list (Single) and the concatentation
   of two lists (Concat).

   Implement a function `sort` which given

     l : a `concatlist`

   returns a regular list with the same element as `l` but in sorted
   order.  You should do this WITHOUT trying to first converting `l`
   into a regular list.  In particular, you CANNOT use the function
   `List.sort`.

   Example:
   let l = Concat (Concat (Single 3, Single 2), Concat (Single 1, Single 10))
   let _ = assert (sort l = [1;2;3;10])

*)

type 'a concatlist
  = Nil
  | Single of 'a
  | Concat of 'a concatlist * 'a concatlist

let sort (l : 'a concatlist) : 'a list =
  let rec find_smallest_index l smallest =
    match l with 
    | [] -> smallest
    | x::l -> if x <= smallest then find_smallest_index l x else find_smallest_index l smallest
  in
  let remove_element l smallest =
    List.filter (fun x -> x <> smallest) l
  in
  let rec make_list l new_list =
    match l with 
    | [] -> new_list
    | x::l ->  make_list (remove_element l x) ((find_smallest_index l x)::new_list)
  in
  match l with
  | Nil -> []
  | Single l -> make_list l []
  | Concat (l1,l2)  -> make_list (l1::l2) []

    
