(* Recipes by Ingredients

   Implement a function `recs_by_ingrs` which given

     recs : a list of recipes
     ingrs : a list of ingredients (i.e., strings)

   returns the list of those recipes in `recs` (in the same order)
   whose ingredients are included in `ingrs`.

   You may assume that `ingrs` and `r.ingrs` for every `r` in `recs`
   do not contain duplicates.

   Hint: The function List.mem may be useful.

   Example:
   let r1 = { name = "1" ; ingrs = ["a"; "b"; "d"] }
   let r2 = { name = "2" ; ingrs = ["a"; "c"; "e"] }
   let r3 = { name = "3" ; ingrs = ["b"; "c"] }
   let _ = assert (recs_by_ingrs [r1;r2;r3] ["a";"b";"c";"d"] = [r1;r3])
   let _ = assert (recs_by_ingrs [r1;r2;r3] ["a";"b";"c";"e"] = [r2;r3])

*)

type ingr = string

type recipe = {
  name : string ;
  ingrs : ingr list;
}

let recs_by_ingrs (l : recipe list) (s : ingr list) : recipe list =
  let rec loop_ingr_in_rec ingr_list =
    match ingr_list with
    | [] -> true
    | head::tail -> if (List.mem head s = false) then false else loop_ingr_in_rec tail
  in 
  let rec loop_rec rec_list new_rec_list = 
    match rec_list with
    | [] -> List.rev new_rec_list 
    | head::tail -> if (loop_ingr_in_rec head.ingrs) = false then loop_rec tail new_rec_list else loop_rec tail (head::new_rec_list)
  in loop_rec l []