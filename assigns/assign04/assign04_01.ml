(* Cyclic Function Application

   Implement a function `apply_cycle` which, given

     funcs : a list of functions of type 'a -> 'a
     n : an arbitrary integer
     x : a starting value of type 'a

   returns the result of applying `max n 0` functions to `x`, where
   the functions applied are the functions in `funcs` in order from
   left to right, starting again from the beginning as necessary.

   For example, `apply_cycle [f1;f2;f3] 5 x` is equivalent to

     f2 (f1 (f3 (f2 (f1 x))))

   Your implementation should be TAIL-RECURSIVE.

   Examples:
   let f x = x + 1
   let g x = x - 1
   let h x = x * x
   let k x = x / 2
   let _ = assert (apply_cycle [f;g;g] 8 0 = -2)
   let _ = assert (apply_cycle [g;f;f] 8 0 = 2)
   let _ = assert (apply_cycle [f;g;g] 0 10 = 10)
   let _ = assert (apply_cycle [f;g;g] (-10) 20 = 20)
   let _ = assert (apply_cycle [f;h;k] 4 5 = 19)
*)

let apply_cycle (funcs : ('a -> 'a) list) (n : int) (x : 'a) : 'a =
  (* creates a sublist of length n *)
  (* used in create_funcs_list when (List.length funcs) % n != 0 *)
  let rec sublist n lst new_lst =
    match lst with
    | [] -> [] 
    | _ when n <= 0 -> List.rev new_lst 
    | x :: xs ->  sublist (n-1) xs (x::new_lst)
  in
  (* creates the list of funcs of length n *)
  let rec create_funcs_list old_funcs new_funcs n =
    if n = 0 then new_funcs
    else if n >= (List.length old_funcs) then create_funcs_list old_funcs (old_funcs@new_funcs) (n-(List.length old_funcs))
    else (new_funcs@(sublist n old_funcs []) )
  in
  (* goes through all the functions in order *)
  let rec go functions current_x=
    match functions with
    | [] -> current_x
    | hd :: tl -> go tl (hd current_x)
  in go (create_funcs_list funcs [] n) x