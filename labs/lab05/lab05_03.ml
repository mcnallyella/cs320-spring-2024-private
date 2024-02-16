(* Matrix-vector multiplication

   Note to TF/TAs and myself: if you have time, go over how to deal
   with errors using options or results.

   Implement the function `mv_mul` which, given

     a : a list of list of floats representing a matrix (as a list of rows)
     v : a list of floats representing a vector

   returns the produce of `a` and `v`.  You may assume that `a` is
   well-formed, and that the multiplication is well-define (i.e., `v`
   has as many entries as `a` does columns.

*)

type 'a matrix = 'a list list

let rec dot_product (a : float list)(b: float list): float =
  match a,b with 
  | [], []-> 0.
  | h1::t1, h2::t2 -> (h1 *. h2) +. dot_product t1 t2
  | _, _ -> failwith "Dot product, wrong sizes"





let rec mv_mul (a : float matrix) (v : float list) : float list =
  match a with
  | [] -> []
  | hd::tl -> dot_product hd v :: mv_mul tl v


