(* Listing Paths

   Implement a function `all_paths` which given

     len : a nonnegative integer
     stp : a point (see below)
     endp : a point

   returns a list of `(dir * int)` lists, where each list contains
   sequence of directions to get from `stp` to `endp` in exactly `len`
   steps.

   Notes:
   A sequence of directions is a list of `dir * int` pairs with the
   property that no adjacent elements have the same `dir`.  For example,

     [(N, 1); (S, 1); (N, 3)]

   is a valid sequence of directions but

     [(N, 1); (N, 3); (S, 1)]

   is not. Instead, this should be

     [(N, 4), (S, 1)]

   Examples:
   let origin = {x=0;y=0}
   let _ = assert (all_paths 0 origin origin = [[]])
   let _ = assert (all_paths 1 origin origin = [])
   let _ = assert (all_paths 2 origin origin =
       [[(N,1);(S,1)] ;
        [(S,1);(N,1)] ;
        [(E,1);(W,1)] ;
        [(W,1);(E,1)] ])
   let _ = assert (all_paths 3 origin origin = [])
   let _ = assert (List.mem [(N,2);(S,2)] (all_paths 4 origin origin))
   let _ = assert (List.mem [(N,1);(E,1);(W,1);(S,1)] (all_paths 4 origin origin))

*)

type dir = N | S | E | W

type point = {
  x : int ;
  y : int ;
}

let move stp dir =
  match dir with
  | N -> { stp with y = stp.y + 1 }
  | S -> { stp with y = stp.y - 1 }
  | E -> { stp with x = stp.x + 1 }
  | W -> { stp with x = stp.x - 1 }

let rec all_paths (len : int) (stp : point) (endp : point) : (dir * int) list list =
  let loop current_point end_point steps_left dist_from_endp_x dist_from_endp_x= 
   []
in
  if len = 0 then [[]]
  else if len = 1 then []
  (* too short. cannot make path *)
  else if abs(endp.x - stp.x) + abs(endp.y - stp.y) > len then []
  else loop stp endp len (abs(endp.x - stp.x)) (abs(endp.y - stp.y))
    

  
