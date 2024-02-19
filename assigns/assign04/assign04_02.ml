(* Walking on a Graph

   We can think of function `g` of type `'a -> 'a -> bool` as a graph
   in which there is an edge between `i` and `j` exactly when `g i j`
   is `true`.

   A function `p` of type `'a -> 'a` generates a list of values by
   repeated application from a given starting point.  We will call
   this a POTENTIAL PATH and we will call `p` a PATH GENERATOR. For
   example, the list `[x; p x; p (p x)]` is potential path of length 2
   starting at `x`

   A potential path is VALID for a given graph `g` if every pair of
   adjacent elements forms an edge in `g`.  Continuing the above
   example, the potential path `[x; p x; p (p x)]` is valid for `g` if
   `g x (p x) && g (p x) (p (p x))` is `true`.

   Implement the function `walks` which, given

     g : a graph (a function of type 'a -> 'a -> bool)
     len : an integer
     paths_starts : a list of (path generator, starting point) pairs

   returns the list of endpoints of all valid paths which have length
   `max 0 len` generated by each path generator on its associated
   starting point.

   The order of endpoints in the output should be consistent with the
   order of the starting points, even if some potential paths are
   dropped because they are not valid in `g`.

   You should use the functions `List.map` and `List.filter`.  It may
   also be useful to try to use the pipelining operator `|>`.

   Examples:
   let g1 (i : int) (j: int) = i < j && i <= 10 && j <= 10
   let g2 (i : int) (j: int) = i <= 10 && j <= 10

   let p1 i = i + 1
   let p2 i = i - 1
   let p3 i = i + 2

   let _ = assert (walks g1 0 [(p1, 0); (p2, 0); (p3, 0)] = [0;0;0])
   let _ = assert (walks g1 1 [(p1, 0); (p2, 0); (p3, 0)] = [1;2])
   let _ = assert (walks g1 3 [(p1, 0); (p2, 0); (p3, 0)] = [3;6])
   let _ = assert (walks g1 6 [(p1, 0); (p2, 0); (p3, 0)] = [6])
   let _ = assert (walks g2 2 [(p1, 3); (p2, 5); (p3, 3)] = [5; 3; 7])
   let _ = assert (walks g2 4 [(p1, -10); (p2, -20); (p3, 8)] = [-6; -24])
   let _ = assert (walks g2 6 [(p1, 5); (p2, 11); (p3, -10)] = [2])
*)

let walks
    (g : 'a -> 'a -> bool)
    (len : int)
    (paths_starts : (('a -> 'a) * 'a) list) : 'a list =

(* Outline: 
    loop through each path
    do length 'len' on path
    see if end point is in g
    if so then add to output list *)
    let rec make_list n x =
      if n <= 0 then []
      else x :: make_list (n - 1) x
    in
    (* goes through all the functions in order *)
  let rec find_endpoint functions current_x=
    match functions with
    | [] -> current_x
    | hd :: tl -> find_endpoint tl (hd current_x)
  in
    let rec loop_paths paths output_points =
      match paths with 
      | [] -> output_points
      | hd::tl ->  let (path, start) = hd in loop_paths tl output_points@((find_endpoint (make_list len path) start)::output_points)
      (* find endpoint, check endpoint, add to output_points, loop again*)
  in 
    let rec check_paths output_points new_points=
      match output_points with
      | [] -> new_points
      | [hd] -> 
        (match new_points with
        | [] -> output_points
        | hd::tl ->  if (g (hd) (List.hd output_points)) = true then new_points@output_points
        else new_points)
      | hd1::hd2::tl -> 
        if (g (hd1) (hd2)) = true then check_paths tl (new_points@[hd1;hd2])
        else 
          (match tl with
          | [] -> 
            (match new_points with
            | [] -> output_points
            | hd::tl ->  if (g (hd) (List.hd output_points)) = true then new_points@output_points
            else new_points)
          | hd::tail -> if (g (hd1) (hd)) = true then check_paths tail (new_points@[hd1;hd])
            else if (g (hd2) (hd)) = true then check_paths tail (new_points@[hd2;hd])
            else check_paths tail (new_points))
    in check_paths (List.rev (loop_paths paths_starts [])) []
   
   

