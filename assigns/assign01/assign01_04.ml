(* Taxicab numbers

   A taxicab number is a positive integer which can be expressed as
   the sum of two positive cubes in more than one way. (This is
   slightly different than the usual definition.)

   Please implement the function `taxicab` of type `int -> int` which,
   given a positive integer `n`, returns the the number of ways that
   `n` can be expressed as the sum of two positive cubes.

   Examples:
   let _ = assert (taxicab 2 = 1)
   let _ = assert (taxicab 5 = 0)
   let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
   let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)

 *)

 let taxicab (n : int) : int =
  (* function to find cube root of n *)
  let cube_n n = (float_of_int n) ** (1.0/.3.0)
  in 
  (* goes through all ints i that are less than the cube root of n *)
  let rec counter n i count =
    (*counts how many times i^3 plus another int si < cube_n n adds to n*)
    let rec sum_counter i si scount =
      if (float_of_int si) > (cube_n n) then scount
      else if (i*i*i) + (si*si*si) = n then sum_counter i (si+1) (scount+1)
      else sum_counter i (si+1) scount 
    in
    if n = 2 then 1
    (* divide count by 2 because each pair is counted twice. (1,5) and (5,1)*)
    else if (float_of_int i) > (cube_n n) then (count / 2)
    else counter n (i+1) (count + (sum_counter i 1 0))
  in 
  if n = 1 then 0
  else counter n 1 0  