(* Matrices

   In this problem you will be building a (very) small interface for
   matrices.  A matrix is represented as a record which keeps track of
   its numbers of rows and columns as well as the values represented
   as a list of rows (i.e., a list of lists).  You will be
   implementing some error handling by working with `result`s.

   ************

   Task 1: Implement a function `mkMatrix` which given

     rs : a list of lists

   returns a matrix represented by this list of lists if it is valid
   or an `error` otherwise.  The error conditions are:

   * If the lengths of the rows in `rs` are not all equal then
   `mkMatrix` should return `Error UnevenRows`.

   * If `rs` is empty then `mkMatrix` should return `Error ZeroRows`.

   * If `rs` contains only empty lists, then `mkMatrix` should reutrn
   `Error ZeroCols`.

   In other words, `mkMatrix` should only return a matrix if `rs`
   represents a nonempty rectangular grid of values.

   Example:
   let l = [[1;2;3];[4;5;6]]
   let rm = mkMatrix l
   let _ = match rm with
     | Ok m ->
       let _ = assert (m.num_rows = 2) in
       let _ = assert (m.num_cols = 3) in
       ()
     | _ -> assert false

   let r = [[1;2;3;4];[1;2;3]]
   let rm' = mkMatrix r
   let _ = match rm' with
     | Ok _ -> assert false
     | Error e -> assert (e = UnevenRows)

   ************

   Task 2: Implement the function `transpose` which, given

     m : a matrix

   returns the transpose of `m`.

   Example:
   let _ = match rm with
     | Ok m ->
       let tm = transpose m in
       let _ = assert (tm.num_rows = 3) in
       let _ = assert (tm.num_cols = 2) in
       let _ = assert (tm.rows = [[1;4];[2;5];[3;6]]) in
       ()
     | _ -> assert false

   ************

   Task 3: Implement the function `multiply` which, given

     m : a matrix
     n : a matrix

   returns the product of `m` and `n` if it is possible to multiply
   them. The error condition:

   * If the number of columns of `m` is not the same as the number of
   rows of `n`, then `multiply` should return `Error MulMismatch`

   Example:
   let a =
     { num_rows = 2 ;
       num_cols = 2 ;
       rows = [[1.;2.];[3.;4.]] ;
     }

   let b =
     { numn_rows = 1 ;
       num_cols = 2 ;
       rows = [[1.; 2.]] ;
     }

   let _ = assert (multiply a a = Ok {
     num_rows = 2 ;
     num_cols = 2 ;
     rows = [[7.;10.];[15.;22.]] ;
    })

   let _ = assert (multiply a b = Error MulMismatch)

   ************

   References:
   https://en.wikipedia.org/wiki/Matrix_multiplication
   https://en.wikipedia.org/wiki/Transpose
   https://www.cs.bu.edu/fac/crovella/cs132-book/landing-page.html

*)

type error
   = UnevenRows
   | ZeroRows
   | ZeroCols
   | MulMismatch

type 'a matrix = {
  num_rows : int ;
  num_cols : int ;
  rows : ('a list) list ;
}

let mkMatrix (rs : 'a list list) : ('a matrix, error) result =
  let rec rows rs new_matrix len_row=
    match rs with 
    | [] -> Ok new_matrix
    | row :: tail when (List.length row) = 0 -> Error ZeroCols
    | row :: tail when List.length row <> len_row -> Error UnevenRows
    | row :: tail ->  rows tail {num_rows=new_matrix.num_rows+1; num_cols=new_matrix.num_cols+1; rows=new_matrix.rows@[row]} (List.length row)
  in
  match rs with
  | [] -> Error ZeroRows  
  | first_row::rs-> rows rs {num_rows=1; num_cols=0; rows=[first_row]}  (List.length first_row)
let transpose (m : 'a matrix) : 'a matrix =
  (* let rec parse_rows (m : 'a matrix) (cols : 'a list list): 'a list list = *)
  let rec parse_rows rows cols=
    match rows with 
    | [] -> cols
    | row::tail -> parse_rows tail (row::cols)
    
    (* {num_rows=m.num_rows-1; num_cols=m.num_cols; rows=tail} row::cols *)
in 
let rec create_t (cols : 'a list list) (t : 'a matrix): 'a matrix =
  match cols with 
  | [] -> t
  | col::tail -> create_t tail {num_rows=t.num_rows+1; num_cols=(List.length col); rows=t.rows@[(List.rev col)]} 
  (* | col::tail -> create_t tail {num_rows=t.num_rows+1; num_cols=(List.length col); rows=col::t.rows} *)
in 
create_t (parse_rows m.rows [[]]) {num_rows=0; num_cols=0; rows=[]} 


let multiply (m : float matrix) (n : float matrix) : (float matrix, error) result =
  
 
