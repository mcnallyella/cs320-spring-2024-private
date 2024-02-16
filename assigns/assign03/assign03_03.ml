(* Boolean Expressions

   Implement a function `eval` which given

     v: an association list mapping `string`s to `bool`s
     e: a boolean expression (see below)

   returns `Some b` if every variable in `e` also appears in `v` and
   `e` evaluates to `b`, or `None` if not all variable in `e` appear
   in `v`.

   Example:
   let v = [("a", true); ("b", false); ("c", true)]
   let e = And (Var "a", Or (Var "b", Var "c"))
   let f = Not (Var "d")
   let _ = assert (eval v e = Some true)
   let _ = assert (eval v f = None)

   One way to think about evaluation: imagine `v` as defining a
   collection of `bools` in OCaml:

     let a = true
     let b = false
     let c = true

   and an expression as defining a boolean expression in OCaml:

     let e = a && (b || c)

   The goal of evaluation is to determine the value of `e`.

   Likewise, if you define an expression with a name that has not
   been defined, you would get a compile-time error

     let f = not d

   which is why the function `eval` should return `None` on `f`.

   Hint: Take a look at the textbook section on association lists
   (they are a simple implementation of a dictionary-like data
   structure), as well as the function List.assoc_opt.

*)

type bexp =
  | Var of string
  | Not of bexp
  | And of bexp * bexp
  | Or of bexp * bexp

let eval (v : (string * bool) list) (e : bexp) : bool option =
  let rec match_func (v : (string * bool) list) (e : bexp) : bool option =
    match e with
    | Var x -> List.assoc_opt x v 
    (* negate e *)
    | Not e -> 
      (match match_func v e with  
               | Some x -> Some (not x)
               | None -> None)
    (* join (and) e1 and e2 *)
    | And (e1, e2) -> 
      (match match_func v e1, match_func v e2 with  
                       | Some x1, Some x2 -> Some (x1&&x2)
                       | _ -> None)
    (* join (or) e1 and e2 *)
    | Or (e1, e2) -> 
      (match match_func v e1, match_func v e2 with  
                      | Some x1, Some x2 -> Some (x1||x2)
                      | _ -> None)
  in match_func v e 
