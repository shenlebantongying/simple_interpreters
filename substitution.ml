type bop =
  | Add
  | Mult
  | Leq;;

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr;;

let unbound_var_err = "Unbound variable";;
let bop_err = "Operator and operand type mismatch";;
let if_guard_err = "Guard of if must have type bool";;

let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | _ -> false;;

(*
e -> expr/env
v -> value
x -> the symbol x
*)

let rec subst e v x = match e with
  | Var y -> if x = y then v else e
  | Bool _ -> e
  | Int _ -> e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
    let e1' = subst e1 v x in
    if x = y
    then Let (y, e1', e2)
    else Let (y, e1', subst e2 v x)
  | If (e1, e2, e3) ->
    If (subst e1 v x, subst e2 v x, subst e3 v x);;

let rec step : expr -> expr = function
  | Int _ | Bool _ -> failwith "Does not step"
  | Var _ -> failwith unbound_var_err
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
    step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 ->
    Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith if_guard_err
  | If (e1, e2, e3) -> If (step e1, e2, e3)
and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err;;

let rec eval (e : expr) : expr =
  if is_value e then e
  else e |> step |> eval;;

eval (Let ("Y", (Int 3),
           (Let ("X", (Int 6),
                 (Binop (Add, (Var "Y"),
                         (Binop (Add, (Int 2),(Var "X"))))))))) ;;
(* 3+6+2 *)
