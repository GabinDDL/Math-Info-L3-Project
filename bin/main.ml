module Sat = Msat_sat
module E = Sat.Int_lit (* expressions *)

let ex_solve_sat_problem () =
  (* Création d'un solveur SAT *)
  let solver = Sat.create () in
    
  let clause1 = [[E.make 1]; [E.neg (E.make 1)]]
  in Sat.assume solver clause1 (); Sat.solve solver

(* Appel de la fonction principale *)
let () = match ex_solve_sat_problem () with
| Sat.Unsat _ -> Format.printf "unsat"
| Sat.Sat _ -> Format.printf "sat"

