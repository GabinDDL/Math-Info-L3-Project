open Cnf

(* extracts a literal that appears as a unit clause in a given cnf *)
val find_pure_symbol : cnf -> assignment option

(* extracts a literal that appears as a unit clause in a given cnf *)
val find_unit_clause : cnf -> assignment option

(* returns a cnf with instances of a symbol and satisfied clauses removed *)
val cleanup : cnf -> symbol -> cnf

(* main method to run dpll algorithm: returns (bool, model) where bool
 * = 'is cnf satisfied by model' *)
val dpll : cnf -> bool * model
