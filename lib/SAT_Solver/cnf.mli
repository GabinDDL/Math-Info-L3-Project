type symbol = string
type literal = symbol * bool
type clause = literal list
type cnf = clause list
type assignment = symbol * bool
type model = assignment list

(* utility method for finding the assignment of a symbol within a model *)
val find_assignment : symbol -> model -> bool option

(* utility method for checking if a model satisfies a clause *)
val is_clause_sat : clause -> model -> bool

(* utility method for checking if a model satisfies a cnf *)
val is_cnf_sat : cnf -> model -> bool

(* utility method for extracting set of symbols in a cnf *)
val symbols_in_cnf : cnf -> symbol list

(* method to print model *)
val print_model : model -> unit

(* method to print cnf *)
val print_cnf : cnf -> unit
