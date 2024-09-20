(* Ensemble qui associe des entiers à des éléments de type 'a *)
type 'a t
(* création *)
val create: unit -> 'a t
(* ajout d'un singleton *)
val add: 'a t -> int -> 'a -> unit
(* remplacement de la valeur associée à un ensemble existant *)
val replace: 'a t -> int -> 'a -> unit
(* recherche de la valeur associée à un ensemble existant *)
val find: 'a t -> int -> 'a
(* fusion de deux ensembles existant, la valeur associée à l'union est le 'b donné*)
val merge: 'a t -> int -> int -> 'a -> unit
(* test d'équivalence de deux variables *)
val equiv: 'a t -> int -> int -> bool
(* itérations des classes d'équivalence (par leur représentants) *)
val iter_repr: (int -> 'a -> unit) -> 'a t -> unit
(* conversion de la table en une liste d'association *)
val to_list: 'a t -> (int * 'a) list

