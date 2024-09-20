(** Calcul des contraintes imposées par l'égalité u = v.
    On les ajoute dans l'ensemble des contraintes déjà existant. *)
val add_constraints : Prooftree.formula Constraints.t -> Prooftree.formula -> Prooftree.formula -> unit

(** Résolution des contraintes *)
val resolve : Prooftree.formula Constraints.t -> unit

(** Résolution d'une égalité u = v *)
val unify : Prooftree.formula -> Prooftree.formula -> (int * Prooftree.formula) list
