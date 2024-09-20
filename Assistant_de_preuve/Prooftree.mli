(** Représentation des formules.
    Les variables sont numérotées par les entiers. *)
type formula =
	| Var of int
	| True
	| False
	| Conj of formula * formula
	| Disj of formula * formula
	| Arrow of formula * formula
	| Forall of int * formula
	| Exists of int * formula

(** α-équivalence entre deux formules *)
val alpha_equiv : formula -> formula -> bool

(** Représentation d'une substitution d'une variable par une formule *)
type substitution = int * formula

(** Substitution sans capture *)
val substitute : formula -> substitution -> formula

(** Contexte d'un séquent *)
type context = formula list

(** Représentation des séquents *)
type sequent =
	{
		context : context;
		conclusion : formula;
	}

(** Un arbre de preuve est donné par son séquent conclusion et par
    la règle de déduction qui permet d'obtenir ce séquent. La règle de
    déduction est l'un des constructeurs du type [proofnode], dont les
    paramètres sont les arbres de preuve prémisses. *)
type prooftree =
	{
		root : sequent;
		node : proofnode;
	}
and proofnode =
	| Axiom
	| ArrowIntro of prooftree
	| ArrowElim of prooftree * prooftree
	| OrIntroLeft of prooftree
	| OrIntroRight of prooftree
	| OrElim of prooftree * prooftree * prooftree
	| AndIntro of prooftree * prooftree
	| AndElimLeft of prooftree
	| AndElimRight of prooftree
	| Efqs of prooftree
	| TrueIntro
	| ForallIntro of prooftree
	| ForallElim of int * prooftree
	| ExistsIntro of int * prooftree
	| ExistsElim of prooftree
	| Absurd of prooftree

(** Fonction qui vérifie qu'un arbre de preuve est valide en logique classique. *)
val lk_checker : prooftree -> bool

(** Fonction qui vérifie qu'un arbre de preuve est valide en logique intuitionniste. *)
val lj_checker : prooftree -> bool
val is_not_free_var : formula -> int -> bool