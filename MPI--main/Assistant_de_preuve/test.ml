open Prooftree
(*
let f = Forall(0, Forall(1, Conj(Var(0),Var(3))));;
let g = Forall(0, Forall(3, Conj(Var(0),Var(3))));;
assert(not(alpha_equiv f g));;
assert(not(is_not_free_var f 3));;
assert(is_not_free_var f 2);;
assert(is_not_free_var g 3);;
let seq1 = {context = [Var(1); Var(2)]; conclusion = Var(1)};;
let tree1 = {root = seq1; node = Axiom};;
let seq2 = {context = [Var(1); Var(2)]; conclusion = Var(3)};;
let tree2 = {root = seq2; node = Axiom};;
assert(lj_checker tree1);;
assert(not(lj_checker tree2));;
*)
(*****************Preuve de la commutation du ou, qui vérifie si le programme maitrise l'aloha équivalence**********)

let s7 = {context = [Disj(Var(0),Var(1)); Var(1);Forall(4,Var(4))]; conclusion = Var(1)};;
let s6 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(0)]; conclusion = Var(0)};;
let s5 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(1)]; conclusion = Disj(Var(1), Var(0))};;
let s4 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(0)]; conclusion = Disj(Var(1), Var(0))};;
let s3 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1))]; conclusion = Disj(Var(0), Var(1))};;
let s2 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1))]; conclusion = Disj(Var(1), Var(0))};;
let s1 = {context = [Forall(2,Var(2))]; conclusion = Arrow(Disj(Var(0),Var(1)), Disj(Var(1),Var(0)))};;
let t7 = {root = s7; node = Axiom};;
let t6 = {root = s6; node = Axiom};;
let t5 = {root = s5; node = OrIntroLeft(t7)};;
let t4 = {root = s4; node = OrIntroRight(t6)};;
let t3 = {root = s3; node = Axiom};;
let t2 = {root = s2; node = OrElim(t3, t4, t5)};;
let t1 = {root = s1; node = ArrowIntro(t2)};;
assert (if not(lj_checker t1) then print_endline "Héhé ça marche pas si bien les table de Hachage ;)"; lj_checker t1);;

(****************Exemple: ces séquents sont les mêmes*************)
let c1 = [Forall(3,Var(3));Disj(Var(0),Var(1)); Var(1)];;
let c2 = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(1)];;

(*****************Test 2 ************************)
let s7 = {context = [Disj(Var(0),Var(1)); Var(1);Forall(4,Var(4))]; conclusion = Var(1)};;
let s6 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(0)]; conclusion = Var(0)};;
let s5 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(1)]; conclusion = Disj(Var(1), Var(0))};;
let s4 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(0)]; conclusion = Disj(Var(1), Var(0))};;
let s3 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1))]; conclusion = Disj(Var(0), Var(1))};;
let s2 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1))]; conclusion = Disj(Var(1), Var(0))};;
let s1 = {context = [Forall(2,Var(2))]; conclusion = Arrow(Disj(Var(0),Var(1)), Disj(Var(1),Var(0)))};;
let t7 = {root = s7; node = Axiom};;
let t6 = {root = s6; node = Axiom};;
let t5 = {root = s5; node = OrIntroLeft(t7)};;
let t4 = {root = s4; node = OrIntroRight(t6)};;
let t3 = {root = s3; node = Axiom};;
let t2 = {root = s2; node = OrElim(t3, t4, t5)};;
let t1 = {root = s1; node = ArrowIntro(t2)};;
assert (if not(lj_checker t1) then print_endline "Les éléments du séquents peuvent commuter"; lj_checker t1);;

(*****************Test 3 ************************)
let s7 = {context = [Forall(4,Var(4));Disj(Var(0),Var(1)); Var(1)]; conclusion = Var(1)};;
let s6 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(0)]; conclusion = Var(0)};;
let s5 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(1); Var(1)]; conclusion = Disj(Var(1), Var(0))};;
let s4 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(0)]; conclusion = Disj(Var(1), Var(0))};;
let s3 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1))]; conclusion = Disj(Var(0), Var(1))};;
let s2 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1))]; conclusion = Disj(Var(1), Var(0))};;
let s1 = {context = [Forall(2,Var(2))]; conclusion = Arrow(Disj(Var(0),Var(1)), Disj(Var(1),Var(0)))};;
let t7 = {root = s7; node = Axiom};;
let t6 = {root = s6; node = Axiom};;
let t5 = {root = s5; node = OrIntroLeft(t7)};;
let t4 = {root = s4; node = OrIntroRight(t6)};;
let t3 = {root = s3; node = Axiom};;
let t2 = {root = s2; node = OrElim(t3, t4, t5)};;
let t1 = {root = s1; node = ArrowIntro(t2)};;
assert (if lj_checker t1 then print_endline "Une hypothèse à été dupliqué donc l'arbre n'est pas recevable"; not(lj_checker t1))

(*****************Test 4 ************************)
let s7 = {context = [Disj(Var(0),Var(1)); Var(1);Forall(4,Var(4))]; conclusion = Var(1)};;
let s6 = {context = [Disj(Var(0),Var(1)); Var(0)]; conclusion = Var(0)};;
let s5 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(1)]; conclusion = Disj(Var(1), Var(0))};;
let s4 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(0)]; conclusion = Disj(Var(1), Var(0))};;
let s3 = {context = [Forall(2,Var(2));Conj(Var(4), Var(7));Disj(Var(0),Var(1))]; conclusion = Disj(Var(0), Var(1))};;
let s2 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1))]; conclusion = Disj(Var(1), Var(0))};;
let s1 = {context = [Forall(2,Var(2))]; conclusion = Arrow(Disj(Var(0),Var(1)), Disj(Var(1),Var(0)))};;
let t7 = {root = s7; node = Axiom};;
let t6 = {root = s6; node = Axiom};;
let t5 = {root = s5; node = ArrowIntro(t7)};;
let t4 = {root = s4; node = OrIntroRight(t6)};;
let t3 = {root = s3; node = Axiom};;
let t2 = {root = s2; node = OrElim(t3, t4, t5)};;
let t1 = {root = s1; node = ArrowIntro(t2)};;
assert (if lj_checker t1 then print_endline "Cet arbre c'est du grand nimporte quoi"; not(lj_checker t1))

(*****************Test 5 ************************)
let s7 = {context = [Disj(Var(0),Var(1)); Var(1);Forall(4,Var(4))]; conclusion = Var(1)};;
let s6 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(0)]; conclusion = Var(0)};;
let s5 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(1)]; conclusion = Disj(Var(1), Var(0))};;
let s4 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(0)]; conclusion = Disj(Var(1), Var(0))};;
let s3 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1))]; conclusion = Disj(Var(0), Var(1))};;
let s2 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1))]; conclusion = Disj(Var(1), Var(0))};;
let s1 = {context = [Forall(2,Var(2))]; conclusion = Arrow(Disj(Var(0),Var(1)), Disj(Var(1),Var(0)))};;
let t7 = {root = s7; node = Axiom};;
let t6 = {root = s6; node = Axiom};;
let t5 = {root = s5; node = OrIntroRight(t7)};;
let t4 = {root = s4; node = OrIntroLeft(t6)};;
let t3 = {root = s3; node = Axiom};;
let t2 = {root = s2; node = OrElim(t3, t4, t5)};;
let t1 = {root = s1; node = ArrowIntro(t2)};;
assert (if lj_checker t1 then print_endline "Connais tu ta gauche et ta droite?"; not(lj_checker t1))

(*****************Test 6 ************************)
let s7 = {context = [Disj(Var(0),Var(1)); Var(1);Forall(4,Var(4))]; conclusion = Var(1)};;
let s6 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(0)]; conclusion = Var(0)};;
let s5 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(1)]; conclusion = Disj(Var(1), Var(0))};;
let s4 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1)); Var(0)]; conclusion = Disj(Var(1), Var(0))};;
let s3 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1))]; conclusion = Disj(Var(0), Var(1))};;
let s2 = {context = [Forall(2,Var(2));Disj(Var(0),Var(1))]; conclusion = Disj(Var(1), Var(0))};;
let s1 = {context = [Forall(2,Var(2))]; conclusion = Arrow(Disj(Var(0),Var(1)), Disj(Var(1),Var(0)))};;
let t7 = {root = s7; node = Axiom};;
let t6 = {root = s6; node = Axiom};;
let t5 = {root = s5; node = OrIntroLeft(t7)};;
let t4 = {root = s4; node = OrIntroRight(t6)};;
let t3 = {root = s3; node = Axiom};;
let t2 = {root = s2; node = OrElim(t3, t4, t5)};;
let t1 = {root = s1; node = ArrowIntro(t2)};;
assert (if lj_checker t1 then print_endline "L'affaiblissement n'est pas légal"; not(lj_checker t1))


let() = Printf.printf "Fin des tests, rien a signaler\n";;
