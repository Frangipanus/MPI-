open Prooftree

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



let s7 = {context = [Disj(Var(0),Var(1)); Var(1)]; conclusion = Var(1)};;
let s6 = {context = [Disj(Var(0),Var(1)); Var(0)]; conclusion = Var(0)};;
let s5 = {context = [Disj(Var(0),Var(1)); Var(1)]; conclusion = Disj(Var(1), Var(0))};;
let s4 = {context = [Disj(Var(0),Var(1)); Var(0)]; conclusion = Disj(Var(1), Var(0))};;
let s3 = {context = [Disj(Var(0),Var(1))]; conclusion = Disj(Var(0), Var(1))};;
let s2 = {context = [Disj(Var(0),Var(1))]; conclusion = Disj(Var(1), Var(0))};;
let s1 = {context = []; conclusion = Arrow(Disj(Var(0),Var(1)), Disj(Var(1),Var(0)))};;
let t7 = {root = s7; node = Axiom};;
let t6 = {root = s6; node = Axiom};;
let t5 = {root = s5; node = OrIntroLeft(t7)};;
let t4 = {root = s4; node = OrIntroRight(t6)};;
let t3 = {root = s3; node = Axiom};;
let t2 = {root = s2; node = OrElim(t3, t4, t5)};;
let t1 = {root = s1; node = ArrowIntro(t2)};;
(*
assert(lj_checker t7);;
assert(lj_checker t6);;
assert(lj_checker t5);;
assert(lj_checker t4);;
assert(lj_checker t3);;
assert(lj_checker t2);;*)
assert(lj_checker t1);;
let() = Printf.printf "Fin des tests, rien a signaler\n";;
