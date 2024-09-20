module IntMap = Map.Make(Int)
type formula =
	| Var of int
	| True
	| False
	| Conj of formula * formula
	| Disj of formula * formula
	| Arrow of formula * formula
	| Forall of int * formula
	| Exists of int * formula
type substitution = int * formula

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

(*-------------------FIN DES TYPES--------------------*)
let alpha_equiv f1 f2 = (*On compare f2 a f1*)
  let rec aux f1 f2 table tablep haut= 
    match f1, f2 with
    | Var(x), Var(y) ->  if (IntMap.mem x table) then ((IntMap.mem y tablep) && ((IntMap.find y tablep) = (IntMap.find x table))) else (not(IntMap.mem y tablep) &&(x=y))
    | True, True -> true
    | False, False -> true
    | Conj(x1,x2), Conj(y1,y2) -> (aux x1 y1 ( table) tablep (haut+1)) && (aux x2 y2  table tablep (haut+1))
    | Disj(x1,x2), Disj(y1,y2) ->(aux x1 y1 ( table) tablep (haut+1)) && (aux x2 y2 table tablep (haut+1))
    | Arrow(x1,x2), Arrow(y1,y2) -> (aux x1 y1 (table) tablep (haut+1)) && (aux x2 y2 table tablep (haut+1))
    | Forall(x, g1), Forall(y, g2) ->begin  let table2 = IntMap.add x (haut) table in 
                                            let tablep2 = IntMap.add y (haut) tablep
                                            in aux g1 g2 table2 tablep2 (haut+1) 
                                    end 
    | Exists(x, g1), Exists(y, g2) ->begin  let table2 = IntMap.add x (haut) (table) in 
                                             let tablep2 = IntMap.add y (haut) tablep
                                          in aux g1 g2 table2 tablep2 (haut+1) 
                                    end     
    |_ -> (Printf.printf "pas normal"; false )
    in
    
  let t = IntMap.empty in
  let t2 = IntMap.empty in
  aux f1 f2 t t2 0

let max x1 x2 = 
  if(x1 < x2) then x2 else x1

let rec max_var f = 
  match f with
  |Var(x) -> x
  |Conj(x1, x2) -> max (max_var x1) (max_var x2)
  |Disj(x1, x2) -> max (max_var x1) (max_var x2)
  |Arrow(x1, x2) -> max (max_var x1) (max_var x2)
  |Forall(x1, x2) -> max ( x1) (max_var x2)
  |Exists(x1, x2) -> max ( x1) (max_var x2)
  |_ -> 0


let rec substitute f1 sub = 
  let deb = (max_var f1) + 1 in 
  let (a_mod, remp) = sub in 
  match f1 with
  |Var(x) -> if(x= a_mod) then remp else f1
  |Conj(x1, x2) -> Conj(substitute x1 sub, substitute x2 sub)
  |Disj(x1, x2) -> Disj(substitute x1 sub, substitute x2 sub) 
  |Arrow(x1, x2) -> Arrow(substitute x1 sub, substitute x2 sub)
  |Forall(x1, f) -> if(x1 == a_mod) then f1 else Forall(deb, substitute ( substitute f (x1, Var(deb))) (a_mod, remp))
  |Exists(x1, f) -> if(x1 == a_mod) then f1 else Exists(deb, substitute ( substitute f (x1, Var(deb))) (a_mod, remp))
  |_ -> f1


let same_hyp c1 c2 = (*Where c1 and c2 context*)
  if (List.length c1 != List.length c2) then false 
  else let length = List.length c1 in
  let t1 = Hashtbl.create (length) in 
  let t2 = Hashtbl.create (length) in
  let rec aux l t = match l with
    |[] -> ()
    |h::q -> begin 
      try 
        let acc = Hashtbl.find t h in 
        Hashtbl.add t h (acc+1);
        aux q t
      with 
      |Not_found -> let acc = 0 in Hashtbl.add t h (acc+1);
      aux q t
      
  end
  in aux c1 t1;
  aux c2 t2;
  let rec aux2 l = match l with
    |[] -> true
    |h::q -> begin
          let acc = Hashtbl.find t1 h in 
          try 
            let acc2 = Hashtbl.find t2 h in 
            if (acc = acc2) then aux2 q else false 
          with 
            |Not_found -> false
    end
  in 
  aux2 c1


let rec is_not_free_var f x =  
  match f with 
  |Var(y) -> y!=x
  |Conj(x1, x2) ->  (is_not_free_var x1 x) && (is_not_free_var x2 x)
  |Disj(x1, x2) -> (is_not_free_var x1 x) && (is_not_free_var x2 x)
  |Arrow(x1, x2) -> (is_not_free_var x1 x) && (is_not_free_var x2 x)
  |Forall(x1, x2) -> if (x1 == x) then true else (is_not_free_var x2 x)
  |Exists(x1, x2) -> if (x1 == x) then true else (is_not_free_var x2 x)
  |_ -> true


let rec checker allow_abs f = 
  let (===) = same_hyp in 
  let (=*=) = alpha_equiv in 
  match f.node with
  |Axiom -> List.mem (f.root.conclusion) (f.root.context)
  |TrueIntro -> true
  |Efqs(f2) -> (False = f.root.conclusion) && (checker allow_abs f2)
  |ArrowIntro(f2) -> begin match f.root.conclusion with
                        |Arrow(g1,g2) -> (g2 =*= f2.root.conclusion) && (List.mem g1 f2.root.context)&&
                        (g1::(f.root.context) === f2.root.context) && (checker allow_abs f2)
                        |_ -> (false)
                    end
  |ArrowElim(f1,f2) -> begin match f1.root.conclusion with
                        |Arrow(g1,g2) -> (f2.root.conclusion =*= g1) && (f.root.conclusion =*= g2)
                                          && (f.root.context === f1.root.context)
                                          && (f.root.context === f2.root.context)
                                          && (checker allow_abs f1)
                                          && (checker allow_abs f2)
                        |_ -> (false)

                    end
  |OrIntroLeft(f2) -> begin match f.root.conclusion with
                        |Disj(g1,g2) -> (g1 =*= f2.root.conclusion) && (f.root.context === f2.root.context) && (checker allow_abs f2)
                        |_ -> (false)
                    end
  |OrIntroRight(f2) -> begin match f.root.conclusion with
                        |Disj(g1,g2) -> (g2 =*= f2.root.conclusion) 
                        && (f.root.context === f2.root.context) && (checker allow_abs f2)
                        |_ ->  false
                    end
  |OrElim(f1,f2,f3) -> begin match f1.root.conclusion with 
                      | Disj(g1,g2) -> begin 
                                        (f3.root.conclusion =*= f.root.conclusion) && (f.root.conclusion =*= f2.root.conclusion)
                                        && (List.mem g1 f2.root.context) &&(List.mem g2 f3.root.context)
                                        && ( f.root.context === f1.root.context) 
                                        && ( (g1::f.root.context) === f2.root.context) 
                                        && ( (g2::f.root.context) === f3.root.context)
                                        &&(checker allow_abs f1) &&(checker allow_abs f2) &&(checker allow_abs f3)
                                      end 
                      |_ -> false
                  end
  |AndIntro(f1,f2) -> begin match f.root.conclusion with 
                      |Conj(g1,g2) ->   
                                  (f1.root.conclusion =*= g1) && (f2.root.conclusion =*= g2)
                                  && (f1.root.context === f.root.context)
                                  && (f2.root.context === f.root.context)
                                  && (checker allow_abs f1 ) && (checker allow_abs f2)
                                
                      |_ -> false
                      end
  |AndElimLeft(f1) -> begin match f1.root.conclusion with
                      |Conj(g1,g2) -> (g1 =*= f.root.conclusion) && (f1.root.context === f.root.context) && (checker allow_abs f1)
                      |_ -> false
                    end
  |AndElimRight(f1) -> begin match f1.root.conclusion with
                      |Conj(g1,g2) -> (g2 =*= f.root.conclusion) && (f1.root.context === f.root.context) && (checker allow_abs f1)
                      |_ ->  false
                    end
  |ForallIntro(f1) -> begin match f.root.conclusion with 
                      |Forall(x, g) -> (List.for_all (fun formule -> is_not_free_var formule x) f1.root.context)
                                     &&(g =*= f1.root.conclusion)
                                     &&(f.root.context === f1.root.context)
                                     &&(checker allow_abs f1)
                      |_ -> false
                    end 
  |ForallElim(t, f1) -> begin match f1.root.conclusion with
                      |Forall(x, g) -> (substitute g (x, Var(t)) =*= f.root.conclusion)
                                       &&(f.root.context === f1.root.context)
                                       &&(checker allow_abs f1)
                      |_ -> false
                    end 
  |ExistsIntro(t,f1) -> begin match f.root.conclusion with
                      |Exists(x,g) -> (f1.root.conclusion =*= substitute g (x, Var(t)))
                                      &&(f.root.context === f1.root.context)
                                      &&(checker allow_abs f1)
                      |_ -> false
                    end
  |ExistsElim(f1) -> begin match f1.root.conclusion with
                      |Exists(x, g) -> begin 
                                          let maxi = ref 0 in 
                                          List.iter (fun form -> maxi := max (!maxi) (max_var form)) f.root.context;
                                          maxi := 1+ max (!maxi) (max_var g);
                                          (f.root.conclusion =*= substitute g (x, Var((!maxi))))
                                        &&(f.root.context === f1.root.context)
                                        &&(checker allow_abs f1)
                                       end 
                      |_ -> false
                    end 
  |Absurd(f1) -> match f1.root.conclusion with
                  |False -> (allow_abs)
                          &&(f1.root.context === (Arrow(f.root.conclusion,False))::f.root.context)
                          &&(checker allow_abs f1)
                  |_ -> false

  
let lj_checker f = 
  checker false f

let lk_checker f = 
  checker true f


  (***************)
  (*La deuxieme est quasi identique a ceci pres qu'on autorise le absurde....*)