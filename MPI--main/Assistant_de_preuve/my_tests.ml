
(* alpha equiv test *)
let () =
  let test_form1 = Prooftree.Conj(Var(0),Var(1)) in
  let test_form2 = Prooftree.Conj(Var(2),Var(1)) in
  assert (not (Prooftree.alpha_equiv test_form1 test_form2))
;;

let () =
  let test_form1 = Prooftree.Exists(42, Conj(Var(42),Var(1))) in
  let test_form2 = Prooftree.Exists(144, Conj(Var(144),Var(1))) in
  assert ((Prooftree.alpha_equiv test_form1 test_form2))
;;

let () =
  let test_form1 = Prooftree.( 
    Forall(1337, Arrow(Forall(9, Arrow(Var(9), Var(1337))), Var(1337)))
  ) in
  let test_form2 = Prooftree.( 
    Forall(1336, Arrow(Forall(10, Arrow(Var(10), Var(1336))), Var(1336)))
  ) in
  assert  (Prooftree.alpha_equiv test_form1 test_form2)
;;


let () =
  let test_form1 = Prooftree.( 
    Exists(1337, Arrow(Forall(9, Arrow(Var(9), Var(1337))), Var(1337)))
  ) in
  let test_form2 = Prooftree.( 
    Exists(1336, Arrow(Forall(10, Arrow(Var(10), Var(1336))), Var(1336)))
  ) in
  assert  (Prooftree.alpha_equiv test_form1 test_form2)
;;


let () =
  let test_form1 = Prooftree.( 
    Exists(1337, Arrow(Forall(9, Arrow(Var(9), Var(1337))), Var(1337)))
  ) in
  let test_form2 = Prooftree.( 
    Forall(1336, Arrow(Exists(10, Arrow(Var(10), Var(1336))), Var(1336)))
  ) in
  assert  (not (Prooftree.alpha_equiv test_form1 test_form2))
;;


(* substitution test *)
let () = 
  let p = Prooftree.Var(5) in
  let q = Prooftree.Var(6) in
  Prooftree.(
    assert(
      alpha_equiv
        (Arrow(Arrow(Arrow(p, False), p), p))
        (substitute 
          (Arrow(Arrow(Arrow(p, q), p), p))
          (6, False)
        )
    )
  )
;;
let () =
  Prooftree.(
    assert(
      alpha_equiv
        (Disj(Arrow(Var(1), Arrow(Var(6), False)) , Arrow(Var(6), False)))
        (substitute 
          (Disj(Arrow(Var(1), Var(2)) , Var(2)))
          (2, Arrow(Var(6), False))
        )
    )
  )
;;
let () =
  Prooftree.(
    assert(
      alpha_equiv
        (Disj(Arrow(Var(1), Arrow(Var(2), False)) , Arrow(Var(2), False)))
        (substitute 
          (Disj(Arrow(Var(1), Var(2)) , Var(2)))
          (2, Arrow(Var(2), False))
        )
    )
  )
;;
let () =
  Prooftree.(
    assert(
      alpha_equiv
      (Arrow(Arrow(Var(42), False) , Exists(5, Arrow(Var(5), Var(9)))))
      (substitute 
        (Arrow(Var(5) , Exists(5, Arrow(Var(5), Var(9)))))
        (5, Arrow(Var(42), False))
      )
    )
  )
;;
let () =
  Prooftree.(
    assert(
      alpha_equiv
      (Arrow(Var(5) , Exists(1, Arrow(Var(1), Arrow(Var(5), False)))))
      (substitute 
        (Arrow(Var(5) , Exists(5, Arrow(Var(5), Var(9)))))
        (9, Arrow(Var(5), False))
     ))
  )
;;



let rec greatest_unused_variable1 formuleF = 
  Prooftree.(
    match formuleF with
    | True | False  -> 0 (* no variable in a constat *)
    | Var(i) -> i
    | Disj(g, d)
    | Conj(g, d)
    | Arrow(g, d) -> 	max (greatest_unused_variable1 g) 
                          (greatest_unused_variable1 d)
    | Exists(bound, phi) | Forall(bound, phi) -> 
      max (greatest_unused_variable1 phi)
      (bound)
  ) 
;;

let tiers_exclu phi =
  Prooftree.(
	let tiers = Disj(phi, Arrow(phi, False)) in
	let cont_0 = [] in
	let cont_1 = Arrow(tiers, False) :: cont_0 in
	let cont_2 = phi :: cont_1 in
	{
		root = {context = cont_0; conclusion = tiers};
		node = Absurd({
			root = {context = cont_1; conclusion = False};
			node = ArrowElim({
				root = {context = cont_1; conclusion = Arrow(tiers, False)};
				node = Axiom
			}, {
				root = {context = cont_1; conclusion = tiers};
				node = OrIntroRight({
					root = {context = cont_1; conclusion = Arrow(phi, False)};
					node = ArrowIntro({
						root = {context = cont_2; conclusion = False};
						node = ArrowElim({
							root = {context = cont_2; conclusion = Arrow(tiers, False)};
							node = Axiom
						}, {
							root = {context = cont_2; conclusion = tiers};
							node = OrIntroLeft({
								root = {context = cont_2; conclusion = phi};
								node = Axiom
							})
						})
					})
				})
			})
		})
	}
  )
;;

let drinking_principal phi =
	let x = 1 in
	let t = 1+ greatest_unused_variable1 phi in
	let u = t+1 in
	let w = u+1 in
  Prooftree.(
	let phi_x_sub_t = substitute phi (x, Var(t)) in
	let phi_x_sub_u = substitute phi (x, Var(u)) in
	let phi_x_sub_w = substitute phi (x, Var(w)) in
	{
		root = {context = []; conclusion = Exists(x, Arrow(phi, Forall(x, phi)))};
		node = OrElim(
			tiers_exclu (Forall(x, phi))
		,{
			root = {context = [Forall(x, phi)]; conclusion = Exists(x, Arrow(phi, Forall(x, phi)))};
			node = ExistsIntro(t, {
				root = {context = [Forall(x, phi)]; conclusion = Arrow(phi_x_sub_t, Forall(x, phi))};
				node = ArrowIntro({
					root = {context = [phi_x_sub_t; Forall(x, phi)]; conclusion = Forall(x, phi)};
					node = Axiom
				})
			})
		},{
			root = {context = [Arrow(Forall(x, phi), False)]; conclusion = Exists(x, Arrow(phi, Forall(x, phi)))};
			node = ArrowElim({
				root = {context = [Arrow(Forall(x, phi), False)]; 
								conclusion = Arrow(Exists(x, Arrow(phi, False)), Exists(x, Arrow(phi, Forall(x, phi))))};
				node = ArrowIntro({
					root = {context = [Exists(x, Arrow(phi, False)); Arrow(Forall(x, phi), False)];
									conclusion = Exists(x, Arrow(phi, Forall(x, phi)))};
					node = ArrowElim({
						root = {context = [Exists(x, Arrow(phi, False)); Arrow(Forall(x, phi), False)];
										conclusion = Arrow(Arrow(phi_x_sub_w, False), Exists(x, Arrow(phi, Forall(x, phi))) )};
						node = ArrowIntro({
							root = {context = [Arrow(phi_x_sub_w, False); Exists(x, Arrow(phi, False)); Arrow(Forall(x, phi), False)];
											conclusion = Exists(x, Arrow(phi, Forall(x, phi)) )};
							node = ExistsIntro(w, {
								root = {context = [Arrow(phi_x_sub_w, False); Exists(x, Arrow(phi, False)); Arrow(Forall(x, phi), False)];
												conclusion = Arrow(phi_x_sub_w, Forall(x, phi))};
								node = ArrowIntro({
									root = {context = [phi_x_sub_w; Arrow(phi_x_sub_w, False); Exists(x, Arrow(phi, False)); Arrow(Forall(x, phi), False)];
													conclusion = Forall(x, phi)};
									node = Efqs({
										root = {context = [phi_x_sub_w; Arrow(phi_x_sub_w, False); Exists(x, Arrow(phi, False)); Arrow(Forall(x, phi), False)];
														conclusion = False};
										node = ArrowElim({
											root = {context = [phi_x_sub_w; Arrow(phi_x_sub_w, False); Exists(x, Arrow(phi, False)); Arrow(Forall(x, phi), False)];
															conclusion = Arrow(phi_x_sub_w, False)};
											node = Axiom
										}, {
											root = {context = [phi_x_sub_w; Arrow(phi_x_sub_w, False); Exists(x, Arrow(phi, False)); Arrow(Forall(x, phi), False)];
															conclusion = phi_x_sub_w};
											node = Axiom
										})
									})
								})
							})
						})
					}, {
						root = {context = [Exists(x, Arrow(phi, False)); Arrow(Forall(x, phi), False)];
										conclusion = Arrow(phi_x_sub_w, False)};
						node = ExistsElim(w, {
							root = {context = [Exists(x, Arrow(phi, False)); Arrow(Forall(x, phi), False)];
											conclusion = Exists(x, Arrow(phi, False))};
							node = Axiom
						})
					})
				})
			}, {
				root = {context = [Arrow(Forall(x, phi), False)]; conclusion = Exists(x, Arrow(phi, False))};
				node = Absurd({
					root = {context = [Arrow(Exists(x, Arrow(phi, False)), False); Arrow(Forall(x, phi), False)]; 
									conclusion = False};
					node = ArrowElim({
						root = {context = [Arrow(Exists(x, Arrow(phi, False)), False); Arrow(Forall(x, phi), False)]; 
										conclusion = Arrow(Forall(x, phi), False)};
						node = Axiom
					},{
						root = {context = [Arrow(Exists(x, Arrow(phi, False)), False); Arrow(Forall(x, phi), False)]; 
										conclusion = Forall(u, phi_x_sub_u)};
						node = ForallIntro({
							root = {context = [Arrow(Exists(x, Arrow(phi, False)), False); Arrow(Forall(x, phi), False)]; 
											conclusion = phi_x_sub_u };
							node = Absurd({
								root = {context = [Arrow(phi_x_sub_u, False); Arrow(Exists(x, Arrow(phi, False)), False); Arrow(Forall(x, phi), False)]; 
												conclusion = False};
								node = ArrowElim({
									root = {context = [Arrow(phi_x_sub_u, False); Arrow(Exists(x, Arrow(phi, False)), False); Arrow(Forall(x, phi), False)]; 
													conclusion = Arrow(Exists(x, Arrow(phi, False)), False)};
									node = Axiom
								}, {
									root = {context = [Arrow(phi_x_sub_u, False); Arrow(Exists(x, Arrow(phi, False)), False); Arrow(Forall(x, phi), False)]; 
													conclusion = Exists(x, Arrow(phi, False))};
									node = ExistsIntro(u, {
										root = {context = [Arrow(phi_x_sub_u, False); Arrow(Exists(x, Arrow(phi, False)), False); Arrow(Forall(x, phi), False)]; 
														conclusion = Arrow(phi_x_sub_u, False)};
										node = Axiom
									})
								})
							})
						})
					})
				})
			})
		})
	}
  )
;;


let () = (* tiers exclu tests *)
  let aorb = tiers_exclu (Disj(Var(0), Var(1))) in
  assert(Prooftree.lk_checker aorb);

  let aandb = tiers_exclu (Conj(Var(0), Var(1))) in
  assert(Prooftree.lk_checker aandb);

  let ex_x = tiers_exclu (Exists(5, Var(5))) in
  assert(Prooftree.lk_checker ex_x);

  let ex_x_arroy_y = tiers_exclu (Exists(5, Arrow(Var(5), Var(6)))) in
  assert(Prooftree.lk_checker ex_x_arroy_y);

;;

let () = (* Drinking Principal *)
  assert(Prooftree.lk_checker (drinking_principal (Var(5))));
  assert(Prooftree.lk_checker (drinking_principal (Var(0))));

  let aorb = drinking_principal (Disj(Var(0), Var(1))) in
  assert(Prooftree.lk_checker aorb);

  let aandb = drinking_principal (Conj(Var(0), Var(1))) in
  assert(Prooftree.lk_checker aandb);

  let ex_x = drinking_principal (Exists(5, Var(5))) in
  assert(Prooftree.lk_checker ex_x);

  let ex_x_arroy_y = drinking_principal (Exists(5, Arrow(Var(5), Var(1)))) in
  assert(Prooftree.lk_checker ex_x_arroy_y);
;;

Printf.printf "Tests Compleated!\n"


