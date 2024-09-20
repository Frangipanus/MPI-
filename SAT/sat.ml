Random.init 0

let nb_test = 100000;;

type litteral = 
|X of int 
|NonX of int 

type fnc = litteral list list 




let simplifie_antagonistes (f : fnc) = 
  List.filter (fun x-> let b = ref true in  let _ = List.map(fun y -> match y with 
                                                          |X(a) -> if((List.mem (NonX(a)) x)) then b:= false
                                                          |NonX(a) -> if(List.mem (X(a)) x) then b:=false                                 
                                                        ) x in 
                                                          !b) f

let nouveau_lit_isole (f : fnc) = 
  let rec aux lst = 
    match lst with
    |[] -> X(-1)
    |h::q-> match h with |x::[]->x 
                         |_-> aux q
        
      in 
  aux f


let print_fnc (f : fnc) = 
  List.iter (fun x-> Printf.printf "["; List.iter (fun y -> match y with 
                                                              |X(a)-> Printf.printf"%d " a
                                                              |NonX(a)->  Printf.printf"Non%d " a) x; Printf.printf"]\n")f



let simplification (l : litteral)(f : fnc) : fnc = 
  match l with 
  |X(a) -> begin let number = a in
  List.filter_map (fun x-> if(List.mem l x) then None else Some(List.filter (fun y -> match y with
                                                                                      |X(a)-> true
                                                                                      |NonX(a) -> if(a = number) then false else true) x)) f

  end
  |NonX(a) -> begin let number = a in 
  List.filter_map (fun x-> if(List.mem l x) then None else Some(List.filter (fun y -> match y with
                                                                                      |NonX(a)-> true
                                                                             |X(a) -> if(a = number) then false else true) x)) f
  end

  let print_tab (tab) = 
    Printf.printf"[";
    List.iter (fun x-> match x with 
                        |X(a)-> Printf.printf"%d " a
                        |NonX(a)->  Printf.printf"Non%d " a) tab; Printf.printf"]\n"
  

let rec propagation_aux (f : fnc) (lst : litteral list) = 
  let lit = nouveau_lit_isole f in 
  match lit with 
  |X(a) ->  if(a = -1) then (f, lst) else propagation_aux (simplification lit f) (lit :: lst)
  |NonX(a) -> if(a = -1) then (f, lst) else propagation_aux (simplification lit f) (lit :: lst)

let propagation (f : fnc) = 
  propagation_aux f []
  
let variables (f : fnc) = 
  let acc = List.flatten f in
  let res = ref [] in 
  List.iter (fun x -> match x with 
                              |X(a)-> if(List.mem a !res) then () else res := a::(!res)
                              |NonX(a) -> if(List.mem a !res) then () else res := a::(!res) )acc;
  !res

let deduction (x : int)(f : fnc) =
  let acc = simplification (NonX(x)) (([NonX(x)])::f) in 
  if(List.mem [] acc) then 1 else 
      begin let acc2 = simplification (X(x)) ([X(x)]::f) in 
  
      
      if(List.mem [] acc2) then (-1) else 0  
end

let propagation2 (f : fnc) = 
  let var = variables f in 
  let (fprime, necessaire) = propagation f in 
  let nec = ref necessaire  in 
  List.iter (fun x -> if((List.mem (X(x)) !nec) || (List.mem (NonX(x)) !nec)) then () else begin 
                                                      let acc = deduction x fprime in
                                                      if (acc = 1) then nec := X(x)::(!nec) 
                                                      else begin
                                                          if(acc = -1) then nec := NonX(x)::(!nec)
                                                          else()
                                                      end
                                                    end) var;
  !nec

let test_clause (clause : litteral list) table = 
  let rec aux  cl = 
    match cl with
    |[]->false
    |h::q -> match h with 
            |X(a)-> if((Hashtbl.find table a) = true) then true else aux q 
            |NonX(a) -> if((Hashtbl.find table a) = false) then true else aux q
in 
aux clause

let test_fnc (fn : fnc) table = 
  let rec aux  f = 
    match f with
    |[]->true
    |h::q ->  if(test_clause h table) then aux q else false
in 
aux fn


let resoudre (f : fnc) =
  let var = variables f in
  let res = propagation2 f in

  let table = Hashtbl.create 42 in
  List.iter (fun x-> Hashtbl.add table x true) var;
  List.iter (fun x -> match x with 
                      |X(a) -> Hashtbl.add table a true
                      |NonX(a) -> Hashtbl.add table a false ) res;
  
  test_fnc  f table
              


let random_litteral (grandn : int) = 
  let i = Random.int grandn in 
  let b = Random.int 2 in 
  if(b = 0) then X(i) else NonX(i)


let random_clause (grandn : int) (p : int) = 
  let rec aux n cl = 
    if n = 0 then cl 
    else aux (n-1) ((random_litteral grandn)::cl)
  in 
  aux p []

let random_instance (grandn : int) (n : int) (p : int) = 
  let rec aux k fnc = 
    if k = 0 then fnc 
    else aux (k-1) ((random_clause grandn p)::fnc)
  in 
  aux n []


let proportion_npsat (grandn : int) (p : int) (n : int) : float = 
  let cmp = ref 0 in 
  for i = 0 to nb_test -1 do 
    let fnc = random_instance grandn p n in 
    if(resoudre (fnc)) then 
        cmp := !cmp + 1
        
  done;

  Float.div (Float.of_int (!cmp)) (Float.of_int nb_test)




let fnc = [[X(1)]; [X(4); X(6); X(7)]; [NonX(1); NonX(6)]; [NonX(7)]];;
let fnc2 = [[X(1)];[NonX(1)]];;
let fnc3 = [[X(1); X(2)];[NonX(1); NonX(2)];[NonX(1); X(2)]; [X(1);NonX(2)] ];;
let fnc3 = simplification (X(4)) fnc2;;
let b = resoudre fnc;;

assert(resoudre fnc = true);;
assert(resoudre fnc2 == false);;
assert(resoudre fnc3 == false);;
let grandn = 10;;
let n = 100;;
let p = 1;;
let max_test = 9 ;;
Printf.printf "La proportion de sat pour N = %d, n = %d p = %d est %f\n" grandn n p (proportion_npsat grandn n p);;
Printf.printf "p\\n     1    |";;
let() = for k = 2 to max_test do 
  Printf.printf "     %d    |" k
done;
Printf.printf"\n";;
let() = for i = 1 to max_test do
  Printf.printf "%d   " i;
  for j = 1 to max_test do 
      Printf.printf"%f | " (proportion_npsat grandn j i)
  done;
  Printf.printf"\n"

done;;