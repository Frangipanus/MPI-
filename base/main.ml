open Trie



let mem text t = 
  let n = String.length text in
  let rec aux i tr = 
    if i>= n then false 
    else (
    if text.[i] = 'A' then 
      match tr.a with 
      |Empty -> false
      |Node(b, tr2) -> if (i=n-1) then b else aux (i+1) (tr2) 
else if text.[i] = 'C' then 
  match tr.c with 
  |Empty -> false
  |Node(b, tr2) -> if (i=n-1) then b else aux (i+1) (tr2) 
else if text.[i] = 'G' then 
  match tr.g with 
  |Empty -> false
  |Node(b, tr2) -> if (i=n-1) then b else aux (i+1) (tr2) 
else 
  match tr.t with 
  |Empty -> false
  |Node(b, tr2) -> if (i=n-1) then b else aux (i+1) (tr2) 
    )
  in
  aux 0 t


let rec aux_q2 tr text i acc = 
  if (i < String.length text) then( 
    if text.[i] = 'A' then 
      match tr.a with 
      |Empty -> (false,acc)
      |Node(b,tr2) -> if b then (b, acc ^ "A") else aux_q2 tr2 text (i+1) (acc ^ "A")
else if text.[i] = 'C' then 
  match tr.c with 
  |Empty -> (false,acc)
  |Node(b,tr2) -> if b then (b, acc ^ "C") else aux_q2 tr2 text (i+1) (acc ^ "C")
else if text.[i] = 'G' then 
  match tr.g with 
  |Empty -> (false,acc)
  |Node(b,tr2) -> if b then (b, acc ^ "G") else aux_q2 tr2 text (i+1) (acc ^ "G")
else 
  match tr.t with 
  |Empty -> (false,acc)
  |Node(b,tr2) -> if b then (b, acc ^ "T") else aux_q2 tr2 text (i+1) (acc ^ "T")
    
  )
else (false, acc)

let find_first text trie = 
  let n = String.length text in 
  let rec aux i = 
    if i >= n then None 
    else let (boolacc, stracc) = aux_q2 trie text i "" in 
    if boolacc then Some(i, stracc) else aux (i+1)
  in 
  aux 0
  

let rec aux_q5 text tr lst i acc = 
  if (i < String.length text) then( 
    if text.[i] = 'A' then 
      match tr.a with 
      |Empty -> lst
      |Node(b,tr2) -> if b then aux_q5 text tr2 ((b, acc ^ "A")::lst) (i+1) (acc ^"A") else aux_q5 text tr2 lst (i+1) (acc ^ "A")
else  if text.[i] = 'C' then 
  match tr.c with 
  |Empty -> lst
  |Node(b,tr2) -> if b then aux_q5 text tr2 ((b, acc ^ "C")::lst) (i+1) (acc ^"C") else aux_q5 text tr2 lst (i+1) (acc ^ "C")
else  if text.[i] = 'G' then 
  match tr.g with 
  |Empty -> lst
  |Node(b,tr2) -> if b then aux_q5 text tr2 ((b, acc ^ "G")::lst) (i+1) (acc ^"G") else aux_q5 text tr2 lst (i+1) (acc ^ "G")
else 
    match tr.t with 
    |Empty -> lst
    |Node(b,tr2) -> if b then aux_q5 text tr2 ((b, acc ^ "T")::lst) (i+1) (acc ^"T") else aux_q5 text tr2 lst (i+1) (acc ^ "T")
    
  )
else lst


let find_all text trie = 
  let n = String.length text in 
  let rec aux lst ind = 
    if ind >= n then lst else aux (aux_q5 text trie lst ind "") (ind+1)
  in 
  aux [] 0




let rec size trie : int = 
  let res = ref 0 in 
  (match trie.a with 
  |Empty -> res := !res
  |Node(_, tr) -> res := !res + 1+ (size tr) )
  ;
  (match trie.g with 
  |Empty -> res := !res
  |Node(_, tr) -> res := !res + 1+ (size tr ))
  ;
  (match trie.c with 
  |Empty -> res := !res
  |Node(_, tr) -> res := !res + 1+ (size tr ))
  ;
  (match trie.t with 
  |Empty -> res := !res
  |Node(_, tr) -> res := !res + 1+ (size tr) )
  ;
  !res

let insert trie mot = 
  if mem mot trie then trie
  else begin 
    let n = String.length mot in 
    let rec aux tr ind = 
      if ind < n then 
        if mot.[ind] = 'A' then 
          match tr.a with 
          |Empty -> {tr with a = if ind = n-1 then Node(true, {a=Empty;c=Empty;g=Empty;t=Empty}) else Node(false, aux empty (ind+1)) }
          |Node(b, tr2) -> if ind = n-1 then {tr with a = Node(true, tr2)}
                          else {tr with a = Node(b, aux tr2 (ind+1))}
        else  if mot.[ind] = 'C' then  match tr.c with 
          |Empty -> {tr with c = if ind = n-1 then Node(true, {a=Empty;c=Empty;g=Empty;t=Empty}) else Node(false, aux empty (ind+1))}
          |Node(b, tr2) -> if ind = n-1 then {tr with c = Node(true, tr2)}
                          else {tr with c = Node(b, aux tr2 (ind+1))}
        else if mot.[ind] = 'G' then  match tr.g with 
          |Empty -> {tr with g = if ind = n-1 then Node(true, {a=Empty;c=Empty;g=Empty;t=Empty}) else Node(false, aux empty (ind+1))}
          |Node(b, tr2) -> if ind = n-1 then {tr with g = Node(true, tr2)}
                          else {tr with g = Node(b, aux tr2 (ind+1))}
        else  match tr.t with 
          |Empty -> {tr with t = if ind = n-1 then Node(true, {a=Empty;c=Empty;g=Empty;t=Empty}) else Node(false, aux empty (ind+1))}
          |Node(b, tr2) -> if ind = n-1 then {tr with t = Node(true, tr2)}
                          else {tr with t = Node(b, aux tr2 (ind+1))}  
      
      else failwith "AILLE"

        in 

  aux trie 0
  end

let make_trie lst = 
  List.fold_left(fun acc elem -> insert acc elem) empty lst



    
  




let () = Printf.printf "Question 2:\n";;
let () = match find_first (read_text "chaine_10.txt") simple_trie with 
  |Some(i, v)->Printf.printf ("Some(%d, %s)\n") i v
  |None -> Printf.printf "None\n"
;;
let () = match find_first (read_text "chaine_100.txt") simple_trie with 
  |Some(i, v)->Printf.printf ("Some(%d, %s)\n") i v
  |None -> Printf.printf "None\n"
;;
let () = match find_first (read_text "chaine_1000.txt") simple_trie with 
  |Some(i, v)->Printf.printf ("Some(%d, %s)\n") i v
  |None -> Printf.printf "None\n"
;;
let () = match find_first (read_text "chaine_10000000.txt") simple_trie with 
  |Some(i, v)->Printf.printf ("Some(%d, %s)\n") i v
  |None -> Printf.printf "None\n"
;;
let () = Printf.printf "%d\n" (size simple_trie);;

let() = Printf.printf "Question 3:\n";;
let () = Printf.printf "%d \n" (size (make_trie (read_motif "motif_5.txt")) mod 10000);;
let () = Printf.printf "%d \n" (size (make_trie (read_motif "motif_10.txt")) mod 10000);;
let () = Printf.printf "%d \n" (size (make_trie (read_motif "motif_100.txt")) mod 10000);;
let () = Printf.printf "%d \n" (size (make_trie (read_motif "motif_1000.txt")) mod 10000);;


let () = Printf.printf "Question 4: \n";;
let () = match find_first (read_text "chaine_1000.txt") (make_trie (read_motif "motif_10.txt")) with 
|Some(i, v)->Printf.printf ("Some(%d, %s)\n") i v
|None -> Printf.printf "None\n"
;;
let () = match find_first (read_text "chaine_10000000.txt") (make_trie (read_motif "motif_10.txt")) with 
|Some(i, v)->Printf.printf ("Some(%d, %s)\n") i v
|None -> Printf.printf "None\n";;

let () = Printf.printf "Question 5: \n";;
let () = Printf.printf "%d\n" (List.length (find_all (read_text "chaine_100.txt") (make_trie (read_motif "motif_10.txt"))) mod 10000);;
let () = Printf.printf "%d\n" (List.length (find_all (read_text "chaine_1000.txt") (make_trie (read_motif "motif_10.txt"))) mod 10000);;
let () = Printf.printf "%d\n" (List.length (find_all (read_text "chaine_5000000.txt") (make_trie (read_motif "motif_10.txt"))) mod 10000);;
let () = Printf.printf "%d\n" (List.length (find_all (read_text "chaine_1000000.txt") (make_trie (read_motif "motif_1000.txt"))) mod 10000);;