
Random.self_init();; (*Mettre une seed si on veut pas du vrai aléatoire*)
let rec find (tableau : int array)(v : int): int = 
  if(tableau.(v)<0) then  v 
  else begin 
    let acc = find tableau tableau.(v) in 
    tableau.(v) <- acc;
    acc
  end

let meme_classe (tableau : int array)(a : int)(b : int): bool = 
  (find tableau a = find tableau b)

let rec union (tab : int array)(u : int)(v : int) = 
  let ru = find tab u in
  let rv = find tab v in
  if ru = rv then (tab.(ru) <- tab.(ru)-1; tab.(rv) <- ru);
  if rv > ru then
    tab.(ru)<-rv
  else
    tab.(rv)<-ru

  

let new_uf (len : int) : int array = 
  Array.make len (-1)

  
let voisines n p k = 
  [(k>=p, k-p);(k+p< n*p-1,k+p);((k mod p)<> n-1, k+1); ((k mod p) <> 0, k-1) ] |> List.filter fst |> List.map snd 

let creer_murs n p : (int*int) list = 
    let res = ref [] in
    for k = 0 to n-1 do
      for i = 0 to p-1 do
        let j = p*k + i in 
        if (k+i) mod 2 =0 then (List.iter (fun x -> res := (j,x)::(!res)) (voisines n p j)) 
        done
      done;
    !res

let melange_murs (murs : (int*int)list) : (int*int) list= 
  let len = List.length murs in
  
  let rec aux mur lst = 
    match mur with
    | [] -> lst
    | h :: q -> aux q ((h,Random.int len)::lst)
  in 
  let acc = List.sort (fun (x,y) (z,a)-> y-a) (aux murs []) in
  let rec aux2 (murss : ((int*int)*int) list) (lst4: (int*int) list) = 
    match murss with
    |[]-> lst4
    |h :: q -> begin let h1,h2 = h in aux2 q (h1::lst4) end
  in 
  aux2 acc []




let creer_lab n p = 
  let murs = creer_murs n p  in 
  let mur_mel = melange_murs murs in 
  let classe = new_uf (n*p) in 
  List.filter_map (fun (a,b)-> if (meme_classe classe a b) then Some(a,b) else 
    begin
      union classe a b;
      None
    end) mur_mel

let trie_mur n p (murs : (int*int) list ) = 
  let acc = List.map (fun (a,b)->if(b>a) then (a,b) else (b,a)) murs in
  List.sort (fun (a,b) (c,d) -> if (a-c)=0 then b-d else (if ( a / n <> c / p) then a-c else b-d ) ) acc (*trie les murs dans l'ordre lexicographique*)


let dessine_lab n p (murs : (int*int) list) = 
  let res = ref (trie_mur n p murs) in
  for j = 0 to n-1 do
    Printf.printf "+--"
  done;
  Printf.printf"+\n";
  for  j = 0 to n-1 do (*Ce qu'on fait sur chaque ligne*)
    Printf.printf "|";
    for k = 0 to p-1 do 
      
      match !res with 
      |[]->if (k <>p-1) then Printf.printf"   " else Printf.printf"  "
      |(a,b)::q -> begin 
                if((a = j*p + k) && (b = a+1)) then (Printf.printf"  |" ; res :=q)else (if (k<>p-1) then Printf.printf"   " else Printf.printf("  "));
          
                   end
      done;
      Printf.printf "|\n";
    
    if(j<>n-1) then begin
      Printf.printf"+";
    for k = 0 to p-1 do 
      match !res with
      |[]->Printf.printf"   "
      |(a,b)::q -> begin 
                if((a = j*p + k)&& b = a + p) then( Printf.printf"--+"; res := q) else Printf.printf"  +";
                  
                  
                   end
      done;
      Printf.printf "\n";
    end
  done;
  for j = 0 to n-1 do
    Printf.printf "+--"
  done;
  Printf.printf"+\n"


  let dessine_lab2 n p (murs : (int*int) list) filename =
    let file = filename in
    let oc = open_out file in 
    let res = ref (trie_mur n p murs) in
    for j = 0 to n-1 do
      Printf.fprintf oc "+--"
    done;
    Printf.fprintf oc "+\n";
    for  j = 0 to n-1 do (*Ce qu'on fait sur chaque ligne*)
      Printf.fprintf oc "|";
      for k = 0 to p-1 do 
        
        match !res with 
        |[]->if (k <>p-1) then Printf.fprintf oc "   " else Printf.fprintf oc "  "
        |(a,b)::q -> begin 
                  if((a = j*p + k) && (b = a+1)) then (Printf.fprintf oc "  |" ; res :=q)else (if (k<>p-1) then Printf.fprintf oc "   " else Printf.fprintf oc ("  "));
            
                    
                     end
        done;
        Printf.fprintf oc "|\n";
      
      if(j<>n-1) then begin
        Printf.fprintf oc "+";
      for k = 0 to p-1 do 
        match !res with
        |[]->Printf.fprintf oc "   "
        |(a,b)::q -> begin 
                  if((a = j*p + k)&& b = a + p) then( Printf.fprintf oc "--+"; res := q) else Printf.fprintf oc"  +";
                    
                    
                     end
        done;
        Printf.fprintf oc "\n";
      end
    done;
    for j = 0 to n-1 do
      Printf.fprintf oc "+--"
    done;
    Printf.fprintf oc "+\n"

let length = 10;;
let width = 10;;
let mel = creer_lab  length width;; 
(*let () = List.iter (fun (a,b) -> Printf.printf "(%d,%d)\n" a b ) (trie_mur 5 5 mel);;*)
let () = Printf.printf "%d\n" (List.length mel);;
let fichier = "Labyrinthe.txt";;
let ()= dessine_lab2 length width mel fichier