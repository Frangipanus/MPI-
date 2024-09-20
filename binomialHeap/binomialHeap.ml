type 'a binomial_node = 
{
  mutable label : 'a;
  mutable parent : 'a bionmial_tree;
  mutable children : 'a binomial_node list;
  mutable sibling : 'a binomial_node list;
  mutable degree : int;
}

and 'a bionmial_tree = 'a binomial_node option
type 'a binomial_heap = 'a binomial_node list
type couple = {debut : int;
                taille : int}
  

let create_single (x :'a) : 'a binomial_node = 
  {label = x; parent = None; children = []; sibling = []; degree = 0}



let rec retasse (relou : 'a binomial_node) : unit = 
  match relou.parent with
  |None -> ()
  | Some parent ->begin  
              let acc = relou.label in 
                if acc < parent.label then begin
                  relou.label <- parent.label;
                  parent.label <- acc;
                  retasse parent
                end
              end

let minimum heap = 
  let rec aux lst mini arbre = 
    match lst with
    |[] -> arbre
    |h::q -> if h.label < mini then aux q h.label h else aux q mini arbre
  in 
  match heap with
  |[] -> failwith "Liste vide"
  |h::q -> aux q h.label h

let link x y = 
  y.children <- x::y.children;
  x.parent <- Some(y);
  y.degree <- y.degree+1;
  retasse x;
  y

let union h1 h2 = 
  let rec auxi l1 l2  retenue = 
    match (l1, l2, retenue) with
    |[], [], Some(h1) ->  [h1]
    |[], [], None -> []
    |[], h::q, Some(h1) -> if h.degree = h1.degree then auxi l1 q (Some(link h1 h))
                             else h1::(auxi l1 q None)
    |h::q,[], Some(h1) -> if h.degree = h1.degree then auxi l1 q (Some(link h1 h))
                                else h1::(auxi l1 q None)
    |[], h::q, None -> auxi l1 q None
    |h::q, [], None -> auxi l1 q None
    |h1::q1, h2::q2, None -> if h1.degree = h2.degree then auxi q1 q2 (Some(link h1 h2)) 
    else if h1.degree < h2.degree then h1::(auxi q1 l2 None) else h2::(auxi q1 l2 None)
    |h1::q1, h2::q2, Some(h3) -> if (h3.degree < h1.degree && h3.degree<h2.degree ) then
                                    h3::(auxi l1 l2 None)
                                else if (h3.degree = h1.degree && h1.degree=h2.degree) then 
                                  h3::(auxi q1 q2 (Some(link h1 h2)))
                                else if (h1.degree < h2.degree) then
                                  auxi q1 l2 (Some (link h1 h3))
                                else auxi l1 q2 (Some (link h2 h3))
  in
  let h = auxi h1 h2 None in 
  h

let insert h1 x = 
  let acc = create_single x in 
  union [acc] h1

let extract_min heap = 
  let res = minimum heap in 
  let min = res.label in 
  let rec aux lst  = 
    match lst with
    |[]-> []
    |h::q -> if h.label = min then q else h::(aux q)
  in 
  let h1 = aux heap in
  let new_heap= union h1 (res.children) in 
  (min, new_heap)



let decrase_key (node : 'a binomial_node)(value : 'a)  = 
  node.label <- node.label - value;
  retasse node


  

let chaise filename = 
  print_string("done with reading");
  let f = Scanf.Scanning.open_in filename in 
  let n = ref  0  in 
  let k = ref 0 in 
  Scanf.bscanf f ("%d %d ") (fun x y z a ->  z := x; a:= y) k n ;
  Printf.printf "%d %d\n" !n !k;
  let s = ref "" in
  Scanf.bscanf f ("%s ") (fun x y-> y := x ) s ; 
  let q = ref 0 in 
  Scanf.bscanf f ("%d") (fun x y-> y := x ) q; 
  Scanf.Scanning.close_in f;
  let c = { debut = -1; taille = !n} in 
  let heap = create_single c in
  let h = ref [heap] in 
  for i = 0 to !k-1 do 
    let (mini,new_heap) = extract_min !h in
    let new_seat = ref 0 in 
    if (mini.taille) mod 2 == 1 then begin
      new_seat := mini.taille/2 + 1
     end
    else begin 
      if !s.[0] == 'R' then 
        new_seat:= mini.taille/2
    else
        new_seat:=mini.taille/2+1
    end;
    let c1 = {debut = mini.debut; taille = !new_seat - mini.debut-1} in 
    let c2 = {debut = mini.debut + !new_seat; taille = mini.taille - !new_seat} in
    let new_heap1 = insert new_heap c1 in
    h := insert new_heap1 c2
  done;
  print_string("all done")

let () = chaise "test.txt"