type 'a repr = { mutable rank : int ; label : 'a }

type 'a o =
	| Repr of 'a repr
	| Ptr of int

type 'a t = (int, 'a o) Hashtbl.t

let create () =
	Hashtbl.create 42

let add t e v =
	Hashtbl.add t e (Repr {rank = 0; label = v })

let rec find_repr t e = 
	match Hashtbl.find t e with
	 | Repr(repr) -> (e, repr)
	 | Ptr(e1) ->
			 let e2,_ as c2 = find_repr t e1 in
			 if e1 <> e2
			 then Hashtbl.replace t e (Ptr(e2));
			 c2

let find t e =
	let (_, repr) = find_repr t e in repr.label

let merge t e1 e2 v =
	let f1,repr1 = find_repr t e1 in
	let f2,repr2 = find_repr t e2 in
	assert (f1 <> f2);
	if repr1.rank < repr2.rank
	then Hashtbl.replace t f1 (Ptr(f2))
	else (
		Hashtbl.replace t f2 (Ptr(f1));
		if repr1.rank = repr2.rank
		then repr1.rank <- repr1.rank + 1
	)

let equiv t e1 e2 =
	e1 = e2 || 
	try
		fst (find_repr t e1) = fst (find_repr t e2)
	with Not_found -> false

let replace t e v = 
	let f, repr = find_repr t e in
	Hashtbl.replace t f (Repr { repr with label = v })

let iter_repr f t =
	Hashtbl.iter (fun x -> function Ptr(_) -> () | Repr(repr) -> f x repr.label) t

let to_list t =
	Hashtbl.fold (fun x _ acc -> (x, find t x) :: acc) t []


