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
  let min = if -tab.(u)>(-tab.(v)) then v else u in
  let max = if min = u then v else u in
  Printf.printf "%d\n" max;
  tab.(min) <- max;
  for i = 0 to Array.length tab- 1 do
    if(find tab i = min) then tab.(i) <- max
  done

let new_uf (len : int) : int array = 
  Array.make len (-1)





let test = [|-1;-3;1;4;-2;1;2;6|];;
let () = union test 1 4;;
let rep = find test 3;;
let ()= Printf.printf "%d\n" rep;;
