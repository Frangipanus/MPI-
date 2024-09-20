let nb_arcs n m = 
  let memo = Array.make(n+m+7*(n-1)+11*(n-1)) (-1) in 
  memo.(0)<-5;
  let total = ref 0 in 
  for i = 1 to Array.length memo -1 do 
    memo.(i)<-19999991*memo.(i-1) mod 19999999
  done;
  
  for i = 0 to n-1 do 
    for j = 0 to n-1 do 
      if i<>j && memo.(n+m+7*i+11*j) mod 1000 < m then 
        total := 1 + !total
    else ()
    done;
  done;
  !total

let acc = nb_arcs 1000 500;;
let () = Printf.printf "%d" acc