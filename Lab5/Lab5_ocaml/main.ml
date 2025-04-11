(*zad2*)
let (+) list1 list2 =
  if list1 = [] || list2 = [] then raise (Failure "Vector cannot be empty!")
  else let rec add(ret, l1, l2) =
         match (l1, l2) with
         | ([], []) -> List.rev ret
         | (_, []) -> add((List.hd l1)::ret, List.tl l1, l2)
         | ([], _) -> add((List.hd l2)::ret, l1, List. tl l2)
         | (_, _) -> add((List.hd l1 +. List.hd l2)::ret, List.tl l1, List. tl l2) in
       add([], list1, list2);;
[2.0; 3.0] + [];;
[2.0; 3.0] + [1.0; 4.0];;
[2.0; 3.0] + [1.0; 4.0; 5.5];;
[] + [];;
(*zad2*)
                                 

                   
