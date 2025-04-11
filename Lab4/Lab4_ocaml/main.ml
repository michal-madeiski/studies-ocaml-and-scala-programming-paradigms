(*zad2*)
let multiListPow(list) =
  if list = [] then []
  else let rec rec_multiListPow(listOrigin, index, newList, counter, el) =
         match (List.tl listOrigin, counter < index) with 
         | ([], _) when counter = index ->  List.rev newList
         | (_, true) -> rec_multiListPow(listOrigin, index, el::newList, counter + 1, el*index)
         | _ -> rec_multiListPow(List.tl listOrigin, index + 1, newList, 0, List.hd (List.tl listOrigin)) in
       rec_multiListPow(list, 1, [], 0, List.hd list);;
multiListPow([1;2;3;4]);;
multiListPow([1]);;
multiListPow([]);;
(*zad2*)
