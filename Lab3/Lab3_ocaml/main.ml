(*zad1*)
let subLists(n, list) =
  if n < 0 || n >= List.length list then raise (Failure "Invalid value of n!")
  else if list = [] then []
   else let rec rec_subLists(index, n, list1, list2, listOrigin)=
         if listOrigin = [] then List.rev list2@List.rev list1
         else if index < n then rec_subLists(index + 1, n, List.hd listOrigin::list1, list2, List.tl listOrigin)
         else  rec_subLists(index + 1, n, list1,  List.hd listOrigin::list2, List.tl listOrigin) in
       rec_subLists(0, n, [], [], list);;
subLists(2, [1;2;5;6]);;
subLists(5, [1;2;5;6]);;
subLists(1, [1;2;5;6]);;
subLists(1, []);;
(*zad1*)

(*zad2 - instrukcje warunkowe*)
let multiList(list) =
  if list = [] then []
  else let rec rec_multiList(listOrigin, index, newList, counter, el) =
         if List.tl listOrigin = [] && counter = index then List.rev newList
         else if counter < index then rec_multiList(listOrigin, index, el::newList, counter + 1, el)
         else rec_multiList(List.tl listOrigin, index + 1, newList, 0, List.hd (List.tl listOrigin)) in
       rec_multiList(list, 1, [], 0, List.hd list);;
multiList([1;2;3]);;
multiList([1]);;
multiList([]);;
(*zad2 - instrukcje warunkowe*)

(*zad2 - dopasowanie wzorca*)
let multiList2(list) =
  if list = [] then []
  else let rec rec_multiList2(listOrigin, index, newList, counter, el) =
         match (List.tl listOrigin, counter < index) with 
         | ([], _) when counter = index ->  List.rev newList
         | (_, true) -> rec_multiList2(listOrigin, index, el::newList, counter + 1, el)
         | _ -> rec_multiList2(List.tl listOrigin, index + 1, newList, 0, List.hd (List.tl listOrigin)) in
       rec_multiList2(list, 1, [], 0, List.hd list);;
multiList2([1;2;3]);;
multiList2([1]);;
multiList2([]);;
(*zad2 - dopasowanie wzorca*)
