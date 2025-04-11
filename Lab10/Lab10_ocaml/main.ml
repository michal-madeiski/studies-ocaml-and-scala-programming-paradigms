(*zad1*)
(*rekurencja ogonowa*)
let array2DMinRec arr2D =
  let resultBuffer = Array.make (Array.length arr2D) 0 in

  let arrayMin arr =
    if Array.length arr = 0 then failwith("Empty array!")
    else let rec helper1 idx min =
           if idx < Array.length arr then
             if arr.(idx) < min then helper1 (idx+1) arr.(idx) else helper1 (idx+1) min
           else min in
         helper1 1 arr.(0) in
  
  let rec helper2 idx arr =
    if idx < Array.length arr then
      (arr.(idx) <- arrayMin arr2D.(idx);
       helper2 (idx+1) arr)
    else arr in
  helper2 0 resultBuffer;;
(*rekurencja ogonowa*)

(*imperatywnie*)
let array2DMinImp arr2D =
  let resultBuffer = Array.make (Array.length arr2D) 0 in

  let arrayMin arr =
    if Array.length arr = 0 then failwith("Empty array!")
    else
      let min = ref arr.(0) in
      for i = 1 to (Array.length arr - 1) do
        if arr.(i) < !min then min := arr.(i)
      done;
      !min in
            
  for i=0 to (Array.length arr2D - 1) do
    resultBuffer.(i) <- arrayMin arr2D.(i)
  done;
  resultBuffer;;
(*imperatywnie*)

let arr2D = 
 [|
   [|1;2;3;4|];
   [|1;2;1;2|];
   [|100;0;-1;2|];
 |];;

array2DMinRec arr2D;;
array2DMinImp arr2D;;
(*zad1*)
