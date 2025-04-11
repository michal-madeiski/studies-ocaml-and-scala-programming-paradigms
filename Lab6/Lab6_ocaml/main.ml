(*zad2*)
type litera = Brak | Zawiera;;
type dane_slow = Puste | Krotkie of litera | Srednie of litera | Dlugie of litera;;

let wordsData(list, x) =
  if list = [] then []
  else
    let containLetter(w, l) =
      if String.contains w l then Zawiera
      else Brak in
    let temp_wordsData(word) =
         match String.length word <= 10 with
         |true when String.length word = 0 -> Puste
         |true -> Krotkie(containLetter(word, x))
         |false when String.length word <= 20 -> Srednie(containLetter(word, x))
         |_ -> Dlugie(containLetter(word, x)) in
       List.map temp_wordsData list;;
wordsData(["abc"; "def"; "ghiiiiiiiiiiiiiiiii"; "aaaaaaaaaaaaaaaaaaaaaa"; "bb"], 'a');;
(*zad2*)
