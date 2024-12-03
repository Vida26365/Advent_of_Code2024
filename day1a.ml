
let rec print_list = function 
[] -> ()
| e::l -> print_string e ; print_string "," ; print_list l


(* let read_file filename =
  let ic = open_in filename in
  let rec read_lines (acc : int list) acc2 =
    try
      let line = input_line ic in
      let splitted = String.split_on_char ' ' line in
      (* print_endline (List.nth splitted 3); *)
      read_lines ((int_of_string (List.nth splitted 0)) :: acc) ((int_of_string (List.nth splitted 3)) :: acc2)   
    with
    | End_of_file -> close_in ic; (List.rev acc , List.rev acc2)
    | e -> close_in ic; raise e 
  in
  read_lines [] [] *)

let read_vsebina vsebina = 
  let splitted = String.split_on_char '\n' vsebina in
  let rec read_lines (acc : int list) acc2 = function
    | [] -> (List.rev acc , List.rev acc2)
    | x :: xs -> 
      let splitted = String.split_on_char ' ' x in
      read_lines ((int_of_string (List.nth splitted 0)) :: acc) ((int_of_string (List.nth splitted 3)) :: acc2) xs
  in
  read_lines [] [] splitted	
  
let naloga1 vsebina_datoteke =
  let (sez1, sez2) = read_vsebina vsebina_datoteke in
  let sort1 = List.sort compare sez1 in
  let sort2 = List.sort compare sez2 in

  List.map2 (fun x y -> abs (x - y)) sort1 sort2
  |> List.fold_left (+) 0
  |> string_of_int

let naloga2 vsebina_datoteke =
  let (sez1, sez2) = read_vsebina vsebina_datoteke in
  (* let rec zanka sez acc =
    match sez with
    | [] -> acc
    |  *)
  let rec count x sez acc=
    match sez with
    | [] -> acc
    | prvi::tail when x = prvi -> count x tail (acc + 1)
    | _::tail -> count x tail acc
  in
  List.map (fun x -> (count x sez2 0) * x) sez1
  |> List.fold_left ( + ) 0
  |> string_of_int

let _ =
  let preberi_datoteko ime_datoteke =
      let chan = open_in ime_datoteke in
      let vsebina = really_input_string chan (in_channel_length chan) in
      (* print_endline vsebina; *)
      close_in chan;
      vsebina
  and izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = preberi_datoteko "day_1.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "day_1_1.out" odgovor1;
  izpisi_datoteko "day_1_2.out" odgovor2