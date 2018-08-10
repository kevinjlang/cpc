(* Copyright 2018, Kevin Lang, Oath Research *)

(*
~/ocaml-4.03.0/bin/ocamlopt -o generateIconPolynomials unix.cmxa columnProbabilities.ml myfit.ml generateIconPolynomials.ml
*)

open ColumnProbabilities
open Myfit

(************************************************************************)

let scheme3_term k c = begin
  let r = c /. k in
  c *. (1.0 +. (r *. r *. r /. 66.774757))  (* the "66.774757" is somewhat arbitrary *)
end

let scheme3_factor k c estimate = estimate /. (scheme3_term k c)

let scheme3_estimate k c factor = factor *. (scheme3_term k c)

(************************************************************************)

type icon_lookup_table = {
    lg_k_: int;
    k_: int;
    max_c_: int; (* The biggest c that uses scheme3. Larger values use the exponential approximation. *)
    c_arr_: float array; (* These two arrays specifiy the mapping from c to the estimate *)
    n_arr_: float array; 
    mutable poly_: float array;
}

(************************************************************************)

let max_c_factor lg_k = (if lg_k < 14 then 5.7 else 5.6)

(************************************************************************)

let make_icon_lookup_table lg_k = begin
  assert (lg_k >= 4); (* the range covered by our accuracy investigation *)
  let k = 1 lsl lg_k in

  let (c_array, n_array) = begin

    if k <= 128 then begin (* start of code for k <= 128 *)
      let len = 6 * k in
      let c_arr = Array.make len 0.0 in
      let n_arr = Array.make len 0.0 in
      for c = 1 to len do
	let index = c - 1 in
	c_arr.(index) <- float c;
	n_arr.(index) <- accurate_n_of_c (float k) (float c);
      done;      
      (c_arr, n_arr)
    end (* end of code for k <= 128 *)

    else begin (* start of code for k > 128 *)
      let accum_c = ref [] in
      let accum_n = ref [] in
      let keep_going = ref true in
      let c = ref 1 in
      while (!keep_going) do
	let icon_est = accurate_n_of_c (float k) (float !c) in
	accum_c := (float !c) :: !accum_c;  accum_n := icon_est :: !accum_n;
	if !c <= 128 then begin (* emphasize the importance of small values of c *)
	  for copies = 2 to 50 do
	    accum_c := (float !c) :: !accum_c;  accum_n := icon_est :: !accum_n;
	  done;
	end;
	keep_going := (!c < 6 * k);
	c := max (!c+1) (129 * !c / 128);
      done;
      (Array.of_list !accum_c,
       Array.of_list !accum_n)
    end (* end of code for k > 128 *)

  end in

  {lg_k_ = lg_k; k_ = k;
   max_c_ = int_of_float ((float k) *. (max_c_factor lg_k));
   c_arr_ = c_array;
   n_arr_ = n_array;
   poly_ = [||]}
end

(************************************************************************)

let convert_icon_lookup_table_to_scheme3 self = begin
  let c_arr = self.c_arr_ in
  let n_arr = self.n_arr_ in
  let k = float self.k_ in
  let len = Array.length c_arr in
  for j = 0 to len-1 do
    let c = c_arr.(j) in
    let n = n_arr.(j) in
    n_arr.(j) <- scheme3_factor k c n;
  done;
end

(************************************************************************)

let icon_exponential_estimate k c = 0.7940236163830469 *. (float k) *. (2.0 ** ((float c) /. (float k)))

(************************************************************************)

let evaluate_polynomial constants x = begin
  let len = Array.length constants in
  let tot = ref constants.(len-1) in
  for j = len-2 downto 0 do
    tot := !tot *. x;
    tot := !tot +. constants.(j);
  done;
  !tot
end

(************************************************************************)

let do_poly_fit obj degree = begin
  let len = Array.length obj.c_arr_ in
  let scaled_c_arr = Array.map (fun x -> x /. (float (2 * obj.k_))) obj.c_arr_ in  (* the "2" is somewhat arbitrary *)
  setup_polynomial_fitter len degree;
  let constants = polynomial_fit scaled_c_arr obj.n_arr_ in
  teardown_polynomial_fitter ();
  obj.poly_ <- constants;
end

(************************************************************************)

let doit_poly () = begin
  for lg_k = 4 to 32 do
    let obj = make_icon_lookup_table lg_k in
    convert_icon_lookup_table_to_scheme3 obj;
    let degree = 19 in
    do_poly_fit obj degree;
    let poly = obj.poly_ in
    Printf.printf "\n // log K = %d\n" lg_k;
    assert (degree=19);
    for i =  0 to  4 do Printf.printf " %.19g," poly.(i) done; Printf.printf "\n";
    for i =  5 to  9 do Printf.printf " %.19g," poly.(i) done; Printf.printf "\n";
    for i = 10 to 14 do Printf.printf " %.19g," poly.(i) done; Printf.printf "\n";
    for i = 15 to 19 do Printf.printf " %.19g," poly.(i) done; Printf.printf "\n";
    flush stdout;
  done; (* end of loop over lg_k *)
end

;;
doit_poly ()

