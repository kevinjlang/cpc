(* Copyright 2018, Kevin Lang, Oath Research *) 

(* 
This program generates the 23 length-limited Huffman codes used by the high-performance 
implementation of Compressed Probabilitistic Counting which is described in the paper [cite].

How to compile and run:

~/ocaml-4.03.0/bin/ocamlopt -o generateHuffmanCodes columnProbabilities.ml generateHuffmanCodes.ml

./generateHuffmanCodes > output-file

We note that since there is floating-point involved, it is conceivable (although unlikely)
that changing the version of ocaml could change this computation enough so that different
huffman codes were constructed, which would break the decompression of sketches that had been
created earlier.

*)

open ColumnProbabilities

let revarg_list_iter a b = List.iter b a

(****************************************************************************************)

(* We use dynamic programming to construct the code that has the
   shortest average codeword length, subject to a constraint on the
   maximum codeword length. We are using a naive DP rather than the
   the more efficient (and complicated) package-merge algorith, but
   our solutions _are_ optimal, and the computation is easily
   affordable for the small problems that we need to solve. 

   We point out that our DP setup is based on the assumption that an
   optimal code tree exists in which the symbols are sorted by
   probability and there are no crossed edges. This is a well-known
   fact for standard Huffman codes, and in fact it allows the code to
   be constructed in linear time once the probabilities have been
   sorted. As for length-limited Huffman codes, high-level
   descriptions of the package-merge algorithm also employ the
   sorted-leaves assumption, so we have done the same below. *)

(****************************************************************************************)

type 'a mytree = Node of 'a mytree * 'a mytree | Leaf of 'a

type 'a tableau_record_type = {
    start_: int;
    length_: int;
    depth_: int;  (* actual depth *)
    leaf_cost: float; (* summed over the tree's leaves *)
    tree_cost: float; 
    tree: 'a mytree;
  }

(***************************************************************)
(* Given a specific number of leaves (n), calculate
   the minimum and maximum tree depth that is feasible. *)

let min_and_max_feasible_depth_for_n n limit = begin
  assert (n >= 1);
  let d = ref 0 in
  while ((1 lsl !d) < n) do incr d done;
  let the_min = !d in
  let raw_max = n-1 in
  assert (the_min <= raw_max);
  assert (the_min <= limit);
  (the_min, min raw_max limit)
end

(***************************************************************)

(*
  In the following, the three dimensions of the "state" array are:
  start: ranges from 0 to num_symbols - 1
  length: only the lengths which make sense. Also, zero-length is in the table but it unused.
  depth_limit: which is an upper bound on the depth of the contained tree (which could, however, be smaller)
*)

let solve_dynamic_program cost_item_pairs_in depth_limit = begin
  let cost_item_pairs = Array.of_list cost_item_pairs_in in
  let num_items = Array.length cost_item_pairs in
  if num_items > (1 lsl depth_limit) then failwith "too many items for depth";
  if num_items < 1 then failwith "empty input";
  Array.sort compare cost_item_pairs; (* important *)
  let dummy_item = snd cost_item_pairs.(num_items-1) in
  let dummy_record = {start_ = (-1); length_ = (-1); depth_ = max_int; leaf_cost = 1e100; tree_cost = 1e100; tree = (Leaf dummy_item)} in

  let tableau = Array.init num_items
      (fun start -> Array.init (num_items-start+1)
	  (fun length -> Array.make (1+depth_limit) dummy_record)) in

(* Initialize with trees for intervals of length 1. *)
  for pos = 0 to num_items-1 do
    let (cost, item) = cost_item_pairs.(pos) in
    let record = {start_ = pos; length_ = 1; depth_ = 0; leaf_cost = cost; tree_cost = 0.0; tree = Leaf item} in
    for md = 0 to depth_limit do        (* Because the depth of a bare leaf is zero, its total tree cost is zero, so it *)
      tableau.(pos).(1).(md) <- record; (* is the cheapest tree covering that single position no matter what the depth limit is. *)
    done;
  done;
  
  (* Consider sucessively longer intervals, building up the optimal solutions as we go. *)
  for length = 2 to num_items do (* note: it is important that the four loops are nested in this order *)
    let (depth_lb, depth_ub) = min_and_max_feasible_depth_for_n length depth_limit in
    for target_depth = depth_lb to depth_ub do (* loop over target depths *)
      for start = 0 to num_items - length do   (* loop over starting positions *)
	let best = ref dummy_record in         (* keep track of the best solution for the current interval *)
	for length_a = 1 to length-1 do        (* loop over the different ways to split the interval into sub-intervals *)
	  let length_b = length - length_a in
	  let start_a = start in
	  let start_b = start + length_a in
	  let record_a = tableau.(start_a).(length_a).(target_depth-1) in
	  let record_b = tableau.(start_b).(length_b).(target_depth-1) in
	  let this_cost = record_a.tree_cost +. record_b.tree_cost +. (record_a.leaf_cost +. record_b.leaf_cost) in
	  if this_cost < !best.tree_cost then begin
	    let this_record = 
	      {start_ = start; length_ = length;
	       depth_ = 1 + (max record_a.depth_ record_b.depth_);
	       tree_cost = this_cost; leaf_cost = record_a.leaf_cost +. record_b.leaf_cost;
	       tree = Node (record_a.tree, record_b.tree);
	     } in
	    best := this_record;
	  end;
	done; (* end of loop over interval splits *)
	assert (!best.depth_ <= target_depth);
	for d = !best.depth_ to depth_limit do (* save the solution at all relevant depths *)
	  if !best.tree_cost < tableau.(start).(length).(d).tree_cost then begin
	    tableau.(start).(length).(d) <- !best;
	  end;
	done;
      done; (* end of loop over starting positions *)
    done; (* end of loop over target depths *)
  done; (* end of loop over interval lengths *)

  tableau

end

(*
  Printf.printf "[%d %d] splits into (%d %d) + (%d %d)\n" start length start_a length_a start_b length_b;
 *)

(************************************************************)

(* returns a list of (length, item) pairs *)
let rec codeword_lengths tree_in = begin
  let accum = ref [] in
  let rec walk depth tree = begin
    match tree with 
    | Leaf item -> accum := (depth, item) :: !accum
    | Node (left, right) -> begin
	walk (depth+1) left;
	walk (depth+1) right;
    end
  end in
  walk 0 tree_in;
  List.sort compare !accum
end
      
(************************************************************)

(* the inputs are (length, codeword, item) triples *)
(* the codewords are ints, and should be sorted from shortest to longest *)
let verify_prefix_property codeword_arr = begin
  let num_codewords = Array.length codeword_arr in
  for i = 0 to num_codewords - 2 do
    let (li,ci,si) = codeword_arr.(i) in
    for j = i+1 to (num_codewords - 1) do
      let (lj,cj,sj) = codeword_arr.(j) in
      assert (lj >= li);
      if (ci = (cj land ((1 lsl li)-1))) then begin
	Printf.printf "Warning: (%d %d) and (%d %d) violate the prefix property\n"
	  li ci lj cj; flush stdout;
      end;
    done;
  done;
end

(************************************************************)

(* We need to reverse the codeword bits because our bitstreams are little-endian. *)
let reverse_bits codeword_in len = begin
  let codeword_out = ref 0 in
  for i = 0 to len-1 do
    let j = (len-1) - i in 
    let b = (codeword_in lsr i) land 1 in
    codeword_out := !codeword_out lor (b lsl j);
  done;
  !codeword_out
end

(************************************************************)
(* The solution to the Dynamic Program contains an explicit code tree
   that we could walk to extract the codewords. Instead we extract the
   codeword lengths and use them to construct a "Canonical Huffman
   Code" whose codewords differ from the tree's, but have the same
   lengths and hence the same compression ratio.  The reason for doing
   this was a hypothetical future optimization in which the library
   statically contains a description of each code that is somewhat
   smaller than a table of 256 shorts. *)

(* returns an array of (length, codeword, item) triples *)
let canonical_huffman_codes len_item_pairs_in = begin
  let len_item_pairs = List.sort compare len_item_pairs_in in (* better safe than sorry *)
  let accum = ref [] in
  let next_code = ref 0 in
  assert (len_item_pairs <> []);
  let prev_len = ref (fst (List.hd len_item_pairs)) in
  revarg_list_iter len_item_pairs
    (fun (len, item) ->
      if len <> !prev_len then (next_code := !next_code lsl (len - !prev_len);
				prev_len := len);
      let reversed_code = reverse_bits !next_code len in
      accum := (len, reversed_code, item) :: !accum;
      incr next_code;
      );
  let result = Array.of_list (List.rev !accum) in
  verify_prefix_property result;
  result
end

(************************************************************)

(* returns a list of (length, codeword, item) triples *)
let construct_code prob_item_pairs length_limit = begin
  let total_prob = List.fold_left (fun t (x,_) -> t +. x) 0.0 prob_item_pairs in
  if not (abs_float (1.0 -. total_prob) < 1e-14) then begin
    Printf.printf "%.19f total prob\n" total_prob; flush stdout;
    assert (abs_float (1.0 -. total_prob) < 1e-14);    
  end;
  let num_items = List.length prob_item_pairs in
  let tableau = solve_dynamic_program prob_item_pairs length_limit in
  let solution = tableau.(0).(num_items).(length_limit) in
  Printf.printf " // avg_length: %.19f; max_length = %d; num_symbols = %d\n"
    solution.tree_cost solution.depth_ num_items;
  let length_item_pairs = codeword_lengths solution.tree in
  let canonical_code_triples = canonical_huffman_codes length_item_pairs in
  canonical_code_triples
end

(*
  Printf.printf " // avg_length: %.19f; length_limit = %d; actual_max_length = %d; num_symbols = %d\n"
    solution.tree_cost length_limit solution.depth_ num_items;
*)

(************************************************************)

(* Print a "Huffman Code" for C.
   Assumes a 12-bit length limit, and also that
   the symbols are 0 through num_symbols-1. *)


let print_code_for_c triples_in = begin
  let triples_arr = Array.copy triples_in in
  Array.sort (fun (_,_,a) (_,_,b) -> compare a b) triples_arr; 
  let arrlen = Array.length triples_arr in
  Printf.printf "{\n";
  Printf.printf "//table, // (4 bits,  12 bits) symbol\n";
  Printf.printf "//entry, // (length, codeword) [byte]\n";
  for i = 0 to arrlen-1 do
    let (codelen, codeword, encoded_symbol) = triples_arr.(i) in assert (encoded_symbol = i);
    let combined = codeword lor (codelen lsl 12) in
    Printf.printf " 0x%.4x%s // (%2d, %4d) %3d\n" 
      combined 
      (if i = (arrlen-1) then " " else ",")
      codelen codeword encoded_symbol;
  done;
  Printf.printf "},\n";
end

(************************************************************)

(*
print_code_for_c (construct_code [(0.5, 5); (0.25, 4); (0.125, 3); (0.0625, 2); (0.03125, 1); (0.03125, 0)] 4);;
 // num_symbols = 6; length_limit = 4; actual_max_length = 4; average_length = 2.000000000000000
{
//short, // (len, codeword) symbol
//table, // (4 bits,  12 bits) symbol\n";
//entry, // (length, codeword) (byte)\n";

 0x4003, // ( 4,    3)   0
 0x400b, // ( 4,   11)   1
 0x4007, // ( 4,    7)   2
 0x400f, // ( 4,   15)   3
 0x2001, // ( 2,    1)   4
 0x1000  // ( 1,    0)   5
},
*)

(************************************************************)

let construct_steady_state_codes () = begin
  Printf.printf "\n\n // Sixteen Encoding Tables for the Steady State.\n";
  for index = 0 to 15 do
    let numer = 1 + 2 * index in
    let phase = (float numer) /. 32.0 in
    Printf.printf "\n // (table %d of 22) (steady %d of 16) (phase = %.9f = %d.0 / 32.0)\n"
      index index phase numer;
    let prob_item_pairs = asymptotic_byte_probs phase in
    let code = construct_code prob_item_pairs 12 in
    print_code_for_c code;
    flush stdout;
  done;
end

(************************************************************)

let construct_midrange_codes () = begin
  Printf.printf "\n // Six Encoding Tables for the Midrange.\n";
  let numers = [|3;5;7;9;11;13|] in
  for index = 0 to 5 do
    let numer = numers.(index) in
    let c_over_k = ((float numer) /. 6.0) in
    Printf.printf "\n // (table %d of 22) (midrange %d of 6) (c/k = %.9f = %d.0 / 6.0)\n"
      (16 + index) index c_over_k numer;
    let prob_item_pairs = initial_byte_probs c_over_k in
    let code = construct_code prob_item_pairs 12 in
    print_code_for_c code;
    flush stdout;
  done;
end

(************************************************************)
(************************************************************)
(*
let probs56 = Array.to_list (Array.init 56 (fun i -> (0.5 ** (float (i+1)), i)));;
let probs64 = Array.to_list (Array.init 64 (fun i -> (0.5 ** (float (i+1)), i)));;
let probs65 = Array.to_list (Array.init 65 (fun i -> (0.5 ** (float (i+1)), i)));;

let code56 = construct_code probs56 12;;
 // avg_length: 2.0205078125000000000; max_length = 12; num_symbols = 56
let code64 = construct_code probs64 12;;
 // avg_length: 2.0244140625000000000; max_length = 12; num_symbols = 64
let code65 = construct_code probs65 12;;
 // avg_length: 2.0249023437500000000; max_length = 12; num_symbols = 65

The "65" comes from an alternate design for the sparse encoder that was rejected, 
but since the 65 code is nearly as efficient as the 64 (or even 56) we kept it.
*)

let construct_length_limited_unary_code () = begin
  Printf.printf "\n // Length-limited \"unary\" code with 65 symbols.\n";
  let prob_item_pairs = Array.to_list (Array.init 65 (fun i -> (0.5 ** (float (i+1)), i))) in
  Printf.printf " // entropy:    2.0\n";
  let code = construct_code prob_item_pairs 12 in
  print_code_for_c code;
  flush stdout;
end

(************************************************************)
(************************************************************)


let construct_all_codes_for_implementation () = begin
  construct_length_limited_unary_code ();
  construct_steady_state_codes ();
  construct_midrange_codes ();
end

(************************************************************)

;;
construct_all_codes_for_implementation ()

