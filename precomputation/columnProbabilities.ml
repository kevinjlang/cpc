(* Copyright 2018, Kevin Lang, Oath Research *)

let rev_compare a b = compare b a

(****************************************************************************************)

(* Note the careful use of log1p() etc. Which is necessary to avoid numerical problems. 
   Be aware that this program was written during the research phase when we were using 
   1-based column indices, whereas the final implementation used 0-based column indices.
*)

let prob_of_1_and_0_with_their_logs k j n = begin (* all arguments are floats *)
  let l2 = log 2.0 in
  let coupon_probability = (0.5 ** j) /. k in
  let log_p0 = n *. (log1p (-1.0 *. coupon_probability)) in
  let p0 = exp log_p0 in
  let p1 = (-1.0 *. (expm1 log_p0)) in
  let log_p1 = if p1 < 0.5 then (log p1) else (log1p (-1.0 *. p0)) in
  let log_p1 = if log_p1 = -0.0 then 0.0 else log_p1 in
  (p1, log_p1 /. l2, 
   p0, log_p0 /. l2)
end

let qnj kf nf ji = begin
  let negfrac = (-1.0) /. (kf *. (2.0 ** (float ji))) in
  let the_log = log1p negfrac in
  (-1.0) *. (expm1 (nf *. the_log))
end

let accurate_c_of_n kf nf = begin
  let total = ref 0.0 in
  for col = 128 downto 1 do (* probably overkill *)
    total := !total +. (qnj kf nf col)
  done;
  kf *. !total
end

let icon_inversion_tol = 1e-15

let rec accurate_n_of_c_aux depth kf target_c lo hi = begin
  if depth > 100 then failwith "bug 1";
  let mid = (hi +. lo) /. 2.0 in
  if (hi -. lo) /. mid < icon_inversion_tol then mid
  else begin
    let mid_b = accurate_c_of_n kf mid in
    if mid_b = target_c then mid
    else if mid_b < target_c then accurate_n_of_c_aux (depth+1) kf target_c mid hi
    else if mid_b > target_c then accurate_n_of_c_aux (depth+1) kf target_c lo mid
    else failwith "bug 2";
  end
end

let accurate_n_of_c kf target_c = begin
  if target_c = 0.0 || target_c = 1.0 then target_c
  else begin
    let lo = target_c in
    assert (accurate_c_of_n kf lo < target_c);
    let hi = ref (2.0 *. target_c) in
    while (accurate_c_of_n kf !hi <= target_c) do hi := !hi *. 2.0 done;
    accurate_n_of_c_aux 1 kf target_c lo !hi
  end
end

(****************************************************************************************)
(****************************************************************************************)

(* The following two functions apply to the first 8 columns of the bit matrix, and is 
   suitable for the HYBRID and PINNED flavors where the values of N and C are such that the
   sliding window exists but hasn't shifted yet. *)

let initial_8_column_probs c_over_k = begin (* intended for c_over_k in the range [0.0, 2.4] *)
  let kf = 2.0 ** 32.0 in 
  let nf = accurate_n_of_c kf (kf *. c_over_k) in
  let accum = ref [] in
  let total = ref 0.0 in
  for col = 1 to 8 do (* here in the math, the column numbering starts at 1 *)
    let (p1, lg1, p0, lg0) = prob_of_1_and_0_with_their_logs kf (float col) nf in
    let entropy = -1.0 *. (p1 *. lg1 +. p0 *. lg0) in
    total := !total +. entropy;
    accum := [|p0; p1|] :: !accum;
  done;
 Printf.printf " // entropy:    %.19f\n" !total; flush stdout;
  Array.of_list (List.rev !accum)
end

(* returns a list of (prob, byteval) pairs *)
let initial_byte_probs c_over_k = begin
  let arr = initial_8_column_probs c_over_k in 
  let accum = ref [] in
  for byte = 0 to 255 do
    let prob = ref 1.0 in
    for i = 0 to 7 do
      let bit = (byte lsr i) land 1 in
      prob := !prob *. arr.(i).(bit);
    done;
    accum := (!prob, byte) :: !accum;
  done;
  List.sort rev_compare !accum
end

(****************************************************************************************)

(* The following functions provide good approximations for the 8-column sliding window 
   in the asymptotic regime where K, N, and C are very large. *)

let asymptotic_shift_phase = 0.380285 

(* The above empirically determined threshold is the phase at which the sliding window should shift
   in order to maximize the entropy covered by the sliding window. The implementation actually shifts
   at phase = 3/8 = 0.375, but the phases for which we call the following subroutine are intentionally
   kept away from either of those versions of the threshold. Note that the 2^32 in the following
   is somewhat arbitrary, as is the 30.0. However, specifying 30 determines the 28 and 29. *)

let asymptotic_8_column_probs phase = begin (* phase is 0.0 through 1.0 *)
  let kf = 2.0 ** 32.0 in 
  let nf = accurate_n_of_c kf (kf *. (30.0 +. phase)) in (* ICON estimator of N *)
  let accum = ref [] in
  let total = ref 0.0 in
  let startingColumn = if phase < asymptotic_shift_phase then 28 else 29 in
  for col = startingColumn to startingColumn + 8 - 1 do
    let (p1, lg1, p0, lg0) = prob_of_1_and_0_with_their_logs kf (float col) nf in
    let entropy = -1.0 *. (p1 *. lg1 +. p0 *. lg0) in
    total := !total +. entropy;
    accum := [|p0; p1|] :: !accum;
  done;
  Printf.printf " // entropy:    %.19f\n" !total; flush stdout;
  Array.of_list (List.rev !accum)
end

(* returns a list of (prob, byteval) pairs *)
let asymptotic_byte_probs phase = begin
  let arr = asymptotic_8_column_probs phase in 
  let accum = ref [] in
  for byte = 0 to 255 do
    let prob = ref 1.0 in
    for i = 0 to 7 do
      let bit = (byte lsr i) land 1 in
      prob := !prob *. arr.(i).(bit);
    done;
    accum := (!prob, byte) :: !accum;
  done;
  List.sort rev_compare !accum
end



