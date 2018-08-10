(* Copyright 2018, Kevin Lang, Oath Research *)

(*
  Source for the linear formulas: http://en.wikipedia.org/wiki/Simple_linear_regression
*)

let array_fold_left2 f x a b = begin
  let lena = Array.length a in
  let lenb = Array.length b in
  assert (lena = lenb);
  let r = ref x in
  for i = 0 to lena - 1 do
    r := f !r (Array.unsafe_get a i) (Array.unsafe_get b i);
  done;
  !r
end

(******************************************************)

let test_xvec = 
[| 1.47; 1.50; 1.52; 1.55; 1.57; 1.60; 1.63; 1.65; 1.68; 1.70; 1.73; 1.75; 1.78; 1.80; 1.83 |]

let test_yvec = 
[| 52.21; 53.12; 54.48; 55.84; 57.20; 58.57; 59.93; 61.29; 63.11; 64.47; 66.28; 68.10; 69.92; 72.19; 74.46 |]

(* answer should be (-39.062,  61.272, ??)  *)

(******************************************************)

let linear_fit xvec yvec = begin

  assert ((Array.length xvec) = (Array.length yvec));
  assert ((Array.length xvec) > 1);

  let x_tot  = Array.fold_left ( +. ) 0.0 xvec in
  let y_tot  = Array.fold_left ( +. ) 0.0 yvec in
  let xy_tot = array_fold_left2 (fun t a b -> t +. a *. b) 0.0 xvec yvec in
  let xx_tot = array_fold_left2 (fun t a b -> t +. a *. b) 0.0 xvec xvec in

  let fl_len = float (Array.length xvec) in
  let x_avg  = x_tot /. fl_len in
  let y_avg  = y_tot  /. fl_len in 
  let xy_avg = xy_tot  /. fl_len in
  let xx_avg = xx_tot  /. fl_len in

  let beta_numer = xy_avg -. (x_avg *. y_avg) in
  let beta_denom = xx_avg -. (x_avg *. x_avg) in
  assert (beta_denom <> 0.0);

  let beta  = beta_numer /. beta_denom in (* slope *)
  let alpha = y_avg -. beta *. x_avg in   (* y_intercept *)

(*
  let predict_y x = beta *. x +. alpha in
  let squared_error_of_prediction x y =
    (let diff = (y -. (predict_y x)) in (diff *. diff)) in
  let total_squared_error_of_predictions =
    array_fold_left2 (fun t x y -> t +. (squared_error_of_prediction x y)) 0.0 xvec yvec in
*)

  (alpha, beta) 

end

(******************************************************)

let linear_fit_with_arbitrary_y_intercept = linear_fit

(******************************************************)

let linear_fit_with_zero_y_intercept xvec yvec = begin
  assert ((Array.length xvec) = (Array.length yvec));
  assert ((Array.length xvec) > 1);

  let x_tot  = Array.fold_left ( +. ) 0.0 xvec in
  let y_tot  = Array.fold_left ( +. ) 0.0 yvec in
  let xy_tot = array_fold_left2 (fun t a b -> t +. a *. b) 0.0 xvec yvec in
  let xx_tot = array_fold_left2 (fun t a b -> t +. a *. b) 0.0 xvec xvec in

  let fl_len = float (Array.length xvec) in
  let x_avg  = x_tot /. fl_len in
  let y_avg  = y_tot  /. fl_len in
  let xy_avg = xy_tot  /. fl_len in
  let xx_avg = xx_tot  /. fl_len in

  let beta_numer = xy_avg in
  let beta_denom = xx_avg in
  assert (beta_denom <> 0.0);

  let beta  = beta_numer /. beta_denom in (* slope *)
  let alpha = 0.0 in   (* y_intercept *)

  let predict_y x = beta *. x +. alpha in

  let squared_error_of_prediction x y =
    (let diff = (y -. (predict_y x)) in (diff *. diff)) in

  let total_squared_error_of_predictions =
    array_fold_left2 (fun t x y -> t +. (squared_error_of_prediction x y)) 0.0 xvec yvec in

  (alpha, (* y_intercept *)
   beta,  (* slope *)
   total_squared_error_of_predictions,
   y_avg /. x_avg)

end

(*****************************************************************************)
(*****************************************************************************)

let quadratic_channels = ref (stdin, stdout) (* placeholder channels of the correct type *)

let setup_quadratic_fitter num_eqns = begin
  let (c1, c2) = !quadratic_channels in
  assert (c1 = stdin);
  let command = Printf.sprintf "~/bin/multi_reading_linsolve %d %d" num_eqns 3 in
  quadratic_channels := Unix.open_process command;
end

let teardown_quadratic_fitter () = begin
  let (back_from, out_to) = !quadratic_channels in
  assert (back_from <> stdin);
  Printf.fprintf out_to "0\n"; (* tell the solver to exit *)
  flush out_to;
  ignore(Unix.close_process !quadratic_channels);
  quadratic_channels := (stdin, stdout);
end

(******************************)

let quadratic_fit x_array y_array = begin
  let (back_from, out_to) = !quadratic_channels in
  assert (back_from <> stdin);

  Printf.fprintf out_to "1\n"; (* tell the solver to keep running *)

  for eqn = 0 to (Array.length x_array) - 1 do (* transmit the matrix *)
    for pow = 0 to 2 do
      Printf.fprintf out_to "%.19g\n" (x_array.(eqn) ** (float pow));
    done;
  done;

  for eqn = 0 to (Array.length x_array) - 1 do (* transmit the target vector *)
    Printf.fprintf out_to "%.19g\n" y_array.(eqn);
  done;

  flush out_to;

  let result = Array.make 3 0.0 in
  for pow = 0 to 2 do
    let str = input_line back_from in
    result.(pow) <- float_of_string str;
  done;

  (result.(0), (* Y intercept *)
   result.(1), (* multiplier for X *)
   result.(2)) (* multiplier for X squared *)

end

(*****************************************************************************)
(*****************************************************************************)

let dummy_polynomial_fitter_state = (stdin, stdout, -99)

let polynomial_fitter_state = ref dummy_polynomial_fitter_state

let setup_polynomial_fitter num_eqns degree = begin
  assert (!polynomial_fitter_state = dummy_polynomial_fitter_state);
  let command = Printf.sprintf "/Users/langk/bin/multi_reading_linsolve %d %d" num_eqns (degree+1) in
  let (a,b) = Unix.open_process command in
  polynomial_fitter_state := (a,b,degree);
end

let teardown_polynomial_fitter () = begin
  assert (!polynomial_fitter_state <> dummy_polynomial_fitter_state);
  let (back_from, out_to, _) = !polynomial_fitter_state in
  Printf.fprintf out_to "0\n"; (* tell the solver to exit *)
  flush out_to;
  ignore(Unix.close_process (back_from, out_to));
  polynomial_fitter_state := dummy_polynomial_fitter_state;
end

(******************************)

let polynomial_fit x_array y_array = begin
  let (back_from, out_to, degree) as stuff = !polynomial_fitter_state in
  assert (stuff <> dummy_polynomial_fitter_state);

  Printf.fprintf out_to "1\n"; (* tell the solver to keep running *)

  for eqn = 0 to (Array.length x_array) - 1 do (* transmit the matrix *)
    for pow = 0 to degree do
      Printf.fprintf out_to "%.19g\n" (x_array.(eqn) ** (float pow));
    done;
  done;

  for eqn = 0 to (Array.length x_array) - 1 do (* transmit the target vector *)
    Printf.fprintf out_to "%.19g\n" y_array.(eqn);
  done;

  flush out_to;

  let result = Array.make (degree+1) 0.0 in
  for pow = 0 to degree do
    let str = input_line back_from in
    result.(pow) <- float_of_string str;
  done;

  result

end

(******************************)

let debug_polynomial_fit x_array y_array degree outfile = begin

  let chan = open_out outfile in

  for eqn = 0 to (Array.length x_array) - 1 do (* transmit the matrix *)
    for pow = 0 to degree do
      Printf.fprintf chan "%.19g\n" (x_array.(eqn) ** (float pow));
    done;
  done;

  for eqn = 0 to (Array.length x_array) - 1 do (* transmit the target vector *)
    Printf.fprintf chan "%.19g\n" y_array.(eqn);
  done;

  close_out chan;

end

