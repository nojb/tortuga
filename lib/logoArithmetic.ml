(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** 4. Arithmetic *)

open LogoTypes
open LogoAtom
open LogoEnv

(** 4.1 Numeric Operations *)

let word_true = intern "true"
let word_false = intern "false"

let sum args =
  let aux n1 n2 =
    match n1, n2 with
    | Int n1, Int n2 -> Int (n1 + n2)
    | Int n1, Real f2 -> Real (float n1 +. f2)
    | Real f1, Int n2 -> Real (f1 +. float n2)
    | Real f1, Real f2 -> Real (f1 +. f2)
    | _ ->
        error "sum: expected number argument"
  in
  List.fold_left aux (Int 0) args

let difference n1 n2 =
  match n1, n2 with
  | Int n1, Int n2 -> Int (n1 - n2)
  | Int n1, Real f2 -> Real (float n1 -. f2)
  | Real f1, Int n2 -> Real (f1 -. float n2)
  | Real f1, Real f2 -> Real (f1 -. f2)
  | _ -> error "difference: wrong no of arguments"

let minus = function
  | Int n -> Int (- n)
  | Real f -> Real (-. f)
  | _ ->
      error "minus: wrong type of argument"

let product args =
  let rec aux n1 n2 =
    match n1, n2 with
    | Int n1, Int n2 -> Int (n1 * n2)
    | Int n1, Real f2 -> Real (float n1 *. f2)
    | Real f1, Int n2 -> Real (f1 *. float n2)
    | Real f1, Real f2 -> Real (f1 *. f2)
    | _ ->
        error "product: wrong arg list"
  in
  List.fold_left aux (Int 1) args

let remainder n1 n2 =
  match n1, n2 with
  | Int n1, Int n2 -> Int (n1 mod n2)
  | _ ->
      error "remainder: wrong arg list"

(* modulo: not implemented *)

let int = function
  | Int _ as n -> n
  | Real f -> Int (int_of_float f)
  | _ ->
      error "int: wrong arg list"

let round = function
  | Int _ as o -> o
  | Real f -> Int (int_of_float (floor (f +. 0.5)))
  | _ ->
      error "round: bad arg list"

let sqrt = function
  | Int n -> Real (sqrt (float n))
  | Real f -> Real (sqrt f)
  | _ ->
      error "sqrt: bad arg list"

let power n1 n2 =
  match n1, n2 with
  | Int n1, Int n2 -> Real (float n1 ** float n2)
  | Int n1, Real f2 -> Real (float n1 ** f2)
  | Real f1, Int n2 -> Real (f1 ** float n2)
  | Real f1, Real f2 -> Real (f1 ** f2)
  | _ ->
      error "power: bad arg list"

(** 4.2 Numeric Predicates *)

let lessp n1 n2 =
  match n1, n2 with
  | Int n1, Int n2 -> if n1 < n2 then word_true else word_false
  | Int n1, Real f2 -> if float n1 < f2 then word_true else word_false
  | Real f1, Int n2 -> if f1 < float n2 then word_true else word_false
  | Real f1, Real f2 -> if f1 < f2 then word_true else word_false
  | _ ->
      error "lessp: bad arg list"

let greaterp n1 n2 =
  match n1, n2 with
  | Int n1, Int n2 -> if n1 > n2 then word_true else word_false
  | Int n1, Real f2 -> if float n1 > f2 then word_true else word_false
  | Real f1, Int n2 -> if f1 > float n2 then word_true else word_false
  | Real f1, Real f2 -> if f1 > f2 then word_true else word_false
  | _ ->
      error "greaterp: bad arg list"

let lessequalp n1 n2 =
  match n1, n2 with
  | Int n1, Int n2 -> if n1 <= n2 then word_true else word_false
  | _ ->
      error "lessequalp: bad arg list"

let greaterequalp n1 n2 =
  match n1, n2 with
  | Int n1, Int n2 -> if n1 >= n2 then word_true else word_false
  | _ ->
      error "greaterp: bad arg list"

let equalp o1 o2 =
  assert false

let notequalp o1 o2 =
  assert false

(*
(** 4.3 Random Numbers *)

let initial_seed = 1728

let _ = Random.self_init ()

let random =
  let names = ["random"] in
  let doc =

    "\
RANDOM num
(RANDOM start end)

    With one input, outputs a random nonnegative integer less than its input,
    which must be a positive integer.

    With two inputs, RANDOM outputs a random integer greater than or equal to
    the first input, and less than or equal to the second input. Both inputs
    must be integers, and the first must be less than the second. (RANDOM 0 9)
    is equivalent to RANDOM 10; (RANDOM 3 8) is equivalent to (RANDOM 6)+3."

  in
  let args = Lga.(int @-> opt int (value int)) in
  let f num1 = function
    | None ->
      if num1 > 0 then Random.int num1
      else error "random: argument must be positive"
    | Some num2 ->
      let d = num2 - num1 in
      if d > 0 then Random.int d + num1
      else error "random: first arg (%i) must be less than second arg (%i) " num1 num2
  in
  prim ~names ~doc ~args ~f

let rerandom =
  let names = ["rerandom"] in
  let doc =

    "\
RERANDOM
(RERANDOM seed)

    Command. Makes the results of RANDOM reproducible. Ordinarily the sequence
    of random numbers is different each time Logo is used. If you need the same
    sequence of pseudo-random numbers repeatedly, e.g. to debug a program, say
    RERANDOM before the first invocation of RANDOM. If you need more than one
    repeatable sequence, you can give RERANDOM an integer input; each possible
    input selects a unique sequence of numbers."

  in
  let args = Lga.(opt int retvoid) in
  let f = function
    | None ->
      Random.init initial_seed
    | Some num ->
      Random.init num
  in
  prim ~names ~doc ~args ~f

(** 4.4 Print Formatting *)

let form =
  let names = ["form"] in
  let doc =

    "\
FORM num width precision

    Outputs a word containing a printable representation of num, possibly
    preceded by spaces (and therefore not a number for purposes of performing
    arithmetic operations), with at least width characters, including exactly
    precision digits after the decimal point. (If precision is 0 then there will
    be no decimal point in the output.)

    As a debugging feature, (FORM num -1 format) will print the floating point
    num according to the C printf format, to allow

      to hex :num
        op form :num -1 \"|%08X %08X|
      end

    to allow finding out the exact result of floating point operations. The
    precise format needed may be machine-dependent."

  in
  let args = Lga.(num @-> int @-> int @-> ret (value word)) in
  let f num width precision =
    (* TODO check that width, precision >= 0 *)
    Printf.sprintf "%*.*f" width precision num
    (* TODO width = -1, precision = format for debuggin, use Sscanf.format_of_string *)
  in
  prim ~names ~doc ~args ~f

(** 4.5 Bitwise Operations *)

let bitand =
  let names = ["bitand"] in
  let doc =

    "\
BITAND num1 num2
(BITAND num1 num2 num3 ...)

    Outputs the bitwise and of its inputs, which must be integers."

  in
  let args = Lga.(int @-> int @-> rest int (value int)) in
  let f num1 num2 nums = List.fold_left (land) (num1 land num2) nums in
  prim ~names ~doc ~args ~f

let bitor =
  let names = ["bitor"] in
  let doc =

    "\
BITOR num1 num2
(BITOR num1 num2 num3 ...)

    Outputs the bitwise or of its inputs, which must be integers."

  in
  let args = Lga.(int @-> int @-> rest int (value int)) in
  let f num1 num2 nums = List.fold_left (lor) (num1 lor num2) nums in
  prim ~names ~doc ~args ~f

let bitxor =
  let names = ["bitxor"] in
  let doc =

    "\
BITXOR num1 num2
(BITXOR num1 num2 num3 ...)

    Outputs the bitwise exclusive or of its inputs, which must be integers."

  in
  let args = Lga.(int @-> int @-> rest int (value int)) in
  let f num1 num2 nums = List.fold_left (lxor) (num1 lxor num2) nums in
  prim ~names ~doc ~args ~f

let bitnot =
  let names = ["bitnot"] in
  let doc =

    "\
BITNOT num

    Outputs the bitwise not of its input, which must be an integer."

  in
  let args = Lga.(int @-> ret (value int)) in
  let f num = lnot num in
  prim ~names ~doc ~args ~f
*)

(* let () = *)
(*   add_pfn "sum" 2 sum; *)
(*   add_pf2 "difference" difference; *)
(*   add_pf1 "minus" minus; *)
(*   add_pfn "product" 2 product; *)
(*   add_pf2 "remainder" remainder; *)
(*   add_pf1 "int" int; *)
(*   add_pf1 "round" round; *)
(*   add_pf1 "sqrt" sqrt; *)
(*   add_pf2 "power" power; *)
(*   add_pf2 "lessp" lessp; *)
(*   add_pf2 "less?" lessp; *)
(*   add_pf2 "greaterp" greaterp; *)
(*   add_pf2 "greater?" greaterp; *)
(*   add_pf2 "lessequalp" lessequalp; *)
(*   add_pf2 "lessequal?" lessequalp; *)
(*   add_pf2 "greaterequalp" greaterequalp; *)
(*   add_pf2 "greaterequal?" greaterequalp *)

(*
      random;
      rerandom;

      form;

      bitand;
      bitor;
      bitxor;
      bitnot
    ]
*)
