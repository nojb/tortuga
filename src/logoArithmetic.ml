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
open LogoGlobals
  
(** 4.1 Numeric Operations *)

let sum =
  let names = ["sum"] in
  let doc =

    "\
SUM num1 num2
(SUM num1 num2 num3 ...)
num1 + num2

    Outputs the sum of its inputs."

  in
  let args = Lga.(num @-> num @-> rest num (value num)) in
  let f num1 num2 nums =
    List.fold_left (+.) (num1 +. num2) nums
  in
  prim ~names ~doc ~args ~f

let difference =
  let names = ["difference"] in
  let doc =

    "\
DIFFERENCE num1 num2
num1 - num2

    Outputs the difference of its inputs. Minus sign means infix difference in
    ambiguous contexts (when preceded by a complete expression), unless it is
    preceded by a space and followed by a nonspace. (See also MINUS.)"

  in
  let args = Lga.(num @-> num @-> ret (value num)) in
  let f num1 num2 = num1 -. num2 in
  prim ~names ~doc ~args ~f

let minus =
  let names = ["minus"] in
  let doc =

    "\
MINUS num
- num

    Outputs the negative of its input. Minus sign means unary minus if the
    previous token is an infix operator or open parenthesis, or it is preceded
    by a space and followed by a nonspace. There is a difference in binding
    strength between the two forms:
 	
      MINUS 3 + 4     means   -(3+4)
      - 3 + 4         means   (-3)+4."

  in
  let args = Lga.(num @-> ret (value num)) in
  let f n = -. n in
  prim ~names ~doc ~args ~f

let product =
  let names = ["product"] in
  let doc =

    "\
PRODUCT num1 num2
(PRODUCT num1 num2 num3 ...)
num1 * num2
outputs the product of its inputs."

  in
  let args = Lga.(num @-> num @-> rest num (value num)) in
  let f num1 num2 nums =
    List.fold_left ( *. ) (num1 *. num2) nums
  in
  prim ~names ~doc ~args ~f

let remainder =
  let names = ["remainder"] in
  let doc =

    "\
REMAINDER num1 num2

    Outputs the remainder on dividing num1 by num2; both must be integers and
    the result is an integer with the same sign as num1."

  in
  let args = Lga.(num @-> num @-> ret (value num)) in
  let f a b = mod_float a b in
  prim ~names ~doc ~args ~f

(* modulo: not implemented *)

let int =
  let names = ["int"] in
  let doc =

    "\
INT num

    Outputs its input with fractional part removed, i.e., an integer with the
    same sign as the input, whose absolute value is the largest integer less
    than or equal to the absolute value of the input."

  in
  let args = Lga.(num @-> ret (value int)) in
  let f num = truncate num in
  prim ~names ~doc ~args ~f

let round =
  let names = ["round"] in
  let doc =

    "\
ROUND num

    Outputs the nearest integer to the input."

  in
  let args = Lga.(num @-> ret (value num)) in
  let f num = floor (num +. 0.5) in
  prim ~names ~doc ~args ~f

let sqrt =
  let names = ["sqrt"] in
  let doc =

    "\
SQRT num

    Outputs the square root of the input, which must be nonnegative."

  in
  let args = Lga.(nn_num @-> ret (value num)) in
  let f num = sqrt num in
  prim ~names ~doc ~args ~f
  
let power =
  let names = ["power"] in
  let doc =

    "\
POWER num1 num2

    Outputs num1 to the num2 power. If num1 is negative, then num2 must be an
    integer."

  in
  let args = Lga.(num @-> num @-> ret (value num)) in
  let f a b = a ** b in
  prim ~names ~doc ~args ~f

(** 4.2 Numeric Predicates *)

let lessp =
  let names = ["lessp"; "less?"] in
  let doc =

    "\
LESSP num1 num2
LESS? num1 num2
num1 < num2

    Outputs TRUE if its first input is strictly less than its second."

  in
  let args = Lga.(num @-> num @-> ret (value any)) in
  let f a b = if a < b then true_word else false_word in
  prim ~names ~doc ~args ~f

let greaterp =
  let names = ["greaterp"; "greater?"] in
  let doc =

    "\
GREATERP num1 num2
GREATER? num1 num2
num1 > num2

    Outputs TRUE if its first input is strictly greater than its second."

  in
  let args = Lga.(num @-> num @-> ret (value any)) in
  let f a b = if a > b then true_word else false_word in
  prim ~names ~doc ~args ~f

let lessequalp =
  let names = ["lessequalp"; "lessequal?"] in
  let doc =

    "\
LESSEQUALP num1 num2
LESSEQUAL? num1 num2
num1 <= num2

    Outputs TRUE if its first input is less than or equal to its second."

  in
  let args = Lga.(num @-> num @-> ret (value any)) in
  let f a b = if a <= b then true_word else false_word in
  prim ~names ~doc ~args ~f

let greaterequalp =
  let names = ["greaterequalp"; "greaterequal?"] in
  let doc =

    "\
GREATEREQUALP num1 num2
GREATEREQUAL? num1 num2
num1 >= num2

    Outputs TRUE if its first input is greater than or equal to its second."

  in
  let args = Lga.(num @-> num @-> ret (value any)) in
  let f a b = if a >= b then true_word else false_word in
  prim ~names ~doc ~args ~f

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

let () =
  List.iter add_prim
    [
      sum;
      difference;
      minus;
      product;
      remainder;
      int;
      round;
      sqrt;
      power;

      lessp;
      greaterp;
      lessequalp;
      greaterequalp;

      random;
      rerandom;

      form;

      bitand;
      bitor;
      bitxor;
      bitnot
    ]
