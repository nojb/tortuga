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

(** 6. Graphics *)

open Gg
open LogoTypes
open LogoAtom
open LogoEnv
  
(** 6.1 Turtle Motion *)

let forward (module T : TURTLE) dist =
  T.move dist

let back (module T : TURTLE) dist =
  T.move (-. dist)

let left (module T : TURTLE) deg =
  T.turn (-. deg)

let right (module T : TURTLE) deg =
  T.turn deg

let setpos (module T : TURTLE) l =
  T.set_pos (List.nth l 0) (List.nth l 1)
  
let setxy (module T : TURTLE) x y =
  T.set_pos x y

let setx (module T : TURTLE) x =
  let _, y = T.get_pos () in
  T.set_pos x y

let sety (module T : TURTLE) y =
  let x, _ = T.get_pos () in
  T.set_pos x y

let setheading (module T : TURTLE) h =
  T.set_heading h

let home (module T : TURTLE) =
  T.set_pos 0.0 0.0;
  T.set_heading 0.0

let arc (module T : TURTLE) angle radius =
  T.arc angle radius

(** 6.2 Turtle Motion Queries *)

let pos (module T : TURTLE) =
  let x, y = T.get_pos () in
  [x; y]

let xcor (module T : TURTLE) =
  let x, _ = T.get_pos () in
  x

let ycor (module T : TURTLE) =
  let _, y = T.get_pos () in
  y

let heading (module T : TURTLE) =
  T.get_heading ()

(** 6.3 Turtle and Window Control *)

let clean (module T : TURTLE) =
  T.clean_screen ()

let clearscreen (module T : TURTLE) =
  T.set_pos 0.0 0.0;
  T.set_heading 0.0;
  T.clean_screen ()

(** 6.5 Pen and Background Control *)

let pendown (module T : TURTLE) =
  T.pen_down ()

let penup (module T : TURTLE) =
  T.pen_up ()

let setpencolor env (module T : TURTLE) = function
    `L color ->
    begin
      try
        let c = H.find env.palette color in
        T.set_color c
      with
      | Not_found ->
        error "setpencolor: unknown color %s" color
    end
  | `R [r; g; b] ->
    let r = float r /. 100.0 in
    let g = float g /. 100.0 in
    let b = float b /. 100.0 in
    T.set_color (Gg.Color.v_srgb r g b)
  | `R _ -> assert false

(* TODO check that 0 <= r, g, b <= 100 *)
let setpalette env name = function
    [r; g; b] ->
    let r = float r /. 100.0 in
    let g = float g /. 100.0 in
    let b = float b /. 100.0 in
    H.add env.palette name (Gg.Color.v_srgb r g b)
  | _ ->
    assert false

let setpensize (module T : TURTLE) size =
  T.set_size size

(** 6.6 Pen Queries *)

let palette env col =
  try
    let c = H.find env.palette col in
    let r = truncate (Gg.Color.r c *. 100.0) in
    let g = truncate (Gg.Color.g c *. 100.0) in
    let b = truncate (Gg.Color.b c *. 100.0) in
    [r; g; b]
  with
  | Not_found ->
    error "palette: color %s not found" col

let init env =
  set_pf env "forward" Lga.(turtle @@ num @-> ret retvoid) forward;
  set_pf env "fd" Lga.(turtle @@ num @-> ret retvoid) forward;
  set_pf env "back" Lga.(turtle @@ num @-> ret retvoid) back;
  set_pf env "bk" Lga.(turtle @@ num @-> ret retvoid) back;
  set_pf env "left" Lga.(turtle @@ num @-> ret retvoid) left;
  set_pf env "lt" Lga.(turtle @@ num @-> ret retvoid) left;
  set_pf env "right" Lga.(turtle @@ num @-> ret retvoid) right;
  set_pf env "rt" Lga.(turtle @@ num @-> ret retvoid) right;
  set_pf env "setpos" Lga.(turtle @@ fix_list num 2 @-> ret retvoid) setpos;
  set_pf env "setxy" Lga.(turtle @@ num @-> num @-> ret retvoid) setxy;
  set_pf env "setx" Lga.(turtle @@ num @-> ret retvoid) setx;
  set_pf env "sety" Lga.(turtle @@ num @-> ret retvoid) sety;
  set_pf env "setheading" Lga.(turtle @@ num @-> ret retvoid) setheading;
  set_pf env "home" Lga.(turtle @@ ret retvoid) home;
  set_pf env "arc" Lga.(turtle @@ num @-> num @-> ret retvoid) arc;

  set_pf env "pos" Lga.(turtle @@ ret (value (list num))) pos;
  set_pf env "xcor" Lga.(turtle @@ ret (value num)) xcor;
  set_pf env "ycor" Lga.(turtle @@ ret (value num)) ycor;
  set_pf env "heading" Lga.(turtle @@ ret (value num)) heading;
  
  set_pf env "clean" Lga.(turtle @@ ret retvoid) clean;
  set_pf env "clearscreen" Lga.(turtle @@ ret retvoid) clearscreen;

  set_pf env "pendown" Lga.(turtle @@ ret retvoid) pendown;
  set_pf env "pd" Lga.(turtle @@ ret retvoid) pendown;
  set_pf env "penup" Lga.(turtle @@ ret retvoid) penup;
  set_pf env "pu" Lga.(turtle @@ ret retvoid) penup;
  set_pf env "setpencolor" Lga.(env @@ turtle @@ alt word (fix_list int 3) @-> ret retvoid)
    setpencolor;
  set_pf env "setpc" Lga.(env @@ turtle @@ alt word (fix_list int 3) @-> ret retvoid)
    setpencolor;
  set_pf env "setpalette" Lga.(env @@ word @-> fix_list int 3 @-> ret retvoid) setpalette;
  set_pf env "setpensize" Lga.(turtle @@ num @-> ret retvoid) setpensize;

  set_pf env "palette" Lga.(env @@ word @-> ret (value (list int))) palette
