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

open Gg
open Vg

let blank = Color.black
let blue = Color.blue
let lime = Color.v_srgbi 191 255 0
let cyan = Color.v_srgbi 0 255 255
let red = Color.red
let magenta = Color.v_srgbi 255 0 255
let yellow = Color.v_srgbi 255 255 0
let white = Color.white
let brown = Color.v_srgbi 150 75 0
let tan = Color.v_srgbi 210 180 140
let green = Color.green
let aquamarine = Color.v_srgbi 127 255 212
let salmon = Color.v_srgbi 250 128 114
let purple = Color.v_srgbi 128 0 128
let orange = Color.v_srgbi 255 127 0
let gray = Color.v_srgbi 128 128 128

type t = {
  point : V2.t;
  angle : float;
  image : image;
  (* outline : P.outline; *)
  penup : bool;
  color : Color.t;
  alpha : float
}

let fresh = {
  point = V2.zero;
  angle = Float.pi_div_2;
  image = I.void;
  penup = false;
  color = Color.black;
  alpha = 1.
}

let move d t =
  let dest = V2.(t.point + of_polar (v d t.angle)) in
  let fg = if t.penup then I.void else I.const (Color.with_a t.color t.alpha) in
  let line = P.line dest (P.sub t.point P.empty) in
  let outline = I.cut ~area:(`O P.o) line fg in
  let image = outline >> I.blend t.image in
  { t with point = dest; image }

let set_pos p t =
  { t with point = p }

let get_pos t =
  t.point

let turn r t =
  { t with angle = t.angle -. Float.rad_of_deg r }

let arc angle r t =
  let src = V2.(t.point + of_polar (v r t.angle)) in
  let dst = V2.add t.point (V2.of_polar (V2.v r (t.angle -. Float.rad_of_deg angle))) in
  let base = if t.penup then I.void else I.const (Color.with_a t.color t.alpha) in
  let p = P.earc ~large:false ~angle (Size2.v r r) dst (P.sub src P.empty) in
  let outline = I.cut ~area:(`O P.o) p base in
  let image = outline >> I.blend t.image in
  { t with image }

let set_color c t =
  { t with color = c }

let clear_screen t =
  { t with point = V2.zero; image = I.void }

let render t out =
  (* 1. Define your image *)

  let size = Size2.v 100. 100. (* mm *) in
  let view = Box2.v_mid P2.o (Size2.v 100. 100.) in
  let image = t.image in

  (* 2. Render *)
  
  let xmp = Vgr.xmp () in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_pdf.target ~xmp ()) (`Channel out) in
  ignore (Vgr.render r (`Image (size, view, image))); 
  ignore (Vgr.render r `End)
