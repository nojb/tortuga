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

module G = Graphics

let _ =
  G.open_graph "";
  G.moveto (G.size_x () / 2) (G.size_y () / 2)

let theta = ref 90

let get_heading () =
  !theta

let set_heading h =
  theta := 90 - h

let get_pos () =
  let x, y = G.current_point () in
  (x - (G.size_x () / 2), y - (G.size_y () / 2))

let set_pos x y =
  G.moveto (x + G.size_x () / 2) (y + G.size_y () / 2)

let set_color c =
  let r c = truncate (Gg.Color.r c *. 255.) in
  let g c = truncate (Gg.Color.g c *. 255.) in
  let b c = truncate (Gg.Color.b c *. 255.) in
  G.set_color (G.rgb (r c) (g c) (b c))

let pi = 4.0 *. atan 1.0

let rad_of_deg d =
  float d /. 180.0 *. pi

let move d =
  let dx = float d *. cos (rad_of_deg !theta) in
  let dx = truncate dx in
  let dy = float d *. sin (rad_of_deg !theta) in
  let dy = truncate dy in
  G.rlineto dx dy
  
let turn r =
  theta := !theta - r

let arc angle r =
  assert false
  (* let src = V2.(t.point + of_polar (v r t.angle)) in *)
  (* let dst = V2.add t.point (V2.of_polar (V2.v r (t.angle -. Float.rad_of_deg angle))) in *)
  (* let base = if t.penup then I.void else I.const (Color.with_a t.color t.alpha) in *)
  (* let p = P.earc ~large:false ~angle (Size2.v r r) dst (P.sub src P.empty) in *)
  (* let outline = I.cut ~area:(`O P.o) p base in *)
  (* let image = outline >> I.blend t.image in *)
  (* { t with image } *)

let clear_screen () =
  G.clear_graph ()
