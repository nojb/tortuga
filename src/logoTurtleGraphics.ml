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

let round d =
  int_of_float (floor (d +. 0.5))
    
let _ =
  G.open_graph "";
  G.moveto (G.size_x () / 2) (G.size_y () / 2)

let theta = ref 90.0

let pendown = ref true

let get_heading () =
  !theta

let set_heading h =
  theta := 90.0 -. h

let get_pos () =
  let x, y = G.current_point () in
  float (x - (G.size_x () / 2)), float (y - (G.size_y () / 2))

let set_pos x y =
  G.moveto (round (x +. float (G.size_x ()) /. 2.0)) (round (y +. float (G.size_y ()) /. 2.0))

let set_color c =
  let r c = round (Gg.Color.r c *. 255.) in
  let g c = round (Gg.Color.g c *. 255.) in
  let b c = round (Gg.Color.b c *. 255.) in
  G.set_color (G.rgb (r c) (g c) (b c))

let pi = 4.0 *. atan 1.0

let rad_of_deg d =
  d /. 180.0 *. pi

let move d =
  let dx = d *. cos (rad_of_deg !theta) in
  let dx = round dx in
  let dy = d *. sin (rad_of_deg !theta) in
  let dy = round dy in
  if !pendown then G.rlineto dx dy else G.rmoveto dx dy
  
let turn r =
  theta := !theta -. r

let arc angle r =
  let rr = round r in
  if !pendown then
    G.draw_arc (G.current_x ()) (G.current_y ())
      rr rr (round !theta) (round (!theta -. r))

let set_size sz =
  G.set_line_width (round sz)

let pen_down () =
  pendown := true

let pen_up () =
  pendown := false
  
let clean_screen () =
  G.clear_graph ()
