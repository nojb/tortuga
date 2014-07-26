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

type state = {
  pos     : v2;
  theta   : float;
  image   : Vg.image;
  outline : P.outline;
  pendown : bool;
  color   : color
}

let t =
  ref {
    pos     = V2.zero;
    theta   = Float.pi_div_2;
    image   = I.const Gg.Color.black;
    outline = { P.o with P.cap = `Round };
    pendown = true;
    color   = Color.white
  }

let render name =
  let name, out = match name with
    | None ->
      Filename.open_temp_file ~temp_dir:(Sys.getcwd ()) "" ".pdf"
    | Some name ->
      name, open_out_bin name
  in

  (* 1. Define your image *)
      
  let size = Size2.v 400. 400. (* mm *) in
  let view = Box2.v_mid P2.o (Size2.v 800. 800.) in
  let image = !t.image in

  (* 2. Render *)
  
  let xmp = Vgr.xmp () in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_pdf.target ~xmp ()) (`Channel out) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End);
  
  close_out out;

  print_endline ("output written to " ^ name)

let get_heading () =
  !t.theta

let set_heading h =
  t := { !t with theta = !t.theta -. Float.rad_of_deg h }

let get_pos () =
  let p = !t.pos in
  V2.x p, V2.y p

let set_pos x y =
  t := { !t with pos = V2.v x y }

let set_color c =
  t := { !t with color = c }

let move d =
  let pt = V2.of_polar (V2.v d !t.theta) in
  let pos = V2.add !t.pos pt in
  let path = P.empty >> P.sub !t.pos >> P.line pos in
  let paintcol = if !t.pendown then I.const !t.color else I.void in
  let outl = I.cut ~area:(`O !t.outline) path paintcol in
  let image = !t.image >> I.blend outl in
  t := { !t with image; pos }

let turn r =
  t := { !t with theta = !t.theta -. Float.rad_of_deg r }

let arc angle r =
  let p = V2.of_polar (V2.v r !t.theta) in
  let pos1 = V2.add !t.pos p in
  let pos2 = V2.to_polar pos1 in
  let pos2 = V2.of_polar (V2.v (V2.x pos2) (V2.y pos2 -. Float.rad_of_deg angle)) in
  let path = P.empty >> P.sub pos1 >> P.earc ~large:false ~cw:true (V2.v r r) pos2 in
  let paintcol = if !t.pendown then I.const !t.color else I.void in
  let outl = I.cut ~area:(`O !t.outline) path paintcol in
  let image = !t.image >> I.blend outl in
  t := { !t with image }
  
let set_size size =
  let outline = { !t.outline with P.width = size } in
  t := { !t with outline }

let pen_up () =
  t := { !t with pendown = false }

let pen_down () =
  t := { !t with pendown = true }

let clean_screen () =
  t := { !t with image = I.void }

open LogoEnv

let init env =
  set_pf env "render" LogoAtom.Lga.(opt word retvoid) render
