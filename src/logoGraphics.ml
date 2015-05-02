(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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
open LogoGlobals

module type TURTLE = sig
  val get_heading : unit -> float
  val set_heading : float -> unit
  val get_pos : unit -> float * float
  val set_pos : float -> float -> unit
  val set_color : Gg.color -> unit
  val move : float -> unit
  val turn : float -> unit
  val arc : float -> float -> unit
  val set_size : float -> unit
  val pen_down : unit -> unit
  val pen_up : unit -> unit
  val clean_screen : unit -> unit
end

module Make (T : TURTLE) = struct

  (** 6.1 Turtle Motion *)

  let forward = function
    | Num dist ->
        T.move dist;
        Word "NIL"
    | _ ->
        error "forward: bad arg list"

  let back = function
    | Num dist ->
        T.move (-. dist);
        Word "NIL"
    | _ ->
        error "back: bad arg list"

  let left = function
    | Num deg ->
        T.turn (-. deg);
        Word "deg"
    | _ ->
        error "left: bad arg list"

  let right = function
    | Num deg ->
        T.turn deg;
        Word "NIL"
    | _ ->
        error "right: bad arg list"

  let setxy x y =
    match x, y with
    | Num x, Num y ->
        T.set_pos x y;
        Word "NIL"
    | _ ->
        error "setxy: bad arg list"

  let setx = function
    | Num x ->
        let _, y = T.get_pos () in
        T.set_pos x y;
        Word "NIL"
    | _ ->
        error "setx: bad arg list"

  let sety = function
    | Num y ->
        let x, _ = T.get_pos () in
        T.set_pos x y;
        Word "NIL"
    | _ ->
        error "sety: bad arg list"

  let setheading = function
    | Num h ->
        T.set_heading h;
        Word "NIL"
    | _ ->
        error "setheading: bad arg list"

  let home () =
    T.set_pos 0.0 0.0;
    T.set_heading 0.0;
    Word "NIL"

  let arc angle radius =
    match angle, radius with
    | Num angle, Num radius ->
        T.arc angle radius;
        Word "NIL"
    | _ ->
        error "arc: bad arg list"

  (** 6.2 Turtle Motion Queries *)

  let pos () =
    let x, y = T.get_pos () in
    List [Num x; Num y]

  let xcor () =
    let x, _ = T.get_pos () in
    Num x

  let ycor () =
    let _, y = T.get_pos () in
    Num y

  let heading () =
    Num (T.get_heading ())

  (** 6.3 Turtle and Window Control *)

  let clean () =
    T.clean_screen ();
    Word "NIL"

  let clearscreen () =
    T.set_pos 0.0 0.0;
    T.set_heading 0.0;
    T.clean_screen ();
    Word "NIL"

  (** 6.5 Pen and Background Control *)

  let pendown () =
    T.pen_down ();
    Word "NIL"

  let penup () =
    T.pen_up ();
    Word "NIL"

  let setpencolor = function
    | Word color :: [] ->
        begin match get_palette color with
        | Some c ->
            T.set_color c;
            Word "NIL"
        | None ->
            error "setpencolor: unknown color %s" color
        end
    | Num r :: Num g :: Num b :: [] ->
        let r = r /. 100.0 in
        let g = g /. 100.0 in
        let b = b /. 100.0 in
        T.set_color (Gg.Color.v_srgb r g b);
        Word "NIL"
    | _ ->
        error "setpencolor: bad arg list"

  (* TODO check that 0 <= r, g, b <= 100 *)
  let setpalette name r g b =
    match name, r, g, b with
    | Word name, Num r, Num g, Num b ->
        let r = r /. 100.0 in
        let g = g /. 100.0 in
        let b = b /. 100.0 in
        set_palette name (Gg.Color.v_srgb r g b)
    | _ ->
        error "setpalette: bad arg list"

  let setpensize = function
    | Num size ->
        T.set_size size;
        Word "NIL"
    | _ ->
        error "setpensize: bad arg list"

  (** 6.6 Pen Queries *)

  let palette = function
    | Word col ->
        begin match get_palette col with
        | Some c ->
            let r = Gg.Color.r c *. 100.0 in
            let g = Gg.Color.g c *. 100.0 in
            let b = Gg.Color.b c *. 100.0 in
            List [Num r; Num g; Num b]
        | None ->
            error "palette: color %s not found" col
        end
    | _ ->
        error "palette: bad arg list"

  let () =
    add_pf1 "forward" forward;
    add_pf1 "fd" forward;
    add_pf1 "back" back;
    add_pf1 "bk" back;
    add_pf1 "left" left;
    add_pf1 "lt" left;
    add_pf1 "right" right;
    add_pf1 "rt" right;
    add_pf2 "setxy" setxy;
    add_pf1 "setx" setx;
    add_pf1 "sety" sety;
    add_pf1 "setheading" setheading;
    add_pf0 "home" home;
    add_pf2 "arc" arc;

    add_pf0 "pos" pos;
    add_pf0 "xcor" xcor;
    add_pf0 "ycor" ycor;
    add_pf0 "heading" heading;

    add_pf0 "clean" clean;
    add_pf0 "clearscreen" clearscreen;

    add_pf0 "pendown" pendown;
    add_pf0 "pd" pendown;
    add_pf0 "penup" penup;
    add_pf0 "pu" penup;
    add_pfn "setpencolor" 1 setpencolor;
    (* add_pfn "setpc" setpencolor; *)
    (* add_pf4 "setpalette" setpalette; *)
    add_pf1 "setpensize" setpensize;

    add_pf1 "palette" palette

end
