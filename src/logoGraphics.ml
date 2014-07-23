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
open LogoTypes
open LogoAtom
open LogoTurtle
open LogoEnv
  
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

let forward dist turtle =
  let dist = iexpr dist in
  move (float dist) turtle
    
let init env =
  set_pft1 env "forward" forward;
  set_pft1 env "fd" forward
