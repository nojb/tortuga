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

(** 7. Workspace Management *)

open LogoTypes
open LogoAtom
open LogoEnv
  
(** 7.2 Variable Definition *)

let make env varname value =
  set_var env varname value
  
let name env value varname =
  set_var env varname value

(* TODO 'local' now accepts the form
   (local varlist varname2 varname3 ...)
   which should be marked as an error. *)
let local env varname rest =
  let create_var env w =
    try
      create_var env w
    with
    | Failure "create_var" ->
      error "'local' used outside of any procedure"
  in
  begin match varname with
  | `L w ->
    create_var env w
  | `R wl ->
    List.iter (create_var env) wl
  end;
  List.iter (create_var env) rest

let localmake env varname value =
  let create_var env w =
    try
      create_var env w
    with
    | Failure "create_var" ->
      error "'localmake' used outside of any procedure"
  in
  create_var env varname;
  set_var env varname value

let thing env varname =
  get_var env varname

(** 7.4 Workspace Predicates *)

let definedp env name =
  if has_routine env name then true_word else false_word

let namep env name =
  if has_var env name then true_word else false_word
  
let init env =
  set_pf env "make" Lga.(env @@ word @-> any @-> ret retvoid) make;
  set_pf env "name" Lga.(env @@ any @-> word @-> ret retvoid) name;
  set_pf env "local" Lga.(env @@ alt word (list word) @-> rest word retvoid) local;
  set_pf env "localmake" Lga.(env @@ word @-> any @-> ret retvoid) localmake;
  set_pf env "thing" Lga.(env @@ word @-> ret (value any)) thing;

  set_pf env "definedp" Lga.(env @@ word @-> ret (value any)) definedp;
  set_pf env "defined?" Lga.(env @@ word @-> ret (value any)) definedp;
  set_pf env "namep" Lga.(env @@ word @-> ret (value any)) namep;
  set_pf env "name?" Lga.(env @@ word @-> ret (value any)) namep
  
