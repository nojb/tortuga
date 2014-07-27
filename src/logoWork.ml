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
open LogoGlobals
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

let definedp name =
  if has_routine name then true_word else false_word

let namep env name =
  if has_var env name then true_word else false_word

(** 7.7 Workspace Control *)

(*
def puts_columns items, star_items=[]
  return if items.empty?

  if star_items && star_items.any?
    items = items.map{|item| star_items.include?(item) ? "#{item}*" : item}
  end

  if $stdout.tty?
    # determine the best width to display for different console sizes
    console_width = `/bin/stty size`.chomp.split(" ").last.to_i
    console_width = 80 if console_width <= 0
    longest = items.sort_by { |item| item.length }.last
    optimal_col_width = (console_width.to_f / (longest.length + 2).to_f).floor
    cols = optimal_col_width > 1 ? optimal_col_width : 1

    IO.popen("/usr/bin/pr -#{cols} -t -w#{console_width}", "w"){|io| io.puts(items) }
  else
    puts items
  end
end
*)

let help = function
  | None ->
    iter_routines print_endline
  | Some name ->
    match get_help name with
    | Some doc ->
      print_endline doc
    | None ->
      print_endline ("No help is available for " ^ name)
  
let () =
  set_pf "make" Lga.(env @@ word @-> any @-> ret retvoid) make;
  set_pf "name" Lga.(env @@ any @-> word @-> ret retvoid) name;
  set_pf "local" Lga.(env @@ alt word (list word) @-> rest word retvoid) local;
  set_pf "localmake" Lga.(env @@ word @-> any @-> ret retvoid) localmake;
  set_pf "thing" Lga.(env @@ word @-> ret (value any)) thing;

  set_pf "definedp" Lga.(word @-> ret (value any)) definedp;
  set_pf "defined?" Lga.(word @-> ret (value any)) definedp;
  set_pf "namep" Lga.(env @@ word @-> ret (value any)) namep;
  set_pf "name?" Lga.(env @@ word @-> ret (value any)) namep;

  set_pf "help" Lga.(opt word retvoid) help
