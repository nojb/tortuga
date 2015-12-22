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
open LogoPrint
open LogoEnv

(** 7.2 Variable Definition *)

let make = function
  | Atom (Word id) :: e :: [] ->
      Make (id, e)
  | _ ->
      failwith "MAKE: bad args"

(*
  let names = ["make"] in
  let doc =

    "\
MAKE varname value

    Command. Assigns the value value to the variable named varname, which must
    be a word. Variable names are case-insensitive. If a variable with the same
    name already exists, the value of that variable is changed. If not, a new
    global variable is created."

  in
  let args = Lga.(env @@ word @-> any @-> ret retvoid) in
  let f env varname value = set_var env varname value in
  prim ~names ~doc ~args ~f

let name =
  let names = ["name"] in
  let doc =

    "\
NAME value varname				(library procedure)

    Command. Same as MAKE but with the inputs in reverse order."

  in
  let args = Lga.(env @@ any @-> word @-> ret retvoid) in
  let f env value varname = set_var env varname value in
  prim ~names ~doc ~args ~f

(* TODO 'local' now accepts the form
   (local varlist varname2 varname3 ...)
   which should be marked as an error. *)
let local =
  let names = ["local"] in
  let doc =

    "\
LOCAL varname
LOCAL varnamelist
(LOCAL varname1 varname2 ...)

    Command. Accepts as inputs one or more words, or a list of words. A variable
    is created for each of these words, with that word as its name. The
    variables are local to the currently running procedure. Logo variables
    follow dynamic scope rules; a variable that is local to a procedure is
    available to any subprocedure invoked by that procedure. The variables
    created by LOCAL have no initial value; they must be assigned a value (e.g.,
    with MAKE) before the procedure attempts to read their value."

  in
  let args = Lga.(env @@ alt word (list word) @-> rest word retvoid) in
  let f env varname rest =
    let create_var env w =
      try
        create_var env w None
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
  in
  prim ~names ~doc ~args ~f

let localmake =
  let names = ["localmake"] in
  let doc =

    "\
LOCALMAKE varname value				(library procedure)

    Command. Makes the named variable local, like LOCAL, and assigns it the
    given value, like MAKE."

  in
  let args = Lga.(env @@ word @-> any @-> ret retvoid) in
  let f env varname value =
    let create_var env w v =
      try
        create_var env w v
      with
      | Failure "create_var" ->
        error "'localmake' used outside of any procedure"
    in
    create_var env varname (Some value)
  in
  prim ~names ~doc ~args ~f

let thing =
  let names = ["thing"] in
  let doc =

    "\
THING varname
:quoted.varname

    Outputs the value of the variable whose name is the input. If there is more
    than one such variable, the innermost local variable of that name is
    chosen. The colon notation is an abbreviation not for THING but for the
    combination

      thing \"

    so that :FOO means THING \"FOO."

  in
  let args = Lga.(env @@ word @-> ret (value any)) in
  let f env varname = get_var env varname in
  prim ~names ~doc ~args ~f

(** 7.3 Property Lists *)

let pprop =
  let names = ["pprop"] in
  let doc =

    "\
PPROP plistname propname value

    Command. Adds a property to the plistname property list with name propname
    and value value."

  in
  let args = Lga.(word @-> word @-> any @-> ret retvoid) in
  let f = put_prop in
  prim ~names ~doc ~args ~f

let gprop =
  let names = ["gprop"] in
  let doc =

    "\
GPROP plistname propname

    Outputs the value of the propname property in the plistname property list,
    or the empty list if there is no such property."

  in
  let args = Lga.(word @-> word @-> ret (value any)) in
  let f plistname propname =
    match get_prop plistname propname with
    | Some a -> a
    | None -> List []
  in
  prim ~names ~doc ~args ~f

let remprop =
  let names = ["remprop"] in
  let doc =

    "\
REMPROP plistname propname

    Command. Removes the property named propname from the property list named plistname."

  in
  let args = Lga.(word @-> word @-> ret retvoid) in
  let f = remove_prop in
  prim ~names ~doc ~args ~f

let plist =
  let names = ["plist"] in
  let doc =

    "\
PLIST plistname

    Outputs a list whose odd-numbered members are the names, and whose
    even-numbered members are the values, of the properties in the property list
    named plistname. The output is a copy of the actual property list; changing
    properties later will not magically change a list output earlier by PLIST."

  in
  let args = Lga.(word @-> ret (value (list any))) in
  let f plistname =
    let lst = prop_list plistname in
    List.concat (List.map (fun (n, v) -> [Word n; v]) lst)
  in
  prim ~names ~doc ~args ~f

(** 7.4 Workspace Predicates *)

let definedp =
  let names = ["definedp"; "defined?"] in
  let doc =

    "\
DEFINEDP name
DEFINED? name

    Outputs TRUE if the input is the name of a user-defined procedure, including
    a library procedure."

  in
  let args = Lga.(word @-> ret (value any)) in
  let f name = if has_routine name then true_word else false_word in
  prim ~names ~doc ~args ~f

let namep =
  let names = ["namep"; "name?"] in
  let doc =

    "\
NAMEP name
NAME? name

    Outputs TRUE if the input is the name of a variable."

  in
  let args = Lga.(env @@ word @-> ret (value any)) in
  let f env name = if has_var env name then true_word else false_word in
  prim ~names ~doc ~args ~f

let plistp =
  let names = ["plistp"; "plist?"] in
  let doc =

    "\
PLISTP name
PLIST? name

    Outputs TRUE if the input is the name of a nonempty property list. (In
    principle every word is the name of a property list; if you haven't put any
    properties in it, PLIST of that name outputs an empty list, rather than
    giving an error message.)"

  in
  let args = Lga.(word @-> ret (value any)) in
  let f plistname =
    if has_plist plistname then true_word else false_word
  in
  prim ~names ~doc ~args ~f

(** 7.7 Workspace Control *)

let help =
  let names = ["help"] in
  let doc =

    "\
HELP name
(HELP)

    Command. Prints information from the reference manual about the primitive
    procedure named by the input. With no input, lists all the primitives about
    which help is available. If there is an environment variable LOGOHELP, then
    its value is taken as the directory in which to look for help files, instead
    of the default help directory.

    If HELP is called with the name of a defined procedure for which there is no
    help file, it will print the title line of the procedure followed by lines
    from the procedure body that start with semicolon, stopping when a
    non-semicolon line is seen.

    Exceptionally, the HELP command can be used without its default input and
    without parentheses provided that nothing follows it on the instruction
    line."

  in
  let args = Lga.(opt word retvoid) in
  let f name =
    match name with
    | None ->
      let rs = fold_routines (fun x l -> x :: l) [] in
      let l = List.length rs in
      if l > 0 then begin
        let a = Array.of_list rs in
        Array.sort (fun s1 s2 -> String.length s1 - String.length s2) a;
        let last = a.(l-1) in
        let cols = truncate (80.0 /. float (String.length last + 3)) in
        let cols = max 1 cols in
        let nc = l / cols in
        Array.sort compare a;
        for i = 0 to nc - 1 do
          for j = 0 to cols - 1 do
            let k = j * nc + i in
            if k < l then cprintf "%-*s" (String.length last + 3) a.(k)
          done;
          cprint "\n"
        done
      end
    | Some name ->
      match get_help name with
      | Some doc ->
        cprintl doc
      | None ->
        cprintlf "No help is available for %s" name
  in
  prim ~names ~doc ~args ~f

let () =
  List.iter add_prim
    [
      make;
      name;
      local;
      localmake;
      thing;

      pprop;
      gprop;
      remprop;
      plist;

      definedp;
      namep;
      plistp;

      help
    ]
*)

let () =
  add_pr2 "make" make
