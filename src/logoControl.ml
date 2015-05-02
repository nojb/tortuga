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

(** 8. Control Structures *)

open LogoTypes
open LogoAtom
open LogoEnv
open LogoGlobals
open LogoEval

exception Throw of string * atom option
exception Toplevel
exception Bye
exception Pause of env

(** 8.1 Control *)

let run env args k =
  match args with
  | List args :: [] ->
      eval_list env (reparse args) k
  | _ ->
      error "run: bad arg list"

let is_int x = floor x = x

let repeat env args k =
  match args with
  | Num num :: List lst :: [] when is_int num ->
      let n = truncate num in
      let lst = reparse lst in
      let rec loop env i x =
        if i > n then k x
        else
          eval_list env lst (loop (step_repcount env) (i + 1))
      in
      loop (start_repcount env) 1 (Word "NIL")
  | _ ->
      error "repeat: bad arg list"

(* FIXME should be able to stop on STOP or THROW *)
let forever env args k =
  match args with
  | List lst :: [] ->
      let lst = reparse lst in
      let rec loop env =
        eval_list env lst (fun _ -> loop (step_repcount env))
      in
      loop (start_repcount env)
  | _ ->
      error "forever: bad arg list"

let repcount env args k =
  match args with
  | [] ->
      k (Num (float (LogoEnv.repcount env)))
  | _ ->
      error "repcount: bad arg list"

(* let eval_tf env tf k = *)
(*   match tf with *)
(*   | `L w -> k w *)
(*   | `R lst -> *)
(*       bool_expression env lst k *)

let is_true = function
  | Word "FALSE" -> false
  | _ -> true

let if_ env args k =
  match args with
  | tf :: List yay :: [] ->
      if is_true tf then
        eval_list env (reparse yay) k
      else
        k (Word "NIL")
  | tf :: List yay :: List nay :: [] ->
      if is_true tf then
        eval_list env (reparse yay) k
      else
        eval_list env (reparse nay) k
  | _ ->
      error "if: bad arg list"

let ifelse env args k =
  match args with
  | tf :: List yay :: List nay :: [] ->
      if is_true tf then
        eval_list env (reparse yay) k
      else
        eval_list env (reparse nay) k
  | _ ->
      error "ifelse: bad arg list"

let test env args k =
  match args with
  | arg :: [] ->
      set_test env (is_true arg);
      k (Word "NIL")
  | _ ->
      error "test: bad arg list"

let iftrue env args k =
  match args with
  | List args :: [] ->
      if get_test env then
        eval_list env (reparse args) k
      else
        k (Word "NIL")
  | _ ->
      error "iftrue: bad arg list"

let iffalse env args k =
  match args with
  | List args :: [] ->
      if get_test env then
        k (Word "NIL")
      else
        eval_list env (reparse args) k
  | _ ->
      error "iffalse: bad arg list"

let stop env args _ =
  match args with
  | [] ->
      output env (Word "NIL")
  | _ ->
      error "stop: bad arg list"

let output env args _ =
  match args with
  | arg :: [] ->
      output env arg
  | _ ->
      error "output: bad arg list"

(*
let catch env args k =
  let names = ["catch"] in
  let doc =

    "\
CATCH tag instructionlist

    Command or operation. Runs its second input. Outputs if that instructionlist
    outputs. If, while running the instructionlist, a THROW instruction is
    executed with a tag equal to the first input (case-insensitive comparison),
    then the running of the instructionlist is terminated immediately. In this
    case the CATCH outputs if a value input is given to THROW. The tag must be a
    word.

    If the tag is the word ERROR, then any error condition that arises during
    the running of the instructionlist has the effect of THROW \"ERROR instead
    of printing an error message and returning to toplevel. The CATCH does not
    output if an error is caught. Also, during the running of the
    instructionlist, the variable ERRACT is temporarily unbound. (If there is an
    error while ERRACT has a value, that value is taken as an instructionlist to
    be run after printing the error message. Typically the value of ERRACT, if
    any, is the list [PAUSE].)"

  in
  let args = Lga.(env @@ word @-> list any @-> ret cont) in
  let f env tag list k =
    try
      instructionlist env (reparse list) k
    with
    | Error _ when String.uppercase tag = "ERROR" ->
      (* TODO Also, during the running of the instructionlist, the variable ERRACT
         is temporarily unbound. (If there is an error while ERRACT has a value, that
         value is taken as an instructionlist to be run after printing the error
         message. Typically the value of ERRACT, if any, is the list [PAUSE].) *)
      k None
    | Throw (tag', v) when String.uppercase tag = String.uppercase tag' ->
      k v
  in
  prim ~names ~doc ~args ~f
*)

(*
let throw =
  let names = ["throw"] in
  let doc =

    "\
THROW tag
(THROW tag value)

    Command. Must be used within the scope of a CATCH with an equal tag. Ends
    the running of the instructionlist of the CATCH. If THROW is used with only
    one input, the corresponding CATCH does not output a value. If THROW is used
    with two inputs, the second provides an output for the CATCH.

    THROW \"TOPLEVEL can be used to terminate all running procedures and
    interactive pauses, and return to the toplevel instruction prompt. Typing
    the system interrupt character (<alt-S> for wxWidgets; otherwise normally
    <control-C> for Unix, <control-Q> for DOS, or <command-period> for Mac) has
    the same effect.

    THROW \"ERROR can be used to generate an error condition. If the error is
    not caught, it prints a message (THROW \"ERROR) with the usual indication of
    where the error (in this case the THROW) occurred. If a second input is used
    along with a tag of ERROR, that second input is used as the text of the
    error message instead of the standard message. Also, in this case, the
    location indicated for the error will be, not the location of the THROW, but
    the location where the procedure containing the THROW was invoked. This
    allows user-defined procedures to generate error messages as if they were
    primitives. Note: in this case the corresponding CATCH \"ERROR, if any, does
    not output, since the second input to THROW is not considered a return
    value.

    THROW \"SYSTEM immediately leaves Logo, returning to the operating system,
    without printing the usual parting message and without deleting any editor
    temporary file written by EDIT."

  in
  let args = Lga.(word @-> opt any retvoid) in
  let f tag value =
    if String.uppercase tag = "ERROR" then
      raise (Error (match value with Some value -> string_of_datum value | None -> ""))
    else if String.uppercase tag = "SYSTEM" then
      raise Bye
    else if String.uppercase tag = "TOPLEVEL" then
      raise Toplevel
    else
      raise (Throw (tag, value))
  in
  prim ~names ~doc ~args ~f

let pause =
  let names = ["pause"] in
  let doc =

    "\
PAUSE

    Command or operation. Enters an interactive pause. The user is prompted for
    instructions, as at toplevel, but with a prompt that includes the name of
    the procedure in which PAUSE was invoked. Local variables of that procedure
    are available during the pause. PAUSE outputs if the pause is ended by a
    CONTINUE with an input.

    If the variable ERRACT exists, and an error condition occurs, the contents
    of that variable are run as an instructionlist. Typically ERRACT is given
    the value [PAUSE] so that an interactive pause will be entered in the event
    of an error. This allows the user to check values of local variables at the
    time of the error.

    Typing the system quit character (<alt-S> for wxWidgets; otherwise normally
    <control-\\> for Unix, <control-W> for DOS, or <command-comma> for Mac) will
    also enter a pause."

  in
  let args = Lga.(env @@ ret cont) in
  let f env k = raise (Pause (new_continue env k)) in
  prim ~names ~doc ~args ~f

let continue =
  let names = ["continue"; "co"] in
  let doc =

    "\
CONTINUE value
CO value
(CONTINUE)
(CO)

    Command. Ends the current interactive pause, returning to the context of the
    PAUSE invocation that began it. If CONTINUE is given an input, that value is
    used as the output from the PAUSE. If not, the PAUSE does not output.

    Exceptionally, the CONTINUE command can be used without its default input
    and without parentheses provided that nothing follows it on the instruction
    line."

  in
  let args = Lga.(env @@ opt any cont) in
  let f env value _ = continue env value in
  prim ~names ~doc ~args ~f

let bye =
  let names = ["bye"] in
  let doc =

    "\
BYE

    Command. Exits from Logo; returns to the operating system."

  in
  let args = Lga.(void @@ ret retvoid) in
  let f () = raise Bye in
  prim ~names ~doc ~args ~f

let ignore =
  let names = ["ignore"] in
  let doc =

    "\
IGNORE value					(library procedure)

    Command. Does nothing. Used when an expression is evaluated for a side
    effect and its actual value is unimportant."

  in
  let args = Lga.(any @-> ret retvoid) in
  let f _ = () in
  prim ~names ~doc ~args ~f

let do_while =
  let names = ["do.while"] in
  let doc =

    "\
DO.WHILE instructionlist tfexpression		(library procedure)

    Command. Repeatedly evaluates the instructionlist as long as the evaluated
    remains TRUE. Evaluates the first input first, so the instructionlist is
    always run at least once. The tfexpression must be an expressionlist whose
    value when evaluated is TRUE or FALSE."

  in
  let args = Lga.(env @@ list any @-> list any @-> ret retvoid) in
  let f env list expr =
    let list = reparse list in
    let expr = reparse expr in
    let rec loop () =
      commandlist env list
        (fun () ->
           bool_expression env expr
             (function true -> loop () | false -> ()))
    in
    loop ()
  in
  prim ~names ~doc ~args ~f

let while_ =
  let names = ["while"] in
  let doc =

    "\
WHILE tfexpression instructionlist		(library procedure)

    Command. Repeatedly evaluates the instructionlist as long as the evaluated
    remains TRUE. Evaluates the first input first, so the instructionlist may
    never be run at all. The tfexpression must be an expressionlist whose value
    when evaluated is TRUE or FALSE."

  in
  let args = Lga.(env @@ list any @-> list any @-> ret retvoid) in
  let f env expr list =
    let expr = reparse expr in
    let list = reparse list in
    let rec loop () =
      bool_expression env expr
        (function true -> commandlist env list loop | false -> ())
    in
    loop ()
  in
  prim ~names ~doc ~args ~f

let do_until =
  let names = ["do.until"] in
  let doc =

    "\
DO.UNTIL instructionlist tfexpression		(library procedure)

    Command. Repeatedly evaluates the instructionlist as long as the evaluated
    remains FALSE. Evaluates the first input first, so the instructionlist is
    always run at least once. The tfexpression must be an expressionlist whose
    value when evaluated is TRUE or FALSE."

  in
  let args = Lga.(env @@ list any @-> list any @-> ret retvoid) in
  let f env list expr =
    let list = reparse list in
    let expr = reparse expr in
    let rec loop () =
      commandlist env list
        (fun () ->
           bool_expression env expr (function true -> () | false -> loop ()))
    in
    loop ()
  in
  prim ~names ~doc ~args ~f

let until =
  let names = ["until"] in
  let doc =

    "\
UNTIL tfexpression instructionlist		(library procedure)

    Command. Repeatedly evaluates the instructionlist as long as the evaluated
    remains FALSE. Evaluates the first input first, so the instructionlist may
    never be run at all. The tfexpression must be an expressionlist whose value
    when evaluated is TRUE or FALSE."

  in
  let args = Lga.(env @@ list any @-> list any @-> ret retvoid) in
  let f env expr list =
    let expr = reparse expr in
    let list = reparse list in
    let rec loop () =
      bool_expression env expr
        (function false -> commandlist env list loop | true -> ())
    in
    loop ()
  in
  prim ~names ~doc ~args ~f
*)

let () =
  add_pfcn "run" 1 run;
  add_pfcn "repeat" 2 repeat;
  add_pfcn "forever" 1 forever;
  add_pfcn "repcount" 0 repcount;
  add_pfcn "#" 0 repcount;
  add_pfcn "if" 2 if_;
  add_pfcn "ifelse" 3 ifelse;
  add_pfcn "test" 1 test;
  add_pfcn "iftrue" 1 iftrue;
  add_pfcn "iffalse" 1 iffalse;
  add_pfcn "stop" 0 stop;
  add_pfcn "output" 1 output
  (* List.iter add_prim
    [
      catch;
      throw;
      pause;
      continue;
      bye;
      ignore;
      do_while;
      while_;
      do_until;
      until
    ]
  *)
