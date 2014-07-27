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

let run =
  let names = ["run"] in
  let doc =

    "RUN instructionlist

Command or operation. Runs the Logo instructions in the input list; outputs if
the list contains an expression that outputs."

  in
  let args = Lga.(env @@ list any @-> ret cont) in
  let f env list k =
    instructionlist env (reparse list) k
  in
  prim ~names ~doc ~args ~f
    
let runresult =
  let names = ["runresult"] in
  let doc =

    "RUNRESULT instructionlist

Runs the instructions in the input; outputs an empty list if those instructions
produce no output, or a list whose only member is the output from running the
input instructionlist. Useful for inventing command-or-operation control
structures:
 	
  local \"result
  make \"result runresult [something]
  if emptyp :result [stop]
  output first :result"

  in
  let args = Lga.(env @@ list any @-> ret cont) in
  let f env list k =
    instructionlist env (reparse list)
      (function None -> k (Some (List [])) | Some a -> k (Some (List [a])))
  in
  prim ~names ~doc ~args ~f
    
let repeat =
  let names = ["repeat"] in
  let doc =

    "REPEAT num instructionlist

Command. Runs the instructionlist repeatedly, num times."

  in
  let args = Lga.(env @@ int @-> list any @-> ret cont) in
  let f env num list k =
    let rec loop i =
      if i > num then k None
      else
        instructionlist env (reparse list)
          (function
            | Some _ ->
              raise (Error "repeat: instruction list should not produce a value")
            | None -> loop (i+1))
    in
    loop 1
  in
  prim ~names ~doc ~args ~f

let forever =
  let names = ["forever"] in
  let doc =

    "FOREVER instructionlist

Command. Runs the \"instructionlist\" repeatedly, until something inside the
instructionlist (such as STOP or THROW) makes it stop."

  in
  let args = Lga.(env @@ list any @-> ret retvoid) in
  let f env list =
    let rec loop () =
      instructionlist env (reparse list)
        (function
          | Some _ -> raise (Error "forever: instruction list should not produce a value")
          | None -> loop ())
    in
    loop ()
  in
  prim ~names ~doc ~args ~f

let if_ =
  let names = ["if"] in
  let doc =

    "IF tf instructionlist
(IF tf instructionlist1 instructionlist2)

Command. If the first input has the value TRUE, then IF runs the second
input. If the first input has the value FALSE, then IF does nothing. (If given a
third input, IF acts like IFELSE, as described below.) It is an error if the
first input is not either TRUE or FALSE.

For compatibility with earlier versions of Logo, if an IF instruction is not
enclosed in parentheses, but the first thing on the instruction line after the
second input expression is a literal list (i.e., a list in square brackets), the
IF is treated as if it were IFELSE, but a warning message is given. If this
aberrant IF appears in a procedure body, the warning is given only the first
time the procedure is invoked in each Logo session."

  in
  let args = Lga.(env @@ any @-> list any @-> opt (list any) cont) in
  let f env tf iftrue iffalse k =
    match iffalse with
    | None ->
      if is_true tf then
        instructionlist env (reparse iftrue)
          (function
            | Some _ -> raise (Error "if: arguments should not produce a value")
            | None -> k None)
      else if is_false tf then
        k None
      else
        raise (Error "if: first argument should be either TRUE or FALSE")
    | Some iffalse ->
      if is_true tf then
        instructionlist env (reparse iftrue)
          (function
            | Some _ -> raise (Error "if: arguments should not produce a value")
            | None -> k None)
      else if is_false tf then
        instructionlist env (reparse iffalse)
          (function
            | Some _ -> raise (Error "if: arguments should not produce a value")
            | None -> k None)
      else
        raise (Error "if: first argument should be either TRUE or FALSE")
  in
  prim ~names ~doc ~args ~f

let ifelse =
  let names = ["ifelse"] in
  let doc =

    "IFELSE tf instructionlist1 instructionlist2

Command or operation. If the first input has the value TRUE, then IFELSE runs
the second input. If the first input has the value FALSE, then IFELSE runs the
third input. IFELSE outputs a value if the instructionlist contains an
expression that outputs a value."

  in
  let args = Lga.(env @@ any @-> list any @-> list any @-> ret cont) in
  let f env tf iftrue iffalse k =
    if is_true tf then
      instructionlist env (reparse iftrue) k
    else if is_false tf then
      instructionlist env (reparse iffalse) k
    else
      raise (Error "ifelse: first argument should be either TRUE or FALSE")
  in
  prim ~names ~doc ~args ~f
      
let stop =
  let names = ["stop"] in
  let doc =

    "STOP

Command. Ends the running of the procedure in which it appears. Control is
returned to the context in which that procedure was invoked. The stopped
procedure does not output a value."

  in
  let args = Lga.(env @@ ret cont) in
  let f env _ = output env None in
  prim ~names ~doc ~args ~f
             
let output =
  let names = ["output"; "op"] in
  let doc =

    "OUTPUT value
OP value

Command. Ends the running of the procedure in which it appears. That procedure
outputs the value value to the context in which it was invoked. Don't be
confused: OUTPUT itself is a command, but the procedure that invokes OUTPUT is
an operation."

  in
  let args = Lga.(env @@ any @-> ret cont) in
  let f env thing _ = output env (Some thing) in
  prim ~names ~doc ~args ~f

let catch =
  let names = ["catch"] in
  let doc =

    "CATCH tag instructionlist

Command or operation. Runs its second input. Outputs if that instructionlist
outputs. If, while running the instructionlist, a THROW instruction is executed
with a tag equal to the first input (case-insensitive comparison), then the
running of the instructionlist is terminated immediately. In this case the CATCH
outputs if a value input is given to THROW. The tag must be a word.

If the tag is the word ERROR, then any error condition that arises during the
running of the instructionlist has the effect of THROW \"ERROR instead of
printing an error message and returning to toplevel. The CATCH does not output
if an error is caught. Also, during the running of the instructionlist, the
variable ERRACT is temporarily unbound. (If there is an error while ERRACT has a
value, that value is taken as an instructionlist to be run after printing the
error message. Typically the value of ERRACT, if any, is the list [PAUSE].)"

  in
  let args = Lga.(env @@ word @-> list any @-> ret cont) in
  let f env tag list k =
    try
      instructionlist env (reparse list) k
    with
      Error _ when String.uppercase tag = "ERROR" ->
      (* TODO Also, during the running of the instructionlist, the variable ERRACT
         is temporarily unbound. (If there is an error while ERRACT has a value, that
         value is taken as an instructionlist to be run after printing the error
         message. Typically the value of ERRACT, if any, is the list [PAUSE].) *)
      k None
    | Throw (tag', v) when String.uppercase tag = String.uppercase tag' ->
      k v
  in
  prim ~names ~doc ~args ~f

let throw =
  let names = ["throw"] in
  let doc =

    "THROW tag
(THROW tag value)

Command. Must be used within the scope of a CATCH with an equal tag. Ends the
running of the instructionlist of the CATCH. If THROW is used with only one
input, the corresponding CATCH does not output a value. If THROW is used with
two inputs, the second provides an output for the CATCH.

THROW \"TOPLEVEL can be used to terminate all running procedures and interactive
pauses, and return to the toplevel instruction prompt. Typing the system
interrupt character (<alt-S> for wxWidgets; otherwise normally <control-C> for
Unix, <control-Q> for DOS, or <command-period> for Mac) has the same effect.

THROW \"ERROR can be used to generate an error condition. If the error is not
caught, it prints a message (THROW \"ERROR) with the usual indication of where
the error (in this case the THROW) occurred. If a second input is used along
with a tag of ERROR, that second input is used as the text of the error message
instead of the standard message. Also, in this case, the location indicated for
the error will be, not the location of the THROW, but the location where the
procedure containing the THROW was invoked. This allows user-defined procedures
to generate error messages as if they were primitives. Note: in this case the
corresponding CATCH \"ERROR, if any, does not output, since the second input to
THROW is not considered a return value.

THROW \"SYSTEM immediately leaves Logo, returning to the operating system,
without printing the usual parting message and without deleting any editor
temporary file written by EDIT."

  in
  let args = Lga.(word @-> opt any retvoid) in
  let f tag value =
    if String.uppercase tag = "ERROR" then
      raise (Error (match value with Some value -> sprint () value | None -> ""))
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

    "PAUSE

Command or operation. Enters an interactive pause. The user is prompted for
instructions, as at toplevel, but with a prompt that includes the name of the
procedure in which PAUSE was invoked. Local variables of that procedure are
available during the pause. PAUSE outputs if the pause is ended by a CONTINUE
with an input.

If the variable ERRACT exists, and an error condition occurs, the contents of
that variable are run as an instructionlist. Typically ERRACT is given the value
[PAUSE] so that an interactive pause will be entered in the event of an
error. This allows the user to check values of local variables at the time of
the error.

Typing the system quit character (<alt-S> for wxWidgets; otherwise normally
<control-\\> for Unix, <control-W> for DOS, or <command-comma> for Mac) will also
enter a pause."

  in
  let args = Lga.(env @@ ret cont) in
  let f env k = raise (Pause (new_continue env k)) in
  prim ~names ~doc ~args ~f

let continue =
  let names = ["continue"; "co"] in
  let doc =

    "CONTINUE value
CO value
(CONTINUE)
(CO)

Command. Ends the current interactive pause, returning to the context of the
PAUSE invocation that began it. If CONTINUE is given an input, that value is
used as the output from the PAUSE. If not, the PAUSE does not output.

Exceptionally, the CONTINUE command can be used without its default input and
without parentheses provided that nothing follows it on the instruction line."

  in
  let args = Lga.(env @@ opt any cont) in
  let f env value _ = continue env value in
  prim ~names ~doc ~args ~f

let bye =
  let names = ["bye"] in
  let doc =

    "BYE

Command. Exits from Logo; returns to the operating system."

  in
  let args = Lga.(void @@ ret retvoid) in
  let f () = raise Bye in
  prim ~names ~doc ~args ~f

let ignore =
  let names = ["ignore"] in
  let doc =

    "IGNORE value					(library procedure)

Command. Does nothing. Used when an expression is evaluated for a side effect
and its actual value is unimportant."

  in
  let args = Lga.(any @-> ret retvoid) in
  let f _ = () in
  prim ~names ~doc ~args ~f

let do_while =
  let names = ["do.while"] in
  let doc =

    "DO.WHILE instructionlist tfexpression		(library procedure)

Command. Repeatedly evaluates the instructionlist as long as the evaluated
remains TRUE. Evaluates the first input first, so the instructionlist is always
run at least once. The tfexpression must be an expressionlist whose value when
evaluated is TRUE or FALSE."

  in
  let args = Lga.(env @@ list any @-> list any @-> ret retvoid) in
  let f env list expr =
    let list = parse (sprint_list () list) in
    let expr = parse (sprint_list () expr) in
    let rec loop () =
      instructionlist env (Stream.of_list list)
        (function
            None ->
            expression env (Stream.of_list expr)
              (fun a ->
                 if is_true a then loop ()
                 else if is_false a then ()
                 else error "do.while condition should produce true or false")
          | Some a -> error "do.while should not produce a value, got %a" sprint a)
    in
    loop ()
  in
  prim ~names ~doc ~args ~f

let while_ =
  let names = ["while"] in
  let doc =

    "WHILE tfexpression instructionlist		(library procedure)

Command. Repeatedly evaluates the instructionlist as long as the evaluated
remains TRUE. Evaluates the first input first, so the instructionlist may never
be run at all. The tfexpression must be an expressionlist whose value when
evaluated is TRUE or FALSE."

  in
  let args = Lga.(env @@ list any @-> list any @-> ret retvoid) in
  let f env expr list =
    let expr = parse (sprint_list () expr) in
    let list = parse (sprint_list () list) in
    let rec loop () =
      expression env (Stream.of_list expr) (fun a ->
          if is_true a then
            instructionlist env (Stream.of_list list) (function
                | None -> loop ()
                | Some a ->
                  error "WHILE should not produce a value, got %a" sprint a)
          else if is_false a then
            ()
          else
            error "WHILE condition should produce true or false")
    in
    loop ()
  in
  prim ~names ~doc ~args ~f

let do_until =
  let names = ["do.until"] in
  let doc =

    "DO.UNTIL instructionlist tfexpression		(library procedure)

Command. Repeatedly evaluates the instructionlist as long as the evaluated
remains FALSE. Evaluates the first input first, so the instructionlist is always
run at least once. The tfexpression must be an expressionlist whose value when
evaluated is TRUE or FALSE."

  in
  let args = Lga.(env @@ list any @-> list any @-> ret retvoid) in
  let f env list expr =
    let list = parse (sprint_list () list) in
    let expr = parse (sprint_list () expr) in
    let rec loop () =
      instructionlist env (Stream.of_list list)
        (function
            None ->
            expression env (Stream.of_list expr)
              (fun a ->
                 if is_true a then ()
                 else if is_false a then loop ()
                 else error "do.until condition should produce true or false")
          | Some a -> error "do.until should not produce a value, got %a" sprint a)
    in
    loop ()
  in
  prim ~names ~doc ~args ~f

let until =
  let names = ["until"] in
  let doc =

    "UNTIL tfexpression instructionlist		(library procedure)

Command. Repeatedly evaluates the instructionlist as long as the evaluated
remains FALSE. Evaluates the first input first, so the instructionlist may never
be run at all. The tfexpression must be an expressionlist whose value when
evaluated is TRUE or FALSE."

  in
  let args = Lga.(env @@ list any @-> list any @-> ret retvoid) in
  let f env expr list =
    let expr = parse (sprint_list () expr) in
    let list = parse (sprint_list () list) in
    let rec loop () =
      expression env (Stream.of_list expr) (fun a ->
          if is_false a then
            instructionlist env (Stream.of_list list) (function
                | None -> loop ()
                | Some a ->
                  error "UNTIL should not produce a value, got %a" sprint a)
          else if is_true a then
            ()
          else
            error "UNTIL condition should produce true or false")
    in
    loop ()
  in
  prim ~names ~doc ~args ~f
      
let () =
  List.iter add_prim
    [
      run;
      runresult;
      repeat;
      forever;
      if_;
      ifelse;
      stop;
      output;
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
