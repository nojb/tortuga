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

(** 2. Data Structures Primitives *)

open LogoTypes
open LogoAtom
open LogoGlobals

(** 2.1 Constructors *)

let word =
  let names = ["word"] in
  let doc =

    "WORD word1 word2
(WORD word1 word2 word3 ...)

Outputs a word formed by concatenating its inputs."

  in
  let args = Lga.(word @-> word @-> rest word (value word)) in
  let f word1 word2 words =
    String.concat "" (word1 :: word2 :: words)
  in
  prim ~names ~doc ~args ~f
    
let list =
  let names = ["list"] in
  let doc =

    "LIST thing1 thing2
(LIST thing1 thing2 thing3 ...)

Outputs a list whose members are its inputs, which can be any Logo datum (word,
list, or array)."

  in
  let args = Lga.(any @-> any @-> rest any (value (list any))) in
  let f thing1 thing2 things =
    thing1 :: thing2 :: things
  in
  prim ~names ~doc ~args ~f

let sentence =
  let names = ["sentence"; "se"] in
  let doc =

    "SENTENCE thing1 thing2
SE thing1 thing2
(SENTENCE thing1 thing2 thing3 ...)
(SE thing1 thing2 thing3 ...)

Outputs a list whose members are its inputs, if those inputs are not lists, or
the members of its inputs, if those inputs are lists."

  in
  let args = Lga.(any @-> any @-> rest any (value (list any))) in
  let f thing1 thing2 things =
    List.concat (List.map (function List l -> l | _ as a -> [a]) (thing1 :: thing2 :: things))
  in
  prim ~names ~doc ~args ~f

let fput thing list =
  match thing, list with
  | _, List l -> List (thing :: l)
  | Array _, _
  | _, Array _ ->
    raise (Error "fput: bad types")
  | _ ->
    let s1 = sexpr thing in
    let s2 = sexpr list in
    if String.length s1 = 1 then
      Word (s1 ^ s2)
    else
      raise (Error "fput: first arg must be a character")

let fput_doc =
  "fput THING LIST

Outputs LIST with one extra member, THING, at the beginning.  If LIST is a word,
then THING must be a one-letter word, and 'fput THING LIST' is equivalent to
'word THING LIST'."

let lput thing list =
  match thing, list with
  | _, List l -> List (l @ [thing])
  | Array _, _
  | _, Array _ ->
    raise (Error "lput: bad types")
  | _ ->
    let s1 = sexpr thing in
    let s2 = sexpr list in
    if String.length s1 = 1 then
      Word (s2 ^ s1)
    else
      raise (Error "lput: first arg must be a character")

let lput_doc =
  "lput THING LIST

Outputs LIST with one extra member, THING, at the end.  If LIST is a word, then
THING must be a one-letter word, and 'lput THING LIST' is equivalent to 'word
LIST THING'."

let array =
  let names = ["array"] in
  let doc =

    "ARRAY size
(ARRAY size origin)

Outputs an array of size members (must be a positive integer), each of which
initially is an empty list. Array members can be selected with ITEM and changed
with SETITEM. The first member of the array is member number 1 unless an origin
input (must be an integer) is given, in which case the first member of the array
has that number as its index. (Typically 0 is used as the origin if anything.)
Arrays are printed by PRINT and friends, and can be typed in, inside curly
braces; indicate an origin with {a b c}@0."

  in
  let args = Lga.(int @-> opt int (value any)) in
  let f size origin =
    let origin = match origin with
      | None -> 1
      | Some origin -> origin
    in
    Array (Array.create size (List []), origin)
  in
  prim ~names ~doc ~args ~f

let combine thing1 thing2 =
  match thing1, thing2 with
  | _, List l -> List (thing1 :: l)
  | _, Array _
  | Array _, _ ->
    raise (Error "combine: bad types")
  | _ ->
    let s1 = sexpr thing1 in
    let s2 = sexpr thing2 in
    Word (s1 ^ s2)

let listtoarray =
  let names = ["listtoarray"] in
  let doc =

    "LISTTOARRAY list
(LISTTOARRAY list origin)

Outputs an array of the same size as the input list, whose members are the
members of the input list."

  in
  let args = Lga.(list any @-> opt int (value (array any))) in
  let f list origin =
    let origin = match origin with
      | None -> 1
      | Some origin -> origin
    in
    (Array.of_list list, origin)
  in
  prim ~names ~doc ~args ~f

let arraytolist =
  let names = ["arraytolist"] in
  let doc =

    "ARRAYTOLIST array

Outputs a list whose members are the members of the input array. The first
member of the output is the first member of the array, regardless of the array's
origin."

  in
  let args = Lga.(array any @-> ret (value (list any))) in
  let f (a, _) =
    Array.to_list a
  in
  prim ~names ~doc ~args ~f

let reverse =
  let names = ["reverse"] in
  let doc =

    "REVERSE list					(library procedure)

Outputs a list whose members are the members of the input list, in reverse
order."

  in
  let args = Lga.(list any @-> ret (value (list any))) in
  let f = List.rev in
  prim ~names ~doc ~args ~f

let gensym =
  let count = ref 0 in
  let names = ["gensym"] in
  let doc =

    "GENSYM						(library procedure)

Outputs a unique word each time it's invoked. The words are of the form G1, G2,
etc."

  in
  let args = Lga.(void @@ ret (value word)) in
  let f () =
    incr count;
    "G" ^ string_of_int !count
  in
  prim ~names ~doc ~args ~f

(** 2.2 Data Selectors *)

let first_aux = function
  | Num n ->
    Word (String.make 1 (string_of_float n).[0])
  | Word "" ->
    raise (Error "first: empty word")
  | Word w ->
    Word (String.make 1 w.[0])
  | List [] ->
    raise (Error "first: empty list")
  | List (x :: _) ->
    x
  | Array (_, orig) ->
    Num (float orig)

let first =
  let names = ["first"] in
  let doc =

    "FIRST thing

If the input is a word, outputs the first character of the word. If the input is
a list, outputs the first member of the list. If the input is an array, outputs
the origin of the array (that is, the index of the first member of the array)."

  in
  let args = Lga.(any @-> ret (value any)) in
  let f = first_aux in
  prim ~names ~doc ~args ~f

let firsts =
  let names = ["firsts"] in
  let doc =

    "FIRSTS list

Outputs a list containing the FIRST of each member of the input list. It is an
error if any member of the input list is empty. (The input itself may be empty,
in which case the output is also empty.) This could be written as
 	
  to firsts :list
    output map \"first :list
  end

but is provided as a primitive in order to speed up the iteration tools MAP,
MAP.SE, and FOREACH.
	
  to transpose :matrix
    if emptyp first :matrix [op []]
    op fput firsts :matrix transpose bfs :matrix
  end"

  in
  let args = Lga.(list any @-> ret (value (list any))) in
  let f l = List.map first_aux l in
  prim ~names ~doc ~args ~f

let last =
  let names = ["last"] in
  let doc =

    "LAST wordorlist

If the input is a word, outputs the last character of the word. If the input is
a list, outputs the last member of the list."

  in
  let args = Lga.(alt word (ne_list any) @-> ret (value any)) in
  let f = function
    | `L w ->
      Word (String.make 1 w.[String.length w - 1])
    | `R lst ->
      let l = List.length lst in
      List.nth lst (l-1)
  in
  prim ~names ~doc ~args ~f

let butfirst =
  let names = ["butfirst"; "bf"] in
  let doc =

    "BUTFIRST wordorlist
BF wordorlist

If the input is a word, outputs a word containing all but the first character of
the input. If the input is a list, outputs a list containing all but the first
member of the input."
      
  in
  let args = Lga.(alt word (ne_list any) @-> ret (value any)) in
  let f = function
    | `L w ->
      let l = String.length w in
      Word (String.sub w 1 (l-1))
    | `R l ->
      List (List.tl l)
  in
  prim ~names ~doc ~args ~f
    
let item =
  let names = ["item"] in
  let doc =

    "ITEM index thing

If the thing is a word, outputs the indexth character of the word. If the thing
is a list, outputs the indexth member of the list. If the thing is an array,
outputs the indexth member of the array. Index starts at 1 for words and lists;
the starting index of an array is specified when the array is created."

  in
  let args = Lga.(int @-> any @-> ret (value any)) in
  let f index = function
    | Num n ->
      let s = string_of_float n in
      Word (String.make 1 s.[index-1])
    | Word w ->
      Word (String.make 1 w.[index-1])
    | List l ->
      List.nth l (index-1)
    | Array (a, orig) ->
      a.(index-orig)
  in
  prim ~names ~doc ~args ~f

let pick =
  let names = ["pick"] in
  let doc =

    "PICK list					(library procedure)

Outputs a randomly chosen member of the input list."

  in
  let args = Lga.(ne_list any @-> ret (value any)) in
  let f l = List.nth l (Random.int (List.length l)) in
  prim ~names ~doc ~args ~f

let quoted =
  let names = ["quoted"] in
  let doc =

    "QUOTED thing					(library procedure)

Outputs its input, if a list; outputs its input with a quotation mark prepended,
if a word."

  in
  let args = Lga.(alt word (list any) @-> ret (value any)) in
  let f = function
    | `L s -> Word ("\"" ^ s)
    | `R l -> List l
  in
  prim ~names ~doc ~args ~f

(** 2.3 Data Mutators *)

(** 2.4 Predicates *)

let equalp =
  let names = ["equalp"; "equal?"] in
  let doc =

    "EQUALP thing1 thing2
EQUAL? thing1 thing2
thing1 = thing2

Outputs TRUE if the inputs are equal, FALSE otherwise. Two numbers are equal if
they have the same numeric value. Two non-numeric words are equal if they
contain the same characters in the same order. If there is a variable named
CASEIGNOREDP whose value is TRUE, then an upper case letter is considered the
same as the corresponding lower case letter. (This is the case by default.) Two
lists are equal if their members are equal. An array is only equal to itself;
two separately created arrays are never equal even if their members are
equal. (It is important to be able to know if two expressions have the same
array as their value because arrays are mutable; if, for example, two variables
have the same array as their values then performing SETITEM on one of them will
also change the other.)"

  in
  let args = Lga.(any @-> any @-> ret (value any)) in
  let f a b =
    if equalaux a b then true_word else false_word
  in
  prim ~names ~doc ~args ~f

let notequalp =
  let names = ["notequalp"; "notequal?"] in
  let doc =

    "NOTEQUALP thing1 thing2
NOTEQUAL? thing1 thing2
thing1 <> thing2

Outputs FALSE if the inputs are equal, TRUE otherwise. See EQUALP for the
meaning of equality for different data types."

  in
  let args = Lga.(any @-> any @-> ret (value any)) in
  let f a b =
    if equalaux a b then false_word else true_word
  in
  prim ~names ~doc ~args ~f

let beforep =
  let names = ["beforep"; "before?"] in
  let doc =

    "BEFOREP word1 word2
BEFORE? word1 word2

Outputs TRUE if word1 comes before word2 in ASCII collating sequence (for words
of letters, in alphabetical order). Case-sensitivity is determined by the value
of CASEIGNOREDP. Note that if the inputs are numbers, the result may not be the
same as with LESSP; for example, BEFOREP 3 12 is false because 3 collates after
1."

  in
  let args = Lga.(word @-> word @-> ret (value any)) in
  let f a b =
    if a < b then true_word else false_word
  in
  prim ~names ~doc ~args ~f

let _eq =
  let names = [".eq"] in
  let doc =

    ".EQ thing1 thing2

Outputs TRUE if its two inputs are the same datum, so that applying a mutator to
one will change the other as well. Outputs FALSE otherwise, even if the inputs
are equal in value.

WARNING: Primitives whose names start with a period are dangerous. Their use by
non-experts is not recommended. The use of mutators can lead to circular data
structures, infinite loops, or Logo crashes."

  in
  let args = Lga.(any @-> any @-> ret (value any)) in
  let f a b =
    if a == b then true_word else false_word
  in
  prim ~names ~doc ~args ~f

let numberp =
  let names = ["numberp"; "number?"] in
  let doc =

    "NUMBERP thing
NUMBER? thing

Outputs TRUE if the input is a number, FALSE otherwise."

  in
  let args = Lga.(any @-> ret (value any)) in
  let f = function
    | Num _ ->
      true_word
    | Word n ->
      if isnumber n then true_word else false_word
    | _ ->
      false_word
  in
  prim ~names ~doc ~args ~f

(** 2.5 Queries *)

let count =
  let names = ["count"] in
  let doc =

    "COUNT thing

Outputs the number of characters in the input, if the input is a word; outputs
the number of members in the input, if it is a list or an array. (For an array,
this may or may not be the index of the last member, depending on the array's
origin.)"

  in
  let args = Lga.(any @-> ret (value int)) in
  let f = function
    | Num n -> String.length (Printf.sprintf "%g" n)
    | Word w -> String.length w
    | List l -> List.length l
    | Array (a, _) -> Array.length a
  in
  prim ~names ~doc ~args ~f

let () =
  List.iter add_prim
    [
      word;
      list;
      sentence;
      (* fput *)
      (* lput *)
      array;
      listtoarray;
      arraytolist;
      (* combine *)
      reverse;
      gensym;

      first;
      firsts;
      last;
      butfirst;
      (* butfirsts *)
      (* butlast *)
      item;
      pick;
      quoted;

      equalp;
      notequalp;
      beforep;
      _eq;
      numberp;

      count
    ]
