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
open LogoEnv
open LogoEval

(** 2.1 Constructors *)

let word word1 word2 words =
  String.concat "" (word1 :: word2 :: words)
    
let list things =
  things

let sentence thing1 thing2 things =
  List.concat (List.map (function List l -> l | _ as a -> [a]) (thing1 :: thing2 :: things))

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

let array size origin =
  let origin = match origin with
      None -> 1
    | Some origin -> origin
  in
  if size < 0 then raise (Error "array: SIZE must be a positive integer");
  Array (Array.create size (List []), origin)

let array_doc =
  "array SIZE
(array SIZE ORIGIN)

Outputs an array of SIZE members (must be a positive integer), each of which
initially is an empty list.  Array members can be selected with 'item' and
changed with 'setitem'.  The first member of the array is member number 1 unless
an ORIGIN input (must be an integer) is given, in which case the first member of
the array has ORIGIN as its index.  (Typically 0 is used as ORIGIN if anything.)
Arrays are printed by 'print' and friends, and can be typed in, inside curly
braces; indicate an origin with {a b c}@0."

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

let listtoarray list origin =
  let origin = match origin with
      None -> 1
    | Some origin -> origin
  in
  (Array.of_list list, origin)
    
let listtoarray_doc =
  "listtoarray LIST
(listtoarray LIST ORIGIN)

Outputs an array of the same size as the LIST, whose members are the members of
LIST.  The first member of the array is member number 1 unless an ORIGIN input
(must be an integer) is given, in which case the first member of the array has
ORIGIN as its index."

let arraytolist (a, _) =
  Array.to_list a
  
let arraytolist_doc =
  "arraytolist ARRAY

Outputs a list whose members are the members of ARRAY.  The first member of the
output is the first member of the array, regardless of the array's origin."

let reverse =
  List.rev

let reverse_doc =
  "reverse LIST

Outputs a list whose members are the members of LIST, in reverse order."

let gensym =
  let count = ref 0 in
  fun () ->
    incr count;
    "G" ^ string_of_int !count

let gensym_doc =
  "gensym

Outputs a unique word each time it's invoked.  The words are of the form G1, G2,
etc."

(** 2.2 Data Selectors *)

let first = function
    Num n ->
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

let firsts l =
  List.map first l

let last = function
    Num n ->
    let s = string_of_float n in
    let l = String.length s in
    Word (String.make 1 (s.[l-1]))
  | Word w ->
    let l = String.length w in
    Word (String.make 1 (w.[l-1]))
  | List [] ->
    raise (Error "last: empty list")
  | List lst ->
    let l = List.length lst in
    List.nth lst (l-1)
  | _ ->
    raise (Error "last: LIST or WORD expected")

let butfirst = function
    Num n ->
    let s = string_of_float n in
    let l = String.length s in
    Word (String.sub s 1 (l-1))
  | Word w ->
    let l = String.length w in
    Word (String.sub w 1 (l-1))
  | List [] ->
    raise (Error "butfirst: empty list")
  | List (_ :: rest) ->
    List rest
  | _ ->
    raise (Error "butfirst: expected WORD or LIST")

let item index = function
    Num n ->
    let s = string_of_float n in
    Word (String.make 1 s.[index-1])
  | Word w ->
    Word (String.make 1 w.[index-1])
  | List l ->
    List.nth l (index-1)
  | Array (a, orig) ->
    a.(index-orig)

let pick = function
    [] ->
    raise (Error "pick: empty list")
  | (_ :: _) as l ->
    List.nth l (Random.int (List.length l))

let quoted = function
    List _ as a -> a
  | Num n ->
    let s = string_of_float n in
    Word ("\"" ^ s)
  | Word w ->
    Word ("\"" ^ w)
  | _ ->
    raise (Error "quoted: LIST or WORD expected")

(** 2.3 Data Mutators *)

(** 2.4 Predicates *)

(** 2.5 Queries *)

let init env =
  set_pf env "word" Lga.(word @-> word @-> rest word (value word)) word;
  set_pf env "list" Lga.(rest any (value (list any))) list;
  set_pf env "sentence" Lga.(any @-> any @-> rest any (value (list any))) sentence;
  set_pf env "se" Lga.(any @-> any @-> rest any (value (list any))) sentence;
  (* set_pf2 env "fput" fput; *)
  (* set_pf2 env "lput" lput; *)
  set_pf env "array" Lga.(int @-> opt int (value any)) array;
  (* set_pf2 env "combine" combine; *)
  set_pf env "listtoarray" Lga.(list any @-> opt int (value (array any))) listtoarray;
  set_pf env "arraytolist" Lga.(array any @-> ret (value (list any))) arraytolist;
  set_pf env "reverse" Lga.(list any @-> ret (value (list any))) reverse;
  set_pf env "gensym" Lga.(void @@ ret (value word)) gensym;

  set_pf env "first" Lga.(any @-> ret (value any)) first;
  set_pf env "firsts" Lga.(list any @-> ret (value (list any))) firsts;
  (* set_pf1 env "last" last; *)
  (* set_pf1 env "butfirst" butfirst; *)
  set_pf env "item" Lga.(int @-> any @-> ret (value any)) item;
  set_pf env "pick" Lga.(list any @-> ret (value any)) pick
  (* set_pf1 env "quoted" quoted *)
