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

let word word1 word2 words =
  String.concat "" (word1 :: word2 :: words)
    
let list thing1 thing2 things =
  thing1 :: thing2 :: things

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
    `L w ->
    Word (String.make 1 w.[String.length w - 1])
  | `R lst ->
    let l = List.length lst in
    List.nth lst (l-1)

let butfirst = function
    `L w ->
    let l = String.length w in
    Word (String.sub w 1 (l-1))
  | `R l ->
    List (List.tl l)

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

let pick l =
  List.nth l (Random.int (List.length l))

let quoted = function
    `L s -> Word ("\"" ^ s)
  | `R l -> List l

(** 2.3 Data Mutators *)

(** 2.4 Predicates *)

(* TODO fix conversion between numbers and words *)
let rec equalaux a b =
  match a, b with
  | Num n, Num m -> n = m
  | Word w1, Word w2 -> w1 == w2
  | List l1, List l2 -> List.length l1 = List.length l2 && List.for_all2 equalaux l1 l2
  | Array (a1, _), Array (a2, _) -> a1 == a2
  | _ -> false

let equalp a b =
  if equalaux a b then true_word else false_word

let equalp_infix env a b k =
  wrap env "=" Lga.(any @-> any @-> ret (value any)) equalp [a; b]
    (function
      | Some a -> k a
      | None -> assert false)

let notequalp a b =
  if equalaux a b then false_word else true_word

let notequalp_infix env a b k =
  wrap env "<>" Lga.(any @-> any @-> ret (value any)) notequalp [a; b]
    (function
      | Some a -> k a
      | None -> assert false)

let beforep a b =
  if a < b then true_word else false_word

let _eq a b =
  if a == b then true_word else false_word

let numberp = function
  | Num _ ->
    true_word
  | Word n ->
    if isnumber n then true_word else false_word
  | _ ->
    false_word

(** 2.5 Queries *)

let count = function
  | Num n -> String.length (Printf.sprintf "%g" n)
  | Word w -> String.length w
  | List l -> List.length l
  | Array (a, _) -> Array.length a

let () =
  set_pf "word" Lga.(word @-> word @-> rest word (value word)) word;
  set_pf "list" Lga.(any @-> any @-> rest any (value (list any))) list;
  set_pf "sentence" Lga.(any @-> any @-> rest any (value (list any))) sentence;
  set_pf "se" Lga.(any @-> any @-> rest any (value (list any))) sentence;
  (* set_pf2 env "fput" fput; *)
  (* set_pf2 env "lput" lput; *)
  set_pf "array" Lga.(int @-> opt int (value any)) array;
  (* set_pf2 env "combine" combine; *)
  set_pf "listtoarray" Lga.(list any @-> opt int (value (array any))) listtoarray;
  set_pf "arraytolist" Lga.(array any @-> ret (value (list any))) arraytolist;
  set_pf "reverse" Lga.(list any @-> ret (value (list any))) reverse;
  set_pf "gensym" Lga.(void @@ ret (value word)) gensym;

  set_pf "first" Lga.(any @-> ret (value any)) first;
  set_pf "firsts" Lga.(list any @-> ret (value (list any))) firsts;
  set_pf "last" Lga.(alt word (ne_list any) @-> ret (value any)) last;
  set_pf "butfirst" Lga.(alt word (ne_list any) @-> ret (value any)) butfirst;
  set_pf "item" Lga.(int @-> any @-> ret (value any)) item;
  set_pf "pick" Lga.(ne_list any @-> ret (value any)) pick;
  set_pf "quoted" Lga.(alt word (list any) @-> ret (value any)) quoted;

  set_pf "equalp" Lga.(any @-> any @-> ret (value any)) equalp;
  set_pf "equal?" Lga.(any @-> any @-> ret (value any)) equalp;
  set_pf "notequalp" Lga.(any @-> any @-> ret (value any)) notequalp;
  set_pf "notequal?" Lga.(any @-> any @-> ret (value any)) notequalp;
  set_pf "beforep" Lga.(word @-> word @-> ret (value any)) beforep;
  set_pf ".eq" Lga.(any @-> any @-> ret (value any)) _eq;
  (* missing : "memberp" *)
  (* set_pf env "substringp" Lga.(any @-> any @-> ret (value any)) substringp *)
  set_pf "numberp" Lga.(any @-> ret (value any)) numberp;

  set_pf "count" Lga.(any @-> ret (value int)) count
