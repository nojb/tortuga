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

{
open LogoTypes
open Lexing

type error =
  | Unexpected_character of char
  | Expected_character of char

exception Error of error

let unexpected c =
  raise (Error (Unexpected_character c))

let expected c =
  raise (Error (Expected_character c))

let report_error = function
  | Unexpected_character c ->
      Printf.sprintf "Unexpected character (%s)" (Char.escaped c)
  | Expected_character c ->
      Printf.sprintf "Expected character (%s)" (Char.escaped c)
}

let space = [' ' '\t']
let nonspace = [^ ' ' '\t']
let identifier = [':''"']? ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let number_literal = ['0'-'9']* '.'? ['0'-'9']+ (['e' 'E'] ['-' '+']? ['0'-'9']+)?
let operator = "<=" | ">=" | "<>" | ['+' '-' '*' '/' '%' '^' '=' '<' '>' '[' ']' '{' '}' '(' ')']

rule token = parse
| space
    { token lexbuf }
| ';'
    { comment lexbuf }
| ['-''+']?['0'-'9']+ as n
    { Int (int_of_string n) }
| identifier as v
    { intern v }
| operator
    { intern (Lexing.lexeme lexbuf) }
| eof
    { raise End_of_file }
| _ as c
    { unexpected c }

and comment lexbuf = parse
  | '\n' { token lexbuf }
  | _ { comment lexbuf }
  | eof { raise End_of_file }
