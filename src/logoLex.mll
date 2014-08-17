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

let is_infix = function
  | "<=" | ">=" | "<>" | "+" | "-" | "*"
  | "/" | "%" | "^" | "=" | "<" | ">" -> true
  | _ -> false
  
let is_minus acc leading_space trailing_space =
  match acc with
  | [] -> true
  | Word "(" :: _ -> true
  | Word w :: _ -> is_infix w
  | _ -> leading_space && not trailing_space

let rewind lexbuf n =
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - n

let minus_word = Word "minus"
}

let space = [' ' '\t']
let nonspace = [^ ' ' '\t']
let identifier = '`' | '.'? ['a'-'z' 'A'-'Z' ','] ['a'-'z' 'A'-'Z' '0'-'9' '_' '.' '?' ',' ':' '\"']*
let string_literal = '\"' [^ ' ' '[' ']' '{' '}' '(' ')']*
let variable = ':' ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let number_literal = ['0'-'9']* '.'? ['0'-'9']+ (['e' 'E'] ['-' '+']? ['0'-'9']+)?
let signed_literal = '-'? ['0'-'9']+
let operator = "<=" | ">=" | "<>" | ['+' '-' '*' '/' '%' '^' '=' '<' '>' '[' ']' '{' '}' '(' ')']

rule parse_atoms acc leading_space = parse
  | space+
    { parse_atoms acc true lexbuf }
  | ';' [^ '~' '\n']*
    { parse_atoms acc leading_space lexbuf }
  | identifier
  | string_literal
  | variable
  | number_literal
    { parse_atoms (Word (Lexing.lexeme lexbuf) :: acc) false lexbuf }
  | '['
    { parse_atoms (parse_list [] lexbuf :: acc) false lexbuf }
  | '{'
    { parse_atoms (parse_array [] lexbuf :: acc) false lexbuf }
  | '-' nonspace
    { rewind lexbuf 1;
      let atom = if is_minus acc leading_space false then minus_word else Word "-" in
      parse_atoms (atom :: acc) false lexbuf }
  | '-' space
    { let atom = if is_minus acc leading_space true then minus_word else Word "-" in
      parse_atoms (atom :: acc) true lexbuf }
  | operator
    { parse_atoms (Word (Lexing.lexeme lexbuf) :: acc) false lexbuf }
  | eof
    { List.rev acc }
  | _ as c
    { unexpected c }

and parse_list acc = parse
  | space+
    { parse_list acc lexbuf }
  | [^ ' ' '\t' '{' '}' '[' ']']+
    { parse_list (Word (Lexing.lexeme lexbuf) :: acc) lexbuf }
  | ']'
    { List (List.rev acc) }
  | '['
    { parse_list (parse_list [] lexbuf :: acc) lexbuf }
  | '{'
    { parse_list (parse_array [] lexbuf :: acc) lexbuf }
  | eof
    { expected ']' }
  | _ as c
    { unexpected c }

and parse_array acc = parse
  | space+
    { parse_array acc lexbuf }
  | [^ ' ' '\t' '[' ']' '{' '}']+
    { parse_array (Word (Lexing.lexeme lexbuf) :: acc) lexbuf }
  | '}' space* '@' space* (signed_literal as origin)
    { Array (Array.of_list (List.rev acc), int_of_string origin) }
  | '}'
    { Array (Array.of_list (List.rev acc), 1) }
  | '['
    { parse_array (parse_list [] lexbuf :: acc) lexbuf }
  | '{'
    { parse_array (parse_array [] lexbuf :: acc) lexbuf }
  | eof
    { expected '}' }
  | _ as c
    { unexpected c }

and line_or_to = parse
  | space+
    { line_or_to lexbuf }
  | ['t' 'T'] ['o' 'O'] space+ (identifier as name)
    { to_arg name [] lexbuf }
  | eof
    { `GotEMPTY }
  | _ as c
    { line_or_cont (String.make 1 c) lexbuf }

and line_or_end = parse
  | space+
    { line_or_end lexbuf }
  | ['e''E']['n''N']['d''D'] space* eof
    { `GotEND }
  | eof
    { `GotEMPTY }
  | _ as c
    { line_or_cont (String.make 1 c) lexbuf }

and line_or_cont buf = parse
  | '~' space* eof
    { `GotCONT buf }
  | '~'
    { line_or_cont (buf ^ "~") lexbuf }
  | eof
    { `GotLINE }
  | [^ '~']+
    { line_or_cont (buf ^ Lexing.lexeme lexbuf) lexbuf }

and to_arg name acc = parse
  | space+
    { to_arg name acc lexbuf }
  | variable as input
    { to_arg name ((Re_str.string_after input 1) :: acc) lexbuf }
  | eof
    { `GotTO (name, List.rev acc) }
  | _ as c
    { unexpected c }
