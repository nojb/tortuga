{
open Logo
open Lexing

type error =
  | Unexpected_character of char
  | Expected_character of char

exception Error of error

let unexpected c =
  raise (Error (Unexpected_character c))

let expected c =
  raise (Error (Expected_character c))

open Format

let report_error ppf = function
  | Unexpected_character c ->
    fprintf ppf "Unexpected character (%s)" (Char.escaped c)
  | Expected_character c ->
    fprintf ppf "Expected character (%s)" (Char.escaped c)

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

let space = [' ' '\010'-'\014']
let nonspace = [^ ' ' '\010'-'\014']
let identifier = '.'? ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '.' '?']*
let string_literal = '\"' [^ ' ' '[' ']' '{' '}' '(' ')']*
let variable = ':' ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let number_literal = ['0'-'9']+
let signed_literal = '-'? number_literal
let operator = "<=" | ">=" | "<>" | ['+' '-' '*' '/' '%' '^' '=' '<' '>' '[' ']' '{' '}' '(' ')']

rule parse_atoms acc leading_space = parse
  | '.'
    { List.rev acc }
  | space+
    { parse_atoms acc true lexbuf }
  | ';' [^ '\n']*
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
    { raise Exit } (* List.rev acc *)
  | _ as c
    { unexpected c }

and parse_list acc = parse
  | space+
    { parse_list acc lexbuf }
  | [^ ' ' '\010'-'\014' '{' '}' '[' ']']+
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
  | [^ ' ' '\010'-'\014' '[' ']' '{' '}']+
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
