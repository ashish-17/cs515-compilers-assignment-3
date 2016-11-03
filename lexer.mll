{
  open Lexing
  open Parser
  open Range
  
  exception Lexer_error of Range.t * string

  let pos_of_lexpos (p:Lexing.position) : pos =
    mk_pos (p.pos_lnum) (p.pos_cnum - p.pos_bol)
    
  let mk_lex_range (p1:Lexing.position) (p2:Lexing.position) : Range.t =
    mk_range p1.pos_fname (pos_of_lexpos p1) (pos_of_lexpos p2)

  let lex_range lexbuf : Range.t = mk_lex_range (lexeme_start_p lexbuf)
      (lexeme_end_p lexbuf)

  let reset_lexbuf (filename:string) lexbuf : unit =
    lexbuf.lex_curr_p <- {
      pos_fname = filename;
      pos_cnum = 0;
      pos_bol = 0;
      pos_lnum = 1;
    }
    
  (* Boilerplate to define exceptional cases in the lexer. *)
  let unexpected_char lexbuf (c:char) : 'a =
    raise (Lexer_error (lex_range lexbuf,
        Printf.sprintf "Unexpected character: '%c'" c))

  let invalid_int lexbuf (value:string) : 'a =
    raise (Lexer_error (lex_range lexbuf,
        Printf.sprintf "Invalid integer: '%s'" value))
}

let digit = ['0'-'9']
let space = [' ' '\t']
let nl = ('\r'|'\n'|"\r\n")

rule token = parse
  | eof             { EOF }
  | space+          { token lexbuf }
  | nl+             { token lexbuf }

  | '('             { LBRACE (lex_range lexbuf)}
  | ')'             { RBRACE (lex_range lexbuf)}

  | '!'             { LOGNOT (lex_range lexbuf) }
  | '~'             { BITNOT (lex_range lexbuf) }

  | '&'             { BITAND (lex_range lexbuf) }
  | '|'             { BITOR (lex_range lexbuf) }
  | '+'             { PLUS (lex_range lexbuf) }
  | '-'             { MINUS (lex_range lexbuf) }
  | '*'             { MULT (lex_range lexbuf) }
  | '<'             { LT (lex_range lexbuf) }
  | "<="            { LTE (lex_range lexbuf) }
  | '>'             { GT (lex_range lexbuf) }
  | ">="            { GTE (lex_range lexbuf) }
  | "=="            { EQUAL (lex_range lexbuf) }
  | "!="            { NOTEQUAL (lex_range lexbuf) }
  | "<<"            { SHL (lex_range lexbuf) }
  | ">>"            { SAR (lex_range lexbuf) }
  | ">>>"           { SHR (lex_range lexbuf) }
    
  | digit+ as value { try 
                        let int_val = Int32.of_string value in 
                        INT (lex_range lexbuf, int_val) 
                      with 
                      | _ -> invalid_int lexbuf value
                    }

  | 'X'             { X (lex_range lexbuf) }
  | _ as c          { unexpected_char lexbuf c }
