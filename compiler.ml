(* compiler.ml *)
(* A compiler for simple arithmetic expressions. *)

(******************************************************************************)

open Printf
open Ast
open Rux86

(* Note that Ast has similarly named constructors that must be
              disambiguated.  For example: Ast.Shl vs. Rux86.Shl *)

(* Parse an AST from a preexisting lexbuf. 
 * The filename is used to generate error messages.
*)
let parse (filename : string) (buf : Lexing.lexbuf) : exp =
  try
    Lexer.reset_lexbuf filename buf;
    Parser.toplevel Lexer.token buf
  with Parsing.Parse_error ->
    failwith (sprintf "Parse error at %s."
        (Range.string_of_range (Lexer.lex_range buf)))



(* Builds a globally-visible X86 instruction block that acts like the C fuction:

   int program(int X) { return <expression>; }

   Follows cdecl calling conventions and platform-specific name mangling policy. *)

let rec emit_exp (e:exp) (stream : insn list) : insn list = 
    begin
        match e with 
        | Cint i -> Mov (Imm i, eax) :: stream 
        | Arg -> Mov (ecx, eax) :: stream
        | Binop (o,l,r) -> (
            match o with
	        | Plus -> 
                    let left_insn = emit_exp l stream in
                    let push_insn = Push (eax) :: left_insn in
                    let right_insn = emit_exp r push_insn in
                    let pop_insn = Pop (ebx)::right_insn in
                    Add (ebx, eax) :: pop_insn
            (*| Times -> "Times" 
            | Minus -> "Minus"*)
            )
	       (* | Eq -> "Eq" 
            | Neq -> "Neq" 
            | Lt -> "Lt" 
            | Lte -> "Lte"
	        | Gt -> "Gt" 
            | Gte -> "Gte" 
            | And -> "And" 
            | Or -> "Or"
	        | Shr -> "Shr" 
            | Sar -> "Sar" 
            | Shl -> "Shl" in
	            Printf.sprintf "(Binop (%s,%s,%s))" binop_str
	            (ml_string_of_exp l) (ml_string_of_exp r)
            )
        | Unop (o,l) -> (
	        match o with
	        | Neg -> "Neg" 
            | Lognot -> "Lognot" 
            | Not -> "Not" in
	            Printf.sprintf "(Unop (%s,%s))" unop_str (ml_string_of_exp l)
            )*)
end

let compile_exp (ast:exp) : Cunit.cunit =
    let block_name = (Platform.decorate_cdecl "program") in
    let rev_insns = emit_exp ast [] in
    let insns = List.rev rev_insns in
    let insn_list = [Push(ebp);Mov(esp, ebp);Mov(stack_offset 8l, ecx)] @ insns @ [Pop(ebp); Ret] in 
    let blk = {
        label= (mk_lbl_named block_name); 
        insns = insn_list; 
        global = true
        } in
    let code = Cunit.Code(blk) in
    [code]
