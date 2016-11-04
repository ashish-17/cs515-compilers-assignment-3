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
        | Arg -> Mov (edx, eax) :: stream
        | Binop (o,l,r) -> (binop_exp o l r stream)
        | Unop (o,l) -> (unop_exp o l stream)
end
    and binop_exp (o:Ast.binop) (l: exp) (r: exp) (stream: insn list) : insn list = 
    begin
        let left_insn = emit_exp l stream in
        let push_insn = Push (eax) :: left_insn in
        let right_insn = emit_exp r push_insn in
        let mov_insn = Mov (eax, ecx) :: right_insn in
        let pop_insn = Pop(eax)::mov_insn in
        match o with
        | Plus -> Add(ecx, eax) :: pop_insn
        | Minus -> Sub(ecx, eax) :: pop_insn
        | Times -> Imul(ecx, Eax) :: pop_insn
        | And -> Rux86.And(ecx, eax) :: pop_insn
        | Or -> Rux86.Or(ecx, eax) :: pop_insn
        | Shr -> Rux86.Shr(ecx, eax) :: pop_insn
        | Sar -> Rux86.Sar(ecx, eax) :: pop_insn
        | Shl -> Rux86.Shl(ecx, eax) :: pop_insn
        | Eq -> [Setb(Rux86.Eq, eax);Mov(Imm 0l, eax);Cmp(ecx, eax)] @ pop_insn
        | Neq -> [Setb(Rux86.NotEq, eax);Mov(Imm 0l, eax);Cmp(ecx, eax)] @ pop_insn
        | Lt -> [Setb(Rux86.Slt, eax);Mov(Imm 0l, eax);Cmp(ecx, eax)] @ pop_insn
        | Lte -> [Setb(Rux86.Sle, eax);Mov(Imm 0l, eax);Cmp(ecx, eax)] @ pop_insn
        | Gt -> [Setb(Rux86.Sgt, eax);Mov(Imm 0l, eax);Cmp(ecx, eax)] @ pop_insn
        | Gte -> [Setb(Rux86.Sge, eax);Mov(Imm 0l, eax);Cmp(ecx, eax)] @ pop_insn
    end
    and unop_exp (o:Ast.unop) (l: exp) (stream: insn list) : insn list = 
    begin
        let left_insn = emit_exp l stream in
        match o with
        | Neg -> Neg(eax) :: left_insn
        | Lognot -> [Setb(Rux86.Eq, eax);Mov(Imm 0l, eax);Cmp(eax, ecx);Mov(Imm 0l, ecx)] @ left_insn
        | Not -> Rux86.Not(eax) :: left_insn
    end

let compile_exp (ast:exp) : Cunit.cunit =
    let block_name = (Platform.decorate_cdecl "program") in
    let rev_insns = emit_exp ast [] in
    let insns = List.rev rev_insns in
    let insn_list = [Push(ebp);Mov(esp, ebp);Mov(stack_offset 8l, edx)] @ insns @ [Pop(ebp); Ret] in 
    let blk = {
        label= (mk_lbl_named block_name); 
        insns = insn_list; 
        global = true
        } in
    let code = Cunit.Code(blk) in
    [code]
