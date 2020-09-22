(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(* Do not distribute                                                      *)
(**************************************************************************)

%{
  open Tigercommon.Absyn   
  open ParserAux 
  open Tigercommon.Symbol
%}

%token EOF
%token <string> ID
%token <int> INT 
%token <string> STRING 
%token COMMA COLON SEMICOLON 
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE 
%token DOT PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE 
%token AND OR ASSIGN ARRAY IF THEN ELSE WHILE FOR TO DO
%token LET IN END OF BREAK NIL FUNCTION VAR TYPE CARET 

%left PLUS
%left MINUS
%left TIMES
%left DIVIDE
%left CARET
%left GT
%left LT
%left LE
%left GE
%left EQ
%left NEQ


%start <Tigercommon.Absyn.exp> program  
(* Observe that we need to use fully qualified types for the start symbol *)

%%

(* Variables *)
var_base:
| id=ID { SimpleVar (symbol id) }


(* Expressions *)
exp_base:
| NIL                                                                  { NilExp }
| i=INT                                                                { IntExp i }
| s=STRING                                                             { StringExp s }
| v=var                                                                { VarExp v }

(*Arithmetic*)
| e1 = exp PLUS e2 = exp                                               { OpExp { left = e1; oper = PlusOp; right = e2 } }
| e1 = exp MINUS e2 = exp                                              { OpExp { left = e1; oper = MinusOp; right = e2 } }
| e1 = exp TIMES e2 = exp                                              { OpExp { left = e1; oper = TimesOp; right = e2 } }
| e1 = exp DIVIDE e2 = exp                                             { OpExp { left = e1; oper = DivideOp; right = e2 } }
| e1 = exp CARET e2 = exp                                              { OpExp { left = e1; oper = ExponentOp; right = e2 } }

(* Binary *)
| e1 = exp GT e2 = exp                                                 { OpExp { left = e1; oper = GtOp; right = e2} }
| e1 = exp LT e2 = exp                                                 { OpExp { left = e1; oper = LtOp; right = e2} }
| e1 = exp LE e2 = exp                                                 { OpExp { left = e1; oper = LeOp; right = e2} }
| e1 = exp GE e2 = exp                                                 { OpExp { left = e1; oper = GeOp; right = e2} }
| e1 = exp EQ e2 = exp                                                 { OpExp { left = e1; oper = EqOp; right = e2} }
| e1 = exp NEQ e2 = exp                                                { OpExp { left = e1; oper = NeqOp; right = e2} }

(*If exp*)
| IF testExp = exp THEN thenExp = exp                                  { IfExp { test = testExp; thn = thenExp; els = None } }
| IF testExp = exp THEN thenExp = exp ELSE elseExp = exp               { IfExp { test = testExp; thn = thenExp; els = Some(elseExp) } }
(*Loops*)
| WHILE e1 = exp DO e2 = exp                                           { WhileExp { test = e1; body = e2 } }
| FOR variable = ID ASSIGN startValue = exp TO limitValue = exp DO bodyExp = exp { ForExp { var = symbol variable; escape = ref true; lo = startValue; hi = limitValue; body = bodyExp } }

(*Sequence exp*)
| LPAREN seqList = seq_base RPAREN                                     { SeqExp seqList } /*TODO: make a list*/
| LPAREN RPAREN                                                        { SeqExp [] }

seq_base:
| e1 = exp                                                             { [e1] }
| e1 = exp SEMICOLON seqList = seq_base                                { [e1] @ seqList  }

/* decl:
(*Normal VarDec*)
| VAR declName = ID ASSIGN valueExp = exp                           { VarDec { name = symbol declName; escape = ref true; typ = None ; init = valueExp ; pos = $startpos } }
(*VarDec with type*)
| VAR declName = ID COLON declType = ID ASSIGN valueExp = exp_base  { VarDec { name = symbol declName; escape = ref true ; typ = Some(symbol declType, $startpos) ; init = valueExp ; pos = $startpos } }
 */

(* Top-level *)
program: e = exp EOF { e }

var:
| v=var_base { v ^@ $startpos }

exp:
| e=exp_base  { e ^! $startpos }
