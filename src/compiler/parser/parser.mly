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
| name = ID LPAREN expList = exp_list RPAREN                           {CallExp { func = symbol name; args = expList } }  
(*Let exp*)
| LET dl=decl_list body = exp                                          {LetExp { decls = dl; body = body } }
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
(*Assign exp*)
| variable = var ASSIGN e1 = exp                                              { AssignExp { var = variable; exp = e1 } }
(*Record exp*)
| id = ID LBRACE RBRACE                                                                      { RecordExp { fields = [] ; typ = symbol id } }
| id = ID LBRACE body = rec_base RBRACE                                                      { RecordExp { fields = body ; typ = symbol id } }
(*Sequence exp*)
| LPAREN seqList = seq_base RPAREN                                     { SeqExp seqList }
| IN seqList = seq_base END                                            { SeqExp seqList }
| LPAREN RPAREN                                                        { SeqExp [] }

exp_list:
| e1 = exp {[e1]}
| e1 = exp COMMA expList = exp_list                                { [e1] @ expList  } /* TODO: add empty list return */

seq_base:
| e1 = exp                                                             { [e1] }
| e1 = exp SEMICOLON seqList = seq_base                                { [e1] @ seqList  } /* TODO: add empty list return */

rec_base:
| id = ID EQ idValue = exp                                             { [(symbol id, idValue)] }
| id = ID EQ idValue = exp SEMICOLON recList = rec_base                { [(symbol id, idValue)] @ recList  } /* TODO: add empty list return */

decl_list:
| d=decl { [d] }
| dl1 = decl_list dl2 = decl_list { dl1 @ dl2 }

decl:
| tyList = tydecldata            { TypeDec tyList }
| funcList = fundecldata         { FunctionDec [funcList] }
/* (*Normal VarDec*)
| VAR declName = ID ASSIGN valueExp = exp                           { VarDec { name = symbol declName; escape = ref true; typ = None ; init = valueExp ; pos = $startpos } }
(*VarDec with type*)
| VAR declName = ID COLON declType = ID ASSIGN valueExp = exp_base  { VarDec { name = symbol declName; escape = ref true ; typ = Some(symbol declType, $startpos) ; init = valueExp ; pos = $startpos } } */

fundecldata:
| FUNCTION name=ID LPAREN fieldData = fielddata RPAREN COLON returnType=ID EQ body=exp { Fdecl { name = symbol name ; params= fieldData; result= Some(((symbol returnType), $startpos)) ; body= body ; pos= $startpos } }

fielddata:
| name=ID COLON typeVal=ID { [Field { name = symbol name; escape= ref true; typ = ((symbol typeVal), $startpos); pos = $startpos }] }
| fd = fielddata COMMA fieldList = fielddata { fd @ fieldList }

tydecldata:
| TYPE id=ID EQ value=ty { [ Tdecl { name = symbol id; ty = value; pos = $startpos} ]  }
| t1 = tydecldata t2 = tydecldata { t1 @ t2 }

ty:
| id=ID { NameTy ((symbol id), $startpos)  }




(* Top-level *)
program: e = exp EOF { e }

var:
| v=var_base { v ^@ $startpos }

exp:
| e=exp_base  { e ^! $startpos }
