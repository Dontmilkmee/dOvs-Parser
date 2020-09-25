(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(* Do not distribute                                                      *)
(****l**********************************************************************)

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

%nonassoc THEN 
%nonassoc DO ELSE
%nonassoc array_tag
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ GT LT LE GE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc unary_minus
%right CARET
%nonassoc FUNCTION
%nonassoc TYPE



%start <Tigercommon.Absyn.exp> program  
(* Observe that we need to use fully qualified types for the start symbol *)

%%

(* Variables *)
%inline var_base:
| id=ID                                                                { SimpleVar (symbol id) }

%inline var_list: 
| DOT id=ID                                                            { FieldPart (symbol id) }
| LBRACK e=exp RBRACK                                                  { SubscriptPart e }

(* Expressions *)
exp_base:
| v=var                                                                { VarExp v  }
| NIL                                                                  { NilExp }
| i=INT                                                                { IntExp i }
| s=STRING                                                             { StringExp s }
| name = ID LPAREN expList = separated_list(COMMA, exp) RPAREN         { CallExp { func = symbol name; args = expList } }
(*Arithmetic*)
| e1 = exp PLUS e2 = exp                                               { OpExp { left = e1; oper = PlusOp; right = e2 } }
| e1 = exp MINUS e2 = exp                                              { OpExp { left = e1; oper = MinusOp; right = e2 } }
| e1 = exp TIMES e2 = exp                                              { OpExp { left = e1; oper = TimesOp; right = e2 } }
| e1 = exp DIVIDE e2 = exp                                             { OpExp { left = e1; oper = DivideOp; right = e2 } }
| e1 = exp CARET e2 = exp                                              { OpExp { left = e1; oper = ExponentOp; right = e2 } }
(* Unary minus*)
| MINUS e = exp %prec unary_minus                                      { OpExp { left = (IntExp 0 ^! $startpos); oper = MinusOp; right = e } }
(* Binary *)
| e1 = exp GT e2 = exp                                                 { OpExp { left = e1; oper = GtOp; right = e2} }
| e1 = exp LT e2 = exp                                                 { OpExp { left = e1; oper = LtOp; right = e2} }
| e1 = exp LE e2 = exp                                                 { OpExp { left = e1; oper = LeOp; right = e2} }
| e1 = exp GE e2 = exp                                                 { OpExp { left = e1; oper = GeOp; right = e2} }
| e1 = exp EQ e2 = exp                                                 { OpExp { left = e1; oper = EqOp; right = e2} }
| e1 = exp NEQ e2 = exp                                                { OpExp { left = e1; oper = NeqOp; right = e2} }
| e1 = exp AND e2 = exp                                                { IfExp { test = e1; thn = e2; els = Some(IntExp 0 ^! $startpos) } }
| e1 = exp OR e2 = exp                                                 { IfExp { test = e1; thn = IntExp 1 ^! $startpos; els = Some(e2) } }
(*Record exp*)
| id = ID LBRACE body = separated_list(COMMA, rec_base) RBRACE                                { RecordExp { fields = body ; typ = symbol id } }
(*Sequence exp*)
| LPAREN seqList = separated_list(SEMICOLON, exp) RPAREN               { SeqExp seqList }
(*Assign exp*)
| variable = var ASSIGN e1 = exp                                       { AssignExp { var = variable; exp = e1 } }
(*If exp*)
| IF testExp = exp THEN thenExp = exp                                  { IfExp { test = testExp; thn = thenExp; els = None } }
| IF testExp = exp THEN thenExp = exp ELSE elseExp = exp               { IfExp { test = testExp; thn = thenExp; els = Some(elseExp) } }
(*Loops*)
| WHILE e1 = exp DO e2 = exp                                           { WhileExp { test = e1; body = e2 } }
| FOR id = ID ASSIGN start = exp TO lim = exp DO body = exp            { ForExp { var = symbol id; escape = ref true; lo = start; hi = lim; body = body } }
| BREAK                                                                { BreakExp }
(*Let exp*)
| LET dl=list(decl) IN body = separated_list(SEMICOLON, exp) END       { LetExp { decls = dl; body = (SeqExp body ^! $startpos) } }
| name=ID LBRACK size=exp RBRACK OF initVal=exp %prec array_tag        { ArrayExp { typ = symbol name; size = size; init = initVal} }

(*Body of RecordExp*)
%inline rec_base:
| id = ID EQ idValue = exp                                             { (symbol id, idValue) }

(*Declarations*)
decl:
| funcList = nonempty_list(fundecldata)                                { FunctionDec funcList }
| tyList = nonempty_list(tydecldata)                                   { TypeDec tyList }
(*Normal VarDec*)
| VAR declName = ID ASSIGN valueExp = exp                              { VarDec { name = symbol declName; escape = ref true; typ = None ; init = valueExp ; pos = $startpos } }
(*VarDec with type*)
| VAR declName = ID COLON declType = ID ASSIGN valueExp = exp          { VarDec { name = symbol declName; escape = ref true ; typ = Some(symbol declType, $startpos) ; init = valueExp ; pos = $startpos } }
 
(*Funcion declaration data*)
%inline fundecldata:
| FUNCTION name=ID LPAREN d = separated_list(COMMA, fielddata) RPAREN COLON rt=ID EQ body=exp { Fdecl { name = symbol name ; params= d; result= Some(((symbol rt), $startpos)) ; body= body ; pos= $startpos } }
| FUNCTION name=ID LPAREN d = separated_list(COMMA, fielddata) RPAREN EQ body=exp             { Fdecl { name = symbol name ; params= d; result= None ; body= body ; pos= $startpos } }

(*Data of fields*)
%inline fielddata:
| name=ID COLON typeVal=ID                                             { Field { name = symbol name; escape= ref true; typ = ((symbol typeVal), $startpos); pos = $startpos } }

(*Type declaration data*)
%inline tydecldata:
| TYPE id=ID EQ value=ty                                               { Tdecl { name = symbol id; ty = value; pos = $startpos}  }

(*Type declarations*)
ty:
| id=ID                                                                { NameTy ((symbol id), $startpos)  }
| LBRACE fieldData=separated_list(COMMA, fielddata) RBRACE             { RecordTy fieldData }
| ARRAY OF id = ID                                                     { ArrayTy ((symbol id), $startpos) }

(* Top-level *)
program: e = exp EOF { e }

var:
| v=var_base vl=list(var_list) { makeLvaluePartSpec (v ^@ $startpos) $startpos vl }


exp:
| e=exp_base  { e ^! $startpos }
