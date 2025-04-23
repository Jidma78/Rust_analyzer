%{
  open Ast

%}

%token <string> IDENT  LABEL TYPE
%token <int> INT
%token <float> FLOAT

%token PLUS MINUS TIMES DIV EQ NEQ LT LE GT GE AND OR XOR SHL SHR ANDAND OROR MOD ASSIGN 

%token  EOF 
%token REF MUTREF NEG NOT DEREF ARROW
%token LPAREN RPAREN ARROW LBRACE RBRACE SEMICOLON COMMA COLON
%token AS BREAK CONTINUE ELSE FALSE FN IF LET LOOP MUT RETURN TRUE WHILE 
%token ABSTRACT ASYNC AWAIT BECOME BOX CONST CRATE DO DYN ENUM EXTERN FINAL FOR IMPL IN MACRO MATCH MOD MOVE OVERRIDE PRIV PUB SELF SELFCAP STATIC STRUCT SUPER TRAIT TYPE TYPEOF UNSAFE UNSIZED USE VIRTUAL WHERE YIELD

%nonassoc ELSE FN LOOP  WHILE IF LOOP LPAREN RPAREN ARROW LBRACE RBRACE SEMICOLON COMMA FALSE TRUE FOR 
%nonassoc NEG NOT REF MUTREF DEREF MUT 
%left AS  
%left TIMES DIV MOD        
%left PLUS MINUS             
%left SHL SHR
%left AND OR
%nonassoc LT LE  GE EQ NEQ                            
%left ANDAND  OROR                

%left XOR                                 
%right  LET COLON
%nonassoc  RETURN BREAK CONTINUE ASSIGN GT 


%start programme 
%type <Ast.programme> programme

%%



programme:
   EOF { [] }
  | declaration programme { $1 :: $2 }

declaration:
| FN IDENT LPAREN parametres RPAREN type_retour LBRACE bloc_instructions RBRACE {
      FuncDecl($2, $4, $6, $8)
    }

  | LET IDENT type_option affectation_option SEMICOLON { (VarDecl($2, $3, $4)) }
  | LET MUT IDENT type_option affectation_option SEMICOLON { (VarDecl($3, $4, $5)) }



bloc:
  | LBRACE bloc_instructions RBRACE { $2 } 

  
bloc_instructions:
  | { [] : block_item list }  
  | instruction bloc_instructions { Expr($1) :: $2 }  
  | declaration bloc_instructions { Decl($1) :: $2 }  


block_item:
  | declaration { Decl($1) }
  | instruction { Expr($1) }

instruction:
   | expression SEMICOLON { ($1) }
   | expression { $1 }

reference_expr:
    | AND TYPE {UnaryOp(Ref , Var $2)}          /*Les reduce/reduce conflict viennent de là également*/
    | MUTREF expression { UnaryOp(MutRef, $2) }  /*les 45 reduce/reduce conflict viennent de là mais je ne sais pas comment régler le problème*/


expression:
  | INT { Const(ConstInt $1) }
  | FLOAT { Const(ConstFloat $1) }
  | TRUE {Const(ConstBool true )}
  | FALSE {Const(ConstBool false)}
  | IDENT { Var $1 }
  | TYPE {Var $1}
  | bloc { Block($1) }
 
  | expression PLUS expression { BinaryOp($1, Add, $3) }
  | expression MINUS expression { BinaryOp($1, Sub, $3) }
  | expression TIMES expression { BinaryOp($1, Mul, $3) }
  | expression DIV expression { BinaryOp($1, Div, $3) }
  | expression EQ expression { BinaryOp($1, Eq, $3) }
  | expression MOD expression {BinaryOp($1, Mod, $3)}
  | expression NEQ expression {BinaryOp($1, Neq, $3)}
  | expression LT expression {BinaryOp($1, Lt, $3)}
  | expression LE expression {BinaryOp($1, Le, $3)}
  | expression GT expression {BinaryOp($1, Gt, $3)}
  | expression GE expression {BinaryOp($1, Ge, $3)}
  | expression AND expression {BinaryOp($1, And, $3)}
  | expression OR expression {BinaryOp($1, Or, $3)}
  | expression XOR expression {BinaryOp($1, Xor, $3)}
  | expression OROR expression {BinaryOp($1, OrOr, $3)}
  | expression ANDAND expression {BinaryOp($1, AndAnd, $3)}
  | expression SHL expression {BinaryOp($1, Shl, $3)}
  | expression SHR expression {BinaryOp($1, Shr, $3)}



  | MINUS expression  { UnaryOp(Neg, $2) }  
  | TIMES expression  { UnaryOp(Deref, $2) } 
  | NOT expression { UnaryOp(Not, $2) }
  | AND MUT TYPE {UnaryOp(MutRef, Var $3)}
  | AND expression {UnaryOp(Ref, $2)}
  | MUTREF expression { UnaryOp(MutRef, $2) }  /*les 45 reduce/reduce conflict viennent de là mais je ne sais pas comment régler le problème*/
  | AND TYPE {UnaryOp(Ref , Var $2)}          /*Les reduce/reduce conflict viennent de là également*/


  | LPAREN expression RPAREN { $2 }
  | expression LPAREN liste_expressions RPAREN {FuncCall($1, $3)}


  | IF expression bloc { If($2, $3, None) }
  | IF expression bloc ELSE bloc { If($2, $3, Some($5)) }
  | RETURN expression SEMICOLON { Return(Some $2) }
  | RETURN { Return(None) }

  | LABEL COLON WHILE expression bloc { While(Some $1, $4, $5) }


  | IDENT ASSIGN expression { Assign($1, $3) }
  | MUT IDENT {UnaryOp(Mut, Var $2) }   
  | expression AS type_base {Cast($1,$3)}


  | WHILE expression bloc { While(None, $2, $3) }

  | CONTINUE { Continue(None) }
  | CONTINUE LABEL { Continue(Some($2)) } 
  | BREAK { Break(None) }
  | BREAK LABEL { Break(Some($2)) } 
  | LOOP bloc { Loop(None, $2) }
  | LABEL LOOP bloc { Loop(Some $1, $3) }  
  | expression COLON type_base {Cast( $1, $3)}
  | SEMICOLON expression {$2}





affectation_option:
  | { None }
  | ASSIGN expression { Some $2 }


parametres:
  | { [] }
  | parametre { [$1] }
  | parametre COMMA parametres { $1 :: $3 }

parametre:
  | MUT IDENT COLON type_base { ("mut " ^ $2, $4) } 
  | IDENT COLON type_base { ($1, $3) }


type_option:
  | { None }
  |  COLON type_base {  Some $2 }
  


type_retour:
  | { None }
  | ARROW type_base { Some $2 }


type_base: 
  | LPAREN RPAREN { Unit}

  
  | TYPE { 
      match ident_to_basic_type $1 with
      | Some t -> BasicType t
      | None -> UnknownType 
    }
  | AND type_base { 
      Reference($2) 
    }
  | MUTREF type_base { 
      MutableReference($2) 
    }
  | LPAREN type_base RPAREN { 
      ParenType($2) 
    }
  ;


liste_expressions:
  | { [] }
  | expression { [$1] }
  
  | expression COMMA liste_expressions { $1 :: $3 }


%%
