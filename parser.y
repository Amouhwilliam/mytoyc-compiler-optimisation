%{
    #include "node.hpp"
    #include <cstdlib>
    #include "tokens.hpp"

    NBlock programBlock; /* the top level root node of our final AST */

    void yyerror(const char *s) { fprintf(stderr,"Error in line %d: %s\n", yylineno,s);exit(1); }
    
%}

/* Represents the many different ways we can access our data */
%union {
    PNODE(Node) node;
    PNODE(NBlock) block;
    PNODE(NExpression) expr;
    PNODE(NStatement) stmt;
    PNODE(NInteger) nint;
    PNODE(NIdentifier) id;
    PNODE(NVariableDeclaration) var_decl;
    PNODE(NVariableList) varlist;
    PNODE(NExpressionList) exprlist;
    PNODE(NStatementList) stmtlist;
    int value;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token TINT_T
%token TIDENTIFIER
%token<value> TINTEGER
%token TASSIGN
// TxOPy: < x=C: Comparison, x=B: Binary > operator; y=precedence
%token<value> TCOP1
%token TLPAREN TRPAREN TLBRACE TRBRACE
%token TSEMICOL TCOMMA
%token<value> TBOP2 TBOP3
%token<value> TUOP4
%token TRETURN TEXTERN
%token TIF TELSE
%token TNONE
%token<value> TTYPE /* William AMOUH modification */ 

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type<id> ident
%type<expr> numeric expr
%type<varlist> func_decl_args
%type<exprlist> call_args
%type<block> program block
%type<stmtlist> stmts
%type<stmt> stmt block_stmt simple_stmt func_decl extern_decl
%type<var_decl> var_decl_with_init var_decl
%type<stmt> if_stmt
%type<expr> comparison var_init
%type<value> type /* William AMOUH modification */ 


/* Operator precedence for mathematical operators */
%left TCOP1 // EQ NE LT LE GT GE
%left TBOP2 // PLUS MINUS
%left TBOP3 // MUL DIV
%left TUOP4 // UNARY OPERATOR OF HIGHEST PRECEDENCE

%start program

%%

program : stmts { programBlock.statements = *$1; $1->clear(); delete $1; }
        ;

stmts : stmt { $$ = new NStatementList();
               $$->push_back($1);
             }
      | stmts stmt { $$=$1;$$->push_back($2); }
      ;

stmt : block_stmt | simple_stmt TSEMICOL
     ;

block_stmt: func_decl | if_stmt
          ;
          
/* William AMOUH modification */ 
simple_stmt: var_decl_with_init { $$ = $1; }
         | extern_decl
         | expr { $$ = new NExpressionStatement($1); }
         | TRETURN expr { $$ = new NReturnStatement($2); }
         | TRETURN { $$ = new NReturnStatement(nullptr); } 
         ;

// Note: We disallow empty statement blocks here -- actually
// only, because it makes LLVM code generation simpler!
block : TLBRACE stmts TRBRACE
         { $$ = new NBlock; $$->statements = *$2; delete $2; }
      ;
/* William AMOUH modification */    
type: TTYPE; // INTEGERS and Void ALLOWED!

var_init: TASSIGN expr { $$ = $2; } | { $$ = nullptr; } ;

/* William AMOUH modification */ 
var_decl : type ident { 
if($1 == TVOID)
  {
    yyerror("can not declare a variable of type void\n");
    YYABORT;
  }
  else
  {
    $$ = new NVariableDeclaration($2);
  }
 };

var_decl_with_init: var_decl var_init
                   { $$ = $1; (*$1).assignmentExpr = $2; }
                  ;
                  
/* William AMOUH modification */
extern_decl : TEXTERN type ident TLPAREN func_decl_args TRPAREN
              { $$ = new NExternDeclaration($2, $3,*$5); delete $5;}
            ;

/* William AMOUH modification */
func_decl : type ident TLPAREN func_decl_args TRPAREN block 
            { $$ = new NFunctionDeclaration($1,$2,*$4,$6);
              delete $4;
            }
          ;

func_decl_args:  /*blank*/  { $$ = new NVariableList; }
          | var_decl { $$ = new NVariableList;
                   $$->push_back($1); }
          | func_decl_args TCOMMA var_decl
            { $$=$1;$$->push_back($3);}
          ;

if_stmt: TIF TLPAREN expr TRPAREN block TELSE block
          { $$ = new NIfStatement($3,$5,$7);
           }
        | TIF TLPAREN expr TRPAREN block
          { $$ = new NIfStatement($3,$5); }
        ;
        
expr : ident TLPAREN call_args TRPAREN
      { $$ = new NFunctionCall($1,*$3); delete $3;}
     | ident TASSIGN expr
      { $$ = new NAssignment($1,$3);}
     | ident { $$ = $1; /* needed, because expr is type expr, ident is type id */ }
     | numeric
     | expr TBOP3 expr
     { $$ = new NBinaryOperator($1, $2,$3); }
     | TBOP2 expr %prec TUOP4
     { // this might be unary plus or minus (+42 or -42)
       if(PLUS == $1){
            $$ = $2;
       } else { 
         // OK, this is not in general optimal, but to always use 3 adresses,
         // -x can be expressed as 0-x
         $$ = new NBinaryOperator(
                  new NInteger(0),
                  MINUS,
                  $2);
        }
      }
      | expr TBOP2 expr
      { $$ = new NBinaryOperator($1, $2,$3); }
     | comparison { $$ = $1; }
     | TLPAREN expr TRPAREN { $$ = $2; }
     ;

ident : TIDENTIFIER { $$ = new NIdentifier(yytext); }
      ;

numeric : TINTEGER { $$ = new NInteger($1); }
        ;

call_args : /*blank*/  { $$ = new NExpressionList; }
          | expr
           { $$ = new NExpressionList;
             $$->push_back($1); }
          | call_args TCOMMA expr
           { $$=$1;
             $$->push_back($3);
           }
          ;

comparison : expr TCOP1 expr
             { $$ = new NComparisonOperator($1,$2,$3); }
           ;

%%

