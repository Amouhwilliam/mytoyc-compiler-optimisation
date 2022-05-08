%{
#include <cstdio>
#include <string>
#include "node.hpp"
#include "parser.hpp"

%}

%option noyywrap
%option yylineno

%%

[ \t\n]                         ;
"//".*$                         ;
"extern"                        {return TEXTERN;}
"return"                        {return TRETURN;}
"int"                           {yylval.value=TINT; return TTYPE;} // William AMOUH modification
"void"                          {yylval.value=TVOID; return TTYPE;} // William AMOUH modification
"if"                            {return TIF;}
"else"                          {return TELSE;}
[a-zA-Z_][a-zA-Z0-9_]*          {return TIDENTIFIER;}
[0-9]+                          {yylval.value=std::stoi(yytext);return TINTEGER;}

"="                             {return TASSIGN;}
"=="                            {yylval.value=EQ;return TCOP1;}
"!="                            {yylval.value=NE;return TCOP1;}
"<"                             {yylval.value=LT;return TCOP1;}
"<="                            {yylval.value=LE;return TCOP1;}
">"                             {yylval.value=GT;return TCOP1;}
">="                            {yylval.value=GE;return TCOP1;}

"("                             {return TLPAREN;}
")"                             {return TRPAREN;}
"{"                             {return TLBRACE;}
"}"                             {return TRBRACE;}

","                             {return TCOMMA;}
";"                             {return TSEMICOL;}

"+"                             {yylval.value=PLUS; return TBOP2;}
"-"                             {yylval.value=MINUS;return TBOP2;}
"*"                             {yylval.value=MUL;  return TBOP3;}
"/"                             {yylval.value=DIV;  return TBOP3;}

.                               {fprintf(stderr,"Unknown token '%s' in line %d!\n",yytext,yylineno); yyterminate();}

%%
