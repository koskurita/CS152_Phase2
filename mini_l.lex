%{
  #include "mini_l.tab.h"
  #include "heading.h"
  #include <string>
  using namespace std;
  extern int yyerror(string s);
  int currPos = 1;
  extern int yylineno;
%}


DIGIT [0-9]
LETTER [a-zA-Z]

%%
"function" {currPos += yyleng; return FUNCTION;}
"beginparams" {currPos += yyleng; return BEGINPARAMS;}
"endparams" {currPos += yyleng; return ENDPARAMS;}
"beginlocals" {currPos += yyleng; return BEGINLOCALS;}
"endlocals" {currPos += yyleng; return ENDLOCALS;}
"beginbody" {currPos += yyleng; return BEGINBODY;}
"endbody" {currPos += yyleng; return ENDBODY;}
"integer" {currPos += yyleng; return INT;}
"array" {currPos += yyleng; return ARRAY;}
"of" {currPos += yyleng; return OF;}
"if" {currPos += yyleng; return IF;}
"then" {currPos += yyleng; return THEN;}
"endif" {currPos += yyleng; return ENDIF;}
"else" {currPos += yyleng; return ELSE;}
"while" {currPos += yyleng; return WHILE;}
"do" {currPos += yyleng; return DO;}
"beginloop" {currPos += yyleng; return BEGINLOOP;}
"endloop" {currPos += yyleng; return ENDLOOP;}
"continue" {currPos += yyleng; return CONTINUE;}
"read" {currPos += yyleng; return READ;}
"write" {currPos += yyleng; return WRITE;}
"and" {currPos += yyleng; return AND;}
"or" {currPos += yyleng; return OR;}
"not" {currPos += yyleng; return NOT;}
"true" {currPos += yyleng; return TRUE;}
"false" {currPos += yyleng; return FALSE;}
"return" {currPos += yyleng; return RETURN;}

"-" {currPos += yyleng; return MINUS;}
"+" {currPos += yyleng; return PLUS;}
"*" {currPos += yyleng; return MULT;}
"/" {currPos += yyleng; return DIV;}
"%" {currPos += yyleng; return MOD;}

"==" {currPos += yyleng; return EQUALTO;}
"<=" {currPos += yyleng; return LESSTHANEQ;}
">=" {currPos += yyleng; return GREATERTHANEQ;}
"<" {currPos += yyleng; return LESSTHAN;}
">" {currPos += yyleng; return GREATERTHAN;}
"<>" {currPos += yyleng; return NOTEQUAL;}


";" {currPos += yyleng; return SEMI;}
":" {currPos += yyleng; return COLON;}
"," {currPos += yyleng; return COMMA;}
"(" {currPos += yyleng; return LPAREN;}
")" {currPos += yyleng; return RPAREN;}
"[" {currPos += yyleng; return LBRACKET;}
"]" {currPos += yyleng; return RBRACKET;}
":=" {currPos += yyleng; return ASSIGN;}

{DIGIT}+ {yylval.str_val = strdup(yytext); currPos += yyleng; return NUMBER;}

("_"[a-zA-Z0-9_]*)|({DIGIT}[a-zA-Z0-9_]*) {yyerror("Identifier must begin with a letter "); exit(1);}

([a-zA-Z0-9_]*"_") {yyerror("Identifier cannot end with an underscore"); exit(1);}

({LETTER}+[a-zA-Z0-9_]*) {yylval.str_val = strdup(yytext); currPos += yyleng; return IDENT;}

[ \t]+ {/*ignore : whitespace */ currPos += yyleng;}

"##".* {/*ignore : comments */ yylineno++; currPos += yyleng;}

"\n" {yylineno++; currPos = 1;}

. { yyerror("Error in parser"); exit(0);}

%%

int yyparse();

int main(int argc, char **argv) {
  if ((argc > 1) && (freopen(argv[1], "r", stdin) == NULL)) {
	std::cerr << argv[0] << ": File " << argv[1] << " cannot be opened.\n";
  }
  
  yyparse();
  return 0;
}
