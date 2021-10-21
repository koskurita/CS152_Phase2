%{
  #include "stdio.h"
  #include <string>
  #include <iostream>
  using namespace std;
  int yyeror(char *s);
  int yyerror(string s);
  int yylex(void);
%}

%union {
  int int_val;
  char* str_val;
}

%start Program
%token FUNCTION;
%token BEGIN_PARAMS;
%token END_PARAMS;
%token BEGIN_LOCALS;
%token END_LOCALS;
%token BEGIN_BODY;
%token END_BODY;
%token INTEGER;
%token ARRAY;
%token OF;
%token IF;
%token THEN;
%token ENDIF;
%token ELSE;
%token WHILE;
%token DO;
%token FOR;
%token BEGINLOOP;
%token ENDLOOP;
%token CONTINUE;
%token READ;
%token WRITE;
%token AND;
%token OR;
%token NOT;
%token TRUE;
%token FALSE;
%token RETURN;

%token SUB;
%token ADD;
%token MULT;
%token DIV;
%token MOD;

%token EQ;
%token NEQ;
%token LT;
%token GT;
%token LTE;
%token GTE;

%token<str_val> IDENT;
%token<str_val> NUMBER;

%token SEMICOLON;
%token COLON;
%token COMMA;
%token L_PAREN;
%token R_PAREN;
%token L_SQUARE_BRACKET;
%token R_SQUARE_BRACKET;
%token ASSIGN;

%%
/* Program */
Program: FunctionList {printf("Program -> FunctionList \n");}
  | %empty {printf("Program -> epsilon \n");}
  ;
FunctionList: FunctionList Function {printf("FunctionList -> FunctionList Function \n");}
  | Function {printf("FunctionList -> Function \n");}
  ;

/* Function */
Function: FUNCTION Identifier SEMICOLON FunctionParams FunctionLocals FunctionBody {printf("Function -> FUNCTION Identifier ; FunctionParams FunctionLocals FunctionBody \n");}
  ;
FunctionParams: BEGIN_PARAMS DeclarationList END_PARAMS {printf("FunctionParams -> BEGIN_PARAMS DeclarationList END_PARAMS \n");}
  | BEGIN_PARAMS END_PARAMS {printf("FunctionParams -> BEGIN_PARAMS END_PARAMS \n");}
  ;
FunctionLocals: BEGIN_LOCALS DeclarationList END_LOCALS {printf("FunctionLocals -> BEGIN_LOCALS DeclarationList END_LOCALS \n");}
  | BEGIN_LOCALS END_LOCALS {printf("FunctionLocals -> BEGIN_LOCALS END_LOCALS \n");}
  ;
FunctionBody: BEGIN_BODY StatementList END_BODY {printf("FunctionBody -> BEGIN_BODY StatementList END_BODY \n");}
  | BEGIN_BODY END_BODY {printf("FunctionBody -> BEGIN_BODY END_BODY \n");}
  ;
DeclarationList: DeclarationList Declaration SEMICOLON {printf("DeclarationList -> DeclarationList Declaration ;\n");}
  | Declaration SEMICOLON {printf("DeclarationList -> Declaration ;\n");}
  ;

/* Declaration */
Declaration: IdentifierList COLON INTEGER {printf("Declaration -> IdentifierList : INTEGER\n");}
  | IdentifierList COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("Declaration -> IdentifierList : ARRAY [ NUMBER ] OF INTEGER \n");}
  ;
IdentifierList: Identifier {printf("IdentifierList -> Identifier \n");}
  | IdentifierList COMMA Identifier {printf("IdentifierList -> IdentifierList , Identifier \n");}
  ;
Identifier: IDENT {printf("Identifier -> %s \n", $1);}
  ;

/* Statement */
Statement: Var ASSIGN Expression {printf("Statement -> Var := Expression\n");}
  | IF BoolExpr THEN StatementList ENDIF {printf("Statement -> IF BoolExpr THEN StatementList ENDIF\n");}
  | IF BoolExpr THEN StatementList ELSE StatementList ENDIF {printf("Statement -> IF BoolExpr THEN StatementList ELSE StatementList ENDIF\n");}
  | WHILE BoolExpr BEGINLOOP StatementList ENDLOOP {printf("Statement -> WHILE BoolExpr BEGINLOOP StatementList ENDLOOP\n");}
  | DO BEGINLOOP StatementList ENDLOOP WHILE BoolExpr {printf("Statement -> DO BEGINLOOP StatementList ENDLOOP WHILE BoolExpr\n");}
  | FOR Var ASSIGN NUMBER SEMICOLON BoolExpr SEMICOLON Var ASSIGN Expression BEGINLOOP StatementList ENDLOOP {printf("Statement -> FOR Var := NUMBER ; BoolExpr ; Var := Expression BEGINLOOP StatementList ENDLOOP\n");}
  | READ VarList {printf("Statement -> READ VarList\n");}
  | WRITE VarList {printf("Statement -> WRITE VarList\n");}
  | CONTINUE {printf("Statement -> CONTINUE\n");}
  | RETURN Expression {printf("Statement -> RETURN Expression\n");}
  ;
StatementList: Statement SEMICOLON {printf("StatementList -> Statement ;\n");}
  | StatementList Statement SEMICOLON {printf("StatementList -> StatementList Statement ;\n");}
  ;

/* Bool-Expr */
BoolExpr: BoolExpr OR RelationAndExpr {printf("BoolExpr -> BoolExpr OR RelationAndExpr\n");}
  | RelationAndExpr {printf("BoolExpr -> RelationAndExpr\n");}
  ;
/* Relation_And_Expr */
RelationAndExpr: RelationAndExpr AND RelationExpr {printf("RelationAndExpr -> RelationAndExpr AND RelationExpr\n");}
  | RelationExpr {printf("RelationAndExpr -> RelationExpr\n");}
  ;

/* Relation_Expr */
RelationExpr: Relations {printf("RelationExpr -> Relations\n");}
  | NOT Relations {printf("RelationExpr -> NOT Relations\n");}
  ;
Relations: Expression Comp Expression {printf("Relations -> Expression Comp Expression\n");}
  | TRUE {printf("Relations -> TRUE\n");}
  | FALSE {printf("Relations -> FALSE\n");}
  | L_PAREN BoolExpr R_PAREN {printf("Relations -> ( BoolExpr )\n");}
  ;

/* Comp */
Comp: EQ {printf("Comp -> ==\n");}
  | NEQ {printf("Comp -> <>\n");}
  | LT {printf("Comp -> <\n");}
  | GT {printf("Comp -> >\n");}
  | LTE {printf("Comp -> <=\n");}
  | GTE {printf("Comp -> >=\n");}
  ;

/* Expression */
Expression: Expression ADD MultiplicativeExpr {printf("Expression -> Expression + MultiplicativeExpr\n");}
  | Expression SUB MultiplicativeExpr {printf("Expression -> Expression - MultiplicativeExpr\n");}
  | MultiplicativeExpr {printf("Expression -> MultiplicativeExpr\n");}
  ;
ExpressionList: ExpressionList COMMA Expression {printf("ExpressionList -> ExpressionList Expression ,\n");}
  | Expression {printf("ExpressionList -> Expression\n");}
  | %empty {printf("ExpressionList -> epsilon\n");}
  ;

/* Multiplicative_Expr */
MultiplicativeExpr: MultiplicativeExpr MULT Term {printf("MultiplicativeExpr -> MultiplicativeExpr * Term\n");}
  | MultiplicativeExpr DIV Term {printf("MultiplicativeExpr -> MultiplicativeExpr / Term\n");}
  | MultiplicativeExpr MOD Term {printf("MultiplicativeExpr -> MultiplicativeExpr % Term\n");}
  | Term {printf("MultiplicativeExpr -> Term\n");}
  ;

/* Term */
Term: TermInner {printf("Term -> TermInner\n");}
  | SUB TermInner {printf("Term -> - TermInner\n");}
  | Identifier L_PAREN ExpressionList R_PAREN {printf("Term -> Identifier ( ExpressionList )\n");}
  ;
TermInner: Var {printf("TermInner -> Var\n");}
  | NUMBER {printf("TermInner -> %s\n", $1);}
  | L_PAREN Expression R_PAREN {printf("TermInner -> ( Expression )\n");}
  ;

/* Var */
Var: Identifier {printf("Var -> Identifier\n");}
  | Identifier L_SQUARE_BRACKET Expression R_SQUARE_BRACKET {printf("Var -> Identifier [ Expression ]\n");}
  ;
VarList: Var {printf("VarList -> Var\n");}
  | Var COMMA VarList {printf("VarList -> VarList Var ,\n");}
  ;

%%
int yyerror(string s) {
  extern int currLine, currPos;
  extern char *yytext;

  cout << "ERROR " << s << " : at symbol " << yytext << " on line " << currLine << ", column " << currPos << endl;
  // printf("ERROR %s at symbol \"%s\" on line %s, column %s\n"), (s, yytext, currLine, currPos);
  exit(1);
}

int yyerror(char* s) {
  return yyerror(string(s));
}
