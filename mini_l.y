%{
  #include <stdio.h>
  #include <string>
  #include <iostream>
  using namespace std;
  
  int yyerror(string s);
  int yylex(void);
%}


%union {
  char* str_val;
  int int_val;
}


%start Program;
%token FUNCTION;
%token SEMI;
%token BEGINPARAMS;
%token ENDPARAMS;
%token BEGINLOCALS;
%token ENDLOCALS;
%token BEGINBODY;
%token ENDBODY;
%token COMMA;
%token COLON;
%token INT;
%token ARRAY;
%token LBRACKET;
%token RBRACKET;
%token OF;
%token ASSIGN;
%token IF;
%token THEN;
%token ELSE;
%token ENDIF;
%token WHILE;
%token BEGINLOOP;
%token ENDLOOP;
%token DO;
%token READ;
%token WRITE;
%token CONTINUE;
%token RETURN;
%token OR;
%token AND;
%token NOT;
%token TRUE;
%token FALSE;
%token LPAREN;
%token RPAREN;
%token EQUALTO;
%token NOTEQUAL;
%token LESSTHAN;
%token GREATERTHAN;
%token LESSTHANEQ;
%token GREATERTHANEQ;
%token PLUS;
%token MINUS
%token MULT;
%token DIV;
%token MOD;
%token<str_val> IDENT;
%token<str_val> NUMBER;
%%

Program: Functions {printf("Program -> Functions \n");}
  | %empty {printf("Program -> epsilon \n");}
  ;
Functions: Functions Function {printf("Functions -> Functions Function \n");}
  | Function {printf("Functions -> Function \n");}
  ;
Function: FUNCTION IDENT SEMI BEGINPARAMS Declarations ENDPARAMS BEGINLOCALS Declarations ENDLOCALS BEGINBODY Statements ENDBODY {printf("Function -> FUNCTION IDENT ; BEGINPARAMS Declarations ENDPARAMS BEGINLOCALS Declarations ENDLOCALS BEGINBODY Statements ENDBODY\n");}
  ;


Declarations: Declarations Declaration SEMI {printf("Declarations -> Declarations Declaration ;\n");}
  | %empty {printf("Declarations -> epsilon\n");}
  ;


/* Declaration */
Declaration: Identifiers COLON INT {printf("Declaration -> Identifiers : INTEGER\n");}
  | Identifiers COLON ARRAY LBRACKET NUMBER RBRACKET OF INT {printf("Declaration -> Identifiers : ARRAY [ NUMBER ] OF INT\n");}
  ;
Identifiers: IDENT {printf("Identifiers -> IDENT \n");}
  | Identifiers COMMA IDENT {printf("Identifiers -> Identifiers , IDENT \n");}
  ;


/* Statement */
Statement: Var ASSIGN Expression {printf("Statement -> Var := Expression\n");}
  | IF BoolExpr THEN Statements ENDIF {printf("Statement -> IF BoolExpr THEN StatementList ENDIF\n");}
  | IF BoolExpr THEN Statements ELSE Statements ENDIF {printf("Statement -> IF BoolExpr THEN StatementList ELSE StatementList ENDIF\n");}
  | WHILE BoolExpr BEGINLOOP Statements ENDLOOP {printf("Statement -> WHILE BoolExpr BEGINLOOP StatementList ENDLOOP\n");}
  | DO BEGINLOOP Statements ENDLOOP WHILE BoolExpr {printf("Statement -> DO BEGINLOOP StatementList ENDLOOP WHILE BoolExpr\n");}
  | READ VarList {printf("Statement -> READ VarList\n");}
  | WRITE VarList {printf("Statement -> WRITE VarList\n");}
  | CONTINUE {printf("Statement -> CONTINUE\n");}
  | RETURN Expression {printf("Statement -> RETURN Expression\n");}
  | Statements Statement SEMI {printf("Statements -> Statements Statement ;\n");}
  ;
Statements: Statement SEMI {printf("Statements -> Statement ;\n");}
  | Statement SEMI Statements {printf("Statements -> Statements Statement ;\n");}
  ;



/* Bool-Expr */
BoolExpr: RelationAndExpr OR RelationAndExpr {printf("BoolExpr -> BoolExpr OR RelationAndExpr\n");}
  | RelationAndExpr {printf("BoolExpr -> RelationAndExpr\n");}
  ;

/* Relation_And_Expr */
RelationAndExpr: RelationExpr AND RelationExpr {printf("RelationAndExpr -> RelationAndExpr AND RelationExpr\n");}
  | RelationExpr {printf("RelationAndExpr -> RelationExpr\n");}
  ;

/* Relation_Expr */
RelationExpr: Relations {printf("RelationExpr -> Relations\n");}
  | NOT Relations {printf("RelationExpr -> NOT Relations\n");}
  ;
Relations: Expression Comp Expression {printf("Relations -> Expression Comp Expression\n");}
  | TRUE {printf("Relations -> TRUE\n");}
  | FALSE {printf("Relations -> FALSE\n");}
  | LPAREN BoolExpr RPAREN {printf("Relations -> ( BoolExpr )\n");}
  ;

/* Comp */
Comp: EQUALTO {printf("Comp -> ==\n");}
  | NOTEQUAL {printf("Comp -> <>\n");}
  | LESSTHAN {printf("Comp -> <\n");}
  | GREATERTHAN {printf("Comp -> >\n");}
  | LESSTHANEQ {printf("Comp -> <=\n");}
  | GREATERTHANEQ {printf("Comp -> >=\n");}
  ;

/* Expression */
Expression: Expression PLUS MultiplicativeExpr {printf("Expression -> Expression + MultiplicativeExpr\n");}
  | Expression MINUS MultiplicativeExpr {printf("Expression -> Expression - MultiplicativeExpr\n");}
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
  | MINUS TermInner {printf("Term -> - TermInner\n");}
  | IDENT LPAREN ExpressionList RPAREN {printf("Term -> IDENT ( ExpressionList )\n");}
  ;
TermInner: Var {printf("TermInner -> Var\n");}
  | NUMBER {printf("TermInner -> %s\n", $1);}
  | LPAREN Expression RPAREN {printf("TermInner -> ( Expression )\n");}
  ;

/* Var */
Var: IDENT {printf("Var -> IDENT\n");}
  | IDENT LBRACKET Expression RBRACKET {printf("Var -> IDENT [ Expression ]\n");}
  ;
VarList: Var {printf("VarList -> Var\n");}
  | Var COMMA VarList {printf("VarList -> VarList Var ,\n");}
  ;




%%
int yyerror(string s) {
  extern int yylineno, currPos;
  extern char *yytext;

  cout << "ERROR - " << s << " : at symbol " << yytext << " on line " << yylineno << ", column " << currPos << endl;
  exit(1);
}

