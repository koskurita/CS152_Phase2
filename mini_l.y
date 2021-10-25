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

/* Program */
Program: Functions {printf("Program -> Functions \n");}
  | %empty {printf("Program -> epsilon \n");}
  ;
Functions: Functions Function {printf("Functions -> Functions Function \n");}
  | Function {printf("Functions -> Function \n");}
  ;

/* Function */
Function: FUNCTION Identifier SEMI BEGINPARAMS Declarations ENDPARAMS BEGINLOCALS Declarations ENDLOCALS BEGINBODY Statements ENDBODY {printf("Function -> FUNCTION Identifier ; FunctionParams FunctionLocals FunctionBody \n");}
  ;
Declarations: Declarations Declaration SEMI {printf("DeclarationList -> DeclarationList Declaration ;\n");}
  | %empty {printf("DeclarationList -> Declaration ;\n");}
  ;

/* Declaration */
Declaration: IdentifierList COLON INT {printf("Declaration -> IdentifierList : INTEGER\n");}
  | IdentifierList COLON ARRAY LBRACKET NUMBER RBRACKET OF INT {printf("Declaration -> IdentifierList : ARRAY [ NUMBER ] OF INTEGER \n");}
  ;
IdentifierList: Identifier {printf("IdentifierList -> Identifier \n");}
  | IdentifierList COMMA Identifier {printf("IdentifierList -> IdentifierList , Identifier \n");}
  ;
Identifier: IDENT {printf("Identifier -> %s \n", $1);}
  ;

/* Statement */
Statement: Var ASSIGN Expression {printf("Statement -> Var := Expression\n");}
  | IF BoolExpr THEN Statements ENDIF {printf("Statement -> IF BoolExpr THEN StatementList ENDIF\n");}
  | IF BoolExpr THEN Statements ELSE Statements ENDIF {printf("Statement -> IF BoolExpr THEN StatementList ELSE StatementList ENDIF\n");}
  | WHILE BoolExpr BEGINLOOP Statements ENDLOOP {printf("Statement -> WHILE BoolExpr BEGINLOOP StatementList ENDLOOP\n");}
  | DO BEGINLOOP Statements ENDLOOP WHILE BoolExpr {printf("Statement -> DO BEGINLOOP StatementList ENDLOOP WHILE BoolExpr\n");}
  | READ Vars {printf("Statement -> READ VarList\n");}
  | WRITE Vars {printf("Statement -> WRITE VarList\n");}
  | CONTINUE {printf("Statement -> CONTINUE\n");}
  | RETURN Expression {printf("Statement -> RETURN Expression\n");}
  ;
Statements: Statement SEMI {printf("StatementList -> Statement ;\n");}
  | Statements Statement SEMI {printf("StatementList -> StatementList Statement ;\n");}
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
RelationExpr: RelationComponent {printf("RelationExpr -> Relations\n");}
  | NOT RelationComponent {printf("RelationExpr -> NOT Relations\n");}
  ;
RelationComponent: Expression Comp Expression {printf("RelationComp -> Expression Comp Expression\n");}
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

Expressions: Expressions COMMA Expression {printf("ExpressionList -> ExpressionList Expression ,\n");}
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
Term: TermTop {printf("Term -> TermInner\n");}
  | MINUS TermTop {printf("Term -> - TermInner\n");}
  | Identifier LPAREN Expressions RPAREN {printf("Term -> Identifier ( ExpressionList )\n");}
  ;
TermTop: Var {printf("TermInner -> Var\n");}
  | NUMBER {printf("TermInner -> %s\n", $1);}
  | LPAREN Expression RPAREN {printf("TermInner -> ( Expression )\n");}
  ;

/* Var */
Var: Identifier {printf("Var -> Identifier\n");}
  | Identifier LBRACKET Expression RBRACKET {printf("Var -> Identifier [ Expression ]\n");}
  ;
Vars: Var {printf("VarList -> Var\n");}
  | Var COMMA Vars {printf("VarList -> VarList Var ,\n");}
  ;


%%
int yyerror(string s) {
  extern int yylineno, currPos;
  extern char *yytext;

  cout << "ERROR - " << s << " : at symbol " << yytext << " on line " << yylineno << ", column " << currPos << endl;
  exit(1);
}

