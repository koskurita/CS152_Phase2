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


Function: FUNCTION Identifier SEMI BEGINPARAMS Declarations ENDPARAMS BEGINLOCALS Declarations ENDLOCALS BEGINBODY Statements ENDBODY {printf("Function -> FUNCTION Identifier ; BEGINPARAMS Declarations ENDPARAMS BEGINLOCALS Declarations ENDLOCALS BEGINBODY Statements ENDBODY\n");}
  ;
Declarations: Declarations Declaration SEMI {printf("Declarations -> Declarations Declaration ;\n");}
  | %empty {printf("Declarations -> epsilon\n");}
  ;


Declaration: Identifiers COLON INT {printf("Declaration -> Identifiers : INT\n");}
  | Identifiers COLON ARRAY LBRACKET NUMBER RBRACKET OF INT {printf("Declaration -> Identifiers : ARRAY [ NUMBER ] OF INT\n");}
  ;
Identifiers: Identifier {printf("Identifiers -> Identifier \n");}
  | Identifiers COMMA Identifier {printf("Identifiers -> Identifiers , Identifier \n");}
  ;
Identifier: IDENT {printf("Identifier -> %s \n", $1);}
  ;


Statement: Var ASSIGN Expression {printf("Statement -> Var := Expression\n");}
  | IF BoolExpr THEN Statements ENDIF {printf("Statement -> IF BoolExpr THEN Statements ENDIF\n");}
  | IF BoolExpr THEN Statements ELSE Statements ENDIF {printf("Statement -> IF BoolExpr THEN Statements ELSE Statements ENDIF\n");}
  | WHILE BoolExpr BEGINLOOP Statements ENDLOOP {printf("Statement -> WHILE BoolExpr BEGINLOOP Statements ENDLOOP\n");}
  | DO BEGINLOOP Statements ENDLOOP WHILE BoolExpr {printf("Statement -> DO BEGINLOOP Statements ENDLOOP WHILE BoolExpr\n");}
  | READ Vars {printf("Statement -> READ Vars\n");}
  | WRITE Vars {printf("Statement -> WRITE Vars\n");}
  | CONTINUE {printf("Statement -> CONTINUE\n");}
  | RETURN Expression {printf("Statement -> RETURN Expression\n");}
  ;
Statements: Statement SEMI {printf("Statements -> Statement ;\n");}
  | Statements Statement SEMI {printf("Statements -> Statements Statement ;\n");}
  ;

BoolExpr: BoolExpr OR RelationAndExpr {printf("BoolExpr -> BoolExpr OR RelationAndExpr\n");}
  | RelationAndExpr {printf("BoolExpr -> RelationAndExpr\n");}
  ;
RelationAndExpr: RelationAndExpr AND RelationExpr {printf("RelationAndExpr -> RelationAndExpr AND RelationExpr\n");}
  | RelationExpr {printf("RelationAndExpr -> RelationExpr\n");}
  ;
RelationExpr: RelationComponent {printf("RelationExpr -> RelationComponent\n");}
  | NOT RelationComponent {printf("RelationExpr -> NOT RelationComponent\n");}
  ;
RelationComponent: Expression Comp Expression {printf("RelationComponent -> Expression Comp Expression\n");}
  | TRUE {printf("RelationComponent -> TRUE\n");}
  | FALSE {printf("RelationComponent -> FALSE\n");}
  | LPAREN BoolExpr RPAREN {printf("RelationComponent -> ( BoolExpr )\n");}
  ;


Comp: EQUALTO {printf("Comp -> ==\n");}
  | NOTEQUAL {printf("Comp -> <>\n");}
  | LESSTHAN {printf("Comp -> <\n");}
  | GREATERTHAN {printf("Comp -> >\n");}
  | LESSTHANEQ {printf("Comp -> <=\n");}
  | GREATERTHANEQ {printf("Comp -> >=\n");}
  ;


Expression: Expression PLUS MultiplicativeExpr {printf("Expression -> Expression + MultiplicativeExpr\n");}
  | Expression MINUS MultiplicativeExpr {printf("Expression -> Expression - MultiplicativeExpr\n");}
  | MultiplicativeExpr {printf("Expression -> MultiplicativeExpr\n");}
  ;
Expressions: Expressions COMMA Expression {printf("Expressions -> Expressions , Expression\n");}
  | Expression {printf("Expressions -> Expression\n");}
  | %empty {printf("Expressions -> epsilon\n");}
  ;


MultiplicativeExpr: MultiplicativeExpr MULT Term {printf("MultiplicativeExpr -> MultiplicativeExpr * Term\n");}
  | MultiplicativeExpr DIV Term {printf("MultiplicativeExpr -> MultiplicativeExpr / Term\n");}
  | MultiplicativeExpr MOD Term {printf("MultiplicativeExpr -> MultiplicativeExpr % Term\n");}
  | Term {printf("MultiplicativeExpr -> Term\n");}
  ;


Term: TermTop {printf("Term -> TermTop\n");}
  | MINUS TermTop {printf("Term -> - TermTop\n");}
  | Identifier LPAREN Expressions RPAREN {printf("Term -> Identifier ( Expressions )\n");}
  ;
TermTop: Var {printf("TermTop -> Var\n");}
  | NUMBER {printf("TermTop -> %s\n", $1);}
  | LPAREN Expression RPAREN {printf("TermTop -> ( Expression )\n");}
  ;

/* Var */
Var: Identifier {printf("Var -> Identifier\n");}
  | Identifier LBRACKET Expression RBRACKET {printf("Var -> Identifier [ Expression ]\n");}
  ;
Vars: Var {printf("Vars -> Var\n");}
  | Var COMMA Vars {printf("Vars -> VarList Var ,\n");}
  ;


%%
int yyerror(string s) {
  extern int yylineno, currPos;
  extern char *yytext;

  cout << "ERROR - " << s << " : at symbol " << yytext << " on line " << yylineno << ", column " << currPos << endl;
  exit(1);
}

