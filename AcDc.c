#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "header.h"


int main( int argc, char *argv[] )
{
	ungotten = 0;
    FILE *source, *target;
    Program program;
    SymbolTable symtab;

    if( argc == 3){
        source = fopen(argv[1], "r");
        target = fopen(argv[2], "w");
        if( !source ){
            printf("can't open the source file\n");
            exit(2);
        }
        else if( !target ){
            printf("can't open the target file\n");
            exit(2);
        }
        else{
            program = parser(source);
            fclose(source);
            symtab = build(program);
			/*
			for(int i=0; i<26; i++)
				printf("%c: %d\n", i+'a', symtab.table[i]);
			*/
            check(&program, &symtab);
            gencode(program, target);
        }
    }
    else{
        printf("Usage: %s source_file target_file\n", argv[0]);
    }


    return 0;
}


/********************************************* 
  Scanning 
 *********************************************/
Token getNumericToken( FILE *source, char c )
{
    Token token;
    int i = 0;

    while( isdigit(c) ) {
        token.tok[i++] = c;
        c = fgetc(source);
    }

    if( c != '.' ){
        //ungetc(c, source);
        token.tok[i] = '\0';
        token.type = IntValue;
        return token;
    }

    token.tok[i++] = '.';

    c = fgetc(source);
    if( !isdigit(c) ){
        //ungetc(c, source);
        printf("Expect a digit : %c\n", c);
        exit(1);
    }

    while( isdigit(c) ){
        token.tok[i++] = c;
        c = fgetc(source);
    }

    //ungetc(c, source);
    token.tok[i] = '\0';
    token.type = FloatValue;
    return token;
}

Token getAlphaToken( FILE *source, char c) {
	Token token;
	int i=0;
	while( isalpha(c) ) {
		token.tok[i++] = c;
		c = fgetc(source);
	}
	token.tok[i] = '\0';
	if( strcmp(token.tok, "f") == 0 )
		token.type = FloatDeclaration;
	else if( strcmp(token.tok, "i") == 0 )
		token.type = IntegerDeclaration;
	else if( strcmp(token.tok, "p") == 0 )
		token.type = PrintOp;
	else {
		token.tok[0] = getVarName(token.tok);
		token.tok[1] = '\0';
		token.type = Alphabet;
	}
	return token;
}

Token scanner( FILE *source )
{
    char c;
    static Token token = {EOFsymbol, ""};

	if(ungotten) {
		ungotten = 0;
		return token;
	}

    while( !feof(source) ){
        c = fgetc(source);

        while( isspace(c) ) c = fgetc(source);

        if( isdigit(c) )
            return token = getNumericToken(source, c);
		if( isalpha(c) )
			return token = getAlphaToken(source, c);

        token.tok[0] = c;
        token.tok[1] = '\0';
        switch(c){
            case '=':
                token.type = AssignmentOp;
                return token;
            case '+':
                token.type = PlusOp;
                return token;
            case '-':
                token.type = MinusOp;
                return token;
            case '*':
                token.type = MulOp;
                return token;
            case '/':
                token.type = DivOp;
                return token;
            case EOF:
                token.type = EOFsymbol;
                token.tok[0] = '\0';
                return token;
            default:
                printf("Invalid character : %c\n", c);
                exit(1);
        }
    }

    token.tok[0] = '\0';
    token.type = EOFsymbol;
    return token;
}


/********************************************************
  Parsing
 *********************************************************/
Declaration parseDeclaration( FILE *source, Token token )
{
    Token token2;
    switch(token.type){
        case FloatDeclaration:
        case IntegerDeclaration:
            token2 = scanner(source);
            if (strcmp(token2.tok, "f") == 0 ||
                    strcmp(token2.tok, "i") == 0 ||
                    strcmp(token2.tok, "p") == 0) {
                printf("Syntax Error: %s cannot be used as id\n", token2.tok);
                exit(1);
            }
            return makeDeclarationNode( token, token2 );
        default:
            printf("Syntax Error: Expect Declaration %s\n", token.tok);
            exit(1);
    }
}

Declarations *parseDeclarations( FILE *source )
{
    Token token = scanner(source);
    Declaration decl;
    Declarations *decls;
    switch(token.type){
        case FloatDeclaration:
        case IntegerDeclaration:
            decl = parseDeclaration(source, token);
            decls = parseDeclarations(source);
            return makeDeclarationTree( decl, decls );
        case PrintOp:
        case Alphabet:
			// TODO
			ungotten = 1;
            //ungetc(token.tok[0], source);
            return NULL;
        case EOFsymbol:
            return NULL;
        default:
            printf("Syntax Error: Expect declarations %s\n", token.tok);
            exit(1);
    }
}

Expression *parseValue( FILE *source )
{
    Token token = scanner(source);
    Expression *value = (Expression *)malloc( sizeof(Expression) );
    value->leftOperand = value->rightOperand = NULL;

    switch(token.type){
        case Alphabet:
            (value->v).type = Identifier;
            (value->v).val.id = token.tok[0];
            break;
        case IntValue:
            (value->v).type = IntConst;
            (value->v).val.ivalue = atoi(token.tok);
            break;
        case FloatValue:
            (value->v).type = FloatConst;
            (value->v).val.fvalue = atof(token.tok);
            break;
        default:
            printf("Syntax Error: Expect Identifier or a Number %s\n", token.tok);
            exit(1);
    }

    return value;
}

Expression *ConstFolding( Expression *expr ) {
	if(expr->leftOperand == NULL || expr->rightOperand == NULL)
		return expr;
	ValueType ltype = (expr->leftOperand->v).type;
	ValueType rtype = (expr->rightOperand->v).type;
	if((ltype == IntConst || ltype == FloatConst)
		&& (rtype == IntConst || rtype == FloatConst)) {
		if(ltype == IntConst && rtype == IntConst) {
			int res;
			(expr->leftOperand->v).type = IntConst;
			switch((expr->v).val.op) {

				case Plus:
					res = (expr->leftOperand->v).val.ivalue + (expr->rightOperand->v).val.ivalue;
					break;
				case Minus:
					res = (expr->leftOperand->v).val.ivalue - (expr->rightOperand->v).val.ivalue;
					break;
				case Mul:
					res = (expr->leftOperand->v).val.ivalue * (expr->rightOperand->v).val.ivalue;
					break;
				case Div:
					res = (expr->leftOperand->v).val.ivalue / (expr->rightOperand->v).val.ivalue;
					break;
				default:
					printf("Constant Folding Error.\n");
					exit(1);
			}
			(expr->leftOperand->v).val.ivalue = res;
		} else {
			float res;
			if(ltype == IntConst)
				(expr->leftOperand->v).val.fvalue = (expr->leftOperand->v).val.ivalue;
			if(rtype == IntConst)
				(expr->rightOperand->v).val.fvalue = (expr->rightOperand->v).val.ivalue;
			(expr->leftOperand->v).type = FloatConst;
			switch((expr->v).val.op) {
				case Plus:
					res = (expr->leftOperand->v).val.fvalue + (expr->rightOperand->v).val.fvalue;
					break;
				case Minus:
					res = (expr->leftOperand->v).val.fvalue - (expr->rightOperand->v).val.fvalue;
					break;
				case Mul:
					res = (expr->leftOperand->v).val.fvalue * (expr->rightOperand->v).val.fvalue;
					break;
				case Div:
					res = (expr->leftOperand->v).val.fvalue / (expr->rightOperand->v).val.fvalue;
					break;
				default:
					printf("Constant Folding Error.\n");
					exit(1);
			}
			(expr->leftOperand->v).val.fvalue = res;
		}
		return expr->leftOperand;
	} else
		return expr;
}

Expression *parseTerm( FILE *source, Expression *lvalue) {
	if(lvalue == NULL) {
		Expression *value = parseValue(source);
		return parseTerm(source, value);
	}

	Token token = scanner(source);
	Expression *term;
	switch(token.type) {
		case MulOp:
			term = (Expression *)malloc( sizeof(Expression) );
			(term->v).type = MulNode;
			(term->v).val.op = Mul;
			term->leftOperand = lvalue;
			term->rightOperand = parseValue(source);
			term = ConstFolding(term);
			return parseTerm(source, term);
		case DivOp:
			term = (Expression *)malloc( sizeof(Expression) );
			(term->v).type = DivNode;
			(term->v).val.op = Div;
			term->leftOperand = lvalue;
			term->rightOperand = parseValue(source);
			term = ConstFolding(term);
			return parseTerm(source, term);
		case PlusOp:
		case MinusOp:
		case Alphabet:
		case PrintOp:
			ungotten = 1;
		case EOFsymbol:
			return lvalue;
		default:
            printf("Syntax Error: You know why\n");
            exit(1);
	}
}

Expression *parseExpression( FILE *source, Expression *lvalue )
{
	if(lvalue == NULL) {
		Expression *term = parseTerm(source, NULL);
		return parseExpression(source, term);
	}

    Token token = scanner(source);
    Expression *expr;
    switch(token.type){
        case PlusOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = PlusNode;
            (expr->v).val.op = Plus;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseTerm(source, NULL);
			expr = ConstFolding(expr);
            return parseExpression(source, expr);
        case MinusOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = MinusNode;
            (expr->v).val.op = Minus;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseTerm(source, NULL);
			expr = ConstFolding(expr);
            return parseExpression(source, expr);
        case Alphabet:
        case PrintOp:
			ungotten = 1;
            //ungetc(token.tok[0], source);
        case EOFsymbol:
            return lvalue;
        default:
            printf("Syntax Error: Expect a numeric value or an identifier %s\n", token.tok);
            exit(1);
    }
}

Statement parseStatement( FILE *source, Token token )
{
    Token next_token;
    //Expression *value, *term, *expr;

    switch(token.type){
        case Alphabet:
            next_token = scanner(source);
            if(next_token.type == AssignmentOp){
				Expression *expr = parseExpression(source, NULL);
                return makeAssignmentNode(token.tok[0], expr);
            }
            else{
                printf("Syntax Error: Expect an assignment op %s\n", next_token.tok);
                exit(1);
            }
        case PrintOp:
            next_token = scanner(source);
            if(next_token.type == Alphabet)
                return makePrintNode(next_token.tok[0]);
            else{
                printf("Syntax Error: Expect an identifier %s\n", next_token.tok);
                exit(1);
            }
            break;
        default:
            printf("Syntax Error: Expect a statement %s\n", token.tok);
            exit(1);
    }
}

Statements *parseStatements( FILE * source )
{

    Token token = scanner(source);
    Statement stmt;
    Statements *stmts;

    switch(token.type){
        case Alphabet:
        case PrintOp:
            stmt = parseStatement(source, token);
            stmts = parseStatements(source);
            return makeStatementTree(stmt , stmts);
        case EOFsymbol:
            return NULL;
        default:
            printf("Syntax Error: Expect statements %s\n", token.tok);
            exit(1);
    }
}


/*********************************************************************
  Build AST
 **********************************************************************/
Declaration makeDeclarationNode( Token declare_type, Token identifier )
{
    Declaration tree_node;

    switch(declare_type.type){
        case FloatDeclaration:
            tree_node.type = Float;
            break;
        case IntegerDeclaration:
            tree_node.type = Int;
            break;
        default:
            break;
    }
    tree_node.name = identifier.tok[0];

    return tree_node;
}

Declarations *makeDeclarationTree( Declaration decl, Declarations *decls )
{
    Declarations *new_tree = (Declarations *)malloc( sizeof(Declarations) );
    new_tree->first = decl;
    new_tree->rest = decls;

    return new_tree;
}


Statement makeAssignmentNode( char id, Expression *expr )
{
    Statement stmt;
    AssignmentStatement assign;

    stmt.type = Assignment;
    assign.id = id;
	assign.expr = expr;
    stmt.stmt.assign = assign;

    return stmt;
}

Statement makePrintNode( char id )
{
    Statement stmt;
    stmt.type = Print;
    stmt.stmt.variable = id;

    return stmt;
}

Statements *makeStatementTree( Statement stmt, Statements *stmts )
{
    Statements *new_tree = (Statements *)malloc( sizeof(Statements) );
    new_tree->first = stmt;
    new_tree->rest = stmts;

    return new_tree;
}

/* parser */
Program parser( FILE *source )
{
    Program program;

    program.declarations = parseDeclarations(source);
    program.statements = parseStatements(source);

    return program;
}


/********************************************************
  Build symbol table
 *********************************************************/
void InitializeTable( SymbolTable *table )
{
    int i;

    for(i = 0 ; i < 26; i++)
        table->table[i] = Notype;
}

void add_table( SymbolTable *table, char c, DataType t )
{
    int index = (int)(c - 'a');

    if(table->table[index] != Notype)
        printf("Error : id %c has been declared\n", c);//error
    table->table[index] = t;
}

SymbolTable build( Program program )
{
    SymbolTable table;
    Declarations *decls = program.declarations;
    Declaration current;

    InitializeTable(&table);

    while(decls !=NULL){
        current = decls->first;
        add_table(&table, current.name, current.type);
        decls = decls->rest;
    }

    return table;
}


/********************************************************************
  Type checking
 *********************************************************************/

void convertType( Expression * old, DataType type )
{
    if(old->type == Float && type == Int){
        printf("error : can't convert float to integer\n");
        return;
    }
    if(old->type == Int && type == Float){
        Expression *tmp = (Expression *)malloc( sizeof(Expression) );
        if(old->v.type == Identifier)
            printf("convert to float %c \n",old->v.val.id);
        else
            printf("convert to float %d \n", old->v.val.ivalue);
        tmp->v = old->v;
        tmp->leftOperand = old->leftOperand;
        tmp->rightOperand = old->rightOperand;
        tmp->type = old->type;

        Value v;
        v.type = IntToFloatConvertNode;
        v.val.op = IntToFloatConvert;
        old->v = v;
        old->type = Int;
        old->leftOperand = tmp;
        old->rightOperand = NULL;
    }
}

DataType generalize( Expression *left, Expression *right )
{
    if(left->type == Float || right->type == Float){
        printf("generalize : float\n");
        return Float;
    }
    printf("generalize : int\n");
    return Int;
}

DataType lookup_table( SymbolTable *table, char c )
{
    int id = c-'a';
    if( table->table[id] != Int && table->table[id] != Float)
        printf("Error : identifier %c is not declared\n", c);//error
    return table->table[id];
}

void checkexpression( Expression * expr, SymbolTable * table )
{
    char c;
    if(expr->leftOperand == NULL && expr->rightOperand == NULL){
        switch(expr->v.type){
            case Identifier:
                c = expr->v.val.id;
                printf("identifier : %c\n",c);
                expr->type = lookup_table(table, c);
                break;
            case IntConst:
                printf("constant : int\n");
                expr->type = Int;
                break;
            case FloatConst:
                printf("constant : float\n");
                expr->type = Float;
                break;
                //case PlusNode: case MinusNode: case MulNode: case DivNode:
            default:
                break;
        }
    }
    else{
        Expression *left = expr->leftOperand;
        Expression *right = expr->rightOperand;

        checkexpression(left, table);
        checkexpression(right, table);

        DataType type = generalize(left, right);
        convertType(left, type);//left->type = type;//converto
        convertType(right, type);//right->type = type;//converto
        expr->type = type;
    }
}

void checkstmt( Statement *stmt, SymbolTable * table )
{
    if(stmt->type == Assignment){
        AssignmentStatement assign = stmt->stmt.assign;
        printf("assignment : %c \n",assign.id);
        checkexpression(assign.expr, table);
        stmt->stmt.assign.type = lookup_table(table, assign.id);
        if (assign.expr->type == Float && stmt->stmt.assign.type == Int) {
            printf("error : can't convert float to integer\n");
        } else {
            convertType(assign.expr, stmt->stmt.assign.type);
        }
    }
    else if (stmt->type == Print){
        printf("print : %c \n",stmt->stmt.variable);
        lookup_table(table, stmt->stmt.variable);
    }
    else printf("error : statement error\n");//error
}

void check( Program *program, SymbolTable * table )
{
    Statements *stmts = program->statements;
    while(stmts != NULL){
        checkstmt(&stmts->first,table);
        stmts = stmts->rest;
    }
}


/***********************************************************************
  Code generation
 ************************************************************************/
void fprint_op( FILE *target, ValueType op )
{
    switch(op){
        case MinusNode:
            fprintf(target,"-\n");
            break;
        case PlusNode:
            fprintf(target,"+\n");
            break;
        case MulNode:
            fprintf(target,"*\n");
            break;
        case DivNode:
            fprintf(target,"/\n");
            break;
        default:
            fprintf(target,"Error in fprintf_op ValueType = %d\n",op);
            break;
    }
}

void fprint_expr(FILE *target, Expression *expr) {

    if(expr->leftOperand == NULL){
        switch( (expr->v).type ){
            case Identifier:
                fprintf(target,"l%c\n",(expr->v).val.id);
                break;
            case IntConst:
				if((expr->v).val.ivalue < 0) {
					fputc('_', target);
					(expr->v).val.ivalue *= (-1);
				}
                fprintf(target,"%d\n",(expr->v).val.ivalue);
                break;
            case FloatConst:
				if((expr->v).val.fvalue < 0) {
					fputc('_', target);
					(expr->v).val.fvalue *= (-1);
				}
                fprintf(target,"%f\n", (expr->v).val.fvalue);
                break;
            default:
                fprintf(target,"Error In fprint_left_expr. (expr->v).type=%d\n",(expr->v).type);
                break;
        }
    }
    else{
        fprint_expr(target, expr->leftOperand);
        if(expr->rightOperand == NULL){
            fprintf(target,"5 k\n");
        } else{
            //	fprint_right_expr(expr->rightOperand);
            fprint_expr(target, expr->rightOperand);
			if(expr->leftOperand->type == Float || expr->leftOperand->type == Float)
				fprintf(target, "5 k\n");
            fprint_op(target, (expr->v).type);
        }
    }
}

void gencode(Program prog, FILE * target)
{
    Statements *stmts = prog.statements;
    Statement stmt;

    while(stmts != NULL){
        stmt = stmts->first;
        switch(stmt.type){
            case Print:
                fprintf(target,"l%c\n",stmt.stmt.variable);
                fprintf(target,"p\n");
                fprintf(target,"s%c\n",stmt.stmt.variable);
                break;
            case Assignment:
                fprint_expr(target, stmt.stmt.assign.expr);
                /*
                   if(stmt.stmt.assign.type == Int){
                   fprintf(target,"0 k\n");
                   }
                   else if(stmt.stmt.assign.type == Float){
                   fprintf(target,"5 k\n");
                   }*/
                fprintf(target,"s%c\n",stmt.stmt.assign.id);
                fprintf(target,"0 k\n");
                break;
        }
        stmts=stmts->rest;
    }

}

char getVarName(char target[]) {
	static char table[26][257] = {{}};
	static int cur=0;
	//printf("** %d\n", cur);
	for(int i=0; i<cur; i++) {
		if(i==5 || i==8 || i==15)
			continue;
		if(strcmp(table[i], target) == 0)
			return i+'a';
	}
	if(cur==5 || cur==8 || cur==15)
		cur++;
	strcpy(table[cur], target);
	char ret = cur+'a';
	cur++;
	//printf("|| %d\n", cur);
	return ret;
}
