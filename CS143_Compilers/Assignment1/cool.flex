/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
#define check_strbuf_len(len) do { \
    if ((strlen(string_buf) + (len) + 1) > MAX_STR_CONST) { \
      good_str = false; \
      cool_yylval.error_msg = "String constant too long"; \
      return wln(ERROR); \
    } \
  } while(0)

#define check_add2str_buf(str) do { \
  if (good_str) { \
    check_strbuf_len(strlen(str)); \
    strcat(string_buf, str); \
  } \
} while(0)

int wln(int ret);  // return with lineno.

int nested_comment = 0;
bool good_str = true;

%}
/*
 * Define names for regular expressions here.
 */
%option yylineno
%s ST_STRING ST_COMMENT ST_COMMENT_DASH

RE_DARROW          =>
RE_INTEGER         [0-9]+
RE_W               [a-zA-Z0-9_]
RE_TYPE_ID         [A-Z]{RE_W}*
RE_OBJ_ID          [a-z]{RE_W}*

/*  keywords : class, else, false, fi, if, in, inherits, isvoid, let, loop, pool, then, while,
case, esac, new, of, not, true 
Except for the constants true and false, keywords are case insensitive.*/
RE_CLASS    (?i:class)
RE_ELSE     (?i:else)
RE_FALSE    f(?i:alse)
RE_FI       (?i:fi)
RE_IF       (?i:if)
RE_IN       (?i:in)
RE_INHERITS (?i:inherits)
RE_ISVOID   (?i:isvoid)
RE_LET      (?i:let)
RE_LOOP     (?i:loop)
RE_POOL     (?i:pool)
RE_THEN     (?i:then)
RE_WHILE    (?i:while)
RE_CASE     (?i:case)
RE_ESAC     (?i:esac)
RE_NEW      (?i:new)
RE_OF       (?i:of)
RE_NOT      (?i:not)
RE_TRUE     t(?i:rue)

%%

 /*
  *  Nested comments
  */
 /* Enter comment */
<INITIAL>"(*" {
  BEGIN(ST_COMMENT);
  nested_comment++;
}

 /* Match nested comment. */
<ST_COMMENT>"(*" {
  nested_comment++;
}

<INITIAL>"*)" {
  cool_yylval.error_msg = "Unmatched *).";
  return wln(ERROR);
}

 /* Exit nested comment */
<ST_COMMENT>"*)" {
  nested_comment--;
  if (nested_comment == 0) {
    BEGIN(0);
  }
}

 /* Match all characters except '*' and '(' */
<ST_COMMENT>[^\*\(]+ ;

 /* Parse '*' and '(' , the "*)" and "(*" will 
  * be pased with enter and exit comment. */
<ST_COMMENT>\* ;
<ST_COMMENT>\( ;

<ST_COMMENT><<EOF>> {
    cool_yylval.error_msg = "EOF in comment";
    BEGIN(0);
    return wln(ERROR);
}

 /* Start dash comment. */
<INITIAL>"--" {
  BEGIN(ST_COMMENT_DASH);
}

<ST_COMMENT_DASH><<EOF>> { yyterminate(); }
<ST_COMMENT_DASH>[^\n] ;
<ST_COMMENT_DASH>\n {
  BEGIN(0);
}

 /*
  *  The multiple-character operators.
  */
<INITIAL>[ \n\f\r\t\v]+ ;

<INITIAL>"("  { return wln(int('(')); }
<INITIAL>")"  { return wln(int(')')); }
<INITIAL>;    { return wln(int(';')); }
<INITIAL>"{"  { return wln(int('{')); }
<INITIAL>"}"  { return wln(int('}')); }
<INITIAL>:    { return wln(int(':')); }
<INITIAL>,    { return wln(int(',')); }
<INITIAL>"."  { return wln(int('.')); }
<INITIAL>@    { return wln(int('@')); }
<INITIAL>~    { return wln(int('~')); }
<INITIAL>"*"  { return wln(int('*')); }
<INITIAL>"/"  { return wln(int('/')); }
<INITIAL>"+"  { return wln(int('+')); }
<INITIAL>"-"  { return wln(int('-')); }
<INITIAL><=   { return wln(LE);       }
<INITIAL><    { return wln(int('<')); }
<INITIAL>=    { return wln(int('=')); }
<INITIAL><"-" { return wln(ASSIGN);   }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
<INITIAL>{RE_CLASS}     { return wln(CLASS); }
<INITIAL>{RE_ELSE}      { return wln(ELSE); }
<INITIAL>{RE_FALSE}     { cool_yylval.boolean = false; return wln(BOOL_CONST); }    
<INITIAL>{RE_FI}        { return wln(FI); }       
<INITIAL>{RE_IF}        { return wln(IF); }       
<INITIAL>{RE_IN}        { return wln(IN); }       
<INITIAL>{RE_INHERITS}  { return wln(INHERITS); }
<INITIAL>{RE_ISVOID}    { return wln(ISVOID); }
<INITIAL>{RE_LET}       { return wln(LET); }
<INITIAL>{RE_LOOP}      { return wln(LOOP); }
<INITIAL>{RE_POOL}      { return wln(POOL); }
<INITIAL>{RE_THEN}      { return wln(THEN); }
<INITIAL>{RE_WHILE}     { return wln(WHILE); }
<INITIAL>{RE_CASE}      { return wln(CASE); }
<INITIAL>{RE_ESAC}      { return wln(ESAC); }
<INITIAL>{RE_NEW}       { return wln(NEW); }
<INITIAL>{RE_OF}        { return wln(OF); }
<INITIAL>{RE_NOT}       { return wln(NOT); }
<INITIAL>{RE_TRUE}      { cool_yylval.boolean = true; return wln(BOOL_CONST); }

<INITIAL>{RE_DARROW}		{ return wln(DARROW); }
<INITIAL>{RE_INTEGER}   {
  cool_yylval.symbol = stringtable.add_string(yytext);
  return wln(INT_CONST);
}
<INITIAL>{RE_TYPE_ID} {
  cool_yylval.symbol = stringtable.add_string(yytext);
  return wln(TYPEID);
}
<INITIAL>{RE_OBJ_ID} {
  cool_yylval.symbol = stringtable.add_string(yytext);
  return wln(OBJECTID);
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
 /*  Enter string. */
<INITIAL>\" {
 /* " just for close quotation in my IDE. */
  BEGIN(ST_STRING);
  string_buf[0] = 0;
  good_str = true;
}

 /*  Exit string. */
<ST_STRING>\" {
 /* " */
  BEGIN(0);
  if (good_str) {
    cool_yylval.symbol = stringtable.add_string(string_buf);
    return wln(STR_CONST);
  }
}

<ST_STRING><<EOF>> {
    cool_yylval.error_msg = "EOF in string constant";
    BEGIN(0);
    return wln(ERROR);
}

 /* In string state : match all character except for '\' , '"' , '\n' */
<ST_STRING>[^\\\"\n]+ {
 /* " */
 /*    if yyleng > strlen(yytext) , representer yytext contains the null character. */
  if (yyleng > static_cast<int>(strlen(yytext))) {
    good_str = false;
    cool_yylval.error_msg = "String contains null character";
    return wln(ERROR);
  }

  check_add2str_buf(yytext);
}

<ST_STRING>\n {
  BEGIN(0);
  cool_yylval.error_msg = "Unterminated string constant";
  return wln(ERROR);
}

 /*  Parse escape character. */
<ST_STRING>\\\n {
  check_add2str_buf("\n");
}

<ST_STRING>\\. {
  switch (yytext[1]) {
    case 'b':
      check_add2str_buf("\b");
      break;
    case 't':
      check_add2str_buf("\t");
      break;
    case 'n':
      check_add2str_buf("\n");
      break;
    case 'f':
      check_add2str_buf("\f");
      break;
    case '\0':
      good_str = false;
      cool_yylval.error_msg = "String contains null character";
      return wln(ERROR);
    default:
      check_add2str_buf(&yytext[1]);
  }
}


 /*
  * invalid character
  */
<INITIAL>. {
  cool_yylval.error_msg = yytext;
  return wln(ERROR);
}

%%

int wln(int ret) {
  curr_lineno = yylineno;
  return ret;
}
