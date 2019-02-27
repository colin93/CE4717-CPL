/*---------------------------------------------------------------------------------------*/
/*                                                                                       */
/*       parser1                                                                         */
/*                                                                                       */
/*                                                                                       */
/*       Group Members:          ID numbers                                              */
/*                                                                                       */
/*           Colin O' Sullivan   16190963                                                */
/*                                                                                       */
/*                                                                                       */
/*                                                                                       */
/*---------------------------------------------------------------------------------------*/
/*                                                                                       */
/*       parser1.c                                                                       */
/*                                                                                       */
/*       Pure parser for CPL											                 */
/*         							                                                     */
/*                                                                                       */
/*                                                                                       */
/*                                  GRAMMAR                                              */
/*                                                                                       */ 
/*        <Program> :== “PROGRAM” <Identifier> “;” [ <Declarations> ]                    */
/*                       { <ProcDeclaration> } <Block> “.”                               */
/*        <Declarations> :== “VAR” <Variable> { “,” <Variable> } “;”                     */
/*        <ProcDeclaration> :== “PROCEDURE” <Identifier> [ <ParameterList> ] “;”         */
/*                              [ <Declarations> ] { <ProcDeclaration> } <Block> “;”     */
/*        <ParameterList> :== “(” <FormalParameter> { “,” <FormalParameter> } “)”        */
/*        <FormalParameter> :== [ “REF” ] <Variable>                                     */
/*        <Block> :== “BEGIN” { <Statement> “;” } “END”                                  */
/*        <Statement> :== <SimpleStatement> | <WhileStatement> | <IfStatement> |         */
/*		           		<ReadStatement> | <WriteStatement>                               */
/*        <SimpleStatement> :== <VarOrProcName> <RestOfStatement>                        */
/*        <RestOfStatement> :== <ProcCallList> | <Assignment> | ε                        */
/*        <ProcCallList> :== “(” <ActualParameter> { “,” <ActualParameter> } “)”         */
/*        <Assignment> :== “:=” <Expression>                                             */
/*        <ActualParameter> :== <Variable> | <Expression>                                */
/*        <WhileStatement> :== “WHILE” <BooleanExpression> “DO” <Block>                  */
/*        <IfStatement> :== “IF” <BooleanExpression> “THEN” <Block> [ “ELSE” <Block> ]   */
/*        <ReadStatement> :== “READ” “(” <Variable> { “,” <Variable> } “)”               */
/*        <WriteStatement> :== “WRITE” “(” <Expression> { “,” <Expression> } “)”         */
/*        <Expression> :== <CompoundTerm> { <AddOp> <CompoundTerm> }                     */
/*        <CompoundTerm> :== <Term> { <MultOp> <Term> }                                  */
/*        <Term> :== [ “−” ] <SubTerm>                                                   */
/*        <SubTerm> :== <Variable> | <IntConst> | “(” <Expression> “)”                   */
/*        <BooleanExpression> :== <Expression> <RelOp> <Expression>                      */
/*        <AddOp> :== “+” | “−”                                                          */
/*        <MultOp> :== “*” | “/”                                                         */
/*        <RelOp> :== “=” | “<=” | “>=” | “<” | “>”                                      */
/*        <Variable> :== <Identifier>                                                    */
/*        <VarOrProcName> :== <Identifier>                                               */
/*        <Identifier> :== <Alpha> { <AlphaNum> }                                        */
/*        <IntConst> :== <Digit> { <Digit> }                                             */
/*        <AlphaNum> :== <Alpha> | <Digit>                                               */
/*        <Alpha> :== “A” . . . “Z” | “a” . . . “z”                                      */
/*        <Digit> :== "0" . . . "9"                                                      */
/*                                                                                       */
/*                                                                                       */
/*       Note - <Identifier> and <IntConst> are provided by the scanner as tokens        */
/*       IDENTIFIER and INTCONST respectively.                                           */
/*                                                                                       */
/*       Although the listing file generator has to be initialised in this program,      */
/*       full listing files cannot be generated in the presence of erros because         */
/*       of the "crash and burn" error-handling policy adopted. Only the first           */
/*       error is reported, the remainder of the input is simply copied to the           */
/*       output (using the routine "ReadToEndOfFile") without further comment.           */
/*       the routine "ReadToEndOfFile") without further comment.                         */
/*                                                                                       */
/*---------------------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "scanner.h"
#include "line.h"


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Global variables used by this parser.                                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE FILE *InputFile;           /*  CPL source comes from here.          */
PRIVATE FILE *ListFile;            /*  For nicely-formatted syntax errors.  */

PRIVATE TOKEN  CurrentToken;       /*  Parser lookahead token.  Updated by  */
                                   /*  routine Accept (below).  Must be     */
                                   /*  initialised before parser starts.    */


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Function prototypes                                                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] );
PRIVATE void Accept( int code );
PRIVATE void ReadToEndOfFile( void );
PRIVATE void ParseProgram( void );
PRIVATE void ParseDeclarations( void );
PRIVATE void ParseProcDeclaration( void );
PRIVATE void ParseParameterList( void );
PRIVATE void ParseFormalParameter( void );
PRIVATE void ParseBlock( void ) ;
PRIVATE int isaStatement( int Token );
PRIVATE void ParseSimpleStatement( void );
PRIVATE void ParseRestOfStatement( void );
PRIVATE void ParseProcCallList( void );
PRIVATE void ParseAssignment( void );
PRIVATE void ParseActualParameter( void );
PRIVATE void ParseWhileStatement( void );
PRIVATE void ParseIfStatement( void );
PRIVATE void ParseReadStatement( void );
PRIVATE void ParseWriteStatement( void );
PRIVATE void ParseExpression( void );
PRIVATE void ParseCompoundTerm( void );
PRIVATE void ParseTerm( void );
PRIVATE void ParseSubTerm( void );
PRIVATE void ParseBooleanExpression( void );

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: parser1 entry point.  Sets up parser globals (opens input and     */
/*        output files, initialises current lookahead), then calls          */
/*        "ParseProgram" to start the parse.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PUBLIC int main ( int argc, char *argv[] )
{
    if ( OpenFiles( argc, argv ) )  {
        InitCharProcessor( InputFile, ListFile );
        CurrentToken = GetToken();
        ParseProgram();
        fclose( InputFile );
        fclose( ListFile );
        return  EXIT_SUCCESS;
    }
    else 
        return EXIT_FAILURE;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseProgram implements:                                                */
/*                                                                          */
/*       <Program>     :== "PROGRAM" <Identifier> ";" [<Declarations>]      */
/*                         {<ProcDeclaration>} <Block> "."                  */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseProgram( void )
{
  Accept( PROGRAM );
  Accept( IDENTIFIER );
  Accept( SEMICOLON );
  if ( CurrentToken.code == VAR )
    ParseDeclarations();
  while (CurrentToken.code == PROCEDURE)
    ParseProcDeclaration();
  ParseBlock();
  Accept( ENDOFPROGRAM );     /* Token "." has name ENDOFPROGRAM          */
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseDeclartations Implements:                                          */
/*                                                                          */
/*       <Declarations> :== "VAR" <Variable> {"," <Variable> } ";"          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseDeclarations( void )
{
  Accept( VAR );
  Accept( IDENTIFIER );
  while (CurrentToken.code  == COMMA)
  {
    Accept( COMMA );
    Accept( IDENTIFIER );
  }
  Accept( SEMICOLON );
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseProcDeclartaion Implements:                                        */
/*                                                                          */
/*       <ProcDeclaration> :== "PROCEDURE" <Identifier> [<ParameterList>]   */
/*                             ";" [<Declerations>] {<ProcDecleration>}     */
/*                             <Block> ";"                                  */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseProcDeclaration( void )
{
  Accept( PROCEDURE );
  Accept( IDENTIFIER );
  if (CurrentToken.code == LEFTPARENTHESIS )
    ParseParameterList();
  Accept( SEMICOLON );
  if (CurrentToken.code == VAR)
    ParseDeclarations();
  while (CurrentToken.code == PROCEDURE)
    ParseProcDeclaration();
  ParseBlock();
  Accept(SEMICOLON);
}

/*------------------------------------------------------------------------------*/                                                                        
/*                                                                              */
/*                                                                              */
/*  ParseParameterList Implements:                                              */
/*                                                                              */
/*       <ParameterList> :== "(" <FormalParameter> {"," <FormalParameter>} ")"  */
/*                                                                              */
/*                                                                              */
/*    Inputs:       None                                                        */
/*                                                                              */
/*    Outputs:      None                                                        */
/*                                                                              */
/*    Returns:      Nothing                                                     */
/*                                                                              */
/*    Side Effects: Lookahead token advanced.                                   */
/*                                                                              */
/*------------------------------------------------------------------------------*/

PRIVATE void ParseParameterList( void )
{
  Accept( LEFTPARENTHESIS );
  ParseFormalParameter();
  while (CurrentToken.code == COMMA )
  {
   	Accept( COMMA );
    ParseFormalParameter();
  }
  Accept( RIGHTPARENTHESIS );
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseFormalParameter Implements:                                        */
/*                                                                          */
/*       <FormalParameter> :== ["REF"] <Variable>                           */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseFormalParameter( void )
{
	if (CurrentToken.code == REF )
		Accept ( REF );
	Accept( IDENTIFIER );
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseBlock Implements:                                                  */
/*                                                                          */
/*       <Block> :== "BEGIN" { <Statement> ";" } "END"                      */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseBlock( void ) 
{
	Accept( BEGIN );
    /* implementation of Statement grammar as nothing else contains statements */
	while (isaStatement(CurrentToken.code))
    {
		if ( CurrentToken.code == IDENTIFIER )
			ParseSimpleStatement();
		else if ( CurrentToken.code == WHILE )
			ParseWhileStatement();
		else if ( CurrentToken.code == IF )
			ParseIfStatement();
		else if ( CurrentToken.code == READ )
			ParseReadStatement();
		else 
			ParseWriteStatement();
		Accept( SEMICOLON );
	}
	Accept( END );
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseSimpleStatement Implements:                                        */
/*                                                                          */
/*       <SimpleStatement> :== <Identifier> <RestOfStatement>               */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseSimpleStatement( void )
{
	Accept( IDENTIFIER );
	ParseRestOfStatement();
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseRestOfStatement Implements:                                        */
/*                                                                          */
/*       <RestOfStatement> :== <ProcCallList> | <Assignment> | ε            */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseRestOfStatement( void )
{
	if ( CurrentToken.code == LEFTPARENTHESIS )
		ParseProcCallList();
	else if ( CurrentToken.code == ASSIGNMENT )
		ParseAssignment();
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseProcCallList Implements:                                           */
/*                                                                          */
/*       <ProcCallList> :== "(" <ActualParameter> {"," <ActualParameter> }  */
/*                           ")"                                            */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseProcCallList( void )
{
	Accept( LEFTPARENTHESIS );
	ParseActualParameter();
	while ( CurrentToken.code == COMMA )
    {
		Accept( COMMA );
		ParseActualParameter();
	}
	Accept( RIGHTPARENTHESIS );
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseAssignment Implements:                                             */
/*                                                                          */
/*       <Assignment> :== ":=" <Expression>                                 */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseAssignment( void )
{
	Accept( ASSIGNMENT );
	ParseExpression();
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseActualParameter Implements:                                        */
/*                                                                          */
/*       <ActualParameter> :== <Variable> | <Expression>                    */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseActualParameter( void )
{
	if ( CurrentToken.code == IDENTIFIER )
      Accept( IDENTIFIER );
	else
		ParseExpression();
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseWhileStatement Implements:                                         */
/*                                                                          */
/*       <WhileStatement> :== "WHILE" <BooleanExpression> "THEN" <Block>    */
/*                            ["ELSE" <Block>]                              */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseWhileStatement( void )
{
	Accept( WHILE );
	ParseBooleanExpression();
	Accept( DO );
	ParseBlock();
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseIfStatement Implements:                                            */
/*                                                                          */
/*       <IfStatement> :== "IF" <BooleanExpression> "THEN" <Block>          */
/*                         ["ELSE" <Block>]                                 */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseIfStatement( void )
{
	Accept( IF );
	ParseBooleanExpression();
	Accept( THEN );
	ParseBlock();
	if ( CurrentToken.code == ELSE )
    {
		Accept( ELSE );
		ParseBlock();
	}
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseReadStatement Implements:                                          */
/*                                                                          */
/*       <ReadStatement> :== "READ" "(" <Variable> {"," <Variable>} ")"     */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseReadStatement( void )
{
	Accept( READ );
	Accept( LEFTPARENTHESIS );
	Accept( IDENTIFIER );
	while ( CurrentToken.code == COMMA )
    {
		Accept( COMMA );
		Accept( IDENTIFIER );
	}
	Accept( RIGHTPARENTHESIS );
}

/*-----------------------------------------------------------------------------*/                                                                        
/*                                                                             */
/*                                                                             */
/*  ParseWriteStatement Implements:                                            */
/*                                                                             */
/*       <WriteStatement> :== "WRITE" "(" <Expression> {"," <Expression>} ")"  */
/*                                                                             */
/*                                                                             */
/*    Inputs:       None                                                       */
/*                                                                             */
/*    Outputs:      None                                                       */
/*                                                                             */
/*    Returns:      Nothing                                                    */
/*                                                                             */
/*    Side Effects: Lookahead token advanced.                                  */
/*                                                                             */
/*-----------------------------------------------------------------------------*/

PRIVATE void ParseWriteStatement( void )
{
	Accept( WRITE );
	Accept( LEFTPARENTHESIS );
	ParseExpression();
	while ( CurrentToken.code == COMMA )
    {
		Accept( COMMA );
		ParseExpression();
	}
	Accept( RIGHTPARENTHESIS );
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseExpression Implements:                                             */
/*                                                                          */
/*       <Expression> :== <CompoundTerm> { <AddOp> <CompoundTerm> }         */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseExpression( void )
{
	ParseCompoundTerm();
	while ( CurrentToken.code == ADD || CurrentToken.code == SUBTRACT )
	{
      /* AddOp grammar */
		if ( CurrentToken.code == ADD )
			Accept( ADD );
		else
			Accept( SUBTRACT );
		ParseCompoundTerm();
	}
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseCompoundTerm Implements:                                           */
/*                                                                          */
/*       <CompoundTerm> :== <Term> { <MultOp> <Term> }                      */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseCompoundTerm( void )
{
	ParseTerm();
	while ( CurrentToken.code == MULTIPLY || CurrentToken.code == DIVIDE )
    {
      /* MultOp grammar */
		if ( CurrentToken.code == MULTIPLY )
			Accept( MULTIPLY );
		else
			Accept( DIVIDE );
		ParseTerm();
	}
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseTerm Implements                                                    */
/*                                                                          */
/*       <Term> :== ["-"] <SubTerm>                                         */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseTerm( void )
{
	if ( CurrentToken.code == SUBTRACT )
		Accept( SUBTRACT );
	ParseSubTerm();
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseSubTerm Implements:                                                */
/*                                                                          */
/*       <SubTerm> :== <Variable> | <IntConst> | "(" <Expression> ")"       */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseSubTerm( void )
{
	if ( CurrentToken.code == IDENTIFIER )
		Accept( IDENTIFIER );
	else if ( CurrentToken.code == INTCONST )
		Accept( INTCONST );
	else
    {
		Accept( LEFTPARENTHESIS );
		ParseExpression();
		Accept( RIGHTPARENTHESIS );
	}
}

/*--------------------------------------------------------------------------*/                                                                        
/*                                                                          */
/*                                                                          */
/*  ParseBooleanExpression Implements:                                      */
/*                                                                          */
/*       <BooleanExpression> :== <Expression> <RelOp> <Expression>          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseBooleanExpression( void )
{
	ParseExpression();
    /* RelOp grammar */
	if ( CurrentToken.code == EQUALITY )
		Accept( EQUALITY );
	else if ( CurrentToken.code == LESSEQUAL )
		Accept( LESSEQUAL );
	else if ( CurrentToken.code == LESS )
		Accept( LESS );
	else if ( CurrentToken.code == GREATER )
      Accept( GREATER );
	else 
		Accept( GREATEREQUAL );
	ParseExpression();
}




/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  End of parser.  Support routines follow.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Accept:  Takes an expected token name as argument, and if the current   */
/*           lookahead matches this, advances the lookahead and returns.    */
/*                                                                          */
/*           If the expected token fails to match the current lookahead,    */
/*           this routine reports a syntax error and exits ("crash & burn"  */
/*           parsing).  Note the use of routine "SyntaxError"               */
/*           (from "scanner.h") which puts the error message on the         */
/*           standard output and on the listing file, and the helper        */
/*           "ReadToEndOfFile" which just ensures that the listing file is  */
/*           completely generated.                                          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       Integer code of expected token                          */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: If successful, advances the current lookahead token     */
/*                  "CurrentToken".                                         */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void Accept( int ExpectedToken )
{
    if ( CurrentToken.code != ExpectedToken )
    {
        SyntaxError( ExpectedToken, CurrentToken );
        ReadToEndOfFile();
        fclose( InputFile );
        fclose( ListFile );
        exit( EXIT_FAILURE );
    }
    else  CurrentToken = GetToken();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  isaStatement: Checks current input token and returns true if that       */
/*              token is associated with the start of a statement           */
/*                                                                          */
/*                                                                          */
/*    Inputs:      Integer code of a token                       .          */
/*                                                                          */
/*    Outputs:     None                                                     */
/*                                                                          */
/*    Returns:     Boolean success flag (i.e., an "int":  1 or 0)           */
/*                                                                          */
/*    Side Effects: None                                                    */
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE int isaStatement( int Token )
{
	if (Token == IDENTIFIER || Token == WHILE || Token == IF ||
        Token == READ || Token == WRITE)
		return 1;
	else 
		return 0;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  OpenFiles:  Reads strings from the command-line and opens the           */
/*              associated input and listing files.                         */
/*                                                                          */
/*    Note that this routine mmodifies the globals "InputFile" and          */
/*    "ListingFile".  It returns 1 ("true" in C-speak) if the input and     */
/*    listing files are successfully opened, 0 if not, allowing the caller  */
/*    to make a graceful exit if the opening process failed.                */
/*                                                                          */
/*                                                                          */
/*    Inputs:       1) Integer argument count (standard C "argc").          */
/*                  2) Array of pointers to C-strings containing arguments  */
/*                  (standard C "argv").                                    */
/*                                                                          */
/*    Outputs:      No direct outputs, but note side effects.               */
/*                                                                          */
/*    Returns:      Boolean success flag (i.e., an "int":  1 or 0)          */
/*                                                                          */
/*    Side Effects: If successful, modifies globals "InputFile" and         */
/*                  "ListingFile".                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] )
{

    if ( argc != 3 )
    {
        fprintf( stderr, "%s <inputfile> <listfile>\n", argv[0] );
        return 0;
    }

    if ( NULL == ( InputFile = fopen( argv[1], "r" ) ) )
    {
        fprintf( stderr, "cannot open \"%s\" for input\n", argv[1] );
        return 0;
    }

    if ( NULL == ( ListFile = fopen( argv[2], "w" ) ) )
    {
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[2] );
        fclose( InputFile );
        return 0;
    }

    return 1;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ReadToEndOfFile:  Reads all remaining tokens from the input file.       */
/*              associated input and listing files.                         */
/*                                                                          */
/*    This is used to ensure that the listing file refects the entire       */
/*    input, even after a syntax error (because of crash & burn parsing,    */
/*    if a routine like this is not used, the listing file will not be      */
/*    complete.  Note that this routine also reports in the listing file    */
/*    exactly where the parsing stopped.  Note that this routine is         */
/*    superfluous in a parser that performs error-recovery.                 */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Reads all remaining tokens from the input.  There won't */
/*                  be any more available input after this routine returns. */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ReadToEndOfFile( void )
{
    if ( CurrentToken.code != ENDOFINPUT )
    {
        Error( "Parsing ends here in this program\n", CurrentToken.pos );
        while ( CurrentToken.code != ENDOFINPUT )  CurrentToken = GetToken();
    }
}
