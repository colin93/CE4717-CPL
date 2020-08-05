/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       parser2                                                            */
/*                                                                          */
/*                                                                          */
/*       Group Members:          ID numbers                                 */
/*                                                                          */
/*           John Doe            12345678                                   */
/*           Jane Murphy         23456789                                   */
/*           Anthony N. Other    12345679                                   */
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       parser2.c                                                          */
/*                                                                          */
/*       An illustration of the use of the character handler and scanner    */
/*       in a parser for the language                                       */
/*                                                                          */
/*       <Program>           :==   "PROGRAM" <Identifier> ";"               */
/*                                  [<Declarations>] { <ProcDeclaration> }  */
/*                                  <Block> "."                             */
/*                                                                          */
/*       <Declarations>      :==   "VAR" <Variable> { "," <Variable> } ";"  */
/*                                                                          */
/*       <ProcDeclaration>   :==   "PROCEDURE" <Identifier> ";"             */
/*                                  [<ParameterList>] [<Declarations>]      */
/*                                  {<ProcDeclarations>} <Block> ";"        */
/*                                                                          */
/*       <ParameterList>     :==   "(" <FormalParameter>                    */
/*                                  { "," <FormalParameter> } ")"           */
/*                                                                          */
/*       <FormalParameter>   :==   [ "REF" ] <Varible>                      */
/*                                                                          */
/*       <Block>             :==   "BEGIN" { <Statement> ";" } "END"        */
/*                                                                          */
/*       <Statement>         :==   <SimpleStatement> | <WhileStatement      */
/*                                 <IfStatement> | <ReadStatement> |        */
/*                                 <WriteStatement>                         */
/*                                                                          */
/*       <SimpleStatement>   :==   <VarOrProcName> <RestOfStatement>"       */
/*                                                                          */
/*       <RestOfStatement>   :==   <ProcCallList> | <Assignment> | e        */
/*                                                                          */
/*       <ProcCallList>      :==   "(" <ActualParameter> { ","              */
/*                                 <ActualParameter> } ")"                  */
/*                                                                          */
/*       <Assignment>        :==   ":=" <Expression>                        */
/*                                                                          */
/*       <ActualParameter>   :==   <Variable> | <Expression>                */
/*                                                                          */
/*       <WhileStatement>    :==   "WHILE" <BooleanExpression> "DO" <Block> */
/*                                                                          */
/*       <IfStatement>       :==   "IF" <BooleanExpression> "THEN"          */
/*                                 <Block> [ "ELSE" <Block> ]               */
/*                                                                          */
/*       <ReadStatement>     :==   "READ" "(" <Variable>                    */
/*                                 { "," <Variable> } ")"                   */
/*                                                                          */
/*       <WriteStatement>    :==   "WRITE"  "(" <Expression>                */
/*                                 { "," <Expression> } ")"                 */
/*                                                                          */
/*       <Expression>        :==   <CompoundTerm>                           */
/*                                 { <AddOp> <CompoundTerm> }               */
/*                                                                          */
/*       <CompoundTerm>      :==   <Term> { <MultOp> <Term>}                */
/*                                                                          */
/*       <Term>              :==   [ "-" ] <SubTerm>                        */
/*                                                                          */
/*       <SubTerm>           :==   <Variable> | <IntConst> |                */
/*                                 "(" <Expression> ")"                     */
/*                                                                          */
/*       <BooleanExpression> :==   <Expression> <RelOp> <Expression>        */
/*                                                                          */
/*       <AddOp>             :==   "+" | "-"                                */
/*                                                                          */
/*       <MultOp>            :==   "*" | "/"                                */
/*                                                                          */
/*       <RelOp>             :==   "=" | "<=" | ">=" | "<" | ">"            */
/*                                                                          */
/*       <Variable>          :==   <Identifier>                             */
/*                                                                          */
/*       <VarOrProcName>     :==   <Identifier>                             */
/*                                                                          */
/*       <Identifier>        :==   <Alpha> { <AlphaNum> }                   */
/*                                                                          */
/*       <IntConst>          :==   <Digit> { <Digit> }                      */
/*                                                                          */
/*       <AlphaNum>          :==   <Alpha> | <Digit>                        */
/*                                                                          */
/*       <Alpha>             :==   "A"..."Z" | "a"..."z"                    */
/*                                                                          */
/*       <Digit>             :==   "0"..."9"                                */
/*                                                                          */
/*       Note - <Identifier> and <IntConst> are provided by the scanner     */
/*       as tokens IDENTIFIER and INTCONST respectively.                    */
/*                                                                          */
/*       Although the listing file generator has to be initialised in       */
/*       this program, full listing files cannot be generated in the        */
/*       presence of errors because of the "crash and burn" error-          */
/*       handling policy adopted. Only the first error is reported, the     */
/*       remainder of the input is simply copied to the output (using       */
/*       the routine "ReadToEndOfFile") without further comment.            */
/*                                                                          */
/*--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "code.h"
#include "scanner.h"
#include "line.h"
#include "sets.h"
#include "symbol.h"
#include "strtab.h"


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Global variables used by this parser.                                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE FILE *InputFile;           /*  CPL source comes from here.          */
PRIVATE FILE *ListFile;            /*  For nicely-formatted syntax errors.  */
PRIVATE FILE *CodeFile;            /*  For output assembly code             */

PRIVATE TOKEN  CurrentToken;       /*  Parser lookahead token.  Updated by  */
                                   /*  routine Accept (below).  Must be     */
                                   /*  initialised before parser starts.    */
int scope=1;
int varaddress=0;

PRIVATE SET StatementFS_aug;
PRIVATE SET StatementFBS;
PRIVATE SET DeclarationFS_aug;
PRIVATE SET ProcDeclarationFS_aug;
PRIVATE SET DeclarationFBS;

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Function prototypes                                                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] );
PRIVATE void ParseProgram( void );
PRIVATE int ParseDeclarations ( void );
PRIVATE void ParseProcDeclaration( void );
PRIVATE void ParseParameterList( void );
PRIVATE void ParseFormalParameter();
PRIVATE void ParseBlock( void );
PRIVATE void ParseStatement( void );
PRIVATE void ParseSimpleStatement( void );
PRIVATE void ParseRestOfStatement( SYMBOL *target );
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
PRIVATE int ParseBooleanExpression( void );
PRIVATE void Accept( int code );
PRIVATE void Synchronise(SET *F, SET *FB);
PRIVATE void SetupSets( void );
PRIVATE void MakeSymbolTableEntry( int symtype );
PRIVATE SYMBOL *LookupSymbol( void );


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: Parser2 entry point.  Sets up parser globals (opens input and */
/*        output files, initialises current lookahead), then calls          */
/*        "ParseProgram" to start the parse.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PUBLIC int main ( int argc, char *argv[] )
{
  if ( OpenFiles( argc, argv ) )  {
    InitCharProcessor( InputFile, ListFile );
    InitCodeGenerator( CodeFile );
    SetupSets();
    CurrentToken = GetToken();
    ParseProgram();
    WriteCodeFile();
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
/*                                                                          */
/*       <Program>         :==     "PROGRAM" <Identifier> ";"               */
/*                                  [<Declarations>] { <ProcDeclaration> }  */
/*                                  <Block> "."                             */
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
  int count;
  Accept( PROGRAM );
  MakeSymbolTableEntry(STYPE_PROGRAM);
  Accept( IDENTIFIER );
  Accept( SEMICOLON );
  Synchronise(&DeclarationFS_aug,&DeclarationFBS);
  if( CurrentToken.code == VAR ) count = ParseDeclarations();
  Synchronise(&ProcDeclarationFS_aug,&DeclarationFBS);
  while( CurrentToken.code == PROCEDURE ) {
    ParseProcDeclaration();
    Synchronise(&ProcDeclarationFS_aug,&DeclarationFBS);
  }
  Emit(I_INC,count);
  ParseBlock();
  Accept( ENDOFPROGRAM );     /* Token "." has name ENDOFPROGRAM          */
  _Emit( I_HALT );
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseDeclarations implements:                                           */
/*                                                                          */
/*       <Declarations>   :== "VAR" <Variable> { "," <Variable> } ";"       */
/*                                                                          */
/*                                                                           */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE int ParseDeclarations( void )
{
  int count=1;
  Accept( VAR );
  MakeSymbolTableEntry( STYPE_VARIABLE );
  Accept( IDENTIFIER );
  while ( CurrentToken.code == COMMA ) {
    Accept( COMMA );
    MakeSymbolTableEntry( STYPE_VARIABLE );
    Accept( IDENTIFIER );
    count++;
  }
  Accept( SEMICOLON );
  return count;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseProcDeclaration implements:                                        */
/*                                                                          */
/*     <ProcDeclaration> :== "PROCEDURE" <Identifier> [<ParameterList>] ";" */
/*                           [<Declarations>] {<ProcDeclarations>}          */
/*                           <Block> ";"                                    */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseProcDeclaration()
{
  Accept( PROCEDURE );
  MakeSymbolTableEntry( STYPE_PROCEDURE );
  Accept( IDENTIFIER );
  scope++;
  if( CurrentToken.code == LEFTPARENTHESIS ) ParseParameterList();
  Accept( SEMICOLON );
  Synchronise(&DeclarationFS_aug,&DeclarationFBS);
  if( CurrentToken.code == VAR ) ParseDeclarations();
  Synchronise(&ProcDeclarationFS_aug, &DeclarationFBS);
  while( CurrentToken.code == PROCEDURE ) {
    ParseProcDeclaration();
    Synchronise(&ProcDeclarationFS_aug, &DeclarationFBS);
    }
  ParseBlock();
  Accept( SEMICOLON );
  RemoveSymbols( scope );
  scope--;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseParameterList implements:                                          */
/*                                                                          */
/*       <ParameterList>   :== "(" <FormalParameter>                        */
/*                             { "," <FormalParameter> } ")"                */
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
PRIVATE void ParseParameterList() {
  Accept( LEFTPARENTHESIS );
  ParseFormalParameter();
  while ( CurrentToken.code == COMMA ) {
    Accept( COMMA );
    ParseFormalParameter();
  }
  Accept( RIGHTPARENTHESIS );
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseFormalParameter implements:                                        */
/*                                                                          */
/*       <FormalParameter>   :== [ "REF" ] <Varible>                        */
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
PRIVATE void ParseFormalParameter()
{
  if( CurrentToken.code == REF ) {
    Accept( REF );
    MakeSymbolTableEntry( STYPE_REFPAR );
  }
  Accept( IDENTIFIER );
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseBlock implements:                                                  */
/*                                                                          */
/*       <Block>   :== "BEGIN" { <Statement> ";"} "END"                     */
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
  Synchronise(&StatementFS_aug, &StatementFBS);
  while( (CurrentToken.code == IDENTIFIER) | (CurrentToken.code == WHILE) |
	 (CurrentToken.code == IF) | (CurrentToken.code == READ) |
	 (CurrentToken.code == WRITE) ) {
    ParseStatement();
    Accept( SEMICOLON );
    Synchronise(&StatementFS_aug, &StatementFBS);
  }
  Accept( END );
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseStatement implements:                                              */
/*                                                                          */
/*       <Statement>   :== <SimpleStatement> | <WhileStatement>             */
/*                         <IfStatement> | <ReadStatement> |                */
/*                         <WriteStatement>                                 */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseStatement( void )
{
  switch( CurrentToken.code ) {
  case( IDENTIFIER ): ParseSimpleStatement(); break;
  case( WHILE ):      ParseWhileStatement();  break;
  case( IF ):         ParseIfStatement();     break;
  case( READ ):       ParseReadStatement();   break;
  default:            ParseWriteStatement();  break;
  }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseSimpleStatement implements:                                        */
/*                                                                          */
/*       <SimpleStatement>   :== <VarOrProcName> <RestOfStatement>"         */
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
PRIVATE void ParseSimpleStatement()
{
  SYMBOL *target;
  
  target = LookupSymbol();
  Accept( IDENTIFIER );
  ParseRestOfStatement( target );
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseRestOfStatement implements:                                        */
/*                                                                          */
/*       <RestOfStatement>   :== <ProcCallList> | <Assignment> | \eps       */
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
PRIVATE void ParseRestOfStatement( SYMBOL *target )
{
  int position=CurrentToken.pos;
  switch( CurrentToken.code ) {
  case( LEFTPARENTHESIS ):
    ParseProcCallList(); 
  case( SEMICOLON ):
    if ( target != NULL && target->type == STYPE_PROCEDURE )
      Emit( I_CALL, target->address );
    else {
      Error( "Symbol is not a procedure",position );
      KillCodeGeneration();
    }
    break;
  case( ASSIGNMENT ):
  default:                 /* Assignment as default case for error handling*/
    ParseAssignment();
    if( target != NULL && target->type == STYPE_VARIABLE)
      Emit( I_STOREA, target->address );
    else {
      Error( "Undeclared variable", position );
      KillCodeGeneration();
    }
    break;
    
  }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseProcCallList implements:                                           */
/*                                                                          */
/*       <ProcCallList>   :== "(" <ActualParameter> { ","                   */
/*                              <ActualParameter> } ")"                     */
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
PRIVATE void ParseProcCallList()
{
  Accept( LEFTPARENTHESIS );
  ParseActualParameter();
  while( CurrentToken.code == COMMA ) {
    Accept( COMMA );
    ParseActualParameter();
  }
  Accept( RIGHTPARENTHESIS );
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseAssignment implements:                                             */
/*                                                                          */
/*       <Assignment>   :==  ":=" <Expression>                              */
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
PRIVATE void ParseAssignment()
{
  Accept( ASSIGNMENT );
  ParseExpression();
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseActualParameter implements:                                        */
/*                                                                          */
/*       <ActualParameter>   :== <Variable> | <Expression>                  */
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
PRIVATE void ParseActualParameter()
{
  ParseExpression();
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseWhileStatement implements:                                         */
/*                                                                          */
/*       <WhileStatement>   :== "WHILE" <BooleanExpression> "DO" <Block>    */
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
PRIVATE void ParseWhileStatement()
{
  int Label1, Label2, L2BackPatchLoc;
  
  Accept( WHILE );
  Label1 = CurrentCodeAddress();
  L2BackPatchLoc=ParseBooleanExpression();
  Accept( DO );
  ParseBlock();
  Emit( I_BR, Label1);
  Label2 = CurrentCodeAddress();
  BackPatch( L2BackPatchLoc, Label2 );
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseIfStatement implements:                                            */
/*                                                                          */
/*       <IfStatement>   :== "IF" <BooleanExpression> "THEN"                */
/*                           <Block> [ "ELSE" <Block> ]                     */
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
PRIVATE void ParseIfStatement()
{
  int L1BackPatchLoc, L2BackPatchLoc;
  Accept( IF );
  L1BackPatchLoc = ParseBooleanExpression();
  Accept( THEN );
  ParseBlock();
  if( CurrentToken.code == ELSE ) {
    L2BackPatchLoc = CurrentCodeAddress();
    Emit( I_BR, 0);
    BackPatch( L1BackPatchLoc, CurrentCodeAddress());
    Accept( ELSE );
    ParseBlock();
    BackPatch( L2BackPatchLoc, CurrentCodeAddress());
  }
  else
    BackPatch( L1BackPatchLoc, CurrentCodeAddress());
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseReadStatement implements:                                          */
/*                                                                          */
/*       <ReadStatement>   :== "READ" "(" <Variable>                        */
/*                              { "," <Variable> } ")"                      */
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
PRIVATE void ParseReadStatement()
{
  SYMBOL *var;
  Accept( READ );
  Accept( LEFTPARENTHESIS );
  var = LookupSymbol();
  _Emit( I_READ );
  if ( var != NULL && var->type == STYPE_VARIABLE )
    Emit( I_STOREA, var->address );
  else {
    Error("Variable undeclared or not a variable",CurrentToken.pos);
    KillCodeGeneration();
  }
    
  Accept( IDENTIFIER );
  while( CurrentToken.code == COMMA ) {
    Accept( COMMA );
    var = LookupSymbol();
    _Emit( I_READ );
    if ( var != NULL && var->type == STYPE_VARIABLE )
      Emit( I_STOREA, var->address );
    else {
      Error("Variable undeclared or not a variable",CurrentToken.pos);
      KillCodeGeneration();
    }
    Accept( IDENTIFIER );
  }
  Accept( RIGHTPARENTHESIS );
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseWriteStatement implements:                                         */
/*                                                                          */
/*       <WriteStatement>   :== "WRITE"  "(" <Expression>                   */
/*                               { "," <Expression> } ")"                   */
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
PRIVATE void ParseWriteStatement()
{
  Accept( WRITE );
  Accept( LEFTPARENTHESIS );
  ParseExpression();
  _Emit( I_WRITE );
  while( CurrentToken.code == COMMA ) {
    Accept( COMMA );
    ParseExpression();
    _Emit( I_WRITE );
  }
  Accept( RIGHTPARENTHESIS );
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseExpression implements:                                             */
/*                                                                          */
/*       <Expression>   :== <CompoundTerm> { <AddOp> <CompoundTerm> }       */
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
  int op;
  
  ParseCompoundTerm();
  while( (op=CurrentToken.code) == ADD || op == SUBTRACT ) {
    if( op == SUBTRACT ) Accept( SUBTRACT );
    else Accept( ADD );
    ParseCompoundTerm();
    if( op == SUBTRACT ) _Emit(I_SUB); else _Emit(I_ADD);
  }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseCompoundTerm implements:                                           */
/*                                                                          */
/*       <CompoundTerm>   :== <Term> { <MultOp> <Term>}                     */
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
PRIVATE void ParseCompoundTerm()
{
  int op;
  
  ParseTerm();
  while( (op=CurrentToken.code) == MULTIPLY || op == DIVIDE ) {
    if( op == MULTIPLY ) Accept( MULTIPLY );
    else Accept( DIVIDE );
    ParseTerm();

    if( op == MULTIPLY ) _Emit( I_MULT); else _Emit(I_DIV);
  }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseTerm implements           :                                        */
/*                                                                          */
/*       <Term>   :== [ "-" ] <SubTerm>                                     */
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
PRIVATE void ParseTerm()
{
  int negFlag=0;
  if( CurrentToken.code == SUBTRACT ) {
    negFlag=1;
    Accept( SUBTRACT );
  }
  ParseSubTerm();

  if ( negFlag ) _Emit( I_NEG );
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseSubTerm implements:                                                */
/*                                                                          */
/*       <SubTerm>   :== <Variable> | <IntConst> | "(" <Expression> ")"     */
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
  SYMBOL *var;
  
  switch( CurrentToken.code ) {
  case( IDENTIFIER ):
  default:
    var = LookupSymbol();
    if( var != NULL && var->type == STYPE_VARIABLE )
      Emit( I_LOADA, var->address );
    else {
      Error("Variable undeclared or not a variable",CurrentToken.pos);
      KillCodeGeneration();
    }
    Accept( IDENTIFIER );
    break;
  case( INTCONST ):
    Emit( I_LOADI, CurrentToken.value );
    Accept( INTCONST );
    break;
  case( LEFTPARENTHESIS ):
    Accept( LEFTPARENTHESIS);
    ParseExpression();
    Accept( RIGHTPARENTHESIS );
    break;
  }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseBooleanExpression implements:                                      */
/*                                                                          */
/*       <BooleanExpression>   :== <Expression> <RelOp> <Expression>        */
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
PRIVATE int ParseBooleanExpression()
{
  int BackPatchAddr, Instruction;
  ParseExpression();
  
  switch( CurrentToken.code ) {
  case( EQUALITY ):
    Instruction = I_BNZ; Accept( EQUALITY );
    break;
  case( LESSEQUAL ):
    Instruction = I_BG; Accept( LESSEQUAL);
    break;
  case( GREATEREQUAL ):
    Instruction = I_BL; Accept( GREATEREQUAL );
    break;
  case( LESS):
    Instruction = I_BGZ; Accept( LESS );
    break;
  default:
    Instruction = I_BLZ; Accept( GREATER );
    break;
  }
  
  ParseExpression();
  _Emit( I_SUB );
  BackPatchAddr = CurrentCodeAddress();
  Emit( Instruction, 0 );
  return BackPatchAddr;
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
/*           this routine reports a syntax error and continues parsing.     */
/*            Note the use of routine "SyntaxError"                         */
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
  static int recovering = 0;

  if( recovering ) {
    while( CurrentToken.code != ExpectedToken &&
	   CurrentToken.code != ENDOFINPUT )
      CurrentToken = GetToken();
    recovering = 0;
  }
  if(  CurrentToken.code != ExpectedToken ) {
    SyntaxError( ExpectedToken, CurrentToken );
    recovering = 1;
    KillCodeGeneration();
  }
  else  CurrentToken = GetToken();
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Synchronise:    Checks                                                  */
/*                                                                          */
/*                                                                          */
/*    Inputs:       Augmented 1st Set & Follow u Beacons                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects:                                                         */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void Synchronise(SET *F, SET *FB)
{
  SET S;
  S=Union(2,F,FB);
  if(!InSet(F,CurrentToken.code)) {
    SyntaxError2(*F,CurrentToken);
    KillCodeGeneration();
    while(!InSet(&S,CurrentToken.code))
      CurrentToken=GetToken();
  }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  SetupSets:      Creates 1st Sets & Follow u Beacons                     */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects:                                                         */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void SetupSets( void )
{
  InitSet( &StatementFS_aug, 6, IDENTIFIER, WHILE, IF, READ, WRITE, END );
  InitSet( &StatementFBS, 4, SEMICOLON, ELSE, ENDOFPROGRAM, ENDOFINPUT );
  InitSet( &DeclarationFS_aug, 3, VAR, PROCEDURE, BEGIN );
  InitSet( &ProcDeclarationFS_aug, 2, PROCEDURE, BEGIN );
  InitSet( &DeclarationFBS, 3, END, ENDOFINPUT, ENDOFPROGRAM );
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

   if ( argc != 4 )  {
        fprintf( stderr, "%s <inputfile> <listfile> <codefile>\n", argv[0] );
        return 0;
    }

    if ( NULL == ( InputFile = fopen( argv[1], "r" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for input\n", argv[1] );
        return 0;
    }

    if ( NULL == ( ListFile = fopen( argv[2], "w" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[2] );
        fclose( InputFile );
        return 0;
    }
    if (NULL == ( CodeFile = fopen( argv[3], "w") ) ) {
      fprintf( stderr, "cannot open \"%s\" for code generation\n", argv[3]);
      fclose( CodeFile );
      return 0;
    }

    return 1;
}


PRIVATE void MakeSymbolTableEntry( int symtype )
{
  int hashindex;
  SYMBOL *oldsptr, *newsptr;
  char *cptr;

  if( CurrentToken.code == IDENTIFIER ) {
    if( NULL == (oldsptr=Probe( CurrentToken.s, &hashindex )) ||
	oldsptr->scope < scope ) {
      if( oldsptr == NULL) cptr=CurrentToken.s; else cptr = oldsptr->s;
      if(NULL == (newsptr=EnterSymbol( cptr, hashindex ))) {
	Error("Fatal internal error exiting",CurrentToken.pos);
	exit(0);
      }
      else {
	if( oldsptr == NULL ) PreserveString();
	newsptr->scope = scope;
	newsptr->type = symtype;
	if( symtype == STYPE_VARIABLE ) {
	  newsptr->address = varaddress; varaddress++;
	}
	else newsptr->address = -1;
      }
    }
    else {
      Error("Symbol already exists", CurrentToken.pos);
      KillCodeGeneration();
    }
  }				      
}

PRIVATE SYMBOL *LookupSymbol( void )
{
  SYMBOL *sptr;
  if( CurrentToken.code == IDENTIFIER ) {
    sptr = Probe( CurrentToken.s, NULL );
    if ( sptr == NULL ) {
      Error("Identifier not declared", CurrentToken.pos);
      KillCodeGeneration();
    }
  }
  else sptr = NULL;
  return sptr;
}
