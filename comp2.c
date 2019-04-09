/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       comp2                                                              */
/*                                                                          */
/*                                                                          */
/*       Group Members:          ID numbers                                 */
/*                                                                          */
/*           Colin O' Sullivan   16190963                                   */
/*           Sean Deely          16174836                                   */
/*                                                                          */
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       comp2.c                                                            */
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
/*  Structure for symbol stack.                                             */
/*                                                                          */
/*--------------------------------------------------------------------------*/

struct Node {
  SYMBOL *param;
  struct Node* link;
};
struct Node* top;


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

int scope=1;                       /*  Global scope for variable symbols    */

PRIVATE SET StatementFS_aug;
PRIVATE SET StatementFBS;          /*  Sets for augmented S-Algol recovery  */
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
PRIVATE int ParseDeclarations ( int *varaddress );
PRIVATE void ParseProcDeclaration( void );
PRIVATE void ParseParameterList( SYMBOL *procedure );
PRIVATE void ParseFormalParameter();
PRIVATE void ParseBlock( void );
PRIVATE void ParseStatement( void );
PRIVATE void ParseSimpleStatement( void );
PRIVATE void ParseRestOfStatement( SYMBOL *target );
PRIVATE void ParseProcCallList( SYMBOL *target );
PRIVATE void ParseAssignment( void );
PRIVATE void ParseActualParameter( SYMBOL *target, int count );
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
PRIVATE SYMBOL *MakeSymbolTableEntry( int symtype, int *varaddress );
PRIVATE SYMBOL *LookupSymbol( void );
PRIVATE void push( SYMBOL *param );
PRIVATE void pop( void );


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: comp2 entry point.  Sets up parser globals (opens input and       */
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
    DumpSymbols(scope);
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
/*    Side Effects: Lookahead token advanced. Program symbol name added to  */
/*                  symbol table. Assembly code added to CodeFile           */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseProgram( void )
{
  int count=0;
  int varaddress=0;
  Accept( PROGRAM );
  MakeSymbolTableEntry(STYPE_PROGRAM, NULL);
  Accept( IDENTIFIER );
  Accept( SEMICOLON );
  Synchronise(&DeclarationFS_aug,&DeclarationFBS);
  if( CurrentToken.code == VAR ) count = ParseDeclarations(&varaddress);
  Synchronise(&ProcDeclarationFS_aug,&DeclarationFBS);
  while( CurrentToken.code == PROCEDURE ) {
    ParseProcDeclaration();
    Synchronise(&ProcDeclarationFS_aug,&DeclarationFBS);
  }
  if ( count )
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
/*                                                                          */
/*    Inputs:       Starting address value for variables                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced. Adds variable names to symbol */
/*                  table.                                                  */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE int ParseDeclarations( int *varaddress )
{
  int count=1;
  Accept( VAR );
  if ( scope == 1)
    MakeSymbolTableEntry( STYPE_VARIABLE, varaddress );
  else {
    printf("Current address value: %d\n",*(varaddress));
    MakeSymbolTableEntry( STYPE_LOCALVAR, varaddress );
  }
  Accept( IDENTIFIER );
  while ( CurrentToken.code == COMMA ) {
    Accept( COMMA );
    MakeSymbolTableEntry( STYPE_VARIABLE, varaddress );
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
/*    Side Effects: Lookahead token advanced. Adds procedure name to symbol */
/*                  table. Assembly code added to CodeFile                  */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseProcDeclaration()
{
  int backpatch_addr;
  int count = 0;
  int varaddress = 3;
  SYMBOL *procedure;
  
  Accept( PROCEDURE );
  procedure = MakeSymbolTableEntry( STYPE_PROCEDURE, NULL );
  Accept( IDENTIFIER );
  backpatch_addr = CurrentCodeAddress();
  Emit( I_BR, 0 );
  procedure->address = CurrentCodeAddress();
  scope++;
  if( CurrentToken.code == LEFTPARENTHESIS ) ParseParameterList( procedure );
  else procedure->pcount = 0;
  Accept( SEMICOLON );
  Synchronise(&DeclarationFS_aug,&DeclarationFBS);
  if( CurrentToken.code == VAR ) count=ParseDeclarations( &varaddress );
  Synchronise(&ProcDeclarationFS_aug, &DeclarationFBS);
  while( CurrentToken.code == PROCEDURE ) {
    ParseProcDeclaration();
    Synchronise(&ProcDeclarationFS_aug, &DeclarationFBS);
  }
  if ( count )
    Emit( I_INC, count );
  ParseBlock();
  Accept( SEMICOLON );
  if ( count )
    Emit( I_DEC, count );
  _Emit( I_RET );
  BackPatch( backpatch_addr, CurrentCodeAddress() );
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
/*    Inputs:       Pointer to procedure symbol                             */
/*                                                                          */
/*    Outputs:      Procedures pcount values are assigned                   */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced. Symbols removed from symbol   */
/*                  stack. Procedure parameters assigned address values     */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseParameterList( SYMBOL *procedure ) {
  int pcount=1;
  int address = -1;
  Accept( LEFTPARENTHESIS );
  procedure->ptypes = 0;
  ParseFormalParameter( procedure , pcount );
  while ( CurrentToken.code == COMMA ) {
    Accept( COMMA );
    pcount++;
    ParseFormalParameter( procedure, pcount );
  }
  Accept( RIGHTPARENTHESIS );
  procedure->pcount = pcount;
  while ( top != NULL ) {
    top->param->address = address;
    address--;
    pop();
  }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseFormalParameter implements:                                        */
/*                                                                          */
/*       <FormalParameter>   :== [ "REF" ] <Varible>                        */
/*                                                                          */
/*                                                                          */
/*    Inputs:       Pointer to procedure symbol                             */
/*                  Integer relating to number of parameters in procedure   */
/*                                                                          */
/*    Outputs:      Assigns value to procedure ptypes                       */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced. Parameters added to symbol    */
/*                  table. Parameters symbols added to symbol stack.        */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseFormalParameter( SYMBOL *procedure, int pcount )
{
  SYMBOL *param;
  if( CurrentToken.code == REF ) {
    Accept( REF );
    param=MakeSymbolTableEntry( STYPE_REFPAR, NULL );
    Accept( IDENTIFIER );
    if (pcount == 1)
      procedure->ptypes = procedure->ptypes +1;
    else
      procedure->ptypes = procedure->ptypes + (1<<(pcount-1));
  }
  else {
    param=MakeSymbolTableEntry( STYPE_VALUEPAR, NULL );
    Accept( IDENTIFIER );
  }
  push( param );
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
/*    Inputs:       Pointer to symbol at start of statement.                */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced. Assembly code added to        */
/*                  CodeFile.                                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseRestOfStatement( SYMBOL *target )
{
  int sig, i;
  int position=CurrentToken.pos;
  
  switch( CurrentToken.code ) {
  case( LEFTPARENTHESIS ):
    ParseProcCallList( target ); 
  case( SEMICOLON ):
    if ( target != NULL && target->type == STYPE_PROCEDURE ){
      sig = scope - target->scope;
      if ( sig == 0)
        _Emit( I_PUSHFP );
      else {
        Emit( I_LOADFP, 0 );
        for ( i=0; i < sig-1 ; i++ )
          Emit( I_LOADSP, 0 );
      }
  
      _Emit( I_BSF );
      Emit( I_CALL, target->address );
      _Emit( I_RSF );
      if ( target->pcount )
        Emit( I_DEC, target->pcount );
    }
    else {
      Error( "Symbol is not a procedure",position );
      KillCodeGeneration();
    }
    break;
  case( ASSIGNMENT ):
  default:                 /* Assignment as default case for error handling*/
    ParseAssignment();
    if( target != NULL) {
      if( target->type == STYPE_VARIABLE )
        Emit( I_STOREA, target->address );
      else if( target->type == STYPE_LOCALVAR ) {
        sig = scope - target->scope;
        if( sig == 0 )
          Emit( I_STOREFP, target->address );
        else {
          Emit( I_LOADFP, 0 );
          for ( i=0; i < sig-1 ; i++ )
            Emit( I_LOADSP, 0 );
          Emit( I_LOADSP, target->address );
        }  
      }
      else if( target->type == STYPE_VALUEPAR )
        Emit( I_STOREFP, target->address );
      else if( target->type == STYPE_REFPAR ) {
        Emit( I_LOADFP, target->address );
        Emit( I_STORESP, 0);
      }
      else {
        Error("Not a Variable", position);
        KillCodeGeneration();
      }
    }
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
/*    Inputs:       Pointer to procedures symbol                            */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseProcCallList( SYMBOL *target )
{
  int count =1;
  Accept( LEFTPARENTHESIS );
  ParseActualParameter( target, count );
  while( CurrentToken.code == COMMA ) {
    Accept( COMMA );
    count++;
    ParseActualParameter( target, count );
  }
  if ( count != target->pcount ) {
    Error( "Wrong number of parameters", CurrentToken.pos );
    KillCodeGeneration();
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
/*    Inputs:       Pointer to procedures symbol. Integer value of current  */
/*                  parameters position in procedures call.                 */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced. Assembly code added to        */
/*                  CodeFile.                                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseActualParameter( SYMBOL *target, int count )
{
  int dS, i;
  SYMBOL *param;
  if( target->ptypes & (1<<(count-1)) ) {
    param = LookupSymbol();
    if ( param->type == STYPE_VARIABLE )
      Emit( I_LOADI, param->address );
    else if( param->type == STYPE_LOCALVAR )  {
      dS = scope - param->scope;
      if( dS == 0 ) {
        _Emit ( I_PUSHFP );
        Emit( I_LOADI, param->address );
        _Emit( I_ADD );
      }
      else {
        Emit( I_LOADFP, 0);
        for( i=0; i < param->scope-1 ; i++ )
          Emit( I_LOADSP, 0 );
        Emit( I_LOADSP, 0 );
        Emit( I_LOADI, param->address );
        _Emit( I_ADD );
      }
    }
    else if( param->type == STYPE_REFPAR ) {
      _Emit( I_PUSHFP );
      Emit( I_LOADI, param->address );
      _Emit( I_ADD );
    }
    else {
      Error("Not a variable",CurrentToken.pos);
      KillCodeGeneration();
    }
    Accept( IDENTIFIER );
  }
  else ParseExpression();
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
/*    Side Effects: Lookahead token advanced. Assembly code added to        */
/*                  CodeFile.                                               */
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
/*    Side Effects: Lookahead token advanced. Assembly code added to        */
/*                  CodeFile.                                               */
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
/*    Side Effects: Lookahead token advanced. Assembly Code added to        */
/*                  CodeFile.                                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseReadStatement()
{
  int sig, i;
  SYMBOL *var;
  Accept( READ );
  Accept( LEFTPARENTHESIS );
  var = LookupSymbol();
  _Emit( I_READ );
  
  if( var != NULL) {
    if( var->type == STYPE_VARIABLE )
      Emit( I_STOREA, var->address );
    else if( var->type == STYPE_LOCALVAR ) {
      sig = scope - var->scope;
      if( sig == 0 )
        Emit( I_STOREFP, var->address );
      else {
        Emit( I_LOADFP, 0 );
        for ( i=0; i < sig-1 ; i++ )
          Emit( I_LOADSP, 0 );
        Emit( I_LOADSP, var->address );
      }  
    }
    else if( var->type == STYPE_VALUEPAR )
      Emit( I_STOREFP, var->address );
    else if( var->type == STYPE_REFPAR ) {
      Emit( I_LOADFP, var->address );
      Emit( I_STORESP, 0);
    }
    else {
      Error("Not a Variable", CurrentToken.pos);
      KillCodeGeneration();
    }
  }
  else {
    Error("Variable not declared", CurrentToken.pos);
    KillCodeGeneration();
  }
  
  Accept( IDENTIFIER );
  
  while( CurrentToken.code == COMMA ) {
    Accept( COMMA );
    var = LookupSymbol();
    _Emit( I_READ );
    if( var != NULL) {
      if( var->type == STYPE_VARIABLE )
        Emit( I_STOREA, var->address );
      else if( var->type == STYPE_LOCALVAR || var->type == STYPE_VALUEPAR ) {
        sig = scope - var->scope;
        if( sig == 0 )
          Emit( I_STOREFP, var->address );
        else {
          Emit( I_LOADFP, 0 );
          for ( i=0; i < sig-1 ; i++ )
            Emit( I_LOADSP, 0 );
          Emit( I_STORESP, var->address );
        }  
      }
      else if( var->type == STYPE_REFPAR ) {
        Emit( I_LOADFP, var->address );
        Emit( I_STORESP, 0);
      }
      else {
        Error("Not a Variable", CurrentToken.pos);
        KillCodeGeneration();
      }
    }

    else {
      Error("Variable not declared", CurrentToken.pos);
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
/*    Side Effects: Lookahead token advanced. Assembly code added to        */
/*                  CodeFile.                                               */
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
/*    Side Effects: Lookahead token advanced. Assembly code added to        */
/*                  CodeFile.                                               */
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
/*    Side Effects: Lookahead token advanced. Assembly code added to        */
/*                  CodeFile.                                               */
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
/*    Side Effects: Lookahead token advanced. Assembly code added to        */
/*                  CodeFile on encounter of "-"                            */
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
/*    Side Effects: Lookahead token advanced. Assembly code added to        */
/*                  CodeFile.                                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseSubTerm( void )
{
  int i, dS;
  SYMBOL *var;
  
  switch( CurrentToken.code ) {
  case( IDENTIFIER ):
  default:
    var = LookupSymbol();
    if( var != NULL) {
      if ( var->type == STYPE_VARIABLE )
        Emit( I_LOADA, var->address );
      else if ( var->type == STYPE_LOCALVAR || var->type == STYPE_VALUEPAR ) {
        dS = scope - var->scope;
        if ( dS==0)
          Emit( I_LOADFP, var->address );
        else {
          _Emit( I_LOADFP );
          for ( i=0; i < dS-1; i++ )
            _Emit( I_LOADSP );
          Emit( I_LOADSP, var->address );
        }
      }
      else if ( var->type == STYPE_REFPAR ) {
        Emit( I_LOADFP, var->address );
        Emit( I_LOADSP, 0 );
      }
      else {
        Error("Not a variable",CurrentToken.pos);
        KillCodeGeneration();
      }   
    }
    else {
      Error("Variable undeclared",CurrentToken.pos);
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
/*    Returns:      Code address value after emits completed                */
/*                                                                          */
/*    Side Effects: Lookahead token advanced. Assembly code added to        */
/*                  CodeFile.                                               */
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
/*    Side Effects: Moves lookahead token forward if unexpected symbol      */
/*                  encountered.                                            */
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
/*    Side Effects: Token values added to sets                              */
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
/*    Side Effects: If successful, modifies globals "InputFile",            */
/*                  "ListingFile" and "CodeFile".                           */
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

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  MakeSymbolTableEntry: Adds a symbol to the symbol table and assigns it  */
/*                        values for its type and address.                  */
/*                                                                          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       1) Integer argument symtype relating to type of symbol  */
/*                     to be created                                        */
/*                  2) Pointer to an integer relating to value of symbols   */
/*                     address.                                             */
/*                                                                          */
/*                                                                          */
/*    Outputs:      Varaddress increased if symtype is a global or local    */
/*                  variable.                                               */
/*                                                                          */
/*    Returns:      Pointer to created symbol.                              */
/*                                                                          */
/*    Side Effects: Symbol added to symbol table.                           */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE SYMBOL *MakeSymbolTableEntry( int symtype, int *varaddress )
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
        if( symtype == STYPE_VARIABLE || symtype == STYPE_LOCALVAR ) {
          newsptr->address = *varaddress; (*varaddress)++;
        }
        else newsptr->address = -1;
      }
      printf("SYMBOL -> '%s' created.\nHASH INDEX -> %d.\n"
			 "SCOPE ->  %d.\nTYPE -> %d.\nADDRESS -> %d\n\n",
			 cptr,hashindex,newsptr->scope, newsptr->type, newsptr->address);
      printf("-------------------------------------------\n");
    }
    else {
      Error("Symbol already exists", CurrentToken.pos);
      KillCodeGeneration();
    }
  }
  return newsptr;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  LookupSymbol: If current token is an identifier return a pointer to     */
/*              its symbol otherwise return nothing.                        */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Pointer to symbol relating to current lookahead token   */
/*                                                                          */
/*    Side Effects: None                                                    */
/*                                                                          */
/*--------------------------------------------------------------------------*/

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

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Push: Adds a pointer to passed symbol to top of symbol stack.           */  
/*                                                                          */
/*                                                                          */
/*    Inputs:       Pointer to a symbol.                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Adds to top of stack                                    */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void push( SYMBOL *param ) {
  struct Node* temp;
  temp = (struct Node*)malloc(sizeof(struct Node));
  if (!temp) {
    printf("\nHeap Overflow\n");
    exit(1);
  }

  temp->param = param;
  temp->link = top;
  top = temp;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Pop: Removes the node at top of symbol pointer stack.                   */  
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Removes top of stack                                    */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void pop( void ) {
  struct Node* temp;
  if( top==NULL) {
    printf("\nStack Underflow\n");
    exit(1);
  }
  else {
    temp = top;
    top = top->link;
    temp->link = NULL;
    free( temp );
  }
}
