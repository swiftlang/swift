//===--- Parser.cpp - Swift Language Parser -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Swift parser.
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/Parse/Lexer.h"
#include "Sema.h"
#include "Scope.h"
#include "ParseResult.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"
using namespace swift;
  
  
//===----------------------------------------------------------------------===//
// Setup and Helper Methods
//===----------------------------------------------------------------------===//

Parser::Parser(unsigned BufferID, ASTContext &Context)
  : SourceMgr(Context.SourceMgr),
    L(*new Lexer(BufferID, Context)),
    Context(Context),
    S(*new Sema(Context)) {
}

Parser::~Parser() {
  delete &L;
  delete &S;
}

void Parser::note(SMLoc Loc, const Twine &Message) {
  SourceMgr.PrintMessage(Loc, Message, "note");
}

void Parser::warning(SMLoc Loc, const Twine &Message) {
  SourceMgr.PrintMessage(Loc, Message, "warning");
}

void Parser::error(SMLoc Loc, const Twine &Message) {
  Context.setHadError();
  SourceMgr.PrintMessage(Loc, Message, "error");
}

/// peekToken - Return the next token that will be installed by consumeToken.
const Token &Parser::peekToken() {
  return L.peekNextToken();
}

SMLoc Parser::consumeToken() {
  SMLoc Loc = Tok.getLoc();
  assert(Tok.isNot(tok::eof) && "Lexing past eof!");
  L.lex(Tok);
  return Loc;
}

/// skipUntil - Read tokens until we get to the specified token, then return.
/// Because we cannot guarantee that the token will ever occur, this skips to
/// some likely good stopping point.
///
void Parser::skipUntil(tok T) {
  // tok::unknown is a sentinel that means "don't skip".
  if (T == tok::unknown) return;
  
  while (Tok.isNot(tok::eof) && Tok.isNot(T)) {
    switch (Tok.getKind()) {
    default: consumeToken(); break;
    // TODO: Handle paren/brace/bracket recovery.
    }
  }
}


//===----------------------------------------------------------------------===//
// Primitive Parsing
//===----------------------------------------------------------------------===//

/// parseIdentifier - Consume an identifier if present and return its name in
/// Result.  Otherwise, emit an error and return true.
bool Parser::parseIdentifier(Identifier &Result, const Twine &Message) {
  if (Tok.is(tok::identifier) || Tok.is(tok::oper)) {
    Result = Context.getIdentifier(Tok.getText());
    consumeToken();
    return false;
  }
  
  error(Tok.getLoc(), Message);
  return true;
}

/// parseToken - The parser expects that 'K' is next in the input.  If so, it is
/// consumed and false is returned.
///
/// If the input is malformed, this emits the specified error diagnostic.
/// Next, if SkipToTok is specified, it calls skipUntil(SkipToTok).  Finally,
/// true is returned.
bool Parser::parseToken(tok K, const char *Message, tok SkipToTok) {
  if (Tok.is(K)) {
    consumeToken(K);
    return false;
  }
  
  error(Tok.getLoc(), Message);
  skipUntil(SkipToTok);
  
  // If we skipped ahead to the missing token and found it, consume it as if
  // there were no error.
  if (K == SkipToTok && Tok.is(SkipToTok))
    consumeToken();
  return true;
}

/// value-specifier:
///   ':' type
///   ':' type '=' expr
///   '=' expr
bool Parser::parseValueSpecifier(Type &Ty, NullablePtr<Expr> &Init,
                                 bool SingleInit) {
  // Diagnose when we don't have a type or an expression.
  if (Tok.isNot(tok::colon) && Tok.isNot(tok::equal)) {
    error(Tok.getLoc(), "expected a type or an initializer");
    // TODO: Recover better by still creating var, but making it have
    // 'invalid' type so that uses of the identifier are not errors.
    return true;
  }
  
  // Parse the type if present.
  if (consumeIf(tok::colon) &&
      parseType(Ty, "expected type in var declaration"))
    return true;
  
  // Parse the initializer, if present.
  if (consumeIf(tok::equal)) {
    ParseResult<Expr> Tmp;
    if (SingleInit) {
      Tmp = parseSingleExpr("expected expression in value specifier");
    } else {
      Tmp = parseExpr("expected expression in value specifier");
    }
    if (Tmp)
      return true;
    
    if (!Tmp.isSemaError())
      Init = Tmp.get();
    
    // If there was an expression, but it had a parse error, give the var decl
    // an artificial int type to avoid chained errors.
    // FIXME: We really need to distinguish erroneous expr from missing expr in
    // ActOnVarDecl.
    if (Tmp.isSemaError() && Ty.isNull())
      Ty = Context.TheInt32Type;
  }
  
  return false;
}


//===----------------------------------------------------------------------===//
// Decl Parsing
//===----------------------------------------------------------------------===//

/// ParseTranslationUnit
///   translation-unit:
///     stmt-brace-item*
TranslationUnitDecl *Parser::parseTranslationUnit() {
  // Prime the lexer.
  consumeToken();
  SMLoc FileStartLoc = Tok.getLoc();
  
  TranslationUnitDecl *Result = new (Context) TranslationUnitDecl(Context);
  
  // Parse the body of the file.
  SmallVector<ExprStmtOrDecl, 128> Items;
  parseBraceItemList(Items, true);
  
  // Notify sema about the end of the translation unit.
  S.decl.handleEndOfTranslationUnit(Result, FileStartLoc, Items, Tok.getLoc());
  
  return Result;
}

/// parseAttribute
///   attribute:
///     'infix' '=' numeric_constant
bool Parser::parseAttribute(DeclAttributes &Attributes) {
  if (Tok.is(tok::identifier) && Tok.getText() == "infix") {
    if (Attributes.InfixPrecedence != -1)
      error(Tok.getLoc(), "infix precedence repeatedly specified");
    consumeToken(tok::identifier);

    // The default infix precedence is 100.
    Attributes.InfixPrecedence = 100;
    
    if (consumeIf(tok::equal)) {
      SMLoc PrecLoc = Tok.getLoc();
      StringRef Text = Tok.getText();
      if (!parseToken(tok::numeric_constant,
                      "expected precedence number in 'infix' attribute")) {
        long long Value;
        if (Text.getAsInteger(10, Value) || Value > 255 || Value < 0)
          error(PrecLoc, "invalid precedence: value must be between 0 and 255");
        else
          Attributes.InfixPrecedence = Value;
      }
    }
    
    return false;
  }
  
  error(Tok.getLoc(), "unknown declaration attribute");
  skipUntil(tok::r_square);
  return true;
}

/// parseAttributeList
///   attribute-list:
///     '[' ']'
///     '[' attribute (',' attribute)* ']'
void Parser::parseAttributeList(DeclAttributes &Attributes) {
  Attributes.LSquareLoc = consumeToken(tok::l_square);
  
  // If this is an empty attribute list, consume it and return.
  if (Tok.is(tok::r_square)) {
    Attributes.RSquareLoc = consumeToken(tok::r_square);
    return;
  }
  
  bool HadError = parseAttribute(Attributes);
  while (Tok.is(tok::comma)) {
    consumeToken(tok::comma);
    HadError |= parseAttribute(Attributes);
  }

  Attributes.RSquareLoc = Tok.getLoc();
  if (consumeIf(tok::r_square))
    return;
  
  // Otherwise, there was an error parsing the attribute list.  If we already
  // reported an error, skip to a ], otherwise report the error.
  if (!HadError)
    parseToken(tok::r_square, "expected ']' or ',' in attribute list",
               tok::r_square);
  else {
    skipUntil(tok::r_square);
    consumeIf(tok::r_square);
  }
}

/// parseDeclImport - Parse an 'import' declaration, returning null (and doing
/// no token skipping) on error.
///
///   decl-import:
///      'import' attribute-list? identifier ('.' identifier)*
///
Decl *Parser::parseDeclImport() {
  SMLoc ImportLoc = consumeToken(tok::kw_import);
  
  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    parseAttributeList(Attributes);
  
  SmallVector<std::pair<Identifier, SMLoc>, 8> ImportPath(1);
  ImportPath.back().second = Tok.getLoc();
  if (parseIdentifier(ImportPath.back().first,
                      "expected module name in import declaration"))
    return 0;
  
  while (consumeIf(tok::period)) {
    ImportPath.push_back(std::make_pair(Identifier(), Tok.getLoc()));
    if (parseIdentifier(ImportPath.back().first,
                        "expected name in import declaration"))
      return 0;
  }
  
  return S.decl.ActOnImportDecl(ImportLoc, ImportPath, Attributes);
}


/// parseVarName
///   var-name:
///     identifier
///     '(' ')'
///     '(' name (',' name)* ')'
bool Parser::parseVarName(DeclVarName &Name) {
  // Single name case.
  if (Tok.is(tok::identifier) || Tok.is(tok::oper)) {
    Name.LPLoc = Name.RPLoc = Tok.getLoc();
    parseIdentifier(Name.Name, "");
    return false;
  }
  
  if (Tok.isNot(tok::l_paren) && Tok.isNot(tok::l_paren_space)) {
    error(Tok.getLoc(), "expected identifier or '(' in var name");
    return true;
  }
  Name.LPLoc = consumeToken();
  
  SmallVector<DeclVarName*, 8> ChildNames;
  
  if (Tok.isNot(tok::r_paren)) {
    do {
      DeclVarName *Elt = new (Context) DeclVarName();
      if (parseVarName(*Elt)) return true;
      ChildNames.push_back(Elt);
    } while (consumeIf(tok::comma));
  }

  Name.RPLoc = Tok.getLoc();
  if (parseToken(tok::r_paren, "expected ')' at end of var name"))
    note(Name.LPLoc, "to match this '('");

  Name.Elements = Context.AllocateCopy(ChildNames);
  return false;
}


/// parseDeclTypeAlias
///   decl-typealias:
///     'typealias' identifier ':' type
TypeAliasDecl *Parser::parseDeclTypeAlias() {
  SMLoc TypeAliasLoc = consumeToken(tok::kw_typealias);
  
  Identifier Id;
  Type Ty;
  if (parseIdentifier(Id, "expected identifier in var declaration") ||
      parseToken(tok::colon, "expected ':' in typealias declaration") ||
      parseType(Ty, "expected type in var declaration"))
    return 0;

  return S.decl.ActOnTypeAlias(TypeAliasLoc, Id, Ty);
}


/// AddElementNamesForVarDecl - This recursive function walks a name specifier
/// adding ElementRefDecls for the named subcomponents and checking that types
/// match up correctly.
static void AddElementNamesForVarDecl(const DeclVarName *Name,
                                    SmallVectorImpl<unsigned> &AccessPath,
                                      VarDecl *VD, SemaDecl &SD,
                              SmallVectorImpl<Parser::ExprStmtOrDecl> &Decls){
  if (Name->isSimple()) {
    // If this is a leaf name, ask sema to create a ElementRefDecl for us with 
    // the specified access path.
    ElementRefDecl *ERD =
      SD.ActOnElementName(Name->Name, Name->LPLoc, VD, AccessPath);
    Decls.push_back(ERD);
    SD.AddToScope(ERD);
    return;
  }
  
  AccessPath.push_back(0);
  for (unsigned i = 0, e = Name->Elements.size(); i != e; ++i) {
    AccessPath.back() = i;
    AddElementNamesForVarDecl(Name->Elements[i], AccessPath, VD, SD, Decls);
  }
  AccessPath.pop_back();
}

/// parseDeclVar - Parse a 'var' declaration, returning null (and doing no
/// token skipping) on error.
///
///   decl-var:
///      'var' attribute-list? var-name value-specifier
bool Parser::parseDeclVar(SmallVectorImpl<ExprStmtOrDecl> &Decls) {
  SMLoc VarLoc = consumeToken(tok::kw_var);
  
  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    parseAttributeList(Attributes);

  DeclVarName VarName;
  if (parseVarName(VarName)) return true;
  
  Type Ty;
  NullablePtr<Expr> Init;
  if (parseValueSpecifier(Ty, Init, /*single*/ false))
    return true;

  VarDecl *VD = S.decl.ActOnVarDecl(VarLoc, VarName, Ty, Init.getPtrOrNull(),
                                    Attributes);
  if (VD == 0) return true;
  
  Decls.push_back(VD);
  
  // Enter the declaration into the current scope.  Since var's are not allowed
  // to be recursive, they are entered after its initializer is parsed.  This
  // does mean that stuff like this is different than C:
  //    var x = 1; { var x = x+1; assert(x == 2); }
  if (VarName.isSimple())
    S.decl.AddToScope(VD);
  else {
    // If there is a more interesting name presented here, then we need to walk
    // through it and synthesize the decls that reference the var elements as
    // appropriate.
    SmallVector<unsigned, 8> AccessPath;
    AddElementNamesForVarDecl(VD->NestedName, AccessPath, VD, S.decl, Decls);
  }
  return false;
}


/// parseDeclFunc - Parse a 'func' declaration, returning null on error.  The
/// caller handles this case and does recovery as appropriate.
///
///   decl-func:
///     'func' attribute-list? identifier arg-list-type stmt-brace?
///     'func' attribute-list? type-identifier '::' identifier 
///            arg-list-type stmt-brace?
FuncDecl *Parser::parseDeclFunc() {
  SMLoc FuncLoc = consumeToken(tok::kw_func);

  DeclAttributes Attributes;
  // FIXME: Implicitly add immutable attribute.
  if (Tok.is(tok::l_square))
    parseAttributeList(Attributes);

  Type ReceiverTy;
  Identifier Name;
  SMLoc TypeNameLoc = Tok.getLoc();
  if (parseIdentifier(Name, "expected identifier in func declaration"))
    return 0;

  // If this is method syntax, the first name is the receiver type.  Parse the
  // actual function name.
  if (consumeIf(tok::coloncolon)) {
    // Look up the type name.
    ReceiverTy = S.decl.LookupTypeName(Name, 
                                       TypeNameLoc)->getAliasType(Context);
    if (parseIdentifier(Name, "expected identifier in 'func' declaration"))
      return 0;
  }
  
  // We force first type of a func declaration to be a tuple for consistency.
  if (Tok.isNot(tok::l_paren) && Tok.isNot(tok::l_paren_space)) {
    error(Tok.getLoc(), "expected '(' in argument list of func declaration");
    return 0;
  }
    
  Type FuncTy;
  if (parseType(FuncTy))
    return 0;
  
  // If the parsed type is not spelled as a function type (i.e., has no '->' in
  // it), then it is implicitly a function that returns ().
  if (!isa<FunctionType>(FuncTy.getPointer()))
    FuncTy = FunctionType::get(FuncTy, TupleType::getEmpty(Context), Context);
  
  // If a receiver type was specified, install the first type as the receiver,
  // as a tuple with element named 'this'.  This turns "int->int" on FooTy into
  // "(this : FooTy)->(int->int)".
  if (!ReceiverTy.isNull()) {
    TupleTypeElt ReceiverElt(ReceiverTy, Context.getIdentifier("this"));
    FuncTy = FunctionType::get(TupleType::get(ReceiverElt, Context),
                               FuncTy, Context);
  }
  
  // Enter the arguments for the function into a new function-body scope.  We
  // need this even if there is no function body to detect argument name
  // duplication.
  FuncExpr *FE = 0;
  {
    Scope FnBodyScope(S.decl);
    
    FE = S.expr.ActOnFuncExprStart(FuncLoc, FuncTy);
    
    // Then parse the expression.
    NullablePtr<Stmt> Body;
    
    // Check to see if we have a "{" which is a brace expr.
    if (Tok.is(tok::l_brace)) {
      ParseResult<BraceStmt> Body = parseStmtBrace();
      if (Body.isSuccess())
        FE->Body = Body.get();
      else  // FIXME: Should do some sort of error recovery here.
        FE = 0;
      
    } else {
      // Note, we just discard FE here.  It is bump pointer allocated, so this
      // is fine (if suboptimal).
      FE = 0;
    }
  }
  
  // Create the decl for the func and add it to the parent scope.
  FuncDecl *FD = new (Context) FuncDecl(FuncLoc, Name, FuncTy, FE,Attributes);
  S.decl.AddToScope(FD);
  return FD;
}

/// parseDeclOneOf - Parse a 'oneof' declaration, returning null (and doing no
/// token skipping) on error.
///
///   decl-oneof:
///      'oneof' attribute-list? identifier oneof-body
///   oneof-body:
///      '{' oneof-element-list '}'
///   oneof-element-list:
///      oneof-element ','?
///      oneof-element ',' oneof-element-list
///   oneof-element:
///      identifier
///      identifier ':' type
///      
Decl *Parser::parseDeclOneOf() {
  SMLoc OneOfLoc = consumeToken(tok::kw_oneof);

  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    parseAttributeList(Attributes);
  
  SMLoc NameLoc = Tok.getLoc();
  Identifier OneOfName;
  Type OneOfType;
  if (parseIdentifier(OneOfName, "expected identifier in oneof declaration"))
    return 0;
  
  TypeAliasDecl *TAD = S.decl.ActOnTypeAlias(NameLoc, OneOfName, Type());
  if (parseTypeOneOfBody(OneOfLoc, Attributes, OneOfType, TAD))
    return 0;

  return TAD;
}


/// parseDeclStruct - Parse a 'struct' declaration, returning null (and doing no
/// token skipping) on error.  A 'struct' is just syntactic sugar for a oneof
/// with a single element.
///
///   decl-struct:
///      'struct' attribute-list? identifier { type-tuple-body? }
///
bool Parser::parseDeclStruct(SmallVectorImpl<ExprStmtOrDecl> &Decls) {
  SMLoc StructLoc = consumeToken(tok::kw_struct);
  
  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    parseAttributeList(Attributes);
  
  Identifier StructName;
  if (parseIdentifier(StructName, "expected identifier in struct declaration"))
    return true;

  SMLoc LBLoc = Tok.getLoc();
  if (parseToken(tok::l_brace, "expected '{' in struct"))
    return true;
  
  Type Ty;
  if (parseTypeTupleBody(LBLoc, Ty)) return true;

  if (parseToken(tok::r_brace, "expected '{' in struct")) {
    note(LBLoc, "to match this opening '{'");
    return true;
  }

  // The type is required to be syntactically a tuple type.
  if (!isa<TupleType>(Ty.getPointer())) {
    error(StructLoc, "element type of struct is not a tuple");
    // FIXME: Should set this as an erroroneous decl.
    return true;
  }
          
  S.decl.ActOnStructDecl(StructLoc, Attributes, StructName, Ty, Decls, *this);
  return false;
}



//===----------------------------------------------------------------------===//
// Expression Parsing
//===----------------------------------------------------------------------===//

bool Parser::isStartOfExpr(const Token &Tok, const Token &Next) {
  if (Tok.is(tok::numeric_constant) || Tok.is(tok::colon) ||
      Tok.is(tok::l_paren_space) || Tok.is(tok::dollarident) ||
      Tok.is(tok::identifier) || Tok.is(tok::oper))
    return true;
  
  // "func(" and "func{" are func expressions.  "func x" is a func declaration.
  if (Tok.is(tok::kw_func) &&
      (Next.is(tok::l_paren) || Next.is(tok::l_paren_space) ||
       Next.is(tok::l_brace)))
    return true;
  return false;
}

/// parseSingleExpr
///
/// Parse an expression in a context that requires a single expression.
ParseResult<Expr> Parser::parseSingleExpr(const char *Message) {
  ParseResult<Expr> Result = parseExpr(Message);
  if (Result) return true;

  // Kill all the following expressions.  This is not necessarily
  // good for certain kinds of recovery.
  if (isStartOfExpr(Tok, peekToken())) {
    error(Tok.getLoc(), "expected a singular expression");
    do {
      ParseResult<Expr> Extra = parseExpr(Message);
      if (Extra) break;
    } while (isStartOfExpr(Tok, peekToken()));
  }

  return Result;
}

/// parseExpr
///   expr:
///     expr-unary
///     expr-unary operator expr
///
/// The sequencing here is not structural, i.e. binary operators are
/// not inherently right-associative.
ParseResult<Expr> Parser::parseExpr(const char *Message) {
  SmallVector<Expr*, 8> SequencedExprs;

  bool HasSemaError = false;

  while (true) {
    // Parse a primary expression.
    ParseResult<Expr> Primary = parseExprUnary(Message);
    if (Primary.isParseError())
      return true;

    if (Primary.isSemaError()) {
      HasSemaError = true;
    } else {
      SequencedExprs.push_back(Primary.get());
    }

    // If the next token is not an operator, we're done.
    if (!Tok.is(tok::oper))
      break;

    // Parse the operator.  If this ever gains the ability to fail, we
    // probably need to do something to keep the SequenceExpr in a
    // valid state.
    Expr *Operator = parseExprOperator();
    SequencedExprs.push_back(Operator);

    // The message is only valid for the first subexpr.
    Message = "expected expression after operator";
  }

  // If we had semantic errors, just fail here.
  if (HasSemaError)
    return ParseResult<Expr>::getSemaError();
  assert(!SequencedExprs.empty());

  // If we saw no operators, don't build a sequence.
  if (SequencedExprs.size() == 1)
    return SequencedExprs[0];

  Expr **NewElements =
    Context.AllocateCopy<Expr*>(SequencedExprs.begin(), SequencedExprs.end());
  
  return new (Context) SequenceExpr(NewElements, SequencedExprs.size());
}

/// parseExprUnary
///
///   expr-unary:
///     expr-primary
///     operator expr-unary
ParseResult<Expr> Parser::parseExprUnary(const char *Message) {
  // TODO: implement
  return parseExprPrimary(Message);
}

/// parseExprPrimary
///
///   expr-primary:
///     expr-literal
///     expr-identifier
///     ':' identifier
///     expr-paren
///     expr-func
///     expr-dot
///     expr-subscript
///
///   expr-literal:
///     numeric_constant
///
///   expr-dot:
///     expr-primary '.' identifier
///     expr-primary '.' dollarident
///
///   expr-subscript:
///     expr-primary '[' expr ']'
ParseResult<Expr> Parser::parseExprPrimary(const char *Message) {
  ParseResult<Expr> Result;
  switch (Tok.getKind()) {
  case tok::numeric_constant:
    Result = S.expr.ActOnNumericConstant(Tok.getText(), Tok.getLoc());
    consumeToken(tok::numeric_constant);
    break;

  case tok::dollarident: // $1
    Result = parseExprDollarIdentifier();
    break;
  case tok::identifier:  // foo   and  foo::bar
    Result = parseExprIdentifier();
    break;

  case tok::colon: {     // :foo
    SMLoc ColonLoc = consumeToken(tok::colon);
    Identifier Name;
    SMLoc NameLoc = Tok.getLoc();
    if (parseIdentifier(Name, "expected identifier after ':' expression"))
      return true;
    
    // Handle :foo by just making an AST node.
    Result = new (Context) UnresolvedMemberExpr(ColonLoc, NameLoc, Name);
    break;
  }

  // A spaced left parenthesis can generally start a tuple expression.
  // What it can't do is start a call.
  case tok::l_paren:
  case tok::l_paren_space:
    Result = parseExprParen();
    break;

  case tok::kw_func:
    Result = parseExprFunc();
    break;
      
  default:
    error(Tok.getLoc(), Message ? Message : "expected expression");
    return true;
  }
  
  // If we had a parse error, don't attempt to parse suffixes.  Do keep going if
  // we had semantic errors though.
  if (Result.isParseError())
    return true;
    
  // Handle suffix expressions.
  while (1) {
    // Check for a .foo suffix.
    SMLoc TokLoc = Tok.getLoc();
    
    if (consumeIf(tok::period)) {
      if (Tok.isNot(tok::identifier) && Tok.isNot(tok::dollarident)) {
        error(Tok.getLoc(), "expected field name");
        return true;
      }
        
      if (!Result.isSemaError()) {
        Identifier Name = Context.getIdentifier(Tok.getText());
        Result = new (Context) UnresolvedDotExpr(Result.get(), TokLoc, Name,
                                                 Tok.getLoc());
      }
      if (Tok.is(tok::identifier))
        consumeToken(tok::identifier);
      else
        consumeToken(tok::dollarident);
      continue;
    }
    
    // Check for a () suffix, which indicates a call.
    // Note that this cannot be a l_paren_space.
    if (Tok.is(tok::l_paren)) {
      ParseResult<Expr> Arg = parseExprParen();
      if (Arg.isParseError())
        return true;
      if (Arg.isSemaError())
        Result = ParseResult<Expr>::getSemaError();
      else if (!Result.isSemaError())
        Result = new (Context) CallExpr(Result.get(), Arg.get(), Type());
      continue;
    }
    
    // Check for a [expr] suffix.
    if (consumeIf(tok::l_square)) {
      ParseResult<Expr> Idx;
      if ((Idx = parseSingleExpr("expected expression parsing array index")))
        return true;
      
      SMLoc RLoc = Tok.getLoc();
      if (parseToken(tok::r_square, "expected ']'")) {
        note(TokLoc, "to match this '['");
        return true;        
      }
      
      if (!Result.isSemaError() && !Idx.isSemaError()) {
        // FIXME: Implement.  This should modify Result like the cases
        // above.
        Result = Result;
      }
    }
        
    break;
  }
  
  return Result;
}

///   expr-identifier:
///     dollarident
ParseResult<Expr> Parser::parseExprDollarIdentifier() {
  StringRef Name = Tok.getText();
  SMLoc Loc = consumeToken(tok::dollarident);
  assert(Name[0] == '$' && "Not a dollarident");
  bool AllNumeric = true;
  for (unsigned i = 1, e = Name.size(); i != e; ++i)
    AllNumeric &= isdigit(Name[i]);
  
  if (Name.size() == 1 || !AllNumeric) {
    error(Loc, "invalid identifier, expected expression");
    return ParseResult<Expr>::getSemaError();
  }
  
  unsigned ArgNo = 0;
  if (Name.substr(1).getAsInteger(10, ArgNo)) {
    error(Loc, "invalid name in $ expression");
    return ParseResult<Expr>::getSemaError();
  }
  
  return new (Context) AnonClosureArgExpr(ArgNo, Loc);
}


/// parseExprOperator - Parse an operator reference expression.  These
/// are not "proper" expressions; they can only appear interlaced in
/// SequenceExprs.
Expr *Parser::parseExprOperator() {
  assert(Tok.is(tok::oper));
  SMLoc Loc = Tok.getLoc();
  Identifier Name;
  parseIdentifier(Name, "");

  ParseResult<Expr> Result = S.expr.ActOnIdentifierExpr(Name, Loc);
  assert(Result.isSuccess() && "operator reference failed?");
  return Result.get();
}

/// parseExprIdentifier - Parse an identifier expression:
///
///   expr-identifier:
///     identifier
///     identifier '::' identifier
ParseResult<Expr> Parser::parseExprIdentifier() {
  assert(Tok.is(tok::identifier));
  SMLoc Loc = Tok.getLoc();
  Identifier Name;
  parseIdentifier(Name, "");

  if (Tok.isNot(tok::coloncolon))
    return S.expr.ActOnIdentifierExpr(Name, Loc);
  
  SMLoc ColonColonLoc = consumeToken(tok::coloncolon);

  SMLoc Loc2 = Tok.getLoc();
  Identifier Name2;
  if (parseIdentifier(Name2, "expected identifier after '" + Name.str() +
                      "::' expression"))
    return true;

  return S.expr.ActOnScopedIdentifierExpr(Name, Loc, ColonColonLoc, Name2,Loc2);
}


/// parseExprParen - Parse a tuple expression.
///
///   expr-paren: 
///     '(' ')'
///     '(' expr-paren-element (',' expr-paren-element)* ')'
///
///   expr-paren-element:
///     ('.' identifier '=')? expr
///
ParseResult<Expr> Parser::parseExprParen() {
  SMLoc LPLoc = consumeToken();
  
  SmallVector<Expr*, 8> SubExprs;
  SmallVector<Identifier, 8> SubExprNames; 
  bool AnySubExprSemaErrors = false;
  
  if (Tok.isNot(tok::r_paren)) {
    do {
      Identifier FieldName;
      // Check to see if there is a field specifier.
      if (consumeIf(tok::period)) {
        if (parseIdentifier(FieldName,
                          "expected field specifier name in tuple expression")||
            parseToken(tok::equal, "expected '=' in tuple expression"))
          return true;
      }
      
      if (!SubExprNames.empty())
        SubExprNames.push_back(FieldName);
      else if (FieldName.get()) {
        SubExprNames.resize(SubExprs.size());
        SubExprNames.push_back(FieldName);
      }
      
      ParseResult<Expr> SubExpr;
      if ((SubExpr = parseSingleExpr("expected expression in parentheses")))
        return true;
      
      if (SubExpr.isSemaError())
        AnySubExprSemaErrors = true;
      else
        SubExprs.push_back(SubExpr.get());
    
    } while (consumeIf(tok::comma));
  }
  
  SMLoc RPLoc = Tok.getLoc();  
  if (parseToken(tok::r_paren, "expected ')' in parenthesis expression")) {
    note(LPLoc, "to match this opening '('");
    return true;
  }

  if (AnySubExprSemaErrors)
    return ParseResult<Expr>::getSemaError();

  return S.expr.ActOnTupleExpr(LPLoc, SubExprs.data(),
                               SubExprNames.empty()?0 : SubExprNames.data(),
                               SubExprs.size(), RPLoc);
}

/// parseExprFunc - Parse a func expression.
///
///   expr-func: 
///     'func' type? stmt-brace
///
/// The type must start with '(' if present.
///
ParseResult<Expr> Parser::parseExprFunc() {
  SMLoc FuncLoc = consumeToken(tok::kw_func);

  Type Ty;
  if (Tok.is(tok::l_brace)) {
    Ty = TupleType::getEmpty(Context);
  } else if (!Tok.is(tok::l_paren) && !Tok.is(tok::l_paren_space)) {
    error(Tok.getLoc(), "expected '(' in func expression argument list");
    return true;
  } else if (parseType(Ty)) {
    return true;
  }
  
  // If the parsed type is not spelled as a function type (i.e., has no '->' in
  // it), then it is implicitly a function that returns ().
  if (!isa<FunctionType>(Ty.getPointer()))
    Ty = FunctionType::get(Ty, TupleType::getEmpty(Context), Context);

  // The arguments to the func are defined in their own scope.
  Scope FuncBodyScope(S.decl);
  FuncExpr *FE = S.expr.ActOnFuncExprStart(FuncLoc, Ty);
  
  // Then parse the expression.
  ParseResult<BraceStmt> Body;
  if ((Body = parseStmtBrace("expected '{' in func expression")))
    return true;
  if (Body.isSemaError())
    return ParseResult<Expr>::getSemaError();
  
  FE->Body = Body.get();
  return FE;
}

