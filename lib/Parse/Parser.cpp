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
#include "swift/Sema/Sema.h"
#include "swift/Sema/Scope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/NullablePtr.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"
using namespace swift;
using llvm::SMLoc;
using llvm::NullablePtr;

//===----------------------------------------------------------------------===//
// Setup and Helper Methods
//===----------------------------------------------------------------------===//

Parser::Parser(unsigned BufferID, ASTContext &Context)
  : SourceMgr(Context.SourceMgr),
    L(*new Lexer(BufferID, Context)),
    S(*new Sema(Context)) {
}

Parser::~Parser() {
  delete &L;
  delete &S;
}

void Parser::note(SMLoc Loc, const llvm::Twine &Message) {
  SourceMgr.PrintMessage(Loc, Message, "note");
}

void Parser::warning(SMLoc Loc, const llvm::Twine &Message) {
  SourceMgr.PrintMessage(Loc, Message, "warning");
}

void Parser::error(SMLoc Loc, const llvm::Twine &Message) {
  S.Context.setHadError();
  SourceMgr.PrintMessage(Loc, Message, "error");
}

void Parser::consumeToken() {
  assert(Tok.isNot(tok::eof) && "Lexing past eof!");
  L.Lex(Tok);
}

/// skipUntil - Read tokens until we get to the specified token, then return.
/// Because we cannot guarantee that the token will ever occur, this skips to
/// some likely good stopping point.
///
void Parser::skipUntil(tok::TokenKind T) {
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
bool Parser::parseIdentifier(Identifier &Result, const llvm::Twine &Message) {
  if (Tok.is(tok::identifier)) {
    Result = S.Context.getIdentifier(Tok.getText());
    consumeToken(tok::identifier);
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
bool Parser::parseToken(tok::TokenKind K, const char *Message,
                        tok::TokenKind SkipToTok) {
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
bool Parser::parseValueSpecifier(Type *&Ty, NullablePtr<Expr> &Init) {
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
    if (parseExpr(Init, "expected expression in var declaration"))
      return true;
    
    // If there was an expression, but it had a parse error, give the var decl
    // an artificial int type to avoid chained errors.
    // FIXME: We really need to distinguish erroneous expr from missing expr in
    // ActOnVarDecl.
    if (Init.isNull() && Ty == 0)
      Ty = S.Context.TheInt32Type;
  }
  
  return false;
}


//===----------------------------------------------------------------------===//
// Decl Parsing
//===----------------------------------------------------------------------===//

/// ParseTranslationUnit
///   translation-unit:
///     decl-top-level*
TranslationUnitDecl *Parser::parseTranslationUnit() {
  // Prime the lexer.
  consumeToken();
  SMLoc FileStartLoc = Tok.getLoc();
  
  TranslationUnitDecl *Result =
    new (S.Context) TranslationUnitDecl(FileStartLoc, S.Context);
  
  llvm::SmallVector<Decl*, 128> Decls;
  {
    // The entire translation unit is in a big scope.
    Scope OuterScope(S.decl);
  
    while (Tok.isNot(tok::eof))
      parseDeclTopLevel(Decls);
  }
  
  // Notify sema about the end of the translation unit.
  S.decl.handleEndOfTranslationUnit();
  
  Result->Decls = S.Context.AllocateCopy(llvm::ArrayRef<Decl*>(Decls));
  return Result;
}

/// parseDeclTopLevel
///   decl-top-level:
///     ';'
///     decl-oneof
///     decl-struct
///     decl-func
///     decl-typealias
///     decl-var
void Parser::parseDeclTopLevel(llvm::SmallVectorImpl<Decl *> &Decls) {
  switch (Tok.getKind()) {
  default:
    error(Tok.getLoc(), "expected a top level declaration");
    break;
  case tok::semi:
    // Could create a decl for top-level semicolons.
    consumeToken(tok::semi);
    return;
      
  case tok::kw_typealias:
    if (TypeAliasDecl *D = parseDeclTypeAlias())
      return Decls.push_back(D);
    break;
  case tok::kw_oneof:
    if (Decl *O = parseDeclOneOf())
      return Decls.push_back(O);
    break;
  case tok::kw_struct:
    if (Decl *D = parseDeclStruct())
      return Decls.push_back(D);
    break;
  case tok::kw_func:
    if (FuncDecl *D = parseDeclFunc())
      return Decls.push_back(D);
    break;
  case tok::kw_var:
    if (!parseDeclVar(Decls))
      return;
    break;
  }
  
  // On error, skip to the next top level declaration.
  while (Tok.isNot(tok::eof) && Tok.isNot(tok::kw_var) &&
         Tok.isNot(tok::kw_func) && Tok.isNot(tok::kw_oneof) &&
         Tok.isNot(tok::kw_struct) && Tok.isNot(tok::kw_typealias))
    consumeToken();
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
      llvm::StringRef Text = Tok.getText();
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
  Attributes.LSquareLoc = Tok.getLoc();
  consumeToken(tok::l_square);
  
  // If this is an empty attribute list, consume it and return.
  if (Tok.is(tok::r_square)) {
    Attributes.RSquareLoc = Tok.getLoc();
    consumeToken(tok::r_square);
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

/// parseVarName
///   var-name:
///     identifier
///     '(' ')'
///     '(' name (',' name)* ')'
bool Parser::parseVarName(DeclVarName &Name) {
  // Single name case.
  if (Tok.is(tok::identifier)) {
    Name.LPLoc = Name.RPLoc = Tok.getLoc();
    parseIdentifier(Name.Name, "");
    return false;
  }
  
  Name.LPLoc = Tok.getLoc();
  if (parseToken(tok::l_paren, "expected identifier or '(' in var name"))
    return true;
  
  llvm::SmallVector<DeclVarName*, 8> ChildNames;
  
  if (Tok.isNot(tok::r_paren)) {
    do {
      DeclVarName *Elt = new (S.Context) DeclVarName();
      if (parseVarName(*Elt)) return true;
      ChildNames.push_back(Elt);
    } while (consumeIf(tok::comma));
  }

  Name.RPLoc = Tok.getLoc();
  if (parseToken(tok::r_paren, "expected ')' at end of var name"))
    note(Name.LPLoc, "to match this '('");

  Name.Elements = ChildNames;
  Name.Elements = S.Context.AllocateCopy(Name.Elements);
  return false;
}


/// parseDeclTypeAlias
///   decl-typealias:
///     'typealias' identifier ':' type
TypeAliasDecl *Parser::parseDeclTypeAlias() {
  SMLoc TypeAliasLoc = Tok.getLoc();
  consumeToken(tok::kw_typealias);
  
  Identifier Id;
  Type *Ty = 0;
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
                                    llvm::SmallVectorImpl<unsigned> &AccessPath,
                                      VarDecl *VD, SemaDecl &SD,
                                      llvm::SmallVectorImpl<Decl *> &Decls) {
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
bool Parser::parseDeclVar(llvm::SmallVectorImpl<Decl *> &Decls) {
  SMLoc VarLoc = Tok.getLoc();
  consumeToken(tok::kw_var);
  
  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    parseAttributeList(Attributes);

  DeclVarName VarName;
  if (parseVarName(VarName)) return true;
  
  Type *Ty = 0;
  NullablePtr<Expr> Init;
  if (parseValueSpecifier(Ty, Init))
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
    llvm::SmallVector<unsigned, 8> AccessPath;
    AddElementNamesForVarDecl(VD->NestedName, AccessPath, VD, S.decl, Decls);
  }
  return false;
}


/// parseDeclFunc - Parse a 'func' declaration, returning null on error.  The
/// caller handles this case and does recovery as appropriate.
///
///   decl-func:
///     'func' attribute-list? identifier arg-list-type '=' expr
///     'func' attribute-list? identifier arg-list-type expr-brace
///     'func' attribute-list? identifier arg-list-type 
FuncDecl *Parser::parseDeclFunc() {
  SMLoc FuncLoc = Tok.getLoc();
  consumeToken(tok::kw_func);

  DeclAttributes Attributes;
  // FIXME: Implicitly add immutable attribute.
  if (Tok.is(tok::l_square))
    parseAttributeList(Attributes);

  Identifier Name;
  if (parseIdentifier(Name, "expected identifier in func declaration"))
    return 0;
  
  // We force first type of a func declaration to be a tuple for consistency.
  if (Tok.isNot(tok::l_paren)) {
    error(Tok.getLoc(), "expected '(' in argument list of func declaration");
    return 0;
  }
    
  Type *FuncTy = 0;
  if (parseType(FuncTy))
    return 0;
  
  // If the parsed function type is not spelled as a function type (i.e., has an
  // '->' in it), then it is implicitly a function that returns ().
  if (!llvm::isa<FunctionType>(FuncTy))
    FuncTy = S.type.ActOnFunctionType(FuncTy, SMLoc(),
                                      S.Context.TheEmptyTupleType);

  // Build the decl for the function.
  FuncDecl *FD = S.decl.ActOnFuncDecl(FuncLoc, Name, FuncTy, Attributes);
  
  // Enter the func into the current scope, which allows it to be visible and
  // used within its body.
  if (FD)
    S.decl.AddToScope(FD);
  
  // Enter the arguments for the function into a new function-body scope.  We
  // need this even if there is no function body to detect argument name
  // duplication.
  Scope FnBodyScope(S.decl);
  
  if (FD)
    S.decl.CreateArgumentDeclsForFunc(FD);

  // Then parse the expression.
  llvm::NullablePtr<Expr> Body;

  // Check to see if we have a "= expr" or "{" which is a brace expr.
  if (consumeIf(tok::equal)) {
    if (parseExpr(Body, "expected expression parsing func body") ||
        Body.isNull())
      return 0;  // FIXME: Need to call a new ActOnFuncBodyError?
  } else if (Tok.is(tok::l_brace)) {
    if (parseExprBrace(Body) || Body.isNull())
      return 0;  // FIXME: Need to call a new ActOnFuncBodyError?
  }

  // If this is a declaration, we're done.
  if (Body.isNull())
    return FD;

  return S.decl.ActOnFuncBody(FD, Body.get());
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
  SMLoc OneOfLoc = Tok.getLoc();
  consumeToken(tok::kw_oneof);

  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    parseAttributeList(Attributes);
  
  SMLoc NameLoc = Tok.getLoc();
  Identifier OneOfName;
  Type *OneOfType = 0;
  if (parseIdentifier(OneOfName, "expected identifier in oneof declaration") ||
      parseTypeOneOfBody(OneOfLoc, Attributes, OneOfType))
    return 0;

  return S.decl.ActOnTypeAlias(NameLoc, OneOfName, OneOfType);
}


/// parseDeclStruct - Parse a 'struct' declaration, returning null (and doing no
/// token skipping) on error.  A 'struct' is just syntactic sugar for a oneof
/// with a single element.
///
///   decl-struct:
///      'struct' attribute-list? identifier type-tuple
///
Decl *Parser::parseDeclStruct() {
  SMLoc StructLoc = Tok.getLoc();
  consumeToken(tok::kw_struct);
  
  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    parseAttributeList(Attributes);
  
  Identifier StructName;
  if (parseIdentifier(StructName, "expected identifier in struct declaration"))
    return 0;

  Type *Ty = 0;
  if (parseType(Ty)) return 0;

  // The type is required to be syntactically a tuple type.
  if (!llvm::isa<TupleType>(Ty)) {
    error(StructLoc, "element type of struct is not a tuple");
    // FIXME: Should set this as an erroroneous decl.
    return 0;
  }      
          
  return S.decl.ActOnStructDecl(StructLoc, Attributes, StructName, Ty);
}


//===----------------------------------------------------------------------===//
// Type Parsing
//===----------------------------------------------------------------------===//

/// parseType
///   type:
///     type-simple
///     type-function
///     type-array
///
///   type-function:
///     type-simple '->' type 
///
///   type-array:
///     type '[' ']'
///     type '[' expr ']'
///
///   type-simple:
///     '__builtin_int32_type'
///     identifier
///     type-tuple
///     type-oneof
///
///   type-oneof:
///     'oneof' attribute-list? oneof-body
///
bool Parser::parseType(Type *&Result, const llvm::Twine &Message) {
  // Parse type-simple first.
  switch (Tok.getKind()) {
  case tok::identifier:
    Result = S.type.ActOnTypeName(Tok.getLoc(),
                                  S.Context.getIdentifier(Tok.getText()));
    consumeToken(tok::identifier);
    break;
  case tok::kw___builtin_int32_type:
    Result = S.type.ActOnInt32Type(Tok.getLoc());
    consumeToken(tok::kw___builtin_int32_type);
    break;
  case tok::l_paren:
    if (parseTypeTuple(Result))
      return true;
    break;
  case tok::kw_oneof: {
    SMLoc OneOfLoc = Tok.getLoc();
    consumeToken(tok::kw_oneof);
      
    DeclAttributes Attributes;
    if (Tok.is(tok::l_square))
      parseAttributeList(Attributes);

    if (parseTypeOneOfBody(OneOfLoc, Attributes, Result))
      return true;
    break;
  }
  default:
    error(Tok.getLoc(), Message);
    return true;
  }
  
   while (1) {
    // If there is an arrow, parse the rest of the type.
    SMLoc TokLoc = Tok.getLoc();
    if (consumeIf(tok::arrow)) {
      Type *SecondHalf = 0;
      if (parseType(SecondHalf, "expected type in result of function type"))
        return true;
      Result = S.type.ActOnFunctionType(Result, TokLoc, SecondHalf);
      continue;
    }
    
    // If there is a square bracket, we have an array.
    if (consumeIf(tok::l_square)) {
      llvm::NullablePtr<Expr> Size;
      if (!Tok.is(tok::r_square) &&
          parseExpr(Size, "expected expression for array type size"))
        return true;
      
      SMLoc RArrayTok = Tok.getLoc();
      if (parseToken(tok::r_square, "expected ']' in array type")) {
        note(TokLoc, "to match this '['");
        return true;
      }
      
      Result = S.type.ActOnArrayType(Result, TokLoc, Size.getPtrOrNull(),
                                     RArrayTok);
      continue;
    }
    
    break;
  }
        
  
  return false;
}

bool Parser::parseType(Type *&Result) {
  return parseType(Result, "expected type");
}

/// parseTypeTuple
///   type-tuple:
///     '(' ')'
///     '(' identifier? value-specifier (',' identifier? value-specifier)* ')'
///
bool Parser::parseTypeTuple(Type *&Result) {
  assert(Tok.is(tok::l_paren) && "Not start of type tuple");
  SMLoc LPLoc = Tok.getLoc();
  consumeToken(tok::l_paren);

  llvm::SmallVector<TupleTypeElt, 8> Elements;

  if (Tok.isNot(tok::r_paren)) {
    bool HadError = false;
    do {
      Elements.push_back(TupleTypeElt());
      TupleTypeElt &Result = Elements.back();
      
      if (Tok.is(tok::identifier))
        parseIdentifier(Result.Name, "");
      
      NullablePtr<Expr> Init;
      if ((HadError = parseValueSpecifier(Result.Ty, Init)))
        break;
      Result.Init = Init.getPtrOrNull();
    } while (consumeIf(tok::comma));
    
    if (HadError) {
      skipUntil(tok::r_paren);
      if (Tok.is(tok::r_paren))
        consumeToken(tok::r_paren);
      return true;
    }
  }
  
  SMLoc RPLoc = Tok.getLoc();
  if (parseToken(tok::r_paren, "expected ')' at end of tuple list",
                 tok::r_paren)) {
    note(LPLoc, "to match this opening '('");
    return true;
  }
  
  Result = S.type.ActOnTupleType(LPLoc, Elements.data(), Elements.size(),RPLoc);
  return false;
}

///   oneof-body:
///      '{' oneof-element-list '}'
///   oneof-element-list:
///      oneof-element ','?
///      oneof-element ',' oneof-element-list
///   oneof-element:
///      identifier
///      identifier ':' type
///      
bool Parser::parseTypeOneOfBody(SMLoc OneOfLoc, const DeclAttributes &Attrs,
                                Type *&Result) {
  if (parseToken(tok::l_brace, "expected '{' in oneof type"))
    return true;
  
  llvm::SmallVector<SemaType::OneOfElementInfo, 8> ElementInfos;
  
  // Parse the comma separated list of oneof elements.
  while (Tok.is(tok::identifier)) {
    SemaType::OneOfElementInfo ElementInfo;
    ElementInfo.Name = Tok.getText();
    ElementInfo.NameLoc = Tok.getLoc();
    ElementInfo.EltType = 0;
    
    consumeToken(tok::identifier);
    
    // See if we have a type specifier for this oneof element.  If so, parse it.
    if (consumeIf(tok::colon) &&
        parseType(ElementInfo.EltType,
                  "expected type while parsing oneof element '" +
                  ElementInfo.Name + "'")) {
      skipUntil(tok::r_brace);
      return true;
    }
    
    ElementInfos.push_back(ElementInfo);
    
    // Require comma separation.
    if (!consumeIf(tok::comma))
      break;
  }
  
  parseToken(tok::r_brace, "expected '}' at end of oneof");
  
  Result = S.type.ActOnOneOfType(OneOfLoc, Attrs, ElementInfos);
  return false;
}


//===----------------------------------------------------------------------===//
// Expression Parsing
//===----------------------------------------------------------------------===//

static bool isStartOfExpr(Token &Tok) {
  return Tok.is(tok::numeric_constant) || Tok.is(tok::colon) ||
         Tok.is(tok::l_paren) || Tok.is(tok::l_brace) ||
         Tok.is(tok::dollarident) || Tok.is(tok::identifier);
}

/// parseExpr
///   expr:
///     expr-primary+
bool Parser::parseExpr(NullablePtr<Expr> &Result, const char *Message) {
  llvm::SmallVector<Expr*, 8> SequencedExprs;
  
  do {
    // Parse the expr-primary.
    Result = 0;
    if (parseExprPrimary(Result) || Result.isNull()) return true;

    SequencedExprs.push_back(Result.get());    
  } while (isStartOfExpr(Tok));
  
  // If there is exactly one element in the sequence, it is a degenerate
  // sequence that just returns the last value anyway, shortcut ActOnSequence.
  if (SequencedExprs.size() == 1) {
    Result = SequencedExprs[0];
    return false;
  }
  
  Result = S.expr.ActOnSequence(SequencedExprs);
  return false;
}

/// parseExprPrimary
///   expr-primary:
///     expr-literal
///     expr-identifier
///     ':' identifier
///     expr-paren
///     expr-brace
///     expr-field
///     expr-subscript
///     expr-primary-fn expr-primary
///
///   expr-literal:
///     numeric_constant
///
///   expr-primary-fn:
///     expr-primary      Type sensitive: iff expr has fn type
///
///   expr-field:
///     expr-primary '.' identifier
///     expr-primary '.' dollarident
///
///   expr-subscript:
///     expr-primary '[' expr ']'
bool Parser::parseExprPrimary(NullablePtr<Expr> &Result, const char *Message) {
  switch (Tok.getKind()) {
  case tok::numeric_constant:
    Result = S.expr.ActOnNumericConstant(Tok.getText(), Tok.getLoc());
    consumeToken(tok::numeric_constant);
    break;

  case tok::dollarident: // $1
  case tok::identifier:  // foo   and  foo::bar
    if (parseExprIdentifier(Result)) return true;
    break;

  case tok::colon: {     // :foo
    SMLoc ColonLoc = Tok.getLoc();
    consumeToken(tok::colon);
    Identifier Name;
    SMLoc NameLoc = Tok.getLoc();
    if (parseIdentifier(Name, "expected identifier after ':' expression"))
      return true;
    Result = S.expr.ActOnUnresolvedMemberExpr(ColonLoc, NameLoc, Name);
    break;
  }
      
  case tok::l_paren:
    if (parseExprParen(Result)) return true;
    break;
      
  case tok::l_brace:
    if (parseExprBrace(Result)) return true;
    break;
    
  default:
    error(Tok.getLoc(), Message ? Message : "expected expression");
    return true;
  }
  
  // Handle suffix expressions.
  while (1) {
    // Check for a .foo suffix.
    SMLoc TokLoc = Tok.getLoc();
    if (consumeIf(tok::period)) {
      if (Tok.isNot(tok::identifier) && Tok.isNot(tok::dollarident)) {
        error(Tok.getLoc(), "expected field name");
        return true;
      }
        
      if (!Result.isNull())
        Result = S.expr.ActOnDotIdentifier(Result.get(), TokLoc, 
                                         S.Context.getIdentifier(Tok.getText()),
                                           Tok.getLoc());
      if (Tok.is(tok::identifier))
        consumeToken(tok::identifier);
      else
        consumeToken(tok::dollarident);
      continue;
    }
    
    // Check for a [expr] suffix.
    if (consumeIf(tok::l_square)) {
      NullablePtr<Expr> Idx;
      if (parseExpr(Idx, "expected expression parsing array index"))
        return true;
      
      SMLoc RLoc = Tok.getLoc();
      if (parseToken(tok::r_square, "expected ']'")) {
        note(TokLoc, "to match this '['");
        return true;        
      }
      
      if (!Result.isNull() && !Idx.isNull())
        Result = S.expr.ActOnArraySubscript(Result.get(), TokLoc, Idx.get(),
                                            RLoc);
    }
        
    break;
  }
  
  return false;
}

/// parseExprIdentifier - Parse an identifier expression:
///
///   expr-identifier:
///     identifier
///     dollarident
///     identifier '::' identifier
bool Parser::parseExprIdentifier(llvm::NullablePtr<Expr> &Result) {
  if (Tok.is(tok::dollarident)) {
    llvm::StringRef Name = Tok.getText();
    SMLoc Loc = Tok.getLoc();
    consumeToken(tok::dollarident);
    assert(Name[0] == '$' && "Not a dollarident");
    bool AllNumeric = true;
    for (unsigned i = 1, e = Name.size(); i != e; ++i)
      AllNumeric &= isdigit(Name[i]);
    
    if (Name.size() == 1 || !AllNumeric) {
      error(Loc, "invalid identifier, expected expression");
      return true;
    }
    Result = S.expr.ActOnDollarIdentExpr(Name, Loc);
    return false;
  }

  assert(Tok.is(tok::identifier));
  SMLoc Loc = Tok.getLoc();
  Identifier Name;
  parseIdentifier(Name, "");

  if (Tok.isNot(tok::coloncolon)) {
    Result = S.expr.ActOnIdentifierExpr(Name, Loc);
    return false;
  }
  
  SMLoc ColonColonLoc = Tok.getLoc();
  consumeToken(tok::coloncolon);

  SMLoc Loc2 = Tok.getLoc();
  Identifier Name2;
  if (parseIdentifier(Name2, "expected identifier after '" + Name.str() +
                      "::' expression"))
    return true;

  Result = S.expr.ActOnScopedIdentifierExpr(Name, Loc, ColonColonLoc,
                                            Name2, Loc2);
  return false;
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
bool Parser::parseExprParen(llvm::NullablePtr<Expr> &Result) {
  SMLoc LPLoc = Tok.getLoc();  
  consumeToken(tok::l_paren);
  
  llvm::SmallVector<Expr*, 8> SubExprs;
  llvm::SmallVector<Identifier, 8> SubExprNames; 
  bool AnyErroneousSubExprs = false;
  
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
      
      NullablePtr<Expr> SubExpr;
      if (parseExpr(SubExpr, "expected expression in parentheses")) return true;
      
      if (SubExpr.isNull())
        AnyErroneousSubExprs = true;
      else
        SubExprs.push_back(SubExpr.get());
    
    } while (consumeIf(tok::comma));
  }
  
  SMLoc RPLoc = Tok.getLoc();  
  if (parseToken(tok::r_paren, "expected ')' in parenthesis expression")) {
    note(LPLoc, "to match this opening '('");
    return true;
  }
  
  if (!AnyErroneousSubExprs)
    Result = S.expr.ActOnTupleExpr(LPLoc, SubExprs.data(),
                                   SubExprNames.empty()?0 : SubExprNames.data(),
                                   SubExprs.size(), RPLoc);
  return false;
}


/// parseExprBrace - A brace enclosed expression list which may optionally end
/// with a ; inside of it.  For example { 1; 4+5; } or { 1; 2 }.
///
///   expr-brace:
///     '{' expr-brace-item* '}'
///   expr-brace-item:
///     expr
///     expr '=' expr
///     decl-var
///     ';'
bool Parser::parseExprBrace(NullablePtr<Expr> &Result) {
  SMLoc LBLoc = Tok.getLoc();
  consumeToken(tok::l_brace);
  
  // This brace expression forms a lexical scope.
  Scope BraceScope(S.decl);

  llvm::SmallVector<llvm::PointerUnion<Expr*, Decl*>, 16> Entries;
  
  // MissingSemiAtEnd - Keep track of whether the last expression in the block
  // had no semicolon.
  bool MissingSemiAtEnd = false;
  
  while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof)) {
    MissingSemiAtEnd = false;

    // If this is a semi, eat it and ignore it.
    if (consumeIf(tok::semi))
      continue;
    
    // Otherwise, we must have a var decl or expression.  Parse it up
    Entries.push_back(llvm::PointerUnion<Expr*, Decl*>());
    
    // Parse the decl or expression.    
    switch (Tok.getKind()) {
    case tok::kw_var: {
      llvm::SmallVector<Decl*, 4> VD;
      if (parseDeclVar(VD)) break; // Error.
      assert(!VD.empty() && "Cannot return an empty decl list on success!");
      Entries.back() = VD[0];
      for (unsigned i = 1, e = VD.size(); i != e; ++i)
        Entries.push_back(VD[i]);
      break;
    }
    case tok::kw_typealias:
      Entries.back() = parseDeclTypeAlias();
      break;
    case tok::kw_oneof:
      Entries.back() = parseDeclOneOf();
      break;
    case tok::kw_struct:
      Entries.back() = parseDeclStruct();
      break;
    default:
      NullablePtr<Expr> ResultExpr;
      if (parseExpr(ResultExpr) || ResultExpr.isNull())
        break;
        
      // FIXME: Assignment is a hack until we get generics.  We really want to
      // parse '=' as any other overloaded/generic binary operator.
      if (Tok.is(tok::equal)) {
        SMLoc EqualLoc = Tok.getLoc();
        consumeToken();
        NullablePtr<Expr> RHSExpr;
        if (parseExpr(RHSExpr) || RHSExpr.isNull())
          break;
        
        // FIXME: Assignment is represented with null Fn.
        ResultExpr = new (S.Context) BinaryExpr(ResultExpr.get(), 0, EqualLoc,
                                                RHSExpr.get());
      }
        
      Entries.back() = ResultExpr.get();
      MissingSemiAtEnd = true;
      break;
    }
    
    if (Entries.back().isNull()) {
      if (Tok.is(tok::semi)) {
        Entries.pop_back();
        continue;  // Consume the ';' and keep going.
      }
      
      // FIXME: QOI: Improve error recovery.
      if (Tok.is(tok::semi) && Tok.isNot(tok::r_brace))
        skipUntil(tok::r_brace);
      consumeIf(tok::r_brace);
      return true;
    }
  }
  
  SMLoc RBLoc = Tok.getLoc();
  if (parseToken(tok::r_brace, "expected '}' at end of brace expression",
                 tok::r_brace)) {
    note(LBLoc, "to match this opening '{'");
    return true;
  }
  
  Result = S.expr.ActOnBraceExpr(LBLoc, Entries, MissingSemiAtEnd, RBLoc);
  return false;
}
