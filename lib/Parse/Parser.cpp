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
#include "swift/AST/ASTConsumer.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/NullablePtr.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/PointerUnion.h"
using namespace swift;
using llvm::SMLoc;
using llvm::NullablePtr;

//===----------------------------------------------------------------------===//
// Setup and Helper Methods
//===----------------------------------------------------------------------===//

Parser::Parser(unsigned BufferID, ASTConsumer &consumer)
  : Consumer(consumer),
    SourceMgr(Consumer.getContext().SourceMgr),
    L(*new Lexer(BufferID, SourceMgr)),
    S(*new Sema(Consumer.getContext())) {
}

Parser::~Parser() {
  delete &L;
  delete &S;
}

void Parser::Note(SMLoc Loc, const char *Message) {
  SourceMgr.PrintMessage(Loc, Message, "note");
}

void Parser::Warning(SMLoc Loc, const char *Message) {
  SourceMgr.PrintMessage(Loc, Message, "warning");
}

void Parser::Error(SMLoc Loc, const char *Message) {
  SourceMgr.PrintMessage(Loc, Message, "error");
}

void Parser::ConsumeToken() {
  assert(Tok.isNot(tok::eof) && "Lexing past eof!");
  L.Lex(Tok);
}

/// SkipUntil - Read tokens until we get to the specified token, then return.
/// Because we cannot guarantee that the token will ever occur, this skips to
/// some likely good stopping point.
///
void Parser::SkipUntil(tok::TokenKind T) {
  // tok::unknown is a sentinel that means "don't skip".
  if (T == tok::unknown) return;
  
  while (Tok.isNot(tok::eof) && Tok.isNot(T)) {
    switch (Tok.getKind()) {
    default: ConsumeToken(); break;
    // TODO: Handle paren/brace/bracket recovery.
    }
  }
}


//===----------------------------------------------------------------------===//
// Primitive Parsing
//===----------------------------------------------------------------------===//

/// ParseIdentifier - Consume an identifier if present and return its name in
/// Result.  Otherwise, emit an error and return true.
bool Parser::ParseIdentifier(llvm::StringRef &Result, const char *Message,
                             tok::TokenKind SkipToTok) {
  if (Tok.is(tok::identifier)) {
    Result = Tok.getText();
    ConsumeToken();
    return false;
  }
  
  Error(Tok.getLoc(), Message ? Message : "expected identifier");
  return true;
}

/// ParseToken - The parser expects that 'K' is next in the input.  If so, it is
/// consumed and false is returned.
///
/// If the input is malformed, this emits the specified error diagnostic.
/// Next, if SkipToTok is specified, it calls SkipUntil(SkipToTok).  Finally,
/// true is returned.
bool Parser::ParseToken(tok::TokenKind K, const char *Message,
                        tok::TokenKind SkipToTok) {
  if (Tok.is(K)) {
    ConsumeToken(K);
    return false;
  }
  
  Error(Tok.getLoc(), Message);
  SkipUntil(SkipToTok);
  
  // If we skipped ahead to the missing token and found it, consume it as if
  // there were no error.
  if (K == SkipToTok && Tok.is(SkipToTok))
    ConsumeToken();
  return true;
}

///   type-or-decl-var:
///     type
///     decl-var
bool Parser::ParseTypeOrDeclVar(llvm::PointerUnion<Type*, NamedDecl*> &Result,
                                const char *Message) {
  if (Tok.is(tok::kw_var)) {
    Result = ParseDeclVar();
    return Result.isNull();
  }
  
  Type *ResultType = 0;
  if (ParseType(ResultType, Message)) return true;
  Result = ResultType;
  return false;
}

///   expr-or-decl-var:
///     type
///     decl-var
bool Parser::ParseExprOrDeclVar(llvm::PointerUnion<Expr*, NamedDecl*> &Result,
                                const char *Message) {
  if (Tok.is(tok::kw_var)) {
    Result = ParseDeclVar();
    return Result.isNull();
  }
  
  NullablePtr<Expr> ResultExpr;
  if (ParseExpr(ResultExpr, Message) || ResultExpr.isNull())
    return true;
  Result = ResultExpr.get();
  return false;
}


//===----------------------------------------------------------------------===//
// Decl Parsing
//===----------------------------------------------------------------------===//

/// ParseTranslationUnit
///   translation-unit:
///     decl-top-level*
void Parser::ParseTranslationUnit() {
  // Prime the lexer.
  ConsumeToken();
  
  {
    // The entire translation unit is in a big scope.
    Scope OuterScope(S.decl);
  
    while (Tok.isNot(tok::eof)) {
      if (Decl *D = ParseDeclTopLevel())
        Consumer.HandleTopLevelDecl(D);
    }
  }
  
  // Notify consumer about the end of the translation unit.
  Consumer.HandleEndOfTranslationUnit();
}

/// ParseDeclTopLevel
///   decl-top-level:
///     ';'
///     decl-var ';'
///     decl-func
Decl *Parser::ParseDeclTopLevel() {
  switch (Tok.getKind()) {
  default:
    Error(Tok.getLoc(), "expected a top level declaration");
    break;
  case tok::semi:
    ConsumeToken(tok::semi);
    return 0; // Could do a top-level semi decl.
  case tok::kw_var:
    if (VarDecl *D = ParseDeclVar()) {
      S.decl.ActOnTopLevelDecl(D);
      
      // On successful parse, eat the ;
      ParseToken(tok::semi, "expected ';' at end of var declaration",
                 tok::semi);
      return D;
    }
    break;
  case tok::kw_func:
    if (FuncDecl *D = ParseDeclFunc()) {
      S.decl.ActOnTopLevelDecl(D);
      return D;
    }
    break;
  }
  
  S.decl.ActOnTopLevelDeclError();
  
  // On error, skip to the next top level declaration.
  while (Tok.isNot(tok::eof) && Tok.isNot(tok::kw_var) &&
         Tok.isNot(tok::kw_func))
    ConsumeToken();
  return 0;
}


/// ParseDeclAttribute
///   decl-attribute:
///     'infix' '=' numeric_constant
bool Parser::ParseDeclAttribute(DeclAttributes &Attributes) {
  if (Tok.is(tok::identifier) && Tok.getText() == "infix") {
    if (Attributes.InfixPrecedence != -1)
      Error(Tok.getLoc(), "infix precedence repeatedly specified");
    ConsumeToken(tok::identifier);

    // The default infix precedence is 100.
    Attributes.InfixPrecedence = 100;
    
    if (ConsumeIf(tok::equal)) {
      SMLoc PrecLoc = Tok.getLoc();
      llvm::StringRef Text = Tok.getText();
      if (!ParseToken(tok::numeric_constant,
                      "expected precedence number in 'infix' attribute")) {
        long long Value;
        if (Text.getAsInteger(10, Value) || Value > 255 || Value < 0)
          Error(PrecLoc, "invalid precedence: value must be between 0 and 255");
        else
          Attributes.InfixPrecedence = Value;
      }
    }
    
    return false;
  }
  
  Error(Tok.getLoc(), "unknown declaration attribute");
  SkipUntil(tok::r_square);
  return true;
}

/// ParseDeclAttributeList
///   decl-attribute-list:
///     '[' ']'
///     '[' decl-attribute (',' decl-attribute)* ']'
void Parser::ParseDeclAttributeList(DeclAttributes &Attributes) {
  Attributes.LSquareLoc = Tok.getLoc();
  ConsumeToken(tok::l_square);
  
  // If this is an empty attribute list, consume it and return.
  if (Tok.is(tok::r_square)) {
    Attributes.RSquareLoc = Tok.getLoc();
    ConsumeToken(tok::r_square);
    return;
  }
  
  bool HadError = ParseDeclAttribute(Attributes);
  while (Tok.is(tok::comma)) {
    ConsumeToken(tok::comma);
    HadError |= ParseDeclAttribute(Attributes);
  }

  Attributes.RSquareLoc = Tok.getLoc();
  if (ConsumeIf(tok::r_square))
    return;
  
  // Otherwise, there was an error parsing the attribute list.  If we already
  // reported an error, skip to a ], otherwise report the error.
  if (!HadError)
    ParseToken(tok::r_square, "expected ']' or ',' in attribute list",
               tok::r_square);
  else {
    SkipUntil(tok::r_square);
    ConsumeIf(tok::r_square);
  }
}

/// ParseDeclVar - Parse a 'var' declaration, returning null (and doing no
/// token skipping) on error.
///
///   decl-var:
///      'var' decl-attribute-list? identifier ':' type
///      'var' decl-attribute-list? identifier ':' type '=' expression 
///      'var' decl-attribute-list? identifier '=' expression
VarDecl *Parser::ParseDeclVar() {
  SMLoc VarLoc = Tok.getLoc();
  ConsumeToken(tok::kw_var);
  
  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    ParseDeclAttributeList(Attributes);
  
  llvm::StringRef Identifier;
  if (ParseIdentifier(Identifier, "expected identifier in var declaration"))
    return 0;
  
  Type *Ty = 0;
  if (ConsumeIf(tok::colon) &&
      ParseType(Ty, "expected type in var declaration"))
    return 0;
  
  NullablePtr<Expr> Init;
  if (ConsumeIf(tok::equal)) {
    if (ParseExpr(Init, "expected expression in var declaration"))
      return 0;
  
    // If there was an expression, but it had a parse error, give the var decl
    // an artificial int type to avoid chained errors.
    // FIXME: We really need to distinguish erroneous expr from missing expr in
    // ActOnVarDecl.
    if (Init.isNull() && Ty == 0)
      Ty = S.Context.IntType;
  }
  
  return S.decl.ActOnVarDecl(VarLoc, Identifier, Ty, Init.getPtrOrNull(),
                             Attributes);
}

/// ParseDeclFunc - Parse a 'func' declaration, returning null on error.  The
/// caller handles this case and does recovery as appropriate.
///
///   decl-func:
///     'func' decl-attribute-list? identifier arg-list ('->' type)? '='? expr
///     'func' decl-attribute-list? identifier arg-list ('->' type)? ';'
///   arg-list:
///     '(' ')'
///     '(' arg-func-formal (',' arg-func-formal)* ')'
///   arg-func-formal:
///     identifier? ':' type
///
/// TODO: eventually allow default arguments and attributes on arguments.
///
FuncDecl *Parser::ParseDeclFunc() {
  SMLoc FuncLoc = Tok.getLoc();
  ConsumeToken(tok::kw_func);

  DeclAttributes Attributes;
  // FIXME: Add immutable attribute.
  if (Tok.is(tok::l_square))
    ParseDeclAttributeList(Attributes);

  llvm::StringRef Identifier;
  if (ParseIdentifier(Identifier, "expected identifier in func declaration"))
    return 0;
  
  SMLoc LPLoc = Tok.getLoc();
  if (ParseToken(tok::l_paren,
                 "expected '(' in argument list of func declaration"))
    return 0;

  Type *ArgListTy;
  if (Tok.is(tok::r_paren)) {
    ArgListTy = S.Context.VoidType;
  } else {
    llvm::SmallVector<llvm::PointerUnion<Type*, NamedDecl*>, 8> Elements;
    
    
    // Read the comma-separated argument list.
    do {
      llvm::StringRef ParamIdentifier;
      SMLoc StartLoc = Tok.getLoc();
      if (Tok.is(tok::identifier)) {
        ParamIdentifier = Tok.getText();
        ConsumeToken(tok::identifier);
      }
      
      Type *ParamType = 0;
      if (ParseToken(tok::colon,
                     "expected ':' in argument list of func declaration") ||
          ParseType(ParamType,
                    "expected type in argument list of func declaration"))
        return 0;
      
      DeclAttributes ParamAttributes;
      VarDecl *ParamDecl = 
        S.decl.ActOnVarDecl(StartLoc, ParamIdentifier, ParamType, 0,
                            ParamAttributes);

      Elements.push_back(ParamDecl);
    } while (ConsumeIf(tok::comma));
    
    ArgListTy = 
      S.type.ActOnTupleType(LPLoc, Elements.data(), Elements.size(),
                            Tok.getLoc());
  }
  
  SMLoc RPLoc = Tok.getLoc();
  if (ParseToken(tok::r_paren,
                 "expected ')' at end of func declaration argument list")) {
    Note(LPLoc, "to match this opening '('");
    return 0;
  }
  
  // If there is a return type, parse it.
  Type *RetTy = S.Context.VoidType;
  SMLoc ArrowLoc = Tok.getLoc();
  if (ConsumeIf(tok::arrow)) {
    if (ParseType(RetTy, "expected func declaration return type"))
      return 0;
  } else {
    ArrowLoc = SMLoc();
  }
  
  // Build the function type.
  Type *FuncTy = S.type.ActOnFunctionType(ArgListTy, ArrowLoc, RetTy);
  
  // If this is a declaration, we're done.
  if (ConsumeIf(tok::semi))
    return S.decl.ActOnFuncDecl(FuncLoc, Identifier, FuncTy, 0,
                                Attributes);

  // Otherwise, we must have an expression.  Eat the optional '=' if present.
  ConsumeIf(tok::equal);

  // Then parse the expression.
  llvm::NullablePtr<Expr> Body;
  if (ParseExpr(Body, "expected expression parsing func body") ||
      Body.isNull())
    return 0;

  return S.decl.ActOnFuncDecl(FuncLoc, Identifier, FuncTy, Body.getPtrOrNull(),
                              Attributes);
}

//===----------------------------------------------------------------------===//
// Type Parsing
//===----------------------------------------------------------------------===//

/// ParseType
///   type:
///     type-simple
///     type-simple '->' type
///
///   type-simple:
///     'int'
///     'void'   // FIXME: Should be a 'type alias' for () in standard library.
///     type-tuple
///
bool Parser::ParseType(Type *&Result, const char *Message) {
  // Parse type-simple first.
  switch (Tok.getKind()) {
  case tok::kw_int:
    Result = S.type.ActOnIntType(Tok.getLoc());
    ConsumeToken(tok::kw_int);
    break;
  case tok::kw_void:
    Result = S.type.ActOnVoidType(Tok.getLoc());
    ConsumeToken(tok::kw_void);
    break;
  case tok::l_paren:
    if (ParseTypeTuple(Result))
      return true;
    break;
  default:
    Error(Tok.getLoc(), Message ? Message : "expected type");
    return true;
  }
  
  // If there is an arrow, parse the rest of the type.
  llvm::SMLoc ArrowLoc = Tok.getLoc();
  if (ConsumeIf(tok::arrow)) {
    Type *SecondHalf = 0;
    if (ParseType(SecondHalf, "expected type in result of function type"))
      return true;
    Result = S.type.ActOnFunctionType(Result, ArrowLoc, SecondHalf);
  }
  
  return false;
}

/// ParseTypeTuple
///   type-tuple:
///     '(' ')'
///     '(' type-or-decl-var (',' type-or-decl-var)* ')'
bool Parser::ParseTypeTuple(Type *&Result) {
  assert(Tok.is(tok::l_paren) && "Not start of type tuple");
  SMLoc LPLoc = Tok.getLoc();
  ConsumeToken(tok::l_paren);

  llvm::SmallVector<llvm::PointerUnion<Type*, NamedDecl*>, 8> Elements;

  if (Tok.isNot(tok::r_paren)) {
    Elements.push_back(llvm::PointerUnion<Type*, NamedDecl*>());
    bool Error = 
      ParseTypeOrDeclVar(Elements.back(),
                         "expected type or var declaration in tuple");

    // Parse (',' type-or-decl-var)* 
    while (!Error && Tok.is(tok::comma)) {
      ConsumeToken(tok::comma);
      Elements.push_back(llvm::PointerUnion<Type*, NamedDecl*>());
      Error = ParseTypeOrDeclVar(Elements.back(),
                                 "expected type or var declaration in tuple");
    }
    
    if (Error) {
      SkipUntil(tok::r_paren);
      if (Tok.is(tok::r_paren))
        ConsumeToken(tok::r_paren);
      return true;
    }
  }
  
  SMLoc RPLoc = Tok.getLoc();
  if (ParseToken(tok::r_paren, "expected ')' at end of tuple list",
                 tok::r_paren)) {
    Note(LPLoc, "to match this opening '('");
    return true;
  }
  
  Result = S.type.ActOnTupleType(LPLoc, Elements.data(), Elements.size(),RPLoc);
  return false;
}

//===----------------------------------------------------------------------===//
// Expression Parsing
//===----------------------------------------------------------------------===//

static bool isStartOfExpr(Token &Tok, Sema &S) {
  if (Tok.is(tok::identifier)) {
    // If this is a binary operator, then it isn't the start of an expr.
    NamedDecl *ND = S.decl.LookupName(S.Context.getIdentifier(Tok.getText()));
    
    // Use of undeclared identifier.
    if (ND == 0) return true;
    
    return ND->Attrs.InfixPrecedence == -1;
  }
  
  return Tok.is(tok::numeric_constant) ||
         Tok.is(tok::l_paren) || Tok.is(tok::l_brace);
}

/// ParseExpr
///   expr:
///     expr-single+
///   expr-single:
///     expr-primary (binary-operator expr-primary)*
bool Parser::ParseExpr(NullablePtr<Expr> &Result, const char *Message) {
  llvm::SmallVector<Expr*, 8> SequencedExprs;
  
  Expr *LastExpr = 0;
  do {
    // Parse the expr-single.
    Result = 0;
    if (ParseExprPrimary(Result, Message) || ParseExprBinaryRHS(Result) ||
        Result.isNull())
      return true;
  
    // Check to see if this juxtaposition is application of a function with its
    // arguments.  If so, bind the function application, otherwise, we have a
    // sequence.
    if (LastExpr == 0)
      LastExpr = Result.get();
    else {
      llvm::PointerIntPair<Expr*, 1, bool>
        ApplyRes = S.expr.ActOnJuxtaposition(LastExpr, Result.get());

      if (!ApplyRes.getInt()) {
        // Function application.
        LastExpr = ApplyRes.getPointer();
        if (LastExpr == 0) return true;
      } else {
        // Sequencing.
        assert(ApplyRes.getPointer() == 0 && "Sequencing with a result?");
        SequencedExprs.push_back(LastExpr);
        LastExpr = Result.get();
      }
    }
  } while (isStartOfExpr(Tok, S));

  assert(LastExpr && "Should have parsed at least one valid expression");
  
  // If there is exactly one element in the sequence, it is a degenerate
  // sequence that just returns the last value anyway, shortcut ActOnSequence.
  if (SequencedExprs.empty()) {
    Result = LastExpr;
    return false;
  }
  
  SequencedExprs.push_back(LastExpr);
  Result = S.expr.ActOnSequence(SequencedExprs.data(), SequencedExprs.size());
  return false;
}

/// ParseExprPrimary
///   expr-primary:
///     numeric_constant
///     identifier
///     '(' ')'
///     '(' expr (',' expr)* ')'
///     expr-brace
bool Parser::ParseExprPrimary(NullablePtr<Expr> &Result, const char *Message) {
  switch (Tok.getKind()) {
  case tok::numeric_constant:
    Result = S.expr.ActOnNumericConstant(Tok.getText(), Tok.getLoc());
    ConsumeToken(tok::numeric_constant);
    return false;

  case tok::identifier:
    Result = S.expr.ActOnIdentifierExpr(Tok.getText(), Tok.getLoc());
    ConsumeToken(tok::identifier);
    return false;
      
  case tok::l_paren: {
    // FIXME: This should eventually become a tuple literal expression.
    SMLoc LPLoc = Tok.getLoc();  
    ConsumeToken(tok::l_paren);
    
    llvm::SmallVector<Expr*, 8> SubExprs;
    bool AnyErroneousSubExprs = false;
    
    if (Tok.isNot(tok::r_paren)) {
      NullablePtr<Expr> SubExpr;
      if (ParseExpr(SubExpr, "expected expression in parentheses")) return true;
      
      if (SubExpr.isNull())
        AnyErroneousSubExprs = true;
      else
        SubExprs.push_back(SubExpr.get());
      
      while (ConsumeIf(tok::comma)) {
        SubExpr = 0;
        if (ParseExpr(SubExpr, "expected expression in parentheses")) return true;
        
        if (SubExpr.isNull())
          AnyErroneousSubExprs = true;
        else
          SubExprs.push_back(SubExpr.get());
      }
    }
    
    SMLoc RPLoc = Tok.getLoc();  
    if (ParseToken(tok::r_paren, "expected ')' in parenthesis expression")) {
      Note(LPLoc, "to match this opening '('");
      return true;
    }
    
    if (!AnyErroneousSubExprs)
      Result = S.expr.ActOnTupleExpr(LPLoc, SubExprs.data(), SubExprs.size(),
                                     RPLoc);
    return false;
  }
      
  case tok::l_brace:
    return ParseExprBrace(Result);
    
  default:
    Error(Tok.getLoc(), Message ? Message : "expected expression");
    return true;
  }
}


/// ParseExprBrace - A brace enclosed expression list which may optionally end
/// with a ; inside of it.  For example { 1; 4+5; } or { 1; 2 }.
///
///   expr-brace:
///     '{' (expr-or-decl-var ';')* expr? }
bool Parser::ParseExprBrace(NullablePtr<Expr> &Result) {
  SMLoc LBLoc = Tok.getLoc();
  ConsumeToken(tok::l_brace);
  
  // This brace expression forms a lexical scope.
  Scope BraceScope(S.decl);

  llvm::SmallVector<llvm::PointerUnion<Expr*, NamedDecl*>, 16> Entries;
  
  // MissingSemiAtEnd - Keep track of whether the last expression in the block
  // had no semicolon.
  bool MissingSemiAtEnd = true;
  
  // Parse the semicolon separated expression/var decls.
  do {
    // If we found a r_brace, then we either have an empty list {} or an
    // expression list terminated with a semicolon.  Remember this and break.
    if (Tok.is(tok::r_brace)) {
      MissingSemiAtEnd = false;
      break;
    }

    // Otherwise, we must have a var decl or expression.  Parse it up
    Entries.push_back(llvm::PointerUnion<Expr*, NamedDecl*>());
    
    // Parse the var or expression.  If we have an error, try to do nice
    // recovery.
    if (ParseExprOrDeclVar(Entries.back())) {
      if (Tok.is(tok::semi)) {
        Entries.pop_back();
        continue;  // Consume the ';' and keep going.
      }
      
      // FIXME: Improve error recovery.
      if (Tok.is(tok::semi) && Tok.isNot(tok::r_brace))
        SkipUntil(tok::r_brace);
      ConsumeIf(tok::r_brace);
      return true;
    }
    
    // If we just parsed a var decl, add it to the current scope.
    if (NamedDecl *D = Entries.back().dyn_cast<NamedDecl*>())
      S.decl.AddToScope(D);
    
  } while (ConsumeIf(tok::semi));
  
  SMLoc RBLoc = Tok.getLoc();
  if (ParseToken(tok::r_brace, "expected '}' at end of brace expression",
                 tok::r_brace)) {
    Note(LBLoc, "to match this opening '{'");
    return true;
  }
  
  // Diagnose cases where there was a ; missing after a 'var'.
  if (MissingSemiAtEnd && Entries.back().is<NamedDecl*>()) {
    Error(RBLoc, "expected ';' after var declaration");
    MissingSemiAtEnd = false;
  }
  
  Result = S.expr.ActOnBraceExpr(LBLoc, Entries.data(), Entries.size(),
                                 MissingSemiAtEnd, RBLoc);
  return false;
}


/// getBinOp - Return the NamedDecl for the token if it is an infix binary
/// operator, otherwise return null.
static NamedDecl *getBinOp(const Token &Tok, Sema &S) {
  if (Tok.isNot(tok::identifier))
    return 0;
  NamedDecl *ND = S.decl.LookupName(S.Context.getIdentifier(Tok.getText()));
  if (ND == 0 || ND->Attrs.InfixPrecedence == -1)
    return 0;
  return ND;
}


/// ParseExprBinaryRHS - Parse the right hand side of a binary expression and
/// assemble it according to precedence rules.
///
///   expr-binary-rhs:
///     (binary-operator expr-primary)*
bool Parser::ParseExprBinaryRHS(NullablePtr<Expr> &Result, unsigned MinPrec) {
  NamedDecl *NextTokOp = getBinOp(Tok, S);
  int NextTokPrec = NextTokOp ? NextTokOp->Attrs.InfixPrecedence : -1;
  while (1) {
    // If this token has a lower precedence than we are allowed to parse (e.g.
    // because we are called recursively, or because the token is not a binop),
    // then we are done!
    if (NextTokPrec < (int)MinPrec)
      return false;
    
    // Consume the operator, saving the operator location.
    SMLoc OpLoc = Tok.getLoc();
    ConsumeToken();
    
    // TODO: Support ternary operators some day.
    
    // Parse another leaf here for the RHS of the operator.
    NullablePtr<Expr> Leaf;
    if (ParseExprPrimary(Leaf, "expected expression after binary operator"))
      return true;

    // Remember the precedence of this operator and get the precedence of the
    // operator immediately to the right of the RHS.
    int ThisPrec = NextTokPrec;
    NamedDecl *ThisTokOp = NextTokOp;
    
    NextTokOp = getBinOp(Tok, S);
    NextTokPrec = NextTokOp ? NextTokOp->Attrs.InfixPrecedence : -1;

    // TODO: All operators are left associative at the moment.
    
    // If the next operator binds more tightly with RHS than we do, evaluate the
    // RHS as a complete subexpression first
    if (ThisPrec < NextTokPrec) {
      // Only parse things on the RHS that bind more tightly than the current
      // operator.
      if (ParseExprBinaryRHS(Leaf, ThisPrec + 1))
        return true;
      
      NextTokOp = getBinOp(Tok, S);
      NextTokPrec = NextTokOp ? NextTokOp->Attrs.InfixPrecedence : -1;
    }
    assert(NextTokPrec <= ThisPrec && "Recursion didn't work!");
    
    // Okay, we've finished the parse, form the AST node for the binop now.
    if (Result.isNonNull() && Leaf.isNonNull())
      Result = S.expr.ActOnBinaryExpr(Result.get(), ThisTokOp, OpLoc,
                                      Leaf.get());
  }
  
  return false;
}
