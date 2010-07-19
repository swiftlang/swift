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
#include "swift/AST/Expr.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;
using llvm::SMLoc;

//===----------------------------------------------------------------------===//
// Setup and Helper Methods
//===----------------------------------------------------------------------===//

Parser::Parser(unsigned BufferID, llvm::SourceMgr &SM, ASTContext &Context)
  : SourceMgr(SM),
    L(*new Lexer(BufferID, SM)),
    S(*new Sema(Context)) {
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
  
  Error(Tok.getLocation(), Message ? Message : "expected identifier");
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
  
  Error(Tok.getLocation(), Message);
  SkipUntil(SkipToTok);
  return true;
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
  
  while (Tok.isNot(tok::eof))
    ParseDeclTopLevel();
}

/// ParseDeclTopLevel
///   decl-top-level:
///     ';'
///     decl-var
void Parser::ParseDeclTopLevel() {
  switch (Tok.getKind()) {
  default:
    Error(Tok.getLocation(), "expected a top level declaration");
    return SkipUntil(tok::semi);
  case tok::semi:   return ConsumeToken(tok::semi); 
  case tok::kw_var: return ParseDeclVar();
  }
}

/// ParseDeclVar
///   decl-var:
///      'var' identifier ':' type ';'
///      'var' identifier ':' type '=' expression ';'
///      'var' identifier '=' expression ';'
void Parser::ParseDeclVar() {
  SMLoc VarLoc = Tok.getLocation();
  ConsumeToken(tok::kw_var);

  llvm::StringRef Identifier;
  if (ParseIdentifier(Identifier, "expected identifier in var declaration"))
    return SkipUntil(tok::semi);

  Type *Ty = 0;
  if (ConsumeIf(tok::colon) &&
      ParseType(Ty, "expected type in var declaration"))
    return SkipUntil(tok::semi);
  
  Expr *Init = 0;
  if (ConsumeIf(tok::equal) &&
      ParseExpr(Init, "expected expression in var declaration"))
    return SkipUntil(tok::semi);
  
  // Diagnose when we don't have a type or an expression.
  // FIXME: Sink into Sema.
  if (Ty == 0 && Init == 0)
    return Error(VarLoc, "var declaration must specify a type if no "
                 "initializer is specified");
  
  ParseToken(tok::semi, "expected ';' at end of var declaration", tok::semi);
}

//===----------------------------------------------------------------------===//
// Type Parsing
//===----------------------------------------------------------------------===//

/// ParseType
///   type:
///     int
bool Parser::ParseType(Type *&Result, const char *Message) {
  switch (Tok.getKind()) {
  case tok::kw_int:
    Result = S.Context.IntType;
    ConsumeToken(tok::kw_int);
    return false;
  case tok::kw_void:
    Result = S.Context.VoidType;
    ConsumeToken(tok::kw_void);
    return false;
  default:
    Error(Tok.getLocation(), Message ? Message : "expected type");
    return true;
  }
}

//===----------------------------------------------------------------------===//
// Expression Parsing
//===----------------------------------------------------------------------===//

/// ParseExpr
///   expr:
///     expr-primary (binary-operator expr-primary)*
bool Parser::ParseExpr(Expr *&Result, const char *Message) {
  return ParseExprPrimary(Result, Message) ||
         ParseExprBinaryRHS(Result);
}

/// ParseExprPrimary
///   expr-primary:
///     numeric_constant
///     '(' expr ')'
bool Parser::ParseExprPrimary(Expr *&Result, const char *Message) {
  switch (Tok.getKind()) {
  case tok::numeric_constant:
    S.Expr.ActOnNumericConstant(Tok.getText(), Tok.getLocation());
    ConsumeToken(tok::numeric_constant);
    return false;
      
  case tok::l_paren: {
    SMLoc LPLoc = Tok.getLocation();  
    ConsumeToken(tok::l_paren);
    Expr *SubExpr = 0;
    if (ParseExpr(SubExpr, "expected expression in parentheses")) return true;
    
    SMLoc RPLoc = Tok.getLocation();  
    if (ParseToken(tok::r_paren, "expected ')' in parenthesis expression")) {
      Note(LPLoc, "to match this opening '('");
      return true;
    }
    
    Result = S.Expr.ActOnParenExpr(LPLoc, SubExpr, RPLoc);
    return false;
  }
    
  default:
    Error(Tok.getLocation(), Message ? Message : "expected expression");
    return true;
  }
}

/// prec::Level - Binary operator precedences.   Low precedences numbers bind
/// more weakly than high numbers.
namespace prec {
  enum Level {
    Unknown = 0,    // Not a binary operator.
    Additive,       // +, -
    Multiplicative  // *, /, %
  };
}

/// getBinOpPrecedence - Return the precedence of the specified binary operator
/// token.
///
static prec::Level getBinOpPrecedence(tok::TokenKind Kind) {
  switch (Kind) {
  default: return prec::Unknown;
  case tok::plus:
  case tok::minus:                return prec::Additive;
  //case tok::percent:
  case tok::slash:
  case tok::star:                 return prec::Multiplicative;
  }
}

/// getBinOpKind - Return the expression kind of the specified token.
static ExprKind getBinOpKind(tok::TokenKind Kind) {
  switch (Kind) {
  default: assert(0 && "not a binary operator!");
  case tok::plus:                 return BinaryAddExprKind;
  case tok::minus:                return BinarySubExprKind;
    //case tok::percent:
  case tok::slash:                return BinaryDivExprKind;
  case tok::star:                 return BinaryMulExprKind;
  }
}


/// ParseExprBinaryRHS - Parse the right hand side of a binary expression and
/// assemble it according to precedence rules.
///
///   expr-binary-rhs:
///     (binary-operator expr-primary)*
bool Parser::ParseExprBinaryRHS(Expr *&Result, unsigned MinPrec) {
  prec::Level NextTokPrec = getBinOpPrecedence(Tok.getKind());
  while (1) {
    // If this token has a lower precedence than we are allowed to parse (e.g.
    // because we are called recursively, or because the token is not a binop),
    // then we are done!
    if (NextTokPrec < (prec::Level)MinPrec)
      return false;
    
    // Consume the operator, saving the operator token for error reporting.
    Token OpToken = Tok;
    ConsumeToken();
    
    // TODO: Support ternary operators some day.
    
    // Parse another leaf here for the RHS of the operator.
    Expr *Leaf = 0;
    if (ParseExprPrimary(Leaf, "expected expression after binary operator"))
      return true;

    // Remember the precedence of this operator and get the precedence of the
    // operator immediately to the right of the RHS.
    prec::Level ThisPrec = NextTokPrec;
    NextTokPrec = getBinOpPrecedence(Tok.getKind());
    
    // TODO: All operators are left associative at the moment.
    
    // If the next operator binds more tightly with RHS than we do, evaluate the
    // RHS as a complete subexpression first
    if (ThisPrec < NextTokPrec) {
      // Only parse things on the RHS that bind more tightly than the current
      // operator.
      if (ParseExprBinaryRHS(Leaf, ThisPrec + 1))
        return true;
      
      NextTokPrec = getBinOpPrecedence(Tok.getKind());
    }
    assert(NextTokPrec <= ThisPrec && "Recursion didn't work!");
    
    Result = S.Expr.ActOnBinaryExpr(getBinOpKind(OpToken.getKind()), Result,
                                    OpToken.getLocation(), Leaf);
  }
  
  return false;
}
