//===--- ParseStmt.cpp - Swift Language Parser for Statements -------------===//
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
// Statement Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "Parser.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

/// isStartOfStmtOtherThanAssignment - Return true if the specified token starts
/// a statement (other than assignment, which starts looking like an expr).
///
/// Note this also returns true for '{' which can be the start of a stmt-brace
/// or the start of an expr-closure.  This ambiguity is resolved towards
/// statements when not in a subexpression context.
bool Parser::isStartOfStmtOtherThanAssignment(const Token &Tok) {
  switch (Tok.getKind()) {
  default: return false;
  case tok::semi:
  case tok::l_brace:
  case tok::kw_return:
  case tok::kw_if:
  case tok::kw_while:
    return true;
  }
}

/// isFuncExpr - Return true if this two token sequence is the start of a func
/// expression (i.e. not a func *decl* or something else).
static bool isFuncExpr(const Token &Tok1, const Token &Tok2) {
  if (Tok1.isNot(tok::kw_func)) return false;
  
  // "func identifier" and "func [attribute]" is a func declaration,
  // otherwise we have a func expression.
  return Tok2.isNot(tok::identifier) && Tok2.isNot(tok::oper) &&
         Tok2.isNot(tok::l_square) && Tok2.isNot(tok::l_square_space);
}

/// isStartOfDecl - Return true if this is the start of a decl or decl-import.
bool Parser::isStartOfDecl(const Token &Tok, const Token &Tok2) {
  switch (Tok.getKind()) {
  case tok::kw_func:
    // "func identifier" and "func [attribute]" is a func declaration,
    // otherwise we have a func expression.
    return !isFuncExpr(Tok, Tok2);
  case tok::kw_static:
    return Tok2.is(tok::kw_func);
  case tok::kw_extension:
  case tok::kw_var:
  case tok::kw_typealias:
  case tok::kw_oneof:
  case tok::kw_struct:
  case tok::kw_protocol:
  case tok::kw_import:
  case tok::kw_subscript:
    return true;
  default:
    return false;
  }
}



///   stmt-brace-item:
///     decl
///     expr
///     stmt
///   stmt:
///     ';'
///     stmt-assign
///     stmt-brace
///     stmt-return
///     stmt-if
///   stmt-assign:
///     expr '=' expr
void Parser::parseBraceItemList(SmallVectorImpl<ExprStmtOrDecl> &Entries,
                                bool IsTopLevel) {
  // This forms a lexical scope.
  Scope BraceScope(this);
    
  SmallVector<Decl*, 8> TmpDecls;
  
  while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof)) {
    bool NeedParseErrorRecovery = false;

    // Parse the decl, stmt, or expression.    
    if (isStartOfStmtOtherThanAssignment(Tok)) {
      NullablePtr<Stmt> Res = parseStmtOtherThanAssignment();
      if (Res.isNull())
        NeedParseErrorRecovery = true;
      else
        Entries.push_back(Res.get());
    } else if (isStartOfDecl(Tok, peekToken())) {
      if (parseDecl(TmpDecls, IsTopLevel ? PD_AllowTopLevel : PD_Default))
        NeedParseErrorRecovery = true;
      else {
        for (Decl *D : TmpDecls)
          Entries.push_back(D);
      }

      TmpDecls.clear();
    } else {
      // Verify that the ambiguity between expr-anon-closure and stmt-brace
      // isn't causing us heartburn.
      assert(Tok.isNot(tok::l_brace) &&
             "expr-anon-closure should be parsed as stmt-brace here");
      NullablePtr<Expr> ResultExpr = parseExpr(diag::expected_expr);
      if (ResultExpr.isNull()) {
        NeedParseErrorRecovery = true;
      } else if (Tok.isNot(tok::equal)) {
        Entries.push_back(ResultExpr.get());
      } else {
        // Check for assignment.  If we don't have it, then we just have a
        // simple expression.
        SourceLoc EqualLoc = consumeToken();
        NullablePtr<Expr> RHSExpr = parseExpr(diag::expected_expr_assignment);
        if (RHSExpr.isNull()) {
          NeedParseErrorRecovery = true;  // Error.
        } else {
          Entries.push_back(new (Context) AssignStmt(ResultExpr.get(),
                                                     EqualLoc, RHSExpr.get()));
        }
      }
    }
   
    // If we had a parse error, skip to the start of the next stmt or decl.  It
    // would be ideal to stop at the start of the next expression (e.g. "X = 4")
    // but distinguishing the start of an expression from the middle of one is
    // "hard".
    if (NeedParseErrorRecovery)
      skipUntilDeclStmtRBrace();

    if (IsTopLevel && !IsMainModule && !NeedParseErrorRecovery) {
      if (Entries.back().is<Stmt*>()) {
        // Statements are not allowed at the top level outside the main module.
        SourceLoc Loc = Entries.back().get<Stmt*>()->getStartLoc();
        diagnose(Loc, diag::illegal_top_level_stmt);
      } else if (Entries.back().is<Expr*>()) {
        // Expressions are not allowed at the top level outside the main module.
        SourceLoc Loc = Entries.back().get<Expr*>()->getStartLoc();
        diagnose(Loc, diag::illegal_top_level_expr);
      }
    }

    if (IsTopLevel && IsMainModule) {
      if (!Entries.back().is<Decl*>() ||
          isa<PatternBindingDecl>(Entries.back().get<Decl*>())) {
        FoundSideEffects = true;
        break;
      }
    }
  }
}

/// parseStmtOtherThanAssignment - Note that this doesn't handle the
/// "expr '=' expr" production.
///
NullablePtr<Stmt> Parser::parseStmtOtherThanAssignment() {
  switch (Tok.getKind()) {
  default:
    diagnose(Tok, diag::expected_stmt);
    return 0;
  case tok::semi:      return new (Context) SemiStmt(consumeToken(tok::semi));
  case tok::l_brace:
    return parseStmtBrace(diag::invalid_diagnostic).getPtrOrNull();
  case tok::kw_return: return parseStmtReturn();
  case tok::kw_if:     return parseStmtIf();
  case tok::kw_while:  return parseStmtWhile();
  }
}

/// parseStmtBrace - A brace enclosed expression/statement/decl list.  For
/// example { 1; 4+5; } or { 1; 2 }.
///
///   stmt-brace:
///     '{' stmt-brace-item* '}'
///
NullablePtr<BraceStmt> Parser::parseStmtBrace(Diag<> ID) {
  if (Tok.isNot(tok::l_brace)) {
    diagnose(Tok.getLoc(), ID);
    return 0;
  }
  SourceLoc LBLoc = consumeToken(tok::l_brace);
  
  SmallVector<ExprStmtOrDecl, 16> Entries;
  SourceLoc RBLoc;

  parseBraceItemList(Entries, false /*NotTopLevel*/);
  if (parseMatchingToken(tok::r_brace, RBLoc,
                         diag::expected_rbrace_in_brace_stmt,
                         LBLoc, diag::opening_brace))
    return 0;
  
  return BraceStmt::create(Context, LBLoc, Entries, RBLoc);
}

/// parseStmtReturn
///
///   stmt-return:
///     return expr?
///   
NullablePtr<Stmt> Parser::parseStmtReturn() {
  SourceLoc ReturnLoc = consumeToken(tok::kw_return);

  // Handle the ambiguity between consuming the expression and allowing the
  // enclosing stmt-brace to get it by eagerly eating it unless the return is
  // followed by a }.
  NullablePtr<Expr> Result;
  if (Tok.isNot(tok::r_brace)) {
    Result = parseExpr(diag::expected_expr_return);
    if (Result.isNull())
      return 0;
  } else {
    // Result value defaults to ().
    Result = new (Context) TupleExpr(SourceLoc(), MutableArrayRef<Expr*>(), 0,
                                     SourceLoc());
  }

  return new (Context) ReturnStmt(ReturnLoc, Result.get());
}


/// 
///   stmt-if:
///     'if' expr stmt-brace stmt-if-else?
///   stmt-if-else:
///    'else' stmt-brace
///    'else' stmt-if
NullablePtr<Stmt> Parser::parseStmtIf() {
  SourceLoc IfLoc = consumeToken(tok::kw_if);

  NullablePtr<Expr> Condition = parseExpr(diag::expected_expr_if);
  if (Condition.isNull()) return 0;
  NullablePtr<BraceStmt> NormalBody =
    parseStmtBrace(diag::expected_lbrace_after_if);
  if (NormalBody.isNull())
    return 0;
    
  NullablePtr<Stmt> ElseBody;
  SourceLoc ElseLoc = Tok.getLoc();
  if (consumeIf(tok::kw_else)) {
    if (Tok.is(tok::kw_if))
      ElseBody = parseStmtIf();
    else
      ElseBody =parseStmtBrace(diag::expected_lbrace_after_else).getPtrOrNull();
    if (ElseBody.isNull())
      return 0;
  } else {
    ElseLoc = SourceLoc();
  }

  // If our condition and normal expression parsed correctly, build an AST.
  return new (Context) IfStmt(IfLoc, Condition.get(), NormalBody.get(),
                              ElseLoc, ElseBody.getPtrOrNull());
}

/// 
///   stmt-while:
///     'while' expr stmt-brace
NullablePtr<Stmt> Parser::parseStmtWhile() {
  SourceLoc WhileLoc = consumeToken(tok::kw_while);
  
  NullablePtr<Expr> Condition = parseExpr(diag::expected_expr_while);
  if (Condition.isNull()) return 0;
  NullablePtr<BraceStmt> Body =
    parseStmtBrace(diag::expected_lbrace_after_while);
  if (Body.isNull())
    return 0;
  
  // If our normal expression parsed correctly, build an AST.
  return new (Context) WhileStmt(WhileLoc, Condition.get(), Body.get());
}


