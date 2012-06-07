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
#include "swift/AST/Attr.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/OwningPtr.h"
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
  case tok::kw_for:
  case tok::kw_break:
  case tok::kw_continue:
    return true;
  }
}

/// isFuncExpr - Return true if this two token sequence is the start of a func
/// expression (i.e. not a func *decl* or something else).
static bool isFuncExpr(const Token &Tok1, const Token &Tok2) {
  if (Tok1.isNot(tok::kw_func)) return false;
  
  // "func identifier" and "func [attribute]" is a func declaration,
  // otherwise we have a func expression.
  return Tok2.isNot(tok::identifier) && Tok2.isNotAnyOperator() &&
         Tok2.isNotAnyLSquare();
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
  case tok::kw_class:
  case tok::kw_protocol:
  case tok::kw_import:
  case tok::kw_subscript:
  case tok::kw_constructor:
    return true;
  default:
    return false;
  }
}

/// parseExprOrStmtAssign
///   expr-or-stmt-assign:
///     expr
///     stmt-assign
bool Parser::parseExprOrStmtAssign(ExprStmtOrDecl &Result) {
  NullablePtr<Expr> ResultExpr = parseExpr(diag::expected_expr);
  if (ResultExpr.isNull())
    return true;

  Result = ResultExpr.get();
  
  // Check for assignment.  If we don't have it, then we just have a
  // simple expression.
  if (Tok.isNot(tok::equal))
    return false;

  SourceLoc EqualLoc = consumeToken();
  NullablePtr<Expr> RHSExpr = parseExpr(diag::expected_expr_assignment);
  if (RHSExpr.isNull())
    return true;
  Result = new (Context) AssignStmt(ResultExpr.get(),
                                    EqualLoc, RHSExpr.get());
  return false;
}

bool Parser::parseExprOrStmt(ExprStmtOrDecl &Result) {
  if (isStartOfStmtOtherThanAssignment(Tok)) {
    NullablePtr<Stmt> Res = parseStmtOtherThanAssignment();
    if (Res.isNull())
      return true;
    Result = Res.get();
    return false;
  }

  assert(Tok.isNot(tok::l_brace) &&
         "expr-anon-closure should be parsed as stmt-brace here");
  return parseExprOrStmtAssign(Result);
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
///     stmt-for-c-style
///     stmt-for-c-each
///   stmt-assign:
///     expr '=' expr
void Parser::parseBraceItemList(SmallVectorImpl<ExprStmtOrDecl> &Entries,
                                bool IsTopLevel) {
  // This forms a lexical scope.
  Scope BraceScope(this, !IsTopLevel);
    
  SmallVector<Decl*, 8> TmpDecls;
  
  while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof)) {
    bool NeedParseErrorRecovery = false;
    TopLevelCodeDecl *TLCD = 0;
    llvm::OwningPtr<ContextChange> CC;

    // Parse the decl, stmt, or expression.
    if (isStartOfDecl(Tok, peekToken())) {
      if (parseDecl(TmpDecls, IsTopLevel ? PD_AllowTopLevel : PD_Default))
        NeedParseErrorRecovery = true;
      else {
        for (Decl *D : TmpDecls)
          Entries.push_back(D);
      }

      TmpDecls.clear();
    } else if (IsTopLevel) {
      TLCD = new (Context) TopLevelCodeDecl(CurDeclContext);
      ContextChange CC(*this, TLCD);

      ExprStmtOrDecl Result;
      if (parseExprOrStmt(Result)) {
        NeedParseErrorRecovery = true;
      } else {
        if (Result.is<Expr*>())
           TLCD->setBody(Result.get<Expr*>());
        else
          TLCD->setBody(Result.get<Stmt*>());
        Entries.push_back(TLCD);
      }
    } else {
      ExprStmtOrDecl Result;
      if (parseExprOrStmt(Result))
        NeedParseErrorRecovery = true;
      else
        Entries.push_back(Result);
    }

    // If we had a parse error, skip to the start of the next stmt or decl.  It
    // would be ideal to stop at the start of the next expression (e.g. "X = 4")
    // but distinguishing the start of an expression from the middle of one is
    // "hard".
    if (NeedParseErrorRecovery)
      skipUntilDeclStmtRBrace();

    if (TLCD && !IsMainModule && !NeedParseErrorRecovery) {
      if (TLCD->getBody().is<Stmt*>()) {
        // Statements are not allowed at the top level outside the main module.
        SourceLoc Loc = TLCD->getBody().get<Stmt*>()->getStartLoc();
        diagnose(Loc, diag::illegal_top_level_stmt);
      } else {
        // Expressions are not allowed at the top level outside the main module.
        SourceLoc Loc = TLCD->getBody().get<Expr*>()->getStartLoc();
        diagnose(Loc, diag::illegal_top_level_expr);
      }
    }

    if (IsTopLevel && IsMainModule && !NeedParseErrorRecovery) {
      if (TLCD || isa<PatternBindingDecl>(Entries.back().get<Decl*>())) {
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
  case tok::kw_for:    return parseStmtFor();
  case tok::kw_break:
    return new (Context) BreakStmt(consumeToken(tok::kw_break));
  case tok::kw_continue:
    return new (Context) ContinueStmt(consumeToken(tok::kw_continue));
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
  Expr *RetExpr = nullptr;
  if (Tok.isNot(tok::r_brace) && Tok.isNot(tok::semi)) {
    NullablePtr<Expr> Result = parseExpr(diag::expected_expr_return);
    if (Result.isNull())
      return 0;
    RetExpr = Result.get();
  }

  return new (Context) ReturnStmt(ReturnLoc, RetExpr);
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

NullablePtr<Stmt> Parser::parseStmtFor() {
  SourceLoc ForLoc = consumeToken(tok::kw_for);
  
  // The c-style-for loop and foreach-style-for loop are conflated together into
  // a single keyword, so we have to do some lookahead to resolve what is going
  // on.  Eventually we will allow optional ()'s around the condition of the
  // c-style-for loop, which will require us to do arbitrary lookahead.  For now
  // though, we can distinguish between the two with two-token lookahead.

  // If we have a leading (, this is a pattern in a foreach loop.
  if (Tok.isAnyLParen())
    return parseStmtForEach(ForLoc);
  
  // If we have a leading identifier followed by a ':' or 'in', then this is a
  // pattern, so it is foreach. 
  if (Tok.is(tok::identifier) && 
      (peekToken().is(tok::colon) || peekToken().is(tok::kw_in)))
    return parseStmtForEach(ForLoc);

  // Otherwise, this is some sort of c-style for loop.
  return parseStmtForCStyle(ForLoc);
}
      
///   stmt-for-c-style:
///     'for' stmt-for-c-style-init? ';' expr? ';' expr-or-stmt-assign?
///           stmt-brace
///   stmt-for-c-style-init:
///     decl-var
///     expr-or-stmt-assign
NullablePtr<Stmt> Parser::parseStmtForCStyle(SourceLoc ForLoc) {
  SourceLoc Semi1Loc, Semi2Loc;
  
  ExprStmtOrDecl First;
  SmallVector<Decl*, 2> FirstDecls;
  NullablePtr<Expr> Second;
  ExprStmtOrDecl Third;
  NullablePtr<BraceStmt> Body;
  
  // Introduce a new scope to contain any var decls in the init value.
  Scope ForScope(this, /*AllowLookup=*/true);
  
  // Parse the first part, either a var, expr, or stmt-assign.
  if (Tok.is(tok::kw_var)) {
    if (parseDeclVar(false, FirstDecls)) return 0;
  } else if ((Tok.isNot(tok::semi) && parseExprOrStmtAssign(First)))
    return 0;
  
  // Parse the rest of the statement.
  if (parseToken(tok::semi, Semi1Loc, diag::expected_semi_for_stmt) ||
      (Tok.isNot(tok::semi) && Tok.isNot(tok::l_brace) &&
        (Second = parseExpr(diag::expected_cond_for_stmt)).isNull()) ||
      parseToken(tok::semi, Semi2Loc, diag::expected_semi_for_stmt) ||
      (Tok.isNot(tok::l_brace) && parseExprOrStmtAssign(Third)) ||
      (Body = parseStmtBrace(diag::expected_lbrace_after_for)).isNull())
    return 0;
  
  PointerUnion<Expr*, AssignStmt*> Initializer, Increment;
  if (First.isNull())
    ;
  else if (First.is<Expr*>())
    Initializer = First.get<Expr*>();
  else
    Initializer = cast<AssignStmt>(First.get<Stmt*>());

  if (Third.isNull())
    ;
  else if (Third.is<Expr*>())
    Increment = Third.get<Expr*>();
  else
    Increment = cast<AssignStmt>(Third.get<Stmt*>());

  ArrayRef<Decl*> FirstDeclsContext;
  if (!FirstDecls.empty())
    FirstDeclsContext = Context.AllocateCopy(FirstDecls);
  
  return new (Context) ForStmt(ForLoc, Initializer, FirstDeclsContext,
                               Semi1Loc, Second,
                               Semi2Loc, Increment, Body.get());
}

/// 
///   stmt-for-each:
///     'for' pattern 'in' expr stmt-brace
NullablePtr<Stmt> Parser::parseStmtForEach(SourceLoc ForLoc) {
  // pattern
  NullablePtr<Pattern> Pattern = parsePattern();
  
  // 'in'
  if (!Tok.is(tok::kw_in)) {
    if (Pattern.isNonNull())
      diagnose(Tok.getLoc(), diag::expected_foreach_in);
    return nullptr;
  }
  SourceLoc InLoc = consumeToken();
  
  // expr
  NullablePtr<Expr> Container = parseExpr(diag::expected_foreach_container);

  // Introduce a new scope and place the variables in the pattern into that
  // scope.
  // FIXME: We may want to merge this scope with the scope introduced by
  // the stmt-brace, as in C++.
  Scope ForEachScope(this, /*AllowLookup=*/true);
  if (Pattern.isNonNull()) {
    SmallVector<Decl *, 2> Decls;
    DeclAttributes Attributes;
    addVarsToScope(Pattern.get(), Decls, Attributes);
  }
  // stmt-brace
  NullablePtr<BraceStmt> Body = parseStmtBrace(diag::expected_foreach_lbrace);
  
  if (Pattern.isNull() || Container.isNull() || Body.isNull())
    return nullptr;

  return new (Context) ForEachStmt(ForLoc, Pattern.get(), InLoc,
                                   Container.get(), Body.get());
}


