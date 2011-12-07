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

/// ActOnCondition - Handle a condition to an if/while statement, inserting
/// the call that will convert to a 1-bit type.
Expr *Parser::actOnCondition(Expr *Cond) {
  assert(Cond);
  
  // The condition needs to be convertible to a logic value.  Build a call to
  // "convertToLogicValue" passing in the condition as an argument.
  Identifier C2LVFuncId = Context.getIdentifier("convertToLogicValue");
  Expr *C2LVFunc = actOnIdentifierExpr(C2LVFuncId, Cond->getLoc());
  
  return new (Context) CallExpr(C2LVFunc, Cond, /*DotSyntax=*/false,
                                TypeJudgement());
}

/// isStartOfStmtOtherThanAssignment - Return true if the specified token starts
/// a statement (other than assignment, which starts looking like an expr).
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
         Tok2.isNot(tok::l_square);
}

/// isStartOfDecl - Return true if this is the start of a decl or decl-import.
bool Parser::isStartOfDecl(const Token &Tok, const Token &Tok2) {
  switch (Tok.getKind()) {
  case tok::kw_func:
    // "func identifier" and "func [attribute]" is a func declaration,
    // otherwise we have a func expression.
    return !isFuncExpr(Tok, Tok2);
  case tok::kw_extension:
  case tok::kw_var:
  case tok::kw_typealias:
  case tok::kw_oneof:
  case tok::kw_struct:
  case tok::kw_protocol:
  case tok::kw_import:
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
///     expr '=' expr
///     stmt-brace
///     stmt-return
///     stmt-if
bool Parser::parseBraceItemList(SmallVectorImpl<ExprStmtOrDecl> &Entries,
                                bool IsTopLevel) {
  // This forms a lexical scope.
  Scope BraceScope(this);
    
  SmallVector<Decl*, 8> TmpDecls;
  
  while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof)) {
    bool NeedParseErrorRecovery = false;
    
    // Parse the decl, stmt, or expression.    
    if (isStartOfStmtOtherThanAssignment(Tok)) {
      ParseResult<Stmt> Res = parseStmtOtherThanAssignment();
      if (Res.isParseError())
        NeedParseErrorRecovery = true;
      else if (!Res.isSemaError())
        Entries.push_back(Res.get());
    } else if (isStartOfDecl(Tok, peekToken())) {
      if (parseDecl(TmpDecls, Type(),
                    IsTopLevel ? PD_AllowTopLevel : PD_Default))
        NeedParseErrorRecovery = true;
      else {
        for (Decl *D : TmpDecls)
          Entries.push_back(D);
      }

      TmpDecls.clear();
    } else {
      ParseResult<Expr> ResultExpr;
      if ((ResultExpr = parseExpr(diag::expected_expr))) {
        NeedParseErrorRecovery = true;
      } else if (Tok.is(tok::equal)) {
        // Check for assignment.  If we don't have it, then we just have a
        // simple expression.
        SourceLoc EqualLoc = consumeToken();
        ParseResult<Expr> RHSExpr;
        if ((RHSExpr = parseExpr(diag::expected_expr_assignment))) {
          NeedParseErrorRecovery = true;  // Error.
        } else if (!ResultExpr.isSemaError() && !RHSExpr.isSemaError())
          Entries.push_back(new (Context) AssignStmt(ResultExpr.get(),
                                                     EqualLoc, RHSExpr.get()));
      } else if (!ResultExpr.isSemaError()) {
        Entries.push_back(ResultExpr.get());
      }
    }
   
    // If we had a parse error, skip to the start of the next stmt or decl.  It
    // would be ideal to stop at the start of the next expression (e.g. "X = 4")
    // but distinguishing the start of an expression from the middle of one is
    // "hard".
    if (NeedParseErrorRecovery)
      skipUntilDeclStmtRBrace();
  }

  return false;
}

/// parseStmtOtherThanAssignment - Note that this doesn't handle the
/// "expr '=' expr" production.
///
ParseResult<Stmt> Parser::parseStmtOtherThanAssignment() {
  switch (Tok.getKind()) {
  default:
    diagnose(Tok, diag::expected_stmt);
    return true;
  case tok::semi:      return new (Context) SemiStmt(consumeToken(tok::semi));
  case tok::l_brace:   return parseStmtBrace(diag::invalid_diagnostic);
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
ParseResult<BraceStmt> Parser::parseStmtBrace(Diag<> ID) {
  if (Tok.isNot(tok::l_brace)) {
    diagnose(Tok.getLoc(), ID);
    return true;
  }
  SourceLoc LBLoc = consumeToken(tok::l_brace);
  
  SmallVector<ExprStmtOrDecl, 16> Entries;
  SourceLoc RBLoc;
  
  if (parseBraceItemList(Entries, false /*NotTopLevel*/) ||
      parseMatchingToken(tok::r_brace, RBLoc,
                         diag::expected_rbrace_in_brace_stmt,
                         LBLoc, diag::opening_brace))
    return true;
  
  return BraceStmt::create(Context, LBLoc, Entries, RBLoc);
}

/// parseStmtReturn
///
///   stmt-return:
///     return expr?
///   
ParseResult<Stmt> Parser::parseStmtReturn() {
  SourceLoc ReturnLoc = consumeToken(tok::kw_return);

  // Handle the ambiguity between consuming the expression and allowing the
  // enclosing stmt-brace to get it by eagerly eating it.
  ParseResult<Expr> Result;
  if (isStartOfExpr(Tok, peekToken())) {
    if ((Result = parseExpr(diag::expected_expr_return)))
      return true;
  } else {
    // Result value defaults to ().
    Result = new (Context) TupleExpr(SourceLoc(), 0, 0, 0, SourceLoc(),
                                     false);
  }

  if (!Result.isSemaError())
    return new (Context) ReturnStmt(ReturnLoc, Result.get());
  return ParseResult<Stmt>::getSemaError();
}


/// 
///   stmt-if:
///     'if' expr stmt-brace stmt-if-else?
///   stmt-if-else:
///    'else' stmt-brace
///    'else' stmt-if
ParseResult<Stmt> Parser::parseStmtIf() {
  SourceLoc IfLoc = consumeToken(tok::kw_if);

  ParseResult<Expr> Condition;
  ParseResult<BraceStmt> NormalBody;
  if ((Condition = parseSingleExpr(diag::expected_expr_if)) ||
      (NormalBody = parseStmtBrace(diag::expected_lbrace_after_if)))
    return true;
    
  ParseResult<Stmt> ElseBody;
  SourceLoc ElseLoc = Tok.getLoc();
  if (consumeIf(tok::kw_else)) {
    if (Tok.is(tok::kw_if))
      ElseBody = parseStmtIf();
    else
      ElseBody = parseStmtBrace(diag::expected_lbrace_after_else);
    if (ElseBody.isParseError()) return true;
  } else {
    ElseLoc = SourceLoc();
  }

  // If our condition and normal expression parsed correctly, build an AST.
  if (Condition.isSemaError() || NormalBody.isSemaError() ||
      ElseBody.isSemaError())
    return ParseResult<Stmt>::getSemaError();
  
  Expr *Cond = actOnCondition(Condition.get());
  
  Stmt *ElseBodyStmt = 0;
  if (!ElseBody.isAbsent())
    ElseBodyStmt = ElseBody.get();
  
  return new (Context) IfStmt(IfLoc, Cond, NormalBody.get(),
                              ElseLoc, ElseBodyStmt);
}

/// 
///   stmt-while:
///     'while' expr stmt-brace
ParseResult<Stmt> Parser::parseStmtWhile() {
  SourceLoc WhileLoc = consumeToken(tok::kw_while);
  
  ParseResult<Expr> Condition;
  ParseResult<BraceStmt> Body;
  if ((Condition = parseSingleExpr(diag::expected_expr_while)) ||
      (Body = parseStmtBrace(diag::expected_lbrace_after_while)))
    return true;
  
  // If our condition and normal expression parsed correctly, build an AST.
  if (Condition.isSemaError() || Body.isSemaError())
    return ParseResult<Stmt>::getSemaError();
  
  Expr *Cond = actOnCondition(Condition.get());

  return new (Context) WhileStmt(WhileLoc, Cond, Body.get());
}


