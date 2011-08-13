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
#include "ParseResult.h"
#include "Scope.h"
#include "Sema.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

/// ActOnCondition - Handle a condition to an if/while statement, inserting
/// the call that will convert to a 1-bit type.
Expr *Parser::actOnCondition(Expr *Cond) {
  assert(Cond);
  
  // The condition needs to be convertible to a logic value.  Build a call to
  // "convertToLogicValue" passing in the condition as an argument.
  Identifier C2LVFuncId = Context.getIdentifier("convertToLogicValue");
  Expr *C2LVFunc = actOnIdentifierExpr(C2LVFuncId, Cond->getLocStart());
  
  return new (Context) CallExpr(C2LVFunc, Cond, Type());
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
  Scope BraceScope(S.decl);
    
  while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof)) {
    bool NeedParseErrorRecovery = false;
    
    // Parse the decl, stmt, or expression.    
    switch (Tok.getKind()) {
    case tok::semi:
    case tok::l_brace:
    case tok::kw_return:
    case tok::kw_if:
    case tok::kw_while: {
      ParseResult<Stmt> Res = parseStmtOtherThanAssignment();
      if (Res.isParseError()) {
        NeedParseErrorRecovery = true;
        break;
      }
      
      if (!Res.isSemaError())
        Entries.push_back(Res.get());
      break;
    }
    case tok::kw_import:
      Entries.push_back(parseDeclImport());

      if (Entries.back() && !IsTopLevel) {
        error(Entries.back().get<Decl*>()->getLocStart(),
              "import is only valid at file scope");
        Entries.pop_back();
      }
      break;

    case tok::kw_var:
      parseDeclVar(Entries);
      break;
    case tok::kw_typealias:
      Entries.push_back(parseDeclTypeAlias());
      break;
    case tok::kw_oneof:
      Entries.push_back(parseDeclOneOf());
      break;
    case tok::kw_struct:
      parseDeclStruct(Entries);
      break;
    case tok::kw_func:
      // "func identifier" and "func [attribute]" is a func declaration,
      // otherwise we have a func expression.
      if (peekToken().is(tok::identifier) ||
          peekToken().is(tok::oper) ||
          peekToken().is(tok::l_square)) {
        Entries.push_back(parseDeclFunc());
        break;
      }
      // FALL THROUGH into expression case.
    default:
      ParseResult<Expr> ResultExpr;
      if ((ResultExpr = parseExpr())) {
        NeedParseErrorRecovery = true;
        break;
      }
      
      // Check for assignment.  If we don't have it, then we just have a simple
      // expression.
      if (Tok.isNot(tok::equal)) {
        if (!ResultExpr.isSemaError())
          Entries.push_back(ResultExpr.get());
        break;
      }
        
      SMLoc EqualLoc = consumeToken();
      ParseResult<Expr> RHSExpr;
      if ((RHSExpr = parseExpr("expected expression in assignment"))) {
        NeedParseErrorRecovery = true;
        break;
      }
      
      if (!ResultExpr.isSemaError() && !RHSExpr.isSemaError())
        Entries.push_back(new (Context) AssignStmt(ResultExpr.get(),
                                                   EqualLoc, RHSExpr.get()));
      break;
    }
    
    // FIXME: This is a hack.
    if (!Entries.empty() && Entries.back().isNull()) {
      Entries.pop_back();
      NeedParseErrorRecovery = true;
    }
    
    if (NeedParseErrorRecovery) {
      if (Tok.is(tok::semi))
        continue;  // Consume the ';' and keep going.
      
      // FIXME: QOI: Improve error recovery.
      if (Tok.isNot(tok::r_brace))
        skipUntil(tok::r_brace);
      consumeIf(tok::r_brace);
      return true;
    }
  }

  return false;
}

/// parseStmtOtherThanAssignment - Note that this doesn't handle the
/// "expr '=' expr" production.
///
ParseResult<Stmt> Parser::parseStmtOtherThanAssignment() {
  switch (Tok.getKind()) {
  default:
    error(Tok.getLoc(), "expected statement");
    return true;
  case tok::semi:      return new (Context) SemiStmt(consumeToken(tok::semi));
  case tok::l_brace:   return parseStmtBrace();
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
ParseResult<BraceStmt> Parser::parseStmtBrace(const char *Message) {
  if (Tok.isNot(tok::l_brace)) {
    error(Tok.getLoc(), Message ? Message : "expected '{'");
    return true;
  }
  SMLoc LBLoc = consumeToken(tok::l_brace);
  
  SmallVector<ExprStmtOrDecl, 16> Entries;
  
  if (parseBraceItemList(Entries, false /*NotTopLevel*/))
    return true;
  
  SMLoc RBLoc = Tok.getLoc();
  if (parseToken(tok::r_brace, "expected '}' at end of brace expression",
                 tok::r_brace)) {
    note(LBLoc, "to match this opening '{'");
    return true;
  }
  
  ExprStmtOrDecl *NewElements = 
    Context.AllocateCopy<ExprStmtOrDecl>(Entries.begin(), Entries.end());
  
  return new (Context) BraceStmt(LBLoc, NewElements, Entries.size(), RBLoc);
}

/// parseStmtReturn
///
///   stmt-return:
///     return expr?
///   
ParseResult<Stmt> Parser::parseStmtReturn() {
  SMLoc ReturnLoc = consumeToken(tok::kw_return);

  // Handle the ambiguity between consuming the expression and allowing the
  // enclosing stmt-brace to get it by eagerly eating it.
  ParseResult<Expr> Result;
  if (isStartOfExpr(Tok, peekToken())) {
    if ((Result = parseExpr("expected expresssion in 'return' statement")))
      return true;
  } else {
    // Result value defaults to ().
    Result = new (Context) TupleExpr(SMLoc(), 0, 0, 0, SMLoc(), false);
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
  SMLoc IfLoc = consumeToken(tok::kw_if);

  ParseResult<Expr> Condition;
  ParseResult<BraceStmt> NormalBody;
  if ((Condition = parseSingleExpr("expected expresssion in 'if' condition")) ||
      (NormalBody = parseStmtBrace("expected '{' after 'if' condition")))
    return true;
    
  ParseResult<Stmt> ElseBody;
  SMLoc ElseLoc = Tok.getLoc();
  if (consumeIf(tok::kw_else)) {
    if (Tok.is(tok::kw_if))
      ElseBody = parseStmtIf();
    else
      ElseBody = parseStmtBrace("expected '{' after 'else'");
    if (ElseBody.isParseError()) return true;
  } else {
    ElseLoc = SMLoc();
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
  SMLoc WhileLoc = consumeToken(tok::kw_while);
  
  ParseResult<Expr> Condition;
  ParseResult<BraceStmt> Body;
  if ((Condition
         = parseSingleExpr("expected expresssion in 'while' condition")) ||
      (Body = parseStmtBrace("expected '{' after 'while' condition")))
    return true;
  
  // If our condition and normal expression parsed correctly, build an AST.
  if (Condition.isSemaError() || Body.isSemaError())
    return ParseResult<Stmt>::getSemaError();
  
  Expr *Cond = actOnCondition(Condition.get());

  return new (Context) WhileStmt(WhileLoc, Cond, Body.get());
}


