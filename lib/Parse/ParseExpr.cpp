//===--- ParseExpr.cpp - Swift Language Parser for Expressions ------------===//
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
// Expression Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "ParseResult.h"
#include "Scope.h"
#include "Sema.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "swift/AST/ASTContext.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

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
