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

#include "Parser.h"
#include "swift/AST/Diagnostics.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

bool Parser::isStartOfExpr(const Token &Tok, const Token &Next) {
  if (Tok.is(tok::integer_literal) || Tok.is(tok::floating_literal) ||
      Tok.is(tok::colon) ||
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

/// parseExpr
///   expr:
///     expr-unary expr-binary*
///   expr-binary:
///     operator expr-unary
///
/// The sequencing here is not structural, i.e. binary operators are
/// not inherently right-associative.
ParseResult<Expr> Parser::parseExpr(Diag<> Message) {
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
    if (Tok.isNot(tok::oper))
      break;

    // Parse the operator.
    Expr *Operator = parseExprOperator();
    SequencedExprs.push_back(Operator);

    // The message is only valid for the first subexpr.
    Message = diag::expected_expr_after_operator;
  }

  // If we had semantic errors, just fail here.
  if (HasSemaError)
    return ParseResult<Expr>::getSemaError();
  assert(!SequencedExprs.empty());

  // If we saw no operators, don't build a sequence.
  if (SequencedExprs.size() == 1)
    return SequencedExprs[0];

  return SequenceExpr::create(Context, SequencedExprs);
}

/// parseExprUnary
///
///   expr-unary:
///     expr-postfix
///     operator expr-unary
ParseResult<Expr> Parser::parseExprUnary(Diag<> Message) {
  // If the next token is not an operator, just parse this as expr-postfix
  if (Tok.isNot(tok::oper))
    return parseExprPostfix(Message);

  // '&' is a very special case.
  if (Tok.getText() == "&") {
    SourceLoc loc = Tok.getLoc();
    consumeToken(tok::oper);

    ParseResult<Expr> SubExpr = parseExprUnary(Message);
    if (!SubExpr.isSuccess()) return SubExpr;

    return new (Context) AddressOfExpr(loc, SubExpr.get(), Type());
  }
  
  // Parse the operator.
  Expr *Operator = parseExprOperator();

  ParseResult<Expr> SubExpr = parseExprUnary(Message);
  if (!SubExpr.isSuccess()) return SubExpr;
  return new (Context) UnaryExpr(Operator, SubExpr.get());
}

/// parseExprOperator - Parse an operator reference expression.  These
/// are not "proper" expressions; they can only appear in binary/unary
/// operators.
Expr *Parser::parseExprOperator() {
  assert(Tok.is(tok::oper));
  SourceLoc Loc = Tok.getLoc();
  Identifier Name = Context.getIdentifier(Tok.getText());
  consumeToken(tok::oper);
  
  return actOnIdentifierExpr(Name, Loc);
}


/// parseExprPostfix
///
///   expr-literal:
///     integer_literal
///     floating_literal
///
///   expr-primary:
///     expr-literal
///     expr-identifier
///     expr-anon-closure-argument
///     expr-delayed-identifier
///     expr-paren
///     expr-func
///
///   expr-delayed-identifier:
///     ':' identifier
///
///   expr-dot:
///     expr-postfix '.' identifier
///     expr-postfix '.' dollarident
///
///   expr-subscript:
///     expr-postfix '[' expr ']'
///
///   expr-call:
///     expr-postfix expr-paren
///
///   expr-postfix:
///     expr-primary
///     expr-dot
///     expr-subscript
///     expr-call
///
ParseResult<Expr> Parser::parseExprPostfix(Diag<> ID) {
  ParseResult<Expr> Result;
  switch (Tok.getKind()) {
  case tok::integer_literal: {
    StringRef Text = Tok.getText();
    SourceLoc Loc = consumeToken(tok::integer_literal);
    Result = new (Context) IntegerLiteralExpr(Text, Loc);
    break;
  }
  case tok::floating_literal: {
    StringRef Text = Tok.getText();
    SourceLoc Loc = consumeToken(tok::floating_literal);
    Result = new (Context) FloatLiteralExpr(Text, Loc);
    break;
  }
  case tok::identifier:  // foo
    Result = parseExprIdentifier();
    break;
  case tok::dollarident: // $1
    Result = parseExprAnonClosureArg();
    break;

  case tok::colon: {     // :foo
    SourceLoc ColonLoc = consumeToken(tok::colon);
    Identifier Name;
    SourceLoc NameLoc = Tok.getLoc();
    if (parseIdentifier(Name, diag::expected_identifier_after_colon_expr))
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
    diagnose(Tok.getLoc(), ID);
    return true;
  }
  
  // If we had a parse error, don't attempt to parse suffixes.  Do keep going if
  // we had semantic errors though.
  if (Result.isParseError())
    return true;
    
  // Handle suffix expressions.
  while (1) {
    // Check for a .foo suffix.
    SourceLoc TokLoc = Tok.getLoc();
    
    if (consumeIf(tok::period)) {
      if (Tok.isNot(tok::identifier) && Tok.isNot(tok::dollarident)) {
        diagnose(Tok, diag::expected_field_name);
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
        Result = new (Context) CallExpr(Result.get(), Arg.get());
      continue;
    }
    
    // Check for a [expr] suffix.
    if (consumeIf(tok::l_square)) {
      ParseResult<Expr> Idx;
      SourceLoc RLoc;
      if ((Idx = parseExpr(diag::expected_expr_subscript_value)) ||
          parseMatchingToken(tok::r_square, RLoc,
                             diag::expected_bracket_array_subscript,
                             TokLoc, diag::opening_bracket))
        return true;
      
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
///     identifier
ParseResult<Expr> Parser::parseExprIdentifier() {
  assert(Tok.is(tok::identifier));
  SourceLoc Loc = Tok.getLoc();
  Identifier Name = Context.getIdentifier(Tok.getText());
  consumeToken(tok::identifier);
  return actOnIdentifierExpr(Name, Loc);
}

///   expr-anon-closure-argument:
///     dollarident
ParseResult<Expr> Parser::parseExprAnonClosureArg() {
  StringRef Name = Tok.getText();
  SourceLoc Loc = consumeToken(tok::dollarident);
  assert(Name[0] == '$' && "Not a dollarident");
  bool AllNumeric = true;
  for (unsigned i = 1, e = Name.size(); i != e; ++i)
    AllNumeric &= isdigit(Name[i]);
  
  if (Name.size() == 1 || !AllNumeric) {
    diagnose(Loc.getAdvancedLoc(1), diag::expected_dollar_numeric);
    return ParseResult<Expr>::getSemaError();
  }
  
  unsigned ArgNo = 0;
  if (Name.substr(1).getAsInteger(10, ArgNo)) {
    diagnose(Loc.getAdvancedLoc(1), diag::dollar_numeric_too_large);
    return ParseResult<Expr>::getSemaError();
  }
  
  return new (Context) AnonClosureArgExpr(ArgNo, Loc);
}

Expr *Parser::actOnIdentifierExpr(Identifier Text, SourceLoc Loc) {
  ValueDecl *D = ScopeInfo.lookupValueName(Text);
  
  if (D == 0)
    return new (Context) UnresolvedDeclRefExpr(Text, Loc);
  
  return new (Context) DeclRefExpr(D, Loc);
}


/// parseExprParen - Parse a tuple expression.
///
///   expr-paren: 
///     lparen-any ')'
///     lparen-any expr-paren-element (',' expr-paren-element)* ')'
///
///   expr-paren-element:
///     (identifier '=')? expr
///
ParseResult<Expr> Parser::parseExprParen() {
  SourceLoc LPLoc = consumeToken();
  
  SmallVector<Expr*, 8> SubExprs;
  SmallVector<Identifier, 8> SubExprNames; 
  bool AnySubExprSemaErrors = false;
  
  if (Tok.isNot(tok::r_paren)) {
    do {
      Identifier FieldName;
      // Check to see if there is a field specifier, like "x =".
      if (Tok.is(tok::identifier) && peekToken().is(tok::equal)) {
        if (parseIdentifier(FieldName,
                            diag::expected_field_spec_name_tuple_expr) ||
            parseToken(tok::equal, diag::expected_equal_in_tuple_expr))
          return true;
      }
      
      if (!SubExprNames.empty())
        SubExprNames.push_back(FieldName);
      else if (FieldName.get()) {
        SubExprNames.resize(SubExprs.size());
        SubExprNames.push_back(FieldName);
      }
      
      ParseResult<Expr> SubExpr;
      if ((SubExpr = parseExpr(diag::expected_expr_parentheses)))
        return true;
      
      if (SubExpr.isSemaError())
        AnySubExprSemaErrors = true;
      else
        SubExprs.push_back(SubExpr.get());
    
    } while (consumeIf(tok::comma));
  }
  
  SourceLoc RPLoc;
  if (parseMatchingToken(tok::r_paren, RPLoc,
                         diag::expected_rparen_parenthesis_expr,
                         LPLoc, diag::opening_paren))
    return true;

  if (AnySubExprSemaErrors)
    return ParseResult<Expr>::getSemaError();

  MutableArrayRef<Expr *> NewSubExprs = Context.AllocateCopy(SubExprs);
  
  Identifier *NewSubExprsNames = 0;
  if (!SubExprNames.empty())
    NewSubExprsNames =
      Context.AllocateCopy<Identifier>(SubExprNames.data(),
                                       SubExprNames.data()+SubExprs.size());
  
  // A tuple with a single, unlabelled element is just parentheses.
  if (SubExprs.size() == 1 &&
      (SubExprNames.empty() || SubExprNames[0].empty())) {
    return new (Context) ParenExpr(LPLoc, SubExprs[0], RPLoc);
  }
  
  return new (Context) TupleExpr(LPLoc, NewSubExprs, NewSubExprsNames, RPLoc);
}


/// parseExprFunc - Parse a func expression.
///
///   expr-func: 
///     'func' func-signature? stmt-brace
///
/// The type must start with '(' if present.
///
ParseResult<Expr> Parser::parseExprFunc() {
  SourceLoc FuncLoc = consumeToken(tok::kw_func);

  SmallVector<Pattern*, 4> Params;
  Type Ty;
  if (Tok.is(tok::l_brace)) {
    Params.push_back(TuplePattern::create(Context, SourceLoc(),
                                          llvm::ArrayRef<TuplePatternElt>(),
                                          SourceLoc()));
    Ty = TupleType::getEmpty(Context);
    Ty = FunctionType::get(Ty, Ty, Context);
  } else if (!Tok.is(tok::l_paren) && !Tok.is(tok::l_paren_space)) {
    diagnose(Tok, diag::func_decl_without_paren);
    return true;
  } else if (parseFunctionSignature(Params, Ty)) {
    return true;
  }
  
  // The arguments to the func are defined in their own scope.
  Scope FuncBodyScope(this);
  FuncExpr *FE = actOnFuncExprStart(FuncLoc, Ty, Params);

  // Establish the new context.
  ContextChange CC(*this, FE);
  
  // Then parse the expression.
  ParseResult<BraceStmt> Body;
  if ((Body = parseStmtBrace(diag::expected_lbrace_func_expr)))
    return true;
  if (Body.isSemaError())
    return ParseResult<Expr>::getSemaError();
  
  FE->setBody(Body.get());
  return FE;
}


/// AddFuncArgumentsToScope - Walk the type specified for a Func object (which
/// is known to be a FunctionType on the outer level) creating and adding named
/// arguments to the current scope.  This causes redefinition errors to be
/// emitted.
static void AddFuncArgumentsToScope(Pattern *pat, FuncExpr *FE, Parser &P) {
  switch (pat->getKind()) {
  case PatternKind::Named: {
    // Reparent the decl and add it to the scope.
    VarDecl *var = cast<NamedPattern>(pat)->getDecl();
    var->setDeclContext(FE);
    P.ScopeInfo.addToScope(var);
    return;
  }

  case PatternKind::Any:
    return;

  case PatternKind::Paren:
    AddFuncArgumentsToScope(cast<ParenPattern>(pat)->getSubPattern(), FE, P);
    return;

  case PatternKind::Typed:
    AddFuncArgumentsToScope(cast<TypedPattern>(pat)->getSubPattern(), FE, P);
    return;

  case PatternKind::Tuple:
    for (const TuplePatternElt &field : cast<TuplePattern>(pat)->getFields())
      AddFuncArgumentsToScope(field.getPattern(), FE, P);
    return;
  }
  llvm_unreachable("bad pattern kind!");
}

FuncExpr *Parser::actOnFuncExprStart(SourceLoc FuncLoc, Type FuncTy,
                                     ArrayRef<Pattern*> Params) {
  FuncExpr *FE = FuncExpr::create(Context, FuncLoc, Params, FuncTy, 0,
                                  CurDeclContext);

  for (Pattern *P : Params)
    AddFuncArgumentsToScope(P, FE, *this);
  
  return FE;
}
