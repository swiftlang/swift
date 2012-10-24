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
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace swift;


/// parseExpr
///   expr:
///     expr-unary expr-binary*
///   expr-binary:
///     operator-binary expr-unary
///
/// The sequencing here is not structural, i.e. binary operators are
/// not inherently right-associative.
NullablePtr<Expr> Parser::parseExpr(Diag<> Message) {
  SmallVector<Expr*, 8> SequencedExprs;

  while (true) {
    // Parse a unary expression.
    auto Primary = parseExprUnary(Message);
    if (Primary.isNull())
      return 0;
    SequencedExprs.push_back(Primary.get());

    // If the next token is not a binary operator, we're done.
    if (!Tok.is(tok::oper_binary))
      break;

    // Parse the operator.
    Expr *Operator = parseExprOperator();
    SequencedExprs.push_back(Operator);

    // The message is only valid for the first subexpr.
    Message = diag::expected_expr_after_operator;
  }

  // If we had semantic errors, just fail here.
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
///     expr-new
///     operator-prefix expr-unary
NullablePtr<Expr> Parser::parseExprUnary(Diag<> Message) {
  // If the next token is the keyword 'new', this must be expr-new.
  if (Tok.is(tok::kw_new))
    return parseExprNew();

  // For recovery purposes, accept an oper_binary here.
  if (Tok.is(tok::oper_binary)) {
    diagnose(Tok.getLoc(), diag::expected_prefix_operator);
    Tok.setKind(tok::oper_prefix);
  }

  // If the next token is not an operator, just parse this as expr-postfix.
  if (Tok.isNot(tok::oper_prefix))
    return parseExprPostfix(Message);

  // '&' is a very special case.
  if (Tok.getText() == "&") {
    SourceLoc loc = Tok.getLoc();
    consumeToken(tok::oper_prefix);

    if (Expr *SubExpr = parseExprUnary(Message).getPtrOrNull())
      return new (Context) AddressOfExpr(loc, SubExpr, Type());
    return 0;
  }
  
  // Parse the operator.
  Expr *Operator = parseExprOperator();

  if (Expr *SubExpr = parseExprUnary(Message).getPtrOrNull())
    return new (Context) PrefixUnaryExpr(Operator, SubExpr);
  return 0;
}

static DeclRefKind getDeclRefKindForOperator(tok kind) {
  switch (kind) {
  case tok::oper_binary:  return DeclRefKind::BinaryOperator;
  case tok::oper_postfix: return DeclRefKind::PostfixOperator;
  case tok::oper_prefix:  return DeclRefKind::PrefixOperator;
  default: llvm_unreachable("bad operator token kind");
  }
}

/// parseExprOperator - Parse an operator reference expression.  These
/// are not "proper" expressions; they can only appear in binary/unary
/// operators.
Expr *Parser::parseExprOperator() {
  assert(Tok.isAnyOperator());
  DeclRefKind refKind = getDeclRefKindForOperator(Tok.getKind());
  SourceLoc loc = Tok.getLoc();
  Identifier name = Context.getIdentifier(Tok.getText());
  consumeToken();

  // Bypass local lookup.
  return new (Context) UnresolvedDeclRefExpr(name, refKind, loc);
}

/// parseExprNew
///
///   expr-new:
///     'new' type-identifier expr-new-bounds?
///   expr-new-bounds:
///     expr-new-bound
///     expr-new-bounds expr-new-bound
///   expr-new-bound:
///     lsquare-unspaced expr ']'
NullablePtr<Expr> Parser::parseExprNew() {
  SourceLoc newLoc = Tok.getLoc();
  consumeToken(tok::kw_new);

  // FIXME: this should probably be type-simple.
  TypeLoc elementTy;
  if (parseTypeIdentifier(elementTy))
    return nullptr;

  bool hadInvalid = false;
  SmallVector<NewArrayExpr::Bound, 4> bounds;
  while (Tok.is(tok::l_square)) {
    SourceRange brackets;
    brackets.Start = Tok.getLoc();
    consumeToken(tok::l_square);

    // If the bound is missing, that's okay unless this is the first bound.
    if (Tok.is(tok::r_square)) {
      if (bounds.empty()) {
        diagnose(Tok.getLoc(), diag::array_new_missing_first_bound);
        hadInvalid = true;
      }

      brackets.End = Tok.getLoc();
      consumeToken(tok::r_square);
      bounds.push_back(NewArrayExpr::Bound(nullptr, brackets));
      continue;
    }

    auto boundValue = parseExpr(diag::expected_expr_new_array_bound);
    if (boundValue.isNull() || !Tok.is(tok::r_square)) {
      if (!boundValue.isNull())
        diagnose(Tok.getLoc(), diag::expected_bracket_array_new);

      skipUntil(tok::r_square);
      if (!Tok.is(tok::r_square)) return nullptr;
      hadInvalid = true;
    }

    brackets.End = Tok.getLoc();
    consumeToken(tok::r_square);

    bounds.push_back(NewArrayExpr::Bound(boundValue.get(), brackets));
  }

  if (hadInvalid) return nullptr;

  if (bounds.empty()) {
    NullablePtr<Expr> Init;
    if (Tok.is(tok::l_paren)) {
      Init = parseExprParen();
      if (Init.isNull())
        return nullptr;
    }
    return new (Context) NewReferenceExpr(elementTy, newLoc,
                                          Init.getPtrOrNull());
  }

  // TODO: we allow a tuple-expr here as an initializer?
  if (Tok.is(tok::l_paren)) {
    diagnose(newLoc, diag::array_new_init_unsupported);
    return nullptr;
  }

  return NewArrayExpr::create(Context, newLoc, elementTy, bounds);
}

/// parseExprPostfix
///
///   expr-literal:
///     integer_literal
///     floating_literal
///     string_literal
///     character_literal
///
///   expr-primary:
///     expr-literal
///     expr-identifier
///     expr-explicit-closure
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
///     expr-metatype
///     expr-subscript
///     expr-call
///     expr-postfix operator-postfix
///
NullablePtr<Expr> Parser::parseExprPostfix(Diag<> ID) {
  NullablePtr<Expr> Result;
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
  case tok::character_literal: {
    uint32_t Codepoint = L->getEncodedCharacterLiteral(Tok);
    SourceLoc Loc = consumeToken(tok::character_literal);
    Result = new (Context) CharacterLiteralExpr(Codepoint, Loc);
    break;
  }
  case tok::string_literal:  // "foo"
    Result = parseExprStringLiteral();
    break;
  case tok::identifier:  // foo
    Result = parseExprIdentifier();
    break;
  case tok::dollarident: // $1
    Result = parseExprAnonClosureArg();
    break;

  case tok::l_brace:     // { expr }
    Result = parseExprExplicitClosure();
    break;

  case tok::period: {     // .foo
    SourceLoc DotLoc = consumeToken(tok::period);
    Identifier Name;
    SourceLoc NameLoc = Tok.getLoc();
    if (parseIdentifier(Name, diag::expected_identifier_after_dot_expr))
      return 0;
    
    // Handle .foo by just making an AST node.
    Result = new (Context) UnresolvedMemberExpr(DotLoc, NameLoc, Name);
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
      
  // Eat an invalid token in an expression context.  Error tokens are diagnosed
  // by the lexer, so there is no reason to emit another diagnostic.
  case tok::unknown:
    consumeToken(tok::unknown);
    return 0;

  default:
    diagnose(Tok.getLoc(), ID);
    return 0;
  }
  
  // If we had a parse error, don't attempt to parse suffixes.
  if (Result.isNull())
    return 0;
    
  // Handle suffix expressions.
  while (1) {
    // Check for a .foo suffix.
    SourceLoc TokLoc = Tok.getLoc();
    
    if (consumeIf(tok::period)) {
      if (Tok.is(tok::kw_metatype)) {
        SourceLoc metatypeLoc = consumeToken(tok::kw_metatype);
        Result = new (Context) MetatypeExpr(Result.get(), metatypeLoc, Type());
        continue;
      }

      if (Tok.isNot(tok::identifier) && Tok.isNot(tok::dollarident)) {
        diagnose(Tok, diag::expected_field_name);
        return 0;
      }
        
      Identifier Name = Context.getIdentifier(Tok.getText());
      Result = new (Context) UnresolvedDotExpr(Result.get(), TokLoc, Name,
                                               Tok.getLoc());
      if (Tok.is(tok::identifier))
        consumeToken(tok::identifier);
      else
        consumeToken(tok::dollarident);
      continue;
    }
    
    // Check for a () suffix, which indicates a call.
    // Note that this cannot be a l_paren_space.
    if (Tok.is(tok::l_paren)) {
      NullablePtr<Expr> Arg = parseExprParen();
      if (Arg.isNull())
        return 0;
      Result = new (Context) CallExpr(Result.get(), Arg.get());
      continue;
    }
    
    // Check for a [expr] suffix.
    // Note that this cannot be a l_square_space.
    if (Tok.is(tok::l_square)) {
      SourceLoc LLoc = consumeToken();
      NullablePtr<Expr> Idx = parseExpr(diag::expected_expr_subscript_value);
      SourceLoc RLoc;
      if (Idx.isNull() ||
          parseMatchingToken(tok::r_square, RLoc,
                             diag::expected_bracket_array_subscript,
                             TokLoc, diag::opening_bracket))
        return 0;
      
      Result = new (Context) SubscriptExpr(Result.get(), LLoc, Idx.get(), RLoc);
      continue;
    }

    // Check for a postfix-operator suffix.
    if (Tok.is(tok::oper_postfix)) {
      Expr *oper = parseExprOperator();
      Result = new (Context) PostfixUnaryExpr(oper, Result.get());
      continue;
    }
        
    break;
  }
  
  return Result;
}

///   expr-literal:
///     string_literal
Expr *Parser::parseExprStringLiteral() {
  llvm::SmallVector<Lexer::StringSegment, 1> Segments;
  L->getEncodedStringLiteral(Tok, Context, Segments);
  SourceLoc Loc = consumeToken();
    
  // The simple case: just a single literal segment.
  if (Segments.size() == 1 &&
      Segments.front().Kind == Lexer::StringSegment::Literal)
    return new (Context) StringLiteralExpr(Segments.front().Data, Loc);
    
  // We are going to mess with Tok to do reparsing for interpolated literals,
  // don't lose our 'next' token.
  llvm::SaveAndRestore<Token> SavedTok(Tok);
  llvm::SmallVector<Expr*, 4> Exprs;
  for (auto Segment : Segments) {
    switch (Segment.Kind) {
    case Lexer::StringSegment::Literal: {
      SourceLoc Loc(llvm::SMLoc::getFromPointer(Segment.Data.data()));
      Exprs.push_back(new (Context) StringLiteralExpr(Segment.Data, Loc));
      break;
    }
        
    case Lexer::StringSegment::Expr: {
      // Create a temporary lexer that lexes from the body of the string.
      Lexer LocalLex(Segment.Data, SourceMgr, &Diags);
      
      // Temporarily swap out the parser's current lexer with our new one.
      llvm::SaveAndRestore<Lexer*> T(L, &LocalLex);
      
      // Prime the new lexer with a '(' as the first token.
      assert(Segment.Data.data()[-1] == '(' &&
             "Didn't get an lparen before interpolated expression");
      Tok.setToken(tok::l_paren, StringRef(Segment.Data.data()-1, 1));
      
      NullablePtr<Expr> E = parseExprParen();
      if (E.isNonNull()) {
        Exprs.push_back(E.get());
        
        if (!Tok.is(tok::eof))
          diagnose(Tok, diag::string_interpolation_extra);
      }
      break;
    }
    }
  }
  
  if (Exprs.empty())
    return new (Context) ErrorExpr(Loc);
  
  return new (Context) InterpolatedStringLiteralExpr(Loc,
                                        Context.AllocateCopy(Exprs));
}
  
///   expr-identifier:
///     identifier
Expr *Parser::parseExprIdentifier() {
  assert(Tok.is(tok::identifier));
  SourceLoc Loc = Tok.getLoc();
  Identifier Name = Context.getIdentifier(Tok.getText());
  consumeToken(tok::identifier);
  return actOnIdentifierExpr(Name, Loc);
}

///   expr-explicit-closure:
///     '{' expr? '}'
NullablePtr<Expr> Parser::parseExprExplicitClosure() {
  SourceLoc LBLoc = consumeToken(tok::l_brace);
  
  ExplicitClosureExpr *ThisClosure =
      new (Context) ExplicitClosureExpr(LBLoc, CurDeclContext);

  ContextChange CC(*this, ThisClosure);
  AnonClosureVars.emplace_back();

  NullablePtr<Expr> Body;
  if (Tok.isNot(tok::r_brace)) {
    Body = parseExpr(diag::expected_expr_closure);
    if (Body.isNull()) return 0;
  } else {
    Body = new (Context) TupleExpr(LBLoc, MutableArrayRef<Expr *>(), 0, LBLoc);
  }
  ThisClosure->setBody(Body.get());
  
  SourceLoc RBLoc;
  if (parseMatchingToken(tok::r_brace, RBLoc,
                         diag::expected_rbrace_in_closure,
                         LBLoc, diag::opening_brace))
    RBLoc = Body.get()->getEndLoc();

  auto& Captures = ValCaptures.back();
  ValueDecl** CaptureCopy = Context.AllocateCopy<ValueDecl*>(Captures.begin(),
                                                             Captures.end());
  ThisClosure->setCaptures(llvm::makeArrayRef(CaptureCopy, Captures.size()));

  ThisClosure->setRBraceLoc(RBLoc);

  auto& Vars = AnonClosureVars.back();
  VarDecl** VarsCopy = Context.AllocateCopy<VarDecl*>(Vars.begin(), Vars.end());
  ThisClosure->setParserVarDecls(llvm::makeArrayRef(VarsCopy, Vars.size()));
  AnonClosureVars.pop_back();

  return ThisClosure;
}

///   expr-anon-closure-argument:
///     dollarident
Expr *Parser::parseExprAnonClosureArg() {
  StringRef Name = Tok.getText();
  SourceLoc Loc = consumeToken(tok::dollarident);
  assert(Name[0] == '$' && "Not a dollarident");
  bool AllNumeric = true;
  for (unsigned i = 1, e = Name.size(); i != e; ++i)
    AllNumeric &= isdigit(Name[i]);
  
  if (Name.size() == 1 || !AllNumeric) {
    diagnose(Loc.getAdvancedLoc(1), diag::expected_dollar_numeric);
    return new (Context) ErrorExpr(Loc);
  }
  
  unsigned ArgNo = 0;
  if (Name.substr(1).getAsInteger(10, ArgNo)) {
    diagnose(Loc.getAdvancedLoc(1), diag::dollar_numeric_too_large);
    return new (Context) ErrorExpr(Loc);
  }

  // Make sure that this is located in an explicit closure expression.
  ExplicitClosureExpr *ECE = dyn_cast<ExplicitClosureExpr>(CurDeclContext);
  if (!ECE) {
    diagnose(Loc, diag::anon_closure_arg_not_in_closure);
    return new (Context) ErrorExpr(Loc);
  }

  ECE->GenerateVarDecls(ArgNo, AnonClosureVars.back(), Context);

  return new (Context) DeclRefExpr(AnonClosureVars.back()[ArgNo], Loc);
}

Expr *Parser::actOnIdentifierExpr(Identifier text, SourceLoc loc) {
  ValueDecl *D = ScopeInfo.lookupValueName(text);
  
  if (D == 0) {
    auto refKind = DeclRefKind::Ordinary;
    return new (Context) UnresolvedDeclRefExpr(text, refKind, loc);
  }

  // Compute captures for local value declaration.
  if (D->needsCapture()) {
    DeclContext *ValDC = D->getDeclContext();
    DeclContext *DC = CurDeclContext;
    unsigned i = ValCaptures.size();
    while (DC != ValDC) {
      ValCaptures[--i].insert(D);
      DC = DC->getParent();
    }
  }
  return new (Context) DeclRefExpr(D, loc);
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
NullablePtr<Expr> Parser::parseExprParen() {
  SourceLoc LPLoc = consumeToken();
  
  SmallVector<Expr*, 8> SubExprs;
  SmallVector<Identifier, 8> SubExprNames; 
  
  if (Tok.isNot(tok::r_paren)) {
    do {
      Identifier FieldName;
      // Check to see if there is a field specifier, like "x =".
      if (Tok.is(tok::identifier) && peekToken().is(tok::equal)) {
        if (parseIdentifier(FieldName,
                            diag::expected_field_spec_name_tuple_expr) ||
            parseToken(tok::equal, diag::expected_equal_in_tuple_expr))
          return 0;
      }
      
      if (!SubExprNames.empty())
        SubExprNames.push_back(FieldName);
      else if (FieldName.get()) {
        SubExprNames.resize(SubExprs.size());
        SubExprNames.push_back(FieldName);
      }
      
      NullablePtr<Expr> SubExpr = parseExpr(diag::expected_expr_parentheses);
      if (SubExpr.isNull())
        return 0;
      SubExprs.push_back(SubExpr.get());
    } while (consumeIf(tok::comma));
  }
  
  
  // Check to see if the lexer stopped with an EOF token whose spelling is ')'.
  // If this happens, then this is actually the tuple that is a string literal
  // interpolation context.  Just accept the ) and build the tuple as we usually
  // do.
  SourceLoc RPLoc;
  if (Tok.is(tok::eof) && Tok.getText()[0] == ')')
    RPLoc = Tok.getLoc();
  else {
    if (parseMatchingToken(tok::r_paren, RPLoc,
                           diag::expected_rparen_parenthesis_expr,
                           LPLoc, diag::opening_paren))
      return 0;
  }

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
NullablePtr<Expr> Parser::parseExprFunc() {
  SourceLoc FuncLoc = consumeToken(tok::kw_func);

  SmallVector<Pattern*, 4> Params;
  TypeLoc RetTy;
  if (Tok.is(tok::l_brace)) {
    // If the func-signature isn't present, then this is a ()->Unresolved
    // function.
    Params.push_back(TuplePattern::create(Context, SourceLoc(),
                                          llvm::ArrayRef<TuplePatternElt>(),
                                          SourceLoc()));
  } else if (Tok.isNotAnyLParen()) {
    diagnose(Tok, diag::func_decl_without_paren);
    return 0;
  } else if (parseFunctionSignature(Params, RetTy)) {
    return 0;
  }
  
  // The arguments to the func are defined in their own scope.
  Scope FuncBodyScope(this, /*AllowLookup=*/true);
  FuncExpr *FE = actOnFuncExprStart(FuncLoc, RetTy, Params);

  // Establish the new context.
  ContextChange CC(*this, FE);
  
  // Then parse the expression.
  NullablePtr<BraceStmt> Body = parseStmtBrace(diag::expected_lbrace_func_expr);
  if (Body.isNull())
    return 0;

  FE->setBody(Body.get());

  auto& Captures = ValCaptures.back();
  ValueDecl** CaptureCopy = Context.AllocateCopy<ValueDecl*>(Captures.begin(),
                                                             Captures.end());
  FE->setCaptures(llvm::makeArrayRef(CaptureCopy, Captures.size()));

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

FuncExpr *Parser::actOnFuncExprStart(SourceLoc FuncLoc, TypeLoc FuncRetTy, 
                                     ArrayRef<Pattern*> Params) {
  FuncExpr *FE = FuncExpr::create(Context, FuncLoc, Params, FuncRetTy,
                                  nullptr, CurDeclContext);

  for (Pattern *P : Params)
    AddFuncArgumentsToScope(P, FE, *this);
  
  return FE;
}
