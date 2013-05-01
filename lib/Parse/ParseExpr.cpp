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
#include "llvm/Support/raw_ostream.h"

using namespace swift;

/// parseExpr
///   expr:
///     expr-sequence
///     expr-sequence expr-is
///     expr-sequence expr-as
///
NullablePtr<Expr> Parser::parseExpr(Diag<> Message) {
  NullablePtr<Expr> expr = parseExprSequence(Message);
  if (expr.isNull())
    return nullptr;
  
  if (Tok.is(tok::kw_is)) {
    return parseExprIs(expr.get());
  }
  if (Tok.is(tok::kw_as)) {
    return parseExprAs(expr.get());
  }
  
  return expr;
}

/// parseExprIs
///   expr-is:
///     'is' type
NullablePtr<Expr> Parser::parseExprIs(Expr *sub) {
  SourceLoc isLoc = consumeToken(tok::kw_is);
  
  TypeLoc type;
  if (parseType(type, diag::expected_type_after_is))
    return nullptr;
  
  return new (Context) IsSubtypeExpr(sub, isLoc, type);
}

/// parseExprAs
///   expr-as:
///     'as' type
///     'as' '!' type
NullablePtr<Expr> Parser::parseExprAs(Expr *sub) {
  SourceLoc asLoc = consumeToken(tok::kw_as);
  SourceLoc bangLoc;
  
  if (Tok.isContextualPunctuator("!")) {
    bangLoc = consumeToken();
  }
  
  TypeLoc type;
  if (parseType(type, diag::expected_type_after_as))
    return nullptr;
  
  return bangLoc.isValid()
    ? (Expr*) new (Context) UncheckedDowncastExpr(sub, asLoc, bangLoc, type)
    : (Expr*) new (Context) CoerceExpr(sub, asLoc, type);
}

/// parseExprSequence
///
///   expr-sequence:
///     expr-unary expr-binary*
///   expr-binary:
///     operator-binary expr-unary
///     '?' expr-unary
///     ':' expr-unary
///
/// The sequencing for binary exprs is not structural, i.e., binary operators
/// are not inherently right-associative. If present, '?' and ':' tokens must
/// match.
NullablePtr<Expr> Parser::parseExprSequence(Diag<> Message) {
  SmallVector<Expr*, 8> SequencedExprs;
  SmallVector<UnresolvedIfExpr*, 2> UnmatchedIfs;

  while (true) {
    // Parse a unary expression.
    auto Primary = parseExprUnary(Message);
    if (Primary.isNull())
      return 0;
    SequencedExprs.push_back(Primary.get());

    switch (Tok.getKind()) {
    case tok::oper_binary: {
      // Parse the operator.
      Expr *Operator = parseExprOperator();
      SequencedExprs.push_back(Operator);
      
      // The message is only valid for the first subexpr.
      Message = diag::expected_expr_after_operator;
      break;
    }
    
    case tok::question: {
      // Save the '?'.
      auto *unresolvedIf = new (Context) UnresolvedIfExpr(consumeToken());
      SequencedExprs.push_back(unresolvedIf);
      UnmatchedIfs.push_back(unresolvedIf);
      
      Message = diag::expected_expr_after_if_question;
      break;
    }

    case tok::colon: {
      // If there's no preceding '?', this isn't a ternary colon. We're done.
      if (UnmatchedIfs.empty())
        goto done;
      UnmatchedIfs.pop_back();

      // Save the ':'.
      auto *unresolvedElse = new (Context) UnresolvedElseExpr(consumeToken());
      SequencedExprs.push_back(unresolvedElse);

      
      Message = diag::expected_expr_after_if_colon;
      break;
    }
        
    default:
      // If the next token is not a binary operator, we're done.
      goto done;
    }
  }
done:
  
  // If we had semantic errors, just fail here.
  assert(!SequencedExprs.empty());

  // If we found invalid ternaries, return an error expr.
  if (!UnmatchedIfs.empty()) {
    for (auto *unmatchedIf : UnmatchedIfs) {
      diagnose(unmatchedIf->getLoc(), diag::expected_colon_after_if_question);
    }
    return new (Context) ErrorExpr({SequencedExprs.front()->getStartLoc(),
                                    SequencedExprs.back()->getEndLoc()});
  }

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
  
  if (Tok.is(tok::amp_prefix)) {
    SourceLoc Loc = consumeToken(tok::amp_prefix);

    if (Expr *SubExpr = parseExprUnary(Message).getPtrOrNull())
      return new (Context) AddressOfExpr(Loc, SubExpr, Type());
    return 0;
  }

  Expr *Operator;
  switch (Tok.getKind()) {
  default:
    // If the next token is not an operator, just parse this as expr-postfix.
    return parseExprPostfix(Message);
  case tok::oper_prefix:
    Operator = parseExprOperator();
    break;
  case tok::oper_binary: {
    // For recovery purposes, accept an oper_binary here.
    SourceLoc OperEndLoc = Tok.getLoc().getAdvancedLoc(Tok.getLength());
    Tok.setKind(tok::oper_prefix);
    Operator = parseExprOperator();

    assert(OperEndLoc != Tok.getLoc() && "binary operator with no spaces?");
    diagnose(PreviousLoc, diag::expected_prefix_operator)
      .fixItRemove(Diagnostic::Range(OperEndLoc,
                                                           Tok.getLoc()));
    break;
  }
  case tok::oper_postfix:
    llvm_unreachable("oper_postfix should not appear here");
  }

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
///     'new' type-identifier expr-new-bounds
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
  while (Tok.isFollowingLSquare()) {
    SourceRange brackets;
    brackets.Start = consumeToken(tok::l_square);

    // If the bound is missing, that's okay unless this is the first bound.
    if (Tok.is(tok::r_square)) {
      if (bounds.empty()) {
        diagnose(Tok.getLoc(), diag::array_new_missing_first_bound);
        hadInvalid = true;
      }

      brackets.End = consumeToken(tok::r_square);
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

    brackets.End = consumeToken(tok::r_square);

    bounds.push_back(NewArrayExpr::Bound(boundValue.get(), brackets));
  }

  if (hadInvalid) return nullptr;

  if (bounds.empty()) {
    diagnose(newLoc, diag::expected_bracket_array_new);
    return new (Context) ErrorExpr({newLoc, PreviousLoc});
  }

  return NewArrayExpr::create(Context, newLoc, elementTy, bounds);
}

#include "llvm/Support/raw_ostream.h"

static VarDecl *getImplicitThisDeclForSuperContext(Parser &P,
                                                   DeclContext *dc,
                                                   SourceLoc loc) {
  if (ConstructorDecl *ctor = dyn_cast<ConstructorDecl>(dc)) {
    return ctor->getImplicitThisDecl();
  } else if (DestructorDecl *dtor = dyn_cast<DestructorDecl>(dc)) {
    return dtor->getImplicitThisDecl();
  } else if (FuncExpr *fe = dyn_cast<FuncExpr>(dc)) {
    auto thisDecl = fe->getImplicitThisDecl();
    if (thisDecl)
      return thisDecl;
  }
  P.diagnose(loc, diag::super_not_in_class_method);
  return nullptr;
}

/// parseExprSuper
///
///   expr-super:
///     expr-super-member
///     expr-super-constructor
///     expr-super-subscript
///   expr-super-member:
///     'super' '.' identifier
///   expr-super-constructor:
///     'super' '.' 'constructor' 
///     'super' '.' 'constructor' '.' selector-args
///   expr-super-subscript:
///     'super' '[' expr ']'
NullablePtr<Expr> Parser::parseExprSuper() {
  // Parse the 'super' reference.
  SourceLoc superLoc = consumeToken(tok::kw_super);
  
  VarDecl *thisDecl = getImplicitThisDeclForSuperContext(*this,
                                                         CurDeclContext,
                                                         superLoc);
  Expr *superRef = thisDecl
    ? cast<Expr>(new (Context) SuperRefExpr(thisDecl, superLoc))
    : cast<Expr>(new (Context) ErrorExpr(superLoc));
  
  if (Tok.is(tok::period)) {
    // 'super.' must be followed by a member or constructor ref.

    SourceLoc dotLoc = consumeToken(tok::period);
    
    if (Tok.is(tok::kw_constructor)) {
      // super.constructor
      SourceLoc ctorLoc = consumeToken(tok::kw_constructor);
      
      // Check that we're actually in a constructor.
      if (!isa<ConstructorDecl>(CurDeclContext)) {
        diagnose(ctorLoc, diag::super_constructor_not_in_constructor);
        return new (Context) ErrorExpr(SourceRange(superLoc, ctorLoc),
                                       ErrorType::get(Context));
      }
      // The constructor decl will be resolved by sema.
      Expr *result = new (Context) UnresolvedConstructorExpr(superRef,
                                                             dotLoc, ctorLoc);
      if (Tok.isFollowingLParen()) {
        // Parse Swift-style constructor arguments.
        NullablePtr<Expr> arg = parseExprList(tok::l_paren, tok::r_paren);
        // FIXME: Unfortunate recovery here.
        if (arg.isNull())
          return nullptr;
        
        result = new (Context) CallExpr(result, arg.get());
      } // It's invalid to refer to an uncalled constructor.
        else {
        diagnose(ctorLoc, diag::super_constructor_must_be_called);
        result->setType(ErrorType::get(Context));
        return result;
      }

      // The result of the called constructor is used to rebind 'this'.
      return new (Context) RebindThisInConstructorExpr(result, thisDecl);
    } else {
      // super.foo
      SourceLoc nameLoc = Tok.getLoc();
      Identifier name;
      if (parseIdentifier(name, diag::expected_identifier_after_super_dot_expr))
        return nullptr;
      
      if (!thisDecl)
        return new (Context) ErrorExpr(SourceRange(superLoc, nameLoc),
                                       ErrorType::get(Context));
      
      return new (Context) UnresolvedDotExpr(superRef, dotLoc,
                                             name, nameLoc);
    }
  } else if (Tok.isFollowingLSquare()) {
    // super[expr]
    NullablePtr<Expr> idx = parseExprList(tok::l_square,
                                          tok::r_square);
    if (idx.isNull())
      return 0;
    return new (Context) SubscriptExpr(superRef, idx.get());
  } else {
    diagnose(superLoc, diag::expected_dot_or_subscript_after_super);
    return nullptr;
  }
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
///     expr-super
///
///   expr-delayed-identifier:
///     ':' identifier
///
///   expr-dot:
///     expr-postfix '.' identifier generic-args?
///     expr-postfix '.' integer_literal
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
  case tok::kw_this:     // this
  case tok::identifier:  // foo
    Result = parseExprIdentifier();
    break;
  case tok::dollarident: // $1
    Result = parseExprAnonClosureArg();
    break;

  case tok::l_brace:     // { expr }
    Result = parseExprExplicitClosure();
    break;

  case tok::period_prefix: {     // .foo
    SourceLoc DotLoc = consumeToken(tok::period_prefix);
    Identifier Name;
    SourceLoc NameLoc = Tok.getLoc();
    if (parseIdentifier(Name, diag::expected_identifier_after_dot_expr))
      return 0;
    
    // Handle .foo by just making an AST node.
    Result = new (Context) UnresolvedMemberExpr(DotLoc, NameLoc, Name);
    break;
  }
      
  case tok::kw_super: {      // super.foo or super[foo]
    Result = parseExprSuper();
    break;
  }

  case tok::l_paren:
    Result = parseExprList(tok::l_paren, tok::r_paren);
    break;

  case tok::l_square:
    Result = parseExprCollection();
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
    
    if (Tok.is(tok::period_prefix) && (peekToken().is(tok::identifier) ||
                                       peekToken().is(tok::integer_literal))) {
      auto Backup = Tok;
      consumeToken();
      bool IsPeriod = peekToken().isFollowingLParen() ||
                      peekToken().isFollowingLSquare();
      Tok = Backup;
      L->backtrackToToken(Backup);
      if (IsPeriod)
        Tok.setKind(tok::period);
    }
    if (consumeIf(tok::period)) {
      if (Tok.isNot(tok::identifier) && Tok.isNot(tok::integer_literal)) {
        diagnose(Tok, diag::expected_field_name);
        return 0;
      }
        
      Identifier Name = Context.getIdentifier(Tok.getText());
      Result = new (Context) UnresolvedDotExpr(Result.get(), TokLoc, Name,
                                               Tok.getLoc());
      if (Tok.is(tok::identifier)) {
        consumeToken(tok::identifier);
        if (canParseAsGenericArgumentList()) {
          MutableArrayRef<TypeLoc> args;
          SourceLoc LAngleLoc, RAngleLoc;
          if (parseGenericArguments(args, LAngleLoc, RAngleLoc)) {
            diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);
          }
          
          Result = new (Context) UnresolvedSpecializeExpr(Result.get(),
                                                          LAngleLoc,
                                                          args,
                                                          RAngleLoc);
        }
      } else
        consumeToken(tok::integer_literal);
      
      continue;
    }
    
    // Check for a () suffix, which indicates a call.
    // Note that this cannot be the start of a new line.
    if (Tok.isFollowingLParen()) {
      NullablePtr<Expr> Arg =parseExprList(tok::l_paren, tok::r_paren);
      if (Arg.isNull())
        return 0;
      Result = new (Context) CallExpr(Result.get(), Arg.get());
      continue;
    }
    
    // Check for a [expr] suffix.
    // Note that this cannot be the start of a new line.
    if (Tok.isFollowingLSquare()) {
      NullablePtr<Expr> Idx = parseExprList(tok::l_square,
                                            tok::r_square);
      if (Idx.isNull())
        return 0;
      Result = new (Context) SubscriptExpr(Result.get(), Idx.get());
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
      Lexer LocalLex(Segment.Data, SourceMgr, &Diags, nullptr /*not SIL*/);
      
      // Temporarily swap out the parser's current lexer with our new one.
      llvm::SaveAndRestore<Lexer*> T(L, &LocalLex);
      
      // Prime the new lexer with a '(' as the first token.
      assert(Segment.Data.data()[-1] == '(' &&
             "Didn't get an lparen before interpolated expression");
      Tok.setToken(tok::l_paren, StringRef(Segment.Data.data()-1, 1));
      
      NullablePtr<Expr> E = parseExprList(tok::l_paren, tok::r_paren);
      if (E.isNonNull()) {
        Exprs.push_back(E.get());
        
        assert(Tok.is(tok::eof) && "segment did not end at close paren");
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
///     identifier generic-args?
///   The generic-args case is ambiguous with an expression involving '<'
///   and '>' operators. The operator expression is favored unless a generic
///   argument list can be successfully parsed, and the closing bracket is
///   followed by one of these tokens:
///     lparen_following rparen lsquare_following rsquare lbrace rbrace
///     period_following comma semicolon
///
Expr *Parser::parseExprIdentifier() {
  assert(Tok.is(tok::identifier) || Tok.is(tok::kw_this));
  SourceLoc Loc = Tok.getLoc();
  Identifier Name = Context.getIdentifier(Tok.getText());
  consumeToken();
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
                         diag::expected_rbrace_in_closure, LBLoc))
    RBLoc = Body.get()->getEndLoc();

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
  MutableArrayRef<TypeLoc> args;
  SourceLoc LAngleLoc, RAngleLoc;
  bool hasGenericArgumentList = false;
  if (canParseAsGenericArgumentList()) {
    hasGenericArgumentList = true;
    if (parseGenericArguments(args, LAngleLoc, RAngleLoc)) {
      diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);
    }
  }
  
  ValueDecl *D = ScopeInfo.lookupValueName(text);
  Expr *E;
  if (D == 0) {
    auto refKind = DeclRefKind::Ordinary;
    E = new (Context) UnresolvedDeclRefExpr(text, refKind, loc);
  } else {
    E = new (Context) DeclRefExpr(D, loc);
  }
  
  if (hasGenericArgumentList) {
    E = new (Context) UnresolvedSpecializeExpr(E, LAngleLoc, args, RAngleLoc);
  }
  return E;
}


/// parseExprList - Parse a list of expressions.
///
///   expr-paren:
///     lparen-any ')'
///     lparen-any binary-op ')'
///     lparen-any expr-paren-element (',' expr-paren-element)* ')'
///
///   expr-paren-element:
///     (identifier ':')? expr
///
NullablePtr<Expr> Parser::parseExprList(tok LeftTok, tok RightTok) {
  SourceLoc RLoc, LLoc = consumeToken(LeftTok);

  SmallVector<Expr*, 8> SubExprs;
  SmallVector<Identifier, 8> SubExprNames;

  bool Invalid = parseList(RightTok, LLoc, RLoc,
                           tok::comma, /*OptionalSep=*/false,
                           RightTok == tok::r_paren ?
                           diag::expected_rparen_expr_list :
                           diag::expected_rsquare_expr_list,
                           [&] () -> bool {
    Identifier FieldName;
    // Check to see if there is a field specifier
    if (Tok.is(tok::identifier) && peekToken().is(tok::colon)) {
      if (parseIdentifier(FieldName,
                          diag::expected_field_spec_name_tuple_expr)) {
        return true;
      }
      consumeToken(tok::colon);
    }

    if (!SubExprNames.empty()) {
      SubExprNames.push_back(FieldName);
    } else if (FieldName.get()) {
      SubExprNames.resize(SubExprs.size());
      SubExprNames.push_back(FieldName);
    }

    // See if we have an operator decl ref '(<op>)'. The operator token in
    // this case lexes as a binary operator because it neither leads nor
    // follows a proper subexpression.
    if (Tok.is(tok::oper_binary) &&
        (peekToken().is(RightTok) || peekToken().is(tok::comma))) {
      SourceLoc Loc = Tok.getLoc();
      Identifier OperName;
      if (parseAnyIdentifier(OperName, diag::expected_operator_ref)) {
        return true;
      }
      // Bypass local lookup. Use an 'Ordinary' reference kind so that the
      // reference may resolve to any unary or binary operator based on
      // context.
      auto *SubExpr = new(Context) UnresolvedDeclRefExpr(OperName,
                                                         DeclRefKind::Ordinary,
                                                         Loc);
      SubExprs.push_back(SubExpr);
    } else {
      NullablePtr<Expr> SubExpr = parseExpr(diag::expected_expr_in_expr_list);
      if (SubExpr.isNull()) {
        return true;
      }
      SubExprs.push_back(SubExpr.get());
    }
    return false;
  });

  if (Invalid) return nullptr;

  MutableArrayRef<Expr *> NewSubExprs = Context.AllocateCopy(SubExprs);
  
  Identifier *NewSubExprsNames = 0;
  if (!SubExprNames.empty())
    NewSubExprsNames =
      Context.AllocateCopy<Identifier>(SubExprNames.data(),
                                       SubExprNames.data()+SubExprs.size());
  
  // A tuple with a single, unlabelled element is just parentheses.
  if (SubExprs.size() == 1 &&
      (SubExprNames.empty() || SubExprNames[0].empty())) {
    return new (Context) ParenExpr(LLoc, SubExprs[0], RLoc);
  }

  return new (Context) TupleExpr(LLoc, NewSubExprs, NewSubExprsNames, RLoc);
}

/// parseExprCollection - Parse a collection literal expression.
///
///   expr-collection:
///     expr-array
///     expr-dictionary
//      lsquare-starting ']'
NullablePtr<Expr> Parser::parseExprCollection() {
  SourceLoc LSquareLoc = consumeToken(tok::l_square);

  // Parse an empty collection literal.
  if (Tok.is(tok::r_square)) {
    // FIXME: We want a special 'empty collection' literal kind.
    SourceLoc RSquareLoc = consumeToken();
    return new (Context) TupleExpr(RSquareLoc, { }, nullptr, RSquareLoc);
  }

  // Parse the first expression.
  NullablePtr<Expr> FirstExpr
    = parseExpr(diag::expected_expr_in_collection_literal);
  if (FirstExpr.isNull()) {
    skipUntil(tok::r_square);
    if (Tok.is(tok::r_square))
      consumeToken();
    return 0;
  }

  // If we have a ':', this is a dictionary literal.
  if (Tok.is(tok::colon)) {
    return parseExprDictionary(LSquareLoc, FirstExpr.get());
  }

  // Otherwise, we have an array literal.
  return parseExprArray(LSquareLoc, FirstExpr.get());
}

/// parseExprArray - Parse an array literal expression.
///
/// The lsquare-starting and first expression have already been
/// parsed, and are passed in as parameters.
///
///   expr-array:
///     lsquare-starting expr (',' expr)* ']'
NullablePtr<Expr> Parser::parseExprArray(SourceLoc LSquareLoc,
                                         Expr *FirstExpr) {
  SmallVector<Expr *, 8> SubExprs;
  SubExprs.push_back(FirstExpr);
  SourceLoc RSquareLoc;
  bool Invalid = false;

  if (Tok.isNot(tok::r_square) && !consumeIf(tok::comma)) {
    SourceLoc InsertLoc = Lexer::getLocForEndOfToken(SourceMgr, PreviousLoc);
    diagnose(Tok, diag::expected_separator, ",")
      .fixItInsert(InsertLoc, ",");
    Invalid |= true;
  }

  Invalid = parseList(tok::r_square, LSquareLoc, RSquareLoc,
                      tok::comma, /*OptionalSep=*/false,
                      diag::expected_rsquare_array_expr,
                      [&] () -> bool {
    NullablePtr<Expr> Element
      = parseExpr(diag::expected_expr_in_collection_literal);
    if (Element.isNull())
      return true;

    SubExprs.push_back(Element.get());
    return false;
  });

  if (Invalid) return nullptr;

  Expr *SubExpr;
  if (SubExprs.size() == 1)
    SubExpr = new (Context) ParenExpr(LSquareLoc, SubExprs[0],
                                      RSquareLoc);
  else
    SubExpr = new (Context) TupleExpr(LSquareLoc,
                                      Context.AllocateCopy(SubExprs),
                                      nullptr, RSquareLoc);

  return new (Context) ArrayExpr(LSquareLoc, SubExpr, RSquareLoc);
}

/// parseExprDictionary - Parse a dictionary literal expression.
///
/// The lsquare-starting and first key have already been parsed, and
/// are passed in as parameters.
///
///   expr-dictionary:
///     lsquare-starting expr ':' expr (',' expr ':' expr)* ']'
NullablePtr<Expr> Parser::parseExprDictionary(SourceLoc LSquareLoc,
                                              Expr *FirstKey) {
  // Each subexpression is a (key, value) tuple. 
  // FIXME: We're not tracking the colon locations in the AST.
  SmallVector<Expr *, 8> SubExprs;
  SourceLoc RSquareLoc;
  bool Invalid = false;

  // Consume the ':'.
  consumeToken(tok::colon);

  // Function that adds a new key/value pair.
  auto addKeyValuePair = [&](Expr *Key, Expr *Value) -> void {
    SmallVector<Expr *, 2> Exprs;
    Exprs.push_back(Key);
    Exprs.push_back(Value);
    SubExprs.push_back(new (Context) TupleExpr(SourceLoc(),
                                               Context.AllocateCopy(Exprs),
                                               nullptr,
                                               SourceLoc()));
  };

  // Parse the first value.
  NullablePtr<Expr> FirstValue 
    = parseExpr(diag::expected_value_in_dictionary_literal);
  if (FirstValue.isNull()) {
    Invalid |= true;
  } else {
    // Add the first key/value pair.
    addKeyValuePair(FirstKey, FirstValue.get());
  }

  consumeIf(tok::comma);

  Invalid |= parseList(tok::r_square, LSquareLoc, RSquareLoc,
                       tok::comma, /*OptionalSep=*/false,
                       diag::expected_rsquare_array_expr, [&] {
    // Parse the next key.
    NullablePtr<Expr> Key
      = parseExpr(diag::expected_key_in_dictionary_literal);
    if (Key.isNull())
      return true;

    // Parse the ':'.
    if (Tok.isNot(tok::colon)) {
      diagnose(Tok, diag::expected_colon_in_dictionary_literal);
      return true;
    }
    consumeToken();

    // Parse the next value.
    NullablePtr<Expr> Value
      = parseExpr(diag::expected_value_in_dictionary_literal);
    if (Value.isNull())
      return true;

    // Add this key/value pair.
    addKeyValuePair(Key.get(), Value.get());
    return false;
  });

  if (Invalid) return nullptr;

  Expr *SubExpr;
  if (SubExprs.size() == 1)
    SubExpr = new (Context) ParenExpr(LSquareLoc, SubExprs[0],
                                      RSquareLoc);
  else
    SubExpr = new (Context) TupleExpr(LSquareLoc,
                                      Context.AllocateCopy(SubExprs),
                                      nullptr, RSquareLoc);

  return new (Context) DictionaryExpr(LSquareLoc, SubExpr, RSquareLoc);
}

/// parseExprFunc - Parse a func expression.
///
///   expr-func: 
///     'func' func-signature? stmt-brace
///
NullablePtr<Expr> Parser::parseExprFunc() {
  SourceLoc FuncLoc = consumeToken(tok::kw_func);

  SmallVector<Pattern*, 4> ArgParams;
  SmallVector<Pattern*, 4> BodyParams;
  TypeLoc RetTy;
  if (Tok.is(tok::l_brace)) {
    // If the func-signature isn't present, then this is a ()->Unresolved
    // function.
    TuplePattern *unitPattern = TuplePattern::create(Context, SourceLoc(),
      llvm::ArrayRef<TuplePatternElt>(),
      SourceLoc());
    ArgParams.push_back(unitPattern);
    BodyParams.push_back(unitPattern);
  } else if (!Tok.is(tok::l_paren)) {
    diagnose(Tok, diag::func_decl_without_paren);
    return 0;
  } else if (parseFunctionSignature(ArgParams, BodyParams, RetTy)) {
    return 0;
  }
  
  // The arguments to the func are defined in their own scope.
  Scope FuncBodyScope(this, /*AllowLookup=*/true);
  FuncExpr *FE = actOnFuncExprStart(FuncLoc, RetTy, ArgParams, BodyParams);

  // Establish the new context.
  ContextChange CC(*this, FE);
  
  // Then parse the expression.
  NullablePtr<BraceStmt> Body = parseStmtBrace(diag::expected_lbrace_func_expr);
  if (Body.isNull())
    return 0;

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

FuncExpr *Parser::actOnFuncExprStart(SourceLoc FuncLoc, TypeLoc FuncRetTy, 
                                     ArrayRef<Pattern*> ArgParams,
                                     ArrayRef<Pattern*> BodyParams) {
  FuncExpr *FE = FuncExpr::create(Context, FuncLoc,
                                  ArgParams, BodyParams, FuncRetTy,
                                  nullptr, CurDeclContext);

  for (Pattern *P : BodyParams)
    AddFuncArgumentsToScope(P, FE, *this);
  
  return FE;
}
