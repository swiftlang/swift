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
///     expr-sequence expr-if*
///     expr-sequence expr-is
///     expr-sequence expr-as
///
NullablePtr<Expr> Parser::parseExpr(Diag<> Message) {
  NullablePtr<Expr> first = parseExprSequence(Message);
  if (first.isNull())
    return nullptr;
  
  if (Tok.is(tok::kw_is)) {
    return parseExprIs(first.get());
  }
  if (Tok.is(tok::kw_as)) {
    return parseExprAs(first.get());
  }
  
  NullablePtr<IfExpr> ifExpr = nullptr;
  
  while (Tok.is(tok::question)) {
    NullablePtr<IfExpr> nextIf = parseExprIf(first.get());
    if (nextIf.isNull())
      return nullptr;
    
    if (ifExpr.isNull())
      ifExpr = nextIf;
    else {
      ifExpr.get()->setElseExpr(nextIf.get());
    }
    first = nextIf.get()->getElseExpr();
  }
  
  return ifExpr.isNull() ? first.get() : ifExpr.get();
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

/// parseExprIf
///
///   expr-if:
///     '?' expr-sequence ':' expr-sequence
///
NullablePtr<IfExpr> Parser::parseExprIf(Expr *condExpr) {
  SourceLoc questionLoc = consumeToken(tok::question);
  
  NullablePtr<Expr> thenExpr
    = parseExprSequence(diag::expected_expr_after_if_question);
  if (thenExpr.isNull())
    return nullptr;
  
  if (!Tok.is(tok::colon)) {
    diagnose(Tok, diag::expected_colon_after_if_question);
    return nullptr;
  }
  
  SourceLoc colonLoc = consumeToken(tok::colon);
  
  NullablePtr<Expr> elseExpr
    = parseExprSequence(diag::expected_expr_after_if_colon);
  if (elseExpr.isNull())
    return nullptr;
  
  return new (Context) IfExpr(condExpr, questionLoc,
                              thenExpr.get(), colonLoc, elseExpr.get());
}

/// parseExprSequence
///
///   expr-sequence:
///     expr-unary expr-binary*
///   expr-binary:
///     operator-binary expr-unary
///
/// The sequencing for binary exprs is not structural, i.e., binary operators
/// are not inherently right-associative.
NullablePtr<Expr> Parser::parseExprSequence(Diag<> Message) {
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

  if (Tok.is(tok::amp_prefix)) {
    SourceLoc Loc = consumeToken(tok::amp_prefix);

    if (Expr *SubExpr = parseExprUnary(Message).getPtrOrNull())
      return new (Context) AddressOfExpr(Loc, SubExpr, Type());
    return 0;
  }

  // If the next token is not an operator, just parse this as expr-postfix.
  if (Tok.isNot(tok::oper_prefix))
    return parseExprPostfix(Message);

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
///     'new' type-identifier expr-new-bounds
///     'new' type-identifier expr-call-suffix?
///     'new' type-identifier '.' selector-args
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
    NullablePtr<Expr> Init;

    // Parse '.' selector-args
    if (Tok.is(tok::period) && peekToken().is(tok::l_paren)) {
      // Consume the '.'.
      consumeToken(tok::period);

      Expr *Arg = nullptr;
      if (parseSelectorArgs(Arg)) {
        return nullptr;
      }

      Init = Arg;
    } 
    // Parse the optional expr-call-suffix.
    else if (Tok.isFollowingLParen()) {
      Init = parseExprCallSuffix(/*isConstructor=*/true);
      if (Init.isNull())
        return nullptr;
    }

    return new (Context) NewReferenceExpr(elementTy, newLoc,
                                          Init.getPtrOrNull());
  }

  // TODO: we allow a tuple-expr here as an initializer?
  if (Tok.isFollowingLParen()) {
    diagnose(newLoc, diag::array_new_init_unsupported);
    return nullptr;
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
///     'super' '.' selector-args
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

      // If we have a '.' '(', there are selector arguments following
      // this.
      if (Tok.is(tok::period) && peekToken().is(tok::l_paren)) {
        // Parse the '.'.
        consumeToken(tok::period);

        // Parse the selector arguments.
        Expr *args;
        if (parseSelectorArgs(args)) {
          return nullptr;
        }
        
        result = new (Context) CallExpr(result, args);
      } else if (Tok.isFollowingLParen()) {
        // Parse Swift-style constructor arguments.
        NullablePtr<Expr> arg = parseExprCallSuffix(/*isConstructor=*/true);
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
    } else if (Tok.is(tok::l_paren)) {
      Identifier name;
      SourceLoc nameLoc;
      Expr *arg;
      // FIXME: Unfortunate recovery here.
      if (parseSelectorArgs(name, nameLoc, arg))
        return 0;

      // FIXME: Loses sugar, which also means we can't differentiate
      // semantics or pretty-print cleanly.
      Expr *result = new (Context) UnresolvedDotExpr(superRef, dotLoc,
                                                     name, nameLoc);

      return new (Context) CallExpr(result, arg);
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
///     expr-postfix '.' selector-args
///
///   expr-subscript:
///     expr-postfix '[' expr ']'
///
///   expr-call:
///     expr-postfix expr-call-suffix
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
      if (Tok.is(tok::kw_metatype)) {
        SourceLoc metatypeLoc = consumeToken(tok::kw_metatype);
        Result = new (Context) MetatypeExpr(Result.get(), metatypeLoc, Type());
        continue;
      }

      if (Tok.is(tok::l_paren)) {
        Identifier Name;
        SourceLoc NameLoc;
        Expr *Arg;
        // FIXME: Unfortunate recovery here.
        if (parseSelectorArgs(Name, NameLoc, Arg))
          return 0;

        // FIXME: Loses sugar, which also means we can't differentiate
        // semantics or pretty-print cleanly.
        Result = new (Context) UnresolvedDotExpr(Result.get(), TokLoc,
                                                 Name, NameLoc);

        Result = new (Context) CallExpr(Result.get(), Arg);
        continue;
      }

      if (Tok.isNot(tok::identifier) && Tok.isNot(tok::integer_literal)) {
        diagnose(Tok, diag::expected_field_name_or_selector_args);
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
      NullablePtr<Expr> Arg = parseExprCallSuffix(/*isConstructor=*/false);
      // FIXME: Unfortunate recovery here.
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
      Lexer LocalLex(Segment.Data, SourceMgr, &Diags);
      
      // Temporarily swap out the parser's current lexer with our new one.
      llvm::SaveAndRestore<Lexer*> T(L, &LocalLex);
      
      // Prime the new lexer with a '(' as the first token.
      assert(Segment.Data.data()[-1] == '(' &&
             "Didn't get an lparen before interpolated expression");
      Tok.setToken(tok::l_paren, StringRef(Segment.Data.data()-1, 1));
      
      NullablePtr<Expr> E = parseExprList(tok::l_paren, tok::r_paren);
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
///     (identifier '=')? expr // FIXME: To be removed.
///     (identifier ':')? expr
///
/// FIXME: the '=' form above will likely go away.
///
NullablePtr<Expr> Parser::parseExprList(tok LeftTok, tok RightTok) {
  SourceLoc LLoc = consumeToken(LeftTok);
  SourceLoc RLoc;

  SmallVector<Expr*, 8> SubExprs;
  SmallVector<Identifier, 8> SubExprNames;

  if (Tok.isNot(RightTok)) {
    do {
      Identifier FieldName;
      // Check to see if there is a field specifier, like "x =" or "x : ".
      if (Tok.is(tok::identifier) &&
          (peekToken().is(tok::equal) || peekToken().is(tok::colon))) {
        if (parseIdentifier(FieldName,
                            diag::expected_field_spec_name_tuple_expr))
          return 0;

        assert(Tok.is(tok::equal) || Tok.is(tok::colon));
        consumeToken();
      }

      if (!SubExprNames.empty())
        SubExprNames.push_back(FieldName);
      else if (FieldName.get()) {
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
        if (parseAnyIdentifier(OperName, diag::expected_operator_ref))
          return nullptr;
        // Bypass local lookup. Use an 'Ordinary' reference kind so that the
        // reference may resolve to any unary or binary operator based on
        // context.
        auto *SubExpr = new(Context)UnresolvedDeclRefExpr(OperName,
                                                          DeclRefKind::Ordinary,
                                                          Loc);
        SubExprs.push_back(SubExpr);
      } else {
        NullablePtr<Expr> SubExpr = parseExpr(diag::expected_expr_in_expr_list);
        if (SubExpr.isNull())
          return 0;
        SubExprs.push_back(SubExpr.get());
      }
    } while (consumeIf(tok::comma));
  }
  
  
  // Check to see if the lexer stopped with an EOF token whose spelling is ')'.
  // If this happens, then this is actually the tuple that is a string literal
  // interpolation context.  Just accept the ) and build the tuple as we usually
  // do.
  if (Tok.is(tok::eof) && Tok.getText()[0] == ')')
    RLoc = Tok.getLoc();
  else {
    if (parseMatchingToken(RightTok, RLoc,
                           RightTok == tok::r_paren
                           ? diag::expected_rparen_expr_list
                           : diag::expected_rsquare_expr_list, LLoc))
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
    return new (Context) ParenExpr(LLoc, SubExprs[0], RLoc);
  }

  return new (Context) TupleExpr(LLoc, NewSubExprs, NewSubExprsNames, RLoc);
}

/// \brief Parse an expression call suffix.
///
/// expr-call-suffix:
///   expr-paren selector-arg*
///
/// selector-arg:
///   ':'? identifier expr-paren
NullablePtr<Expr> Parser::parseExprCallSuffix(bool isConstructor) {
  assert(Tok.isFollowingLParen() && "Not a call suffix?");

  // Parse the first argument.
  NullablePtr<Expr> firstArg = parseExprList(Tok.getKind(), tok::r_paren);
  if (firstArg.isNull())
    return 0;

  // If we don't have any selector arguments, we're done.
  if (!(Tok.is(tok::colon) && Tok.isAtStartOfLine()) &&
      !(Tok.is(tok::identifier) && !Tok.isAtStartOfLine()))
    return firstArg;

  // Parse the selector arguments.
  SmallVector<Expr *, 4> selectorArgs;
  SmallVector<Identifier, 4> selectorPieces;
  selectorArgs.push_back(firstArg.get());
  selectorPieces.push_back(Identifier());
  while (true) {
    // If there is a colon on a new line, parse it. This is a continuation.
    if (Tok.is(tok::colon) && Tok.isAtStartOfLine()) {
      SourceLoc colonLoc = consumeToken();

      // If there is no identifier after the colon, we have an error.
      if (Tok.isNot(tok::identifier)) {
        diagnose(Tok, diag::selector_argument_name_missing)
          << colonLoc;
        break;
      }
    }
    // Otherwise, an identifier on the same line continues the
    // selector arguments.
    else if (Tok.isNot(tok::identifier) || Tok.isAtStartOfLine()) {
      // We're done.
      break;
    }

    // Consume the selector piece.
    Identifier selectorPiece = Context.getIdentifier(Tok.getText());
    consumeToken(tok::identifier);

    // Look for the following '(' that provides arguments.
    if (Tok.isNot(tok::l_paren)) {
      diagnose(Tok, diag::expected_selector_call_args, selectorPiece);
      break;
    }

    // Parse the expression. We parse a full expression list, but
    // complain about it later.
    NullablePtr<Expr> selectorArg = parseExprList(tok::l_paren, tok::r_paren);
    if (selectorArg.isNull()) {
      // FIXME: Crummy recovery
      return 0;
    }
    selectorArgs.push_back(selectorArg.get());
    selectorPieces.push_back(selectorPiece);
  }

  // Verify that each argument is a simple, parenthesized expression.
  bool isFirst = true;
  for (auto &selectorArg : selectorArgs) {
    bool wasFirst = isFirst;
    isFirst = false;
    
    // Parenthesized expressions are fine.
    if (isa<ParenExpr>(selectorArg))
      continue;

    // We have a tuple.
    auto tuple = cast<TupleExpr>(selectorArg);
    unsigned size = tuple->getElements().size();

    // Empty tuples are okay.
    if (size == 0)
      continue;

    // Check whether we have too many elements.
    if (size > 1) {
      diagnose(tuple->getLParenLoc(),
               diag::selector_call_multiple_args,
               size)
        << SourceRange(Lexer::getLocForEndOfToken(
                         SourceMgr,
                         tuple->getElement(0)->getEndLoc()),
                       tuple->getRParenLoc());
      selectorArg = tuple->getElement(0);
      continue;
    }

    // Check whether the first element has a name.
    if (!tuple->getElementName(0).empty()) {
      if (wasFirst && isConstructor) {
        selectorPieces[0] = tuple->getElementName(0);
      } else {
        // FIXME: Use the location of the name, if we stored it.
        // FIXME: Fix-It to remove the name.
        diagnose(tuple->getElement(0)->getStartLoc(),
                 diag::selector_call_named_arg);
      }

      selectorArg = new (Context) ParenExpr(tuple->getLParenLoc(),
                                            tuple->getElement(0),
                                            tuple->getRParenLoc());
      continue;
    }
  }

  return new (Context) TupleExpr(selectorArgs.front()->getStartLoc(),
                                 Context.AllocateCopy(selectorArgs),
                                 Context.AllocateCopy<Identifier>(
                                   selectorPieces.begin(),
                                   selectorPieces.end()),
                                 selectorArgs.back()->getEndLoc());
}

/// \brief Parse a selector argument following the ".".
///
/// selector-args:
///   lparen_starting selector-arg-list selector-variadic-args? ')'
///
/// selector-arg-list:
///   selector-arg selector-arg-list?
///
/// selector-arg:
///   identifier ':' expr
///
/// selector-variadic-args:
///   ',' expr selector-variadic-args?
bool
Parser::parseSelectorArgs(Identifier &Name, SourceLoc &NameLoc, Expr *&Arg,
                          bool SeparateFirstName) {
  SourceLoc leftParen = consumeToken(tok::l_paren);
  SourceLoc SelectorIdent = Tok.getLoc();

  bool Invalid = false;
  bool isFirst = true;

  SmallVector<Expr*, 8> args;
  SmallVector<Identifier, 8> argNames;

  // Parse selector-arg-list.
  for (; Tok.is(tok::identifier); SelectorIdent = Tok.getLoc()) {
    // Parse the identifier.
    Identifier argName = Context.getIdentifier(Tok.getText());
    SourceLoc argNameLoc = consumeToken(tok::identifier);

    // Parse the ':'.
    if (Tok.isNot(tok::colon)) {
      diagnose(Tok, diag::expected_selector_colon_separator, argName);
      Invalid = true;
      if (Tok.isNot(tok::identifier))
        consumeToken();
      continue;
    }
    consumeToken(tok::colon);

    // Parse the expression.
    // FIXME: Pass through the name of the argument?
    NullablePtr<Expr> arg = parseExpr(diag::expected_selector_argument);
    if (arg.isNull()) {
      Invalid = true;
      continue;
    }

    if (isFirst) {
      if (SeparateFirstName) {
        Name = argName;
        NameLoc = argNameLoc;
        argNames.push_back(Identifier());
      } else {
        argNames.push_back(argName);
      }
      args.push_back(arg.get());
      isFirst = false;
    } else {
      argNames.push_back(argName);
      args.push_back(arg.get());
    }
  }

  if (argNames.empty() && Invalid == false) {
    diagnose(SelectorIdent, diag::expected_selector_piece);
    return true;
  }

  // Parse selector-variadic-args.
  while (consumeIf(tok::comma)) {
    NullablePtr<Expr> arg =parseExpr(diag::expected_selector_variadic_argument);
    if (arg.isNull()) {
      Invalid = true;
    } else {
      argNames.push_back(Identifier());
      args.push_back(arg.get());
    }
  }

  SourceLoc rightParen;
  if (parseMatchingToken(tok::r_paren, rightParen,
                         diag::expected_rparen_selector_args, leftParen)) {
    return true;
  }

  // Allocate the argument.
  if (args.size() == 1) {
    Arg = new (Context) ParenExpr(leftParen, args.front(), rightParen);
  } else {
    Arg = new (Context) TupleExpr(leftParen, Context.AllocateCopy(args),
                                  Context.AllocateCopy(argNames).data(),
                                  rightParen);
  }
  return Invalid;
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
  do {
    // If we see the closing square bracket, we're done.
    if (Tok.is(tok::r_square)) {
      RSquareLoc = consumeToken();
      break;
    }

    // If we don't see a comma, we're done.
    if (!Tok.is(tok::comma)) {
      skipUntil(tok::r_square);
      RSquareLoc = Tok.getLoc();
      if (Tok.is(tok::r_square))
        consumeToken();
      break;
    }

    // Parse the comma.
    consumeToken(tok::comma);

    // Parse the next expression.
    NullablePtr<Expr> Element
      = parseExpr(diag::expected_expr_in_collection_literal);
    if (Element.isNull()) {
      skipUntil(tok::r_square);
      RSquareLoc = Tok.getLoc();
      if (Tok.is(tok::r_square))
        consumeToken();
      break;
    }

    SubExprs.push_back(Element.get());
  } while (true);

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

  // Consume the ':'.
  consumeToken(tok::colon);

  // Parse the first value.
  NullablePtr<Expr> FirstValue 
    = parseExpr(diag::expected_value_in_dictionary_literal);
  if (FirstValue.isNull()) {
    skipUntil(tok::r_square);
    RSquareLoc = Tok.getLoc();
    if (Tok.is(tok::r_square))
      consumeToken();
    return 0;
  }

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

  // Add the first key/value pair.
  addKeyValuePair(FirstKey, FirstValue.get());

  do {
    // If we see the closing square bracket, we're done.
    if (Tok.is(tok::r_square)) {
      RSquareLoc = consumeToken();
      break;
    }

    // If we don't see a comma, we're done.
    if (!Tok.is(tok::comma)) {
      skipUntil(tok::r_square);
      RSquareLoc = Tok.getLoc();
      if (Tok.is(tok::r_square))
        consumeToken();
      break;
    }

    // Parse the comma.
    consumeToken(tok::comma);

    // Parse the next key.
    NullablePtr<Expr> Key
      = parseExpr(diag::expected_key_in_dictionary_literal);
    if (Key.isNull()) {
      skipUntil(tok::r_square);
      RSquareLoc = Tok.getLoc();
      if (Tok.is(tok::r_square))
        consumeToken();
      break;
    }

    // Parse the ':'.
    if (Tok.isNot(tok::colon)) {
      diagnose(Tok, diag::expected_colon_in_dictionary_literal);
      skipUntil(tok::r_square);
      RSquareLoc = Tok.getLoc();
      if (Tok.is(tok::r_square))
        consumeToken();
      break;      
    }
    consumeToken();

    // Parse the next value.
    NullablePtr<Expr> Value
      = parseExpr(diag::expected_value_in_dictionary_literal);
    if (Value.isNull()) {
      skipUntil(tok::r_square);
      RSquareLoc = Tok.getLoc();
      if (Tok.is(tok::r_square))
        consumeToken();
      break;
    }

    // Add this key/value pair.
    addKeyValuePair(Key.get(), Value.get());
  } while (true);

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
