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

/// \brief Create an argument with a trailing closure, with (optionally)
/// the elements, names, and parentheses locations from an existing argument.
static Expr *createArgWithTrailingClosure(ASTContext &context,
                                          SourceLoc leftParen,
                                          ArrayRef<Expr *> elementsIn,
                                          Identifier *namesIn,
                                          SourceLoc rightParen,
                                          Expr *closure) {
  // If there are no elements, just build a parenthesized expression around
  // the cosure.
  if (elementsIn.empty()) {
    return new (context) ParenExpr(leftParen, closure, rightParen,
                                   /*hasTrailingClosure=*/true);
  }

  // Create the list of elements, and add the trailing closure to the end.
  SmallVector<Expr *, 4> elements(elementsIn.begin(), elementsIn.end());
  elements.push_back(closure);

  Identifier *names = nullptr;
  if (namesIn) {
    names = context.Allocate<Identifier>(elements.size());
    std::copy(namesIn, namesIn + elements.size() - 1, names);
    new (namesIn + elements.size() - 1) Identifier();
  }

  // Form a full tuple expression.
  return new (context) TupleExpr(leftParen, context.AllocateCopy(elements),
                                 names, rightParen,
                                 /*hasTrailingClosure=*/true);
}

/// \brief Add the given trailing closure argument to the call argument.
static Expr *addTrailingClosureToArgument(ASTContext &context,
                                          Expr *arg, Expr *closure) {
  // Deconstruct the call argument to find its elements, element names,
  // and the locations of the left and right parentheses.
  if (auto tuple = dyn_cast<TupleExpr>(arg)) {
    // Deconstruct a tuple expression.
    return createArgWithTrailingClosure(context,
                                        tuple->getLParenLoc(),
                                        tuple->getElements(),
                                        tuple->getElementNames(),
                                        tuple->getRParenLoc(),
                                        closure);
  }

  // Deconstruct a parenthesized expression.
  auto paren = dyn_cast<ParenExpr>(arg);
  return createArgWithTrailingClosure(context,
                                      paren->getLParenLoc(),
                                      paren->getSubExpr(),
                                      nullptr,
                                      paren->getRParenLoc(), closure);
}

/// \brief Determine whether the given expression is an expr-postfix.
///
/// This routine inspects the form of an expression AST to determine whether it
/// was produced by parsing an expr-postfix, e.g., a call, member access, or
/// primary expression such as a parenthesized expression or tuple.
static bool isExprPostfix(Expr *expr) {
  switch (expr->getKind()) {
  // Not postfix expressions.
  case ExprKind::AddressOf:
  case ExprKind::Coerce:
  case ExprKind::PostfixUnary:
  case ExprKind::PrefixUnary:
  case ExprKind::Sequence:
  case ExprKind::Isa:
  case ExprKind::UnconditionalCheckedCast:
  case ExprKind::Assign:
  case ExprKind::UnresolvedPattern:
    return false;

  // Postfix expressions.
  case ExprKind::Array:
  case ExprKind::Call:
  case ExprKind::CharacterLiteral:
  case ExprKind::DeclRef:
  case ExprKind::Dictionary:
  case ExprKind::FloatLiteral:
  case ExprKind::Func:
  case ExprKind::MemberRef:
  case ExprKind::Metatype:
  case ExprKind::Module:
  case ExprKind::NewArray:
  case ExprKind::OverloadedDeclRef:
  case ExprKind::Paren:
  case ExprKind::PipeClosure:
  case ExprKind::RebindThisInConstructor:
  case ExprKind::IntegerLiteral:
  case ExprKind::InterpolatedStringLiteral:
  case ExprKind::StringLiteral:
  case ExprKind::Subscript:
  case ExprKind::SuperRef:
  case ExprKind::Tuple:
  case ExprKind::UnresolvedConstructor:
  case ExprKind::UnresolvedDeclRef:
  case ExprKind::UnresolvedDot:
  case ExprKind::UnresolvedMember:
  case ExprKind::UnresolvedSpecialize:
    return true;

  // Can't occur in the parser.
  case ExprKind::ArchetypeToSuper:
  case ExprKind::ArchetypeMemberRef:
  case ExprKind::ArchetypeSubscript:
  case ExprKind::Binary:
  case ExprKind::BridgeToBlock:
  case ExprKind::ConstructorRefCall:
  case ExprKind::DefaultValue:
  case ExprKind::DerivedToBase:
  case ExprKind::DotSyntaxBaseIgnored:
  case ExprKind::DotSyntaxCall:
  case ExprKind::Erasure:
  case ExprKind::ExistentialMemberRef:
  case ExprKind::ExistentialSubscript:
  case ExprKind::FunctionConversion:
  case ExprKind::GenericMemberRef:
  case ExprKind::GenericSubscript:
  case ExprKind::If:
  case ExprKind::ImplicitClosure:
  case ExprKind::Load:
  case ExprKind::Materialize:
  case ExprKind::MetatypeConversion:
  case ExprKind::OpaqueValue:
  case ExprKind::OtherConstructorDeclRef:
  case ExprKind::OverloadedMemberRef:
  case ExprKind::Requalify:
  case ExprKind::ScalarToTuple:
  case ExprKind::Specialize:
  case ExprKind::TupleElement:
  case ExprKind::TupleShuffle:
  case ExprKind::ZeroValue:
    llvm_unreachable("Not a parsed expression");

  // Treat error cases as postfix expressions.
  case ExprKind::Error:
    return true;
  }
}

/// parseExpr
///
///   expr:
///     expr-basic
///     expr-trailing-closure
///
///   expr-basic:
///     expr-sequence
///
///   expr-trailing-closure:
///     expr-postfix expr-closure+
///
/// \param isExprBasic Whether we're only parsing an expr-basic.
NullablePtr<Expr> Parser::parseExpr(Diag<> Message, bool isExprBasic) {
  // If we see a pattern in expr position, parse it to an UnresolvedPatternExpr.
  // Name binding will resolve whether it's in a valid pattern position.
  if (isOnlyStartOfMatchingPattern()) {
    NullablePtr<Pattern> pattern = parseMatchingPattern();
    if (pattern.isNull())
      return nullptr;
    return new (Context) UnresolvedPatternExpr(pattern.get());
  }
  
  NullablePtr<Expr> expr = parseExprSequence(Message);
  if (expr.isNull())
    return nullptr;
  
  // If we got a bare identifier inside a 'var' pattern, it forms a variable
  // binding pattern. Raise an error if the identifier shadows an existing
  // binding.
  //
  // TODO: We could check for a bare identifier followed by a non-postfix
  // token first thing with a lookahead.
  if (VarPatternDepth > 0) {
    if (auto *declRef = dyn_cast<DeclRefExpr>(expr.get())) {
      diagnose(declRef->getLoc(),
               diag::pattern_binding_shadows_var,
               declRef->getDecl()->getName().str());
      diagnose(declRef->getDecl()->getLoc(),
               diag::decl_declared_here,
               declRef->getDecl()->getName());
      
      auto pattern = createBindingFromPattern(declRef->getLoc(),
                                              declRef->getDecl()->getName());
      return new (Context) UnresolvedPatternExpr(pattern);
    }
    if (auto *udre = dyn_cast<UnresolvedDeclRefExpr>(expr.get())) {
      auto pattern = createBindingFromPattern(udre->getLoc(),
                                              udre->getName());
      return new (Context) UnresolvedPatternExpr(pattern);
    }
  }

  // Parse trailing closure, if we're allowed to.
  while (!isExprBasic && Tok.is(tok::l_brace)) {
    // Parse the closure.
    Expr *closure = parseExprClosure();

    // The grammar only permits a postfix-expression. However, we've
    // parsed a expr-sequence, so diagnose cases where we didn't get a
    // trailing closure.
    if (!isExprPostfix(expr.get())) {
      diagnose(closure->getStartLoc(), diag::trailing_closure_not_postfix)
        .highlight(expr.get()->getSourceRange());

      // Suggest parentheses around the complete expression.
      SourceLoc afterExprLoc
        = Lexer::getLocForEndOfToken(SourceMgr, expr.get()->getEndLoc());
      diagnose(expr.get()->getStartLoc(),
               diag::trailing_closure_full_expr_parentheses)
        .fixItInsert(expr.get()->getStartLoc(), "(")
        .fixItInsert(afterExprLoc, ")");

      // Suggest parentheses around the smallest postfix-expression and the
      // closure, if we can find it.
      if (auto seq = dyn_cast<SequenceExpr>(expr.get())) {
        Expr *last = seq->getElements().back();
        if (isExprPostfix(last)) {
          SourceLoc afterClosureLoc
            = Lexer::getLocForEndOfToken(SourceMgr, closure->getEndLoc());
          diagnose(last->getStartLoc(),
                   diag::trailing_closure_postfix_parentheses)
            .fixItInsert(last->getStartLoc(), "(")
            .fixItInsert(afterClosureLoc, ")");
        }
      }

      // FIXME: We have no idea which of the two options above, if any,
      // will actually type-check, which causes cascading failures. Should we
      // simply mark the result expression as erroneous?
    }

    // Introduce the trailing closure into the call, or form a call, as
    // necessary.
    if (auto call = dyn_cast<CallExpr>(expr.get())) {
      // When a closure follows a call, it becomes the last argument of
      // that call.
      Expr *arg = addTrailingClosureToArgument(Context, call->getArg(),
                                               closure);
      call->setArg(arg);
    } else {
      // Otherwise, the closure implicitly forms a call.
      Expr *arg = createArgWithTrailingClosure(Context, SourceLoc(), { },
                                               nullptr, SourceLoc(), closure);
      expr = new (Context) CallExpr(expr.get(), arg);
    }
  }

  return expr;
}

/// parseExprIs
///   expr-is:
///     'is' type
NullablePtr<Expr> Parser::parseExprIs() {
  SourceLoc isLoc = consumeToken(tok::kw_is);
  
  TypeLoc type;
  if (parseType(type, diag::expected_type_after_is))
    return nullptr;
  
  return new (Context) IsaExpr(isLoc, type);
}

/// parseExprAs
///   expr-as:
///     'as' type
///     'as' '!' type
NullablePtr<Expr> Parser::parseExprAs() {
  SourceLoc asLoc = consumeToken(tok::kw_as);
  SourceLoc bangLoc;
  
  if (Tok.isContextualPunctuator("!")) {
    bangLoc = consumeToken();
  }
  
  TypeLoc type;
  if (parseType(type, diag::expected_type_after_as))
    return nullptr;
  
  return bangLoc.isValid()
    ? (Expr*) new (Context) UnconditionalCheckedCastExpr(asLoc, bangLoc, type)
    : (Expr*) new (Context) CoerceExpr(asLoc, type);
}

/// \brief Determine whether the given token starts with a pipe ('|').
static bool startsWithPipe(Token tok) {
  return tok.isAnyOperator() && tok.getText()[0] == '|';
}

/// parseExprSequence
///
///   expr-sequence:
///     expr-unary expr-binary* expr-cast?
///   expr-binary:
///     operator-binary expr-unary
///     '?' expr-sequence ':' expr-unary
///     '=' expr-unary
///   expr-cast:
///     expr-is
///     expr-as
///
/// The sequencing for binary exprs is not structural, i.e., binary operators
/// are not inherently right-associative. If present, '?' and ':' tokens must
/// match.
NullablePtr<Expr> Parser::parseExprSequence(Diag<> Message) {
  SmallVector<Expr*, 8> SequencedExprs;
  SourceLoc startLoc = Tok.getLoc();
  
  Expr *suffix = nullptr;

  while (true) {
    // Parse a unary expression.
    auto Primary = parseExprUnary(Message);
    if (Primary.isNull())
      return nullptr;
    SequencedExprs.push_back(Primary.get());

    switch (Tok.getKind()) {
    case tok::oper_binary: {
      // If '|' is a delimiter and this operator starts with a '|',
      // we're done.
      if (PipeIsDelimiter && startsWithPipe(Tok))
        goto done;

      // Parse the operator.
      Expr *Operator = parseExprOperator();
      SequencedExprs.push_back(Operator);
      
      // The message is only valid for the first subexpr.
      Message = diag::expected_expr_after_operator;
      break;
    }
    
    case tok::question: {
      // Save the '?'.
      SourceLoc questionLoc = consumeToken();
      
      // Parse the middle expression of the ternary.
      NullablePtr<Expr> middle
        = parseExprSequence(diag::expected_expr_after_if_question);
      if (middle.isNull())
        return nullptr;
      
      // Make sure there's a matching ':' after the middle expr.
      if (!Tok.is(tok::colon)) {
        diagnose(questionLoc, diag::expected_colon_after_if_question);

        return new (Context) ErrorExpr({startLoc,
                                        middle.get()->getSourceRange().End});
      }
      
      SourceLoc colonLoc = consumeToken();
      
      auto *unresolvedIf
        = new (Context) IfExpr(questionLoc,
                               middle.get(),
                               colonLoc);
      SequencedExprs.push_back(unresolvedIf);
      Message = diag::expected_expr_after_if_colon;
      break;
    }
        
    case tok::equal: {
      SourceLoc equalsLoc = consumeToken();
      
      auto *assign = new (Context) AssignExpr(equalsLoc);
      SequencedExprs.push_back(assign);
      Message = diag::expected_expr_assignment;
      break;
    }
        
    default:
      // If the next token is not a binary operator, we're done.
      goto done;
    }
  }
done:
  
  // Check for a cast suffix.
  if (Tok.is(tok::kw_is)) {
    NullablePtr<Expr> is = parseExprIs();
    if (is.isNull()) return nullptr;
    suffix = is.get();
  }
  else if (Tok.is(tok::kw_as)) {
    NullablePtr<Expr> as = parseExprAs();
    if (as.isNull()) return nullptr;
    suffix = as.get();
  }
  
  // If present, push the cast suffix onto the sequence with a placeholder
  // RHS. (The real RHS is the type parameter encoded in the node itself.)
  if (suffix) {
    SequencedExprs.push_back(suffix);
    SequencedExprs.push_back(suffix);
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
    // If '|' is a delimiter and this operator starts with a '|',
    // we're done. Complain about needing an expression here.
    if (PipeIsDelimiter && startsWithPipe(Tok)) {
      diagnose(Tok, diag::expected_expr_after_operator)
        .highlight(SourceRange(PreviousLoc));
      return nullptr;
    }

    // For recovery purposes, accept an oper_binary here.
    SourceLoc OperEndLoc = Tok.getLoc().getAdvancedLoc(Tok.getLength());
    Tok.setKind(tok::oper_prefix);
    Operator = parseExprOperator();

    assert(OperEndLoc != Tok.getLoc() && "binary operator with no spaces?");
    diagnose(PreviousLoc, diag::expected_prefix_operator)
      .fixItRemove(Diagnostic::Range(OperEndLoc, Tok.getLoc()));
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

    // Pipe is not a delimiter within the square brackets.
    llvm::SaveAndRestore<bool> pipeIsNotDelimiter(PipeIsDelimiter, false);
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
      SourceLoc nameLoc;
      Identifier name;
      if (parseIdentifier(name, nameLoc,
                          diag::expected_identifier_after_super_dot_expr))
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
///     expr-closure
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

  case tok::l_brace:     // expr-closure
    Result = parseExprClosure();
    break;

  case tok::period_prefix: {     // .foo
    SourceLoc DotLoc = consumeToken(tok::period_prefix);
    Identifier Name;
    SourceLoc NameLoc;
    if (parseIdentifier(Name, NameLoc,diag::expected_identifier_after_dot_expr))
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
    bool IsPeriod = false;
    // Look ahead to see if we have '.foo(', '.foo[', '.foo.1(' or '.foo.1['.
    if (Tok.is(tok::period_prefix) && (peekToken().is(tok::identifier) ||
                                       peekToken().is(tok::integer_literal))) {
      BacktrackingScope BS(*this);
      consumeToken(tok::period_prefix);
      IsPeriod = peekToken().isFollowingLParen() ||
                 peekToken().isFollowingLSquare();
    }
    if (consumeIf(tok::period) || (IsPeriod && consumeIf(tok::period_prefix))) {
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

    // Check for a postfix-operator suffix, ignoring leading pipes when
    // pipe is a delimiter.
    if (Tok.is(tok::oper_postfix) && !
        (PipeIsDelimiter && startsWithPipe(Tok))) {
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

/// \brief Consume a pipe at the beginning of the current token.
static SourceLoc consumePipe(Parser &parser) {
  assert(startsWithPipe(parser.Tok));

  // Simple case: it's just a pipe.
  if (parser.Tok.getLength() == 1)
    return parser.consumeToken();

  // Skip the starting '|' of the token.
  SourceLoc loc = parser.Tok.getLoc();
  StringRef remaining = parser.Tok.getText().substr(1);
  parser.Tok.setToken(parser.L->getTokenKind(remaining), remaining);

  // If the token was treated as a binary operator, treat it as a prefix.
  if (parser.Tok.getKind() == tok::oper_binary)
    parser.Tok.setKind(tok::oper_prefix);
  
  return loc;
}

// Note: defined below.
static void AddFuncArgumentsToScope(const Pattern *pat, CapturingExpr *CE,
                                    Parser &P);

Expr *Parser::parseExprClosure() {
  assert(Tok.is(tok::l_brace) && "Not at a left brace?");

  // Parse the opening left brace.
  SourceLoc leftBrace = consumeToken();

  // If we started with a pipe, parse the closure-signature.
  Pattern *params = nullptr;
  SourceLoc arrowLoc;
  TypeLoc explicitResultType;
  if (startsWithPipe(Tok)) {
    // Consume the starting pipe.
    SourceLoc leftPipe = consumePipe(*this);

    // The pipe is treated as a delimiter within the closure parameter list.
    llvm::SaveAndRestore<bool> pipeIsDelimiter(PipeIsDelimiter, true);

    // Parse the list of tuple pattern elements.
    SmallVector<TuplePatternElt, 4> elements;
    bool invalid = false;
    if (!startsWithPipe(Tok)) {
      do {
        // If we see an arrow here, the user forgot a pattern. We'll complain
        // below.
        if (Tok.is(tok::arrow)) {
          break;
        }

        // Parse a pattern-tuple-element.
        // FIXME: Eventually, we want to allow default arguments.
        Optional<TuplePatternElt> elt = parsePatternTupleElement(false);
        if (!elt) {
          invalid = true;
          break;
        }

        // Variadic elements must come last.
        // FIXME: Unnecessary restriction. It makes conversion more interesting,
        // but is not complicated to support.
        if (!elements.empty() && elements.back().isVararg()) {
          diagnose(elements.back().getPattern()->getLoc(),
                   diag::ellipsis_pattern_not_at_end)
          .highlight(elt->getPattern()->getSourceRange());

          // Make the previous element non-variadic.
          elements.back().revertToNonVariadic();
        }

        // Add this element to the list.
        elements.push_back(*elt);

        // Parse the comma.
        if (Tok.is(tok::comma)) {
          consumeToken();
          continue;
        }

        // If it looks like we might have a pattern, assume that it's
        // just a missing comma.
        if (isStartOfPattern(Tok)) {
          SourceLoc endOfPreviousLoc = getEndOfPreviousLoc();
          diagnose(endOfPreviousLoc, diag::missing_comma_in_pattern)
            .fixItInsert(endOfPreviousLoc, ",");
          continue;
        }

        // Break out; we'll complain below.
        break;
      } while (true);
    }

    // Parse the closing pipe.
    SourceLoc rightPipe;
    if (!startsWithPipe(Tok)) {
      // If the user simply forgot the pipe, we can provide a Fix-It.
      if (Tok.is(tok::arrow)) {
        rightPipe = getEndOfPreviousLoc();
        diagnose(rightPipe, diag::expected_rpipe)
          .fixItInsert(rightPipe, "|");
        diagnose(leftPipe, diag::opening_pipe);
      } else if (invalid) {
        // Don't complain; just assign a location for the right
        // pipe to the current token.
        rightPipe = Tok.getLoc();
      } else {
        // Complain, but without a Fix-It because we don't know where it
        // would go.
        rightPipe = Tok.getLoc();
        diagnose(rightPipe, diag::expected_rpipe);
        diagnose(leftPipe, diag::opening_pipe);
      }
    } else {
      // Consume the pipe.
      rightPipe = consumePipe(*this);
    }

    params = TuplePattern::createSimple(Context, leftPipe, elements, rightPipe);

    // Parse the optional return type.
    if (Tok.is(tok::arrow)) {
      // Consume the ->.
      arrowLoc = consumeToken();

      // Parse the type.
      if (parseType(explicitResultType, diag::expected_closure_result_type)) {
        // If we couldn't parse the result type, clear out the arrow location.
        arrowLoc = SourceLoc();
      }
    }
  }

  // Create the closure expression and enter its context.
  PipeClosureExpr *closure = new (Context) PipeClosureExpr(params, arrowLoc,
                                                           explicitResultType,
                                                           CurDeclContext);
  // The arguments to the func are defined in their own scope.
  Scope S(this, ScopeKind::ClosureParams);
  ContextChange cc(*this, closure);

  // Handle parameters.
  if (params) {
    // Add the parameters into scope.
    AddFuncArgumentsToScope(params, closure, *this);
  } else {
    // There are no parameters; allow anonymous closure variables.
    // FIXME: We could do this all the time, and then provide Fix-Its
    // to map $i -> the appropriately-named argument. This might help
    // users who are refactoring code by adding names.
    AnonClosureVars.emplace_back();
  }

  // The pipe is not a delimiter within the body of the closure.
  llvm::SaveAndRestore<bool> pipeIsDelimiter(PipeIsDelimiter, false);

  // Parse the body.
  SmallVector<ExprStmtOrDecl, 4> bodyElements;
  parseBraceItems(bodyElements, /*IsTopLevel=*/false,
                     BraceItemListKind::Brace);

  // Parse the closing '}'.
  SourceLoc rightBrace;
  parseMatchingToken(tok::r_brace, rightBrace, diag::expected_closure_rbrace,
                     leftBrace);

  // We always need a right brace location, even if we couldn't parse the
  // actual right brace.
  // FIXME: Is this a local hack, should parseMatchingToken handle this?
  if (rightBrace.isInvalid())
    rightBrace = PreviousLoc;

  // If we didn't have any parameters, create a parameter list from the
  // anonymous closure arguments.
  if (!params) {
    // Create a parameter pattern containing the anonymous variables.
    auto& anonVars = AnonClosureVars.back();
    SmallVector<TuplePatternElt, 4> elements;
    for (auto anonVar : anonVars) {
      elements.push_back(TuplePatternElt(new (Context) NamedPattern(anonVar)));
    }
    params = TuplePattern::createSimple(Context, SourceLoc(), elements,
                                        SourceLoc());

    // Pop out of the anonymous closure variables scope.
    AnonClosureVars.pop_back();

    // Attach the parameters to the closure.
    closure->setParams(params, /*anonymousClosureVars=*/true);
  }

  // If the body consists of a single expression, turn it into a return
  // statement.
  bool hasSingleExpressionBody = false;
  if (bodyElements.size() == 1 && bodyElements[0].is<Expr *>()) {
    hasSingleExpressionBody = true;
    bodyElements[0] = new (Context) ReturnStmt(SourceLoc(),
                                               bodyElements[0].get<Expr*>());
  }

  // Set the body of the closure.
  closure->setBody(BraceStmt::create(Context, leftBrace, bodyElements,
                                     rightBrace),
                   hasSingleExpressionBody);

  return closure;
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

  // If this is a closure expression that did not have any named parameters,
  // generate the anonymous variables we need.
  auto closure = dyn_cast<PipeClosureExpr>(CurDeclContext);
  if (!closure || closure->getParams()) {
    // FIXME: specialize diagnostic when there were closure parameters.
    // We can be fairly smart here.
    diagnose(Loc, diag::anon_closure_arg_not_in_closure);
    return new (Context) ErrorExpr(Loc);
  }

  auto &decls = AnonClosureVars.back();
  while (ArgNo >= decls.size()) {
    unsigned nextIdx = decls.size();
    llvm::SmallVector<char, 4> StrBuf;
    StringRef varName = ("$" + Twine(nextIdx)).toStringRef(StrBuf);
    Identifier ident = Context.getIdentifier(varName);
    SourceLoc varLoc; // FIXME: Location?
    VarDecl *var = new (Context) VarDecl(varLoc, ident, Type(), closure);
    decls.push_back(var);
  }

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
      SourceLoc Loc;
      Identifier OperName;
      if (parseAnyIdentifier(OperName, Loc, diag::expected_operator_ref)) {
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
    return new (Context) ParenExpr(LLoc, SubExprs[0], RLoc,
                                   /*hasTrailingClosure=*/false);
  }

  return new (Context) TupleExpr(LLoc, NewSubExprs, NewSubExprsNames, RLoc,
                                 /*hasTrailingClosure=*/false);
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
    return new (Context) TupleExpr(RSquareLoc, { }, nullptr, RSquareLoc,
                                   /*hasTrailingClosure=*/false);
  }

  // Pipe is not a delimiter within the square brackets.
  llvm::SaveAndRestore<bool> pipeIsNotDelimiter(PipeIsDelimiter, false);

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
                                      RSquareLoc,
                                      /*hasTrailingClosure=*/false);
  else
    SubExpr = new (Context) TupleExpr(LSquareLoc,
                                      Context.AllocateCopy(SubExprs),
                                      nullptr, RSquareLoc,
                                      /*hasTrailingClosure=*/false);

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
                                               SourceLoc(),
                                               /*hasTrailingClosure=*/false));
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
                                      RSquareLoc,
                                      /*hasTrailingClosure=*/false);
  else
    SubExpr = new (Context) TupleExpr(LSquareLoc,
                                      Context.AllocateCopy(SubExprs),
                                      nullptr, RSquareLoc,
                                      /*hasTrailingClosure=*/false);

  return new (Context) DictionaryExpr(LSquareLoc, SubExpr, RSquareLoc);
}

/// AddFuncArgumentsToScope - Walk the type specified for a Func object (which
/// is known to be a FunctionType on the outer level) creating and adding named
/// arguments to the current scope.  This causes redefinition errors to be
/// emitted.
static void AddFuncArgumentsToScope(const Pattern *pat, CapturingExpr *CE,
                                    Parser &P) {
  switch (pat->getKind()) {
  case PatternKind::Named: {
    // Reparent the decl and add it to the scope.
    VarDecl *var = cast<NamedPattern>(pat)->getDecl();
    var->setDeclContext(CE);
    P.ScopeInfo.addToScope(var);
    return;
  }

  case PatternKind::Any:
    return;

  case PatternKind::Paren:
    AddFuncArgumentsToScope(cast<ParenPattern>(pat)->getSubPattern(), CE, P);
    return;

  case PatternKind::Typed:
    AddFuncArgumentsToScope(cast<TypedPattern>(pat)->getSubPattern(), CE, P);
    return;

  case PatternKind::Tuple:
    for (const TuplePatternElt &field : cast<TuplePattern>(pat)->getFields())
      AddFuncArgumentsToScope(field.getPattern(), CE, P);
    return;
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
    llvm_unreachable("pattern can't appear as a func argument!");
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
