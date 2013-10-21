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
#include "swift/AST/Diagnostics.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/Twine.h"
#include "swift/Basic/Fallthrough.h"
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
                                 /*hasTrailingClosure=*/true,
                                 /*Implicit=*/false);
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

/// parseExpr
///
///   expr:
///     expr-sequence(basic | trailing-closure)
///
/// \param isExprBasic Whether we're only parsing an expr-basic.
ParserResult<Expr> Parser::parseExpr(Diag<> Message, bool isExprBasic) {
  // If we see a pattern in expr position, parse it to an UnresolvedPatternExpr.
  // Name binding will resolve whether it's in a valid pattern position.
  if (isOnlyStartOfMatchingPattern()) {
    ParserResult<Pattern> pattern = parseMatchingPattern();
    if (pattern.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (pattern.isNull())
      return nullptr;
    return makeParserResult(new (Context) UnresolvedPatternExpr(pattern.get()));
  }
  
  ParserResult<Expr> expr = parseExprSequence(Message, isExprBasic);
  if (expr.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
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
      // This is ill-formed, but the problem will be caught later by scope
      // resolution.
      auto pattern = createBindingFromPattern(declRef->getLoc(),
                                              declRef->getDecl()->getName());
      return makeParserResult(new (Context) UnresolvedPatternExpr(pattern));
    }
    if (auto *udre = dyn_cast<UnresolvedDeclRefExpr>(expr.get())) {
      auto pattern = createBindingFromPattern(udre->getLoc(),
                                              udre->getName());
      return makeParserResult(new (Context) UnresolvedPatternExpr(pattern));
    }
  }

  return makeParserResult(expr.get());
}

/// parseExprIs
///   expr-is:
///     'is' type
ParserResult<Expr> Parser::parseExprIs() {
  SourceLoc isLoc = consumeToken(tok::kw_is);

  ParserResult<TypeRepr> type = parseType(diag::expected_type_after_is);
  if (type.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
  if (type.isNull())
    return nullptr;

  return makeParserResult(new (Context) IsaExpr(isLoc, type.get()));
}

/// parseExprAs
///   expr-as:
///     'as' type
ParserResult<Expr> Parser::parseExprAs() {
  SourceLoc asLoc = consumeToken(tok::kw_as);

  ParserResult<TypeRepr> type = parseType(diag::expected_type_after_as);
  if (type.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
  if (type.isNull())
    return nullptr;
  
  Expr *parsed = new (Context) ConditionalCheckedCastExpr(asLoc, type.get());
  
  return makeParserResult(parsed);
}

/// parseExprSequence
///
///   expr-sequence(Mode):
///     expr-unary(Mode) expr-binary(Mode)* expr-cast?
///   expr-binary(Mode):
///     operator-binary expr-unary(Mode)
///     '?' expr-sequence(Mode) ':' expr-unary(Mode)
///     '=' expr-unary
///   expr-cast:
///     expr-is
///     expr-as
///
/// The sequencing for binary exprs is not structural, i.e., binary operators
/// are not inherently right-associative. If present, '?' and ':' tokens must
/// match.
ParserResult<Expr> Parser::parseExprSequence(Diag<> Message, bool isExprBasic) {
  SmallVector<Expr*, 8> SequencedExprs;
  SourceLoc startLoc = Tok.getLoc();
  
  Expr *suffix = nullptr;

  while (true) {
    // Parse a unary expression.
    ParserResult<Expr> Primary = parseExprUnary(Message, isExprBasic);
    if (Primary.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (Primary.isNull())
      return nullptr;
    SequencedExprs.push_back(Primary.get());

    switch (Tok.getKind()) {
    case tok::oper_binary: {
      // If '>' is not an operator and this token starts with a '>', we're done.
      if (!GreaterThanIsOperator && startsWithGreater(Tok))
        goto done;

      // Parse the operator.
      Expr *Operator = parseExprOperator();
      SequencedExprs.push_back(Operator);
      
      // The message is only valid for the first subexpr.
      Message = diag::expected_expr_after_operator;
      break;
    }
    
    case tok::question_infix: {
      // Save the '?'.
      SourceLoc questionLoc = consumeToken();
      
      // Parse the middle expression of the ternary.
      ParserResult<Expr> middle =
          parseExprSequence(diag::expected_expr_after_if_question, isExprBasic);
      if (middle.hasCodeCompletion())
        return makeParserCodeCompletionResult<Expr>();
      if (middle.isNull())
        return nullptr;
      
      // Make sure there's a matching ':' after the middle expr.
      if (!Tok.is(tok::colon)) {
        diagnose(questionLoc, diag::expected_colon_after_if_question);

        return makeParserErrorResult(new (Context) ErrorExpr(
            {startLoc, middle.get()->getSourceRange().End}));
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
    ParserResult<Expr> is = parseExprIs();
    if (is.isNull() || is.hasCodeCompletion())
      return nullptr;
    suffix = is.get();
  }
  else if (Tok.is(tok::kw_as)) {
    ParserResult<Expr> as = parseExprAs();
    if (as.isNull() || as.hasCodeCompletion())
      return nullptr;
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
    return makeParserResult(SequencedExprs[0]);

  return makeParserResult(SequenceExpr::create(Context, SequencedExprs));
}

/// parseExprUnary
///
///   expr-unary(Mode):
///     expr-postfix(Mode)
///     expr-new
///     operator-prefix expr-unary(Mode)
///     '&' expr-unary(Mode)
///     expr-discard
///
///   expr-discard: '_'
///
ParserResult<Expr> Parser::parseExprUnary(Diag<> Message, bool isExprBasic) {
  UnresolvedDeclRefExpr *Operator;
  switch (Tok.getKind()) {
  default:
    // If the next token is not an operator, just parse this as expr-postfix.
    return parseExprPostfix(Message, isExprBasic);

  // If the next token is '_', parse a discard expression.
  case tok::kw__: {
    SourceLoc Loc = consumeToken();
    return makeParserResult(
        new (Context) DiscardAssignmentExpr(Loc, /*Implicit=*/false));
  }

  // If the next token is the keyword 'new', this must be expr-new.
  case tok::kw_new:
    return parseExprNew();
    
  case tok::amp_prefix: {
    SourceLoc Loc = consumeToken(tok::amp_prefix);

    ParserResult<Expr> SubExpr = parseExprUnary(Message, isExprBasic);
    if (SubExpr.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (SubExpr.isNull())
      return nullptr;
    return makeParserResult(
        new (Context) AddressOfExpr(Loc, SubExpr.get(), Type()));
  }

  case tok::oper_postfix:
    // Postfix operators cannot start a subexpression, but can happen
    // syntactically because the operator may just follow whatever preceeds this
    // expression (and that may not always be an expression).
    diagnose(Tok, diag::invalid_postfix_operator);
    Tok.setKind(tok::oper_prefix);
    SWIFT_FALLTHROUGH;
  case tok::oper_prefix:
    Operator = parseExprOperator();
    break;
  case tok::oper_binary: {
    // For recovery purposes, accept an oper_binary here.
    SourceLoc OperEndLoc = Tok.getLoc().getAdvancedLoc(Tok.getLength());
    Tok.setKind(tok::oper_prefix);
    Operator = parseExprOperator();

    if (OperEndLoc == Tok.getLoc())
      diagnose(PreviousLoc, diag::expected_expr_after_unary_operator);
    else
      diagnose(PreviousLoc, diag::expected_prefix_operator)
          .fixItRemoveChars(OperEndLoc, Tok.getLoc());
    break;
  }
  }

  ParserResult<Expr> SubExpr = parseExprUnary(Message, isExprBasic);
  if (SubExpr.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
  if (SubExpr.isNull())
    return nullptr;

  // Check if we have an unary '-' with integer literal sub-expression, for
  // example, "-42".
  if (auto *ILE = dyn_cast<IntegerLiteralExpr>(SubExpr.get())) {
    if (!Operator->getName().empty() && Operator->getName().str() == "-") {
      ILE->setNegative(Operator->getLoc());
      return makeParserResult(ILE);
    }
  }

  return makeParserResult(
      new (Context) PrefixUnaryExpr(Operator, SubExpr.get()));
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
UnresolvedDeclRefExpr *Parser::parseExprOperator() {
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
///     'new' type-simple expr-new-bounds expr-closure?
///   expr-new-bounds:
///     expr-new-bound
///     expr-new-bounds expr-new-bound
///   expr-new-bound:
///     lsquare-unspaced expr ']'
ParserResult<Expr> Parser::parseExprNew() {
  SourceLoc newLoc = Tok.getLoc();
  consumeToken(tok::kw_new);

  ParserResult<TypeRepr> elementTy = parseTypeSimple();
  if (elementTy.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
  if (elementTy.isNull())
    return nullptr;

  bool hadInvalid = false;
  SmallVector<NewArrayExpr::Bound, 4> bounds;
  while (Tok.isFollowingLSquare()) {
    SourceRange brackets;
    brackets.Start = consumeToken(tok::l_square);

    // If the bound is missing, that's okay unless this is the first bound.
    if (Tok.is(tok::r_square)) {
      if (bounds.empty()) {
        diagnose(Tok, diag::array_new_missing_first_bound);
        hadInvalid = true;
      }

      brackets.End = consumeToken(tok::r_square);
      bounds.push_back(NewArrayExpr::Bound(nullptr, brackets));
      continue;
    }

    auto boundValue = parseExpr(diag::expected_expr_new_array_bound);
    if (boundValue.isNull() || !Tok.is(tok::r_square)) {
      if (!boundValue.isNull())
        diagnose(Tok, diag::expected_bracket_array_new);

      skipUntil(tok::r_square);
      if (!Tok.is(tok::r_square))
        return nullptr;
      hadInvalid = true;
    }

    brackets.End = consumeToken(tok::r_square);

    bounds.push_back(NewArrayExpr::Bound(boundValue.get(), brackets));
  }

  if (hadInvalid)
    return nullptr;
  
  // Check for an initialization closure.
  Expr *constructExpr = nullptr;
  if (Tok.isFollowingLBrace()) {
    constructExpr = parseExprClosure();
    assert(constructExpr);
  }

  if (bounds.empty()) {
    diagnose(newLoc, diag::expected_bracket_array_new);
    // No need to indicate the error to the caller because it was not a parse
    // error.
    return makeParserResult(new (Context) ErrorExpr({newLoc, PreviousLoc}));
  }

  return makeParserResult(
      NewArrayExpr::create(Context, newLoc, elementTy.get(), bounds,
                           constructExpr));
}

static VarDecl *getImplicitSelfDeclForSuperContext(Parser &P,
                                                   DeclContext *DC,
                                                   SourceLoc Loc) {
  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
    if (auto *SelfDecl = AFD->getImplicitSelfDecl())
      return SelfDecl;
  }
  P.diagnose(Loc, diag::super_not_in_class_method);
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
///   expr-super-subscript:
///     'super' '[' expr ']'
ParserResult<Expr> Parser::parseExprSuper() {
  // Parse the 'super' reference.
  SourceLoc superLoc = consumeToken(tok::kw_super);
  
  VarDecl *selfDecl = getImplicitSelfDeclForSuperContext(*this,
                                                         CurDeclContext,
                                                         superLoc);
  Expr *superRef = selfDecl
    ? cast<Expr>(new (Context) SuperRefExpr(selfDecl, superLoc,
                                            /*Implicit=*/false))
    : cast<Expr>(new (Context) ErrorExpr(superLoc));
  
  if (Tok.is(tok::period)) {
    // 'super.' must be followed by a member or constructor ref.

    SourceLoc dotLoc = consumeToken(tok::period);
    
    if (Tok.is(tok::kw_init)) {
      // super.init
      SourceLoc ctorLoc = consumeToken();
      
      // Check that we're actually in a constructor.
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(CurDeclContext)) {
        if (!isa<ConstructorDecl>(AFD)) {
          diagnose(ctorLoc, diag::super_initializer_not_in_initializer);
          // No need to indicate error to the caller because this is not a parse
          // error.
          return makeParserResult(new (Context) ErrorExpr(
              SourceRange(superLoc, ctorLoc), ErrorType::get(Context)));
        }
      }
      // The constructor decl will be resolved by sema.
      Expr *result = new (Context) UnresolvedConstructorExpr(superRef,
                                                             dotLoc, ctorLoc,
                                                             /*Implicit=*/false);
      if (Tok.isFollowingLParen()) {
        // Parse Swift-style constructor arguments.
        ParserResult<Expr> arg = parseExprList(tok::l_paren, tok::r_paren);
        if (arg.hasCodeCompletion())
          return makeParserCodeCompletionResult<Expr>();
        // FIXME: Unfortunate recovery here.
        if (arg.isNull())
          return nullptr;
        
        result = new (Context) CallExpr(result, arg.get(), /*Implicit=*/false);
      } else {
        // It's invalid to refer to an uncalled constructor.
        diagnose(ctorLoc, diag::super_initializer_must_be_called);
        result->setType(ErrorType::get(Context));
        return makeParserErrorResult(result);
      }

      // The result of the called constructor is used to rebind 'self'.
      return makeParserResult(
          new (Context) RebindSelfInConstructorExpr(result, selfDecl));
    } else if (Tok.is(tok::code_complete)) {
      if (CodeCompletion) {
        if (auto *SRE = dyn_cast<SuperRefExpr>(superRef))
          CodeCompletion->completeExprSuperDot(SRE);
      }
      // Eat the code completion token because we handled it.
      consumeToken(tok::code_complete);
      return makeParserCodeCompletionResult(superRef);
    } else {
      // super.foo
      SourceLoc nameLoc;
      Identifier name;
      if (parseIdentifier(name, nameLoc,
                          diag::expected_identifier_after_super_dot_expr))
        return nullptr;
      
      if (!selfDecl)
        return makeParserErrorResult(new (Context) ErrorExpr(
            SourceRange(superLoc, nameLoc), ErrorType::get(Context)));
      
      return makeParserResult(new (Context) UnresolvedDotExpr(superRef, dotLoc,
                                                              name, nameLoc,
                                                           /*Implicit=*/false));
    }
  } else if (Tok.isFollowingLSquare()) {
    // super[expr]
    ParserResult<Expr> idx = parseExprList(tok::l_square, tok::r_square);
    if (idx.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (idx.isNull())
      return nullptr;
    return makeParserResult(new (Context) SubscriptExpr(superRef, idx.get()));
  }
  if (Tok.is(tok::code_complete)) {
    if (CodeCompletion) {
      if (auto *SRE = dyn_cast<SuperRefExpr>(superRef))
      CodeCompletion->completeExprSuper(SRE);
    }
    // Eat the code completion token because we handled it.
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionResult(superRef);
  }
  diagnose(Tok, diag::expected_dot_or_subscript_after_super);
  return nullptr;
}

/// parseExprPostfix
///
///   expr-literal:
///     integer_literal
///     floating_literal
///     string_literal
///     character_literal
///     '__FILE__'
///     '__LINE__'
///     '__COLUMN__'
///
///   expr-primary:
///     expr-literal
///     expr-identifier
///     expr-closure
///     expr-anon-closure-argument
///     expr-delayed-identifier
///     expr-paren
///     expr-super
///
///   expr-delayed-identifier:
///     '.' identifier
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
///   expr-force-value:
///     expr-postfix '!'
///
///   expr-trailing-closure:
///     expr-postfix(trailing-closure) expr-closure
///
///   expr-postfix(Mode):
///     expr-postfix(Mode) operator-postfix
///
///   expr-postfix(basic):
///     expr-primary
///     expr-dot
///     expr-metatype
///     expr-subscript
///     expr-call
///     expr-force-value
///
///   expr-postfix(trailing-closure):
///     expr-postfix(basic)
///     expr-trailing-closure

/// Copy a numeric literal value into AST-owned memory, stripping underscores
/// so the semantic part of the value can be parsed by APInt/APFloat parsers.
static StringRef copyAndStripUnderscores(ASTContext &C, StringRef orig) {
  char *start = static_cast<char*>(C.Allocate(orig.size(), 1));
  char *p = start;
  
  for (char c : orig)
    if (c != '_')
      *p++ = c;
  
  return StringRef(start, p - start);
}

ParserResult<Expr> Parser::parseExprPostfix(Diag<> ID, bool isExprBasic) {
  ParserResult<Expr> Result;
  switch (Tok.getKind()) {
  case tok::integer_literal: {
    StringRef Text = copyAndStripUnderscores(Context, Tok.getText());
    SourceLoc Loc = consumeToken(tok::integer_literal);
    Result = makeParserResult(new (Context) IntegerLiteralExpr(Text, Loc,
                                                           /*Implicit=*/false));
    break;
  }
  case tok::floating_literal: {
    StringRef Text = copyAndStripUnderscores(Context, Tok.getText());
    SourceLoc Loc = consumeToken(tok::floating_literal);
    Result = makeParserResult(new (Context) FloatLiteralExpr(Text, Loc,
                                                           /*Implicit=*/false));
    break;
  }
  case tok::character_literal: {
    uint32_t Codepoint = L->getEncodedCharacterLiteral(Tok);
    SourceLoc Loc = consumeToken(tok::character_literal);
    Result = makeParserResult(
        new (Context) CharacterLiteralExpr(Codepoint, Loc));
    break;
  }
  case tok::string_literal:  // "foo"
    Result = makeParserResult(parseExprStringLiteral());
    break;
  case tok::kw___FILE__: {  // __FILE__
    auto Kind = MagicIdentifierLiteralExpr::File;
    SourceLoc Loc = consumeToken(tok::kw___FILE__);
    Result = makeParserResult(
        new (Context) MagicIdentifierLiteralExpr(Kind, Loc, /*Implicit=*/false));
    break;
  }
  case tok::kw___LINE__: {  // __LINE__
    auto Kind = MagicIdentifierLiteralExpr::Line;
    SourceLoc Loc = consumeToken(tok::kw___LINE__);
    Result = makeParserResult(
        new (Context) MagicIdentifierLiteralExpr(Kind, Loc, /*Implicit=*/false));
    break;
  }

  case tok::kw___COLUMN__: { // __COLUMN__
    auto Kind = MagicIdentifierLiteralExpr::Column;
    SourceLoc Loc = consumeToken(tok::kw___COLUMN__);
    Result = makeParserResult(
        new (Context) MagicIdentifierLiteralExpr(Kind, Loc, /*Implicit=*/false));
    break;
  }
      
  case tok::kw_self:     // self
  case tok::identifier:  // foo
    Result = makeParserResult(parseExprIdentifier());
    break;
  case tok::dollarident: // $1
    Result = makeParserResult(parseExprAnonClosureArg());
    break;

  case tok::l_brace:     // expr-closure
    Result = makeParserResult(parseExprClosure());
    break;

  case tok::period_prefix: {     // .foo
    SourceLoc DotLoc = consumeToken(tok::period_prefix);
    Identifier Name;
    SourceLoc NameLoc;
    if (parseIdentifier(Name, NameLoc,diag::expected_identifier_after_dot_expr))
      return nullptr;
    
    // Handle .foo by just making an AST node.
    Result = makeParserResult(
        new (Context) UnresolvedMemberExpr(DotLoc, NameLoc, Name));
    break;
  }
      
  case tok::kw_super: {      // super.foo or super[foo]
    Result = parseExprSuper();
    break;
  }

  case tok::l_paren:
    if (Expr *E = parseExprList(tok::l_paren, tok::r_paren).getPtrOrNull())
      Result = makeParserResult(E);
    else
      Result = makeParserErrorResult<Expr>();
    break;

  case tok::l_square:
    Result = parseExprCollection();
    break;

  case tok::code_complete:
    if (CodeCompletion)
      CodeCompletion->completePostfixExprBeginning();
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionResult<Expr>();

  // Eat an invalid token in an expression context.  Error tokens are diagnosed
  // by the lexer, so there is no reason to emit another diagnostic.
  case tok::unknown:
    consumeToken(tok::unknown);
    return nullptr;

  default:
    // FIXME: offer a fixit: 'Self' -> 'self'
    diagnose(Tok, ID);
    return nullptr;
  }
  
  // If we had a parse error, don't attempt to parse suffixes.
  if (Result.isNull())
    return nullptr;

  bool hasBindOptional = false;
    
  // Handle suffix expressions.
  while (1) {
    // Check for a .foo suffix.
    SourceLoc TokLoc = Tok.getLoc();
    bool IsPeriod = false;
    // Look ahead to see if we have '.foo(', '.foo[', '.foo{',
    //   '.foo.1(', '.foo.1[', or '.foo.1{'.
    if (Tok.is(tok::period_prefix) && (peekToken().is(tok::identifier) ||
                                       peekToken().is(tok::integer_literal))) {
      BacktrackingScope BS(*this);
      consumeToken(tok::period_prefix);
      IsPeriod = peekToken().isFollowingLParen() ||
                 peekToken().isFollowingLSquare() ||
                 peekToken().isFollowingLBrace();
    }
    if (consumeIf(tok::period) || (IsPeriod && consumeIf(tok::period_prefix))) {
      if (Tok.isNot(tok::identifier) && Tok.isNot(tok::integer_literal)) {
        // If we have '.<keyword><code_complete>', try to recover by creating
        // an identifier with the same spelling as the keyword.
        if (Tok.isKeyword() && peekToken().is(tok::code_complete)) {
          Identifier Name = Context.getIdentifier(Tok.getText());
          Result = makeParserResult(
              new (Context) UnresolvedDotExpr(Result.get(), TokLoc,
                                              Name, Tok.getLoc(),
                                              /*Implicit=*/false));
          consumeToken();
        }
        if (Tok.is(tok::code_complete)) {
          if (CodeCompletion && Result.isNonNull())
            CodeCompletion->completeDotExpr(Result.get());
          // Eat the code completion token because we handled it.
          consumeToken(tok::code_complete);
          Result.setHasCodeCompletion();
          return Result;
        }
        diagnose(Tok, diag::expected_member_name);
        return nullptr;
      }

      // Don't allow '.<integer literal>' following a numeric literal
      // expression.
      if (Tok.is(tok::integer_literal) && Result.isNonNull() &&
          (isa<FloatLiteralExpr>(Result.get()) ||
           isa<IntegerLiteralExpr>(Result.get()))) {
        diagnose(Tok, diag::numeric_literal_numeric_member)
          .highlight(Result.get()->getSourceRange());
        consumeToken();
        continue;
      }

      Identifier Name = Context.getIdentifier(Tok.getText());
      Result = makeParserResult(
          new (Context) UnresolvedDotExpr(Result.get(), TokLoc, Name,
                                          Tok.getLoc(),
                                          /*Implicit=*/false));
      if (Tok.is(tok::identifier)) {
        consumeToken(tok::identifier);
        if (canParseAsGenericArgumentList()) {
          SmallVector<TypeRepr*, 8> args;
          SourceLoc LAngleLoc, RAngleLoc;
          if (parseGenericArguments(args, LAngleLoc, RAngleLoc)) {
            diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);
          }

          SmallVector<TypeLoc, 8> locArgs;
          for (auto ty : args)
            locArgs.push_back(ty);
          Result = makeParserResult(new (Context) UnresolvedSpecializeExpr(
              Result.get(), LAngleLoc, Context.AllocateCopy(locArgs),
              RAngleLoc));
        }
      } else {
        consumeToken(tok::integer_literal);
      }

      continue;
    }
    
    // Check for a () suffix, which indicates a call.
    // Note that this cannot be the start of a new line.
    if (Tok.isFollowingLParen()) {
      ParserResult<Expr> Arg = parseExprList(tok::l_paren, tok::r_paren);
      if (Arg.hasCodeCompletion())
        return makeParserCodeCompletionResult<Expr>();
      if (Arg.isNull())
        return nullptr;
      Result = makeParserResult(
          new (Context) CallExpr(Result.get(), Arg.get(), /*Implicit=*/false));
      continue;
    }
    
    // Check for a [expr] suffix.
    // Note that this cannot be the start of a new line.
    if (Tok.isFollowingLSquare()) {
      ParserResult<Expr> Idx = parseExprList(tok::l_square, tok::r_square);
      if (Idx.hasCodeCompletion())
        return makeParserCodeCompletionResult<Expr>();
      if (Idx.isNull())
        return nullptr;
      Result = makeParserResult(
          new (Context) SubscriptExpr(Result.get(), Idx.get()));
      continue;
    }
    
    // Check for a trailing closure, if allowed.
    if (!isExprBasic && Tok.isFollowingLBrace()) {
      // Parse the closure.
      Expr *closure = parseExprClosure();
      
      // Introduce the trailing closure into the call, or form a call, as
      // necessary.
      if (auto call = dyn_cast<CallExpr>(Result.get())) {
        // When a closure follows a call, it becomes the last argument of
        // that call.
        Expr *arg = addTrailingClosureToArgument(Context, call->getArg(),
                                                 closure);
        call->setArg(arg);
      } else {
        // Otherwise, the closure implicitly forms a call.
        Expr *arg = createArgWithTrailingClosure(Context, SourceLoc(), { },
                                                 nullptr, SourceLoc(), closure);
        Result = makeParserResult(new (Context) CallExpr(Result.get(), arg,
                                                       /*Implicit=*/true));
      }
      continue;
    }

    // Check for a ? suffix.
    if (consumeIf(tok::question_postfix)) {
      Result = makeParserResult(
          new (Context) BindOptionalExpr(Result.get(), TokLoc));
      hasBindOptional = true;
      continue;
    }

    // Check for a ! suffix.
    if (consumeIf(tok::exclaim_postfix)) {
      Result = makeParserResult(new (Context) ForceValueExpr(Result.get(), 
                                                             TokLoc));
      continue;
    }

    // Check for a postfix-operator suffix.
    if (Tok.is(tok::oper_postfix)) {
      // If '>' is not an operator and this token starts with a '>', we're done.
      if (!GreaterThanIsOperator && startsWithGreater(Tok))
        return Result;

      Expr *oper = parseExprOperator();
      Result = makeParserResult(
          new (Context) PostfixUnaryExpr(oper, Result.get()));
      continue;
    }

    if (Tok.is(tok::code_complete)) {
      if (Tok.isAtStartOfLine()) {
        // Postfix expression is located on a different line than the code
        // completion token, and thus they are not related.
        return Result;
      }
      if (CodeCompletion && Result.isNonNull())
        CodeCompletion->completePostfixExpr(Result.get());
      // Eat the code completion token because we handled it.
      consumeToken(tok::code_complete);
      return makeParserCodeCompletionResult<Expr>();
    }
    break;
  }

  // If we had a ? suffix expression, bind the entire postfix chain
  // within an OptionalEvaluationExpr.
  if (hasBindOptional) {
    Result = makeParserResult(
               new (Context) OptionalEvaluationExpr(Result.get()));
  }
  
  return Result;
}

static StringLiteralExpr *
createStringLiteralExprFromSegment(ASTContext &Ctx,
                                   const Lexer *L,
                                   Lexer::StringSegment &Segment,
                                   SourceLoc TokenLoc) {
  assert(Segment.Kind == Lexer::StringSegment::Literal);
  // FIXME: Consider lazily encoding the string when needed.
  llvm::SmallString<256> Buf;
  StringRef EncodedStr = L->getEncodedStringSegment(Segment, Buf);
  if (!Buf.empty()) {
    assert(EncodedStr.begin() == Buf.begin() &&
           "Returned string is not from buffer?");
    EncodedStr = Ctx.AllocateCopy(EncodedStr);
  }
  return new (Ctx) StringLiteralExpr(EncodedStr, TokenLoc);
}

///   expr-literal:
///     string_literal
Expr *Parser::parseExprStringLiteral() {
  SmallVector<Lexer::StringSegment, 1> Segments;
  L->getStringLiteralSegments(Tok, Segments);
  SourceLoc Loc = consumeToken();
    
  // The simple case: just a single literal segment.
  if (Segments.size() == 1 &&
      Segments.front().Kind == Lexer::StringSegment::Literal) {
    return createStringLiteralExprFromSegment(Context, L, Segments.front(),
                                              Loc);
  }
    
  SmallVector<Expr*, 4> Exprs;
  for (auto Segment : Segments) {
    switch (Segment.Kind) {
    case Lexer::StringSegment::Literal: {
      Exprs.push_back(
          createStringLiteralExprFromSegment(Context, L, Segment, Loc));
      break;
    }
        
    case Lexer::StringSegment::Expr: {
      // We are going to mess with Tok to do reparsing for interpolated literals,
      // don't lose our 'next' token.
      llvm::SaveAndRestore<Token> SavedTok(Tok);

      // Create a temporary lexer that lexes from the body of the string.
      Lexer::State BeginState =
          L->getStateForBeginningOfTokenLoc(Segment.Loc);
      // We need to set the EOF at r_paren, to prevent the Lexer from eagerly
      // trying to lex the token beyond it. Parser::parseList() does a special
      // check for a tok::EOF that is spelled with a ')'.
      // FIXME: This seems like a hack, there must be a better way..
      Lexer::State EndState = BeginState.advance(Segment.Length-1);
      Lexer LocalLex(*L, BeginState, EndState);

      // Temporarily swap out the parser's current lexer with our new one.
      llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

      // Prime the new lexer with a '(' as the first token.
      // We might be at tok::eof now, so ensure that consumeToken() does not
      // assert about lexing past eof.
      Tok.setKind(tok::unknown);
      consumeToken();
      assert(Tok.is(tok::l_paren));
      
      ParserResult<Expr> E = parseExprList(tok::l_paren, tok::r_paren);
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
  assert(Tok.is(tok::identifier) || Tok.is(tok::kw_self));
  SourceLoc Loc = Tok.getLoc();
  Identifier Name = Context.getIdentifier(Tok.getText());
  consumeToken();
  return actOnIdentifierExpr(Name, Loc);
}

// Note: defined below.
static void addFunctionParametersToScope(const Pattern *Pat, DeclContext *DC,
                                         Parser &P);

bool Parser::parseClosureSignatureIfPresent(Pattern *&params,
                                            SourceLoc &arrowLoc,
                                            TypeRepr *&explicitResultType,
                                            SourceLoc &inLoc) {
  // Clear out result parameters.
  params = nullptr;
  arrowLoc = SourceLoc();
  explicitResultType = nullptr;
  inLoc = SourceLoc();

  // Check whether we have a closure signature here.
  // FIXME: We probably want to be a bit more permissive here.
  if (Tok.is(tok::l_paren)) {
    // Parse pattern-tuple func-signature-result? 'in'.
    BacktrackingScope backtrack(*this);

    // Parse the pattern-tuple.
    consumeToken();
    if (!canParseTypeTupleBody())
      return false;

    // Parse the func-signature-result, if present.
    if (consumeIf(tok::arrow)) {
      if (!canParseType())
        return false;
    }

    // Parse the 'in' at the end.
    if (!Tok.is(tok::kw_in)) {
      return false;
    }

    // Okay, we have a closure signature.
  } else if (Tok.is(tok::identifier) || Tok.is(tok::kw__)) {
    BacktrackingScope backtrack(*this);

    // Parse identifier (',' identifier)*
    consumeToken();
    while (consumeIf(tok::comma)) {
      if (Tok.is(tok::identifier) || Tok.is(tok::kw__)) {
        consumeToken();
        continue;
      }

      return false;
    }

    // Parse the func-signature-result, if present.
    if (consumeIf(tok::arrow)) {
      if (!canParseType())
        return false;
    }

    // Parse the 'in' at the end.
    if (!Tok.is(tok::kw_in)) {
      return false;
    }

    // Okay, we have a closure signature.
  } else {
    // No closure signature.
    return false;
  }

  // At this point, we know we have a closure signature. Parse the parameters.
  bool invalid = false;
  if (Tok.is(tok::l_paren)) {
    // Parse the pattern-tuple.
    auto pattern = parsePatternTuple(/*AllowInitExpr=*/false);
    if (pattern.isNonNull())
      params = pattern.get();
    else
      invalid = true;
  } else {
    // Parse identifier (',' identifier)*
    SmallVector<TuplePatternElt, 4> elements;
    do {
      if (Tok.is(tok::identifier)) {
        auto var = new (Context) VarDecl(Tok.getLoc(),
                                         Context.getIdentifier(Tok.getText()),
                                         Type(), nullptr);
        elements.push_back(TuplePatternElt(new (Context) NamedPattern(var)));
        consumeToken();
      } else if (Tok.is(tok::kw__)) {
        elements.push_back(TuplePatternElt(
                             new (Context) AnyPattern(Tok.getLoc())));
        consumeToken();
      } else {
        diagnose(Tok, diag::expected_closure_parameter_name);
        invalid = true;
        break;
      }

      // Consume a comma to continue.
      if (consumeIf(tok::comma)) {
        continue;
      }

      break;
    } while (true);

    params = TuplePattern::create(Context, SourceLoc(), elements, SourceLoc());
  }

  // Parse the optional explicit return type.
  if (Tok.is(tok::arrow)) {
    // Consume the '->'.
    arrowLoc = consumeToken();

    // Parse the type.
    explicitResultType =
        parseType(diag::expected_closure_result_type).getPtrOrNull();
    if (!explicitResultType) {
      // If we couldn't parse the result type, clear out the arrow location.
      arrowLoc = SourceLoc();
      invalid = true;
    }
  }

  // Parse the 'in'.
  if (Tok.is(tok::kw_in)) {
    inLoc = consumeToken();
  } else {
    // Scan forward to see if we can find the 'in'. This re-synchronizes the
    // parser so we can at least parse the body correctly.
    SourceLoc startLoc = Tok.getLoc();
    ParserPosition pos = getParserPosition();
    while (Tok.isNot(tok::eof) && !Tok.is(tok::kw_in) &&
           Tok.isNot(tok::r_brace)) {
      skipSingle();
    }

    if (Tok.is(tok::kw_in)) {
      // We found the 'in'. If this is the first error, complain about the
      // junk tokens in-between but re-sync at the 'in'.
      if (!invalid) {
        diagnose(startLoc, diag::unexpected_tokens_before_closure_in);
      }
      inLoc = consumeToken();
    } else {
      // We didn't find an 'in', backtrack to where we started. If this is the
      // first error, complain about the missing 'in'.
      backtrackToPosition(pos);
      if (!invalid) {
        diagnose(Tok, diag::expected_closure_in)
          .fixItInsert(Tok.getLoc(), "in ");
      }
      inLoc = Tok.getLoc();
    }
  }

  return invalid;
}

Expr *Parser::parseExprClosure() {
  assert(Tok.is(tok::l_brace) && "Not at a left brace?");

  // Parse the opening left brace.
  SourceLoc leftBrace = consumeToken();

  // Parse the closure-signature, if present.
  Pattern *params = nullptr;
  SourceLoc arrowLoc;
  TypeRepr *explicitResultType;
  SourceLoc inLoc;
  parseClosureSignatureIfPresent(params, arrowLoc, explicitResultType, inLoc);

  // Create the closure expression and enter its context.
  ClosureExpr *closure = new (Context) ClosureExpr(params, arrowLoc,
                                                   explicitResultType,
                                                   CurDeclContext);
  // The arguments to the func are defined in their own scope.
  Scope S(this, ScopeKind::ClosureParams);
  ContextChange cc(*this, closure);

  // Handle parameters.
  if (params) {
    // Add the parameters into scope.
    ::addFunctionParametersToScope(params, closure, *this);
  } else {
    // There are no parameters; allow anonymous closure variables.
    // FIXME: We could do this all the time, and then provide Fix-Its
    // to map $i -> the appropriately-named argument. This might help
    // users who are refactoring code by adding names.
    AnonClosureVars.emplace_back();
  }

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
    closure->setParams(params);
    closure->setHasAnonymousClosureVars();
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
    AllNumeric &= bool(isdigit(Name[i]));
  
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
  auto closure = dyn_cast_or_null<ClosureExpr>(
      dyn_cast<AbstractClosureExpr>(CurDeclContext));
  if (!closure || closure->getParams()) {
    // FIXME: specialize diagnostic when there were closure parameters.
    // We can be fairly smart here.
    diagnose(Loc, diag::anon_closure_arg_not_in_closure);
    return new (Context) ErrorExpr(Loc);
  }

  auto &decls = AnonClosureVars.back();
  while (ArgNo >= decls.size()) {
    unsigned nextIdx = decls.size();
    SmallVector<char, 4> StrBuf;
    StringRef varName = ("$" + Twine(nextIdx)).toStringRef(StrBuf);
    Identifier ident = Context.getIdentifier(varName);
    SourceLoc varLoc = Loc;
    VarDecl *var = new (Context) VarDecl(varLoc, ident, Type(), closure);
    decls.push_back(var);
  }

  return new (Context) DeclRefExpr(AnonClosureVars.back()[ArgNo], Loc,
                                   /*Implicit=*/false);
}

Expr *Parser::actOnIdentifierExpr(Identifier text, SourceLoc loc) {
  SmallVector<TypeRepr*, 8> args;
  SourceLoc LAngleLoc, RAngleLoc;
  bool hasGenericArgumentList = false;

  // Axle: if the identifier is 'Vec' or 'Matrix' and it's followed by
  // an argument list for the sugared vector or matrix types, parse them.
  if (Context.LangOpts.Axle) {
    if (text.str().equals("Vec") && canParseAsAxleSugarArguments()) {
      // Parse the vector type.
      ParserResult<VecTypeRepr> vecTy = parseTypeAxleVec(loc);
      LAngleLoc = Tok.getLoc();
      if (vecTy.isParseError()) {
        diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);
        return new (Context) ErrorExpr(loc);
      }

      // FIXME: Better handling of code completion here?
      if (vecTy.hasCodeCompletion())
        return new (Context) ErrorExpr(loc);

      return new (Context) MetatypeExpr(vecTy.get());
    }

    if (text.str().equals("Matrix") && canParseAsAxleSugarArguments()) {
      // Parse the matrix type.
      ParserResult<MatrixTypeRepr> matrixTy = parseTypeAxleMatrix(loc);
      LAngleLoc = Tok.getLoc();
      if (matrixTy.isParseError()) {
        diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);
        return new (Context) ErrorExpr(loc);
      }

      // FIXME: Better handling of code completion here?
      if (matrixTy.hasCodeCompletion())
        return new (Context) ErrorExpr(loc);

      return new (Context) MetatypeExpr(matrixTy.get());

    }
  }

  if (canParseAsGenericArgumentList()) {
    hasGenericArgumentList = true;
    if (parseGenericArguments(args, LAngleLoc, RAngleLoc)) {
      diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);
    }
  }

  if (CurDeclContext == CurVars.first) {
    for (auto activeVar : CurVars.second) {
      if (activeVar->getName() == text) {
        diagnose(loc, diag::var_init_self_referential);
        return new (Context) ErrorExpr(loc);
      }
    }
  }
  
  ValueDecl *D = lookupInScope(text);
  // FIXME: We want this to work: "var x = { x() }", but for now it's better to
  // disallow it than to crash.
  if (!D && CurDeclContext != CurVars.first) {
    for (auto activeVar : CurVars.second) {
      if (activeVar->getName() == text) {
        diagnose(loc, diag::var_init_self_referential);
        return new (Context) ErrorExpr(loc);
      }
    }
  }

  Expr *E;
  if (D == 0) {
    auto refKind = DeclRefKind::Ordinary;
    auto unresolved = new (Context) UnresolvedDeclRefExpr(text, refKind, loc);
    unresolved->setSpecialized(hasGenericArgumentList);
    E = unresolved;
  } else {
    auto declRef = new (Context) DeclRefExpr(D, loc, /*Implicit=*/false);
    declRef->setGenericArgs(args);
    E = declRef;
  }
  
  if (hasGenericArgumentList) {
    SmallVector<TypeLoc, 8> locArgs;
    for (auto ty : args)
      locArgs.push_back(ty);
    E = new (Context) UnresolvedSpecializeExpr(E, LAngleLoc,
                                               Context.AllocateCopy(locArgs),
                                               RAngleLoc);
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
ParserResult<Expr> Parser::parseExprList(tok LeftTok, tok RightTok) {
  SourceLoc LLoc = consumeToken(LeftTok);
  SourceLoc RLoc;

  SmallVector<Expr*, 8> SubExprs;
  SmallVector<Identifier, 8> SubExprNames;

  ParserStatus Status = parseList(RightTok, LLoc, RLoc,
                                  tok::comma, /*OptionalSep=*/false,
                                  RightTok == tok::r_paren ?
                                  diag::expected_rparen_expr_list :
                                  diag::expected_rsquare_expr_list,
                                  [&] () -> ParserStatus {
    Identifier FieldName;
    // Check to see if there is a field specifier
    if (Tok.is(tok::identifier) && peekToken().is(tok::colon)) {
      if (parseIdentifier(FieldName,
                          diag::expected_field_spec_name_tuple_expr)) {
        return makeParserError();
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
        return makeParserError();
      }
      // Bypass local lookup. Use an 'Ordinary' reference kind so that the
      // reference may resolve to any unary or binary operator based on
      // context.
      auto *SubExpr = new(Context) UnresolvedDeclRefExpr(OperName,
                                                         DeclRefKind::Ordinary,
                                                         Loc);
      SubExprs.push_back(SubExpr);
    } else {
      ParserResult<Expr> SubExpr = parseExpr(diag::expected_expr_in_expr_list);
      if (SubExpr.isNonNull())
        SubExprs.push_back(SubExpr.get());
      return SubExpr;
    }
    return makeParserSuccess();
  });

  if (Status.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();

  MutableArrayRef<Expr *> NewSubExprs = Context.AllocateCopy(SubExprs);
  
  Identifier *NewSubExprsNames = 0;
  if (!SubExprNames.empty())
    NewSubExprsNames =
      Context.AllocateCopy<Identifier>(SubExprNames.data(),
                                       SubExprNames.data()+SubExprs.size());
  
  // A tuple with a single, unlabelled element is just parentheses.
  if (SubExprs.size() == 1 &&
      (SubExprNames.empty() || SubExprNames[0].empty())) {
    return makeParserResult(
        Status, new (Context) ParenExpr(LLoc, SubExprs[0], RLoc,
                                        /*hasTrailingClosure=*/false));
  }

  return makeParserResult(
      new (Context) TupleExpr(LLoc, NewSubExprs, NewSubExprsNames, RLoc,
                              /*hasTrailingClosure=*/false,
                              /*Implicit=*/false));
}

/// parseExprCollection - Parse a collection literal expression.
///
///   expr-collection:
///     expr-array
///     expr-dictionary
//      lsquare-starting ']'
ParserResult<Expr> Parser::parseExprCollection() {
  SourceLoc LSquareLoc = consumeToken(tok::l_square);

  // Parse an empty collection literal.
  if (Tok.is(tok::r_square)) {
    // FIXME: We want a special 'empty collection' literal kind.
    SourceLoc RSquareLoc = consumeToken();
    return makeParserResult(
        new (Context) TupleExpr(LSquareLoc, { }, nullptr, RSquareLoc,
                                /*hasTrailingClosure=*/false,
                                /*Implicit=*/false));
  }

  // Parse the first expression.
  ParserResult<Expr> FirstExpr
    = parseExpr(diag::expected_expr_in_collection_literal);
  if (FirstExpr.isNull() || FirstExpr.hasCodeCompletion()) {
    skipUntil(tok::r_square);
    if (Tok.is(tok::r_square))
      consumeToken();
    if (FirstExpr.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    return nullptr;
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
ParserResult<Expr> Parser::parseExprArray(SourceLoc LSquareLoc,
                                          Expr *FirstExpr) {
  SmallVector<Expr *, 8> SubExprs;
  SubExprs.push_back(FirstExpr);

  SourceLoc RSquareLoc;
  ParserStatus Status;

  if (Tok.isNot(tok::r_square) && !consumeIf(tok::comma)) {
    SourceLoc InsertLoc = Lexer::getLocForEndOfToken(SourceMgr, PreviousLoc);
    diagnose(Tok, diag::expected_separator, ",")
        .fixItInsert(InsertLoc, ",");
    Status.setIsParseError();
  }

  Status |= parseList(tok::r_square, LSquareLoc, RSquareLoc,
                      tok::comma, /*OptionalSep=*/false,
                      diag::expected_rsquare_array_expr,
                      [&] () -> ParserStatus {
    ParserResult<Expr> Element
      = parseExpr(diag::expected_expr_in_collection_literal);
    if (Element.isNonNull())
      SubExprs.push_back(Element.get());
    return Element;
  });

  if (Status.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();

  assert(SubExprs.size() >= 1);

  Expr *SubExpr;
  if (SubExprs.size() == 1)
    SubExpr = new (Context) ParenExpr(LSquareLoc, SubExprs[0],
                                      RSquareLoc,
                                      /*hasTrailingClosure=*/false);
  else
    SubExpr = new (Context) TupleExpr(LSquareLoc,
                                      Context.AllocateCopy(SubExprs),
                                      nullptr, RSquareLoc,
                                      /*hasTrailingClosure=*/false,
                                      /*Implicit=*/false);

  return makeParserResult(
      Status, new (Context) ArrayExpr(LSquareLoc, SubExpr, RSquareLoc));
}

/// parseExprDictionary - Parse a dictionary literal expression.
///
/// The lsquare-starting and first key have already been parsed, and
/// are passed in as parameters.
///
///   expr-dictionary:
///     lsquare-starting expr ':' expr (',' expr ':' expr)* ']'
ParserResult<Expr> Parser::parseExprDictionary(SourceLoc LSquareLoc,
                                               Expr *FirstKey) {
  // Each subexpression is a (key, value) tuple. 
  // FIXME: We're not tracking the colon locations in the AST.
  SmallVector<Expr *, 8> SubExprs;
  SourceLoc RSquareLoc;
  ParserStatus Status;

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
                                               /*hasTrailingClosure=*/false,
                                               /*Implicit=*/false));
  };

  // Parse the first value.
  ParserResult<Expr> FirstValue
    = parseExpr(diag::expected_value_in_dictionary_literal);
  if (FirstValue.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
  Status |= FirstValue;
  if (FirstValue.isNonNull()) {
    // Add the first key/value pair.
    addKeyValuePair(FirstKey, FirstValue.get());
  }

  consumeIf(tok::comma);

  Status |= parseList(tok::r_square, LSquareLoc, RSquareLoc,
                      tok::comma, /*OptionalSep=*/false,
                      diag::expected_rsquare_array_expr,
                      [&] () -> ParserStatus {
    // Parse the next key.
    ParserResult<Expr> Key
      = parseExpr(diag::expected_key_in_dictionary_literal);
    if (Key.isNull() || Key.hasCodeCompletion())
      return Key;

    // Parse the ':'.
    if (Tok.isNot(tok::colon)) {
      diagnose(Tok, diag::expected_colon_in_dictionary_literal);
      return makeParserError();
    }
    consumeToken();

    // Parse the next value.
    ParserResult<Expr> Value
      = parseExpr(diag::expected_value_in_dictionary_literal);
    if (Value.isNull() || Value.hasCodeCompletion())
      return Value;

    // Add this key/value pair.
    addKeyValuePair(Key.get(), Value.get());
    return makeParserSuccess();
  });

  if (Status.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();

  assert(SubExprs.size() >= 1);

  Expr *SubExpr;
  if (SubExprs.size() == 1)
    SubExpr = new (Context) ParenExpr(LSquareLoc, SubExprs[0],
                                      RSquareLoc,
                                      /*hasTrailingClosure=*/false);
  else
    SubExpr = new (Context) TupleExpr(LSquareLoc,
                                      Context.AllocateCopy(SubExprs),
                                      nullptr, RSquareLoc,
                                      /*hasTrailingClosure=*/false,
                                      /*Implicit=*/false);

  return makeParserResult(
      new (Context) DictionaryExpr(LSquareLoc, SubExpr, RSquareLoc));
}

/// \brief Walk the given pattern adding named parameters to the current scope.
/// This also causes redefinition errors to be emitted.
static void addFunctionParametersToScope(const Pattern *Pat, DeclContext *DC,
                                         Parser &P) {
  switch (Pat->getKind()) {
  case PatternKind::Named: {
    // Reparent the decl and add it to the scope.
    VarDecl *VD = cast<NamedPattern>(Pat)->getDecl();
    VD->setDeclContext(DC);
    P.addToScope(VD);
    return;
  }

  case PatternKind::Any:
    return;

  case PatternKind::Paren:
    addFunctionParametersToScope(cast<ParenPattern>(Pat)->getSubPattern(),
                                 DC, P);
    return;

  case PatternKind::Typed:
    addFunctionParametersToScope(cast<TypedPattern>(Pat)->getSubPattern(),
                                 DC, P);
    return;

  case PatternKind::Tuple:
    for (const TuplePatternElt &field : cast<TuplePattern>(Pat)->getFields())
      addFunctionParametersToScope(field.getPattern(), DC, P);
    return;
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
    llvm_unreachable("pattern can't appear as a func argument!");
  }

  llvm_unreachable("bad pattern kind!");
}

void Parser::addFunctionParametersToScope(ArrayRef<Pattern *> BodyPatterns,
                                          DeclContext *DC) {
  for (Pattern *P : BodyPatterns)
    ::addFunctionParametersToScope(P, DC, *this);
}

