//===--- ParseExpr.cpp - Swift Language Parser for Expressions ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Expression Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/Basic/EditorPlaceholder.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/SyntaxParsingContext.h"
#include "swift/Syntax/SyntaxBuilders.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/TokenSyntax.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::syntax;

/// parseExpr
///
///   expr:
///     expr-sequence(basic | trailing-closure)
///
/// \param isExprBasic Whether we're only parsing an expr-basic.
ParserResult<Expr> Parser::parseExprImpl(Diag<> Message,
                                         bool isExprBasic) {
  // Start a context for creating expression syntax.
  SyntaxParsingContext ExprParsingContext(SyntaxContext, SyntaxContextKind::Expr);

  // If we are parsing a refutable pattern, check to see if this is the start
  // of a let/var/is pattern.  If so, parse it to an UnresolvedPatternExpr and
  // name binding will perform final validation.
  //
  // Only do this if we're parsing a pattern, to improve QoI on malformed
  // expressions followed by (e.g.) let/var decls.
  //
  if (InVarOrLetPattern && isOnlyStartOfMatchingPattern()) {
    ParserResult<Pattern> pattern = parseMatchingPattern(/*isExprBasic*/false);
    if (pattern.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (pattern.isNull())
      return nullptr;
    SyntaxContext->setCreateSyntax(SyntaxKind::UnresolvedPatternExpr);
    return makeParserResult(new (Context) UnresolvedPatternExpr(pattern.get()));
  }
  
  auto expr = parseExprSequence(Message, isExprBasic,
                                /*forConditionalDirective*/false);
  if (expr.hasCodeCompletion())
    return expr;
  if (expr.isNull())
    return nullptr;
  
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

  return makeParserResult(new (Context) IsExpr(isLoc, type.get()));
}

/// parseExprAs
///   expr-as:
///     'as' type
///     'as?' type
///     'as!' type
ParserResult<Expr> Parser::parseExprAs() {
  // Parse the 'as'.
  SourceLoc asLoc = consumeToken(tok::kw_as);

  // Parse the postfix '?'.
  SourceLoc questionLoc;
  SourceLoc exclaimLoc;
  if (Tok.is(tok::question_postfix)) {
    questionLoc = consumeToken(tok::question_postfix);
  } else if (Tok.is(tok::exclaim_postfix)) {
    exclaimLoc = consumeToken(tok::exclaim_postfix);
  }

  ParserResult<TypeRepr> type = parseType(diag::expected_type_after_as);

  if (type.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
  if (type.isNull())
    return nullptr;

  Expr *parsed;
  if (questionLoc.isValid()) {
    parsed = new (Context) ConditionalCheckedCastExpr(asLoc, questionLoc,
                                                      type.get());
  } else if (exclaimLoc.isValid()) {
    parsed = new (Context) ForcedCheckedCastExpr(asLoc, exclaimLoc, type.get());
  } else {
    parsed = new (Context) CoerceExpr(asLoc, type.get());
  }
  return makeParserResult(parsed);
}

/// parseExprArrow
///
///   expr-arrow:
///     '->'
///     'throws' '->'
ParserResult<Expr> Parser::parseExprArrow() {
  SourceLoc throwsLoc, arrowLoc;
  if (Tok.is(tok::kw_throws)) {
    throwsLoc = consumeToken(tok::kw_throws);
    if (!Tok.is(tok::arrow)) {
      diagnose(throwsLoc, diag::throws_in_wrong_position);
      return nullptr;
    }
  }
  arrowLoc = consumeToken(tok::arrow);
  if (Tok.is(tok::kw_throws)) {
    diagnose(Tok.getLoc(), diag::throws_in_wrong_position);
    throwsLoc = consumeToken(tok::kw_throws);
  }
  auto arrow = new (Context) ArrowExpr(throwsLoc, arrowLoc);
  return makeParserResult(arrow);
}

/// parseExprSequence
///
///   expr-sequence(Mode):
///     expr-sequence-element(Mode) expr-binary(Mode)*
///   expr-binary(Mode):
///     operator-binary expr-sequence-element(Mode)
///     '?' expr-sequence(Mode) ':' expr-sequence-element(Mode)
///     '=' expr-unary
///     expr-is
///     expr-as
///
/// The sequencing for binary exprs is not structural, i.e., binary operators
/// are not inherently right-associative. If present, '?' and ':' tokens must
/// match.
///
/// Similarly, the parsing of 'try' as part of expr-sequence-element
/// is not structural.  'try' is not permitted at arbitrary points in
/// a sequence; in the places it's permitted, it's hoisted out to
/// apply to everything to its right.
ParserResult<Expr> Parser::parseExprSequence(Diag<> Message,
                                             bool isExprBasic,
                                             bool isForConditionalDirective) {
  SyntaxParsingContext ExprSequnceContext(SyntaxContext, SyntaxContextKind::Expr);

  SmallVector<Expr*, 8> SequencedExprs;
  SourceLoc startLoc = Tok.getLoc();
  bool HasCodeCompletion = false;
  bool PendingTernary = false;

  while (true) {
    if (isForConditionalDirective && Tok.isAtStartOfLine())
      break;

    // Parse a unary expression.
    ParserResult<Expr> Primary =
      parseExprSequenceElement(Message, isExprBasic);

    HasCodeCompletion |= Primary.hasCodeCompletion();
    if (Primary.isNull()) {
      if (Primary.hasCodeCompletion()) {
        if (CodeCompletion) {
          CodeCompletion->setLeadingSequenceExprs(SequencedExprs);
        }
        return Primary;
      } else {
        return nullptr;
      }
    }
    SequencedExprs.push_back(Primary.get());

    // We know we can make a syntax node for ternary expression.
    if (PendingTernary) {
      SyntaxContext->createNodeInPlace(SyntaxKind::TernaryExpr);
      PendingTernary = false;
    }

    if (isForConditionalDirective && Tok.isAtStartOfLine())
      break;
    
parse_operator:
    switch (Tok.getKind()) {
    case tok::oper_binary_spaced:
    case tok::oper_binary_unspaced: {
      // If this is an "&& #available()" expression (or related things that
      // show up in a stmt-condition production), then don't eat it.
      //
      // These are not general expressions, and && is an infix operator,
      // so the code is invalid.  We get better recovery if we bail out from
      // this, because then we can produce a fixit to rewrite the && into a ,
      // if we're in a stmt-condition.
      if (Tok.getText() == "&&" &&
          peekToken().isAny(tok::pound_available,
                            tok::kw_let, tok::kw_var, tok::kw_case))
        goto done;
      
      // Parse the operator.
      SyntaxParsingContext OperatorContext(SyntaxContext,
                                           SyntaxKind::BinaryOperatorExpr);
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

      // Wait for the next expression to make a syntax node for ternary
      // expression.
      PendingTernary = true;
      break;
    }
        
    case tok::equal: {
      // If we're parsing an expression as the body of a refutable var/let
      // pattern, then an assignment doesn't make sense.  In a "if let"
      // statement the equals is the start of the condition, so don't parse it
      // as a binary operator.
      if (InVarOrLetPattern)
        goto done;
      SyntaxParsingContext AssignContext(SyntaxContext,
                                         SyntaxKind::AssignmentExpr);
      SourceLoc equalsLoc = consumeToken();
      auto *assign = new (Context) AssignExpr(equalsLoc);
      SequencedExprs.push_back(assign);
      Message = diag::expected_expr_assignment;
      if (Tok.is(tok::code_complete)) {
        if (CodeCompletion) {
          auto RHS = new (Context) ErrorExpr(
            SourceRange(Tok.getRange().getStart(), Tok.getRange().getEnd()));
          assign->setSrc(RHS);
          SequencedExprs.pop_back();
          assign->setDest(SequencedExprs.back());
          SequencedExprs.pop_back();
          SequencedExprs.push_back(assign);
          CodeCompletion->completeAssignmentRHS(assign);
        }
        consumeToken();
        if (!SequencedExprs.empty() && (SequencedExprs.size() & 1) == 0) {
          // Make sure we have odd number of sequence exprs.
          SequencedExprs.pop_back();
        }
        auto Result = SequencedExprs.size() == 1 ?
          makeParserResult(SequencedExprs[0]):
          makeParserResult(SequenceExpr::create(Context, SequencedExprs));
        Result.setHasCodeCompletion();
        return Result;
      }
      break;
    }
        
    case tok::kw_is: {
      SyntaxParsingContext IsContext(SyntaxContext, SyntaxKind::IsExpr);
      // Parse a type after the 'is' token instead of an expression.
      ParserResult<Expr> is = parseExprIs();
      if (is.isNull() || is.hasCodeCompletion())
        return is;
      
      // Store the expr itself as a placeholder RHS. The real RHS is the
      // type parameter stored in the node itself.
      SequencedExprs.push_back(is.get());
      SequencedExprs.push_back(is.get());
      
      // We already parsed the right operand as part of the 'is' production.
      // Jump directly to parsing another operator.
      goto parse_operator;
    }
        
    case tok::kw_as: {
      SyntaxParsingContext AsContext(SyntaxContext, SyntaxKind::AsExpr);
      ParserResult<Expr> as = parseExprAs();
      if (as.isNull() || as.hasCodeCompletion())
        return as;
        
      // Store the expr itself as a placeholder RHS. The real RHS is the
      // type parameter stored in the node itself.
      SequencedExprs.push_back(as.get());
      SequencedExprs.push_back(as.get());
      
      // We already parsed the right operand as part of the 'is' production.
      // Jump directly to parsing another operator.
      goto parse_operator;
    }

    case tok::arrow:
    case tok::kw_throws: {
      SyntaxParsingContext ArrowContext(SyntaxContext, SyntaxKind::ArrowExpr);
      ParserResult<Expr> arrow = parseExprArrow();
      if (arrow.isNull() || arrow.hasCodeCompletion())
        return arrow;
      SequencedExprs.push_back(arrow.get());
      break;
    }
        
    default:
      // If the next token is not a binary operator, we're done.
      goto done;
    }
  }
done:

  // For conditional directives, we stop parsing after a line break.
  if (isForConditionalDirective && (SequencedExprs.size() & 1) == 0) {
    diagnose(getEndOfPreviousLoc(),
             diag::incomplete_conditional_compilation_directive);
    return makeParserError();
  }

  // If we had semantic errors, just fail here.
  assert(!SequencedExprs.empty());

  // If we saw no operators, don't build a sequence.
  if (SequencedExprs.size() == 1) {
    auto Result = makeParserResult(SequencedExprs[0]);
    if (HasCodeCompletion)
      Result.setHasCodeCompletion();
    return Result;
  }

  ExprSequnceContext.createNodeInPlace(SyntaxKind::ExprList);
  ExprSequnceContext.setCreateSyntax(SyntaxKind::SequenceExpr);
  auto Result = makeParserResult(SequenceExpr::create(Context, SequencedExprs));
  if (HasCodeCompletion)
    Result.setHasCodeCompletion();
  return Result;
}

/// parseExprSequenceElement
///
///   expr-sequence-element(Mode):
///     'try' expr-unary(Mode)
///     'try' '?' expr-unary(Mode)
///     'try' '!' expr-unary(Mode)
///     expr-unary(Mode)
///
/// 'try' is not actually allowed at an arbitrary position of a
/// sequence, but this isn't enforced until sequence-folding.
ParserResult<Expr> Parser::parseExprSequenceElement(Diag<> message,
                                                    bool isExprBasic) {
  SyntaxParsingContext ElementContext(SyntaxContext,
                                      SyntaxContextKind::Expr);
  SourceLoc tryLoc;
  bool hadTry = consumeIf(tok::kw_try, tryLoc);
  Optional<Token> trySuffix;
  if (hadTry && Tok.isAny(tok::exclaim_postfix, tok::question_postfix)) {
    trySuffix = Tok;
    consumeToken();
  }

  // Try to parse '@' sign or 'inout' as a attributed typerepr.
  if (Tok.isAny(tok::at_sign, tok::kw_inout)) {
    bool isType = false;
    {
      BacktrackingScope backtrack(*this);
      isType = canParseType();
    }
    if (isType) {
      ParserResult<TypeRepr> ty = parseType();
      if (ty.isNonNull())
        return makeParserResult(
            new (Context) TypeExpr(TypeLoc(ty.get(), Type())));
      checkForInputIncomplete();
      return nullptr;
    }
  }

  ParserResult<Expr> sub = parseExprUnary(message, isExprBasic);

  if (hadTry && !sub.hasCodeCompletion() && !sub.isNull()) {
    ElementContext.setCreateSyntax(SyntaxKind::TryExpr);
    switch (trySuffix ? trySuffix->getKind() : tok::NUM_TOKENS) {
    case tok::exclaim_postfix:
      sub = makeParserResult(
          new (Context) ForceTryExpr(tryLoc, sub.get(), trySuffix->getLoc()));
      break;
    case tok::question_postfix:
      sub = makeParserResult(
          new (Context) OptionalTryExpr(tryLoc, sub.get(),
                                        trySuffix->getLoc()));
      break;
    default:
      // If this is a simple "try expr" situation, where the expr is a closure
      // literal, and the next token is a 'catch', then the user wrote
      // try/catch instead of do/catch.  Emit a fixit hint to rewrite to the
      // correct do/catch construct.
      if (Tok.is(tok::kw_catch) && isa<ClosureExpr>(sub.get())) {
        diagnose(tryLoc, diag::docatch_not_trycatch)
          .fixItReplace(tryLoc, "do");
        
        // Eat all of the catch clauses, so we don't trip over them in error
        // recovery.
        while (Tok.is(tok::kw_catch)) {
          ParserResult<CatchStmt> clause = parseStmtCatch();
          if (clause.hasCodeCompletion() && clause.isNull())
            break;
        }

        return makeParserResult(new (Context) ErrorExpr(tryLoc));
      }
        
      sub = makeParserResult(new (Context) TryExpr(tryLoc, sub.get()));
      break;
    }
  }

  return sub;
}

static Expr *formUnaryArgument(ASTContext &context, Expr *argument) {
  if (isa<ParenExpr>(argument))
    return argument;

  auto *arg = new (context)
      ParenExpr(argument->getStartLoc(), argument, argument->getEndLoc(),
                /*hasTrailingClosure*/ false);
  arg->setImplicit();
  return arg;
}

/// parseExprUnary
///
///   expr-unary(Mode):
///     expr-postfix(Mode)
///     operator-prefix expr-unary(Mode)
///     '&' expr-unary(Mode)
///
ParserResult<Expr> Parser::parseExprUnary(Diag<> Message, bool isExprBasic) {
  SyntaxParsingContext UnaryContext(SyntaxContext, SyntaxContextKind::Expr);
  UnresolvedDeclRefExpr *Operator;
  switch (Tok.getKind()) {
  default:
    // If the next token is not an operator, just parse this as expr-postfix.
    return parseExprPostfix(Message, isExprBasic);

  case tok::amp_prefix: {
    SyntaxParsingContext AmpCtx(SyntaxContext, SyntaxKind::InOutExpr);
    SourceLoc Loc = consumeToken(tok::amp_prefix);

    ParserResult<Expr> SubExpr = parseExprUnary(Message, isExprBasic);
    if (SubExpr.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (SubExpr.isNull())
      return nullptr;
    return makeParserResult(
        new (Context) InOutExpr(Loc, SubExpr.get(), Type()));
  }

  case tok::backslash:
    return parseExprKeyPath();

  case tok::oper_postfix:
    // Postfix operators cannot start a subexpression, but can happen
    // syntactically because the operator may just follow whatever precedes this
    // expression (and that may not always be an expression).
    diagnose(Tok, diag::invalid_postfix_operator);
    Tok.setKind(tok::oper_prefix);
    LLVM_FALLTHROUGH;
  case tok::oper_prefix:
    Operator = parseExprOperator();
    break;
  case tok::oper_binary_spaced:
  case tok::oper_binary_unspaced: {
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
  ParserStatus Status = SubExpr;
  if (SubExpr.isNull())
    return Status;

  // We are sure we can create a prefix prefix operator expr now.
  UnaryContext.setCreateSyntax(SyntaxKind::PrefixOperatorExpr);

  // Check if we have a unary '-' with number literal sub-expression, for
  // example, "-42" or "-1.25".
  if (auto *LE = dyn_cast<NumberLiteralExpr>(SubExpr.get())) {
    if (Operator->hasName() && Operator->getName().getBaseName() == "-") {
      LE->setNegative(Operator->getLoc());
      return makeParserResult(Status, LE);
    }
  }

  return makeParserResult(
      Status, new (Context) PrefixUnaryExpr(
                  Operator, formUnaryArgument(Context, SubExpr.get())));
}

/// expr-keypath-swift:
///   \ type? . initial-key-path-component key-path-components
///
/// key-path-components:
//    key-path-component*
///   <empty>
///
/// key-path-component:
///   .identifier
///   ?
///   !
///   [ expression ]
///
/// initial-key-path-component:
///   identifier
///   ?
///   !
///   [ expression ]
ParserResult<Expr> Parser::parseExprKeyPath() {
  SyntaxParsingContext KeyPathCtx(SyntaxContext, SyntaxKind::KeyPathExpr);
  // Consume '\'.
  SourceLoc backslashLoc = consumeToken(tok::backslash);
  llvm::SaveAndRestore<bool> S(InSwiftKeyPath, true);

  // FIXME: diagnostics
  ParserResult<Expr> rootResult, pathResult;
  if (!startsWithSymbol(Tok, '.')) {
    rootResult = parseExprPostfix(diag::expr_keypath_expected_expr,
                                  /*isBasic=*/true);

    if (rootResult.isParseError())
      return rootResult;
  }

  if (startsWithSymbol(Tok, '.')) {
    SyntaxParsingContext ExprContext(SyntaxContext, SyntaxContextKind::Expr);

    auto dotLoc = Tok.getLoc();
    // For uniformity, \.foo is parsed as if it were MAGIC.foo, so we need to
    // make sure the . is there, but parsing the ? in \.? as .? doesn't make
    // sense. This is all made more complicated by .?. being considered an
    // operator token. Since keypath allows '.!' '.?' and '.[', consume '.'
    // the token is a operator starts with '.', or the following token is '['.
    if ((Tok.isAnyOperator() && Tok.getLength() != 1) ||
        peekToken().is(tok::l_square)) {
      SyntaxParsingContext KeyPathBaseContext(SyntaxContext,
                                              SyntaxKind::KeyPathBaseExpr);
      consumeStartingCharacterOfCurrentToken(tok::period);
    }

    auto inner = makeParserResult(new (Context) KeyPathDotExpr(dotLoc));
    bool unusedHasBindOptional = false;

    // Inside a keypath's path, the period always behaves normally: the key path
    // behavior is only the separation between type and path.
    pathResult = parseExprPostfixSuffix(inner, /*isExprBasic=*/true,
                                        /*periodHasKeyPathBehavior=*/false,
                                        unusedHasBindOptional);
    if (pathResult.isParseError())
      return pathResult;
  }

  auto keypath = new (Context) KeyPathExpr(
      backslashLoc, rootResult.getPtrOrNull(), pathResult.getPtrOrNull());

  // Handle code completion.
  if ((Tok.is(tok::code_complete) && !Tok.isAtStartOfLine()) ||
      (Tok.is(tok::period) && peekToken().isAny(tok::code_complete))) {
    SourceLoc DotLoc;
    consumeIf(tok::period, DotLoc);
    if (CodeCompletion)
      CodeCompletion->completeExprKeyPath(keypath, DotLoc);
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionResult(keypath);
  }

  return makeParserResult(keypath);
}

///   expr-keypath-objc:
///     '#keyPath' '(' unqualified-name ('.' unqualified-name) * ')'
///
ParserResult<Expr> Parser::parseExprKeyPathObjC() {
  SyntaxParsingContext ObjcKPCtx(SyntaxContext, SyntaxKind::ObjcKeyPathExpr);
  // Consume '#keyPath'.
  SourceLoc keywordLoc = consumeToken(tok::pound_keyPath);

  // Parse the leading '('.
  if (!Tok.is(tok::l_paren)) {
    diagnose(Tok, diag::expr_keypath_expected_lparen);
    return makeParserError();
  }
  SourceLoc lParenLoc = consumeToken(tok::l_paren);

  SmallVector<KeyPathExpr::Component, 4> components;
  /// Handler for code completion.
  auto handleCodeCompletion = [&](SourceLoc DotLoc) -> ParserResult<Expr> {
    KeyPathExpr *expr = nullptr;
    if (!components.empty()) {
      expr = new (Context)
          KeyPathExpr(Context, keywordLoc, lParenLoc, components, Tok.getLoc());
    }

    if (CodeCompletion)
      CodeCompletion->completeExprKeyPath(expr, DotLoc);

    // Eat the code completion token because we handled it.
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionResult(expr);
  };

  // Parse the sequence of unqualified-names.
  ParserStatus status;
  SourceLoc LastDotLoc;
  while (true) {
    SyntaxParsingContext NamePieceCtx(SyntaxContext, SyntaxKind::ObjcNamePiece);
    // Handle code completion.
    if (Tok.is(tok::code_complete))
      return handleCodeCompletion(LastDotLoc);

    // Parse the next name.
    DeclNameLoc nameLoc;
    bool afterDot = !components.empty();
    auto name = parseUnqualifiedDeclName(
                  afterDot, nameLoc, 
                  diag::expr_keypath_expected_property_or_type);
    if (!name) {
      status.setIsParseError();
      break;
    }

    // Record the name we parsed.
    auto component = KeyPathExpr::Component::forUnresolvedProperty(name,
                                                      nameLoc.getBaseNameLoc());
    components.push_back(component);

    // Handle code completion.
    if (Tok.is(tok::code_complete))
      return handleCodeCompletion(SourceLoc());

    // Parse the next period to continue the path.
    if (consumeIf(tok::period, LastDotLoc))
      continue;

    break;
  }

  // Collect all name pieces to an objc name.
  SyntaxContext->collectNodesInPlace(SyntaxKind::ObjcName);

  // Parse the closing ')'.
  SourceLoc rParenLoc;
  if (status.isError()) {
    skipUntilDeclStmtRBrace(tok::r_paren);
    if (Tok.is(tok::r_paren))
      rParenLoc = consumeToken();
    else
      rParenLoc = PreviousLoc;
  } else {
    parseMatchingToken(tok::r_paren, rParenLoc,
                       diag::expr_keypath_expected_rparen, lParenLoc);
  }

  // If we cannot build a useful expression, just return an error
  // expression.
  if (components.empty() || status.isError()) {
    return makeParserResult<Expr>(
             new (Context) ErrorExpr(SourceRange(keywordLoc, rParenLoc)));
  }

  // We're done: create the key-path expression.
  return makeParserResult<Expr>(new (Context) KeyPathExpr(
      Context, keywordLoc, lParenLoc, components, rParenLoc));
}

/// parseExprSelector
///
///   expr-selector:
///     '#selector' '(' expr ')'
///     '#selector' '(' 'getter' ':' expr ')'
///     '#selector' '(' 'setter' ':' expr ')'
///
ParserResult<Expr> Parser::parseExprSelector() {
  SyntaxParsingContext ExprCtxt(SyntaxContext, SyntaxKind::ObjcSelectorExpr);
  // Consume '#selector'.
  SourceLoc keywordLoc = consumeToken(tok::pound_selector);

  // Parse the leading '('.
  if (!Tok.is(tok::l_paren)) {
    diagnose(Tok, diag::expr_selector_expected_lparen);
    return makeParserError();
  }
  SourceLoc lParenLoc = consumeToken(tok::l_paren);
  SourceLoc modifierLoc;

  // Parse possible 'getter:' or 'setter:' modifiers, and determine
  // the kind of selector we're working with.
  ObjCSelectorExpr::ObjCSelectorKind selectorKind;
  if (peekToken().is(tok::colon) &&
      (Tok.isContextualKeyword("getter") ||
       Tok.isContextualKeyword("setter"))) {
    // Parse the modifier.
    if (Tok.isContextualKeyword("getter"))
      selectorKind = ObjCSelectorExpr::Getter;
    else
      selectorKind = ObjCSelectorExpr::Setter;

    Tok.setKind(tok::contextual_keyword);
    modifierLoc = consumeToken();
    (void)consumeToken(tok::colon);
  } else {
    selectorKind = ObjCSelectorExpr::Method;
  }

  ObjCSelectorContext selectorContext;
  switch (selectorKind) {
  case ObjCSelectorExpr::Getter:
    selectorContext = ObjCSelectorContext::GetterSelector;
    break;
  case ObjCSelectorExpr::Setter:
    selectorContext = ObjCSelectorContext::SetterSelector;
    break;
  case ObjCSelectorExpr::Method:
    selectorContext = ObjCSelectorContext::MethodSelector;
  }

  // Parse the subexpression.
  CodeCompletionCallbacks::InObjCSelectorExprRAII
    InObjCSelectorExpr(CodeCompletion, selectorContext);
  ParserResult<Expr> subExpr =
    parseExpr(selectorKind == ObjCSelectorExpr::Method
                ? diag::expr_selector_expected_method_expr
                : diag::expr_selector_expected_property_expr);
  if (subExpr.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();

  // Parse the closing ')'.
  SourceLoc rParenLoc;
  if (subExpr.isParseError()) {
    skipUntilDeclStmtRBrace(tok::r_paren);
    if (Tok.is(tok::r_paren))
      rParenLoc = consumeToken();
    else
      rParenLoc = PreviousLoc;
  } else {
    parseMatchingToken(tok::r_paren, rParenLoc,
                       diag::expr_selector_expected_rparen, lParenLoc);
  }

  // If the subexpression was in error, just propagate the error.
  if (subExpr.isParseError())
    return makeParserResult<Expr>(
      new (Context) ErrorExpr(SourceRange(keywordLoc, rParenLoc)));

  return makeParserResult<Expr>(
    new (Context) ObjCSelectorExpr(selectorKind, keywordLoc, lParenLoc,
                                   modifierLoc, subExpr.get(), rParenLoc));
}

static DeclRefKind getDeclRefKindForOperator(tok kind) {
  switch (kind) {
  case tok::oper_binary_spaced:
  case tok::oper_binary_unspaced:  return DeclRefKind::BinaryOperator;
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
  return new (Context) UnresolvedDeclRefExpr(name, refKind, DeclNameLoc(loc));
}

static VarDecl *getImplicitSelfDeclForSuperContext(Parser &P,
                                                   DeclContext *DC,
                                                   SourceLoc Loc) {
  auto *methodContext = DC->getInnermostMethodContext();
  if (!methodContext) {
    P.diagnose(Loc, diag::super_not_in_class_method);
    return nullptr;
  }

  // Do an actual lookup for 'self' in case it shows up in a capture list.
  auto *methodSelf = methodContext->getImplicitSelfDecl();
  auto *lookupSelf = P.lookupInScope(P.Context.Id_self);
  if (lookupSelf && lookupSelf != methodSelf) {
    // FIXME: This is the wrong diagnostic for if someone manually declares a
    // variable named 'self' using backticks.
    P.diagnose(Loc, diag::super_in_closure_with_capture);
    P.diagnose(lookupSelf->getLoc(), diag::super_in_closure_with_capture_here);
    return nullptr;
  }

  return methodSelf;
}

/// parseExprSuper
///
///   expr-super:
///     expr-super-member
///     expr-super-init
///     expr-super-subscript
///   expr-super-member:
///     'super' '.' identifier
///   expr-super-init:
///     'super' '.' 'init'
///   expr-super-subscript:
///     'super' '[' expr ']'
ParserResult<Expr> Parser::parseExprSuper(bool isExprBasic) {
  SyntaxParsingContext SuperCtxt(SyntaxContext, SyntaxContextKind::Expr);
  // Parse the 'super' reference.
  SourceLoc superLoc = consumeToken(tok::kw_super);
  
  VarDecl *selfDecl = getImplicitSelfDeclForSuperContext(*this,
                                                         CurDeclContext,
                                                         superLoc);
  bool ErrorOccurred = selfDecl == nullptr;

  SyntaxContext->createNodeInPlace(SyntaxKind::SuperRefExpr);
  Expr *superRef = !ErrorOccurred
    ? cast<Expr>(new (Context) SuperRefExpr(selfDecl, superLoc,
                                            /*Implicit=*/false))
    : cast<Expr>(new (Context) ErrorExpr(superLoc));
  
  if (Tok.isAny(tok::period, tok::period_prefix)) {
    // 'super.' must be followed by a member or initializer ref.

    SourceLoc dotLoc = consumeToken();
    
    if (Tok.is(tok::code_complete)) {
      if (CodeCompletion) {
        if (auto *SRE = dyn_cast<SuperRefExpr>(superRef))
          CodeCompletion->completeExprSuperDot(SRE);
      }

      // Eat the code completion token because we handled it.
      consumeToken(tok::code_complete);
      return makeParserCodeCompletionResult(superRef);
    }

    DeclNameLoc nameLoc;
    DeclName name = parseUnqualifiedDeclName(/*afterDot=*/true, nameLoc,
                      diag::expected_identifier_after_super_dot_expr);
    if (!name)
      return nullptr;

    SyntaxContext->createNodeInPlace(SyntaxKind::MemberAccessExpr);
    return makeParserResult(
             new (Context) UnresolvedDotExpr(superRef, dotLoc, name, nameLoc,
                                             /*Implicit=*/false));
  }

  if (Tok.isFollowingLSquare()) {
    // super[expr]
    SourceLoc lSquareLoc, rSquareLoc;
    SmallVector<Expr *, 2> indexArgs;
    SmallVector<Identifier, 2> indexArgLabels;
    SmallVector<SourceLoc, 2> indexArgLabelLocs;
    Expr *trailingClosure;

    ParserStatus status = parseExprList(tok::l_square, tok::r_square,
                                        /*isPostfix=*/true, isExprBasic,
                                        lSquareLoc, indexArgs, indexArgLabels,
                                        indexArgLabelLocs,
                                        rSquareLoc,
                                        trailingClosure,
                                        SyntaxKind::FunctionCallArgumentList);
    if (status.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (status.isError())
      return nullptr;
    SyntaxContext->createNodeInPlace(SyntaxKind::SubscriptExpr);
    return makeParserResult(
      SubscriptExpr::create(Context, superRef, lSquareLoc, indexArgs,
                            indexArgLabels, indexArgLabelLocs, rSquareLoc,
                            trailingClosure, ConcreteDeclRef(),
                            /*implicit=*/false));
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
  
  if (consumeIf(tok::unknown))
    return nullptr;
  
  diagnose(Tok, diag::expected_dot_or_subscript_after_super);
  return nullptr;
}

/// Copy a numeric literal value into AST-owned memory, stripping underscores
/// so the semantic part of the value can be parsed by APInt/APFloat parsers.
StringRef Parser::copyAndStripUnderscores(StringRef orig) {
  char *start = static_cast<char*>(Context.Allocate(orig.size(), 1));
  char *p = start;

  if (p) {
    for (char c : orig) {
      if (c != '_') {
        *p++ = c;
      }
    }
  }
  
  return StringRef(start, p - start);
}

/// Disambiguate the parse after '{' token that is in a place that might be
/// the start of a trailing closure, or start the variable accessor block.
///
/// Check to see if the '{' is followed by a 'didSet' or a 'willSet' label,
/// possibly preceded by attributes.  If so, we disambiguate the parse as the
/// start of a get-set block in a variable definition (not as a trailing
/// closure).
static bool isStartOfGetSetAccessor(Parser &P) {
  assert(P.Tok.is(tok::l_brace) && "not checking a brace?");
  
  // The only case this can happen is if the accessor label is immediately after
  // a brace (possibly preceded by attributes).  "get" is implicit, so it can't
  // be checked for.  Conveniently however, get/set properties are not allowed
  // to have initializers, so we don't have an ambiguity, we just have to check
  // for observing accessors.
  //
  // If we have a 'didSet' or a 'willSet' label, disambiguate immediately as
  // an accessor block.
  Token NextToken = P.peekToken();
  if (NextToken.isContextualKeyword("didSet") ||
      NextToken.isContextualKeyword("willSet"))
    return true;

  // If we don't have attributes, then it cannot be an accessor block.
  if (NextToken.isNot(tok::at_sign))
    return false;

  Parser::BacktrackingScope Backtrack(P);

  // Eat the "{".
  P.consumeToken(tok::l_brace);

  // Eat attributes, if present.
  while (P.consumeIf(tok::at_sign)) {
    if (!P.consumeIf(tok::identifier)) return false;
    // Eat paren after attribute name; e.g. @foo(x)
    if (P.Tok.is(tok::l_paren)) P.skipSingle();
  }

  // Check if we have 'didSet'/'willSet' after attributes.
  return P.Tok.isContextualKeyword("didSet") ||
         P.Tok.isContextualKeyword("willSet");
}

/// Recover invalid uses of trailing closures in a situation
/// where the parser requires an expr-basic (which does not allow them).  We
/// handle this by doing some lookahead in common situations. And later, Sema
/// will emit a diagnostic with a fixit to add wrapping parens.
static bool isValidTrailingClosure(bool isExprBasic, Parser &P){
  assert(P.Tok.is(tok::l_brace) && "Couldn't be a trailing closure");
  
  // If this is the start of a get/set accessor, then it isn't a trailing
  // closure.
  if (isStartOfGetSetAccessor(P))
    return false;

  // If this is a normal expression (not an expr-basic) then trailing closures
  // are allowed, so this is obviously one.
  // TODO: We could handle try to disambiguate cases like:
  //   let x = foo
  //   {...}()
  // by looking ahead for the ()'s, but this has been replaced by do{}, so this
  // probably isn't worthwhile.
  //
  if (!isExprBasic)
    return true;
  
  // If this is an expr-basic, then a trailing closure is not allowed.  However,
  // it is very common for someone to write something like:
  //
  //    for _ in numbers.filter {$0 > 4} {
  //
  // and we want to recover from this very well.   We need to perform arbitrary
  // look-ahead to disambiguate this case, so we only do this in the case where
  // the token after the { is on the same line as the {.
  if (P.peekToken().isAtStartOfLine())
    return false;
  
  
  // Determine if the {} goes with the expression by eating it, and looking
  // to see if it is immediately followed by '{', 'where', or comma.  If so,
  // we consider it to be part of the proceeding expression.
  Parser::BacktrackingScope backtrack(P);
  P.consumeToken(tok::l_brace);
  P.skipUntil(tok::r_brace);
  SourceLoc endLoc;
  if (!P.consumeIf(tok::r_brace, endLoc) ||
      P.Tok.isNot(tok::l_brace, tok::kw_where, tok::comma)) {
    return false;
  }

  // Recoverable case. Just return true here and Sema will emit a diagnostic
  // later. see: Sema/MiscDiagnostics.cpp#checkStmtConditionTrailingClosure
  return true;
}



/// Map magic literal tokens such as #file to their
/// MagicIdentifierLiteralExpr kind.
static MagicIdentifierLiteralExpr::Kind
getMagicIdentifierLiteralKind(tok Kind) {
  switch (Kind) {
  case tok::kw___COLUMN__:
  case tok::pound_column:
    return MagicIdentifierLiteralExpr::Kind::Column;
  case tok::kw___FILE__:
  case tok::pound_file:
    return MagicIdentifierLiteralExpr::Kind::File;
  case tok::kw___FUNCTION__:
  case tok::pound_function:
    return MagicIdentifierLiteralExpr::Kind::Function;
  case tok::kw___LINE__:
  case tok::pound_line:
    return MagicIdentifierLiteralExpr::Kind::Line;
  case tok::kw___DSO_HANDLE__:
  case tok::pound_dsohandle:
    return MagicIdentifierLiteralExpr::Kind::DSOHandle;

  default:
    llvm_unreachable("not a magic literal");
  }
}

ParserResult<Expr>
Parser::parseExprPostfixSuffix(ParserResult<Expr> Result, bool isExprBasic,
                               bool periodHasKeyPathBehavior,
                               bool &hasBindOptional) {
  hasBindOptional = false;

  // Handle suffix expressions.
  while (1) {
    // FIXME: Better recovery.
    if (Result.isNull())
      return Result;

    if (Result.hasCodeCompletion() &&
        SourceMgr.getCodeCompletionLoc() == PreviousLoc) {
      // Don't parse suffixes if the expression ended with code completion
      // token. Because, for example, given:
      //   [.foo(), .bar()]
      // If user want to insert another element in between:
      //   [.foo(), <HERE> .bar()]
      // '.bar()' is probably not a part of the inserting element. Moreover,
      // having suffixes doesn't help type inference in any way.
      return Result;
    }

    // Check for a .foo suffix.
    SourceLoc TokLoc = Tok.getLoc();
    if (Tok.is(tok::period) || Tok.is(tok::period_prefix)) {
      // A key path is special, because it allows .[, unlike anywhere else. The
      // period itself should be left in the token stream. (.? and .! end up
      // being operators, and so aren't handled here.)
      if (periodHasKeyPathBehavior && peekToken().is(tok::l_square)) {
        break;
      }
      // Completion for keyPath expression is handled in parseExprKeyPath.
      if (InSwiftKeyPath && peekToken().is(tok::code_complete))
        break;

      Tok.setKind(tok::period);
      consumeToken();

      // Handle "x.42" - a tuple index.
      if (Tok.is(tok::integer_literal)) {
        DeclName name = Context.getIdentifier(Tok.getText());
        SourceLoc nameLoc = consumeToken(tok::integer_literal);
        SyntaxContext->createNodeInPlace(SyntaxKind::MemberAccessExpr);

        // Don't allow '.<integer literal>' following a numeric literal
        // expression (unless in #if env, for 1.2.3.4 version numbers)
        if (!InPoundIfEnvironment && Result.isNonNull() &&
            isa<NumberLiteralExpr>(Result.get())) {
          diagnose(nameLoc, diag::numeric_literal_numeric_member)
              .highlight(Result.get()->getSourceRange());
          continue;
        }

        Result = makeParserResult(
            Result, new (Context) UnresolvedDotExpr(Result.get(), TokLoc, name,
                                                    DeclNameLoc(nameLoc),
                                                    /*Implicit=*/false));
        continue;
      }

      // Handle "x.self" expr.
      if (Tok.is(tok::kw_self)) {
        Result = makeParserResult(
            Result,
            new (Context) DotSelfExpr(Result.get(), TokLoc, consumeToken()));
        SyntaxContext->createNodeInPlace(SyntaxKind::DotSelfExpr);
        continue;
      }

      // Handle the deprecated 'x.dynamicType' and migrate it to `type(of: x)`
      if (Tok.getText() == "dynamicType") {
        auto range = Result.get()->getSourceRange();
        auto dynamicTypeExprRange = SourceRange(TokLoc, Tok.getLoc());
        diagnose(TokLoc, diag::expr_dynamictype_deprecated)
            .highlight(dynamicTypeExprRange)
            .fixItReplace(dynamicTypeExprRange, ")")
            .fixItInsert(range.Start, "type(of: ");

        // fallthrough to an UnresolvedDotExpr.
      }

      // Handle "x.<tab>" for code completion.
      if (Tok.is(tok::code_complete)) {
        assert(!InSwiftKeyPath);
        if (CodeCompletion) {
          CodeCompletion->completeDotExpr(Result.get(), /*DotLoc=*/TokLoc);
        }
        // Eat the code completion token because we handled it.
        consumeToken(tok::code_complete);
        Result.setHasCodeCompletion();
        return Result;
      }

      DeclNameLoc NameLoc;
      DeclName Name = parseUnqualifiedDeclName(/*afterDot=*/true, NameLoc,
                                               diag::expected_member_name);
      if (!Name)
        return nullptr;
      SyntaxContext->createNodeInPlace(SyntaxKind::MemberAccessExpr);
      Result = makeParserResult(Result, new (Context) UnresolvedDotExpr(
                                            Result.get(), TokLoc, Name, NameLoc,
                                            /*Implicit=*/false));

      if (canParseAsGenericArgumentList()) {
        SmallVector<TypeRepr *, 8> args;
        SourceLoc LAngleLoc, RAngleLoc;
        if (parseGenericArguments(args, LAngleLoc, RAngleLoc)) {
          diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);
        }

        SmallVector<TypeLoc, 8> locArgs;
        for (auto ty : args)
          locArgs.push_back(ty);
        SyntaxContext->createNodeInPlace(SyntaxKind::SpecializeExpr);
        Result = makeParserResult(
            Result, UnresolvedSpecializeExpr::create(
                        Context, Result.get(), LAngleLoc, locArgs, RAngleLoc));
      }

      continue;
    }

    // If there is an expr-call-suffix, parse it and form a call.
    if (Tok.isFollowingLParen()) {
      Result = parseExprCallSuffix(Result, isExprBasic);
      SyntaxContext->createNodeInPlace(SyntaxKind::FunctionCallExpr);
      continue;
    }

    // Check for a [expr] suffix.
    // Note that this cannot be the start of a new line.
    if (Tok.isFollowingLSquare()) {
      SourceLoc lSquareLoc, rSquareLoc;
      SmallVector<Expr *, 2> indexArgs;
      SmallVector<Identifier, 2> indexArgLabels;
      SmallVector<SourceLoc, 2> indexArgLabelLocs;
      Expr *trailingClosure;

      ParserStatus status = parseExprList(
          tok::l_square, tok::r_square,
          /*isPostfix=*/true, isExprBasic, lSquareLoc, indexArgs,
          indexArgLabels, indexArgLabelLocs, rSquareLoc, trailingClosure,
          SyntaxKind::FunctionCallArgumentList);
      Result = makeParserResult(
          status | Result,
          SubscriptExpr::create(Context, Result.get(), lSquareLoc, indexArgs,
                                indexArgLabels, indexArgLabelLocs, rSquareLoc,
                                trailingClosure, ConcreteDeclRef(),
                                /*implicit=*/false));
      SyntaxContext->createNodeInPlace(SyntaxKind::SubscriptExpr);
      continue;
    }

    // Check for a trailing closure, if allowed.
    if (Tok.is(tok::l_brace) && isValidTrailingClosure(isExprBasic, *this)) {
      // FIXME: if Result has a trailing closure, break out.

      // Stop after literal expressions, which may never have trailing closures.
      const auto *callee = Result.get();
      if (isa<LiteralExpr>(callee) || isa<CollectionExpr>(callee) ||
          isa<TupleExpr>(callee))
        break;

      if (SyntaxContext->isEnabled()) {
        // Add dummy blank argument list to the call expression syntax.
        SyntaxContext->addSyntax(
            SyntaxFactory::makeBlankFunctionCallArgumentList(
                SyntaxContext->getArena()));
      }

      ParserResult<Expr> closure =
          parseTrailingClosure(callee->getSourceRange());
      if (closure.isNull())
        return nullptr;

      // Trailing closure implicitly forms a call.
      Result = makeParserResult(
          ParserStatus(closure) | ParserStatus(Result),
          CallExpr::create(Context, Result.get(), SourceLoc(), {}, {}, {},
                           SourceLoc(), closure.get(), /*implicit=*/false));
      SyntaxContext->createNodeInPlace(SyntaxKind::FunctionCallExpr);

      // We only allow a single trailing closure on a call.  This could be
      // generalized in the future, but needs further design.
      if (Tok.is(tok::l_brace))
        break;
      continue;
    }

    // Check for a ? suffix.
    if (consumeIf(tok::question_postfix)) {
      Result = makeParserResult(Result, new (Context) BindOptionalExpr(
                                            Result.get(), TokLoc, /*depth*/ 0));
      SyntaxContext->createNodeInPlace(SyntaxKind::OptionalChainingExpr);
      hasBindOptional = true;
      continue;
    }

    // Check for a ! suffix.
    if (consumeIf(tok::exclaim_postfix)) {
      Result = makeParserResult(
          Result, new (Context) ForceValueExpr(Result.get(), TokLoc));
      SyntaxContext->createNodeInPlace(SyntaxKind::ForcedValueExpr);
      continue;
    }

    // Check for a postfix-operator suffix.
    if (Tok.is(tok::oper_postfix)) {
      // KeyPaths are more restricted in what can go after a ., and so we treat
      // them specially.
      if (periodHasKeyPathBehavior && startsWithSymbol(Tok, '.'))
        break;

      Expr *oper = parseExprOperator();

      Result = makeParserResult(
          Result, new (Context) PostfixUnaryExpr(
                      oper, formUnaryArgument(Context, Result.get())));
      SyntaxContext->createNodeInPlace(SyntaxKind::PostfixUnaryExpr);
      continue;
    }

    if (Tok.is(tok::code_complete)) {
      if (InSwiftKeyPath)
        return Result;

      if (Tok.isAtStartOfLine()) {
        // Postfix expression is located on a different line than the code
        // completion token, and thus they are not related.
        return Result;
      }

      if (CodeCompletion && Result.isNonNull()) {
        bool hasSpace = Tok.getLoc() != getEndOfPreviousLoc();
        CodeCompletion->completePostfixExpr(Result.get(), hasSpace);
      }
      // Eat the code completion token because we handled it.
      consumeToken(tok::code_complete);
      return makeParserCodeCompletionResult<Expr>();
    }

    // If we end up with an unknown token on this line, return an ErrorExpr
    // covering the range of the token.
    if (!Tok.isAtStartOfLine() && consumeIf(tok::unknown)) {
      Result = makeParserResult(
          Result, new (Context) ErrorExpr(Result.get()->getSourceRange()));
      continue;
    }

    // Otherwise, we don't know what this token is, it must end the expression.
    break;
  }

  return Result;
}

/// parseExprPostfix
///
///   expr-dot:
///     expr-postfix '.' 'type'
///     expr-postfix '.' (identifier|keyword) generic-args? expr-call-suffix?
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
///     expr-init
///     expr-subscript
///     expr-call
///     expr-force-value
///
///   expr-postfix(trailing-closure):
///     expr-postfix(basic)
///     expr-trailing-closure
///
ParserResult<Expr> Parser::parseExprPostfix(Diag<> ID, bool isExprBasic) {
  SyntaxParsingContext ExprContext(SyntaxContext, SyntaxContextKind::Expr);
  auto Result = parseExprPrimary(ID, isExprBasic);
  // If we couldn't parse any expr, don't attempt to parse suffixes.
  if (Result.isNull())
    return Result;

  bool hasBindOptional = false;
  Result = parseExprPostfixSuffix(Result, isExprBasic,
                                  /*periodHasKeyPathBehavior=*/InSwiftKeyPath,
                                  hasBindOptional);
  if (Result.isParseError() || Result.hasCodeCompletion())
    return Result;

  // If we had a ? suffix expression, bind the entire postfix chain
  // within an OptionalEvaluationExpr.
  if (hasBindOptional) {
    Result = makeParserResult(new (Context) OptionalEvaluationExpr(Result.get()));
  }

  return Result;
}

/// parseExprPrimary
///
///   expr-literal:
///     integer_literal
///     floating_literal
///     string_literal
///     nil
///     true
///     false
///     #file
///     #line
///     #column
///     #function
///     #dsohandle
///
///   expr-delayed-identifier:
///     '.' identifier
///
///   expr-discard:
///     '_'
///
///   expr-primary:
///     expr-literal
///     expr-identifier expr-call-suffix?
///     expr-closure
///     expr-anon-closure-argument
///     expr-delayed-identifier
///     expr-paren
///     expr-super
///     expr-discard
///     expr-selector
///
ParserResult<Expr> Parser::parseExprPrimary(Diag<> ID, bool isExprBasic) {
  SyntaxParsingContext ExprContext(SyntaxContext, SyntaxContextKind::Expr);
  switch (Tok.getKind()) {
  case tok::integer_literal: {
    StringRef Text = copyAndStripUnderscores(Tok.getText());
    SourceLoc Loc = consumeToken(tok::integer_literal);
    ExprContext.setCreateSyntax(SyntaxKind::IntegerLiteralExpr);
    return makeParserResult(new (Context)
                                IntegerLiteralExpr(Text, Loc,
                                                   /*Implicit=*/false));
  }
  case tok::floating_literal: {
    StringRef Text = copyAndStripUnderscores(Tok.getText());
    SourceLoc Loc = consumeToken(tok::floating_literal);
    ExprContext.setCreateSyntax(SyntaxKind::FloatLiteralExpr);
    return makeParserResult(new (Context) FloatLiteralExpr(Text, Loc,
                                                           /*Implicit=*/false));
  }
  case tok::at_sign:
    // Objective-C programmers habitually type @"foo", so recover gracefully
    // with a fixit.  If this isn't @"foo", just handle it like an unknown
    // input.
    if (peekToken().isNot(tok::string_literal))
      goto UnknownCharacter;
    
    diagnose(Tok.getLoc(), diag::string_literal_no_atsign)
      .fixItRemove(Tok.getLoc());
    consumeToken(tok::at_sign);
    LLVM_FALLTHROUGH;
      
  case tok::string_literal:  // "foo"
    return parseExprStringLiteral();
  
  case tok::kw_nil:
    ExprContext.setCreateSyntax(SyntaxKind::NilLiteralExpr);
    return makeParserResult(new (Context)
                                NilLiteralExpr(consumeToken(tok::kw_nil)));

  case tok::kw_true:
  case tok::kw_false: {
    ExprContext.setCreateSyntax(SyntaxKind::BooleanLiteralExpr);
    bool isTrue = Tok.is(tok::kw_true);
    return makeParserResult(new (Context)
                                BooleanLiteralExpr(isTrue, consumeToken()));
  }

  case tok::kw___FILE__:
  case tok::kw___LINE__:
  case tok::kw___COLUMN__:
  case tok::kw___FUNCTION__:
  case tok::kw___DSO_HANDLE__: {
    StringRef replacement = "";
    switch (Tok.getKind()) {
    default: llvm_unreachable("can't get here");
    case tok::kw___FILE__: replacement = "#file"; break;
    case tok::kw___LINE__: replacement = "#line"; break;
    case tok::kw___COLUMN__: replacement = "#column"; break;
    case tok::kw___FUNCTION__:  replacement = "#function"; break;
    case tok::kw___DSO_HANDLE__: replacement = "#dsohandle"; break;
    }

    diagnose(Tok.getLoc(), diag::snake_case_deprecated,
             Tok.getText(), replacement)
      .fixItReplace(Tok.getLoc(), replacement);
    LLVM_FALLTHROUGH;
  }
  case tok::pound_column:
  case tok::pound_file:
  case tok::pound_function:
  case tok::pound_line:
  case tok::pound_dsohandle: {
    SyntaxKind SKind = SyntaxKind::UnknownExpr;
    switch (Tok.getKind()) {
    case tok::pound_column: SKind = SyntaxKind::PoundColumnExpr; break;
    case tok::pound_file: SKind = SyntaxKind::PoundFileExpr; break;
    case tok::pound_function: SKind = SyntaxKind::PoundFunctionExpr; break;
    // FIXME: #line was renamed to #sourceLocation
    case tok::pound_line: SKind = SyntaxKind::PoundLineExpr; break;
    case tok::pound_dsohandle: SKind = SyntaxKind::PoundDsohandleExpr; break;
    default: break;
    }
    ExprContext.setCreateSyntax(SKind);
    auto Kind = getMagicIdentifierLiteralKind(Tok.getKind());
    SourceLoc Loc = consumeToken();
    return makeParserResult(new (Context) MagicIdentifierLiteralExpr(
        Kind, Loc, /*implicit=*/false));
  }
      
  case tok::identifier:  // foo
  case tok::kw_self:     // self

    // If we are parsing a refutable pattern and are inside a let/var pattern,
    // the identifiers change to be value bindings instead of decl references.
    // Parse and return this as an UnresolvedPatternExpr around a binding.  This
    // will be resolved (or rejected) by sema when the overall refutable pattern
    // it transformed from an expression into a pattern.
    if ((InVarOrLetPattern == IVOLP_ImplicitlyImmutable ||
         InVarOrLetPattern == IVOLP_InVar ||
         InVarOrLetPattern == IVOLP_InLet) &&
        // If we have "case let x." or "case let x(", we parse x as a normal
        // name, not a binding, because it is the start of an enum pattern or
        // call pattern.
        peekToken().isNot(tok::period, tok::period_prefix, tok::l_paren)) {
      Identifier name;
      SourceLoc loc = consumeIdentifier(&name);
      auto specifier = (InVarOrLetPattern != IVOLP_InVar)
                     ? VarDecl::Specifier::Let
                     : VarDecl::Specifier::Var;
      auto pattern = createBindingFromPattern(loc, name, specifier);
      if (SyntaxContext->isEnabled()) {
        PatternSyntax PatternNode = SyntaxFactory::makeIdentifierPattern(
            SyntaxContext->popToken(), SyntaxContext->getArena());
        ExprSyntax ExprNode = SyntaxFactory::makeUnresolvedPatternExpr(
            PatternNode, SyntaxContext->getArena());
        SyntaxContext->addSyntax(ExprNode);
      }
      return makeParserResult(new (Context) UnresolvedPatternExpr(pattern));
    }

    LLVM_FALLTHROUGH;
  case tok::kw_Self:     // Self
    return makeParserResult(parseExprIdentifier());

  case tok::kw_Any: { // Any
    ExprContext.setCreateSyntax(SyntaxKind::TypeExpr);
    auto TyR = parseAnyType();
    return makeParserResult(new (Context) TypeExpr(TypeLoc(TyR.get())));
  }

  case tok::dollarident: // $1
    return makeParserResult(parseExprAnonClosureArg());

  case tok::kw__: // _
    ExprContext.setCreateSyntax(SyntaxKind::DiscardAssignmentExpr);
    return makeParserResult(
      new (Context) DiscardAssignmentExpr(consumeToken(), /*Implicit=*/false));

  case tok::pound_selector: // expr-selector
    return parseExprSelector();

  case tok::pound_keyPath:
    return parseExprKeyPathObjC();

  case tok::l_brace:     // expr-closure
    return parseExprClosure();

  case tok::period:              //=.foo
  case tok::period_prefix: {     // .foo
    Tok.setKind(tok::period_prefix);
    SourceLoc DotLoc = consumeToken();
    
    // Special case ".<integer_literal>" like ".4".  This isn't valid, but the
    // developer almost certainly meant to use "0.4".  Diagnose this, and
    // recover as if they wrote that.
    if (Tok.is(tok::integer_literal) && !Tok.isAtStartOfLine()) {
      diagnose(DotLoc, diag::invalid_float_literal_missing_leading_zero,
               Tok.getText())
        .fixItInsert(DotLoc, "0")
        .highlight({DotLoc, Tok.getLoc()});
      char *Ptr = (char*)Context.Allocate(Tok.getLength()+2, 1);
      memcpy(Ptr, "0.", 2);
      memcpy(Ptr+2, Tok.getText().data(), Tok.getLength());
      auto FltText = StringRef(Ptr, Tok.getLength()+2);
      FltText = copyAndStripUnderscores(FltText);
      
      consumeToken(tok::integer_literal);
      return makeParserResult(new (Context)
                                  FloatLiteralExpr(FltText, DotLoc,
                                                   /*Implicit=*/false));
    }
    
    DeclName Name;
    DeclNameLoc NameLoc;

    if (Tok.is(tok::code_complete)) {
      auto CCE = new (Context) CodeCompletionExpr(Tok.getLoc());
      auto Result = makeParserResult(CCE);
      Result.setHasCodeCompletion();
      if (CodeCompletion) {
        CodeCompletion->completeUnresolvedMember(CCE, DotLoc);
      }
      consumeToken();
      return Result;
    }

    Name = parseUnqualifiedDeclName(/*afterDot=*/true, NameLoc,
                                    diag::expected_identifier_after_dot_expr);
    if (!Name) return nullptr;
    SyntaxContext->createNodeInPlace(SyntaxKind::ImplicitMemberExpr);

    // Check for a () suffix, which indicates a call when constructing
    // this member.  Note that this cannot be the start of a new line.
    if (Tok.isFollowingLParen()) {
      SourceLoc lParenLoc, rParenLoc;
      SmallVector<Expr *, 2> args;
      SmallVector<Identifier, 2> argLabels;
      SmallVector<SourceLoc, 2> argLabelLocs;
      Expr *trailingClosure;
      
      ParserStatus status = parseExprList(tok::l_paren, tok::r_paren,
                                          /*isPostfix=*/true, isExprBasic,
                                          lParenLoc, args, argLabels,
                                          argLabelLocs,
                                          rParenLoc,
                                          trailingClosure,
                                          SyntaxKind::FunctionCallArgumentList);
      SyntaxContext->createNodeInPlace(SyntaxKind::FunctionCallExpr);
      return makeParserResult(
                 status,
                 UnresolvedMemberExpr::create(Context, DotLoc, NameLoc, Name,
                                              lParenLoc, args, argLabels,
                                              argLabelLocs, rParenLoc,
                                              trailingClosure,
                                              /*implicit=*/false));
    }

    // Check for a trailing closure, if allowed.
    if (Tok.is(tok::l_brace) && isValidTrailingClosure(isExprBasic, *this)) {
      if (SyntaxContext->isEnabled()) {
        // Add dummy blank argument list to the call expression syntax.
        SyntaxContext->addSyntax(
            SyntaxFactory::makeBlankFunctionCallArgumentList(
                SyntaxContext->getArena()));
      }

      ParserResult<Expr> closure =
        parseTrailingClosure(NameLoc.getSourceRange());
      if (closure.isNull()) return nullptr;

      SyntaxContext->createNodeInPlace(SyntaxKind::FunctionCallExpr);
      // Handle .foo by just making an AST node.
      return makeParserResult(
                 ParserStatus(closure),
                 UnresolvedMemberExpr::create(Context, DotLoc, NameLoc, Name,
                                              SourceLoc(), { }, { }, { },
                                              SourceLoc(), closure.get(),
                                              /*implicit=*/false));
    }

    // Handle .foo by just making an AST node.
    return makeParserResult(
               UnresolvedMemberExpr::create(Context, DotLoc, NameLoc, Name,
                                            /*implicit=*/false));
  }
      
  case tok::kw_super: // super.foo or super[foo]
    return parseExprSuper(isExprBasic);

  case tok::l_paren:
    // Build a tuple expression syntax node.
    // AST differentiates paren and tuple expression where the former allows
    // only one element without label. However, libSyntax tree doesn't have this
    // differentiation. A tuple expression node in libSyntax can have a single
    // element without label.
    ExprContext.setCreateSyntax(SyntaxKind::TupleExpr);
    return parseExprList(tok::l_paren, tok::r_paren,
                         SyntaxKind::TupleElementList);

  case tok::l_square:
    return parseExprCollection();

  case tok::pound_available: {
    // For better error recovery, parse but reject #available in an expr
    // context.
    diagnose(Tok.getLoc(), diag::availability_query_outside_if_stmt_guard);
    auto res = parseStmtConditionPoundAvailable();
    if (res.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
    if (res.isParseError() || res.isNull())
      return nullptr;
    return makeParserResult(new (Context)
                                ErrorExpr(res.get()->getSourceRange()));
  }

#define POUND_OBJECT_LITERAL(Name, Desc, Proto)                                \
  case tok::pound_##Name:                                                      \
    return parseExprObjectLiteral(ObjectLiteralExpr::Name, isExprBasic);
#include "swift/Syntax/TokenKinds.def"

  case tok::code_complete: {
    auto Result =
        makeParserResult(new (Context) CodeCompletionExpr(Tok.getLoc()));
    Result.setHasCodeCompletion();
    if (CodeCompletion &&
        // We cannot code complete anything after var/let.
        (!InVarOrLetPattern || InVarOrLetPattern == IVOLP_InMatchingPattern)) {
      if (InPoundIfEnvironment) {
        CodeCompletion->completePlatformCondition();
      } else {
        CodeCompletion->completePostfixExprBeginning(
            cast<CodeCompletionExpr>(Result.get()));
      }
    }
    consumeToken(tok::code_complete);
    return Result;
  }

  case tok::pound:
    if (peekToken().is(tok::identifier) && !peekToken().isEscapedIdentifier() &&
        Tok.getLoc().getAdvancedLoc(1) == peekToken().getLoc()) {
      return parseExprPoundUnknown(SourceLoc());
    }
    if (peekToken().is(tok::code_complete) &&
        Tok.getLoc().getAdvancedLoc(1) == peekToken().getLoc()) {
      return parseExprPoundCodeCompletion(/*ParentKind*/None);
    }
    goto UnknownCharacter;

  // Eat an invalid token in an expression context.  Error tokens are diagnosed
  // by the lexer, so there is no reason to emit another diagnostic.
  case tok::unknown:
    if (Tok.getText().startswith("\"\"\"")) {
      // This was due to unterminated multi-line string.
      IsInputIncomplete = true;
    }
    consumeToken(tok::unknown);
    return nullptr;

  default:
  UnknownCharacter:
    checkForInputIncomplete();
    // FIXME: offer a fixit: 'Self' -> 'self'
    diagnose(Tok, ID);
    return nullptr;
  }
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

ParserStatus Parser::
parseStringSegments(SmallVectorImpl<Lexer::StringSegment> &Segments,
                    Token EntireTok,
                    VarDecl *InterpolationVar,
                    /* remaining parameters are outputs: */
                    SmallVectorImpl<ASTNode> &Stmts,
                    unsigned &LiteralCapacity,
                    unsigned &InterpolationCount) {
  SourceLoc Loc = EntireTok.getLoc();
  ParserStatus Status;
  Trivia EmptyTrivia;
  bool First = true;

  DeclName appendLiteral(Context, Context.Id_appendLiteral, { Identifier() });
  DeclName appendInterpolation(Context.Id_appendInterpolation);

  for (auto Segment : Segments) {
    auto InterpolationVarRef =
      new (Context) DeclRefExpr(InterpolationVar,
                                DeclNameLoc(Segment.Loc), /*implicit=*/true);

    switch (Segment.Kind) {
    case Lexer::StringSegment::Literal: {

      // The end location of the entire string literal.
      SourceLoc EndLoc = EntireTok.getLoc().getAdvancedLoc(EntireTok.getLength());

      auto TokenLoc = First ? Loc : Segment.Loc;
      auto Literal = createStringLiteralExprFromSegment(Context, L, Segment,
                                                        TokenLoc);

      LiteralCapacity += Literal->getValue().size();

      auto AppendLiteralRef =
        new (Context) UnresolvedDotExpr(InterpolationVarRef,
                                        /*dotloc=*/SourceLoc(),
                                        appendLiteral,
                                        /*nameloc=*/DeclNameLoc(), 
                                        /*Implicit=*/true);
      auto AppendLiteralCall =
        CallExpr::createImplicit(Context, AppendLiteralRef, {Literal}, {});
      Stmts.push_back(AppendLiteralCall);

      // Since the string is already parsed, Tok already points to the first
      // token after the whole string, but PreviousLoc is not exactly correct.
      PreviousLoc = TokenLoc;
      SourceLoc TokEnd = Segment.IsLastSegment ? EndLoc : Segment.getEndLoc();
      unsigned CommentLength = 0;

      // First segment shall inherit the attached comments.
      if (First && EntireTok.hasComment()) {
        CommentLength = SourceMgr.getByteDistance(EntireTok.getCommentRange().
          getStart(), TokenLoc);
      }
      consumeExtraToken(Token(tok::string_literal,
                              CharSourceRange(SourceMgr, TokenLoc, TokEnd).str(),
                              CommentLength));
      SyntaxParsingContext StrSegContext(SyntaxContext,
                                         SyntaxKind::StringSegment);

      // Make an unknown token to encapsulate the entire string segment and add
      // such token to the context.
      Token content(tok::string_segment,
                    CharSourceRange(Segment.Loc, Segment.Length).str());
      SyntaxContext->addToken(content, EmptyTrivia, EmptyTrivia);
      break;
    }
        
    case Lexer::StringSegment::Expr: {
      SyntaxParsingContext ExprContext(SyntaxContext,
                                       SyntaxKind::ExpressionSegment);

      // Backslash is part of an expression segment.
      Token BackSlash(tok::backslash, "\\");
      ExprContext.addToken(BackSlash, EmptyTrivia, EmptyTrivia);
      // Create a temporary lexer that lexes from the body of the string.
      LexerState BeginState =
          L->getStateForBeginningOfTokenLoc(Segment.Loc);
      // We need to set the EOF at r_paren, to prevent the Lexer from eagerly
      // trying to lex the token beyond it. Parser::parseList() does a special
      // check for a tok::EOF that is spelled with a ')'.
      // FIXME: This seems like a hack, there must be a better way..
      LexerState EndState = BeginState.advance(Segment.Length-1);
      Lexer LocalLex(*L, BeginState, EndState);

      // Temporarily swap out the parser's current lexer with our new one.
      llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

      // Prime the new lexer with a '(' as the first token.
      // We might be at tok::eof now, so ensure that consumeToken() does not
      // assert about lexing past eof.
      Tok.setKind(tok::unknown);
      consumeTokenWithoutFeedingReceiver();
      assert(Tok.is(tok::l_paren));
      TokReceiver->registerTokenKindChange(Tok.getLoc(),
                                           tok::string_interpolation_anchor);

      SourceLoc lParen, rParen;
      SmallVector<Expr *, 4> args;
      SmallVector<Identifier, 4> argLabels;
      SmallVector<SourceLoc, 4> argLabelLocs;
      Expr *trailingClosureNeverPresent;
      ParserStatus S =
          parseExprList(tok::l_paren, tok::r_paren,
                        /*isPostfix=*/false, /*isExprBasic=*/true,
                        lParen, args, argLabels, argLabelLocs, rParen,
                        trailingClosureNeverPresent,
                        SyntaxKind::Unknown);
      assert(!trailingClosureNeverPresent);

      Status |= S;
      // If there was an error parsing a parameter, add an ErrorExpr to
      // represent it. Prevents spurious errors about a nonexistent
      // appendInterpolation() overload.
      if (S.isError() && args.size() == 0)
        args.push_back(new (Context) ErrorExpr(SourceRange(lParen, rParen)));
      
      while (argLabels.size() < args.size())
        argLabels.push_back(Identifier());
      while (argLabelLocs.size() < args.size())
        argLabelLocs.push_back(SourceLoc());

      // If we stopped parsing the expression before the expression segment is
      // over, eat the remaining tokens into a token list
      if (Segment.getEndLoc() !=
          L->getLocForEndOfToken(SourceMgr, Tok.getLoc())) {
        SyntaxParsingContext RemainingTokens(SyntaxContext,
                                             SyntaxKind::NonEmptyTokenList);
        do {
          consumeToken();
        } while (Segment.getEndLoc() !=
                 L->getLocForEndOfToken(SourceMgr, Tok.getLoc()));
      }

      InterpolationCount += 1;
      
      // In Swift 4.2 and earlier, a single argument with a label would ignore
      // the label (at best), and multiple arguments would form a tuple.
      if (!Context.isSwiftVersionAtLeast(5)) {
        if (args.size() > 1) {
          diagnose(args[1]->getLoc(), diag::string_interpolation_list_changing)
            .highlightChars(args[1]->getLoc(), rParen);
          diagnose(args[1]->getLoc(),
                   diag::string_interpolation_list_insert_parens)
            .fixItInsertAfter(lParen, "(")
            .fixItInsert(rParen, ")");
          
          args = {
            TupleExpr::create(Context,
                              lParen, args, argLabels, argLabelLocs, rParen,
                              /*hasTrailingClosure=*/false,
                              /*Implicit=*/false)
          };
          argLabels = { Identifier() };
          argLabelLocs = { SourceLoc() };
        }
        else if (args.size() == 1 && argLabels[0] != Identifier()) {
          diagnose(argLabelLocs[0], diag::string_interpolation_label_changing)
            .highlightChars(argLabelLocs[0], args[0]->getStartLoc());
          diagnose(argLabelLocs[0], diag::string_interpolation_remove_label,
                   argLabels[0])
            .fixItRemoveChars(argLabelLocs[0], args[0]->getStartLoc());
          
          argLabels[0] = Identifier();
        }
      }
      
      auto AppendInterpolationRef =
        new (Context) UnresolvedDotExpr(InterpolationVarRef,
                                        /*dotloc=*/SourceLoc(),
                                        appendInterpolation,
                                        /*nameloc=*/DeclNameLoc(), 
                                        /*Implicit=*/true);
      auto AppendInterpolationCall =
        CallExpr::create(Context, AppendInterpolationRef,
                         lParen, args, argLabels, argLabelLocs, rParen,
                         /*trailingClosure=*/nullptr, /*implicit=*/false);
      
      Stmts.push_back(AppendInterpolationCall);

      if (!Tok.is(tok::eof)) {
        diagnose(Tok, diag::string_interpolation_extra);
      } else if (Tok.getText() == ")") {
        Tok.setKind(tok::string_interpolation_anchor);
        // We don't allow trailing trivia for this anchor, because the
        // trivia is a part of the next string segment.
        TrailingTrivia.clear();
        consumeToken();
      }
      break;
    }
    }
    First = false;
  }
  return Status;
}

///   expr-literal:
///     string_literal
ParserResult<Expr> Parser::parseExprStringLiteral() {
  SyntaxParsingContext LocalContext(SyntaxContext,
                                    SyntaxKind::StringLiteralExpr);

  SmallVector<Lexer::StringSegment, 1> Segments;
  L->getStringLiteralSegments(Tok, Segments);

  Token EntireTok = Tok;

  // The start location of the entire string literal.
  SourceLoc Loc = Tok.getLoc();
  SourceLoc EndLoc = Loc.getAdvancedLoc(Tok.getLength());

  // The simple case: just a single literal segment.
  if (Segments.size() == 1 &&
      Segments.front().Kind == Lexer::StringSegment::Literal) {
    consumeToken();
    return makeParserResult(
        createStringLiteralExprFromSegment(Context, L, Segments.front(), Loc));
  }

  // We are now sure this is a string interpolation expression.
  LocalContext.setCreateSyntax(SyntaxKind::StringInterpolationExpr);
  StringRef Quote;
  tok QuoteKind;
  std::tie(Quote, QuoteKind) = Tok.isMultilineString() ?
    std::make_tuple("\"\"\"", tok::multiline_string_quote) :
    std::make_tuple("\"", tok::string_quote);

  // Make unknown tokens to represent the open and close quote.
  Token OpenQuote(QuoteKind, Quote);
  Token CloseQuote(QuoteKind, Quote);
  Trivia EmptyTrivia;
  Trivia EntireTrailingTrivia = TrailingTrivia;

  // Add the open quote to the context; the quote should have the leading trivia
  // of the entire string token and a void trailing trivia.
  SyntaxContext->addToken(OpenQuote, LeadingTrivia, EmptyTrivia);

  // We don't expose the entire interpolated string as one token. Instead, we
  // should expose the tokens in each segment.
  consumeTokenWithoutFeedingReceiver();
  // We are going to mess with Tok to do reparsing for interpolated literals,
  // don't lose our 'next' token.
  llvm::SaveAndRestore<Token> SavedTok(Tok);
  llvm::SaveAndRestore<Trivia> SavedLeadingTrivia(LeadingTrivia);
  llvm::SaveAndRestore<Trivia> SavedTrailingTrivia(TrailingTrivia);

  // We're not in a place where an interpolation would be valid.
  if (!CurLocalContext) {
    // Return an error, but include an empty InterpolatedStringLiteralExpr 
    // so that parseDeclPoundDiagnostic() can figure out why this string 
    // literal was bad.
    return makeParserErrorResult(
        new (Context) InterpolatedStringLiteralExpr(Loc, 0, 0, nullptr));
  }

  unsigned LiteralCapacity = 0;
  unsigned InterpolationCount = 0;
  TapExpr * AppendingExpr;

  ParserStatus Status;
  {
    Scope S(this, ScopeKind::Brace);
    SmallVector<ASTNode, 4> Stmts;

    // Make the variable which will contain our temporary value.
    auto InterpolationVar =
      new (Context) VarDecl(/*IsStatic=*/false, VarDecl::Specifier::Var,
                            /*IsCaptureList=*/false, /*NameLoc=*/SourceLoc(),
                            Context.Id_dollarInterpolation, CurDeclContext);
    InterpolationVar->setImplicit(true);
    InterpolationVar->setHasNonPatternBindingInit(true);
    InterpolationVar->setUserAccessible(false);
    addToScope(InterpolationVar);
    setLocalDiscriminator(InterpolationVar);
    
    Stmts.push_back(InterpolationVar);

    // Collect all string segments.
    SyntaxParsingContext SegmentsCtx(SyntaxContext,
                                     SyntaxKind::StringInterpolationSegments);
    Status = parseStringSegments(Segments, EntireTok, InterpolationVar, 
                                 Stmts, LiteralCapacity, InterpolationCount);

    auto Body = BraceStmt::create(Context, Loc, Stmts, EndLoc,
                                  /*implicit=*/false);
    AppendingExpr = new (Context) TapExpr(nullptr, Body);
  }

  // Add the close quote the context; the quote should have a void leading trivia
  // and the trailing trivia of the entire string token.
  SyntaxContext->addToken(CloseQuote, EmptyTrivia, EntireTrailingTrivia);

  if (AppendingExpr->getBody()->getNumElements() == 1) {
    Status.setIsParseError();
    return makeParserResult(Status, new (Context) ErrorExpr(Loc));
  }

  return makeParserResult(Status, new (Context) InterpolatedStringLiteralExpr(
                                      Loc, LiteralCapacity, InterpolationCount,
                                      AppendingExpr));
}

void Parser::parseOptionalArgumentLabel(Identifier &name, SourceLoc &loc) {
  // Check to see if there is an argument label.
  if (Tok.canBeArgumentLabel() && peekToken().is(tok::colon)) {
    auto text = Tok.getText();

    // If this was an escaped identifier that need not have been escaped, say
    // so. Only _ needs escaping, because we take foo(_: 3) to be equivalent
    // to foo(3), to be more uniform with _ in function declaration as well as
    // the syntax for referring to the function pointer (foo(_:)),
    auto escaped = Tok.isEscapedIdentifier();
    auto underscore = Tok.is(tok::kw__) || (escaped && text == "_");
    if (escaped && !underscore && canBeArgumentLabel(text)) {
      SourceLoc start = Tok.getLoc();
      SourceLoc end = start.getAdvancedLoc(Tok.getLength());
      diagnose(Tok, diag::escaped_parameter_name, text)
          .fixItRemoveChars(start, start.getAdvancedLoc(1))
          .fixItRemoveChars(end.getAdvancedLoc(-1), end);
    }

    loc = consumeArgumentLabel(name);
    consumeToken(tok::colon);
  }
}

DeclName Parser::parseUnqualifiedDeclName(bool afterDot,
                                          DeclNameLoc &loc,
                                          const Diagnostic &diag,
                                          bool allowOperators,
                                          bool allowZeroArgCompoundNames) {
  // Consume the base name.
  DeclBaseName baseName;
  SourceLoc baseNameLoc;
  if (Tok.isAny(tok::identifier, tok::kw_Self, tok::kw_self)) {
    baseName = Context.getIdentifier(Tok.getText());
    baseNameLoc = consumeToken();
  } else if (allowOperators && Tok.isAnyOperator()) {
    baseName = Context.getIdentifier(Tok.getText());
    baseNameLoc = consumeToken();
  } else if (afterDot && Tok.isKeyword()) {
    // Syntax highlighting should treat this token as an identifier and
    // not as a keyword.
    if (Tok.is(tok::kw_init))
      baseName = DeclBaseName::createConstructor();
    else
      baseName = Context.getIdentifier(Tok.getText());
    Tok.setKind(tok::identifier);
    baseNameLoc = consumeToken();
  } else {
    baseName = Context.getIdentifier(Tok.getText());
    checkForInputIncomplete();
    diagnose(Tok, diag);
    return DeclName();
  }

  // If the next token isn't a following '(', we don't have a compound name.
  if (!Tok.isFollowingLParen()) {
    loc = DeclNameLoc(baseNameLoc);
    return baseName;
  }


  // If the next token is a ')' then we have a 0-arg compound name. This is
  // explicitly differentiated from "simple" (non-compound) name in DeclName.
  // Unfortunately only some places in the grammar are ok with accepting this
  // kind of name; in other places it's ambiguous with trailing calls.
  if (allowZeroArgCompoundNames && peekToken().is(tok::r_paren)) {
    SyntaxParsingContext ArgsCtxt(SyntaxContext, SyntaxKind::DeclNameArguments);
    consumeToken(tok::l_paren);
    if (SyntaxContext->isEnabled())
      SyntaxContext->addSyntax(SyntaxFactory::makeBlankDeclNameArgumentList(
          SyntaxContext->getArena()));
    consumeToken(tok::r_paren);
    loc = DeclNameLoc(baseNameLoc);
    SmallVector<Identifier, 2> argumentLabels;
    return DeclName(Context, baseName, argumentLabels);
  }

  // If the token after that isn't an argument label or ':', we don't have a
  // compound name.
  if ((!peekToken().canBeArgumentLabel() && !peekToken().is(tok::colon)) ||
      Identifier::isEditorPlaceholder(peekToken().getText())) {
    loc = DeclNameLoc(baseNameLoc);
    return baseName;
  }

  // Try to parse a compound name.
  SyntaxParsingContext ArgsCtxt(SyntaxContext, SyntaxKind::DeclNameArguments);
  BacktrackingScope backtrack(*this);

  SmallVector<Identifier, 2> argumentLabels;
  SmallVector<SourceLoc, 2> argumentLabelLocs;
  SourceLoc lparenLoc = consumeToken(tok::l_paren);
  SourceLoc rparenLoc;
  while (Tok.isNot(tok::r_paren)) {
    SyntaxParsingContext ArgCtxt(SyntaxContext, SyntaxKind::DeclNameArgument);

    // If we see a ':', the user forgot the '_';
    if (Tok.is(tok::colon)) {
      diagnose(Tok, diag::empty_arg_label_underscore)
          .fixItInsert(Tok.getLoc(), "_");
      argumentLabels.push_back(Identifier());
      argumentLabelLocs.push_back(consumeToken(tok::colon));
    }

    Identifier argName;
    SourceLoc argLoc;
    parseOptionalArgumentLabel(argName, argLoc);
    if (argLoc.isValid()) {
      argumentLabels.push_back(argName);
      argumentLabelLocs.push_back(argLoc);
      continue;
    }

    // This is not a compound name.
    // FIXME: Could recover better if we "know" it's a compound name.
    loc = DeclNameLoc(baseNameLoc);
    ArgsCtxt.setBackTracking();
    return baseName;
  }
  // We have a compound name. Cancel backtracking and build that name.
  backtrack.cancelBacktrack();

  ArgsCtxt.collectNodesInPlace(SyntaxKind::DeclNameArgumentList);
  rparenLoc = consumeToken(tok::r_paren);

  assert(!argumentLabels.empty() && "Logic above should prevent this");
  assert(argumentLabels.size() == argumentLabelLocs.size());

  loc = DeclNameLoc(Context, baseNameLoc, lparenLoc, argumentLabelLocs,
                    rparenLoc);
  return DeclName(Context, baseName, argumentLabels);
}

///   expr-identifier:
///     unqualified-decl-name generic-args?
Expr *Parser::parseExprIdentifier() {
  assert(Tok.isAny(tok::identifier, tok::kw_self, tok::kw_Self));
  SyntaxParsingContext IDSyntaxContext(SyntaxContext,
                                       SyntaxKind::IdentifierExpr);
  Token IdentTok = Tok;

  // Parse the unqualified-decl-name.
  DeclNameLoc loc;
  DeclName name = parseUnqualifiedDeclName(/*afterDot=*/false, loc,
                                           diag::expected_expr);

  SmallVector<TypeRepr*, 8> args;
  SourceLoc LAngleLoc, RAngleLoc;
  bool hasGenericArgumentList = false;
  
  ///   The generic-args case is ambiguous with an expression involving '<'
  ///   and '>' operators. The operator expression is favored unless a generic
  ///   argument list can be successfully parsed, and the closing bracket is
  ///   followed by one of these tokens:
  ///     lparen_following rparen lsquare_following rsquare lbrace rbrace
  ///     period_following comma semicolon
  ///
  if (canParseAsGenericArgumentList()) {
    SyntaxContext->createNodeInPlace(SyntaxKind::IdentifierExpr);
    SyntaxContext->setCreateSyntax(SyntaxKind::SpecializeExpr);
    if (parseGenericArguments(args, LAngleLoc, RAngleLoc)) {
      diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);
    }
    
    // The result can be empty in error cases.
    hasGenericArgumentList = !args.empty();
  }
  
  ValueDecl *D = nullptr;
  if (!InPoundIfEnvironment) {
    D = lookupInScope(name);
    // FIXME: We want this to work: "var x = { x() }", but for now it's better
    // to disallow it than to crash.
    if (D) {
      for (auto activeVar : DisabledVars) {
        if (activeVar == D) {
          diagnose(loc.getBaseNameLoc(), DisabledVarReason);
          return new (Context) ErrorExpr(loc.getSourceRange());
        }
      }
    } else {
      for (auto activeVar : DisabledVars) {
        if (activeVar->getFullName() == name) {
          diagnose(loc.getBaseNameLoc(), DisabledVarReason);
          return new (Context) ErrorExpr(loc.getSourceRange());
        }
      }
    }
  }
  
  Expr *E;
  if (D == nullptr) {
    if (name.getBaseName().isEditorPlaceholder()) {
      IDSyntaxContext.setCreateSyntax(SyntaxKind::EditorPlaceholderExpr);
      return parseExprEditorPlaceholder(IdentTok, name.getBaseIdentifier());
    }

    auto refKind = DeclRefKind::Ordinary;
    E = new (Context) UnresolvedDeclRefExpr(name, refKind, loc);
  } else if (auto TD = dyn_cast<TypeDecl>(D)) {
    // When parsing default argument expressions for generic functions,
    // we haven't built a FuncDecl or re-parented the GenericTypeParamDecls
    // to the FuncDecl yet. Other than that, we should only ever find
    // global or local declarations here.
    assert(!TD->getDeclContext()->isTypeContext() ||
           isa<GenericTypeParamDecl>(TD));
    E = TypeExpr::createForDecl(loc.getBaseNameLoc(), TD, /*DC*/nullptr,
                                /*implicit*/false);
  } else {
    E = new (Context) DeclRefExpr(D, loc, /*Implicit=*/false);
  }
  
  if (hasGenericArgumentList) {
    SmallVector<TypeLoc, 8> locArgs;
    for (auto ty : args)
      locArgs.push_back(ty);
    E = UnresolvedSpecializeExpr::create(Context, E, LAngleLoc, locArgs,
                                         RAngleLoc);
  }
  return E;
}

Expr *Parser::parseExprEditorPlaceholder(Token PlaceholderTok,
                                         Identifier PlaceholderId) {
  assert(PlaceholderTok.is(tok::identifier));
  assert(PlaceholderId.isEditorPlaceholder());

  auto parseTypeForPlaceholder = [&](TypeLoc &TyLoc, TypeRepr *&ExpansionTyR) {
    Optional<EditorPlaceholderData> DataOpt =
      swift::parseEditorPlaceholder(PlaceholderTok.getText());
    if (!DataOpt)
      return;
    StringRef TypeStr = DataOpt->Type;
    if (TypeStr.empty())
      return;

    // Ensure that we restore the parser state at exit.
    ParserPositionRAII PPR(*this);

    auto parseTypeString = [&](StringRef TyStr) -> TypeRepr* {
      unsigned Offset = TyStr.data() - PlaceholderTok.getText().data();
      SourceLoc TypeStartLoc = PlaceholderTok.getLoc().getAdvancedLoc(Offset);
      SourceLoc TypeEndLoc = TypeStartLoc.getAdvancedLoc(TyStr.size());

      LexerState StartState = L->getStateForBeginningOfTokenLoc(TypeStartLoc);
      LexerState EndState = L->getStateForBeginningOfTokenLoc(TypeEndLoc);

      // Create a lexer for the type sub-string.
      Lexer LocalLex(*L, StartState, EndState);

      // Temporarily swap out the parser's current lexer with our new one.
      llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

      // Don't feed to syntax token recorder.
      ConsumeTokenReceiver DisabledRec;
      llvm::SaveAndRestore<ConsumeTokenReceiver *> R(TokReceiver, &DisabledRec);
      SyntaxParsingContext SContext(SyntaxContext);
      SContext.disable();

      Tok.setKind(tok::unknown); // we might be at tok::eof now.
      consumeTokenWithoutFeedingReceiver();
      return parseType().getPtrOrNull();
    };

    TypeRepr *TyR = parseTypeString(TypeStr);
    TyLoc = TyR;
    if (DataOpt->TypeForExpansion == TypeStr) {
      ExpansionTyR = TyR;
    } else {
      ExpansionTyR = parseTypeString(DataOpt->TypeForExpansion);
    }
  };

  TypeLoc TyLoc;
  TypeRepr *ExpansionTyR = nullptr;
  parseTypeForPlaceholder(TyLoc, ExpansionTyR);
  return new (Context) EditorPlaceholderExpr(PlaceholderId,
                                             PlaceholderTok.getLoc(),
                                             TyLoc, ExpansionTyR);
}

// Extract names of the tuple elements and preserve the structure
// of the tuple (with any nested tuples inside) to be able to use
// it in the fix-it without any type information provided by user.
static void printTupleNames(const TypeRepr *typeRepr, llvm::raw_ostream &OS) {
  if (!typeRepr)
    return;
  
  auto tupleRepr = dyn_cast<TupleTypeRepr>(typeRepr);
  if (!tupleRepr)
    return;
  
  OS << "(";
  unsigned elementIndex = 0;
  llvm::SmallVector<TypeRepr *, 10> elementTypes;
  tupleRepr->getElementTypes(elementTypes);
  interleave(elementTypes,
             [&](const TypeRepr *element) {
               if (isa<TupleTypeRepr>(element)) {
                 printTupleNames(element, OS);
               } else {
                 auto name = tupleRepr->getElementName(elementIndex);
                 // If there is no label from the element
                 // it means that it's malformed and we can
                 // use the type instead.
                 if (name.empty())
                   element->print(OS);
                 else
                   OS << name;
               }
               
               ++elementIndex;
             },
             [&] { OS << ", "; });
  OS << ")";
}

bool Parser::
parseClosureSignatureIfPresent(SmallVectorImpl<CaptureListEntry> &captureList,
                               ParameterList *&params, SourceLoc &throwsLoc,
                               SourceLoc &arrowLoc,
                               TypeRepr *&explicitResultType, SourceLoc &inLoc){
  // Clear out result parameters.
  params = nullptr;
  throwsLoc = SourceLoc();
  arrowLoc = SourceLoc();
  explicitResultType = nullptr;
  inLoc = SourceLoc();

  // If we have a leading token that may be part of the closure signature, do a
  // speculative parse to validate it and look for 'in'.
  if (Tok.isAny(tok::l_paren, tok::l_square, tok::identifier, tok::kw__)) {
    BacktrackingScope backtrack(*this);

    // Skip by a closure capture list if present.
    if (consumeIf(tok::l_square)) {
      skipUntil(tok::r_square);
      if (!consumeIf(tok::r_square))
        return false;
    }

    // Parse pattern-tuple func-signature-result? 'in'.
    if (consumeIf(tok::l_paren)) {      // Consume the ')'.

      // While we don't have '->' or ')', eat balanced tokens.
      while (!Tok.is(tok::r_paren) && !Tok.is(tok::eof))
        skipSingle();

      // Consume the ')', if it's there.
      if (consumeIf(tok::r_paren)) {
        consumeIf(tok::kw_throws) || consumeIf(tok::kw_rethrows);
        // Parse the func-signature-result, if present.
        if (consumeIf(tok::arrow)) {
          if (!canParseType())
            return false;
        }
      }

      // Okay, we have a closure signature.
    } else if (Tok.isIdentifierOrUnderscore()) {
      // Parse identifier (',' identifier)*
      consumeToken();
      while (consumeIf(tok::comma)) {
        if (Tok.isIdentifierOrUnderscore()) {
          consumeToken();
          continue;
        }

        return false;
      }
      
      consumeIf(tok::kw_throws) || consumeIf(tok::kw_rethrows);

      // Parse the func-signature-result, if present.
      if (consumeIf(tok::arrow)) {
        if (!canParseType())
          return false;
      }
    }
    
    // Parse the 'in' at the end.
    if (Tok.isNot(tok::kw_in))
      return false;

    // Okay, we have a closure signature.
  } else {
    // No closure signature.
    return false;
  }
  SyntaxParsingContext ClosureSigCtx(SyntaxContext, SyntaxKind::ClosureSignature);
  if (Tok.is(tok::l_square) && peekToken().is(tok::r_square)) {
    SyntaxParsingContext CaptureCtx(SyntaxContext,
                                    SyntaxKind::ClosureCaptureSignature);
    consumeToken(tok::l_square);
    consumeToken(tok::r_square);
  } else if (Tok.is(tok::l_square) && !peekToken().is(tok::r_square)) {
    SyntaxParsingContext CaptureCtx(SyntaxContext,
                                    SyntaxKind::ClosureCaptureSignature);
    consumeToken(tok::l_square);
    // At this point, we know we have a closure signature. Parse the capture list
    // and parameters.
    bool HasNext;
    do {
      SyntaxParsingContext CapturedItemCtx(SyntaxContext,
                                           SyntaxKind::ClosureCaptureItem);
      SWIFT_DEFER { HasNext = consumeIf(tok::comma); };
      // Check for the strength specifier: "weak", "unowned", or
      // "unowned(safe/unsafe)".
      SourceLoc ownershipLocStart, ownershipLocEnd;
      auto ownershipKind = ReferenceOwnership::Strong;
      if (Tok.isContextualKeyword("weak")){
        ownershipLocStart = ownershipLocEnd = consumeToken(tok::identifier);
        ownershipKind = ReferenceOwnership::Weak;
      } else if (Tok.isContextualKeyword("unowned")) {
        ownershipLocStart = ownershipLocEnd = consumeToken(tok::identifier);
        ownershipKind = ReferenceOwnership::Unowned;

        // Skip over "safe" and "unsafe" if present.
        if (consumeIf(tok::l_paren)) {
          if (Tok.getText() == "safe")
            ownershipKind =
                ReferenceOwnership::Unowned; // FIXME: No "safe" variant.
          else if (Tok.getText() == "unsafe")
            ownershipKind = ReferenceOwnership::Unmanaged;
          else
            diagnose(Tok, diag::attr_unowned_invalid_specifier);
          consumeIf(tok::identifier, ownershipLocEnd);
          if (!consumeIf(tok::r_paren, ownershipLocEnd))
            diagnose(Tok, diag::attr_unowned_expected_rparen);
        }
      } else if (Tok.isAny(tok::identifier, tok::kw_self) &&
                 peekToken().isAny(tok::equal, tok::comma, tok::r_square)) {
        // "x = 42", "x," and "x]" are all strong captures of x.
      } else {
        diagnose(Tok, diag::expected_capture_specifier);
        skipUntil(tok::comma, tok::r_square);
        continue;
      }

      if (Tok.isNot(tok::identifier, tok::kw_self)) {
        diagnose(Tok, diag::expected_capture_specifier_name);
        skipUntil(tok::comma, tok::r_square);
        continue;
      }

      // Squash all tokens, if any, as the specifier of the captured item.
      CapturedItemCtx.collectNodesInPlace(SyntaxKind::TokenList);

      // The thing being capture specified is an identifier, or as an identifier
      // followed by an expression.
      Expr *initializer;
      Identifier name;
      SourceLoc nameLoc = Tok.getLoc();
      SourceLoc equalLoc;
      if (peekToken().isNot(tok::equal)) {
        // If this is the simple case, then the identifier is both the name and
        // the expression to capture.
        name = Context.getIdentifier(Tok.getText());
        initializer = parseExprIdentifier();

        // It is a common error to try to capture a nested field instead of just
        // a local name, reject it with a specific error message.
        if (Tok.isAny(tok::period, tok::exclaim_postfix,tok::question_postfix)){
          diagnose(Tok, diag::cannot_capture_fields);
          skipUntil(tok::comma, tok::r_square);
          continue;
        }

      } else {
        // Otherwise, the name is a new declaration.
        consumeIdentifier(&name);
        equalLoc = consumeToken(tok::equal);

        auto ExprResult = parseExpr(diag::expected_init_capture_specifier);
        if (ExprResult.isNull())
          continue;
        initializer = ExprResult.get();
      }

      // Create the VarDecl and the PatternBindingDecl for the captured
      // expression.  This uses the parent declcontext (not the closure) since
      // the initializer expression is evaluated before the closure is formed.
      auto specifierKind = (ownershipKind != ReferenceOwnership::Weak)
                               ? VarDecl::Specifier::Let
                               : VarDecl::Specifier::Var;
      auto *VD = new (Context) VarDecl(/*isStatic*/false,
                                       specifierKind,
                                       /*isCaptureList*/true,
                                       nameLoc, name, CurDeclContext);

      // Attributes.
      if (ownershipKind != ReferenceOwnership::Strong)
        VD->getAttrs().add(new (Context) ReferenceOwnershipAttr(
          SourceRange(ownershipLocStart, ownershipLocEnd), ownershipKind));

      auto pattern = new (Context) NamedPattern(VD, /*implicit*/true);

      auto *PBD = PatternBindingDecl::create(
          Context, /*StaticLoc*/ SourceLoc(), StaticSpellingKind::None,
          /*VarLoc*/ nameLoc, pattern, /*EqualLoc*/ equalLoc, initializer,
          CurDeclContext);

      captureList.push_back(CaptureListEntry(VD, PBD));
    } while (HasNext);

    SyntaxContext->collectNodesInPlace(SyntaxKind::ClosureCaptureItemList);
    // The capture list needs to be closed off with a ']'.
    if (!consumeIf(tok::r_square)) {
      diagnose(Tok, diag::expected_capture_list_end_rsquare);
      skipUntil(tok::r_square);
      if (Tok.is(tok::r_square))
        consumeToken(tok::r_square);
    }
  }
  
  bool invalid = false;
  if (Tok.isNot(tok::kw_in)) {
    if (Tok.is(tok::l_paren)) {
      // Parse the closure arguments.
      auto pattern = parseSingleParameterClause(ParameterContextKind::Closure);
      if (pattern.isNonNull())
        params = pattern.get();
      else
        invalid = true;
    } else {
      SyntaxParsingContext ClParamListCtx(SyntaxContext,
                                          SyntaxKind::ClosureParamList);
      // Parse identifier (',' identifier)*
      SmallVector<ParamDecl*, 4> elements;
      bool HasNext;
      do {
        SyntaxParsingContext ClParamCtx(SyntaxContext, SyntaxKind::ClosureParam);
        if (Tok.isNot(tok::identifier, tok::kw__)) {
          diagnose(Tok, diag::expected_closure_parameter_name);
          invalid = true;
          break;
        }

        Identifier name = Tok.is(tok::identifier) ?
            Context.getIdentifier(Tok.getText()) : Identifier();
        auto var = new (Context)
            ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                      Identifier(), Tok.getLoc(), name, nullptr);
        elements.push_back(var);
        consumeToken();

        // Consume a comma to continue.
        HasNext = consumeIf(tok::comma);
      } while (HasNext);

      params = ParameterList::create(Context, elements);
    }
    
    if (Tok.is(tok::kw_throws)) {
      throwsLoc = consumeToken();
    } else if (Tok.is(tok::kw_rethrows)) {
      throwsLoc = consumeToken();
      diagnose(throwsLoc, diag::rethrowing_function_type)
        .fixItReplace(throwsLoc, "throws");
    }

    // Parse the optional explicit return type.
    if (Tok.is(tok::arrow)) {
      SyntaxParsingContext ReturnCtx(SyntaxContext, SyntaxKind::ReturnClause);
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

  if (!params)
    return invalid;

  // If this was a closure declaration (maybe even trailing)
  // tuple parameter destructuring is one of the common
  // problems, and is misleading to users, so it's imperative
  // to detect any tuple splat or destructuring as early as
  // possible and give a proper fix-it. See SE-0110 for more details.
  auto isTupleDestructuring = [](ParamDecl *param) -> bool {
    if (!param->isInvalid())
      return false;
    auto &typeLoc = param->getTypeLoc();
    if (auto typeRepr = typeLoc.getTypeRepr())
      return !param->hasName() && isa<TupleTypeRepr>(typeRepr);
    return false;
  };

  for (unsigned i = 0, e = params->size(); i != e; ++i) {
    auto *param = params->get(i);
    if (!isTupleDestructuring(param))
      continue;

    auto argName = "arg" + std::to_string(i);
    auto typeLoc = param->getTypeLoc();

    SmallString<64> fixIt;
    llvm::raw_svector_ostream OS(fixIt);
    auto isMultiLine = Tok.isAtStartOfLine();
    StringRef indent = Lexer::getIndentationForLine(SourceMgr, Tok.getLoc());
    if (isMultiLine)
      OS << '\n' << indent;

    OS << "let ";
    printTupleNames(typeLoc.getTypeRepr(), OS);
    OS << " = " << argName << (isMultiLine ? "\n" + indent : "; ");

    diagnose(param->getStartLoc(), diag::anon_closure_tuple_param_destructuring)
        .fixItReplace(param->getSourceRange(), argName)
        .fixItInsert(Tok.getLoc(), OS.str());

    invalid = true;
  }

  return invalid;
}

ParserResult<Expr> Parser::parseExprClosure() {
  assert(Tok.is(tok::l_brace) && "Not at a left brace?");
  SyntaxParsingContext ClosureContext(SyntaxContext, SyntaxKind::ClosureExpr);
  // We may be parsing this closure expr in a matching pattern context.  If so,
  // reset our state to not be in a pattern for any recursive pattern parses.
  llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
  T(InVarOrLetPattern, IVOLP_NotInVarOrLet);
  
  // Parse the opening left brace.
  SourceLoc leftBrace = consumeToken();

  // Parse the closure-signature, if present.
  ParameterList *params = nullptr;
  SourceLoc throwsLoc;
  SourceLoc arrowLoc;
  TypeRepr *explicitResultType;
  SourceLoc inLoc;
  SmallVector<CaptureListEntry, 2> captureList;
  parseClosureSignatureIfPresent(captureList, params, throwsLoc, arrowLoc,
                                 explicitResultType, inLoc);

  // If the closure was created in the context of an array type signature's
  // size expression, there will not be a local context. A parse error will
  // be reported at the signature's declaration site.
  if (!CurLocalContext) {
    skipUntil(tok::r_brace);
    if (Tok.is(tok::r_brace))
      consumeToken();
    return makeParserError();
  }
  
  unsigned discriminator = CurLocalContext->claimNextClosureDiscriminator();

  // Create the closure expression and enter its context.
  auto *closure = new (Context) ClosureExpr(params, throwsLoc, arrowLoc, inLoc,
                                            explicitResultType,
                                            discriminator, CurDeclContext);
  // The arguments to the func are defined in their own scope.
  Scope S(this, ScopeKind::ClosureParams);
  ParseFunctionBody cc(*this, closure);

  // Handle parameters.
  if (params) {
    // Add the parameters into scope.
    addParametersToScope(params);
    setLocalDiscriminatorToParamList(params);
  } else {
    // There are no parameters; allow anonymous closure variables.
    // FIXME: We could do this all the time, and then provide Fix-Its
    // to map $i -> the appropriately-named argument. This might help
    // users who are refactoring code by adding names.
    AnonClosureVars.push_back({ leftBrace, {}});
  }
  
  // Add capture list variables to scope.
  for (auto c : captureList)
    addToScope(c.Var);

  // Parse the body.
  SmallVector<ASTNode, 4> bodyElements;
  ParserStatus Status;
  Status |= parseBraceItems(bodyElements, BraceItemListKind::Brace);

  // Parse the closing '}'.
  SourceLoc rightBrace;
  parseMatchingToken(tok::r_brace, rightBrace, diag::expected_closure_rbrace,
                     leftBrace);

  // If we didn't have any parameters, create a parameter list from the
  // anonymous closure arguments.
  if (!params) {
    // Create a parameter pattern containing the anonymous variables.
    auto &anonVars = AnonClosureVars.back().second;
    SmallVector<ParamDecl*, 4> elements;
    for (auto anonVar : anonVars)
      elements.push_back(anonVar);
    
    params = ParameterList::create(Context, leftBrace, elements, leftBrace);

    // Pop out of the anonymous closure variables scope.
    AnonClosureVars.pop_back();

    // Attach the parameters to the closure.
    closure->setParameterList(params);
    closure->setHasAnonymousClosureVars();
  }

  // If the body consists of a single expression, turn it into a return
  // statement.
  //
  // But don't do this transformation during code completion, as the source
  // may be incomplete and the type mismatch in return statement will just
  // confuse the type checker.
  bool hasSingleExpressionBody = false;
  if (!Status.hasCodeCompletion() && bodyElements.size() == 1) {
    // If the closure's only body element is a single return statement,
    // use that instead of creating a new wrapping return expression.
    Expr *returnExpr = nullptr;
    
    if (bodyElements[0].is<Stmt *>()) {
      if (auto returnStmt =
                  dyn_cast<ReturnStmt>(bodyElements[0].get<Stmt*>())) {
        
        if (!returnStmt->hasResult()) {
          
          returnExpr = TupleExpr::createEmpty(Context,
                                              SourceLoc(),
                                              SourceLoc(),
                                              /*implicit*/true);
          
          returnStmt->setResult(returnExpr);
        }
        
        hasSingleExpressionBody = true;
      }
    }
    
    // Otherwise, create the wrapping return.
    if (bodyElements[0].is<Expr *>()) {
      hasSingleExpressionBody = true;
      returnExpr = bodyElements[0].get<Expr*>();
      bodyElements[0] = new (Context) ReturnStmt(SourceLoc(),
                                                 returnExpr);
    }
  }

  // Set the body of the closure.
  closure->setBody(BraceStmt::create(Context, leftBrace, bodyElements,
                                     rightBrace),
                   hasSingleExpressionBody);

  // If the closure includes a capture list, create an AST node for it as well.
  Expr *result = closure;
  if (!captureList.empty())
    result = CaptureListExpr::create(Context, captureList, closure);

  return makeParserResult(Status, result);
}

///   expr-anon-closure-argument:
///     dollarident
Expr *Parser::parseExprAnonClosureArg() {
  SyntaxParsingContext ExprContext(SyntaxContext, SyntaxKind::IdentifierExpr);
  StringRef Name = Tok.getText();
  SourceLoc Loc = consumeToken(tok::dollarident);
  assert(Name[0] == '$' && "Not a dollarident");

  // We know from the lexer that this is all-numeric.

  unsigned ArgNo = 0;
  if (Name.substr(1).getAsInteger(10, ArgNo)) {
    diagnose(Loc.getAdvancedLoc(1), diag::dollar_numeric_too_large);
    return new (Context) ErrorExpr(Loc);
  }

  // If this is a closure expression that did not have any named parameters,
  // generate the anonymous variables we need.
  auto closure = dyn_cast_or_null<ClosureExpr>(
      dyn_cast<AbstractClosureExpr>(CurDeclContext));
  if (!closure) {
    diagnose(Loc, diag::anon_closure_arg_not_in_closure);
    return new (Context) ErrorExpr(Loc);
  }
  // When the closure already has explicit parameters, offer their names as
  // replacements.
  if (auto *params = closure->getParameters()) {
    if (ArgNo < params->size() && params->get(ArgNo)->hasName()) {
      auto paramName = params->get(ArgNo)->getNameStr();
      diagnose(Loc, diag::anon_closure_arg_in_closure_with_args_typo, paramName)
        .fixItReplace(Loc, paramName);
      return new (Context) DeclRefExpr(params->get(ArgNo), DeclNameLoc(Loc),
                                       /*Implicit=*/false);
    } else {
      diagnose(Loc, diag::anon_closure_arg_in_closure_with_args);
      return new (Context) ErrorExpr(Loc);
    }
  }

  auto leftBraceLoc = AnonClosureVars.back().first;
  auto &decls = AnonClosureVars.back().second;
  while (ArgNo >= decls.size()) {
    unsigned nextIdx = decls.size();
    SmallVector<char, 4> StrBuf;
    StringRef varName = ("$" + Twine(nextIdx)).toStringRef(StrBuf);
    Identifier ident = Context.getIdentifier(varName);
    SourceLoc varLoc = leftBraceLoc;
    auto *var = new (Context)
        ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                  Identifier(), varLoc, ident, closure);
    var->setImplicit();
    decls.push_back(var);
  }

  return new (Context) DeclRefExpr(decls[ArgNo], DeclNameLoc(Loc),
                                   /*Implicit=*/false);
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
ParserResult<Expr>
Parser::parseExprList(tok leftTok, tok rightTok, SyntaxKind Kind) {
  SmallVector<Expr*, 8> subExprs;
  SmallVector<Identifier, 8> subExprNames;
  SmallVector<SourceLoc, 8> subExprNameLocs;
  Expr *trailingClosure = nullptr;

  SourceLoc leftLoc, rightLoc;
  ParserStatus status = parseExprList(leftTok, rightTok, /*isPostfix=*/false,
                                      /*isExprBasic=*/true,
                                      leftLoc,
                                      subExprs,
                                      subExprNames,
                                      subExprNameLocs,
                                      rightLoc,
                                      trailingClosure,
                                      Kind);

  // A tuple with a single, unlabeled element is just parentheses.
  if (subExprs.size() == 1 &&
      (subExprNames.empty() || subExprNames[0].empty())) {
    return makeParserResult(
        status, new (Context) ParenExpr(leftLoc, subExprs[0], rightLoc,
                                        /*hasTrailingClosure=*/false));
  }

  return makeParserResult(
      status,
      TupleExpr::create(Context, leftLoc, subExprs, subExprNames,
                        subExprNameLocs, rightLoc, /*HasTrailingClosure=*/false,
                        /*Implicit=*/false));
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
ParserStatus Parser::parseExprList(tok leftTok, tok rightTok,
                                   bool isPostfix,
                                   bool isExprBasic,
                                   SourceLoc &leftLoc,
                                   SmallVectorImpl<Expr *> &exprs,
                                   SmallVectorImpl<Identifier> &exprLabels,
                                   SmallVectorImpl<SourceLoc> &exprLabelLocs,
                                   SourceLoc &rightLoc,
                                   Expr *&trailingClosure,
                                   SyntaxKind Kind) {
  trailingClosure = nullptr;

  StructureMarkerRAII ParsingExprList(*this, Tok);
  
  if (ParsingExprList.isFailed()) {
    return makeParserError();
  }

  leftLoc = consumeToken(leftTok);
  ParserStatus status = parseList(rightTok, leftLoc, rightLoc,
                                  /*AllowSepAfterLast=*/false,
                                  rightTok == tok::r_paren
                                    ? diag::expected_rparen_expr_list
                                    : diag::expected_rsquare_expr_list,
                                  Kind,
                                  [&] (SmallVectorImpl<ASTNode> *Elements,
                                       bool IsActive) -> ParserStatus {
    Identifier FieldName;
    SourceLoc FieldNameLoc;
    if (Kind != SyntaxKind::YieldStmt)
      parseOptionalArgumentLabel(FieldName, FieldNameLoc);

    // See if we have an operator decl ref '(<op>)'. The operator token in
    // this case lexes as a binary operator because it neither leads nor
    // follows a proper subexpression.
    ParserStatus Status;
    Expr *SubExpr = nullptr;
    if (Tok.isBinaryOperator() && peekToken().isAny(rightTok, tok::comma)) {
      SyntaxParsingContext operatorContext(SyntaxContext,
                                           SyntaxKind::IdentifierExpr);
      SourceLoc Loc;
      Identifier OperName;
      if (parseAnyIdentifier(OperName, Loc, diag::expected_operator_ref)) {
        return makeParserError();
      }
      // Bypass local lookup. Use an 'Ordinary' reference kind so that the
      // reference may resolve to any unary or binary operator based on
      // context.
      SubExpr = new(Context) UnresolvedDeclRefExpr(OperName,
                                                   DeclRefKind::Ordinary,
                                                   DeclNameLoc(Loc));
    } else if (Kind == SyntaxKind::FunctionCallArgumentList &&
               Tok.is(tok::code_complete)) {
      // Handle call arguments specially because it may need argument labels.
      auto CCExpr = new (Context) CodeCompletionExpr(Tok.getLoc());
      if (CodeCompletion)
        CodeCompletion->completeCallArg(CCExpr);
      consumeIf(tok::code_complete);
      SubExpr = CCExpr;
      Status.setHasCodeCompletion();
    } else {
      auto ParsedSubExpr = parseExpr(diag::expected_expr_in_expr_list);
      SubExpr = ParsedSubExpr.getPtrOrNull();
      Status = ParsedSubExpr;
    }

    // If we got a subexpression, add it.
    if (SubExpr) {
      // Update names and locations.
      if (!exprLabels.empty()) {
        exprLabels.push_back(FieldName);
        exprLabelLocs.push_back(FieldNameLoc);
      } else if (FieldNameLoc.isValid()) {
        exprLabels.resize(exprs.size());
        exprLabels.push_back(FieldName);

        exprLabelLocs.resize(exprs.size());
        exprLabelLocs.push_back(FieldNameLoc);
      }

      // Add the subexpression.
      exprs.push_back(SubExpr);
    }

    return Status;
  });

  // If we aren't interested in trailing closures, or there isn't a valid one,
  // we're done.
  if (!isPostfix || Tok.isNot(tok::l_brace) ||
      !isValidTrailingClosure(isExprBasic, *this))
    return status;

  // Parse the closure.
  ParserResult<Expr> closure =
    parseTrailingClosure(SourceRange(leftLoc, rightLoc));
  status |= closure;
  if (closure.isNull())
    return status;

  // Record the trailing closure.
  trailingClosure = closure.get();

  return status;
}

ParserResult<Expr> Parser::parseTrailingClosure(SourceRange calleeRange) {
  SourceLoc braceLoc = Tok.getLoc();

  // Record the line numbers for the diagnostics below.
  // Note that *do not* move this to after 'parseExprClosure()' it slows down
  // 'getLineNumber()' call because of cache in SourceMgr.
  auto origLine = SourceMgr.getLineNumber(calleeRange.End);
  auto braceLine = SourceMgr.getLineNumber(braceLoc);

  // Parse the closure.
  ParserResult<Expr> closure = parseExprClosure();
  if (closure.isNull())
    return makeParserError();

  // Warn if the trailing closure is separated from its callee by more than
  // one line. A single-line separation is acceptable for a trailing closure
  // call, and will be diagnosed later only if the call fails to typecheck.
  if (braceLine > origLine + 1) {
    diagnose(braceLoc, diag::trailing_closure_after_newlines);
    diagnose(calleeRange.Start, diag::trailing_closure_callee_here);
    
    auto *CE = dyn_cast<ClosureExpr>(closure.get());
    if (CE && CE->hasAnonymousClosureVars() &&
        CE->getParameters()->size() == 0) {
      diagnose(braceLoc, diag::brace_stmt_suggest_do)
        .fixItInsert(braceLoc, "do ");
    }
  }

  return closure;
}
 
/// \brief Parse an object literal expression.
///
/// expr-literal:
///   '#' identifier expr-paren
ParserResult<Expr>
Parser::parseExprObjectLiteral(ObjectLiteralExpr::LiteralKind LitKind,
                               bool isExprBasic) {
  SyntaxParsingContext ObjectLiteralContext(SyntaxContext,
                                            SyntaxKind::ObjectLiteralExpr);
  SourceLoc PoundLoc = consumeToken();
  // Parse a tuple of args
  if (!Tok.is(tok::l_paren)) {
    diagnose(Tok, diag::expected_arg_list_in_object_literal);
    return makeParserError();
  }

  // Parse the argument list.
  SourceLoc lParenLoc, rParenLoc;
  SmallVector<Expr *, 2> args;
  SmallVector<Identifier, 2> argLabels;
  SmallVector<SourceLoc, 2> argLabelLocs;
  Expr *trailingClosure;

  ParserStatus status = parseExprList(tok::l_paren, tok::r_paren,
                                      /*isPostfix=*/true, isExprBasic,
                                      lParenLoc, args, argLabels,
                                      argLabelLocs,
                                      rParenLoc,
                                      trailingClosure,
                                      SyntaxKind::FunctionCallArgumentList);
  if (status.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
  if (status.isError())
    return makeParserError();

  return makeParserResult(
    ObjectLiteralExpr::create(Context, PoundLoc, LitKind, lParenLoc, args,
                              argLabels, argLabelLocs, rParenLoc,
                              trailingClosure, /*implicit=*/false));
}

/// \brief Parse and diagnose unknown pound expression
///
/// If it look like a legacy (Swift 2) object literal expression, suggest fix-it
/// to use new object literal syntax.
///
/// expr-unknown-pound:
///   '#' identifier expr-paren?
///   '[' '#' identifier expr-paren? '#' ']' ; Legacy object literal
ParserResult<Expr> Parser::parseExprPoundUnknown(SourceLoc LSquareLoc) {
  SourceLoc PoundLoc = consumeToken(tok::pound);

  assert(Tok.is(tok::identifier) && !Tok.isEscapedIdentifier() &&
         PoundLoc.getAdvancedLoc(1) == Tok.getLoc());

  Identifier Name;
  SourceLoc NameLoc = consumeIdentifier(&Name);

  // Parse arguments if exist.
  SourceLoc LParenLoc, RParenLoc;
  SmallVector<SourceLoc, 2> argLabelLocs;
  SmallVector<Expr *, 2> args;
  SmallVector<Identifier, 2> argLabels;
  Expr *trailingClosure;
  if (Tok.isFollowingLParen()) {
    // Parse arguments.
    ParserStatus status =
        parseExprList(tok::l_paren, tok::r_paren,
                      /*isPostfix=*/false, /*isExprBasic*/ false, LParenLoc,
                      args, argLabels, argLabelLocs, RParenLoc, trailingClosure,
                      SyntaxKind::FunctionCallArgumentList);
    if (status.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (status.isError())
      return makeParserError();
  }

  std::pair<StringRef, StringRef> NewNameArgPair =
      llvm::StringSwitch<std::pair<StringRef, StringRef>>(Name.str())
          .Case("Color", {"colorLiteral", "red"})
          .Case("Image", {"imageLiteral", "resourceName"})
          .Case("FileReference", {"fileLiteral", "resourceName"})
          .Default({});

  // If it's not legacy object literal, we don't know how to handle this.
  if (NewNameArgPair.first.empty()) {
    diagnose(PoundLoc, diag::unknown_pound_expr, Name.str());
    return makeParserError();
  }

  // Diagnose legacy object literal.

  // Didn't have arguments.
  if (LParenLoc.isInvalid()) {
    diagnose(Tok.getLoc(), diag::expected_arg_list_in_object_literal);
    return makeParserError();
  }

  // If it's started with '[', try to parse closing '#]'.
  SourceLoc RPoundLoc, RSquareLoc;
  if (LSquareLoc.isValid() && consumeIf(tok::pound, RPoundLoc))
    consumeIf(tok::r_square, RSquareLoc);

  auto diag = diagnose(LSquareLoc.isValid() ? LSquareLoc : PoundLoc,
                       diag::legacy_object_literal, LSquareLoc.isValid(),
                       Name.str(), NewNameArgPair.first);

  // Remove '[' if exist.
  if (LSquareLoc.isValid())
    diag.fixItRemove(LSquareLoc);
  // Replace the literal name.
  diag.fixItReplace(NameLoc, NewNameArgPair.first);
  // Replace the first argument.
  if (!argLabelLocs.empty() && argLabelLocs[0].isValid())
    diag.fixItReplace(argLabelLocs[0], NewNameArgPair.second);
  // Remove '#]' if exist.
  if (RPoundLoc.isValid())
    diag.fixItRemove(
        {RPoundLoc, RSquareLoc.isValid() ? RSquareLoc : RPoundLoc});

  return makeParserError();
}

/// \brief Handle code completion after pound in expression position.
///
/// In case it's in a stmt condition position, specify \p ParentKind to
/// decide the position accepts #available(...) condtion.
///
/// expr-pound-codecompletion:
///   '#' code-completion-token
ParserResult<Expr>
Parser::parseExprPoundCodeCompletion(Optional<StmtKind> ParentKind) {
  assert(Tok.is(tok::pound) && peekToken().is(tok::code_complete) &&
         Tok.getLoc().getAdvancedLoc(1) == peekToken().getLoc());
  consumeToken(); // '#' token.
  auto CodeCompletionPos = consumeToken();
  auto Expr = new (Context) CodeCompletionExpr(CodeCompletionPos);
  if (CodeCompletion)
    CodeCompletion->completeAfterPoundExpr(Expr, ParentKind);
  return makeParserCodeCompletionResult(Expr);
}

/// \brief Parse an expression call suffix.
///
/// expr-call-suffix:
///   expr-paren
///   expr-closure (except in expr-basic)
ParserResult<Expr>
Parser::parseExprCallSuffix(ParserResult<Expr> fn, bool isExprBasic) {
  assert(Tok.isFollowingLParen() && "Not a call suffix?");

  // Parse the first argument.

  // If there is a code completion token right after the '(', do a special case
  // callback.
  if (peekToken().is(tok::code_complete) && CodeCompletion) {
    consumeToken(tok::l_paren);
    auto CCE = new (Context) CodeCompletionExpr(Tok.getLoc());
    auto Result = makeParserResult(fn,
      CallExpr::create(Context, fn.get(), SourceLoc(),
                       { CCE },
                       { Identifier() },
                       { },
                       SourceLoc(),
                       /*trailingClosure=*/nullptr,
                       /*implicit=*/false));
    CodeCompletion->completePostfixExprParen(fn.get(), CCE);
    // Eat the code completion token because we handled it.
    consumeToken(tok::code_complete);
    Result.setHasCodeCompletion();
    return Result;
  }

  // Parse the argument list.
  SourceLoc lParenLoc, rParenLoc;
  SmallVector<Expr *, 2> args;
  SmallVector<Identifier, 2> argLabels;
  SmallVector<SourceLoc, 2> argLabelLocs;
  Expr *trailingClosure;

  ParserStatus status = parseExprList(tok::l_paren, tok::r_paren,
                                      /*isPostfix=*/true, isExprBasic,
                                      lParenLoc, args, argLabels,
                                      argLabelLocs,
                                      rParenLoc,
                                      trailingClosure,
                                      SyntaxKind::FunctionCallArgumentList);

  // Form the call.
  return makeParserResult(
      status | fn, CallExpr::create(Context, fn.get(), lParenLoc, args,
                                    argLabels, argLabelLocs, rParenLoc,
                                    trailingClosure, /*implicit=*/false));
}

/// parseExprCollection - Parse a collection literal expression.
///
///   expr-collection:
///     expr-array
///     expr-dictionary
//      lsquare-starting ']'
ParserResult<Expr> Parser::parseExprCollection() {
  SyntaxParsingContext ArrayOrDictContext(SyntaxContext);
  SourceLoc LSquareLoc = consumeToken(tok::l_square);

  Parser::StructureMarkerRAII ParsingCollection(
                                *this, LSquareLoc,
                                StructureMarkerKind::OpenSquare);

  // [] is always an array.
  if (Tok.is(tok::r_square)) {
    if (SyntaxContext->isEnabled())
      SyntaxContext->addSyntax(
          SyntaxFactory::makeBlankArrayElementList(Context.getSyntaxArena()));
    SourceLoc RSquareLoc = consumeToken(tok::r_square);
    ArrayOrDictContext.setCreateSyntax(SyntaxKind::ArrayExpr);
    return makeParserResult(
                    ArrayExpr::create(Context, LSquareLoc, {}, {}, RSquareLoc));
  }

  // [:] is always an empty dictionary.
  if (Tok.is(tok::colon) && peekToken().is(tok::r_square)) {
    consumeToken(tok::colon);
    SourceLoc RSquareLoc = consumeToken(tok::r_square);
    ArrayOrDictContext.setCreateSyntax(SyntaxKind::DictionaryExpr);
    return makeParserResult(
               DictionaryExpr::create(Context, LSquareLoc, {}, {}, RSquareLoc));
  }

  // [#identifier is likely to be a legacy object literal.
  if (Tok.is(tok::pound) && peekToken().is(tok::identifier) &&
      !peekToken().isEscapedIdentifier() &&
      LSquareLoc.getAdvancedLoc(1) == Tok.getLoc() &&
      Tok.getLoc().getAdvancedLoc(1) == peekToken().getLoc()) {
    ArrayOrDictContext.setCoerceKind(SyntaxContextKind::Expr);
    return parseExprPoundUnknown(LSquareLoc);
  }

  bool ParseDict;
  {
    BacktrackingScope Scope(*this);
    auto HasDelayedDecl = State->hasDelayedDecl();

    // Skip any initial conditionals.
    while (Tok.isAny(tok::pound_if, tok::pound_elseif,
                     tok::pound_else, tok::pound_endif)) {
      consumeToken();
      skipUntilTokenOrEndOfLine(tok::NUM_TOKENS);
    }

    // Parse the first expression.
    ParserResult<Expr> FirstExpr
      = parseExpr(diag::expected_expr_in_collection_literal);
    if (FirstExpr.isNull() || Tok.is(tok::eof)) {
      skipUntil(tok::r_square);
      if (Tok.is(tok::r_square))
        consumeToken();
      Scope.cancelBacktrack();
      ArrayOrDictContext.setCoerceKind(SyntaxContextKind::Expr);
      return FirstExpr;
    }
    if (!HasDelayedDecl)
      State->takeDelayedDeclState();
    // If we have a ':', this is a dictionary literal.
    ParseDict = Tok.is(tok::colon);
  }

  if (ParseDict) {
    ArrayOrDictContext.setCreateSyntax(SyntaxKind::DictionaryExpr);
    return parseExprDictionary(LSquareLoc);
  } else {
    ArrayOrDictContext.setCreateSyntax(SyntaxKind::ArrayExpr);
    return parseExprArray(LSquareLoc);
  }
}

/// parseExprArray - Parse an array literal expression.
///
/// The lsquare-starting and first expression have already been
/// parsed, and are passed in as parameters.
///
///   expr-array:
///     '[' expr (',' expr)* ','? ']'
///     '[' ']'
ParserResult<Expr> Parser::parseExprArray(SourceLoc LSquareLoc) {
  SmallVector<Expr *, 8> SubExprs;
  SmallVector<SourceLoc, 8> CommaLocs;
  Parser::ConfigMap IfConfigMap;

  SourceLoc RSquareLoc;
  ParserStatus Status;
  Status |= parseList(tok::r_square, LSquareLoc, RSquareLoc,
                      /*AllowSepAfterLast=*/true,
                      diag::expected_rsquare_array_expr,
                      SyntaxKind::ArrayElementList,
                      [&] (SmallVectorImpl<ASTNode> *Elements,
                           bool IsActive) -> ParserStatus
  {
    ParserResult<Expr> Element
      = parseExpr(diag::expected_expr_in_collection_literal);
    if (auto elt = Element.getPtrOrNull()) {
      if (IsActive)
        SubExprs.push_back(IfConfigMap.registerActiveDataElement(elt));
      if (Elements)
        Elements->push_back(elt);
    }

    if (Tok.is(tok::comma) && IsActive)
      CommaLocs.push_back(Tok.getLoc());
    return Element;
  }, &IfConfigMap);

  return makeParserResult(Status, IfConfigMap.closeCollection(
          ArrayExpr::create(Context, LSquareLoc, SubExprs, CommaLocs,
                            RSquareLoc)));
}

/// parseExprDictionary - Parse a dictionary literal expression.
///
/// The lsquare-starting and first key have already been parsed, and
/// are passed in as parameters.
///
///   expr-dictionary:
///     '[' expr ':' expr (',' expr ':' expr)* ','? ']'
///     '[' ':' ']'
ParserResult<Expr> Parser::parseExprDictionary(SourceLoc LSquareLoc) {
  // Each subexpression is a (key, value) tuple.
  // FIXME: We're not tracking the colon locations in the AST.
  SmallVector<Expr *, 8> SubExprs;
  SmallVector<SourceLoc, 8> CommaLocs;
  Parser::ConfigMap IfConfigMap;
  SourceLoc RSquareLoc;

  ParserStatus Status;
  Status |=
      parseList(tok::r_square, LSquareLoc, RSquareLoc,
                /*AllowSepAfterLast=*/true,
                diag::expected_rsquare_array_expr,
                SyntaxKind::DictionaryElementList,
                [&](SmallVectorImpl<ASTNode> *Elements,
                    bool IsActive) -> ParserStatus {
    // Parse the next key.
    ParserResult<Expr> Key;

    Key = parseExpr(diag::expected_key_in_dictionary_literal);
    if (Key.isNull())
      return Key;

    // Parse the ':'.
    if (Tok.isNot(tok::colon)) {
      diagnose(Tok, diag::expected_colon_in_dictionary_literal);
      return ParserStatus(Key) | makeParserError();
    }
    consumeToken();

    // Parse the next value.
    ParserResult<Expr> Value =
        parseExpr(diag::expected_value_in_dictionary_literal);

    if (Value.isNull())
      Value = makeParserResult(Value, new (Context) ErrorExpr(PreviousLoc));

    // Add this key/value pair.
    Expr *Exprs[] = {Key.get(), Value.get()};
    auto keyValue = TupleExpr::createImplicit(Context, Exprs, { });
    if (IsActive)
      SubExprs.push_back(IfConfigMap.registerActiveDataElement(keyValue));
    if (Elements)
      Elements->push_back(keyValue);

    if (Tok.is(tok::comma) && IsActive)
      CommaLocs.push_back(Tok.getLoc());

    return ParserStatus(Key) | ParserStatus(Value);
  }, &IfConfigMap);

  return makeParserResult(Status, IfConfigMap.closeCollection(
          DictionaryExpr::create(Context, LSquareLoc, SubExprs, CommaLocs,
                                 RSquareLoc)));
}

void Parser::addPatternVariablesToScope(ArrayRef<Pattern *> Patterns) {
  for (Pattern *Pat : Patterns) {
    Pat->forEachVariable([&](VarDecl *VD) {
      if (VD->hasName()) {
        // Add any variable declarations to the current scope.
        addToScope(VD);
      }
    });
  }
}

void Parser::addParametersToScope(ParameterList *PL) {
  for (auto param : *PL)
    if (param->hasName())
      addToScope(param);
}



/// Parse availability query specification.
///
///  availability-spec:
///     '*'
///     language-version-constraint-spec
///     platform-version-constraint-spec
ParserResult<AvailabilitySpec> Parser::parseAvailabilitySpec() {
  if (Tok.isBinaryOperator() && Tok.getText() == "*") {
    SourceLoc StarLoc = Tok.getLoc();
    consumeToken();

    return makeParserResult(new (Context) OtherPlatformAvailabilitySpec(StarLoc));
  }
  if (Tok.isIdentifierOrUnderscore() && Tok.getText() == "swift") {
    return parseLanguageVersionConstraintSpec();
  }

  return parsePlatformVersionConstraintSpec();
}

/// Parse language-version constraint specification.
///
///  language-version-constraint-spec:
///     "swift" version-tuple
ParserResult<LanguageVersionConstraintAvailabilitySpec>
Parser::parseLanguageVersionConstraintSpec() {
  SyntaxParsingContext VersionRestrictionContext(
      SyntaxContext, SyntaxKind::AvailabilityVersionRestriction);
  SourceLoc SwiftLoc;
  llvm::VersionTuple Version;
  SourceRange VersionRange;
  if (!(Tok.isIdentifierOrUnderscore() && Tok.getText() == "swift"))
    return nullptr;

  SwiftLoc = Tok.getLoc();
  consumeToken();
  if (parseVersionTuple(Version, VersionRange,
                        diag::avail_query_expected_version_number)) {
    return nullptr;
  }
  return makeParserResult(new (Context)
                          LanguageVersionConstraintAvailabilitySpec(
                            SwiftLoc, Version, VersionRange));
}

/// Parse platform-version constraint specification.
///
///  platform-version-constraint-spec:
///     identifier version-comparison version-tuple
ParserResult<PlatformVersionConstraintAvailabilitySpec>
Parser::parsePlatformVersionConstraintSpec() {
  SyntaxParsingContext VersionRestrictionContext(
      SyntaxContext, SyntaxKind::AvailabilityVersionRestriction);

  Identifier PlatformIdentifier;
  SourceLoc PlatformLoc;
  if (Tok.is(tok::code_complete)) {
    consumeToken();
    if (CodeCompletion) {
      CodeCompletion->completePoundAvailablePlatform();
    }
    return makeParserCodeCompletionStatus();
  }

  if (parseIdentifier(PlatformIdentifier, PlatformLoc,
                      diag::avail_query_expected_platform_name)) {
    return nullptr;
  }

  if (Tok.isBinaryOperator() && Tok.getText() == ">=") {
    diagnose(Tok, diag::avail_query_version_comparison_not_needed)
        .fixItRemove(Tok.getLoc());
    consumeToken();
  }

  llvm::VersionTuple Version;
  SourceRange VersionRange;

  if (parseVersionTuple(Version, VersionRange,
                        diag::avail_query_expected_version_number)) {
    return nullptr;
  }

  Optional<PlatformKind> Platform =
      platformFromString(PlatformIdentifier.str());

  if (!Platform.hasValue() || Platform.getValue() == PlatformKind::none) {
    diagnose(Tok, diag::avail_query_unrecognized_platform_name,
             PlatformIdentifier);
    return nullptr;
  }

  // Register the platform name as a keyword token.
  TokReceiver->registerTokenKindChange(PlatformLoc, tok::contextual_keyword);

  return makeParserResult(new (Context) PlatformVersionConstraintAvailabilitySpec(
      Platform.getValue(), PlatformLoc, Version, VersionRange));
}
