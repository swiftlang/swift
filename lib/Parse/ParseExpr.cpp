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
#include "swift/AST/DiagnosticsParse.h"
#include "swift/Basic/EditorPlaceholder.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
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
                                          ArrayRef<Identifier> namesIn,
                                          ArrayRef<SourceLoc> nameLocsIn,
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

  SmallVector<Identifier, 4> names;
  SmallVector<SourceLoc, 4> nameLocs;
  if (!namesIn.empty()) {
    names.append(namesIn.begin(), namesIn.end());
    names.push_back(Identifier());

    nameLocs.append(nameLocsIn.begin(), nameLocsIn.end());
    nameLocs.push_back(SourceLoc());
  }

  // Form a full tuple expression.
  return TupleExpr::create(context, leftParen, elements, names, nameLocs,
                           rightParen, /*hasTrailingClosure=*/true,
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
                                        tuple->getElementNameLocs(),
                                        tuple->getRParenLoc(),
                                        closure);
  }

  // Deconstruct a parenthesized expression.
  auto paren = dyn_cast<ParenExpr>(arg);
  return createArgWithTrailingClosure(context,
                                      paren->getLParenLoc(),
                                      paren->getSubExpr(),
                                      { },
                                      { },
                                      paren->getRParenLoc(), closure);
}

/// parseExpr
///
///   expr:
///     expr-sequence(basic | trailing-closure)
///
/// \param isExprBasic Whether we're only parsing an expr-basic.
ParserResult<Expr> Parser::parseExprImpl(Diag<> Message, bool isExprBasic) {
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
    return makeParserResult(new (Context) UnresolvedPatternExpr(pattern.get()));
  }
  
  ParserResult<Expr> expr = parseExprSequence(Message, isExprBasic);
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
                                             bool isConfigCondition) {
  SmallVector<Expr*, 8> SequencedExprs;
  SourceLoc startLoc = Tok.getLoc();
  
  while (true) {
    if (isConfigCondition && Tok.isAtStartOfLine())
      break;
    
    // Parse a unary expression.
    ParserResult<Expr> Primary =
      parseExprSequenceElement(Message, isExprBasic);
    if (Primary.hasCodeCompletion())
      return Primary;
    if (Primary.isNull())
      return nullptr;
    SequencedExprs.push_back(Primary.get());
    
parse_operator:
    switch (Tok.getKind()) {
    case tok::oper_binary_spaced:
    case tok::oper_binary_unspaced: {
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
      // If we're parsing an expression as the body of a refutable var/let
      // pattern, then an assignment doesn't make sense.  In a "if let"
      // statement the equals is the start of the condition, so don't parse it
      // as a binary operator.
      if (InVarOrLetPattern)
        goto done;
      
      SourceLoc equalsLoc = consumeToken();
      auto *assign = new (Context) AssignExpr(equalsLoc);
      SequencedExprs.push_back(assign);
      Message = diag::expected_expr_assignment;
      break;
    }
        
    case tok::kw_is: {
      // Parse a type after the 'is' token instead of an expression.
      ParserResult<Expr> is = parseExprIs();
      if (is.isNull() || is.hasCodeCompletion())
        return nullptr;
      
      // Store the expr itself as a placeholder RHS. The real RHS is the
      // type parameter stored in the node itself.
      SequencedExprs.push_back(is.get());
      SequencedExprs.push_back(is.get());
      
      // We already parsed the right operand as part of the 'is' production.
      // Jump directly to parsing another operator.
      goto parse_operator;
    }
        
    case tok::kw_as: {
      ParserResult<Expr> as = parseExprAs();
      if (as.isNull() || as.hasCodeCompletion())
        return nullptr;
        
      // Store the expr itself as a placeholder RHS. The real RHS is the
      // type parameter stored in the node itself.
      SequencedExprs.push_back(as.get());
      SequencedExprs.push_back(as.get());
      
      // We already parsed the right operand as part of the 'is' production.
      // Jump directly to parsing another operator.
      goto parse_operator;
    }
        
    default:
      // If the next token is not a binary operator, we're done.
      goto done;
    }
  }
done:
  
  if (SequencedExprs.empty()) {
    if (isConfigCondition) {
      diagnose(startLoc, diag::expected_close_to_config_stmt);
      return makeParserError();
    } else {
      // If we had semantic errors, just fail here.
      assert(!SequencedExprs.empty());
    }
  }

  // If we saw no operators, don't build a sequence.
  if (SequencedExprs.size() == 1)
    return makeParserResult(SequencedExprs[0]);

  return makeParserResult(SequenceExpr::create(Context, SequencedExprs));
}

/// parseExprSequenceElement
///
///   expr-sequence-element(Mode):
///     'try' expr-unary(Mode)
///     expr-unary(Mode)
///
/// 'try' is not actually allowed at an arbitrary position of a
/// sequence, but this isn't enforced until sequence-folding.
ParserResult<Expr> Parser::parseExprSequenceElement(Diag<> message,
                                                    bool isExprBasic) {
  SourceLoc tryLoc;
  bool hadTry = consumeIf(tok::kw_try, tryLoc);
  SourceLoc exclaimLoc;
  bool hadExclaim = (hadTry && consumeIf(tok::exclaim_postfix, exclaimLoc));

  ParserResult<Expr> sub = parseExprUnary(message, isExprBasic);

  if (hadTry && !sub.hasCodeCompletion() && !sub.isNull()) {
    if (hadExclaim) {
      sub = makeParserResult(new (Context) ForceTryExpr(tryLoc, sub.get(),
                                                        exclaimLoc));
    } else {
      sub = makeParserResult(new (Context) TryExpr(tryLoc, sub.get()));
    }
  }

  return sub;
}

/// parseExprUnary
///
///   expr-unary(Mode):
///     expr-postfix(Mode)
///     operator-prefix expr-unary(Mode)
///     '&' expr-unary(Mode)
///
ParserResult<Expr> Parser::parseExprUnary(Diag<> Message, bool isExprBasic) {
  UnresolvedDeclRefExpr *Operator;
  switch (Tok.getKind()) {
  default:
    // If the next token is not an operator, just parse this as expr-postfix.
    return parseExprPostfix(Message, isExprBasic);

  case tok::amp_prefix: {
    SourceLoc Loc = consumeToken(tok::amp_prefix);

    ParserResult<Expr> SubExpr = parseExprUnary(Message, isExprBasic);
    if (SubExpr.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (SubExpr.isNull())
      return nullptr;
    return makeParserResult(
        new (Context) InOutExpr(Loc, SubExpr.get(), Type()));
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
  if (SubExpr.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
  if (SubExpr.isNull())
    return nullptr;

  // Check if we have an unary '-' with number literal sub-expression, for
  // example, "-42" or "-1.25".
  if (auto *LE = dyn_cast<NumberLiteralExpr>(SubExpr.get())) {
    if (Operator->hasName() && Operator->getName().str() == "-") {
      LE->setNegative(Operator->getLoc());
      return makeParserResult(LE);
    }
  }

  return makeParserResult(
      new (Context) PrefixUnaryExpr(Operator, SubExpr.get()));
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
  return new (Context) UnresolvedDeclRefExpr(name, refKind, loc);
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
///     'super' '.' 'init' expr-paren?
///     'super' '.' 'init' identifier expr-call-suffix
///   expr-super-subscript:
///     'super' '[' expr ']'
ParserResult<Expr> Parser::parseExprSuper() {
  // Parse the 'super' reference.
  SourceLoc superLoc = consumeToken(tok::kw_super);
  
  VarDecl *selfDecl = getImplicitSelfDeclForSuperContext(*this,
                                                         CurDeclContext,
                                                         superLoc);
  bool ErrorOccurred = selfDecl == nullptr;

  Expr *superRef = !ErrorOccurred
    ? cast<Expr>(new (Context) SuperRefExpr(selfDecl, superLoc,
                                            /*Implicit=*/false))
    : cast<Expr>(new (Context) ErrorExpr(superLoc));
  
  if (Tok.is(tok::period)) {
    // 'super.' must be followed by a member or initializer ref.

    SourceLoc dotLoc = consumeToken(tok::period);
    
    // FIXME: This code is copy-paste from the general handling for kw_init.
    if (Tok.is(tok::kw_init)) {

      if (ErrorOccurred)
        return makeParserError();

      // super.init
      SourceLoc ctorLoc = consumeToken();
      
      // Check that we're actually in an initializer
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
        // Parse initializer arguments.
        ParserResult<Expr> arg = parseExprList(tok::l_paren, tok::r_paren);
        if (arg.hasCodeCompletion())
          return makeParserCodeCompletionResult<Expr>();

        if (arg.isParseError())
          return makeParserError();
        
        result = new (Context) CallExpr(result, arg.get(), /*Implicit=*/false);
      } else {
        // It's invalid to refer to an uncalled initializer.
        diagnose(ctorLoc, diag::super_initializer_must_be_called);
        result->setType(ErrorType::get(Context));
        return makeParserErrorResult(result);
      }

      // The result of the called initializer is used to rebind 'self'.
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

/// Disambiguate the parse after '{' token that is in a place that might be
/// the start of a trailing closure, or start the variable accessor block.
///
/// Check to see if the '{' is followed by a 'didSet' or a 'willSet' label,
/// possibly preceeded by attributes.  If so, we disambiguate the parse as the
/// start of a get-set block in a variable definition (not as a trailing
/// closure).
static bool isStartOfGetSetAccessor(Parser &P) {
  assert(P.Tok.is(tok::l_brace) && "not checking a brace?");
  
  // The only case this can happen is if the accessor label is immediately after
  // a brace (possibly preceeded by attributes).  "get" is implicit, so it can't
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

  // If we don't have attributes, then it can not be an accessor block.
  if (NextToken.isNot(tok::at_sign))
    return false;

  Parser::BacktrackingScope Backtrack(P);

  // Eat the "{".
  P.consumeToken(tok::l_brace);

  // Eat attributes, if present.
  if (!P.canParseAttributes())
    return false;

  // Check if we have 'didSet'/'willSet' after attributes.
  return P.Tok.isContextualKeyword("didSet") ||
         P.Tok.isContextualKeyword("willSet");
}

/// Map magic literal tokens such as __FILE__ to their
/// MagicIdentifierLiteralExpr kind.
MagicIdentifierLiteralExpr::Kind getMagicIdentifierLiteralKind(tok Kind) {
  switch (Kind) {
  case tok::kw___COLUMN__:
    return MagicIdentifierLiteralExpr::Kind::Column;
  case tok::kw___FILE__:
    return MagicIdentifierLiteralExpr::Kind::File;
  case tok::kw___FUNCTION__:
    return MagicIdentifierLiteralExpr::Kind::Function;
  case tok::kw___LINE__:
    return MagicIdentifierLiteralExpr::Kind::Line;
  case tok::kw___DSO_HANDLE__:
    return MagicIdentifierLiteralExpr::Kind::DSOHandle;

  default:
    llvm_unreachable("not a magic literal");
  }
}

/// Look ahead to see if we have '.foo(', '.foo[', '.foo{', or '.foo.',
/// or if '.foo' appears on a line by itself, which is an indication of a
/// builder-pattern-like method chain.
bool Parser::periodPrefixIsFollowedByBuilderPatternLikeExpr() {
  assert(Tok.is(tok::period_prefix) && "Tok should be the period_prefix token");
  if (peekToken().is(tok::identifier) || peekToken().is(tok::integer_literal)) {
    BacktrackingScope BS(*this);
    consumeToken(tok::period_prefix);
    return peekToken().isFollowingLParen() ||
      peekToken().isFollowingLSquare() ||
      peekToken().isAtStartOfLine() ||
      peekToken().is(tok::period) ||
      peekToken().is(tok::l_brace);
  }
  return false;
}

/// parseExprPostfix
///
///   expr-literal:
///     integer_literal
///     floating_literal
///     string_literal
///     character_literal
///     nil
///     true
///     false
///     '__FILE__'
///     '__LINE__'
///     '__COLUMN__'
///     '__FUNCTION__'
///     '__DSO_HANDLE__'
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
///
///   expr-delayed-identifier:
///     '.' identifier
///
///   expr-discard:
///     '_'
///
///   expr-dot:
///     expr-postfix '.' 'type'
///     expr-postfix '.' identifier generic-args? expr-call-suffix?
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
  
  case tok::at_sign:
    // Objective-C programmers habitually type @"foo", so recover gracefully
    // with a fixit.  If this isn't @"foo", just handle it like an unknown
    // input.
    if (peekToken().isNot(tok::string_literal))
      goto UnknownCharacter;
    
    diagnose(Tok.getLoc(), diag::string_literal_no_atsign)
      .fixItRemove(Tok.getLoc());
    consumeToken(tok::at_sign);
    SWIFT_FALLTHROUGH;
      
  case tok::string_literal:  // "foo"
    Result = makeParserResult(parseExprStringLiteral());
    break;
  
  case tok::kw_nil:
    Result = makeParserResult(
                      new (Context) NilLiteralExpr(consumeToken(tok::kw_nil)));
    break;

  case tok::kw_true:
  case tok::kw_false: {
    bool isTrue = Tok.is(tok::kw_true);
    Result = makeParserResult(
               new (Context) BooleanLiteralExpr(isTrue, consumeToken()));
    break;
  }
    
  case tok::kw___FILE__:
  case tok::kw___LINE__:
  case tok::kw___COLUMN__:
  case tok::kw___FUNCTION__:
  case tok::kw___DSO_HANDLE__: {
    auto Kind = getMagicIdentifierLiteralKind(Tok.getKind());
    SourceLoc Loc = consumeToken();
    Result = makeParserResult(
       new (Context) MagicIdentifierLiteralExpr(Kind, Loc, /*Implicit=*/false));
    break;
  }
      
  case tok::identifier:  // foo
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
      auto pattern = createBindingFromPattern(loc, name,
                                              InVarOrLetPattern != IVOLP_InVar);
      Result = makeParserResult(new (Context) UnresolvedPatternExpr(pattern));
      break;
    }

    SWIFT_FALLTHROUGH;
  case tok::kw_self:     // self
  case tok::kw_Self:     // Self
    Result = makeParserResult(parseExprIdentifier());

    // If there is an expr-call-suffix, parse it and form a call.
    if (Tok.isFollowingLParen()) {
      Result = parseExprCallSuffix(Result);
      break;
    }

    break;
  case tok::dollarident: // $1
    Result = makeParserResult(parseExprAnonClosureArg());
    break;

    // If the next token is '_', parse a discard expression.
  case tok::kw__:
    Result = makeParserResult(
      new (Context) DiscardAssignmentExpr(consumeToken(), /*Implicit=*/false));
    break;

  case tok::l_brace:     // expr-closure
    Result = parseExprClosure();
    break;

  case tok::period:              //=.foo
  case tok::period_prefix: {     // .foo
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
      FltText = copyAndStripUnderscores(Context, FltText);
      
      consumeToken(tok::integer_literal);
      Result = makeParserResult(new (Context)
                                FloatLiteralExpr(FltText, DotLoc,
                                                 /*Implicit=*/false));
      break;
    }
    
    Identifier Name;
    SourceLoc NameLoc;
    if (parseIdentifier(Name, NameLoc,diag::expected_identifier_after_dot_expr))
      return nullptr;

    ParserResult<Expr> Arg;

    // Check for a () suffix, which indicates a call when constructing
    // this member.  Note that this cannot be the start of a new line.
    if (Tok.isFollowingLParen()) {
      Arg = parseExprList(tok::l_paren, tok::r_paren);
      if (Arg.hasCodeCompletion())
        return makeParserCodeCompletionResult<Expr>();
      if (Arg.isNull())
        return nullptr;
    }

    // Handle .foo by just making an AST node.
    Result = makeParserResult(
               new (Context) UnresolvedMemberExpr(DotLoc, NameLoc, Name,
                                                  Arg.getPtrOrNull()));
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

  case tok::l_square_lit: // [#Color(...)#], [#Image(...)#]
    Result = parseExprObjectLiteral();
    break;

  case tok::pound_available: {
    // For better error recovery, parse but reject #available in an expr
    // context.
    diagnose(Tok.getLoc(), diag::availability_query_outside_if_stmt_guard);
    auto res = parseStmtConditionPoundAvailable();
    if (res.isParseError() || res.isNull())
      return nullptr;
    Result = makeParserResult(
                  new (Context) ErrorExpr(res.get()->getSourceRange()));
    break;
  }

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
  UnknownCharacter:
    checkForInputIncomplete();
    // FIXME: offer a fixit: 'Self' -> 'self'
    diagnose(Tok, ID);
    return nullptr;
  }

  // If we had a parse error, don't attempt to parse suffixes.
  if (Result.isParseError())
    return Result;

  bool hasBindOptional = false;
    
  // Handle suffix expressions.
  while (1) {
    // FIXME: Better recovery.
    if (Result.isNull())
      return Result;

    // Check for a .foo suffix.
    SourceLoc TokLoc = Tok.getLoc();
    bool IsPeriod = false;
    // Look ahead to see if we have '.foo(', '.foo[', '.foo{',
    //   '.foo.1(', '.foo.1[', or '.foo.1{', or if the '.foo' appears on a line
    //   by itself.
    if (Tok.is(tok::period_prefix)) {
      IsPeriod = periodPrefixIsFollowedByBuilderPatternLikeExpr();
    }
    if (consumeIf(tok::period) || (IsPeriod && consumeIf(tok::period_prefix))) {
      // Non-identifier cases.
      if (Tok.isNot(tok::identifier) && Tok.isNot(tok::integer_literal)) {
        // A metatype expr.
        if (Tok.is(tok::kw_dynamicType)) {
          Result = makeParserResult(
            new (Context) DynamicTypeExpr(Result.get(), consumeToken(),
                                       Type()));
          continue;
        }
        
        // A '.self' expr.
        if (Tok.is(tok::kw_self)) {
          Result = makeParserResult(
            new (Context) DotSelfExpr(Result.get(), TokLoc, consumeToken()));
          continue;
        }
        
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

        // expr-init ::= expr-postfix '.' 'init'.
        if (Tok.is(tok::kw_init)) {
          // Form the reference to the constructor.
          Expr *initRef = new (Context) UnresolvedConstructorExpr(
                                          Result.get(),
                                          TokLoc,
                                          Tok.getLoc(),
                                          /*Implicit=*/false);
          SourceLoc initLoc = consumeToken(tok::kw_init);

          // FIXME: This is really a semantic restriction for 'self.init'
          // masquerading as a parser restriction.
          if (Tok.isFollowingLParen()) {
            // Parse initializer arguments.
            ParserResult<Expr> arg = parseExprList(tok::l_paren, tok::r_paren);
            if (arg.hasCodeCompletion())
              return makeParserCodeCompletionResult<Expr>();
            // FIXME: Unfortunate recovery here.
            if (arg.isNull())
              return nullptr;

            initRef = new (Context) CallExpr(initRef, arg.get(),
                                             /*Implicit=*/false);

            // Dig out the 'self' declaration we're using so we can rebind it.
            // FIXME: Should be in the type checker, not here.
            if (auto func = dyn_cast<ConstructorDecl>(CurDeclContext)) {
              if (auto selfDecl = func->getImplicitSelfDecl()) {
                initRef = new (Context) RebindSelfInConstructorExpr(initRef,
                                                                    selfDecl);
              }
            }
          } else {
            // It's invalid to refer to an uncalled initializer.
            diagnose(initLoc, diag::init_ref_must_be_called);
            initRef->setType(ErrorType::get(Context));
          }


          Result = makeParserResult(initRef);
          continue;
        }

        if (Tok.is(tok::code_complete)) {
          if (CodeCompletion && Result.isNonNull())
            CodeCompletion->completeDotExpr(Result.get(), /*DotLoc=*/TokLoc);
          // Eat the code completion token because we handled it.
          consumeToken(tok::code_complete);
          Result.setHasCodeCompletion();
          return Result;
        }
        checkForInputIncomplete();
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

      if (Result.isParseError())
        continue;

      Identifier Name = Context.getIdentifier(Tok.getText());
      SourceLoc NameLoc = Tok.getLoc();
      if (Tok.is(tok::identifier)) {
        consumeToken(tok::identifier);
        
        // If this is a selector reference, collect the selector pieces.
        bool IsSelector = false;
        if (Tok.is(tok::colon) && peekToken().isIdentifierOrUnderscore()) {
          BacktrackingScope BS(*this);

          consumeToken(); // ':'
          consumeToken(); // identifier or '_'
          IsSelector = consumeIf(tok::colon);
        }
        
        if (IsSelector) {
          // Collect the selector pieces.
          SmallVector<UnresolvedSelectorExpr::ComponentLoc, 2> Locs;
          SmallVector<Identifier, 2> ArgumentNames;
          
          Locs.push_back({NameLoc, consumeToken(tok::colon)});

          // Add entry for the unwritten first argument name.
          Locs.push_back({SourceLoc(), SourceLoc()});
          ArgumentNames.push_back(Identifier());
          while (Tok.isIdentifierOrUnderscore() && peekToken().is(tok::colon)) {
            Identifier SelName;
            if (Tok.is(tok::identifier))
              SelName = Context.getIdentifier(Tok.getText());
            SourceLoc SelLoc = consumeToken();
            SourceLoc ColonLoc = consumeToken(tok::colon);
            Locs.push_back({SelLoc, ColonLoc});
            ArgumentNames.push_back(SelName);
          }
          auto FullName = DeclName(Context, Name, ArgumentNames);
          Result = makeParserResult(
            UnresolvedSelectorExpr::create(Context, Result.get(), TokLoc,
                                           FullName, Locs));
        } else {
          Result = makeParserResult(
            new (Context) UnresolvedDotExpr(Result.get(), TokLoc, Name, NameLoc,
                                            /*Implicit=*/false));
        }
        
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

        // If there is an expr-call-suffix, parse it and form a call.
        if (Tok.isFollowingLParen()) {
          Result = parseExprCallSuffix(Result);
          continue;
        }
      } else {
        Result = makeParserResult(
            new (Context) UnresolvedDotExpr(Result.get(), TokLoc, Name, NameLoc,
                                            /*Implicit=*/false));
        consumeToken(tok::integer_literal);
      }

      continue;
    }
    
    // Check for a () suffix, which indicates a call.
    // Note that this cannot be the start of a new line.
    if (Tok.isFollowingLParen()) {
      if (peekToken().is(tok::code_complete)) {
        consumeToken(tok::l_paren);
        if (CodeCompletion && Result.isNonNull())
          CodeCompletion->completePostfixExprParen(Result.get());
        // Eat the code completion token because we handled it.
        consumeToken(tok::code_complete);
        return makeParserCodeCompletionResult<Expr>();
      }
      ParserResult<Expr> Arg = parseExprList(tok::l_paren, tok::r_paren);
      if (Arg.hasCodeCompletion())
        return makeParserCodeCompletionResult<Expr>();

      if (Arg.isParseError())
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
      if (Idx.isNull() || Result.isNull())
        return nullptr;
      Result = makeParserResult(
          new (Context) SubscriptExpr(Result.get(), Idx.get()));
      continue;
    }

    // Check for a trailing closure, if allowed.
    if (!isExprBasic && Tok.is(tok::l_brace) &&
        !isStartOfGetSetAccessor(*this)) {
      SourceLoc braceLoc = Tok.getLoc();
      // Parse the closure.
      ParserResult<Expr> closure = parseExprClosure();
      if (closure.isNull())
        return nullptr;

      // Track the original end location of the expression we're trailing so
      // we can warn about excess newlines.
      auto origEndLoc = Result.get()->getEndLoc();
      auto origLineCol = SourceMgr.getLineAndColumn(origEndLoc);
      auto braceLineCol = SourceMgr.getLineAndColumn(braceLoc);
      if (((int)braceLineCol.first - (int)origLineCol.first) > 1) {
        diagnose(braceLoc, diag::trailing_closure_excess_newlines);
        diagnose(Result.get()->getLoc(), diag::trailing_closure_call_here);
      }

      // Introduce the trailing closure into the call, or form a call, as
      // necessary.
      if (auto call = dyn_cast<CallExpr>(Result.get())) {
        // When a closure follows a call, it becomes the last argument of
        // that call.
        Expr *arg = addTrailingClosureToArgument(Context, call->getArg(),
                                                 closure.get());
        call->setArg(arg);

        if (closure.hasCodeCompletion())
          Result.setHasCodeCompletion();
      } else {
        // Otherwise, the closure implicitly forms a call.
        Expr *arg = createArgWithTrailingClosure(Context, SourceLoc(), { },
                                                 { }, { }, SourceLoc(), 
                                                 closure.get());
        Result = makeParserResult(
            ParserStatus(closure),
            new (Context) CallExpr(Result.get(), arg, /*Implicit=*/true));
      }

      if (Result.hasCodeCompletion())
        return Result;

      // We only allow a single trailing closure on a call.  This could be
      // generalized in the future, but needs further design.
      if (Tok.is(tok::l_brace)) break;
      continue;
    }

    // Check for a ? suffix.
    if (consumeIf(tok::question_postfix)) {
      Result = makeParserResult(
          new (Context) BindOptionalExpr(Result.get(), TokLoc, /*depth*/ 0));
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
  bool First = true;
  for (auto Segment : Segments) {
    switch (Segment.Kind) {
    case Lexer::StringSegment::Literal: {
      auto TokenLoc = First ? Loc : Segment.Loc;
      Exprs.push_back(
          createStringLiteralExprFromSegment(Context, L, Segment, TokenLoc));

      // Since the string is already parsed, Tok already points to the first
      // token after the whole string, but PreviousLoc is not exactly correct.
      PreviousLoc = TokenLoc;
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

        if (!Tok.is(tok::eof)) {
          diagnose(Tok, diag::string_interpolation_extra);
        }
      }
      break;
    }
    }
    First = false;
  }
  
  if (Exprs.empty())
    return new (Context) ErrorExpr(Loc);

  return new (Context) InterpolatedStringLiteralExpr(Loc,
                                        Context.AllocateCopy(Exprs));
}
  
///   expr-identifier:
///     identifier generic-args?
Expr *Parser::parseExprIdentifier() {
  assert(Tok.is(tok::identifier) || Tok.is(tok::kw_self) ||
         Tok.is(tok::kw_Self));

  Token IdentTok = Tok;
  Identifier name;
  SourceLoc loc = consumeIdentifier(&name);
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
    hasGenericArgumentList = true;
    if (parseGenericArguments(args, LAngleLoc, RAngleLoc)) {
      diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);
    }
  }
  
  ValueDecl *D = lookupInScope(name);
  // FIXME: We want this to work: "var x = { x() }", but for now it's better
  // to disallow it than to crash.
  if (D) {
    for (auto activeVar : DisabledVars) {
      if (activeVar == D) {
        diagnose(loc, DisabledVarReason);
        return new (Context) ErrorExpr(loc);
      }
    }
  } else {
    for (auto activeVar : DisabledVars) {
      if (activeVar->getName() == name) {
        diagnose(loc, DisabledVarReason);
        return new (Context) ErrorExpr(loc);
      }
    }
  }
  
  Expr *E;
  if (D == 0) {
    if (name.isEditorPlaceholder())
      return parseExprEditorPlaceholder(IdentTok, name);

    auto refKind = DeclRefKind::Ordinary;
    auto unresolved = new (Context) UnresolvedDeclRefExpr(name, refKind, loc);
    unresolved->setSpecialized(hasGenericArgumentList);
    E = unresolved;
  } else if (auto TD = dyn_cast<TypeDecl>(D)) {
    if (!hasGenericArgumentList)
      E = TypeExpr::createForDecl(loc, TD);
    else
      E = TypeExpr::createForSpecializedDecl(loc, TD,
                                             Context.AllocateCopy(args),
                                             SourceRange(LAngleLoc,
                                                         RAngleLoc));
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

      Lexer::State StartState = L->getStateForBeginningOfTokenLoc(TypeStartLoc);
      Lexer::State EndState = L->getStateForBeginningOfTokenLoc(TypeEndLoc);

      // Create a lexer for the type sub-string.
      Lexer LocalLex(*L, StartState, EndState);

      // Temporarily swap out the parser's current lexer with our new one.
      llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);
      Tok.setKind(tok::unknown); // we might be at tok::eof now.
      consumeToken();
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

bool Parser::
parseClosureSignatureIfPresent(SmallVectorImpl<CaptureListEntry> &captureList,
                               Pattern *&params, SourceLoc &throwsLoc,
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

  // At this point, we know we have a closure signature. Parse the capture list
  // and parameters.
  if (consumeIf(tok::l_square) &&
      !consumeIf(tok::r_square)) {
    do {
      // Check for the strength specifier: "weak", "unowned", or
      // "unowned(safe/unsafe)".
      SourceLoc loc;
      Ownership ownershipKind = Ownership::Strong;
      if (Tok.isContextualKeyword("weak")){
        loc = consumeToken(tok::identifier);
        ownershipKind = Ownership::Weak;
      } else if (Tok.isContextualKeyword("unowned")) {
        loc = consumeToken(tok::identifier);
        ownershipKind = Ownership::Unowned;

        // Skip over "safe" and "unsafe" if present.
        if (consumeIf(tok::l_paren)) {
          if (Tok.getText() == "safe")
            ownershipKind = Ownership::Unowned; // FIXME: No "safe" variant.
          else if (Tok.getText() == "unsafe")
            ownershipKind = Ownership::Unmanaged;
          else
            diagnose(Tok, diag::attr_unowned_invalid_specifier);
          consumeIf(tok::identifier);
          if (!consumeIf(tok::r_paren))
            diagnose(Tok, diag::attr_unowned_expected_rparen);
        }
      } else if (Tok.is(tok::identifier) &&
                 peekToken().isAny(tok::equal, tok::comma, tok::r_square)) {
        // "x = 42", "x," and "x]" are all strong captures of x.
        loc = Tok.getLoc();
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

      // The thing being capture specified is an identifier, or as an identifier
      // followed by an expression.
      Expr *initializer;
      Identifier name;
      SourceLoc nameLoc = Tok.getLoc();
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
        consumeToken(tok::equal);

        auto ExprResult = parseExpr(diag::expected_init_capture_specifier);
        if (ExprResult.isNull())
          continue;
        initializer = ExprResult.get();
      }

      // Create the VarDecl and the PatternBindingDecl for the captured
      // expression.  This uses the parent declcontext (not the closure) since
      // the initializer expression is evaluated before the closure is formed.
      auto *VD = new (Context) VarDecl(/*isStatic*/false,
                                       /*isLet*/ownershipKind !=Ownership::Weak,
                                       nameLoc, name, Type(), CurDeclContext);
      // Attributes.
      if (ownershipKind != Ownership::Strong)
        VD->getAttrs().add(new (Context) OwnershipAttr(ownershipKind));
      
      auto pattern = new (Context) NamedPattern(VD, /*implicit*/true);
      
      auto *PBD = PatternBindingDecl::create(Context, /*staticloc*/SourceLoc(),
                                             StaticSpellingKind::None,
                                             nameLoc, pattern, initializer,
                                             CurDeclContext);
                                                   
      
      
      captureList.push_back(CaptureListEntry(VD, PBD));
    } while (consumeIf(tok::comma));
    
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
      // Parse identifier (',' identifier)*
      SmallVector<TuplePatternElt, 4> elements;
      do {
        if (Tok.is(tok::identifier) || Tok.is(tok::kw__)) {
          Identifier name = Tok.is(tok::identifier) ?
              Context.getIdentifier(Tok.getText()) : Identifier();
          auto var = new (Context) ParamDecl(/*IsLet*/ true,
                                             SourceLoc(), Identifier(),
                                             Tok.getLoc(),
                                             name,
                                             Type(), nullptr);
          elements.push_back(TuplePatternElt(new (Context) NamedPattern(var)));
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

      params = TuplePattern::create(Context, SourceLoc(), elements,SourceLoc());
    }
    
    if (Tok.is(tok::kw_throws)) {
      throwsLoc = consumeToken();
    } else if (Tok.is(tok::kw_rethrows)) {
      throwsLoc = consumeToken();
      diagnose(throwsLoc, diag::rethrowing_function_type);
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

ParserResult<Expr> Parser::parseExprClosure() {
  assert(Tok.is(tok::l_brace) && "Not at a left brace?");

  // Parse the opening left brace.
  SourceLoc leftBrace = consumeToken(tok::l_brace);

  // Parse the closure-signature, if present.
  Pattern *params = nullptr;
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
  auto *closure = new (Context) ClosureExpr(leftBrace,
                                            params, throwsLoc, arrowLoc, inLoc,
                                            explicitResultType,
                                            discriminator, SourceLoc(),
                                            CurDeclContext);
  // The arguments to the func are defined in their own scope.
  Scope S(this, ScopeKind::ClosureParams);
  ParseFunctionBody cc(*this, closure);

  // Handle parameters.
  if (params) {
    // Add the parameters into scope.
    addPatternVariablesToScope(params);
  } else {
    // There are no parameters; allow anonymous closure variables.
    // FIXME: We could do this all the time, and then provide Fix-Its
    // to map $i -> the appropriately-named argument. This might help
    // users who are refactoring code by adding names.
    AnonClosureVars.emplace_back();
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
  closure->setRBraceLoc(rightBrace);

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
    
    if (returnExpr)
      returnExpr->setIsReturnExpr();
  }

  // Set the body of the closure.
  closure->setBody(BraceStmt::create(Context, leftBrace, bodyElements,
                                     rightBrace),
                   hasSingleExpressionBody);

  // If the closure includes a capture list, create an AST node for it as well.
  Expr *result = closure;
  if (!captureList.empty())
    result = new (Context) CaptureListExpr(Context.AllocateCopy(captureList),
                                           closure);

  return makeParserResult(Status, result);
}

///   expr-anon-closure-argument:
///     dollarident
Expr *Parser::parseExprAnonClosureArg() {
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
  if (!closure || closure->getParams()) {
    // FIXME: specialize diagnostic when there were closure parameters.
    // We can be fairly smart here.
    diagnose(Loc, closure ? diag::anon_closure_arg_in_closure_with_args
                          : diag::anon_closure_arg_not_in_closure);
    return new (Context) ErrorExpr(Loc);
  }

  auto &decls = AnonClosureVars.back();
  while (ArgNo >= decls.size()) {
    unsigned nextIdx = decls.size();
    SmallVector<char, 4> StrBuf;
    StringRef varName = ("$" + Twine(nextIdx)).toStringRef(StrBuf);
    Identifier ident = Context.getIdentifier(varName);
    SourceLoc varLoc = Loc;
    VarDecl *var = new (Context) ParamDecl(/*IsLet*/ true,
                                           SourceLoc(), Identifier(),
                                           varLoc, ident, Type(), closure);
    decls.push_back(var);
  }

  return new (Context) DeclRefExpr(AnonClosureVars.back()[ArgNo], Loc,
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
ParserResult<Expr> Parser::parseExprList(tok LeftTok, tok RightTok) {
  StructureMarkerRAII ParsingExprList(*this, Tok);

  SourceLoc LLoc = consumeToken(LeftTok);
  SourceLoc RLoc;

  SmallVector<Expr*, 8> SubExprs;
  SmallVector<Identifier, 8> SubExprNames;
  SmallVector<SourceLoc, 8> SubExprNameLocs;

  ParserStatus Status = parseList(RightTok, LLoc, RLoc,
                                  tok::comma, /*OptionalSep=*/false,
                                  /*AllowSepAfterLast=*/false,
                                  RightTok == tok::r_paren ?
                                      diag::expected_rparen_expr_list :
                                      diag::expected_rsquare_expr_list,
                                  [&] () -> ParserStatus {
    Identifier FieldName;
    SourceLoc FieldNameLoc;

    // Check to see if there is a field specifier
    if (Tok.is(tok::identifier) && peekToken().is(tok::colon)) {
      FieldNameLoc = Tok.getLoc();
      if (parseIdentifier(FieldName,
                          diag::expected_field_spec_name_tuple_expr)) {
        return makeParserError();
      }
      consumeToken(tok::colon);
    }

    // See if we have an operator decl ref '(<op>)'. The operator token in
    // this case lexes as a binary operator because it neither leads nor
    // follows a proper subexpression.
    ParserStatus Status;
    Expr *SubExpr = nullptr;
    if (Tok.isBinaryOperator() && peekToken().isAny(RightTok, tok::comma)) {
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
                                                   Loc);
    } else {
      ParserResult<Expr> ParsedSubExpr 
        = parseExpr(diag::expected_expr_in_expr_list);
      SubExpr = ParsedSubExpr.getPtrOrNull();
      Status = ParsedSubExpr;
    }

    // If we got a subexpression, add it.
    if (SubExpr) {
      // Update names and locations.
      if (!SubExprNames.empty()) {
        SubExprNames.push_back(FieldName);
        SubExprNameLocs.push_back(FieldNameLoc);
      } else if (FieldName.get()) {
        SubExprNames.resize(SubExprs.size());
        SubExprNames.push_back(FieldName);

        SubExprNameLocs.resize(SubExprs.size());
        SubExprNameLocs.push_back(FieldNameLoc);
      }

      // Add the subexpression.
      SubExprs.push_back(SubExpr);
    }

    return Status;
  });

  // A tuple with a single, unlabelled element is just parentheses.
  if (SubExprs.size() == 1 &&
      (SubExprNames.empty() || SubExprNames[0].empty())) {
    return makeParserResult(
        Status, new (Context) ParenExpr(LLoc, SubExprs[0], RLoc,
                                        /*hasTrailingClosure=*/false));
  }

  return makeParserResult(
      Status,
      TupleExpr::create(Context, LLoc, SubExprs, SubExprNames, SubExprNameLocs,
                        RLoc, /*hasTrailingClosure=*/false,
                        /*Implicit=*/false));
}

/// \brief Parse an object literal expression.
///
/// expr-literal:
///   '[#' identifier expr-paren '#]'
ParserResult<Expr>
Parser::parseExprObjectLiteral() {
  SourceLoc LLitLoc = consumeToken(tok::l_square_lit);
  Identifier Name;
  SourceLoc NameLoc;
  if (parseIdentifier(Name, NameLoc,
                      diag::expected_identifier_after_l_square_lit)) {
    return makeParserError();
  }
  // Parse a tuple of args
  if (!Tok.is(tok::l_paren)) {
    diagnose(Tok, diag::expected_arg_list_in_object_literal);
    return makeParserError();
  }
  ParserResult<Expr> Arg;
  Arg = parseExprList(tok::l_paren, tok::r_paren);
  if (Arg.hasCodeCompletion()) {
    return Arg;
  }
  if (Arg.isParseError()) {
    return makeParserError();
  }
  if (!Tok.is(tok::r_square_lit)) {
    diagnose(Tok, diag::expected_r_square_lit_after_object_literal);
    return makeParserError();
  }
  SourceLoc RLitLoc = consumeToken(tok::r_square_lit);
  return makeParserResult(
    new (Context) ObjectLiteralExpr(LLitLoc, Name, NameLoc, Arg.get(), RLitLoc,
                                    /*implicit=*/false));
}

/// \brief Parse an expression call suffix.
///
/// expr-call-suffix:
///   expr-paren selector-arg*
///   expr-closure selector-arg* (except in expr-basic)
///
/// selector-arg:
///   identifier expr-paren
ParserResult<Expr>
Parser::parseExprCallSuffix(ParserResult<Expr> fn,
                            Identifier firstSelectorPiece,
                            SourceLoc firstSelectorPieceLoc) {
  assert(Tok.isFollowingLParen() && "Not a call suffix?");

  // Parse the first argument.

  // If there is a code completion token right after the '(', do a special case
  // callback.
  if (peekToken().is(tok::code_complete) && CodeCompletion) {
    consumeToken(tok::l_paren);
    CodeCompletion->completePostfixExprParen(fn.get());
    // Eat the code completion token because we handled it.
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionResult<Expr>();
  }

  ParserResult<Expr> firstArg = parseExprList(Tok.getKind(), tok::r_paren);
  if (firstArg.hasCodeCompletion())
    return firstArg;
  if (fn.isParseError())
    return fn;
  if (firstArg.isParseError())
    return firstArg;

  // Form the call.
  return makeParserResult(new (Context) CallExpr(fn.get(), firstArg.get(),
                                                 /*Implicit=*/false));
}

/// parseExprCollection - Parse a collection literal expression.
///
///   expr-collection:
///     expr-array
///     expr-dictionary
//      lsquare-starting ']'
ParserResult<Expr> Parser::parseExprCollection() {
  Parser::StructureMarkerRAII ParsingCollection(*this, Tok);
  SourceLoc LSquareLoc = consumeToken(tok::l_square);

  // [] is always an array.
  if (Tok.is(tok::r_square)) {
    SourceLoc RSquareLoc = consumeToken(tok::r_square);
    return makeParserResult(
                    ArrayExpr::create(Context, LSquareLoc, {}, RSquareLoc));
  }

  // [:] is always an empty dictionary.
  if (Tok.is(tok::colon) && peekToken().is(tok::r_square)) {
    consumeToken(tok::colon);
    SourceLoc RSquareLoc = consumeToken(tok::r_square);
    return makeParserResult(
                  DictionaryExpr::create(Context, LSquareLoc, {}, RSquareLoc));
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
///     '[' expr (',' expr)* ','? ']'
///     '[' ']'
ParserResult<Expr> Parser::parseExprArray(SourceLoc LSquareLoc,
                                          Expr *FirstExpr) {
  SmallVector<Expr *, 8> SubExprs;
  SubExprs.push_back(FirstExpr);

  SourceLoc RSquareLoc;
  ParserStatus Status;

  if (Tok.isNot(tok::r_square) && !consumeIf(tok::comma)) {
    diagnose(Tok, diag::expected_separator, ",")
        .fixItInsertAfter(PreviousLoc, ",");
    Status.setIsParseError();
  }

  Status |= parseList(tok::r_square, LSquareLoc, RSquareLoc,
                      tok::comma, /*OptionalSep=*/false,
                      /*AllowSepAfterLast=*/true,
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
  return makeParserResult(Status,
          ArrayExpr::create(Context, LSquareLoc, SubExprs, RSquareLoc));
}

/// parseExprDictionary - Parse a dictionary literal expression.
///
/// The lsquare-starting and first key have already been parsed, and
/// are passed in as parameters.
///
///   expr-dictionary:
///     '[' expr ':' expr (',' expr ':' expr)* ','? ']'
///     '[' ':' ']'
ParserResult<Expr> Parser::parseExprDictionary(SourceLoc LSquareLoc,
                                               Expr *FirstKey) {
  assert(Tok.is(tok::colon));

  // Each subexpression is a (key, value) tuple.
  // FIXME: We're not tracking the colon locations in the AST.
  SmallVector<Expr *, 8> SubExprs;
  SourceLoc RSquareLoc;

  // Function that adds a new key/value pair.
  auto addKeyValuePair = [&](Expr *Key, Expr *Value) -> void {
    Expr *Exprs[] = {Key, Value};
    SubExprs.push_back(TupleExpr::createImplicit(Context, Exprs, { }));
  };

  bool FirstPair = true;

  ParserStatus Status =
      parseList(tok::r_square, LSquareLoc, RSquareLoc, tok::comma,
                /*OptionalSep=*/false, /*AllowSepAfterLast=*/true,
                diag::expected_rsquare_array_expr, [&]() -> ParserStatus {
    // Parse the next key.
    ParserResult<Expr> Key;
    if (FirstPair) {
      Key = makeParserResult(FirstKey);
      FirstPair = false;
    } else {
      Key = parseExpr(diag::expected_key_in_dictionary_literal);
      if (Key.isNull() || Key.hasCodeCompletion())
        return Key;
    }

    // Parse the ':'.
    if (Tok.isNot(tok::colon)) {
      diagnose(Tok, diag::expected_colon_in_dictionary_literal);
      return makeParserError();
    }
    consumeToken();

    // Parse the next value.
    ParserResult<Expr> Value =
        parseExpr(diag::expected_value_in_dictionary_literal);
    if (Value.hasCodeCompletion())
      return Value;

    if (Value.isNull())
      Value = makeParserResult(Value, new (Context) ErrorExpr(PreviousLoc));

    // Add this key/value pair.
    addKeyValuePair(Key.get(), Value.get());
    return Value;
  });

  if (Status.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();

  assert(SubExprs.size() >= 1);
  return makeParserResult(DictionaryExpr::create(Context, LSquareLoc, SubExprs,
                                                 RSquareLoc));
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


/// Parse availability query specification.
///
///  availability-spec:
///     '*'
///     version-constraint-spec
ParserResult<AvailabilitySpec> Parser::parseAvailabilitySpec() {
  if (Tok.isBinaryOperator() && Tok.getText() == "*") {
    SourceLoc StarLoc = Tok.getLoc();
    consumeToken();

    return makeParserResult(new (Context) OtherPlatformAvailabilitySpec(StarLoc));
  }
  return parseVersionConstraintSpec();
}

/// Parse version constraint specification.
///
///  version-constraint-spec:
///     identifier version-comparison version-tuple
ParserResult<VersionConstraintAvailabilitySpec>
Parser::parseVersionConstraintSpec() {
  Identifier PlatformIdentifier;
  SourceLoc PlatformLoc;
  if (parseIdentifier(PlatformIdentifier, PlatformLoc,
                      diag::avail_query_expected_platform_name)) {
    return nullptr;
  }

  if (Tok.isBinaryOperator() && Tok.getText() == ">=") {
    diagnose(Tok, diag::avail_query_version_comparison_not_needed)
        .fixItRemove(Tok.getLoc());
    consumeToken();
  }

  clang::VersionTuple Version;
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

  return makeParserResult(new (Context) VersionConstraintAvailabilitySpec(
      Platform.getValue(), PlatformLoc, Version, VersionRange));
}
