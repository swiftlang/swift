//===--- ParseExpr.cpp - Swift Language Parser for Expressions ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
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
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

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
  SmallVector<Expr*, 8> SequencedExprs;
  SourceLoc startLoc = Tok.getLoc();
  bool HasCodeCompletion = false;

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
        if (SequencedExprs.size() > 0 && (SequencedExprs.size() & 1) == 0) {
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
  
  if (SequencedExprs.empty()) {
    if (isForConditionalDirective) {
      diagnose(startLoc, diag::expected_close_to_if_directive);
      return makeParserError();
    } else {
      // If we had semantic errors, just fail here.
      assert(!SequencedExprs.empty());
    }
  }

  // If we saw no operators, don't build a sequence.
  if (SequencedExprs.size() == 1) {
    auto Result = makeParserResult(SequencedExprs[0]);
    if (HasCodeCompletion)
      Result.setHasCodeCompletion();
    return Result;
  }

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

  case tok::pound_keyPath:
    return parseExprKeyPath();

  case tok::oper_postfix:
    // Postfix operators cannot start a subexpression, but can happen
    // syntactically because the operator may just follow whatever precedes this
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

  // Check if we have a unary '-' with number literal sub-expression, for
  // example, "-42" or "-1.25".
  if (auto *LE = dyn_cast<NumberLiteralExpr>(SubExpr.get())) {
    if (Operator->hasName() && Operator->getName().getBaseName().str() == "-") {
      LE->setNegative(Operator->getLoc());
      return makeParserResult(LE);
    }
  }

  return makeParserResult(
      new (Context) PrefixUnaryExpr(Operator, SubExpr.get()));
}

///   expr-keypath:
///     '#keyPath' '(' unqualified-name ('.' unqualified-name) * ')'
///
ParserResult<Expr> Parser::parseExprKeyPath() {
  // Consume '#keyPath'.
  SourceLoc keywordLoc = consumeToken(tok::pound_keyPath);

  // Parse the leading '('.
  if (!Tok.is(tok::l_paren)) {
    diagnose(Tok, diag::expr_keypath_expected_lparen);
    return makeParserError();
  }
  SourceLoc lParenLoc = consumeToken(tok::l_paren);

  // Handle code completion.
  SmallVector<Identifier, 4> names;
  SmallVector<SourceLoc, 4> nameLocs;
  auto handleCodeCompletion = [&](bool hasDot) -> ParserResult<Expr> {
    ObjCKeyPathExpr *expr = nullptr;
    if (!names.empty()) {
      expr = ObjCKeyPathExpr::create(Context, keywordLoc, lParenLoc, names,
                                     nameLocs, Tok.getLoc());
    }

    if (CodeCompletion)
      CodeCompletion->completeExprKeyPath(expr, hasDot);

    // Eat the code completion token because we handled it.
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionResult(expr);
  };

  // Parse the sequence of unqualified-names.
  ParserStatus status;
  while (true) {
    // Handle code completion.
    if (Tok.is(tok::code_complete))
      return handleCodeCompletion(!names.empty());

    // Parse the next name.
    DeclNameLoc nameLoc;
    bool afterDot = !names.empty();
    auto name = parseUnqualifiedDeclName(
                  afterDot, nameLoc, 
                  diag::expr_keypath_expected_property_or_type);
    if (!name) {
      status.setIsParseError();
      break;
    }

    // Cannot use compound names here.
    if (name.isCompoundName()) {
      diagnose(nameLoc.getBaseNameLoc(), diag::expr_keypath_compound_name,
               name)
        .fixItReplace(nameLoc.getSourceRange(), name.getBaseName().str());
    }

    // Record the name we parsed.
    names.push_back(name.getBaseName());
    nameLocs.push_back(nameLoc.getBaseNameLoc());

    // Handle code completion.
    if (Tok.is(tok::code_complete))
      return handleCodeCompletion(false);

    // Parse the next period to continue the path.
    if (consumeIf(tok::period))
      continue;

    break;
  }

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
  if (names.empty() || status.isError()) {
    return makeParserResult<Expr>(
             new (Context) ErrorExpr(SourceRange(keywordLoc, rParenLoc)));
  }

  // We're done: create the key-path expression.
  return makeParserResult<Expr>(
           ObjCKeyPathExpr::create(Context, keywordLoc, lParenLoc, names,
                                   nameLocs, rParenLoc));
}

/// parseExprSelector
///
///   expr-selector:
///     '#selector' '(' expr ')'
///     '#selector' '(' 'getter' ':' expr ')'
///     '#selector' '(' 'setter' ':' expr ')'
///
ParserResult<Expr> Parser::parseExprSelector() {
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

    modifierLoc = consumeToken(tok::identifier);
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

    return makeParserResult(
             new (Context) UnresolvedDotExpr(superRef, dotLoc, name, nameLoc,
                                             /*Implicit=*/false));
  }

  // NOTE: l_square_lit is for migrating the old object literal syntax.
  // Eventually this block can be removed.
  if (Tok.is(tok::l_square_lit) && !Tok.isAtStartOfLine() &&
      isCollectionLiteralStartingWithLSquareLit()) {
    assert(Tok.getLength() == 1);
    Tok.setKind(tok::l_square);
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
                                        trailingClosure);
    if (status.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (status.isError())
      return nullptr;
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

/// See if type(of: <expr>) can be parsed backtracking on failure.
static bool canParseTypeOf(Parser &P) {
  // We parsed `type(of:)` as a special syntactic form in Swift 3. In Swift 4
  // it is handled by overload resolution.
  if (!P.Context.LangOpts.isSwiftVersion3())
    return false;

  if (!(P.Tok.getText() == "type" && P.peekToken().is(tok::l_paren))) {
    return false;
  }
  // Look ahead to parse the parenthesized expression.
  Parser::BacktrackingScope Backtrack(P);
  P.consumeToken(tok::identifier);
  P.consumeToken(tok::l_paren);
  // The first argument label must be 'of'.
  if (!(P.Tok.getText() == "of" && P.peekToken().is(tok::colon))) {
    return false;
  }

  // Parse to the closing paren.
  while (!P.Tok.is(tok::r_paren) && !P.Tok.is(tok::eof)) {
    // Anything that looks like another argument label is bogus.  It is
    // sufficient to parse for a single trailing comma.  Backtracking will
    // fall back to an unresolved decl.
    if (P.Tok.is(tok::comma)) {
      return false;
    }
    P.skipSingle();
  }
  return true;
}

/// parseExprPostfix
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
///   expr-delayed-identifier:
///     '.' identifier
///
///   expr-discard:
///     '_'
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
    Result = parseExprStringLiteral();
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
    SWIFT_FALLTHROUGH;
  }
  case tok::pound_column:
  case tok::pound_file:
  case tok::pound_function:
  case tok::pound_line:
  case tok::pound_dsohandle: {
    auto Kind = getMagicIdentifierLiteralKind(Tok.getKind());
    SourceLoc Loc = consumeToken();
    Result = makeParserResult(
       new (Context) MagicIdentifierLiteralExpr(Kind, Loc, /*implicit=*/false));
    break;
  }
      
  case tok::identifier:  // foo
    // Attempt to parse for 'type(of: <expr>)'.
    if (canParseTypeOf(*this)) {
      Result = parseExprTypeOf();
      break;
    }

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
      Result = parseExprCallSuffix(Result, isExprBasic);
      break;
    }

    break;

  case tok::kw_Any: { // Any
    ParserResult<TypeRepr> repr = parseAnyType();
    auto expr = new (Context) TypeExpr(TypeLoc(repr.get()));
    Result = makeParserResult(expr);
    break;
  }

  case tok::dollarident: // $1
    Result = makeParserResult(parseExprAnonClosureArg());
    break;

    // If the next token is '_', parse a discard expression.
  case tok::kw__:
    Result = makeParserResult(
      new (Context) DiscardAssignmentExpr(consumeToken(), /*Implicit=*/false));
    break;

  case tok::pound_selector: // expr-selector
    Result = parseExprSelector();
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
    
    DeclName Name;
    DeclNameLoc NameLoc;

    if (Tok.is(tok::code_complete)) {
      auto Expr = UnresolvedMemberExpr::create(
                    Context, DotLoc, DeclNameLoc(DotLoc.getAdvancedLoc(1)),
                    Context.getIdentifier("_"), /*implicit=*/false);
      Result = makeParserResult(Expr);
      if (CodeCompletion) {
        std::vector<StringRef> Identifiers;

        // Move lexer to the start of the current line.
        L->backtrackToState(L->getStateForBeginningOfTokenLoc(
          L->getLocForStartOfLine(SourceMgr, Tok.getLoc())));

        bool HasReturn = false;

        // Until we see the code completion token, collect identifiers.
        for (L->lex(Tok); !Tok.is(tok::code_complete); consumeToken()) {
          if (!HasReturn)
            HasReturn = Tok.is(tok::kw_return);
          if (Tok.is(tok::identifier)) {
            Identifiers.push_back(Tok.getText());
          }
        }
        CodeCompletion->completeUnresolvedMember(Expr, Identifiers, HasReturn);
      } else {
        Result.setHasCodeCompletion();
      }
      consumeToken();
      return Result;
    }

    Name = parseUnqualifiedDeclName(/*afterDot=*/true, NameLoc,
                                    diag::expected_identifier_after_dot_expr);
    if (!Name) return nullptr;

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
                                          trailingClosure);
      if (status.isError())
        return nullptr;

      Result = makeParserResult(
                 status,
                 UnresolvedMemberExpr::create(Context, DotLoc, NameLoc, Name,
                                              lParenLoc, args, argLabels,
                                              argLabelLocs, rParenLoc,
                                              trailingClosure,
                                              /*implicit=*/false));
      if (Result.hasCodeCompletion())
        return Result;
      break;
    }

    // Check for a trailing closure, if allowed.
    if (Tok.is(tok::l_brace) && isValidTrailingClosure(isExprBasic, *this)) {
      ParserResult<Expr> closure =
        parseTrailingClosure(NameLoc.getSourceRange());
      if (closure.isNull()) return nullptr;

      // Handle .foo by just making an AST node.
      Result = makeParserResult(
                 ParserStatus(closure),
                 UnresolvedMemberExpr::create(Context, DotLoc, NameLoc, Name,
                                              SourceLoc(), { }, { }, { },
                                              SourceLoc(), closure.get(),
                                              /*implicit=*/false));

      if (Result.hasCodeCompletion())
        return Result;

      break;
    }

    // Handle .foo by just making an AST node.
    Result = makeParserResult(
               UnresolvedMemberExpr::create(Context, DotLoc, NameLoc, Name,
                                            /*implicit=*/false));
    break;
  }
      
  case tok::kw_super: {      // super.foo or super[foo]
    Result = parseExprSuper(isExprBasic);
    break;
  }

  case tok::l_paren:
    Result = parseExprList(tok::l_paren, tok::r_paren);
    break;

  case tok::l_square:
    Result = parseExprCollection();
    break;

  case tok::pound_available: {
    // For better error recovery, parse but reject #available in an expr
    // context.
    diagnose(Tok.getLoc(), diag::availability_query_outside_if_stmt_guard);
    auto res = parseStmtConditionPoundAvailable();
    if (res.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
    if (res.isParseError() || res.isNull())
      return nullptr;
    Result = makeParserResult(
                  new (Context) ErrorExpr(res.get()->getSourceRange()));
    break;
  }

  // NOTE: This is for migrating the old object literal syntax.
  // Eventually this block can be removed.
  case tok::l_square_lit: {// [#Color(...)#], [#Image(...)#]
    // If this is actually a collection literal starting with '[#', handle it
    // as such.
    if (isCollectionLiteralStartingWithLSquareLit()) {
      // Split the token into two.
      SourceLoc LSquareLoc = consumeStartingCharacterOfCurrentToken();
      // Consume the '[' token.
      Result = parseExprCollection(LSquareLoc);
      break;
    }     

    auto LSquareLoc = Tok.getLoc();
    auto LSquareTokRange = Tok.getRange();
    (void)consumeToken(tok::l_square_lit);
    
    if (Tok.is(tok::pound)) {
      consumeToken();
      if (!Tok.is(tok::identifier))
        diagnose(LSquareLoc, diag::expected_object_literal_identifier);
      skipUntil(tok::r_square_lit);
      Result = makeParserError();
    }
    else {
      Result = parseExprPostfix(ID, isExprBasic);
    }

    // This should be an invariant based on the check in
    // isCollectionLiteralStartingWithLSquareLit().
    auto RSquareTokRange = Tok.getRange();
    (void)consumeToken(tok::r_square_lit);

    // Issue a diagnostic for the legacy syntax and provide a fixit
    // to strip away the '[#' and '#]'
    diagnose(LSquareLoc, diag::legacy_object_literal_syntax)
      .fixItRemoveChars(LSquareTokRange.getStart(), LSquareTokRange.getEnd())
      .fixItRemoveChars(RSquareTokRange.getStart(), RSquareTokRange.getEnd());
    
    break;
  }

#define POUND_OBJECT_LITERAL(Name, Desc, Proto) case tok::pound_##Name:  \
  Result = parseExprObjectLiteral(ObjectLiteralExpr::Name, isExprBasic); \
  break;
#include "swift/Parse/Tokens.def"

#define POUND_OLD_OBJECT_LITERAL(Name, NewName, NewArg, OldArg)\
  case tok::pound_##Name:                                               \
    Result = parseExprObjectLiteral(ObjectLiteralExpr::NewName, isExprBasic, \
    "#" #NewName);                                                      \
  break;
#include "swift/Parse/Tokens.def"

  case tok::code_complete:
    Result = makeParserResult(new (Context) CodeCompletionExpr(Tok.getLoc()));
    Result.setHasCodeCompletion();
    if (CodeCompletion &&
        // We cannot code complete anything after var/let.
        (!InVarOrLetPattern || InVarOrLetPattern == IVOLP_InMatchingPattern))
      CodeCompletion->completePostfixExprBeginning(dyn_cast<CodeCompletionExpr>(
        Result.get()));
    consumeToken(tok::code_complete);
    break;

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
    if (consumeIf(tok::period) || consumeIf(tok::period_prefix)) {

      // Handle "x.42" - a tuple index.
      if (Tok.is(tok::integer_literal)) {
        DeclName name = Context.getIdentifier(Tok.getText());
        SourceLoc nameLoc = consumeToken(tok::integer_literal);
        
        // Don't allow '.<integer literal>' following a numeric literal
        // expression (unless in #if env, for 1.2.3.4 version numbers)
        if (!InPoundIfEnvironment &&
            Result.isNonNull() && isa<NumberLiteralExpr>(Result.get())) {
          diagnose(nameLoc, diag::numeric_literal_numeric_member)
            .highlight(Result.get()->getSourceRange());
          continue;
        }
        
        Result = makeParserResult(
            new (Context) UnresolvedDotExpr(Result.get(), TokLoc, name,
                                            DeclNameLoc(nameLoc),
                                            /*Implicit=*/false));
        continue;
      }

      // Handle "x.self" expr.
      if (Tok.is(tok::kw_self)) {
        Result = makeParserResult(
          new (Context) DotSelfExpr(Result.get(), TokLoc, consumeToken()));
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
           
      // If we have '.<keyword><code_complete>', try to recover by creating
      // an identifier with the same spelling as the keyword.
      if (Tok.isKeyword() && peekToken().is(tok::code_complete)) {
        Identifier Name = Context.getIdentifier(Tok.getText());
        Result = makeParserResult(
            new (Context) UnresolvedDotExpr(Result.get(), TokLoc,
                                            Name, DeclNameLoc(Tok.getLoc()),
                                            /*Implicit=*/false));
        consumeToken();
        // Fall into the next code completion handler.
      }

      // Handle "x.<tab>" for code completion.
      if (Tok.is(tok::code_complete)) {
        if (CodeCompletion && Result.isNonNull())
          CodeCompletion->completeDotExpr(Result.get(), /*DotLoc=*/TokLoc);
        // Eat the code completion token because we handled it.
        consumeToken(tok::code_complete);
        Result.setHasCodeCompletion();
        return Result;
      }

      DeclNameLoc NameLoc;
      DeclName Name = parseUnqualifiedDeclName(/*afterDot=*/true,
                                               NameLoc,
                                               diag::expected_member_name);
      if (!Name) return nullptr;
    
      Result = makeParserResult(
                 new (Context) UnresolvedDotExpr(Result.get(), TokLoc, Name,
                                                 NameLoc,
                                                 /*Implicit=*/false));
        
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

      continue;
    }

    // If there is an expr-call-suffix, parse it and form a call.
    if (Tok.isFollowingLParen()) {
      Result = parseExprCallSuffix(Result, isExprBasic);
      continue;
    }
    
    // NOTE: l_square_lit is for migrating the old object literal syntax.
    // Eventually this block can be removed.
    if (Tok.is(tok::l_square_lit) && !Tok.isAtStartOfLine() &&
        isCollectionLiteralStartingWithLSquareLit()) {
      assert(Tok.getLength() == 1);
      Tok.setKind(tok::l_square);
    }

    // Check for a [expr] suffix.
    // Note that this cannot be the start of a new line.
    if (Tok.isFollowingLSquare()) {
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
                                          trailingClosure);
      if (status.hasCodeCompletion())
        return makeParserCodeCompletionResult<Expr>();
      if (status.isError() || Result.isNull())
        return nullptr;
      Result = makeParserResult(
        SubscriptExpr::create(Context, Result.get(), lSquareLoc, indexArgs,
                              indexArgLabels, indexArgLabelLocs, rSquareLoc,
                              trailingClosure, ConcreteDeclRef(),
                              /*implicit=*/false));
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

      ParserResult<Expr> closure =
        parseTrailingClosure(callee->getSourceRange());
      if (closure.isNull()) return nullptr;

      // Trailing closure implicitly forms a call.
      Result = makeParserResult(
                 ParserStatus(closure),
                 CallExpr::create(Context, Result.get(), SourceLoc(),
                                  { }, { }, { }, SourceLoc(),
                                  closure.get(), /*implicit=*/false));

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
                                new (Context) ErrorExpr(Result.get()->getSourceRange()));
      continue;
    }
    
    // Otherwise, we don't know what this token is, it must end the expression.
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
ParserResult<Expr> Parser::parseExprStringLiteral() {
  SmallVector<Lexer::StringSegment, 1> Segments;
  L->getStringLiteralSegments(Tok, Segments);
  SourceLoc Loc = consumeToken();
    
  // The simple case: just a single literal segment.
  if (Segments.size() == 1 &&
      Segments.front().Kind == Lexer::StringSegment::Literal) {
    return makeParserResult(
        createStringLiteralExprFromSegment(Context, L, Segments.front(), Loc));
  }

  ParserStatus Status;
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
      Status |= E;
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

  if (Exprs.empty()) {
    Status.setIsParseError();
    return makeParserResult(Status, new (Context) ErrorExpr(Loc));
  }

  return makeParserResult(Status, new (Context) InterpolatedStringLiteralExpr(
                                      Loc, Context.AllocateCopy(Exprs)));
}

void Parser::diagnoseEscapedArgumentLabel(const Token &tok) {
  assert(tok.isEscapedIdentifier() && "Only for escaped identifiers");
  if (!canBeArgumentLabel(tok.getText())) return;

  SourceLoc start = tok.getLoc();
  SourceLoc end = start.getAdvancedLoc(tok.getLength());
  diagnose(tok, diag::escaped_parameter_name, tok.getText())
    .fixItRemoveChars(start, start.getAdvancedLoc(1))
    .fixItRemoveChars(end.getAdvancedLoc(-1), end);
}

DeclName Parser::parseUnqualifiedDeclName(bool afterDot,
                                          DeclNameLoc &loc,
                                          const Diagnostic &diag) {
  // Consume the base name.
  Identifier baseName = Context.getIdentifier(Tok.getText());
  SourceLoc baseNameLoc;
  if (Tok.isAny(tok::identifier, tok::kw_Self, tok::kw_self)) {
    baseNameLoc = consumeIdentifier(&baseName);
  } else if (afterDot && Tok.isKeyword()) {
    baseNameLoc = consumeToken();
  } else {
    checkForInputIncomplete();
    diagnose(Tok, diag);
    return DeclName();
  }

  // If the next token isn't a following '(', we don't have a compound name.
  if (!Tok.isFollowingLParen()) {
    loc = DeclNameLoc(baseNameLoc);
    return baseName;
  }

  // If the token after that isn't an argument label or ':', we don't have a
  // compound name.
  if ((!peekToken().canBeArgumentLabel() && !peekToken().is(tok::colon)) ||
      Identifier::isEditorPlaceholder(peekToken().getText())) {
    loc = DeclNameLoc(baseNameLoc);
    return baseName;
  }

  // Try to parse a compound name.
  BacktrackingScope backtrack(*this);

  SmallVector<Identifier, 2> argumentLabels;
  SmallVector<SourceLoc, 2> argumentLabelLocs;
  SourceLoc lparenLoc = consumeToken(tok::l_paren);
  SourceLoc rparenLoc;
  while (true) {
    // Terminate at ')'.
    if (Tok.is(tok::r_paren)) {
      rparenLoc = consumeToken(tok::r_paren);
      break;
    }

    // If we see a ':', the user forgot the '_';
    if (Tok.is(tok::colon)) {
      diagnose(Tok, diag::empty_arg_label_underscore)
        .fixItInsert(Tok.getLoc(), "_");
      argumentLabels.push_back(Identifier());
      argumentLabelLocs.push_back(consumeToken(tok::colon));
    }

    // If we see a potential argument label followed by a ':', consume
    // it.
    if (Tok.canBeArgumentLabel() && peekToken().is(tok::colon)) {
      // If this was an escaped identifier that need not have been escaped,
      // say so.
      if (Tok.isEscapedIdentifier())
        diagnoseEscapedArgumentLabel(Tok);

      if (Tok.is(tok::kw__))
        argumentLabels.push_back(Identifier());
      else
        argumentLabels.push_back(Context.getIdentifier(Tok.getText()));
      argumentLabelLocs.push_back(consumeToken());
      (void)consumeToken(tok::colon);
      continue;
    }

    // This is not a compound name.
    // FIXME: Could recover better if we "know" it's a compound name.
    loc = DeclNameLoc(baseNameLoc);
    return baseName;
  }

  assert(!argumentLabels.empty() && "Logic above should prevent this");
  assert(argumentLabels.size() == argumentLabelLocs.size());

  // We have a compound name. Cancel backtracking and build that name.
  backtrack.cancelBacktrack();
  loc = DeclNameLoc(Context, baseNameLoc, lparenLoc, argumentLabelLocs,
                    rparenLoc);
  return DeclName(Context, baseName, argumentLabels);
}

static bool shouldAddSelfFixit(DeclContext* Current, DeclName Name,
                               DescriptiveDeclKind &Kind) {
  if (Current->isTypeContext() || !Current->getInnermostTypeContext())
    return false;
  if (auto *Nominal = Current->getInnermostTypeContext()->
      getAsNominalTypeOrNominalTypeExtensionContext()){
    // FIXME: we cannot resolve members appear later in the body of the nominal.
    auto LookupResults = Nominal->lookupDirect(Name);
    if (!LookupResults.empty()) {
      Kind = LookupResults.front()->getDescriptiveKind();
      return true;
    }
  }
  return false;
}

///   expr-identifier:
///     unqualified-decl-name generic-args?
Expr *Parser::parseExprIdentifier() {
  assert(Tok.isAny(tok::identifier, tok::kw_self, tok::kw_Self));
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
    if (parseGenericArguments(args, LAngleLoc, RAngleLoc)) {
      diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);
    }
    
    // The result can be empty in error cases.
    hasGenericArgumentList = !args.empty();
  }
  
  ValueDecl *D = lookupInScope(name);
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
        DescriptiveDeclKind Kind;
        if (DisabledVarReason.ID == diag::var_init_self_referential.ID &&
            shouldAddSelfFixit(CurDeclContext, name, Kind)) {
          diagnose(loc.getBaseNameLoc(), diag::expected_self_before_reference,
                   Kind).fixItInsert(loc.getBaseNameLoc(), "self.");
        } else {
          diagnose(loc.getBaseNameLoc(), DisabledVarReason);
        }
        return new (Context) ErrorExpr(loc.getSourceRange());
      }
    }
  }
  
  Expr *E;
  if (D == nullptr) {
    if (name.getBaseName().isEditorPlaceholder())
      return parseExprEditorPlaceholder(IdentTok, name.getBaseName());

    auto refKind = DeclRefKind::Ordinary;
    auto unresolved = new (Context) UnresolvedDeclRefExpr(name, refKind, loc);
    unresolved->setSpecialized(hasGenericArgumentList);
    E = unresolved;
  } else if (auto TD = dyn_cast<TypeDecl>(D)) {
    if (!hasGenericArgumentList)
      E = TypeExpr::createForDecl(loc.getBaseNameLoc(), TD, /*implicit*/false);
    else
      E = TypeExpr::createForSpecializedDecl(loc.getBaseNameLoc(), TD,
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
                                       /*isCaptureList*/true,
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
      SmallVector<ParamDecl*, 4> elements;
      do {
        if (Tok.isNot(tok::identifier, tok::kw__)) {
          diagnose(Tok, diag::expected_closure_parameter_name);
          invalid = true;
          break;
        }

        Identifier name = Tok.is(tok::identifier) ?
            Context.getIdentifier(Tok.getText()) : Identifier();
        auto var = new (Context) ParamDecl(/*IsLet*/ true, SourceLoc(),
                                           SourceLoc(), Identifier(),
                                           Tok.getLoc(), name, Type(), nullptr);
        elements.push_back(var);
        consumeToken();
 
        // Consume a comma to continue.
      } while (consumeIf(tok::comma));

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
    auto *var = new (Context) ParamDecl(/*IsLet*/ true, SourceLoc(),SourceLoc(),
                                        Identifier(), varLoc, ident, Type(),
                                        closure);
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
ParserResult<Expr> Parser::parseExprList(tok leftTok, tok rightTok) {
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
                                      trailingClosure);

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
                                   Expr *&trailingClosure) {
  trailingClosure = nullptr;

  StructureMarkerRAII ParsingExprList(*this, Tok);

  leftLoc = consumeToken(leftTok);
  ParserStatus status = parseList(rightTok, leftLoc, rightLoc,
                                  /*AllowSepAfterLast=*/false,
                                  rightTok == tok::r_paren
                                    ? diag::expected_rparen_expr_list
                                    : diag::expected_rsquare_expr_list,
                                  [&] () -> ParserStatus {
    Identifier FieldName;
    SourceLoc FieldNameLoc;

    // Check to see if there is an argument label.
    if (Tok.canBeArgumentLabel() && peekToken().is(tok::colon)) {
      // If this was an escaped identifier that need not have been escaped,
      // say so.
      if (Tok.isEscapedIdentifier())
        diagnoseEscapedArgumentLabel(Tok);

      if (!Tok.is(tok::kw__))
        FieldName = Context.getIdentifier(Tok.getText());
      FieldNameLoc = consumeToken();
      consumeToken(tok::colon);
    }

    // See if we have an operator decl ref '(<op>)'. The operator token in
    // this case lexes as a binary operator because it neither leads nor
    // follows a proper subexpression.
    ParserStatus Status;
    Expr *SubExpr = nullptr;
    if (Tok.isBinaryOperator() && peekToken().isAny(rightTok, tok::comma)) {
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
    } else {
      ParserResult<Expr> ParsedSubExpr 
        = parseExpr(diag::expected_expr_in_expr_list);
      SubExpr = ParsedSubExpr.getPtrOrNull();
      Status = ParsedSubExpr;
    }

    // If we got a subexpression, add it.
    if (SubExpr) {
      // Update names and locations.
      if (!exprLabels.empty()) {
        exprLabels.push_back(FieldName);
        exprLabelLocs.push_back(FieldNameLoc);
      } else if (FieldName.get()) {
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

  // Parse the closure.
  ParserResult<Expr> closure = parseExprClosure();
  if (closure.isNull())
    return makeParserError();

  // Warn if the trailing closure is separated from its callee by more than
  // one line. A single-line separation is acceptable for a trailing closure
  // call, and will be diagnosed later only if the call fails to typecheck.
  auto origLine = SourceMgr.getLineNumber(calleeRange.End);
  auto braceLine = SourceMgr.getLineNumber(braceLoc);
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

// NOTE: this is to detect the old object literal syntax.
// This will be removed in the future.
bool Parser::isCollectionLiteralStartingWithLSquareLit() {
   BacktrackingScope backtracking(*this);
   (void)consumeToken(tok::l_square_lit);
   switch (Tok.getKind()) {
     // Handle both degenerate '#' and '# identifier'.
     case tok::pound:
      (void) consumeToken();
      if (Tok.is(tok::identifier)) skipSingle();
      break;
#define POUND_OBJECT_LITERAL(kw, desc, proto)\
     case tok::pound_##kw: (void)consumeToken(); break;
#define POUND_OLD_OBJECT_LITERAL(kw, new_kw, old_arg, new_arg)\
     case tok::pound_##kw: (void)consumeToken(); break;
#include "swift/Parse/Tokens.def"
     default:
       return true;
   }

   // Skip over a parenthesized argument, if present.
   if (Tok.is(tok::l_paren)) skipSingle();
 
   return Tok.isNot(tok::r_square_lit);
 }
 
/// \brief Parse an object literal expression.
///
/// expr-literal:
///   '#' identifier expr-paren
ParserResult<Expr>
Parser::parseExprObjectLiteral(ObjectLiteralExpr::LiteralKind LitKind,
                               bool isExprBasic,
                               StringRef NewName) {
  auto PoundTok = Tok;
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
                                      trailingClosure);
  if (status.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
  if (status.isError())
    return makeParserError();

  // If the legacy name was used (e.g., #Image instead of #imageLiteral)
  // prompt an error and a fixit.
  if (!NewName.empty()) {
    auto diag =
      diagnose(PoundTok, diag::object_literal_legacy_name, 
               PoundTok.getText(), NewName);
    auto Range = PoundTok.getRange();
    
    // Create a FixIt for the keyword.
    diag.fixItReplaceChars(Range.getStart(), Range.getEnd(), NewName);

    // Try and construct a FixIt for the argument label.
    if (argLabelLocs.size() > 0 && !argLabels[0].empty()) {
      auto ArgLoc = argLabelLocs[0];
      auto FirstElementName = argLabels[0];
            
      if (ArgLoc.isValid() && !FirstElementName.empty()) { 
        auto OldArg = FirstElementName.str();
        auto NewArg =
          llvm::StringSwitch<StringRef>(OldArg)
#define POUND_OLD_OBJECT_LITERAL(kw, new_kw, old_arg, new_arg)\
            .Case(#old_arg, #new_arg)
#include "swift/Parse/Tokens.def"
            .Default("");
       
        if (!NewArg.empty()) {    
          auto Loc = argLabelLocs[0];
          diag.fixItReplaceChars(Loc,
                                 Loc.getAdvancedLocOrInvalid(OldArg.size()),
                                 NewArg);
        }
      }
    }
    
    return makeParserError();
  }

  return makeParserResult(
    ObjectLiteralExpr::create(Context, PoundLoc, LitKind, lParenLoc, args,
                              argLabels, argLabelLocs, rParenLoc,
                              trailingClosure, /*implicit=*/false));
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
    auto Result = makeParserResult(
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
                                      trailingClosure);

  // Form the call.
  auto Result = makeParserResult(status | fn, 
                                 CallExpr::create(Context, fn.get(), lParenLoc,
                                                  args, argLabels, argLabelLocs,
                                                  rParenLoc, trailingClosure,
                                                  /*implicit=*/false));

  if (status.hasCodeCompletion()) {
    if (CodeCompletion) {
      CodeCompletion->completeCallArg(Result.get());
    }
    Result.setHasCodeCompletion();
  }

  return Result;
}

/// parseExprCollection - Parse a collection literal expression.
///
///   expr-collection:
///     expr-array
///     expr-dictionary
//      lsquare-starting ']'
ParserResult<Expr> Parser::parseExprCollection(SourceLoc LSquareLoc) {
  // If the caller didn't already consume the '[', do so now.
  if (LSquareLoc.isInvalid())
    LSquareLoc = consumeToken(tok::l_square);

  Parser::StructureMarkerRAII ParsingCollection(
                                *this, LSquareLoc,
                                StructureMarkerKind::OpenSquare);

  // [] is always an array.
  if (Tok.is(tok::r_square)) {
    SourceLoc RSquareLoc = consumeToken(tok::r_square);
    return makeParserResult(
                    ArrayExpr::create(Context, LSquareLoc, {}, {}, RSquareLoc));
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
  SmallVector<SourceLoc, 8> CommaLocs;
  SubExprs.push_back(FirstExpr);

  SourceLoc CommaLoc, RSquareLoc;
  ParserStatus Status;

  if (Tok.isNot(tok::r_square) && !consumeIf(tok::comma, CommaLoc)) {
    diagnose(Tok, diag::expected_separator, ",")
        .fixItInsertAfter(PreviousLoc, ",");
    Status.setIsParseError();
  }

  CommaLocs.push_back(CommaLoc);

  Status |= parseList(tok::r_square, LSquareLoc, RSquareLoc,
                      /*AllowSepAfterLast=*/true,
                      diag::expected_rsquare_array_expr,
                      [&] () -> ParserStatus
  {
    ParserResult<Expr> Element
      = parseExpr(diag::expected_expr_in_collection_literal);
    if (Element.isNonNull())
      SubExprs.push_back(Element.get());

    if (Tok.is(tok::comma))
      CommaLocs.push_back(Tok.getLoc());

    return Element;
  });

  if (Status.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();

  assert(SubExprs.size() >= 1);
  return makeParserResult(Status,
          ArrayExpr::create(Context, LSquareLoc, SubExprs, CommaLocs,
                            RSquareLoc));
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
      parseList(tok::r_square, LSquareLoc, RSquareLoc,
                /*AllowSepAfterLast=*/true,
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
  SourceLoc SwiftLoc;
  clang::VersionTuple Version;
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

  return makeParserResult(new (Context) PlatformVersionConstraintAvailabilitySpec(
      Platform.getValue(), PlatformLoc, Version, VersionRange));
}

/// parseExprTypeOf
///
///   expr-dynamictype:
///     'type' '(' 'of:' expr ')'
///
ParserResult<Expr> Parser::parseExprTypeOf() {
  // Consume 'type'
  SourceLoc keywordLoc = consumeToken();

  // Parse the leading '('.
  SourceLoc lParenLoc = consumeToken(tok::l_paren);

  // Parse `of` label.
  if (Tok.getText() == "of" && peekToken().is(tok::colon)) {
    // Consume the label.
    consumeToken();
    consumeToken(tok::colon);
  } else {
    // There cannot be a richer diagnostic here because the user may have
    // defined a function `type(...)` that conflicts with the magic expr.
    diagnose(Tok, diag::expr_typeof_expected_label_of);
  }

  // Parse the subexpression.
  ParserResult<Expr> subExpr = parseExpr(diag::expr_typeof_expected_expr);
  if (subExpr.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();

  // Parse the closing ')'
  SourceLoc rParenLoc;
  if (subExpr.isParseError()) {
    skipUntilDeclStmtRBrace(tok::r_paren);
    if (Tok.is(tok::r_paren))
      rParenLoc = consumeToken();
    else
      rParenLoc = PreviousLoc;
  } else {
    parseMatchingToken(tok::r_paren, rParenLoc,
                       diag::expr_typeof_expected_rparen, lParenLoc);
  }

  // If the subexpression was in error, just propagate the error.
  if (subExpr.isParseError())
    return makeParserResult<Expr>(
      new (Context) ErrorExpr(SourceRange(keywordLoc, rParenLoc)));

  return makeParserResult(
           new (Context) DynamicTypeExpr(keywordLoc, lParenLoc,
                                         subExpr.get(), rParenLoc, Type()));
}
