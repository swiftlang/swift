//===--- ParsePattern.cpp - Swift Language Parser for Patterns ------------===//
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
// Pattern Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/AST/ExprHandle.h"
#include "llvm/ADT/StringMap.h"

using namespace swift;

/// Parse function arguments.
///   func-arguments:
///     curried-arguments | selector-arguments
///   curried-arguments:
///     pattern-tuple+
///   selector-arguments:
///     '(' selector-element ')' (identifier '(' selector-element ')')+
///   selector-element:
///      identifier '(' pattern-atom (':' type-annotation)? ('=' expr)? ')'

static ParserStatus
parseCurriedFunctionArguments(Parser &P,
                              SmallVectorImpl<Pattern *> &argPat,
                              SmallVectorImpl<Pattern *> &bodyPat) {
  // parseFunctionArguments parsed the first argument pattern.
  // Parse additional curried argument clauses as long as we can.
  while (P.Tok.is(tok::l_paren)) {
    ParserResult<Pattern> pattern = P.parsePatternTuple(/*AllowInitExpr=*/false);
    if (pattern.isNull() || pattern.hasCodeCompletion())
      return pattern;

    argPat.push_back(pattern.get());
    bodyPat.push_back(pattern.get());
  }
  return makeParserSuccess();
}

/// \brief Determine the kind of a default argument given a parsed
/// expression that has not yet been type-checked.
static DefaultArgumentKind getDefaultArgKind(ExprHandle *init) {
  if (!init || !init->getExpr())
    return DefaultArgumentKind::None;

  auto magic = dyn_cast<MagicIdentifierLiteralExpr>(init->getExpr());
  if (!magic)
    return DefaultArgumentKind::Normal;

  switch (magic->getKind()) {
  case MagicIdentifierLiteralExpr::Column:
    return DefaultArgumentKind::Column;
  case MagicIdentifierLiteralExpr::File:
    return DefaultArgumentKind::File;
  case MagicIdentifierLiteralExpr::Line:
    return DefaultArgumentKind::Line;
  }
}

static ParserStatus
parseSelectorArgument(Parser &P,
                      SmallVectorImpl<TuplePatternElt> &argElts,
                      SmallVectorImpl<TuplePatternElt> &bodyElts,
                      llvm::StringMap<VarDecl *> &selectorNames,
                      SourceLoc &rp) {
  ParserResult<Pattern> argPattern = P.parsePatternIdentifier();
  assert(argPattern.isNonNull() &&
         "selector argument did not start with an identifier!");
  
  // Check that a selector name isn't used multiple times, which would
  // lead to the function type having multiple arguments with the same name.
  if (NamedPattern *name = dyn_cast<NamedPattern>(argPattern.get())) {
    VarDecl *decl = name->getDecl();
    StringRef id = decl->getName().str();
    auto prevName = selectorNames.find(id);
    if (prevName != selectorNames.end()) {
      P.diagnoseRedefinition(prevName->getValue(), decl);
    } else {
      selectorNames[id] = decl;
    }
  }
  
  if (!P.Tok.is(tok::l_paren)) {
    P.diagnose(P.Tok, diag::func_selector_without_paren);
    return makeParserError();
  }
  P.consumeToken();
  
  if (P.Tok.is(tok::r_paren)) {
    P.diagnose(P.Tok, diag::func_selector_with_not_one_argument);
    rp = P.consumeToken(tok::r_paren);
    return makeParserError();
  }

  ParserResult<Pattern> bodyPattern = P.parsePatternAtom();
  if (bodyPattern.isNull()) {
    P.skipUntil(tok::r_paren);
    return makeParserError();
  }
  
  if (P.consumeIf(tok::colon)) {
    ParserResult<TypeRepr> type = P.parseTypeAnnotation();
    bool HadParseError = false;
    if (type.isNull()) {
      HadParseError = true;
      type = makeParserResult(new (P.Context) ErrorTypeRepr(P.Tok.getLoc()));
      P.skipUntil(tok::r_paren);
    }
    if (type.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
    
    argPattern = makeParserResult(
        new (P.Context) TypedPattern(argPattern.get(), type.get()));
    bodyPattern = makeParserResult(
        new (P.Context) TypedPattern(bodyPattern.get(), type.get()));
    if (HadParseError)
      return makeParserError();
  }
  
  ExprHandle *init = nullptr;
  if (P.consumeIf(tok::equal)) {
    NullablePtr<Expr> initR =
      P.parseExpr(diag::expected_initializer_expr);
    if (initR.isNull()) {
      P.skipUntil(tok::r_paren);
      return makeParserError();
    }
    init = ExprHandle::get(P.Context, initR.get());
  }
  
  if (P.Tok.is(tok::comma)) {
    P.diagnose(P.Tok, diag::func_selector_with_not_one_argument);
    P.skipUntil(tok::r_paren);
    P.consumeIf(tok::r_paren);
    return makeParserError();
  }
  
  if (P.Tok.isNot(tok::r_paren)) {
    P.diagnose(P.Tok, diag::expected_rparen_tuple_pattern_list);
    return makeParserError();
  }
  
  rp = P.consumeToken(tok::r_paren);
  argElts.push_back(TuplePatternElt(argPattern.get(), init,
                                    getDefaultArgKind(init)));
  bodyElts.push_back(TuplePatternElt(bodyPattern.get(), init,
                                     getDefaultArgKind(init)));
  return makeParserSuccess();
}

static Pattern *getFirstSelectorPattern(ASTContext &Context,
                                        const Pattern *argPattern,
                                        SourceLoc loc)
{
  Pattern *pattern = new (Context) AnyPattern(loc);
  if (auto typed = dyn_cast<TypedPattern>(argPattern)) {
    pattern = new (Context) TypedPattern(pattern, typed->getTypeLoc());
  }
  return pattern;
}

static ParserStatus
parseSelectorFunctionArguments(Parser &P,
                               SmallVectorImpl<Pattern *> &argPat,
                               SmallVectorImpl<Pattern *> &bodyPat,
                               Pattern *firstPattern) {
  SourceLoc lp;
  SourceLoc rp;
  SmallVector<TuplePatternElt, 8> argElts;
  SmallVector<TuplePatternElt, 8> bodyElts;

  // For the argument pattern, try to convert the first parameter pattern to
  // an anonymous AnyPattern of the same type as the body parameter.
  if (ParenPattern *firstParen = dyn_cast<ParenPattern>(firstPattern)) {
    bodyElts.push_back(TuplePatternElt(firstParen->getSubPattern()));
    lp = firstParen->getLParenLoc();
    rp = firstParen->getRParenLoc();
    argElts.push_back(TuplePatternElt(
      getFirstSelectorPattern(P.Context,
                              firstParen->getSubPattern(),
                              firstParen->getLoc())));
  } else if (TuplePattern *firstTuple = dyn_cast<TuplePattern>(firstPattern)) {
    lp = firstTuple->getLParenLoc();
    rp = firstTuple->getRParenLoc();
    if (firstTuple->getNumFields() != 1) {
      P.diagnose(P.Tok, diag::func_selector_with_not_one_argument);
    } else {
      TuplePatternElt const &firstElt = firstTuple->getFields()[0];
      bodyElts.push_back(firstElt);
      argElts.push_back(TuplePatternElt(
          getFirstSelectorPattern(P.Context,
                                  firstElt.getPattern(),
                                  firstTuple->getLoc()),
        firstElt.getInit(),
        firstElt.getDefaultArgKind()));
    }
  } else
    llvm_unreachable("unexpected function argument pattern!");
  
  // Parse additional selectors as long as we can.
  llvm::StringMap<VarDecl*> selectorNames;

  ParserStatus Status;
  for (;;) {
    if (P.isStartOfBindingName(P.Tok)) {
      Status |= parseSelectorArgument(P, argElts, bodyElts, selectorNames, rp);
    } else if (P.Tok.is(tok::l_paren)) {
      P.diagnose(P.Tok, diag::func_selector_with_curry);
      // FIXME: better recovery: just parse a tuple.
      P.skipUntilDeclRBrace(tok::l_brace);
      return makeParserError();
    } else
      break;
  }
  
  argPat.push_back(TuplePattern::create(P.Context, lp, argElts, rp));
  bodyPat.push_back(TuplePattern::create(P.Context, lp, bodyElts, rp));
  return Status;
}

ParserStatus
Parser::parseFunctionArguments(SmallVectorImpl<Pattern *> &ArgPatterns,
                               SmallVectorImpl<Pattern *> &BodyPatterns) {
  // Parse the first function argument clause.
  ParserResult<Pattern> FirstPattern = parsePatternTuple(/*AllowInitExpr=*/true);
  if (FirstPattern.isNull() || FirstPattern.hasCodeCompletion())
    // FIXME: improve recovery: we should not stop if isNull().
    // But if we saw code completion token, there is no point in continuing.
    return FirstPattern;

  if (isStartOfBindingName(Tok)) {
    // This looks like a selector-style argument.  Try to convert the first
    // argument pattern into a single argument type and parse subsequent
    // selector forms.
    return ParserStatus(FirstPattern) |
           parseSelectorFunctionArguments(*this, ArgPatterns, BodyPatterns,
                                          FirstPattern.get());
  } else {
    ArgPatterns.push_back(FirstPattern.get());
    BodyPatterns.push_back(FirstPattern.get());
    return ParserStatus(FirstPattern) |
           parseCurriedFunctionArguments(*this, ArgPatterns, BodyPatterns);
  }
}

/// parseFunctionSignature - Parse a function definition signature.
///   func-signature:
///     func-arguments func-signature-result?
///   func-signature-result:
///     '->' type
///
/// Note that this leaves retType as null if unspecified.
ParserStatus
Parser::parseFunctionSignature(SmallVectorImpl<Pattern *> &argPatterns,
                               SmallVectorImpl<Pattern *> &bodyPatterns,
                               TypeRepr *&retType) {
  ParserStatus Status = parseFunctionArguments(argPatterns, bodyPatterns);

  // If there's a trailing arrow, parse the rest as the result type.
  if (consumeIf(tok::arrow)) {
    ParserResult<TypeRepr> ResultType = parseType();
    if (ResultType.hasCodeCompletion())
      return ResultType;
    retType = ResultType.getPtrOrNull();
    if (!retType) {
      Status.setIsParseError();
      return Status;
    }
  } else {
    // Otherwise, we leave retType null.
    retType = nullptr;
  }

  return Status;
}

/// Parse a pattern.
///   pattern ::= pattern-atom
///   pattern ::= pattern-atom ':' type-annotation
ParserResult<Pattern> Parser::parsePattern() {
  // First, parse the pattern atom.
  ParserResult<Pattern> Result = parsePatternAtom();

  // Now parse an optional type annotation.
  if (consumeIf(tok::colon)) {
    if (Result.isNull()) {
      // Recover by creating AnyPattern.
      Result = makeParserErrorResult(new (Context) AnyPattern(Tok.getLoc()));
    }

    ParserResult<TypeRepr> Ty = parseTypeAnnotation();
    if (Ty.hasCodeCompletion())
      return makeParserCodeCompletionResult<Pattern>();

    if (Ty.isNull())
      Ty = makeParserResult(new (Context) ErrorTypeRepr(Tok.getLoc()));

    Result = makeParserResult(Result,
        new (Context) TypedPattern(Result.get(), Ty.get()));
  }

  return Result;
}

/// \brief Determine whether this token can start a pattern.
bool Parser::isStartOfPattern(Token tok) {
  return tok.is(tok::kw__) || tok.is(tok::identifier) || tok.is(tok::l_paren);
}

/// \brief Determine whether this token can start a binding name, whether an
/// identifier or the special discard-value binding '_'.
bool Parser::isStartOfBindingName(Token tok) {
  return tok.is(tok::kw__) || tok.is(tok::identifier);
}

Pattern *Parser::createBindingFromPattern(SourceLoc loc,
                                          Identifier name) {
  VarDecl *var = new (Context) VarDecl(loc, name, Type(), nullptr);
  return new (Context) NamedPattern(var);
}

/// Parse an identifier as a pattern.
ParserResult<Pattern> Parser::parsePatternIdentifier() {
  SourceLoc loc = Tok.getLoc();
  if (consumeIf(tok::kw__)) {
    return makeParserResult(new (Context) AnyPattern(loc));
  }
  
  StringRef text = Tok.getText();
  if (consumeIf(tok::identifier)) {
    Identifier ident = Context.getIdentifier(text);
    return makeParserResult(createBindingFromPattern(loc, ident));
  }

  return nullptr;
}

/// Parse a pattern "atom", meaning the part that precedes the
/// optional type annotation.
///
///   pattern-atom ::= identifier
///   pattern-atom ::= '_'
///   pattern-atom ::= pattern-tuple
ParserResult<Pattern> Parser::parsePatternAtom() {
  switch (Tok.getKind()) {
  case tok::l_paren:
    return parsePatternTuple(/*AllowInitExpr*/false);

  case tok::identifier:
  case tok::kw__:
    return parsePatternIdentifier();

#define IDENTIFIER_KEYWORD(kw) case tok::kw_##kw:
#include "swift/Parse/Tokens.def"
    diagnose(Tok, diag::expected_pattern_is_keyword);
    consumeToken();
    return nullptr;

  default:
    diagnose(Tok, diag::expected_pattern);
    return nullptr;
  }
}

std::pair<ParserStatus, Optional<TuplePatternElt>>
Parser::parsePatternTupleElement(bool allowInitExpr) {
  // Parse the pattern.
  ParserResult<Pattern> pattern = parsePattern();
  if (pattern.hasCodeCompletion())
    return std::make_pair(makeParserCodeCompletionStatus(), Nothing);

  if (pattern.isNull())
    return std::make_pair(makeParserError(), Nothing);

  // Parse the optional initializer.
  ExprHandle *init = nullptr;
  if (Tok.is(tok::equal)) {
    SourceLoc EqualLoc = consumeToken();
    NullablePtr<Expr> initR = parseExpr(diag::expected_initializer_expr);

    if (!allowInitExpr) {
      auto inFlight = diagnose(EqualLoc, diag::non_func_decl_pattern_init);
      if (initR.isNonNull())
        inFlight.fixItRemove(SourceRange(EqualLoc, initR.get()->getEndLoc()));
    }

    // FIXME: Silently dropping initializer expressions where they aren't
    // permitted.
    if (allowInitExpr && initR.isNonNull())
      init = ExprHandle::get(Context, initR.get());
  }

  return std::make_pair(
      makeParserSuccess(),
      TuplePatternElt(pattern.get(), init, getDefaultArgKind(init)));
}

/// Parse a tuple pattern.
///
///   pattern-tuple:
///     '(' pattern-tuple-body? ')'
///   pattern-tuple-body:
///     pattern-tuple-element (',' pattern-tuple-body)*

ParserResult<Pattern> Parser::parsePatternTuple(bool AllowInitExpr) {
  SourceLoc RPLoc, LPLoc = consumeToken(tok::l_paren);
  SourceLoc EllipsisLoc;

  // Parse all the elements.
  SmallVector<TuplePatternElt, 8> elts;
  ParserStatus ListStatus = parseList(tok::r_paren, LPLoc, RPLoc,
                                      tok::comma, /*OptionalSep=*/false,
                                      diag::expected_rparen_tuple_pattern_list,
                                      [&] () -> ParserStatus {
    // Parse the pattern tuple element.
    ParserStatus EltStatus;
    Optional<TuplePatternElt> elt;
    std::tie(EltStatus, elt) = parsePatternTupleElement(AllowInitExpr);
    if (EltStatus.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
    if (!elt)
      return makeParserError();

    // Add this element to the list.
    elts.push_back(*elt);

    // If there is no ellipsis, we're done with the element.
    if (Tok.isNot(tok::ellipsis))
      return makeParserSuccess();
    SourceLoc ellLoc = consumeToken(tok::ellipsis);

    // An element cannot have both an initializer and an ellipsis.
    if (elt->getInit()) {
      diagnose(ellLoc, diag::tuple_ellipsis_init)
        .highlight(elt->getInit()->getExpr()->getSourceRange());
      // Return success since the error was semantic, and the caller should not
      // attempt recovery.
      return makeParserSuccess();
    }

    // An ellipsis element shall have a specified element type.
    // FIXME: This seems unnecessary.
    TypedPattern *typedPattern = dyn_cast<TypedPattern>(elt->getPattern());
    if (!typedPattern) {
      diagnose(ellLoc, diag::untyped_pattern_ellipsis)
        .highlight(elt->getPattern()->getSourceRange());
      // Return success so that the caller does not attempt recovery -- it
      // should have already happened when we were parsing the tuple element.
      return makeParserSuccess();
    }

    // Variadic elements must come last.
    // FIXME: Unnecessary restriction. It makes conversion more interesting,
    // but is not complicated to support.
    if (Tok.is(tok::r_paren)) {
      EllipsisLoc = ellLoc;
    } else {
      diagnose(ellLoc, diag::ellipsis_pattern_not_at_end);
    }

    return makeParserSuccess();
  });

  return makeParserResult(ListStatus, TuplePattern::createSimple(
                                          Context, LPLoc, elts, RPLoc,
                                          EllipsisLoc.isValid(), EllipsisLoc));
}

ParserResult<Pattern> Parser::parseMatchingPattern() {
  // TODO: Since we expect a pattern in this position, we should optimistically
  // parse pattern nodes for productions shared by pattern and expression
  // grammar. For short-term ease of initial implementation, we always go
  // through the expr parser for ambiguious productions.

  // Parse productions that can only be patterns.
  // matching-pattern ::= matching-pattern-var
  if (Tok.is(tok::kw_var)) {
    return parseMatchingPatternVar();
  }
  // matching-pattern ::= '_'
  if (Tok.is(tok::kw__)) {
    return makeParserResult(new (Context) AnyPattern(consumeToken()));
  }
  // matching-pattern ::= 'is' type
  if (Tok.is(tok::kw_is)) {
    return parseMatchingPatternIsa();
  }
  
  // matching-pattern ::= expr
  // Fall back to expression parsing for ambiguous forms. Name lookup will
  // disambiguate.
  NullablePtr<Expr> subExpr = parseExpr(diag::expected_pattern);
  if (subExpr.isNull())
    return nullptr;
  
  return makeParserResult(new (Context) ExprPattern(subExpr.get()));
}

ParserResult<Pattern> Parser::parseMatchingPatternVar() {
  // 'var' patterns shouldn't nest.
  if (VarPatternDepth >= 1)
    diagnose(Tok, diag::var_pattern_in_var);
  
  VarPatternScope scope(*this);
  
  SourceLoc varLoc = consumeToken(tok::kw_var);
  ParserResult<Pattern> subPattern = parseMatchingPattern();
  if (subPattern.isNull())
    return nullptr;
  return makeParserResult(new (Context) VarPattern(varLoc, subPattern.get()));
}

ParserResult<Pattern> Parser::parseMatchingPatternIsa() {
  SourceLoc isLoc = consumeToken(tok::kw_is);
  ParserResult<TypeRepr> castType = parseType();
  if (castType.isNull() || castType.hasCodeCompletion())
    return nullptr;
  return makeParserResult(new (Context) IsaPattern(isLoc, castType.get()));
}

bool Parser::isOnlyStartOfMatchingPattern() {
  return Tok.is(tok::kw_var)
    || Tok.is(tok::kw__)
    || Tok.is(tok::kw_is);
}
