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

#include "Parser.h"
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

static bool parseCurriedFunctionArguments(Parser &P,
                                          SmallVectorImpl<Pattern*> &argPat,
                                          SmallVectorImpl<Pattern*> &bodyPat) {
  // parseFunctionArguments parsed the first argument pattern.
  // Parse additional curried argument clauses as long as we can.
  while (P.Tok.is(tok::l_paren)) {
    NullablePtr<Pattern> pattern = P.parsePatternTuple(/*AllowInitExpr=*/true);
    if (pattern.isNull())
      return true;
    else {
      argPat.push_back(pattern.get());
      bodyPat.push_back(pattern.get());
    }
  }
  return false;
}

static bool parseSelectorArgument(Parser &P,
                                  SmallVectorImpl<TuplePatternElt> &argElts,
                                  SmallVectorImpl<TuplePatternElt> &bodyElts,
                                  llvm::StringMap<VarDecl*> &selectorNames,
                                  SourceLoc &rp)
{
  NullablePtr<Pattern> argPattern = P.parsePatternIdentifier();
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
    P.diagnose(P.Tok.getLoc(),
                     diag::func_selector_without_paren);
    return true;
  }
  P.consumeToken();
  
  if (P.Tok.is(tok::r_paren)) {
    P.diagnose(P.Tok, diag::func_selector_with_not_one_argument);
    return true;
  }

  NullablePtr<Pattern> bodyPattern = P.parsePatternAtom();
  if (bodyPattern.isNull()) {
    P.skipUntil(tok::r_paren);
    return true;
  }
  
  if (P.consumeIf(tok::colon)) {
    TypeLoc type;
    if (P.parseTypeAnnotation(type)) {
      P.skipUntil(tok::r_paren);
      return true;
    }
    
    argPattern = new (P.Context) TypedPattern(argPattern.get(), type);
    bodyPattern = new (P.Context) TypedPattern(bodyPattern.get(), type);
  }
  
  ExprHandle *init = nullptr;
  if (P.consumeIf(tok::equal)) {
    NullablePtr<Expr> initR =
      P.parseExpr(diag::expected_initializer_expr);
    if (initR.isNull()) {
      P.skipUntil(tok::r_paren);
      return true;
    }
    init = ExprHandle::get(P.Context, initR.get());
  }
  
  if (P.Tok.is(tok::comma)) {
    P.diagnose(P.Tok, diag::func_selector_with_not_one_argument);
    P.skipUntil(tok::r_paren);
    return true;
  }
  
  if (P.Tok.isNot(tok::r_paren)) {
    P.diagnose(P.Tok, diag::expected_rparen_tuple_pattern_list);
    return true;
  }
  
  rp = P.consumeToken(tok::r_paren);
  argElts.push_back(TuplePatternElt(argPattern.get(), init));
  bodyElts.push_back(TuplePatternElt(bodyPattern.get(), init));
  return false;
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

static bool parseSelectorFunctionArguments(Parser &P,
                                           SmallVectorImpl<Pattern*> &argPat,
                                           SmallVectorImpl<Pattern*> &bodyPat,
                                           Pattern *firstPattern)
{
  SourceLoc lp;
  SmallVector<TuplePatternElt, 8> argElts;
  SmallVector<TuplePatternElt, 8> bodyElts;

  // For the argument pattern, try to convert the first parameter pattern to
  // an anonymous AnyPattern of the same type as the body parameter.
  if (ParenPattern *firstParen = dyn_cast<ParenPattern>(firstPattern)) {
    bodyElts.push_back(TuplePatternElt(firstParen->getSubPattern()));
    lp = firstParen->getLParenLoc();
    argElts.push_back(TuplePatternElt(
      getFirstSelectorPattern(P.Context,
                              firstParen->getSubPattern(),
                              firstParen->getLoc())));
  } else if (TuplePattern *firstTuple = dyn_cast<TuplePattern>(firstPattern)) {
    if (firstTuple->getNumFields() != 1) {
      P.diagnose(P.Tok, diag::func_selector_with_not_one_argument);
      return true;
    }
    
    TuplePatternElt const &firstElt = firstTuple->getFields()[0];
    bodyElts.push_back(firstElt);
    argElts.push_back(TuplePatternElt(
      getFirstSelectorPattern(P.Context,
                              firstElt.getPattern(),
                              firstTuple->getLoc()),
      firstElt.getInit(),
      firstElt.getVarargBaseType()));
  } else
    llvm_unreachable("unexpected function argument pattern!");
  
  // Parse additional selectors as long as we can.
  SourceLoc rp;
  llvm::StringMap<VarDecl*> selectorNames;

  for (;;) {
    if (P.isStartOfBindingName(P.Tok)) {
      if (parseSelectorArgument(P, argElts, bodyElts, selectorNames, rp)) {
        return true;
      }
    } else if (P.Tok.is(tok::l_paren)) {
      P.diagnose(P.Tok, diag::func_selector_with_curry);
      return true;
    } else
      break;
  }
  
  argPat.push_back(TuplePattern::create(P.Context, lp, argElts, rp));
  bodyPat.push_back(TuplePattern::create(P.Context, lp, bodyElts, rp));
  return false;
}

bool Parser::parseFunctionArguments(SmallVectorImpl<Pattern*> &argPatterns,
                                    SmallVectorImpl<Pattern*> &bodyPatterns) {
  // Parse the first function argument clause.
  NullablePtr<Pattern> pattern = parsePatternTuple(/*AllowInitExpr=*/true);
  if (pattern.isNull())
    return true;
  else {
    Pattern *firstPattern = pattern.get();
    
    if (isStartOfBindingName(Tok)) {
      // This looks like a selector-style argument. Try to convert the first
      // argument pattern into a single argument type and parse subsequent
      // selector forms.
      return parseSelectorFunctionArguments(*this,
                                            argPatterns, bodyPatterns,
                                            pattern.get());
    } else {
      argPatterns.push_back(firstPattern);
      bodyPatterns.push_back(firstPattern);
      return parseCurriedFunctionArguments(*this,
                                           argPatterns, bodyPatterns);
    }
  }
}

/// parseFunctionSignature - Parse a function definition signature.
///   func-signature:
///     func-arguments func-signature-result?
///   func-signature-result:
///     '->' type
///
/// Note that this leaves retType as null if unspecified.
bool Parser::parseFunctionSignature(SmallVectorImpl<Pattern*> &argPatterns,
                                    SmallVectorImpl<Pattern*> &bodyPatterns,
                                    TypeLoc &retType) {
  if (parseFunctionArguments(argPatterns, bodyPatterns))
    return true;

  // If there's a trailing arrow, parse the rest as the result type.
  if (consumeIf(tok::arrow)) {
    if (parseType(retType))
      return true;
  }
  // Otherwise, we leave retType null.

  return false;
}

/// Parse a pattern.
///   pattern ::= pattern-atom
///   pattern ::= pattern-atom ':' type-annotation
NullablePtr<Pattern> Parser::parsePattern() {
  // First, parse the pattern atom.
  NullablePtr<Pattern> pattern = parsePatternAtom();
  if (pattern.isNull()) return nullptr;

  // Now parse an optional type annotation.
  if (consumeIf(tok::colon)) {
    TypeLoc type;
    if (parseTypeAnnotation(type))
      return nullptr;

    pattern = new (Context) TypedPattern(pattern.get(), type);
  }

  return pattern;
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

/// Parse an identifier as a pattern.
NullablePtr<Pattern> Parser::parsePatternIdentifier() {
  SourceLoc loc = Tok.getLoc();
  if (consumeIf(tok::kw__)) {
    return new (Context) AnyPattern(loc);
  }
  
  StringRef text = Tok.getText();
  if (consumeIf(tok::identifier)) {
    Identifier ident = Context.getIdentifier(text);
    VarDecl *var = new (Context) VarDecl(loc, ident, Type(), nullptr);
    return new (Context) NamedPattern(var);
  }
  return nullptr;
}

/// Parse a pattern "atom", meaning the part that precedes the
/// optional type annotation.
///
///   pattern-atom ::= identifier
///   pattern-atom ::= '_'
///   pattern-atom ::= pattern-tuple
NullablePtr<Pattern> Parser::parsePatternAtom() {
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

Optional<TuplePatternElt> Parser::parsePatternTupleElement(bool allowInitExpr) {
  // Parse the pattern.
  NullablePtr<Pattern> pattern = parsePattern();
  if (pattern.isNull())
    return Nothing;

  // Parse the optional initializer.
  ExprHandle *init = nullptr;
  if (Tok.is(tok::equal)) {
    SourceLoc EqualLoc = consumeToken();
    if (!allowInitExpr) {
      diagnose(EqualLoc, diag::non_func_decl_pattern_init);
    }
    NullablePtr<Expr> initR = parseExpr(diag::expected_initializer_expr);

    // FIXME: Silently dropping initializer expressions where they aren't
    // permitted.
    if (allowInitExpr && initR.isNonNull())
      init = ExprHandle::get(Context, initR.get());
  }

  // The result, should we succeed.
  TuplePatternElt result(pattern.get(), init);

  // If there is no ellipsis, we're done.
  if (Tok.isNot(tok::ellipsis))
    return result;

  // An element cannot have both an initializer and an ellipsis.
  if (init) {
    diagnose(Tok.getLoc(), diag::tuple_ellipsis_init)
      .highlight(init->getExpr()->getSourceRange());
    consumeToken(tok::ellipsis);
    return result;
  }
  
  SourceLoc ellipsisLoc = consumeToken(tok::ellipsis);

  // An ellipsis element shall have a specified element type.
  // FIXME: This seems unnecessary.
  TypedPattern *typedPattern = dyn_cast<TypedPattern>(result.getPattern());
  if (!typedPattern) {
    diagnose(ellipsisLoc, diag::untyped_pattern_ellipsis)
      .highlight(result.getPattern()->getSourceRange());
    return result;
  }

  // Update the element and pattern to make it variadic.
  Type subTy = typedPattern->getTypeLoc().getType();
  result.setVarargBaseType(subTy);
  typedPattern->getTypeLoc()
    = TypeLoc(ArraySliceType::get(subTy, Context),
                                  typedPattern->getTypeLoc().getSourceRange());
  return result;
}

/// Parse a tuple pattern.
///
///   pattern-tuple:
///     '(' pattern-tuple-body? ')'
///   pattern-tuple-body:
///     pattern-tuple-element (',' pattern-tuple-body)*

NullablePtr<Pattern> Parser::parsePatternTuple(bool AllowInitExpr) {
  SourceLoc RPLoc, LPLoc = consumeToken(tok::l_paren);

  // Parse all the elements.
  SmallVector<TuplePatternElt, 8> elts;
  bool Invalid = parseList(tok::r_paren, LPLoc, RPLoc,
                           tok::comma, /*OptionalSep=*/false,
                           diag::expected_rparen_tuple_pattern_list,
                           [&] () -> bool {
    // Parse the pattern tuple element.
    Optional<TuplePatternElt> elt = parsePatternTupleElement(AllowInitExpr);
    if (!elt)
      return true;

    // Variadic elements must come last.
    // FIXME: Unnecessary restriction. It makes conversion more interesting,
    // but is not complicated to support.
    if (!elts.empty() && elts.back().isVararg()) {
      diagnose(elts.back().getPattern()->getLoc(),
               diag::ellipsis_pattern_not_at_end)
        .highlight(elt->getPattern()->getSourceRange());

      // Make the previous element non-variadic.
      elts.back().revertToNonVariadic();
    }

    // Add this element to the list.
    elts.push_back(*elt);
    return false;
  });

  if (Invalid)
    return nullptr;

  // A pattern which wraps a single anonymous pattern is not a tuple.
  if (elts.size() == 1 &&
      elts[0].getInit() == nullptr &&
      elts[0].getPattern()->getBoundName().empty() &&
      !elts[0].isVararg())
    return new (Context) ParenPattern(LPLoc, elts[0].getPattern(), RPLoc);

  return TuplePattern::create(Context, LPLoc, elts, RPLoc);
}

NullablePtr<Pattern> Parser::parseMatchingPattern() {
  // TODO: Since we expect a pattern in this position, we should optimistically
  // parse pattern nodes for shared productions. For ease of initial
  // implementation, for now we always go through the expr parser and let name
  // binding determine what productions are really patterns.
  NullablePtr<Expr> subExpr = parseExpr(diag::expected_pattern);
  if (subExpr.isNull())
    return nullptr;
  
  return new (Context) ExprPattern(subExpr.get(),
                                   /*isResolved*/ false,
                                   /*matchExpr*/ nullptr);
}
