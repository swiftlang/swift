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

using namespace swift;

/// parseFunctionSignature - Parse a function definition signature.
///   func-signature:
///     pattern-tuple+ func-signature-result?
///   func-signature-result:
///     '->' type
bool Parser::parseFunctionSignature(SmallVectorImpl<Pattern*> &params,
                                    Type &retType, TypeLoc *&retLoc) {
  // Parse curried function argument clauses as long as we can.
  do {
    NullablePtr<Pattern> pattern = parsePatternTuple();
    if (pattern.isNull())
      return true;
    else
      params.push_back(pattern.get());
  } while (Tok.isAnyLParen());

  // If there's a trailing arrow, parse the rest as the result type.
  if (consumeIf(tok::arrow)) {
    if (parseType(retType, retLoc))
      return true;

  // Otherwise, we implicitly return ().
  } else {
    retType = TupleType::getEmpty(Context);
    retLoc = nullptr;
  }

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
    Type type;
    TypeLoc *loc;
    if (parseTypeAnnotation(type, loc))
      return nullptr;

    pattern = new (Context) TypedPattern(pattern.get(), type, loc);
  }

  return pattern;
}

/// Parse a pattern "atom", meaning the part that precedes the
/// optional type annotation.
///
///   pattern-atom ::= identifier
///   pattern-atom ::= pattern-tuple
NullablePtr<Pattern> Parser::parsePatternAtom() {
  switch (Tok.getKind()) {
  case tok::l_paren:
  case tok::l_paren_space:
    return parsePatternTuple();

  case tok::identifier: {
    SourceLoc loc = Tok.getLoc();
    StringRef text = Tok.getText();
    consumeToken(tok::identifier);

    // '_' is a special case which means 'ignore this'.
    if (text == "_") {
      return new (Context) AnyPattern(loc);
    } else {
      Identifier ident = Context.getIdentifier(text);
      VarDecl *var = new (Context) VarDecl(loc, ident, Type(), nullptr);
      return new (Context) NamedPattern(var);
    }
  }

  default:
    diagnose(Tok, diag::expected_pattern);
    return nullptr;
  }
}

/// Parse a tuple pattern.
///
///   pattern-tuple:
////    '(' pattern-tuple-body? ')'
///   pattern-tuple-body:
///     pattern-tuple-element (',' pattern-tuple-body)*
///   pattern-tuple-element:
///     pattern ('=' expr)?
NullablePtr<Pattern> Parser::parsePatternTuple() {
  assert(Tok.isAnyLParen());

  // We're looking at the left parenthesis; consume it.
  SourceLoc lp = consumeToken();

  // Parse all the elements.
  SmallVector<TuplePatternElt, 8> elts;
  bool hadEllipsis = false;
  if (Tok.isNot(tok::r_paren)) {
    do {
      NullablePtr<Pattern> pattern = parsePattern();
      ExprHandle *init = nullptr;

      if (pattern.isNull()) {
        skipUntil(tok::r_paren);
        return nullptr;
      } else if (consumeIf(tok::equal)) {
        NullablePtr<Expr> initR = parseExpr(diag::expected_initializer_expr);
        if (initR.isNull()) {
          skipUntil(tok::r_paren);
          return nullptr;
        }
        
        init = ExprHandle::get(Context, initR.get());
      }

      elts.push_back(TuplePatternElt(pattern.get(), init));
    } while (consumeIf(tok::comma));

    if (Tok.is(tok::ellipsis)) {
      if (elts.back().getInit()) {
        diagnose(Tok.getLoc(), diag::tuple_ellipsis_init);
        skipUntil(tok::r_paren);
        return nullptr;
      }
      hadEllipsis = true;
      consumeToken(tok::ellipsis);

      TypedPattern *subpattern =
          dyn_cast<TypedPattern>(elts.back().getPattern());
      if (!subpattern) {
        diagnose(elts.back().getPattern()->getLoc(),
                 diag::untyped_pattern_ellipsis);
        skipUntil(tok::r_paren);
        return nullptr;
      }
      Type subTy = subpattern->getType();
      elts.back().setVarargBaseType(subTy);
      subpattern->overwriteType(ArraySliceType::get(subTy, SourceLoc(),
                                                    Context));
    }

    if (Tok.isNot(tok::r_paren)) {
      diagnose(Tok, diag::expected_rparen_tuple_pattern_list);
      skipUntil(tok::r_paren);
      return nullptr;
    }
  }

  // Consume the right parenthesis.
  SourceLoc rp = consumeToken(tok::r_paren);

  // A pattern which wraps a single anonymous pattern is not a tuple.
  if (elts.size() == 1 &&
      elts[0].getInit() == nullptr &&
      elts[0].getPattern()->getBoundName().empty() &&
      !hadEllipsis)
    return new (Context) ParenPattern(lp, elts[0].getPattern(), rp);

  return TuplePattern::create(Context, lp, elts, rp);
}    
