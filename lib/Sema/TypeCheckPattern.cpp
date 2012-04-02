//===--- TypeCheckPattern.cpp - Type Checking for Patterns ----------------===//
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
// This file implements semantic analysis for patterns, analysing a
// pattern tree in both bottom-up and top-down ways.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Attr.h"
using namespace swift;

static bool hasExplicitType(Pattern *P) {
  while (ParenPattern *PP = dyn_cast<ParenPattern>(P))
    P = PP->getSubPattern();
  return isa<TypedPattern>(P);
}

/// Set the type of the pattern if it does not already have a type.
/// Only evaluate the type expression if needed.
#define SET_TYPE(pattern, type) { \
  Pattern *PAT = (pattern); \
  if (PAT->hasType()) \
    assert(PAT->getType()->isEqual(type)); \
  else \
    PAT->setType(type); \
}

/// Perform bottom-up type-checking on a pattern.  If this returns
/// false, the type of the pattern will have been set.
bool TypeChecker::typeCheckPattern(Pattern *P) {
  switch (P->getKind()) {
  // Type-check paren patterns by checking the sub-pattern and
  // propagating that type out.
  case PatternKind::Paren: {
    Pattern *SP = cast<ParenPattern>(P)->getSubPattern();
    if (typeCheckPattern(SP))
      return true;
    SET_TYPE(P, SP->getType());
    return false;
  }

  // If we see an explicit type annotation, coerce the sub-pattern to
  // that type.
  case PatternKind::Typed:
    if (validateType(P->getType()))
      return true;
    if (coerceToType(cast<TypedPattern>(P)->getSubPattern(), P->getType()))
      return true;
    return false;

  // A wildcard or name pattern cannot appear by itself in a context
  // which requires an explicit type.
  case PatternKind::Any:
  case PatternKind::Named:
    diagnose(P->getLoc(), diag::cannot_infer_type_for_pattern);
    return true;

  // A tuple pattern propagates its tuple-ness out.
  case PatternKind::Tuple: {
    bool hadError = false;
    SmallVector<TupleTypeElt, 8> typeElts;

    for (TuplePatternElt &elt : cast<TuplePattern>(P)->getFields()) {
      Type type;
      Expr *init = elt.getInit();
      Pattern *pattern = elt.getPattern();

      // If there's an initializer and the pattern doesn't have an
      // explicit type, then we want to type-check the initializer and
      // coerce the pattern to that type.
      if (init && !hasExplicitType(pattern)) {
        if (typeCheckExpression(init)) {
          hadError = true;
        } else {
          type = init->getType();
          elt.setInit(init);
        }

      // Otherwise, we want to type-check the pattern.
      } else if (typeCheckPattern(pattern)) {
        hadError = true;
      } else {
        type = pattern->getType();

        // If there's an initializer, use the pattern's type
        // to type-check the initializer.
        if (init) {
          init = coerceToType(init, type);
          if (!init) {
            hadError = true;
          } else {
            elt.setInit(init);
          }
        }
      }

      typeElts.push_back(TupleTypeElt(type, pattern->getBoundName(), init));
    }

    if (hadError) return true;
    SET_TYPE(P, TupleType::get(typeElts, Context));
    return false;
  }
  }
  llvm_unreachable("bad pattern kind!");
}

/// Perform top-down type coercion on the given pattern.
bool TypeChecker::coerceToType(Pattern *P, Type type) {
  switch (P->getKind()) {
  // For parens, just set the type annotation and propagate inwards.
  case PatternKind::Paren:
    P->setType(type);
    return coerceToType(cast<ParenPattern>(P)->getSubPattern(), type);

  // If we see an explicit type annotation, coerce the sub-pattern to
  // that type.
  case PatternKind::Typed: {
    TypedPattern *TP = cast<TypedPattern>(P);
    if (validateType(TP->getType()))
      return true;

    // Complain if the types don't match exactly.
    // TODO: allow implicit conversions?
    if (!type->isEqual(TP->getType())) {
      diagnose(P->getLoc(), diag::pattern_type_mismatch_context, type);
      return true;
    }

    return coerceToType(TP->getSubPattern(), TP->getType());
  }

  // For wildcard and name patterns, just set the type.
  case PatternKind::Named: {
    NamedPattern *NP = cast<NamedPattern>(P);
    if (LValueType *lv = type->getAs<LValueType>()) {
      NP->getDecl()->overwriteType(lv->getObjectType());
      NP->getDecl()->getMutableAttrs().Byref = true;
    } else {
      NP->getDecl()->overwriteType(type);

      if (!type->isMaterializable())
        diagnose(NP->getLoc(), diag::named_pattern_not_materializable, type);
    }
    // fallthrough
  }
  case PatternKind::Any:
    P->setType(type);
    return false;

  // We can match a tuple pattern with a tuple type.
  // TODO: permit implicit conversions?
  case PatternKind::Tuple: {
    TuplePattern *TP = cast<TuplePattern>(P);

    // The context type must be a tuple.
    TupleType *tupleTy = type->getAs<TupleType>();
    if (!tupleTy) {
      diagnose(TP->getLParenLoc(), diag::tuple_pattern_in_non_tuple_context,
               type);
      return true;
    }

    // The number of elements must match exactly.
    // TODO: incomplete tuple patterns, with some syntax.
    if (tupleTy->getFields().size() != TP->getNumFields()) {
      diagnose(TP->getLParenLoc(), diag::tuple_pattern_length_mismatch, type);
      return true;
    }

    // Coerce each tuple element to the respective type.
    // TODO: detect and diagnose shuffling
    // TODO: permit shuffling
    P->setType(type);

    for (unsigned i = 0, e = TP->getNumFields(); i != e; ++i) {
      TuplePatternElt &elt = TP->getFields()[i];
      Pattern *pattern = elt.getPattern();
      if (elt.getInit()) {
        diagnose(pattern->getLoc(), diag::coerced_tuple_pattern_has_init)
          << pattern->getSourceRange();
        elt.setInit(nullptr);
      }

      if (coerceToType(pattern, tupleTy->getFields()[i].getType()))
        return true;
    }

    return false;
  }
  }
  llvm_unreachable("bad pattern kind!");
}
