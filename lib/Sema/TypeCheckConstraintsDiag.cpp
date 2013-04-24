//===--- TypeCheckConstraintsDiag.cpp - Constraint Diagnostics ------------===//
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
// This file implements diagnostics for the type checker.
//
//===----------------------------------------------------------------------===//
#include "ConstraintSystem.h"
using namespace swift;
using namespace constraints;

/// \brief Simplify the given locator by zeroing in on the most specific
/// subexpression described by the locator.
///
/// \param range1 Will be populated with an "interesting" range 
static ConstraintLocator *simplifyLocator(ConstraintSystem &cs,
                                          ConstraintLocator *locator,
                                          SourceRange &range1,
                                          SourceRange &range2) {
  range1 = SourceRange();
  range2 = SourceRange();

  auto path = locator->getPath();
  auto anchor = locator->getAnchor();
  while (!path.empty()) {
    switch (path[0].getKind()) {
    case ConstraintLocator::ApplyArgument:
      // Extract application argument.
      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        anchor = applyExpr->getArg();
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::ApplyFunction:
      // Extract application function.
      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        anchor = applyExpr->getArg();
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::Load:
    case ConstraintLocator::RvalueAdjustment:
      // Loads and rvalue adjustments are implicit.
      path = path.slice(1);
      continue;

    case ConstraintLocator::NamedTupleElement:
    case ConstraintLocator::TupleElement:
      // Extract tuple element.
      if (auto tupleExpr = dyn_cast<TupleExpr>(anchor)) {
        anchor = tupleExpr->getElement(path[0].getValue());
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::MemberRefBase:
      if (auto dotExpr = dyn_cast<UnresolvedDotExpr>(anchor)) {
        range1 = dotExpr->getNameLoc();
        anchor = dotExpr->getBase();
        path = path.slice(1);
        continue;
      }
      break;

    default:
      // FIXME: Lots of other cases to handle.
      break;
    }

    // If we get here, we couldn't simplify the path further.
    break;
  }

  if (anchor == locator->getAnchor() &&
      path.size() == locator->getPath().size()) {
    return locator;
  }

  return cs.getConstraintLocator(anchor, path);
}

bool ConstraintSystem::diagnose() {
  if (SharedState->Failures.size() == 1) {
    auto &failure = *SharedState->Failures.begin();
    if (failure.getLocator() && failure.getLocator()->getAnchor()) {
      SourceRange range1, range2;

      auto locator = simplifyLocator(*this, failure.getLocator(), range1,
                                     range2);
      auto &tc = getTypeChecker();

      auto anchor = locator->getAnchor();
      auto loc = anchor->getLoc();
      switch (failure.getKind()) {
      case Failure::TupleSizeMismatch: {
        auto tuple1 = failure.getFirstType()->castTo<TupleType>();
        auto tuple2 = failure.getSecondType()->castTo<TupleType>();
        tc.diagnose(loc, diag::invalid_tuple_size, tuple1, tuple2,
                    tuple1->getFields().size(),
                    tuple2->getFields().size())
          .highlight(range1).highlight(range2);
        break;
      }

      case Failure::TupleUnused:
        tc.diagnose(loc, diag::invalid_tuple_element_unused,
                    failure.getFirstType(),
                    failure.getSecondType())
          .highlight(range1).highlight(range2);
        break;

      case Failure::TypesNotEqual:
      case Failure::TypesNotTrivialSubtypes:
      case Failure::TypesNotSubtypes:
      case Failure::TypesNotConvertible:
      case Failure::TypesNotConstructible:
        tc.diagnose(loc, diag::invalid_relation,
                    failure.getKind() - Failure::TypesNotEqual,
                    failure.getFirstType(),
                    failure.getSecondType())
          .highlight(range1).highlight(range2);
        break;

      case Failure::DoesNotHaveMember:
        tc.diagnose(loc, diag::does_not_have_member,
                    failure.getFirstType(),
                    failure.getName())
          .highlight(range1).highlight(range2);
        break;

      case Failure::DoesNotConformToProtocol:
        tc.diagnose(loc, diag::type_does_not_conform,
                    failure.getFirstType(),
                    failure.getSecondType())
          .highlight(range1).highlight(range2);
        break;

      default:
        // FIXME: Handle all failure kinds
        return false;
      }

      return true;
    }
  }

  return false;
}
