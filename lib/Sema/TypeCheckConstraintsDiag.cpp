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

void Failure::dump(llvm::SourceMgr *sm) {

  llvm::raw_ostream &out = llvm::errs();
  out << "(";
  if (locator) {
    out << "@";
    locator->dump(sm);
    out << ": ";
  }

  switch (getKind()) {
  case DoesNotConformToProtocol:
    out << getFirstType().getString() << " does not conform to "
        << getSecondType().getString();
    break;

  case DoesNotHaveMember:
    out << getFirstType().getString() << " does not have a member named '"
        << getName().str() << "'";
    break;

  case FunctionAutoclosureMismatch:
    out << "autoclosure mismatch " << getFirstType().getString() << " vs. "
        << getSecondType().getString();
    break;

  case IsNotArchetype:
    out << getFirstType().getString() << " is not an archetype";
    break;

  case LValueQualifiers:
    out << "lvalue qualifier mismatch between " << getFirstType().getString()
        << " and " << getSecondType().getString();
    break;

  case TupleNameMismatch:
  case TupleNamePositionMismatch:
  case TupleSizeMismatch:
  case TupleVariadicMismatch:
  case TupleUnused:
    out << "mismatched tuple types " << getFirstType().getString() << " and "
        << getSecondType().getString();
    break;

  case TypesNotConstructible:
    out << getFirstType().getString() << " is not a constructible argument for "
        << getSecondType().getString();
    break;

  case TypesNotConvertible:
    out << getFirstType().getString() << " is not convertible to "
        << getSecondType().getString();
    break;

  case TypesNotSubtypes:
    out << getFirstType().getString() << " is not a subtype of "
        << getSecondType().getString();
    break;

  case TypesNotTrivialSubtypes:
    out << getFirstType().getString() << " is not a trivial subtype of "
        << getSecondType().getString();
    break;

  case TypesNotEqual:
    out << getFirstType().getString() << " is not equal to "
        << getSecondType().getString();
    break;
  }

  out << ")\n";
}


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

/// \brief Emit a diagnostic for the given failure.
///
/// \param cs The constraint system in which the diagnostic was generated.
/// \param failure The failure to emit.
///
/// \returns true if the diagnostic was emitted successfully.
static bool diagnoseFailure(ConstraintSystem &cs, Failure &failure) {
  // If there's no anchor, we have no location information to use when emitting
  // the diagnostic.
  if (!failure.getLocator() || !failure.getLocator()->getAnchor())
    return false;

  SourceRange range1, range2;

  auto locator = simplifyLocator(cs, failure.getLocator(), range1, range2);
  auto &tc = cs.getTypeChecker();

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

bool ConstraintSystem::diagnose() {
  // If there were any unavoidable failures, emit the first one we can.
  if (!unavoidableFailures.empty()) {
    for (auto failure : unavoidableFailures) {
      if (diagnoseFailure(*this, *failure))
        return true;
    }
    
    return false;
  }

  // There were no unavoidable failures, so attempt to solve again, capturing
  // any failures that come from our attempts to select overloads or bind
  // type variables.
  {
    SmallVector<Solution, 4> solutions;
    
    // Set up solver state.
    SolverState state;
    state.recordFailures = true;
    this->solverState = &state;

    // Solve the system.
    solve(solutions);

    // FIXME: If we were able to actually fix things along the way,
    // we may have to hunt for the best solution. For now, we don't care.

    // Remove the solver state.
    this->solverState = nullptr;

    // Fall through to produce diagnostics.
  }

  if (failures.size() == 1) {
    auto &failure = unavoidableFailures.empty()? *failures.begin()
                                               : **unavoidableFailures.begin();
    return diagnoseFailure(*this, failure);
  }

  return false;
}
