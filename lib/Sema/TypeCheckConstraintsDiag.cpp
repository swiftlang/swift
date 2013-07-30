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

  case FunctionTypesMismatch:
    out << "function type" << getFirstType().getString() << " is not equal to "
    << getSecondType().getString();
    break;

  case FunctionAutoclosureMismatch:
    out << "autoclosure mismatch " << getFirstType().getString() << " vs. "
        << getSecondType().getString();
    break;

  case FunctionNoReturnMismatch:
    out << "noreturn attribute mismatch " << getFirstType().getString()
    << " vs. " << getSecondType().getString();
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


ConstraintLocator *constraints::simplifyLocator(ConstraintSystem &cs,
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
        anchor = applyExpr->getFn();
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

    case ConstraintLocator::InterpolationArgument:
      if (auto interp = dyn_cast<InterpolatedStringLiteralExpr>(anchor)) {
        anchor = interp->getSegments()[path[0].getValue()];
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

/// Simplify the given locator down to a specific anchor expression,
/// if possible.
///
/// \returns the anchor expression if it fully describes the locator, or
/// null otherwise.
static Expr *simplifyLocatorToAnchor(ConstraintSystem &cs,
                                     ConstraintLocator *locator) {
  if (!locator || !locator->getAnchor())
    return nullptr;

  SourceRange range1, range2;
  locator = simplifyLocator(cs, locator, range1, range2);
  if (!locator->getAnchor() || !locator->getPath().empty())
    return nullptr;

  return locator->getAnchor();
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
  case Failure::FunctionTypesMismatch:
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
    // FIXME: Probably want to do this within the actual solver, because at
    // this point it's too late to actually recover fully.
    tc.conformsToProtocol(failure.getFirstType(),
                          failure.getSecondType()->castTo<ProtocolType>()
                            ->getDecl(),
                          nullptr,
                          loc);
    break;

  default:
    // FIXME: Handle all failure kinds
    return false;
  }

  return true;
}

/// \brief Determine the number of distinct overload choices in the
/// provided set.
static unsigned countDistinctOverloads(ArrayRef<OverloadChoice> choices) {
  llvm::SmallPtrSet<void *, 4> uniqueChoices;
  unsigned result = 0;
  for (auto choice : choices) {
    if (uniqueChoices.insert(choice.getOpaqueChoiceSimple()))
      ++result;
  }
  return result;
}

/// \brief Determine the name of the overload in a set of overload choices.
static Identifier getOverloadChoiceName(ArrayRef<OverloadChoice> choices) {
  for (auto choice : choices) {
    if (choice.getKind() == OverloadChoiceKind::Decl)
      return choice.getDecl()->getName();
  }

  return Identifier();
}

bool diagnoseAmbiguity(ConstraintSystem &cs, ArrayRef<Solution> solutions) {
  // Produce a diff of the solutions.
  SolutionDiff diff(solutions);

  // Find the locators which have the largest numbers of
  SmallVector<unsigned, 2> mostDistinctOverloads;
  unsigned maxDistinctOverloads = 0;
  for (unsigned i = 0, n = diff.overloads.size(); i != n; ++i) {
    auto &overload = diff.overloads[i];

    // If we can't resolve the locator to an anchor expression with no path,
    // we can't diagnose this well.
    if (!simplifyLocatorToAnchor(cs, overload.locator))
      continue;

    // If we don't have a name to hang on to, it'll be hard to diagnose this
    // overload.
    if (getOverloadChoiceName(overload.choices).empty())
      continue;

    unsigned distinctOverloads = countDistinctOverloads(overload.choices);

    // We need at least two overloads to make this interesting.
    if (distinctOverloads < 2)
      continue;

    // If we have more distinct overload choices for this locator than for
    // prior locators, just keep this locator.
    if (distinctOverloads > maxDistinctOverloads) {
      maxDistinctOverloads = distinctOverloads;
      mostDistinctOverloads.clear();
      mostDistinctOverloads.push_back(i);
      continue;
    }

    // If we have as many distinct overload choices for this locator as
    // the best so far, add this locator to the set.
    if (distinctOverloads == maxDistinctOverloads) {
      mostDistinctOverloads.push_back(i);
      continue;
    }

    // We have better results. Ignore this one.
  }

  // FIXME: Should be able to pick the best locator, e.g., based on some
  // depth-first numbering of expressions.
  if (mostDistinctOverloads.size() == 1) {
    auto &overload = diff.overloads[mostDistinctOverloads[0]];
    auto name = getOverloadChoiceName(overload.choices);
    auto anchor = simplifyLocatorToAnchor(cs, overload.locator);

    // Emit the ambiguity diagnostic.
    auto &tc = cs.getTypeChecker();
    tc.diagnose(anchor->getLoc(),
                name.isOperator() ? diag::ambiguous_operator_ref
                                  : diag::ambiguous_decl_ref,
                name);

    // Emit candidates.
    for (auto choice : overload.choices) {
      switch (choice.getKind()) {
      case OverloadChoiceKind::Decl:
        // FIXME: show deduced types, etc, etc.
        tc.diagnose(choice.getDecl(), diag::found_candidate);
        break;

      case OverloadChoiceKind::BaseType:
      case OverloadChoiceKind::FunctionReturningBaseType:
      case OverloadChoiceKind::IdentityFunction:
      case OverloadChoiceKind::TupleIndex:
        // FIXME: Actually diagnose something here.
        break;
      }
    }

    return true;
  }

  // FIXME: If we inferred different types for literals (for example),
  // could diagnose ambiguity that way as well.

  return false;
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

    // If there are multiple solutions, try to diagnose an ambiguity.
    if (solutions.size() > 1 && diagnoseAmbiguity(*this, solutions)) {
      return true;
    }

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
