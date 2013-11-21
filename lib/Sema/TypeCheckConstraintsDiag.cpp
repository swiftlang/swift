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

void Failure::dump(SourceManager *sm) const {
  dump(sm, llvm::errs());
}

void Failure::dump(SourceManager *sm, raw_ostream &out) const {
  out << "(";
  if (locator) {
    out << "@";
    locator->dump(sm, out);
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
    out << "function type " << getFirstType().getString() << " is not equal to "
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

  case IsNotClass:
    out << getFirstType().getString() << " is not a class";
    break;

  case IsNotDynamicLookup:
    out << getFirstType().getString() << " is not an dynamic lookup value";
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

  case IsForbiddenLValue:
    out << "disallowed l-value binding of " << getFirstType().getString()
        << " and " << getSecondType().getString();
    break;
  }

  out << ")\n";
}


ConstraintLocator *
constraints::simplifyLocator(ConstraintSystem &cs,
                             ConstraintLocator *locator,
                             SourceRange &range1,
                             SourceRange &range2,
                             ConstraintLocator **targetLocator) {
  // Clear out the target locator result.
  if (targetLocator)
    *targetLocator = nullptr;

  // The path to be tacked on to the target locator to identify the specific
  // target.
  Expr *targetAnchor;
  SmallVector<LocatorPathElt, 4> targetPath;

  auto path = locator->getPath();
  auto anchor = locator->getAnchor();
  simplifyLocator(anchor, path, targetAnchor, targetPath, range1, range2);


  // If we have a target anchor, build and simplify the target locator.
  if (targetLocator && targetAnchor) {
    SourceRange targetRange1, targetRange2;
    *targetLocator = simplifyLocator(cs,
                                     cs.getConstraintLocator(targetAnchor,
                                                             targetPath),
                                     targetRange1, targetRange2);
  }

  // If we didn't simplify anything, just return the input.
  if (anchor == locator->getAnchor() &&
      path.size() == locator->getPath().size()) {
    return locator;
  }

  return cs.getConstraintLocator(anchor, path);
}

void constraints::simplifyLocator(Expr *&anchor,
                                  ArrayRef<LocatorPathElt> &path,
                                  Expr *&targetAnchor,
                                  SmallVectorImpl<LocatorPathElt> &targetPath,
                                  SourceRange &range1, SourceRange &range2) {
  range1 = SourceRange();
  range2 = SourceRange();
  targetAnchor = nullptr;

  while (!path.empty()) {
    switch (path[0].getKind()) {
    case ConstraintLocator::ApplyArgument:
      // Extract application argument.
      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        // The target anchor is the function being called.
        targetAnchor = applyExpr->getFn();
        targetPath.push_back(path[0]);

        anchor = applyExpr->getArg();
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::ApplyFunction:
      // Extract application function.
      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        // No additional target locator information.
        targetAnchor = nullptr;
        targetPath.clear();

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
        // Append this extraction to the target locator path.
        if (targetAnchor) {
          targetPath.push_back(path[0]);
        }

        anchor = tupleExpr->getElement(path[0].getValue());
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::MemberRefBase:
      if (auto dotExpr = dyn_cast<UnresolvedDotExpr>(anchor)) {
        // No additional target locator information.
        targetAnchor = nullptr;
        targetPath.clear();

        range1 = dotExpr->getNameLoc();
        anchor = dotExpr->getBase();
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::InterpolationArgument:
      if (auto interp = dyn_cast<InterpolatedStringLiteralExpr>(anchor)) {
        // No additional target locator information.
        // FIXME: Dig out the constructor we're trying to call?
        targetAnchor = nullptr;
        targetPath.clear();

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

/// Retrieve the argument pattern for the given declaration.
///
static Pattern *getParameterPattern(ValueDecl *decl) {
  if (auto func = dyn_cast<FuncDecl>(decl))
    return func->getArgParamPatterns()[0];
  if (auto constructor = dyn_cast<ConstructorDecl>(decl))
    return constructor->getArgParams();
  if (auto subscript = dyn_cast<SubscriptDecl>(decl))
    return subscript->getIndices();

  // FIXME: Variables of function type?
  return nullptr;
}

ResolvedLocator constraints::resolveLocatorToDecl(
                  ConstraintSystem &cs,
                  ConstraintLocator *locator,
                  std::function<Optional<OverloadChoice>(ConstraintLocator *)>
                    findOvlChoice) {
  assert(locator && "Null locator");
  if (!locator->getAnchor())
    return ResolvedLocator();

  ValueDecl *decl = nullptr;
  auto anchor = locator->getAnchor();
  // Unwrap any specializations, constructor calls, implicit conversions, and
  // '.'s.
  // FIXME: This is brittle.
  do {
    if (auto specialize = dyn_cast<UnresolvedSpecializeExpr>(anchor)) {
      anchor = specialize->getSubExpr();
      continue;
    }

    if (auto implicit = dyn_cast<ImplicitConversionExpr>(anchor)) {
      anchor = implicit->getSubExpr();
      continue;
    }

    if (auto constructor = dyn_cast<ConstructorRefCallExpr>(anchor)) {
      anchor = constructor->getFn();
      continue;
    }

    if (auto dotSyntax = dyn_cast<DotSyntaxBaseIgnoredExpr>(anchor)) {
      anchor = dotSyntax->getRHS();
      continue;
    }

    if (auto dotSyntax = dyn_cast<DotSyntaxCallExpr>(anchor)) {
      anchor = dotSyntax->getFn();
      continue;
    }

    break;
  } while (true);

  if (auto dre = dyn_cast<DeclRefExpr>(anchor)) {
    // Simple case: direct reference to a declaration.
    decl = dre->getDecl();
  } else if (auto mre = dyn_cast<MemberRefExpr>(anchor)) {
    // Simple case: direct reference to a declaration.
    decl = mre->getMember().getDecl();
  } else if (isa<OverloadedDeclRefExpr>(anchor) ||
             isa<OverloadedMemberRefExpr>(anchor) ||
             isa<UnresolvedDeclRefExpr>(anchor) ||
             isa<UnresolvedMemberExpr>(anchor)) {
    // Overloaded and unresolved cases: find the resolved overload.
    auto anchorLocator = cs.getConstraintLocator(anchor, { });
    if (auto choice = findOvlChoice(anchorLocator)) {
      if (choice->getKind() == OverloadChoiceKind::Decl)
        decl = choice->getDecl();
    }
  }

  // If we didn't find the declaration, we're out of luck.
  if (!decl)
    return ResolvedLocator();

  // Use the declaration and the path to produce a more specific result.
  // FIXME: This is an egregious hack. We'd be far better off
  // FIXME: Perform deeper path resolution?
  auto path = locator->getPath();
  Pattern *parameterPattern = nullptr;
  bool impliesFullPattern = false;
  while (!path.empty()) {
    switch (path[0].getKind()) {
    case ConstraintLocator::ApplyArgument:
      // If we're calling into something that has parameters, dig into the
      // actual parameter pattern.
      parameterPattern = getParameterPattern(decl);
      if (!parameterPattern)
        break;

      impliesFullPattern = true;
      path = path.slice(1);
      continue;

    case ConstraintLocator::TupleElement:
    case ConstraintLocator::NamedTupleElement:
      if (parameterPattern) {
        unsigned index = path[0].getValue();
        if (auto tuple = dyn_cast<TuplePattern>(
                           parameterPattern->getSemanticsProvidingPattern())) {
          parameterPattern = tuple->getFields()[index].getPattern();
          impliesFullPattern = false;
          path = path.slice(1);
          continue;
        }
        parameterPattern = nullptr;
      }
      break;

    case ConstraintLocator::ScalarToTuple:
      continue;

    default:
      break;
    }

    break;
  }

  // If we have a parameter pattern that refers to a parameter, grab it.
  if (parameterPattern) {
    parameterPattern = parameterPattern->getSemanticsProvidingPattern();
    if (impliesFullPattern) {
      if (auto tuple = dyn_cast<TuplePattern>(parameterPattern)) {
        if (tuple->getFields().size() == 1) {
          parameterPattern = tuple->getFields()[0].getPattern();
          parameterPattern = parameterPattern->getSemanticsProvidingPattern();
        }
      }
    }

    if (auto named = dyn_cast<NamedPattern>(parameterPattern)) {
      return ResolvedLocator(named->getDecl());
    }
  }

  // Otherwise, do the best we can with the declaration we found.
  if (auto func = dyn_cast<FuncDecl>(decl))
    return ResolvedLocator(func);
  if (auto constructor = dyn_cast<ConstructorDecl>(decl))
    return ResolvedLocator(constructor);

  // FIXME: Deal with the other interesting cases here, e.g.,
  // subscript declarations.
  return ResolvedLocator();
}

/// Emit a note referring to the target of a diagnostic, e.g., the function
/// or parameter being used.
static void noteTargetOfDiagnostic(ConstraintSystem &cs,
                                   const Failure &failure,
                                   ConstraintLocator *targetLocator) {
  // If there's no anchor, there's nothing we can do.
  if (!targetLocator->getAnchor())
    return;

  // Try to resolve the locator to a particular declaration.
  auto resolved
    = resolveLocatorToDecl(cs, targetLocator,
        [&](ConstraintLocator *locator) -> Optional<OverloadChoice> {
          for (auto resolved = failure.getResolvedOverloadSets();
               resolved; resolved = resolved->Previous) {
            if (resolved->Locator == locator)
              return resolved->Choice;
          }

          return Nothing;
        });

  // We couldn't resolve the locator to a declaration, so we're done.
  if (!resolved)
    return;

  switch (resolved.getKind()) {
  case ResolvedLocatorKind::Unresolved:
    // Can't emit any diagnostic here.
    return;

  case ResolvedLocatorKind::Function: {
    auto name = resolved.getDecl()->getName();
    cs.getTypeChecker().diagnose(resolved.getDecl(),
                                 name.isOperator()? diag::note_call_to_operator
                                                  : diag::note_call_to_func,
                                 resolved.getDecl()->getName());
    return;
  }

  case ResolvedLocatorKind::Constructor:
    // FIXME: Specialize for implicitly-generated constructors.
    cs.getTypeChecker().diagnose(resolved.getDecl(),
                                 diag::note_call_to_initializer);
    return;

  case ResolvedLocatorKind::Parameter:
    cs.getTypeChecker().diagnose(resolved.getDecl(), diag::note_init_parameter,
                                 resolved.getDecl()->getName());
    return;
  }
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

  ConstraintLocator *targetLocator;
  auto locator = simplifyLocator(cs, failure.getLocator(), range1, range2,
                                 &targetLocator);
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
    if (targetLocator)
      noteTargetOfDiagnostic(cs, failure, targetLocator);
    break;

  case Failure::DoesNotHaveMember:
    if (auto moduleTy = failure.getFirstType()->getAs<ModuleType>()) {
      tc.diagnose(loc, diag::no_member_of_module,
                  moduleTy->getModule()->Name,
                  failure.getName())
        .highlight(range1).highlight(range2);
    } else {
      tc.diagnose(loc, diag::does_not_have_member,
                  failure.getFirstType(),
                  failure.getName())
        .highlight(range1).highlight(range2);
    }
    break;

  case Failure::DoesNotConformToProtocol:
    // FIXME: Probably want to do this within the actual solver, because at
    // this point it's too late to actually recover fully.
    tc.conformsToProtocol(failure.getFirstType(),
                          failure.getSecondType()->castTo<ProtocolType>()
                            ->getDecl(),
                          cs.DC,
                          nullptr,
                          loc);
    if (targetLocator)
      noteTargetOfDiagnostic(cs, failure, targetLocator);
    break;

  case Failure::IsForbiddenLValue:
    if (auto lvalueTy = failure.getSecondType()->getAs<LValueType>()) {
      if (!lvalueTy->getQualifiers().isImplicit()) {
        tc.diagnose(loc, diag::reference_non_inout, lvalueTy->getObjectType())
          .highlight(range1).highlight(range2);
        return true;
      }
    }
    // FIXME: diagnose other cases
    return false;

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

  // Find the locators which have the largest numbers of distinct overloads.
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
      case OverloadChoiceKind::DeclViaDynamic:
      case OverloadChoiceKind::TypeDecl:
        // FIXME: show deduced types, etc, etc.
        tc.diagnose(choice.getDecl(), diag::found_candidate);
        break;

      case OverloadChoiceKind::BaseType:
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
    if (solutions.size() > 1) {
      if (getASTContext().LangOpts.DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log << "---Ambiguity error: "
            << solutions.size() << " solutions found---\n";
        int i = 0;
        for (auto &solution : solutions) {
          log << "---Ambiguous solution #" << i++ << "---\n";
          solution.dump(&TC.Context.SourceMgr, log);
          log << "\n";
        }
      }        

      if (diagnoseAmbiguity(*this, solutions)) {
        return true;
      }
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
