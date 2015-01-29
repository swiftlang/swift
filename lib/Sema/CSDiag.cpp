//===--- CSDiag.cpp - Constraint Diagnostics ------------------------------===//
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
        << getName() << "'";
    break;

  case DoesNotHaveNonMutatingMember:
    out << " immutable value of type " << getFirstType().getString()
        << " only has mutating members named '"
        << getName() << "'";
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

  case FunctionNoEscapeMismatch:
    out << "noescape attribute mismatch " << getFirstType().getString()
        << " vs. " << getSecondType().getString();
    break;
    
  case IsNotMetatype:
    out << getFirstType().getString() << " is not a metatype";
    break;

  case IsNotArchetype:
    out << getFirstType().getString() << " is not an archetype";
    break;

  case IsNotClass:
    out << getFirstType().getString() << " is not a class";
    break;
      
  case IsNotBridgedToObjectiveC:
    out << getFirstType().getString() << "is not bridged to Objective-C";
    break;

  case IsNotDynamicLookup:
    out << getFirstType().getString() << " is not a dynamic lookup value";
    break;
      
  case IsNotOptional:
    out << getFirstType().getString() << "is not an optional type";
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

  case TypesNotEqual:
    out << getFirstType().getString() << " is not equal to "
        << getSecondType().getString();
    break;

  case IsForbiddenLValue:
    out << "disallowed l-value binding of " << getFirstType().getString()
        << " and " << getSecondType().getString();
    break;

  case OutOfOrderArgument:
    out << "out-of-order argument " << getValue() << " should come before "
        << getSecondValue();
    break;

  case MissingArgument:
    out << "missing argument for parameter " << getValue();
    break;

  case ExtraArgument:
    out << "extra argument " << getValue();
    break;

  case NoPublicInitializers:
    out << getFirstType().getString()
        << " does not have any public initializers";
    break;

  case UnboundGenericParameter:
    out << getFirstType().getString()
        << " is an unbound generic parameter";
    break;

  case ExistentialGenericParameter:
    out << getFirstType().getString()
        << " bound to non-@objc existential type "
        << getSecondType().getString();
    break;
  }

  out << ")\n";
}

/// Given a subpath of an old locator, compute its summary flags.
static unsigned recomputeSummaryFlags(ConstraintLocator *oldLocator,
                                      ArrayRef<LocatorPathElt> path) {
  if (oldLocator->getSummaryFlags() != 0)
    return ConstraintLocator::getSummaryFlagsForPath(path);
  return 0;
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
    unsigned targetFlags = recomputeSummaryFlags(locator, targetPath);
    *targetLocator = simplifyLocator(cs,
                                     cs.getConstraintLocator(targetAnchor,
                                                             targetPath,
                                                             targetFlags),
                                     targetRange1, targetRange2);
  }

  // If we didn't simplify anything, just return the input.
  if (anchor == locator->getAnchor() &&
      path.size() == locator->getPath().size()) {
    return locator;
  }

  // Recompute the summary flags if we had any to begin with.  This is
  // necessary because we might remove e.g. tuple elements from the path.
  unsigned summaryFlags = recomputeSummaryFlags(locator, path);
  return cs.getConstraintLocator(anchor, path, summaryFlags);
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

      // The unresolved member itself is the function.
      if (auto unresolvedMember = dyn_cast<UnresolvedMemberExpr>(anchor)) {
        if (unresolvedMember->getArgument()) {
          // No additional target locator information.
          targetAnchor = nullptr;
          targetPath.clear();

          anchor = unresolvedMember;
          path = path.slice(1);
        }
        continue;
      }

      break;

    case ConstraintLocator::Load:
    case ConstraintLocator::RvalueAdjustment:
    case ConstraintLocator::ScalarToTuple:
      // Loads, rvalue adjustment, and scalar-to-tuple conversions are implicit.
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

    case ConstraintLocator::ApplyArgToParam:
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

      // Extract subexpression in parentheses.
      if (auto parenExpr = dyn_cast<ParenExpr>(anchor)) {
        assert(path[0].getValue() == 0);

        // Append this extraction to the target locator path.
        if (targetAnchor) {
          targetPath.push_back(path[0]);
        }

        anchor = parenExpr->getSubExpr();
        path = path.slice(1);
      }
      break;

    case ConstraintLocator::Member:
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

    case ConstraintLocator::AssignSource:
      if (auto assign = dyn_cast<AssignExpr>(anchor)) {
        targetAnchor = assign->getDest();
        targetPath.clear();

        anchor = assign->getSrc();
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::SubscriptIndex:
      if (auto subscript = dyn_cast<SubscriptExpr>(anchor)) {
        targetAnchor = subscript->getBase();
        targetPath.clear();

        anchor = subscript->getIndex();
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
    return func->getBodyParamPatterns()[0];
  if (auto constructor = dyn_cast<ConstructorDecl>(decl))
    return constructor->getBodyParamPatterns()[1];
  if (auto subscript = dyn_cast<SubscriptDecl>(decl))
    return subscript->getIndices();

  // FIXME: Variables of function type?
  return nullptr;
}

ResolvedLocator constraints::resolveLocatorToDecl(
   ConstraintSystem &cs,
   ConstraintLocator *locator,
   std::function<Optional<SelectedOverload>(ConstraintLocator *)> findOvlChoice,
   std::function<ConcreteDeclRef (ValueDecl *decl,
                                  Type openedType)> getConcreteDeclRef)
{
  assert(locator && "Null locator");
  if (!locator->getAnchor())
    return ResolvedLocator();

  ConcreteDeclRef declRef;
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
  
  auto getConcreteDeclRefFromOverload
    = [&](const SelectedOverload &selected) -> ConcreteDeclRef {
      return getConcreteDeclRef(selected.choice.getDecl(),
                                selected.openedType);
    };
  
  if (auto dre = dyn_cast<DeclRefExpr>(anchor)) {
    // Simple case: direct reference to a declaration.
    declRef = dre->getDeclRef();
  } else if (auto mre = dyn_cast<MemberRefExpr>(anchor)) {
    // Simple case: direct reference to a declaration.
    declRef = mre->getMember();
  } else if (isa<OverloadedDeclRefExpr>(anchor) ||
             isa<OverloadedMemberRefExpr>(anchor) ||
             isa<UnresolvedDeclRefExpr>(anchor)) {
    // Overloaded and unresolved cases: find the resolved overload.
    auto anchorLocator = cs.getConstraintLocator(anchor);
    if (auto selected = findOvlChoice(anchorLocator)) {
      if (selected->choice.isDecl())
        declRef = getConcreteDeclRefFromOverload(*selected);
    }
  } else if (isa<UnresolvedMemberExpr>(anchor)) {
    // Unresolved member: find the resolved overload.
    auto anchorLocator = cs.getConstraintLocator(
                           anchor,
                           ConstraintLocator::UnresolvedMember);
    if (auto selected = findOvlChoice(anchorLocator)) {
      if (selected->choice.isDecl())
        declRef = getConcreteDeclRefFromOverload(*selected);
    }
  } else if (auto ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(anchor)) {
    declRef = ctorRef->getDeclRef();
  }

  // If we didn't find the declaration, we're out of luck.
  if (!declRef)
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
      parameterPattern = getParameterPattern(declRef.getDecl());
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

    case ConstraintLocator::ApplyArgToParam:
      if (parameterPattern) {
        unsigned index = path[0].getValue2();
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
      return ResolvedLocator(ResolvedLocator::ForVar, named->getDecl());
    }
  }

  // Otherwise, do the best we can with the declaration we found.
  if (isa<FuncDecl>(declRef.getDecl()))
    return ResolvedLocator(ResolvedLocator::ForFunction, declRef);
  if (isa<ConstructorDecl>(declRef.getDecl()))
    return ResolvedLocator(ResolvedLocator::ForConstructor, declRef);

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
        [&](ConstraintLocator *locator) -> Optional<SelectedOverload> {
          for (auto resolved = failure.getResolvedOverloadSets();
               resolved; resolved = resolved->Previous) {
            if (resolved->Locator == locator)
              return SelectedOverload{resolved->Choice,
                                      resolved->OpenedFullType,
                                      // FIXME: opened type?
                                      Type()};
          }

          return None;
        },
        [&](ValueDecl *decl,
            Type openedType) -> ConcreteDeclRef {
          return decl;
        });

  // We couldn't resolve the locator to a declaration, so we're done.
  if (!resolved)
    return;

  switch (resolved.getKind()) {
  case ResolvedLocatorKind::Unresolved:
    // Can't emit any diagnostic here.
    return;

  case ResolvedLocatorKind::Function: {
    auto name = resolved.getDecl().getDecl()->getName();
    cs.getTypeChecker().diagnose(resolved.getDecl().getDecl(),
                                 name.isOperator()? diag::note_call_to_operator
                                                  : diag::note_call_to_func,
                                 resolved.getDecl().getDecl()->getName());
    return;
  }

  case ResolvedLocatorKind::Constructor:
    // FIXME: Specialize for implicitly-generated constructors.
    cs.getTypeChecker().diagnose(resolved.getDecl().getDecl(),
                                 diag::note_call_to_initializer);
    return;

  case ResolvedLocatorKind::Parameter:
    cs.getTypeChecker().diagnose(resolved.getDecl().getDecl(),
                                 diag::note_init_parameter,
                                 resolved.getDecl().getDecl()->getName());
    return;
  }
}

/// \brief Emit a diagnostic for the given failure.
///
/// \param cs The constraint system in which the diagnostic was generated.
/// \param failure The failure to emit.
/// \param expr The expression associated with the failure.
/// \param useExprLoc If the failure lacks a location, use the one associated
/// with expr.
///
/// \returns true if the diagnostic was emitted successfully.
static bool diagnoseFailure(ConstraintSystem &cs,
                            Failure &failure,
                            Expr *expr,
                            bool useExprLoc) {
  ConstraintLocator *cloc;
  if (!failure.getLocator() || !failure.getLocator()->getAnchor()) {
    if (useExprLoc)
      cloc = cs.getConstraintLocator(expr);
    else
      return false;
  } else {
    cloc = failure.getLocator();
  }
  
  SourceRange range1, range2;

  ConstraintLocator *targetLocator;
  auto locator = simplifyLocator(cs, cloc, range1, range2,
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

  case Failure::TypesNotConvertible:
  case Failure::TypesNotEqual:
  case Failure::TypesNotSubtypes:
  case Failure::TypesNotConstructible:
  case Failure::FunctionTypesMismatch: {
    
    // For conversion failures in 'as' and 'as!' expressions, diagnose that
    // expression directly.
    if ((isa<CoerceExpr>(cloc->getAnchor()) ||
         isa<ForcedCheckedCastExpr>(cloc->getAnchor())) &&
        failure.getKind() == Failure::TypesNotConvertible) {
      if (cs.diagnoseFailureForExpr(cloc->getAnchor())) {
        return true;
      }
    }

    // We can do a better job of diagnosing application argument conversion
    // failures elsewhere.
    if (isa<ApplyExpr>(expr) ||
        isa<InOutExpr>(expr) ||
        isa<AssignExpr>(expr))
      return false;

    tc.diagnose(loc, diag::invalid_relation,
                failure.getKind() - Failure::TypesNotEqual,
                failure.getFirstType(),
                failure.getSecondType())
      .highlight(range1).highlight(range2);
    if (targetLocator && !useExprLoc)
      noteTargetOfDiagnostic(cs, failure, targetLocator);
    }
    break;

  case Failure::DoesNotHaveMember:
  case Failure::DoesNotHaveNonMutatingMember:
    if (auto moduleTy = failure.getFirstType()->getAs<ModuleType>()) {
      tc.diagnose(loc, diag::no_member_of_module,
                  moduleTy->getModule()->Name,
                  failure.getName())
        .highlight(range1).highlight(range2);
    } else {
      bool IsNoMember = failure.getKind() == Failure::DoesNotHaveMember;

      tc.diagnose(loc, IsNoMember ? diag::does_not_have_member :
                                    diag::does_not_have_non_mutating_member,
                  failure.getFirstType(),
                  failure.getName())
        .highlight(range1).highlight(range2);
    }
    break;

  case Failure::DoesNotConformToProtocol:
    // FIXME: Probably want to do this within the actual solver, because at
    // this point it's too late to actually recover fully.
      
    // We can do a better job of diagnosing application argument conversion
    // failures elsewhere.
    if (isa<ApplyExpr>(expr) ||
        isa<InOutExpr>(expr) ||
        isa<AssignExpr>(expr))
      return false;
      
    tc.conformsToProtocol(failure.getFirstType(),
                          failure.getSecondType()->castTo<ProtocolType>()
                            ->getDecl(),
                          cs.DC,
                          true,
                          nullptr,
                          loc);
    if (targetLocator)
      noteTargetOfDiagnostic(cs, failure, targetLocator);
    break;

  case Failure::IsNotBridgedToObjectiveC:
    tc.diagnose(loc, diag::type_not_bridged, failure.getFirstType());
    if (targetLocator)
      noteTargetOfDiagnostic(cs, failure, targetLocator);
    break;

  case Failure::IsForbiddenLValue:
    if (auto iotTy = failure.getSecondType()->getAs<InOutType>()) {
      tc.diagnose(loc, diag::reference_non_inout, iotTy->getObjectType())
        .highlight(range1).highlight(range2);
      return true;
    }
    // FIXME: diagnose other cases
    return false;

  case Failure::OutOfOrderArgument: 
    if (auto tuple = dyn_cast_or_null<TupleExpr>(anchor)) {
      unsigned firstIdx = failure.getValue();
      Identifier first = tuple->getElementName(firstIdx);
      unsigned secondIdx = failure.getSecondValue();
      Identifier second = tuple->getElementName(secondIdx);
      if (!first.empty()  && !second.empty()) {
        tc.diagnose(tuple->getElementNameLoc(firstIdx),
                    diag::argument_out_of_order, first, second)
          .highlight(tuple->getElement(firstIdx)->getSourceRange())
          .highlight(SourceRange(tuple->getElementNameLoc(secondIdx),
                                 tuple->getElement(secondIdx)->getEndLoc()));
        return true;
      }
    }
    // FIXME: Can this even happen?
    return false;

  case Failure::MissingArgument: {
    Identifier name;
    unsigned idx = failure.getValue();
    if (auto tupleTy = failure.getFirstType()->getAs<TupleType>()) {
      name = tupleTy->getFields()[idx].getName();
    } else {
      // Scalar.
      assert(idx == 0);
    }

    if (name.empty())
      tc.diagnose(loc, diag::missing_argument_positional, idx+1);
    else
      tc.diagnose(loc, diag::missing_argument_named, name);
    return true;
  }
    
  case Failure::ExtraArgument: {
    if (auto tuple = dyn_cast_or_null<TupleExpr>(anchor)) {
      unsigned firstIdx = failure.getValue();
      auto name = tuple->getElementName(firstIdx);
      if (name.empty())
        tc.diagnose(loc, diag::extra_argument_positional)
          .highlight(tuple->getElement(firstIdx)->getSourceRange());
      else
        tc.diagnose(loc, diag::extra_argument_named, name)
          .highlight(tuple->getElement(firstIdx)->getSourceRange());        
      return true;
    }

    return false;
  }
      
  case Failure::IsNotOptional: {
    if (auto force = dyn_cast_or_null<ForceValueExpr>(anchor)) {
      // If there was an 'as' cast in the subexpression, note it.
      if (auto cast = findForcedDowncast(tc.Context, force->getSubExpr())) {
        tc.diagnose(force->getLoc(), diag::forcing_explicit_downcast,
                    failure.getFirstType())
          .highlight(cast->getLoc())
          .fixItRemove(force->getLoc());
        return true;
      }
      
      tc.diagnose(loc, diag::forcing_injected_optional,
                  failure.getFirstType())
        .highlight(force->getSourceRange())
        .fixItRemove(force->getExclaimLoc());
      
      return true;
    }
    
    if (auto bind = dyn_cast_or_null<BindOptionalExpr>(anchor)) {
      tc.diagnose(loc, diag::binding_injected_optional,
                  failure.getFirstType())
        .highlight(bind->getSourceRange())
        .fixItRemove(bind->getQuestionLoc());
      
      return true;      
    }
    return false;
  }

  case Failure::NoPublicInitializers: {
    tc.diagnose(loc, diag::no_accessible_initializers, failure.getFirstType())
      .highlight(range1);
    if (targetLocator && !useExprLoc)
      noteTargetOfDiagnostic(cs, failure, targetLocator);
    break;
  }

  case Failure::UnboundGenericParameter: {
    tc.diagnose(loc, diag::unbound_generic_parameter, failure.getFirstType())
      .highlight(range1);
    if (!useExprLoc)
      noteTargetOfDiagnostic(cs, failure, locator);
    break;
  }

  case Failure::ExistentialGenericParameter: {
    tc.diagnose(loc, diag::generic_parameter_binds_to_non_objc_existential,
                failure.getFirstType(), failure.getSecondType())
      .highlight(range1);
    if (!useExprLoc)
      noteTargetOfDiagnostic(cs, failure, locator);
    break;
  }

  case Failure::FunctionNoEscapeMismatch: {
    tc.diagnose(loc, diag::noescape_functiontype_mismatch,
                failure.getSecondType()).highlight(range2);
    if (!useExprLoc)
      noteTargetOfDiagnostic(cs, failure, locator);
    break;
  }

  case Failure::FunctionAutoclosureMismatch:
  case Failure::FunctionNoReturnMismatch:
  case Failure::IsNotArchetype:
  case Failure::IsNotClass:
  case Failure::IsNotDynamicLookup:
  case Failure::IsNotMetatype:
  case Failure::TupleNameMismatch:
  case Failure::TupleNamePositionMismatch:
  case Failure::TupleVariadicMismatch:
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
    if (uniqueChoices.insert(choice.getOpaqueChoiceSimple()).second)
      ++result;
  }
  return result;
}

/// \brief Determine the name of the overload in a set of overload choices.
static Identifier getOverloadChoiceName(ArrayRef<OverloadChoice> choices) {
  for (auto choice : choices) {
    if (choice.isDecl())
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

    // Emit candidates.  Use a SmallPtrSet to make sure only emit a particular
    // candidate once.  FIXME: Why is one candidate getting into the overload
    // set multiple times?
    SmallPtrSet<Decl*, 8> EmittedDecls;
    for (auto choice : overload.choices) {
      switch (choice.getKind()) {
      case OverloadChoiceKind::Decl:
      case OverloadChoiceKind::DeclViaDynamic:
      case OverloadChoiceKind::TypeDecl:
      case OverloadChoiceKind::DeclViaBridge:
      case OverloadChoiceKind::DeclViaUnwrappedOptional:
        // FIXME: show deduced types, etc, etc.
        if (EmittedDecls.insert(choice.getDecl()).second)
          tc.diagnose(choice.getDecl(), diag::found_candidate);
        break;

      case OverloadChoiceKind::BaseType:
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

Constraint *getConstraintChoice(Constraint *constraint,
                                 ConstraintKind kind,
                                 bool takeAny = false) {
  if ((constraint->getKind() != ConstraintKind::Disjunction) &&
      (constraint->getKind() != ConstraintKind::Conjunction)) {
    return nullptr;
  }
  
  auto nestedConstraints = constraint->getNestedConstraints();
  
  for (auto nestedConstraint : nestedConstraints) {
    if (takeAny ||
        (nestedConstraint->getKind() == kind)) {
      
      // If this is a last-chance search, and we have a conjunction or
      // disjunction, look within.
      if (takeAny &&
          ((nestedConstraint->getKind() == ConstraintKind::Disjunction) ||
           (nestedConstraint->getKind() == ConstraintKind::Conjunction))) {
            return getConstraintChoice(nestedConstraint,
                                       kind,
                                       takeAny);
          }
      
      return nestedConstraint;
    }
  }
  
  return nullptr;
}

Constraint *getComponentConstraint(Constraint *constraint) {
  if (constraint->getKind() != ConstraintKind::Disjunction) {
    return constraint;
  }
  
  return constraint->getNestedConstraints().front();
}

/// For a given expression type, extract the appropriate type for a constraint-
/// based diagnostic.
static Type getDiagnosticTypeFromExpr(Expr *expr) {
  
  // For a forced checked cast expression or coerce expression, use the type of
  // the sub-expression.
  if (auto fcc = dyn_cast<ForcedCheckedCastExpr>(expr)) {
    auto subExpr = fcc->getSubExpr();
    return subExpr->getType();
  }
  if (auto coerceExpr = dyn_cast<CoerceExpr>(expr)) {
    auto subExpr = coerceExpr->getSubExpr();
    return subExpr->getType();
  }
  
  // For an application expression, use the argument type.
  if (auto applyExpr = dyn_cast<ApplyExpr>(expr)) {
    return applyExpr->getArg()->getType();
  }
  
  // For a subscript expression, use the index type.
  if (auto subscriptExpr = dyn_cast<SubscriptExpr>(expr)) {
    return subscriptExpr->getIndex()->getType();
  }
  
  return expr->getType();
}

/// If a type variable was created for an opened literal expression, substitute
/// in the default literal for the type variable's literal conformance.
static Type substituteLiteralForTypeVariable(ConstraintSystem *CS,
                                             TypeVariableType *tv) {
  if (auto proto = tv->getImpl().literalConformanceProto) {
    
    auto kind = proto->getKnownProtocolKind();
    
    if (kind.hasValue()) {
      auto altLits = CS->getAlternativeLiteralTypes(kind.getValue());
      if (!altLits.empty()) {
        if (auto altType = altLits[0]) {
          return altType;
        }
      }
    }
  }
  
  return tv;
}

static std::pair<Type, Type> getBoundTypesFromConstraint(ConstraintSystem *CS,
                                                         Expr *expr,
                                                         Constraint *constraint) {
  auto type1 = getDiagnosticTypeFromExpr(expr);
  auto type2 = constraint->getSecondType();
  
  if (type1->isEqual(type2))
    if (auto firstType = constraint->getFirstType())
      type1 = firstType;
  
  if (auto typeVariableType =
      dyn_cast<TypeVariableType>(type2.getPointer())) {
    SmallVector<Type, 4> bindings;
    CS->getComputedBindings(typeVariableType, bindings);
    auto binding = bindings.size() ? bindings.front() : Type();
    
    if (!binding.isNull()) {
      if (binding.getPointer() != type1.getPointer())
        type2 = binding;
    } else {
      auto impl = typeVariableType->getImpl();
      if (auto archetypeType = impl.getArchetype()) {
        type2 = archetypeType;
      } else {
        auto implAnchor = impl.getLocator()->getAnchor();
        auto anchorType = implAnchor->getType();
        
        // Don't re-substitute an opened type variable for itself.
        if (anchorType.getPointer() != type1.getPointer())
          type2 = anchorType;
      }
    }
  }
  
  if (auto typeVariableType =
      dyn_cast<TypeVariableType>(type1.getPointer())) {
    SmallVector<Type, 4> bindings;
    
    CS->getComputedBindings(typeVariableType, bindings);
    
    for (auto binding : bindings) {
      if (type2.getPointer() != binding.getPointer()) {
        type1 = binding;
        break;
      }
    }
  }
  
  // If we still have a literal type variable, substitute in the default type.
  if (auto tv1 = type1->getAs<TypeVariableType>()) {
    type1 = substituteLiteralForTypeVariable(CS, tv1);
  }
  if (auto tv2 = type2->getAs<TypeVariableType>()) {
    type2 = substituteLiteralForTypeVariable(CS, tv2);
  }
  
  return std::pair<Type, Type>(type1->getLValueOrInOutObjectType(),
                               type2->getLValueOrInOutObjectType());
}

/// Determine if a type resulting from a failed typecheck operation is fully-
/// specialized, or if it still has type variable type arguments.
/// (This diverges slightly from hasTypeVariable, in that certain tyvars,
/// such as for nil literals, will be treated as specialized.)
bool typeIsNotSpecialized(Type type) {
  if (type.isNull())
    return true;
  
  if (auto tv = type->getAs<TypeVariableType>()) {
    
      // If it's a nil-literal conformance, there's no reason to re-specialize.
      if (tv->getImpl().literalConformanceProto) {
      
        auto knownProtoKind =
            tv->getImpl().literalConformanceProto->getKnownProtocolKind();
        
        if (knownProtoKind.hasValue() &&
            (knownProtoKind.getValue() ==
                KnownProtocolKind::NilLiteralConvertible)) {
          return false;
      }
    }
    
    return true;
  }
  
  // Desugar, if necessary.
  if (auto sugaredTy = type->getAs<SyntaxSugarType>())
    type = sugaredTy->getBaseType();
  
  // If it's generic, check the type arguments.
  if (auto bgt = type->getAs<BoundGenericType>()) {
    for (auto tyarg : bgt->getGenericArgs()) {
      if (typeIsNotSpecialized(tyarg))
        return true;
    }
    
    return false;
  }
  
  // If it's a tuple, check the members.
  if (auto tupleTy = type->getAs<TupleType>()) {
    for (auto elementTy : tupleTy->getElementTypes()) {
      if (typeIsNotSpecialized((elementTy)))
        return true;
    }
    
    return false;
  }
  
  // If it's an inout type, check the inner type.
  if (auto inoutTy = type->getAs<InOutType>()) {
    return typeIsNotSpecialized(inoutTy->getObjectType());
  }
  
  // If it's a function, check the parameter and return types.
  if (auto functionTy = type->getAs<AnyFunctionType>()) {
    if (typeIsNotSpecialized(functionTy->getResult()) ||
        typeIsNotSpecialized(functionTy->getInput()))
      return true;
    
    return false;
  }
  
  // Otherwise, broadly check for type variables.
  return type->hasTypeVariable();
}

/// Obtain the colloquial description for a known protocol kind.
std::string getDescriptionForKnownProtocolKind(KnownProtocolKind kind) {
  switch (kind) {
#define PROTOCOL(Id) \
case KnownProtocolKind::Id: \
return #Id;
      
#define LITERAL_CONVERTIBLE_PROTOCOL(Id, Description) \
case KnownProtocolKind::Id: \
return #Description;
      
#define BUILTIN_LITERAL_CONVERTIBLE_PROTOCOL(Id) \
case KnownProtocolKind::Id: \
return #Id;
      
#include "swift/AST/KnownProtocols.def"
  }
  
  llvm_unreachable("unrecognized known protocol kind");
}

/// Determine if the type is an error type, or its metatype.
bool isErrorTypeKind(Type t) {
  
  if (auto mt = t->getAs<MetatypeType>())
    t = mt->getInstanceType();
  
  return t->is<ErrorType>();
}

/// Obtain a "user friendly" type name. E.g., one that uses colloquial names
/// for literal convertible protocols if necessary, and is devoid of type
/// variables.
std::string getUserFriendlyTypeName(Type t, bool unwrap = true) {
  
  assert(!t.isNull());
  
  // Unwrap any l-value types.
  if (unwrap) {
    if (auto lv = t->getAs<LValueType>()) {
      t = lv->getObjectType();
    }
  }
  
  if (auto tv = t->getAs<TypeVariableType>()) {
    if (tv->getImpl().literalConformanceProto) {
      Optional<KnownProtocolKind> kind =
          tv->getImpl().literalConformanceProto->getKnownProtocolKind();
      
      if (kind.hasValue()) {
        return getDescriptionForKnownProtocolKind(kind.getValue());
      }
    }
  }
  
  return t.getString();
}

/// Conveniently unwrap a paren expression, if necessary.
Expr* unwrapParenExpr(Expr *e) {
  if (auto parenExpr = dyn_cast<ParenExpr>(e))
    return unwrapParenExpr(parenExpr->getSubExpr());
  
  return e;
}

/// Given a vector of names and types, obtain a stringified comma-separated
/// list of the names (if present) and their associated "user friendly" type
/// names.
std::string getTypeListString(SmallVectorImpl<Identifier> &names,
                              SmallVectorImpl<Type> &types) {
  
  std::string typeList = "";
  
  if (types.size()) {
    if (!names[0].empty()) {
      typeList += names[0].get();
      typeList += ": ";
    }
    typeList += getUserFriendlyTypeName(types[0]);
    
    for (size_t i = 1; i < types.size(); i++) {
      typeList += ", ";
      if (!names[i].empty()) {
        typeList += names[i].get();
        typeList += ": ";
      }
      typeList += getUserFriendlyTypeName(types[i]);
    }
  }
  
  return typeList;
  
}

GeneralFailureDiagnosis::GeneralFailureDiagnosis(Expr *expr,
                                                 ConstraintSystem *cs) :
expr(expr), CS(cs) {
  
  assert(expr && CS);
  
  // Collect and categorize constraint information from the failed system.
  
  if(!CS->ActiveConstraints.empty()) {
    // If any active conformance constraints are in the system, we know that
    // any inactive constraints are in its service. Capture the constraint and
    // present this information to the user.
    auto *constraint = &CS->ActiveConstraints.front();
    
    activeConformanceConstraint = getComponentConstraint(constraint);
  }
  
  for (auto & constraintRef : CS->InactiveConstraints) {
    auto constraint = &constraintRef;
    
    // Capture the first non-disjunction constraint we find. We'll use this
    // if we can't find a clearer reason for the failure.
    if ((!fallbackConstraint || constraint->isFavored()) &&
        (constraint->getKind() != ConstraintKind::Disjunction) &&
        (constraint->getKind() != ConstraintKind::Conjunction)) {
      fallbackConstraint = constraint;
    }
    
    // Store off conversion constraints, favoring existing conversion
    // constraints.
    if ((!(activeConformanceConstraint ||
           conformanceConstraint) || constraint->isFavored()) &&
        constraint->getKind() == ConstraintKind::ConformsTo) {
      conformanceConstraint = constraint;
    }
    
    // Failed binding constraints point to a missing member.
    if ((!valueMemberConstraint || constraint->isFavored()) &&
        ((constraint->getKind() == ConstraintKind::ValueMember) ||
         (constraint->getKind() == ConstraintKind::UnresolvedValueMember))) {
          valueMemberConstraint = constraint;
        }
    
    // A missed argument conversion can result in better error messages when
    // a user passes the wrong arguments to a function application.
    if ((!argumentConstraint || constraint->isFavored())) {
      argumentConstraint = getConstraintChoice(constraint,
                                               ConstraintKind::
                                               ArgumentTupleConversion);
    }
    
    // Overload resolution failures are often nicely descriptive, so store
    // off the first one we find.
    if ((!overloadConstraint || constraint->isFavored())) {
      overloadConstraint = getConstraintChoice(constraint,
                                               ConstraintKind::BindOverload);
    }
    
    // Conversion constraints are also nicely descriptive, so we'll grab the
    // first one of those as well.
    if ((!conversionConstraint || constraint->isFavored()) &&
        (constraint->getKind() == ConstraintKind::Conversion ||
         constraint->getKind() == ConstraintKind::ExplicitConversion ||
         constraint->getKind() == ConstraintKind::ArgumentTupleConversion)) {
          conversionConstraint = constraint;
        }
    
    // Also check for bridging failures.
    if ((!bridgeToObjCConstraint || constraint->isFavored()) &&
        constraint->getKind() == ConstraintKind::BridgedToObjectiveC) {
      bridgeToObjCConstraint = constraint;
    }
    
    // When all else fails, inspect a potential conjunction or disjunction for a
    // consituent conversion.
    if (!disjunctionConversionConstraint || constraint->isFavored()) {
      disjunctionConversionConstraint =
      getConstraintChoice(constraint, ConstraintKind::Conversion, true);
    }
  }
  
  // If no more descriptive constraint was found, use the fallback constraint.
  if (fallbackConstraint &&
      !(conversionConstraint ||
        overloadConstraint ||
        argumentConstraint)) {
        
        if (fallbackConstraint->getKind() == ConstraintKind::ArgumentConversion)
          argumentConstraint = fallbackConstraint;
        else
          conversionConstraint = fallbackConstraint;
      }
  
  // If there's still no conversion to diagnose, use the disjunction conversion.
  if (!conversionConstraint) {
    conversionConstraint = disjunctionConversionConstraint;
  }
  
  // If there was already a conversion failure, use it.
  if (!conversionConstraint &&
      CS->failedConstraint &&
      CS->failedConstraint->getKind() != ConstraintKind::Disjunction) {
    conversionConstraint = CS->failedConstraint;
  }
}

bool GeneralFailureDiagnosis::diagnoseGeneralValueMemberFailure() {
  
  if (valueMemberConstraint) {
    auto memberName = valueMemberConstraint->getMember().getBaseName();
    CS->TC.diagnose(expr->getLoc(),
                    diag::could_not_find_member,
                    memberName)
    .highlight(expr->getSourceRange());
    
    return true;
  }
  
  return false;
}

bool GeneralFailureDiagnosis::diagnoseGeneralOverloadFailure() {
  
  // In the absense of a better conversion constraint failure, point out the
  // inability to find an appropriate overload.
  if (overloadConstraint) {
    auto overloadChoice = overloadConstraint->getOverloadChoice();
    auto overloadName = overloadChoice.getDecl()->getName();
    Type argType = getDiagnosticTypeFromExpr(expr);
    
    if (!argType.isNull() &&
        !argType->getAs<TypeVariableType>() &&
        isa<ApplyExpr>(expr)) {
      if (argType->getAs<TupleType>()) {
        CS->TC.diagnose(expr->getLoc(),
                        diag::cannot_find_appropriate_overload_with_type_list,
                        overloadName.str(), argType)
        .highlight(expr->getSourceRange());
      } else {
        CS->TC.diagnose(expr->getLoc(),
                        diag::cannot_find_appropriate_overload_with_type,
                        overloadName.str(),
                        argType)
        .highlight(expr->getSourceRange());
      }
    } else {
      CS->TC.diagnose(expr->getLoc(),
                      diag::cannot_find_appropriate_overload,
                      overloadName.str())
      .highlight(expr->getSourceRange());
    }
    return true;
  }
  
  return false;
}

bool GeneralFailureDiagnosis::diagnoseGeneralConversionFailure() {
  
  // Otherwise, if we have a conversion constraint, use that as the basis for
  // the diagnostic.
  if (conversionConstraint || argumentConstraint) {
    auto constraint = argumentConstraint ?
    argumentConstraint :
    conversionConstraint;
    
    if (conformanceConstraint) {
      if (conformanceConstraint->getTypeVariables().size() <
          constraint->getTypeVariables().size()) {
        constraint = conformanceConstraint;
      }
    }
    
    auto locator = constraint->getLocator();
    auto anchor = locator ? locator->getAnchor() : expr;
    std::pair<Type, Type> types = getBoundTypesFromConstraint(CS,
                                                              expr,
                                                              constraint);
    
    if (argumentConstraint) {
      CS->TC.diagnose(expr->getLoc(),
                      diag::could_not_convert_argument,
                      types.first).
      highlight(anchor->getSourceRange());
    } else {
      
      // If it's a type variable failing a conformance, avoid printing the type
      // variable and just print the conformance.
      if ((constraint->getKind() == ConstraintKind::ConformsTo) &&
          types.first->getAs<TypeVariableType>()) {
        CS->TC.diagnose(anchor->getLoc(),
                        diag::single_expression_conformance_failure,
                        types.first)
        .highlight(anchor->getSourceRange());
      } else {
        // If the second type is a type variable, the expression itself is
        // ambiguous.
        if (types.first->getAs<UnboundGenericType>() ||
            types.second->getAs<TypeVariableType>()) {
          if (isa<ClosureExpr>(expr)) {
            CS->TC.diagnose(expr->getLoc(),
                            diag::cannot_infer_closure_type);
          } else {
            CS->TC.diagnose(expr->getLoc(),
                            diag::type_of_expression_is_ambiguous);
          }
        } else {
          auto failureKind =
          Failure::TypesNotConvertible - Failure::TypesNotEqual;
          
          CS->TC.diagnose(anchor->getLoc(),
                          diag::invalid_relation,
                          failureKind,
                          types.first, types.second)
          .highlight(anchor->getSourceRange());
        }
      }
    }
    
    return true;
  }
  
  return false;
}

bool GeneralFailureDiagnosis::diagnoseGeneralFailure() {
  
  return diagnoseGeneralValueMemberFailure() ||
         diagnoseGeneralOverloadFailure() ||
         diagnoseGeneralConversionFailure();
}

Type FailureDiagnosis::getTypeOfIndependentSubExpression(Expr *subExpr) {
  
  if (!isa<ClosureExpr>(subExpr) &&
      typeIsNotSpecialized(subExpr->getType())) {
    
    // Store off the sub-expression, in case a new one is provided via the
    // type check operation.
    Expr *preCheckedExpr = subExpr;
    
    CS->TC.eraseTypeData(subExpr);

    CS->TC.typeCheckExpression(subExpr, CS->DC, Type(), Type(), false);
 
    // Reset the type of the previous expression. This prevents stale type
    // variable data from being leaked out of the temporary constraint system.
    if (preCheckedExpr != subExpr)
      preCheckedExpr->setType(subExpr->getType());
  }
  
  assert(subExpr->getType());
  
  return subExpr->getType();
}

/// FIXME: Right now, a "matching" overload is one with a parameter whose type
/// is identical to one of the argument types. We can obviously do something
/// more sophisticated with this.
void FailureDiagnosis::suggestPotentialOverloads(
                               const StringRef functionName,
                               const SourceLoc &loc,
                               const SmallVectorImpl<Type> &paramLists,
                               const SmallVectorImpl<Type> &argTypes) {
  if (!argTypes.size()) {
    return;
  }
  
  std::string suggestionText = "";
  std::map<std::string, bool> dupes;

  for (auto paramList : paramLists) {
    SmallVector<Identifier, 16> paramNames;
    SmallVector<Type, 16> paramTypes;
    
    // Assemble the parameter type list.
    if (auto parenType = dyn_cast<ParenType>(paramList.getPointer())) {
      paramNames.push_back(Identifier());
      paramTypes.push_back(parenType->getUnderlyingType());
    } else if (auto tupleType = paramList->getAs<TupleType>()) {
      for (auto field : tupleType->getFields()) {
        paramNames.push_back(field.getName());
        paramTypes.push_back(field.getType());
      }
    }
    
    if (paramTypes.size() != argTypes.size())
      continue;
    
    for (size_t i = 0; i < paramTypes.size(); i++) {
      auto pt = paramTypes[i];
      auto at = argTypes[i];
    
      if (pt->isEqual(at)) {
        auto typeListString = getTypeListString(paramNames, paramTypes);
        if (!dupes[typeListString]) {
          dupes[typeListString] = true;
          if (suggestionText.length())
            suggestionText += ", ";
          suggestionText += "(" + typeListString + ")";
        }
        break;
      }
    }
  }
  
  if (!suggestionText.length())
    return;
  
  CS->TC.diagnose(loc,
                  diag::suggest_partial_overloads,
                  functionName,
                  suggestionText);
}

bool FailureDiagnosis::diagnoseFailureForBinaryExpr() {
  assert(expr->getKind() == ExprKind::Binary);
  
  CleanupIllFormedExpressionRAII cleanup(*CS, expr);
  
  auto binop = cast<BinaryExpr>(expr);
  
  auto argExpr = dyn_cast<TupleExpr>(binop->getArg());
  auto argTuple = getTypeOfIndependentSubExpression(argExpr)->
  getAs<TupleType>();
  
  // If the argument type is not a tuple, we've posted the diagnostic
  // recursively.
  if (!argTuple)
    return true;
  
  std::string overloadName;
  
  SmallVector<Type, 16> paramLists;
  SmallVector<Type, 2> argTypes;
  
  if (auto DRE = dyn_cast<DeclRefExpr>(binop->getFn())) {
    overloadName = DRE->getDecl()->getNameStr();
  } else if (auto ODRE = dyn_cast<OverloadedDeclRefExpr>(binop->getFn())) {
    overloadName = ODRE->getDecls()[0]->getNameStr();
    
    for (auto DRE : ODRE->getDecls()) {
      if (auto fnType = DRE->getType()->getAs<AnyFunctionType>()) {
        paramLists.push_back(fnType->getInput());
      }
    }
  } else if (overloadConstraint) {
    overloadName = overloadConstraint->getOverloadChoice().
    getDecl()->getName().str();
  } else {
    llvm_unreachable("unrecognized binop function kind");
  }
  
  assert(!overloadName.empty());
  
  argTypes.push_back(argTuple->getElementType(0));
  argTypes.push_back(argTuple->getElementType(1));
  
  auto argTyName1 = getUserFriendlyTypeName(argTypes[0]);
  auto argTyName2 = getUserFriendlyTypeName(argTypes[1]);
  
  if (argTyName1.compare(argTyName2)) {
    CS->TC.diagnose(argExpr->getElements()[0]->getLoc(),
                    diag::cannot_apply_binop_to_args,
                    overloadName,
                    argTyName1,
                    argTyName2);
  } else {
    CS->TC.diagnose(argExpr->getElements()[0]->getLoc(),
                    diag::cannot_apply_binop_to_same_args,
                    overloadName,
                    argTyName1);
  }
  
  if (paramLists.size())
    suggestPotentialOverloads(overloadName,
                              argExpr->getElements()[0]->getLoc(),
                              paramLists,
                              argTypes);
  
  return true;
}
  
bool FailureDiagnosis::diagnoseFailureForUnaryExpr() {
  assert(expr->getKind() == ExprKind::PostfixUnary ||
         expr->getKind() == ExprKind::PrefixUnary);
  
  CleanupIllFormedExpressionRAII cleanup(*CS, expr);
  
  auto applyExpr = cast<ApplyExpr>(expr);
  auto argExpr = applyExpr->getArg();
  auto argType = getTypeOfIndependentSubExpression(argExpr);
  
  // If the argument type is an error, we've posted the diagnostic
  // recursively.
  if (isErrorTypeKind(argType))
    return true;
  
  auto argTyName = getUserFriendlyTypeName(argType);
  
  std::string overloadName;
  
  if (overloadConstraint) {
    overloadName = overloadConstraint->getOverloadChoice().
    getDecl()->getName().str();
  } else if (auto declRefExpr = dyn_cast<DeclRefExpr>(applyExpr->getFn())) {
    overloadName = declRefExpr->getDecl()->getNameStr();
  } else if (auto overloadedDRE =
             dyn_cast<OverloadedDeclRefExpr>(applyExpr->getFn())) {
    overloadName = overloadedDRE->getDecls()[0]->getNameStr();
  } else {
    llvm_unreachable("unrecognized unop function kind");
  }
  
  assert(!overloadName.empty());
  
  // FIXME: Note that we don't currently suggest a partially matching overload.
  CS->TC.diagnose(argExpr->getLoc(),
                  diag::cannot_apply_unop_to_arg,
                  overloadName,
                  argTyName);
  
  return true;
}

bool FailureDiagnosis::diagnoseFailureForPrefixUnaryExpr() {
  return diagnoseFailureForUnaryExpr();
}

bool FailureDiagnosis::diagnoseFailureForPostfixUnaryExpr() {
  return diagnoseFailureForUnaryExpr();
}

bool FailureDiagnosis::diagnoseFailureForSubscriptExpr() {
  assert(expr->getKind() == ExprKind::Subscript ||
         expr->getKind() == ExprKind::PrefixUnary);
  
  CleanupIllFormedExpressionRAII cleanup(*CS, expr);
  
  auto subscriptExpr = cast<SubscriptExpr>(expr);
  auto indexExpr = subscriptExpr->getIndex();
  auto baseExpr = subscriptExpr->getBase();
  
  auto indexType = getTypeOfIndependentSubExpression(indexExpr);
  
  // Extract the exact argument type from the argument tuple.
  if (auto parenTy = dyn_cast<ParenType>(indexType.getPointer())) {
    indexType = parenTy->getUnderlyingType();
  }
  
  // An error has been posted elsewhere.
  if (isErrorTypeKind(indexType))
    return true;
  
  auto baseType = getTypeOfIndependentSubExpression(baseExpr);
  
  auto indexTypeName = getUserFriendlyTypeName(indexType);
  auto baseTypeName = getUserFriendlyTypeName(baseType);
  
  assert(!(indexTypeName.empty() || baseTypeName.empty()));
  
  // FIXME: As with unary applications, we don't currently suggest a partially
  // matching overload.
  CS->TC.diagnose(indexExpr->getLoc(),
                  diag::cannot_subscript_with_index,
                  baseTypeName,
                  indexTypeName);
  
  return true;
}

bool FailureDiagnosis::diagnoseFailureForCallExpr() {
  assert(expr->getKind() == ExprKind::Call);
  
  CleanupIllFormedExpressionRAII cleanup(*CS, expr);
  
  auto callExpr = cast<CallExpr>(expr);
  auto fnExpr = callExpr->getFn();
  auto argExpr = callExpr->getArg();
  
  // An error was posted elsewhere.
  if (isErrorTypeKind(fnExpr->getType())) {
    return true;
  }
  
  std::string overloadName = "";
  
  bool isClosureInvocation = false;
  bool isInvalidTrailingClosureTarget = false;
  bool foundIntermediateError = false;
  bool isInitializer = false;
  bool isOverloadedFn = false;
  
  llvm::SmallVector<Type, 16> paramLists;
  llvm::SmallVector<Identifier, 16> argNames;
  llvm::SmallVector<Type, 16> argTypes;
  
  // Obtain the function's name, and collect any parameter lists for diffing
  // purposes.
  if (auto DRE = dyn_cast<DeclRefExpr>(fnExpr)) {
    overloadName = DRE->getDecl()->getNameStr();
    
    if (auto fnType = DRE->getDecl()->getType()->getAs<AnyFunctionType>()) {
      paramLists.push_back(fnType->getInput());
    }
    
  } else if (auto ODRE = dyn_cast<OverloadedDeclRefExpr>(fnExpr)) {
    isOverloadedFn = true;
    overloadName = ODRE->getDecls()[0]->getNameStr().str();
    
    // Collect the parameters for later use.
    for (auto D : ODRE->getDecls()) {
      if (auto fnType = D->getType()->getAs<AnyFunctionType>()) {
        paramLists.push_back(fnType->getInput());
      }
    }
    
  } else if (auto TE = dyn_cast<TypeExpr>(fnExpr)) {
    isInitializer = true;
    
    // It's always a metatype type, so use the instance type name.
    auto instanceType = TE->getType()->getAs<MetatypeType>()->getInstanceType();
    overloadName = instanceType->getString();

    // TODO: figure out right value for isKnownPrivate
    if (!instanceType->getAs<TupleType>()) {
      auto ctors = CS->TC.lookupConstructors(instanceType, CS->DC, false);
      for (auto ctor : ctors) {
        if (auto fnType = ctor->getType()->getAs<AnyFunctionType>()) {
          // skip type argument
          if (auto fnType2 = fnType->getResult()->getAs<AnyFunctionType>()) {
            paramLists.push_back(fnType2->getInput());
          }
        }
      }
    }
    if (paramLists.size() > 1) {
      isOverloadedFn = true;
    }
  } else if (auto UDE = dyn_cast<UnresolvedDotExpr>(fnExpr)) {
    overloadName = UDE->getName().str().str();
  } else {
    isClosureInvocation = true;
    
    auto unwrappedExpr = unwrapParenExpr(fnExpr);
    isInvalidTrailingClosureTarget = !isa<ClosureExpr>(unwrappedExpr);
  }
  
  // Build the argument type list.
  if (auto parenExpr = dyn_cast<ParenExpr>(argExpr)) {
    auto subType =
        getTypeOfIndependentSubExpression((parenExpr->getSubExpr()));
    
    if (isErrorTypeKind(subType))
      foundIntermediateError = true;
    
    argNames.push_back(Identifier());
    argTypes.push_back(subType);
  } else if (auto tupleExpr = dyn_cast<TupleExpr>(argExpr)) {
    for (unsigned i = 0; i < tupleExpr->getNumElements(); i++) {
      Identifier elName = tupleExpr->getElementName(i);
      Expr *elExpr = tupleExpr->getElement(i);
      auto elType = getTypeOfIndependentSubExpression(elExpr);
      
      if (isErrorTypeKind(elType))
        foundIntermediateError = true;
      
      argNames.push_back(elName);
      argTypes.push_back(elType);
    }
  }
  
  if (foundIntermediateError)
    return true;
  
  if (argTypes.size()) {
    
    std::string argString = "(" + getTypeListString(argNames, argTypes) + ")";
    
    if (!isOverloadedFn) {
      if (isClosureInvocation) {
        
        if (isInvalidTrailingClosureTarget) {
          CS->TC.diagnose(fnExpr->getLoc(),
                          diag::invalid_trailing_closure_target);
        } else {
          CS->TC.diagnose(fnExpr->getLoc(),
                          diag::cannot_invoke_closure,
                          argString);
        }
      } else {
        CS->TC.diagnose(fnExpr->getLoc(),
                        isInitializer ?
                          diag::cannot_apply_initializer_to_args :
                          diag::cannot_apply_function_to_args,
                        overloadName,
                        argString);
      }
    } else {
      CS->TC.diagnose(fnExpr->getLoc(),
                      isInitializer ?
                        diag::cannot_find_appropriate_initializer_with_list :
                        diag::cannot_find_appropriate_overload_with_list,
                      overloadName,
                      argString);
    }
  } else {
    
    if (isClosureInvocation) {
      CS->TC.diagnose(fnExpr->getLoc(), diag::cannot_infer_closure_type);
      
      if (!isInvalidTrailingClosureTarget) {
        auto closureExpr = dyn_cast<ClosureExpr>(unwrapParenExpr(fnExpr));
        
        if (!closureExpr->hasSingleExpressionBody() &&
            !closureExpr->hasExplicitResultType() &&
            closureExpr->getBody()->getElements().size()) {
          CS->TC.diagnose(fnExpr->getLoc(),
                          diag::mult_stmt_closures_require_explicit_result);
        }
        
      }
      
    } else {
      CS->TC.diagnose(fnExpr->getLoc(),
                      isInitializer ?
                        diag::cannot_find_initializer_with_no_params :
                        diag::cannot_find_overload_with_no_params,
                      overloadName);
    }
  }
  
  // Did the user intend on invoking a different overload?
  if (paramLists.size()) {
    if(!isOverloadedFn) {
      std::string paramString = "";
      SmallVector<Identifier, 16> paramNames;
      SmallVector<Type, 16> paramTypes;
      
      if (auto parenType = dyn_cast<ParenType>(paramLists[0].getPointer())) {
        paramNames.push_back(Identifier());
        paramTypes.push_back(parenType->getUnderlyingType());
      } else if (auto tupleType = paramLists[0]->getAs<TupleType>()) {
        for (auto field : tupleType->getFields()) {
          paramNames.push_back(field.getName());
          paramTypes.push_back(field.getType());
        }
      }
      
      if (paramTypes.size()) {
        paramString = "(" + getTypeListString(paramNames, paramTypes) + ")";
        
        CS->TC.diagnose(argExpr->getLoc(),
                        diag::expected_certain_args,
                        paramString);
      }
    } else {
      suggestPotentialOverloads(overloadName,
                                fnExpr->getLoc(),
                                paramLists,
                                argTypes);
    }
  }
  
  return true;
}

bool FailureDiagnosis::diagnoseFailureForAssignExpr() {
  assert(expr->getKind() == ExprKind::Assign);
  
  CleanupIllFormedExpressionRAII cleanup(*CS, expr);
  
  auto assignExpr = cast<AssignExpr>(expr);
  auto destExpr = assignExpr->getDest();
  auto srcExpr = assignExpr->getSrc();
  
  auto destType = getTypeOfIndependentSubExpression(destExpr);
  auto srcType = getTypeOfIndependentSubExpression(srcExpr);
  
  // If the source type is already an error type, we've likely already posted
  // an error due to a contextual type conversion error.
  if (isErrorTypeKind(srcType) || isErrorTypeKind(destType)) {
    return true;
  }
  
  auto destTypeName = getUserFriendlyTypeName(destType);
  auto srcTypeName = getUserFriendlyTypeName(srcType);
  
  if (isa<LiteralExpr>(destExpr)) {
    CS->TC.diagnose(destExpr->getLoc(),
                    diag::cannot_assign_to_literal);
  } else if (!srcTypeName.compare(destTypeName)) {
    
    if (destType->is<ArchetypeType>()) {
      CS->TC.diagnose(srcExpr->getLoc(),
                      diag::cannot_assign_invariant_self,
                      srcTypeName);
    } else {
      
      // If the types are the same, this might not be an assignability error -
      // something else may have gone wrong. In that case, independently
      // re-typecheck the source expression to see if there's a different
      // failure.
      CS->TC.eraseTypeData(srcExpr);
      CS->TC.typeCheckExpression(srcExpr, CS->DC, Type(), Type(), false);
      
      if (srcExpr->getType()->getAs<ErrorType>())
        return true;
      
      CS->TC.diagnose(destExpr->getLoc(),
                      diag::cannot_assign_to_immutable_expr,
                      destTypeName);
    }
  } else {
    CS->TC.diagnose(srcExpr->getLoc(),
                    diag::cannot_assign_values,
                    srcTypeName,
                    destTypeName);
  }
  
  return true;
}

bool FailureDiagnosis::diagnoseFailureForInOutExpr() {
  assert(expr->getKind() == ExprKind::InOut);
  
  CleanupIllFormedExpressionRAII cleanup(*CS, expr);
  
  auto inoutExpr = cast<InOutExpr>(expr);
  auto addressedExpr = inoutExpr->getSubExpr();
  
  if (auto DRE = dyn_cast<DeclRefExpr>(addressedExpr)) {
    if (auto VD = dyn_cast<VarDecl>(DRE->getDecl())) {
      if (VD->hasAccessorFunctions() && !VD->getSetter()) {
        CS->TC.diagnose(DRE->getLoc(),
                        diag::assignment_get_only_property,
                        VD->getName());
        return true;
      }
      
      if (VD->isLet()) {
        CS->TC.diagnose(addressedExpr->getLoc(),
                        diag::cannot_assign_to_immutable_expr,
                        getUserFriendlyTypeName(addressedExpr->getType()));
        return true;
      }
    }
  } else if (isa<UnresolvedDotExpr>(addressedExpr)) {
    // For now, keep the UDE distinct from the above to allow for potentially
    // better diagnostics.
    CS->TC.diagnose(addressedExpr->getLoc(),
                    diag::cannot_assign_to_immutable_expr,
                    getUserFriendlyTypeName(addressedExpr->getType()));
    return true;
  }
  
  return diagnoseGeneralFailure();
}

enum class CoerceExprDiagnosis {
  // The 'as' expression fails to type-check, even after being changed to 'as!'.
  Failure,

  // The 'as' expression type-checks if changed to 'as!'.
  ChangeToForcedCheckedCast,

  // The 'as' expression type-checks as-is.
  Success
};

// Type-check the coerceExpr independently of any other expression; if it
// fails, see if changing it to an 'as!' expression helps any. Return the
// result of the type checking.
static CoerceExprDiagnosis diagnoseCoerceExpr(
    CoerceExpr *coerceExpr, ConstraintSystem *CS) {
  class NoDiagnosticsListener : public ExprTypeCheckListener {
  public:
    bool suppressDiagnostics() const override { return true; }
  } listener;

  // See if changing the 'as' expression to 'as!' fixes things.
  Expr *forceExpr = new (CS->getASTContext()) ForcedCheckedCastExpr(
    coerceExpr->getSubExpr(), coerceExpr->getLoc(), SourceLoc(),
    coerceExpr->getCastTypeLoc());
  CS->TC.eraseTypeData(forceExpr);
  bool forceTypeCheckFailed =
    CS->TC.typeCheckExpression(forceExpr, CS->DC, Type(), Type(), false,
                               FreeTypeVariableBinding::Disallow, &listener);
  if (!forceTypeCheckFailed) {
    return CoerceExprDiagnosis::ChangeToForcedCheckedCast;
  }

  return CoerceExprDiagnosis::Failure;
}

bool FailureDiagnosis::diagnoseFailureForCoerceExpr() {
  CleanupIllFormedExpressionRAII cleanup(*CS, expr);
  CoerceExpr *coerceExpr = cast<CoerceExpr>(expr);

  Expr *subExpr = coerceExpr->getSubExpr();
  Type subType = getTypeOfIndependentSubExpression(subExpr);
  if (isErrorTypeKind(subType)) {
    return true;
  }

  std::pair<Type, Type> conversionTypes(nullptr, nullptr);
  if (conversionConstraint &&
      conversionConstraint->getKind() == ConstraintKind::ExplicitConversion &&
      conversionConstraint->getLocator()->getAnchor() == expr) {
    conversionTypes = getBoundTypesFromConstraint(CS, coerceExpr,
                                                       conversionConstraint);
  } else {
    conversionTypes.first = subType->getLValueOrInOutObjectType();
    conversionTypes.second = coerceExpr->getType();
  }

  if (conversionTypes.first && conversionTypes.second) {
    switch (diagnoseCoerceExpr(coerceExpr, CS)) {
    case CoerceExprDiagnosis::Failure:
      CS->TC.diagnose(coerceExpr->getLoc(), diag::invalid_relation,
                      Failure::TypesNotConvertible - Failure::TypesNotEqual,
                      conversionTypes.first, conversionTypes.second)
        .highlight(coerceExpr->getSourceRange());
      return true;
    case CoerceExprDiagnosis::ChangeToForcedCheckedCast:
      CS->TC.diagnose(coerceExpr->getLoc(), diag::missing_forced_downcast,
                      conversionTypes.first, conversionTypes.second)
        .highlight(coerceExpr->getSourceRange())
        .fixItReplace(coerceExpr->getLoc(), "as!");
      return true;
    case CoerceExprDiagnosis::Success:
      break;
    }
  }

  return diagnoseGeneralFailure();
}

bool FailureDiagnosis::diagnoseFailureForForcedCheckedCastExpr() {
  assert(expr->getKind() == ExprKind::ForcedCheckedCast);
  CleanupIllFormedExpressionRAII cleanup(*CS, expr);
  ForcedCheckedCastExpr *castExpr = dyn_cast<ForcedCheckedCastExpr>(expr);

  Expr *subExpr = castExpr->getSubExpr();
  Type subType = getTypeOfIndependentSubExpression(subExpr);
  if (isErrorTypeKind(subType)) {
    return true;
  }

  std::pair<Type, Type> conversionTypes(nullptr, nullptr);
  if (conversionConstraint &&
      conversionConstraint->getKind() == ConstraintKind::CheckedCast &&
      conversionConstraint->getLocator()->getAnchor() == expr) {
    conversionTypes = getBoundTypesFromConstraint(CS, castExpr,
                                                       conversionConstraint);
  } else {
    conversionTypes.first = subType->getLValueOrInOutObjectType();
    conversionTypes.second = castExpr->getType();
  }

  if (conversionTypes.first && conversionTypes.second) {
    CS->TC.diagnose(castExpr->getLoc(), diag::invalid_relation,
                    Failure::TypesNotConvertible - Failure::TypesNotEqual,
                    conversionTypes.first, conversionTypes.second)
      .highlight(castExpr->getSourceRange());
    return true;
  }

  return diagnoseGeneralFailure();
}

bool FailureDiagnosis::diagnoseFailure() {
  assert(CS && expr);
  
  if (activeConformanceConstraint) {
    std::pair<Type, Type> types =
    getBoundTypesFromConstraint(CS,
                                expr,
                                activeConformanceConstraint);
    
    CS->TC.diagnose(expr->getLoc(),
                         diag::does_not_conform_to_constraint,
                         types.first,
                         types.second)
    .highlight(expr->getSourceRange());
    
    return true;
  }
  
  // If a bridging conversion slips through, treat it as ambiguous.
  if (bridgeToObjCConstraint) {
    CS->TC.diagnose(expr->getLoc(), diag::type_of_expression_is_ambiguous);
    
    return true;
  }
  
  // Move on to expr-specific diagnostics.
  switch(expr->getKind()) {
      
#define EXPR(ID, PARENT) \
  case ExprKind::ID: return diagnoseFailureFor##ID##Expr();
#include "swift/AST/ExprNodes.def"

  }
  
  llvm_unreachable("unrecognized expr kind");
}

/// Given a specific expression and the remnants of the failed constraint
/// system, produce a specific diagnostic.
bool ConstraintSystem::diagnoseFailureForExpr(Expr *expr) {
  
  FailureDiagnosis diagnosis(expr, this);
  
  // Now, attempt to diagnose the failure from the info we've collected.
  if (diagnosis.diagnoseFailure())
    return true;
  
  // A DiscardAssignmentExpr is special in that it introduces a new type
  // variable but places no constraints upon it. Instead, it relies on the rhs
  // of its assignment expression to determine its type. Unfortunately, in the
  // case of error recovery, the "_" expression may be left alone with no
  // constraints for us to derive an error from. In that case, we'll fall back
  // to the "outside assignment" error.
  if (ActiveConstraints.empty() &&
      InactiveConstraints.empty() &&
      !failedConstraint) {
    
    if (isa<DiscardAssignmentExpr>(expr)) {
      TC.diagnose(expr->getLoc(), diag::discard_expr_outside_of_assignment)
        .highlight(expr->getSourceRange());
      
      return true;
    }
    
    if (auto dot = dyn_cast<UnresolvedDotExpr>(expr)) {
      TC.diagnose(expr->getLoc(),
                  diag::not_enough_context_for_generic_method_reference,
                  dot->getName());
      
      return true;
    }
    
    // If there are no posted constraints or failures, then there was
    // not enough contextual information available to infer a type for the
    // expression.
    TC.diagnose(expr->getLoc(), diag::type_of_expression_is_ambiguous);
  
    return true;
  }
  
  return false;
}

bool ConstraintSystem::salvage(SmallVectorImpl<Solution> &viable,
                               Expr *expr,
                               bool onlyFailures) {
  
  // If there were any unavoidable failures, emit the first one we can.
  if (!unavoidableFailures.empty()) {
    for (auto failure : unavoidableFailures) {
      
      // In the 'onlyFailures' case, we'll want to synthesize a locator if one
      // does not exist. That allows us to emit decent diagnostics for
      // constraint application failures where the constraints themselves lack
      // a valid location.
      if (diagnoseFailure(*this, *failure, expr, onlyFailures))
        return true;
    }
    
    if (onlyFailures)
      return true;

    // If we can't make sense of the existing constraints (or none exist), go
    // ahead and try the unavoidable failures again, but with locator
    // substitutions in place.
    if (!this->diagnoseFailureForExpr(expr) &&
        !unavoidableFailures.empty()) {
      for (auto failure : unavoidableFailures) {
        if (diagnoseFailure(*this, *failure, expr, true))
          return true;
      }
    }
    
    return true;
  }

  // There were no unavoidable failures, so attempt to solve again, capturing
  // any failures that come from our attempts to select overloads or bind
  // type variables.
  {
    viable.clear();

    // Set up solver state.
    SolverState state(*this);
    state.recordFailures = true;
    this->solverState = &state;

    // Solve the system.
    solve(viable);

    // Check whether we have a best solution; this can happen if we found
    // a series of fixes that worked.
    if (auto best = findBestSolution(viable, /*minimize=*/true)) {
      if (*best != 0)
        viable[0] = std::move(viable[*best]);
      viable.erase(viable.begin() + 1, viable.end());
      return false;
    }

    // FIXME: If we were able to actually fix things along the way,
    // we may have to hunt for the best solution. For now, we don't care.

    // If there are multiple solutions, try to diagnose an ambiguity.
    if (viable.size() > 1) {
      if (getASTContext().LangOpts.DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log << "---Ambiguity error: "
            << viable.size() << " solutions found---\n";
        int i = 0;
        for (auto &solution : viable) {
          log << "---Ambiguous solution #" << i++ << "---\n";
          solution.dump(&TC.Context.SourceMgr, log);
          log << "\n";
        }
      }        

      if (diagnoseAmbiguity(*this, viable)) {
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
    
    if (diagnoseFailure(*this, failure, expr, false))
      return true;
  }
  
  if (getExpressionTooComplex()) {
    TC.diagnose(expr->getLoc(), diag::expression_too_complex).
    highlight(expr->getSourceRange());
    
    return true;
  }
  
  // If all else fails, attempt to diagnose the failure by looking through the
  // system's constraints.
  this->diagnoseFailureForExpr(expr);
  
  return true;
}
