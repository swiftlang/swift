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
#include "llvm/Support/SaveAndRestore.h"

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

  case FunctionThrowsMismatch:
    out << "function throws mismatch " << getFirstType().getString() << " vs. "
        << getSecondType().getString();
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

  case IsNotMaterializable:
    out << getFirstType().getString() << " is not materializable";
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
constraints::simplifyLocator(ConstraintSystem &cs, ConstraintLocator *locator,
                             SourceRange &range,
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
  simplifyLocator(anchor, path, targetAnchor, targetPath, range);


  // If we have a target anchor, build and simplify the target locator.
  if (targetLocator && targetAnchor) {
    SourceRange targetRange;
    unsigned targetFlags = recomputeSummaryFlags(locator, targetPath);
    auto loc = cs.getConstraintLocator(targetAnchor, targetPath, targetFlags);
    *targetLocator = simplifyLocator(cs, loc, targetRange);
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
                                  SourceRange &range) {
  range = SourceRange();
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

#ifdef SWIFT_ENABLE_OBJECT_LITERALS
      if (auto objectLiteralExpr = dyn_cast<ObjectLiteralExpr>(anchor)) {
        targetAnchor = nullptr;
        targetPath.clear();

        anchor = objectLiteralExpr->getArg();
        path = path.slice(1);
        continue;
      }
#endif // SWIFT_ENABLE_OBJECT_LITERALS
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
    case ConstraintLocator::UnresolvedMember:
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

    case ConstraintLocator::ConstructorMember:
      if (auto typeExpr = dyn_cast<TypeExpr>(anchor)) {
        // This is really an implicit 'init' MemberRef, so point at the base,
        // i.e. the TypeExpr.
        targetAnchor = nullptr;
        targetPath.clear();
        range = SourceRange();
        anchor = typeExpr;
        path = path.slice(1);
        continue;
      }
      SWIFT_FALLTHROUGH;

    case ConstraintLocator::Member:
    case ConstraintLocator::MemberRefBase:
      if (auto UDE = dyn_cast<UnresolvedDotExpr>(anchor)) {
        // No additional target locator information.
        targetAnchor = nullptr;
        targetPath.clear();
        
        range = UDE->getNameLoc();
        anchor = UDE->getBase();
        path = path.slice(1);
        continue;
      }
      if (auto USE = dyn_cast<UnresolvedSelectorExpr>(anchor)) {
        // No additional target locator information.
        targetAnchor = nullptr;
        targetPath.clear();
        
        range = USE->getNameRange();
        anchor = USE->getBase();
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
    case ConstraintLocator::SubscriptMember:
      if (auto subscript = dyn_cast<SubscriptExpr>(anchor)) {
        anchor = subscript->getBase();
        targetAnchor = nullptr;
        targetPath.clear();
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::CheckedCastOperand:
      if (auto castExpr = dyn_cast<ExplicitCastExpr>(anchor)) {
        targetAnchor = nullptr;
        targetPath.clear();
        anchor = castExpr->getSubExpr();
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::ClosureResult:
      if (auto CE = dyn_cast<ClosureExpr>(anchor)) {
        if (CE->hasSingleExpressionBody()) {
          targetAnchor = nullptr;
          targetPath.clear();
          anchor = CE->getSingleExpressionBody();
          path = path.slice(1);
          continue;
        }
      }
      break;
        
    case ConstraintLocator::IfThen:
      if (auto ITE = dyn_cast<IfExpr>(anchor)) {
        targetAnchor = nullptr;
        targetPath.clear();
        anchor = ITE->getThenExpr();
        path = path.slice(1);
        continue;
      }
      break;
    case ConstraintLocator::IfElse:
      if (auto ITE = dyn_cast<IfExpr>(anchor)) {
        targetAnchor = nullptr;
        targetPath.clear();
        anchor = ITE->getElseExpr();
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

  SourceRange range;
  locator = simplifyLocator(cs, locator, range);
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

    if (auto identity = dyn_cast<IdentityExpr>(anchor)) {
      anchor = identity->getSubExpr();
      continue;
    }

    if (auto tryExpr = dyn_cast<AnyTryExpr>(anchor)) {
      if (isa<OptionalTryExpr>(tryExpr))
        break;

      anchor = tryExpr->getSubExpr();
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
          parameterPattern = tuple->getElement(index).getPattern();
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
          parameterPattern = tuple->getElement(index).getPattern();
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
        if (tuple->getNumElements() == 1) {
          parameterPattern = tuple->getElement(0).getPattern();
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
                                   const Failure *failure,
                                   ConstraintLocator *targetLocator) {
  // If there's no anchor, there's nothing we can do.
  if (!targetLocator->getAnchor())
    return;

  // Try to resolve the locator to a particular declaration.
  auto resolved
    = resolveLocatorToDecl(cs, targetLocator,
        [&](ConstraintLocator *locator) -> Optional<SelectedOverload> {
          if (!failure) return None;
          for (auto resolved = failure->getResolvedOverloadSets();
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
static bool diagnoseFailure(ConstraintSystem &cs, Failure &failure,
                            Expr *expr, bool useExprLoc) {
  ConstraintLocator *cloc;
  if (!failure.getLocator() || !failure.getLocator()->getAnchor()) {
    if (useExprLoc)
      cloc = cs.getConstraintLocator(expr);
    else
      return false;
  } else {
    cloc = failure.getLocator();
  }
  
  SourceRange range;

  ConstraintLocator *targetLocator;
  auto locator = simplifyLocator(cs, cloc, range, &targetLocator);
  auto &tc = cs.getTypeChecker();

  auto anchor = locator->getAnchor();
  auto loc = anchor->getLoc();
  switch (failure.getKind()) {
  case Failure::TupleSizeMismatch: {
    auto tuple1 = failure.getFirstType()->castTo<TupleType>();
    auto tuple2 = failure.getSecondType()->castTo<TupleType>();
    tc.diagnose(loc, diag::invalid_tuple_size, tuple1, tuple2,
                tuple1->getNumElements(), tuple2->getNumElements())
      .highlight(range);
    return true;
  }

  case Failure::TupleUnused:
    tc.diagnose(loc, diag::invalid_tuple_element_unused,
                failure.getFirstType(), failure.getSecondType())
      .highlight(range);
    return true;

  case Failure::TypesNotConvertible:
  case Failure::TypesNotEqual:
  case Failure::TypesNotSubtypes:
  case Failure::TypesNotConstructible:
  case Failure::FunctionTypesMismatch:
  case Failure::DoesNotHaveMember:
  case Failure::DoesNotConformToProtocol:
  case Failure::FunctionNoEscapeMismatch:
  case Failure::FunctionThrowsMismatch:
    // The Expr path handling these constraints does a good job.
    return false;

  case Failure::IsNotBridgedToObjectiveC:
    tc.diagnose(loc, diag::type_not_bridged, failure.getFirstType());
    if (targetLocator)
      noteTargetOfDiagnostic(cs, &failure, targetLocator);
    break;

  case Failure::IsForbiddenLValue:
    // FIXME: Probably better handled by InOutExpr later.
    if (auto iotTy = failure.getSecondType()->getAs<InOutType>()) {
      tc.diagnose(loc, diag::reference_non_inout, iotTy->getObjectType())
        .highlight(range);
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
      name = tupleTy->getElement(idx).getName();
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
      if (auto *cast = findForcedDowncast(tc.Context, force->getSubExpr())) {
        tc.diagnose(force->getLoc(), diag::forcing_explicit_downcast,
                    failure.getFirstType())
          .highlight(cast->getLoc())
          .fixItRemove(force->getLoc());
        return true;
      }
      
      tc.diagnose(loc, diag::invalid_force_unwrap,
                  failure.getFirstType())
        .highlight(force->getSourceRange())
        .fixItRemove(force->getExclaimLoc());
      
      return true;
    }
    
    if (auto bind = dyn_cast_or_null<BindOptionalExpr>(anchor)) {
      tc.diagnose(loc, diag::invalid_optional_chain,
                  failure.getFirstType())
        .highlight(bind->getSourceRange())
        .fixItRemove(bind->getQuestionLoc());
      
      return true;      
    }
    return false;
  }

  case Failure::NoPublicInitializers: {
    tc.diagnose(loc, diag::no_accessible_initializers, failure.getFirstType())
      .highlight(range);
    if (targetLocator && !useExprLoc)
      noteTargetOfDiagnostic(cs, &failure, targetLocator);
    break;
  }

  case Failure::IsNotMaterializable: {
    tc.diagnose(loc, diag::cannot_bind_generic_parameter_to_type,
                failure.getFirstType())
      .highlight(range);
    if (!useExprLoc)
      noteTargetOfDiagnostic(cs, &failure, locator);
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
static DeclName getOverloadChoiceName(ArrayRef<OverloadChoice> choices) {
  for (auto choice : choices) {
    if (choice.isDecl())
      return choice.getDecl()->getFullName();
  }

  return DeclName();
}

static bool diagnoseAmbiguity(ConstraintSystem &cs,
                              ArrayRef<Solution> solutions,
                              Expr *expr) {
  // Produce a diff of the solutions.
  SolutionDiff diff(solutions);

  // Find the locators which have the largest numbers of distinct overloads.
  Optional<unsigned> bestOverload;
  unsigned maxDistinctOverloads = 0;
  unsigned maxDepth = 0;
  unsigned minIndex = std::numeric_limits<unsigned>::max();

  // Get a map of expressions to their depths and post-order traversal indices.
  // Heuristically, all other things being equal, we should complain about the
  // ambiguous expression that (1) has the most overloads, (2) is deepest, or
  // (3) comes earliest in the expression.
  auto depthMap = expr->getDepthMap();
  auto indexMap = expr->getPreorderIndexMap();

  for (unsigned i = 0, n = diff.overloads.size(); i != n; ++i) {
    auto &overload = diff.overloads[i];

    // If we can't resolve the locator to an anchor expression with no path,
    // we can't diagnose this well.
    auto *anchor = simplifyLocatorToAnchor(cs, overload.locator);
    if (!anchor)
      continue;
    auto it = indexMap.find(anchor);
    if (it == indexMap.end())
      continue;
    unsigned index = it->second;
    it = depthMap.find(anchor);
    if (it == depthMap.end())
      continue;
    unsigned depth = it->second;

    // If we don't have a name to hang on to, it'll be hard to diagnose this
    // overload.
    if (!getOverloadChoiceName(overload.choices))
      continue;

    unsigned distinctOverloads = countDistinctOverloads(overload.choices);

    // We need at least two overloads to make this interesting.
    if (distinctOverloads < 2)
      continue;

    // If we have more distinct overload choices for this locator than for
    // prior locators, just keep this locator.
    
    bool better = false;
    if (bestOverload) {
      if (distinctOverloads > maxDistinctOverloads) {
        better = true;
      } else if (distinctOverloads == maxDistinctOverloads) {
        if (depth > maxDepth) {
          better = true;
        } else if (depth == maxDepth) {
          if (index < minIndex) {
            better = true;
          }
        }
      }
    }

    if (!bestOverload || better) {
      bestOverload = i;
      maxDistinctOverloads = distinctOverloads;
      maxDepth = depth;
      minIndex = index;
      continue;
    }

    // We have better results. Ignore this one.
  }

  // FIXME: Should be able to pick the best locator, e.g., based on some
  // depth-first numbering of expressions.
  if (bestOverload) {
    auto &overload = diff.overloads[*bestOverload];
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

static SmallVector<TupleTypeElt, 4> decomposeArgumentType(Type ty) {
  SmallVector<TupleTypeElt, 4> result;

  // Assemble the parameter type list.
  if (auto parenType = dyn_cast<ParenType>(ty.getPointer())) {
    result.push_back(parenType->getUnderlyingType());
  } else if (auto tupleType = ty->getAs<TupleType>()) {
    result.append(tupleType->getElements().begin(),
                  tupleType->getElements().end());
  } else {
    result.push_back(ty);
  }
  return result;
}

static std::string getTypeListString(Type type) {
  // Assemble the parameter type list.
  auto tupleType = type->getAs<TupleType>();
  if (!tupleType) {
    if (auto PT = dyn_cast<ParenType>(type.getPointer()))
      type = PT->getUnderlyingType();

    return "(" + type->getString() + ")";
  }

  std::string result = "(";
  for (auto field : tupleType->getElements()) {
    if (result.size() != 1)
      result += ", ";
    if (!field.getName().empty()) {
      result += field.getName().str();
      result += ": ";
    }

    if (!field.isVararg())
      result += field.getType()->getString();
    else {
      result += field.getVarargBaseTy()->getString();
      result += "...";
    }
  }
  result += ")";
  return result;
}


/// If an UnresolvedDotExpr, SubscriptMember, etc has been resolved by the
/// constraint system, return the decl that it references.
static ValueDecl *findResolvedMemberRef(ConstraintLocator *locator,
                                        ConstraintSystem &CS) {
  auto *resolvedOverloadSets = CS.getResolvedOverloadSets();
  if (!resolvedOverloadSets) return nullptr;

  // Search through the resolvedOverloadSets to see if we have a resolution for
  // this member.  This is an O(n) search, but only happens when producing an
  // error diagnostic.
  for (auto resolved = resolvedOverloadSets;
       resolved; resolved = resolved->Previous) {
    if (resolved->Locator != locator) continue;

    // We only handle the simplest decl binding.
    if (resolved->Choice.getKind() != OverloadChoiceKind::Decl)
      return nullptr;
    return resolved->Choice.getDecl();
  }

  return nullptr;
}


/// Given an expression that has a non-lvalue type, dig into it until we find
/// the part of the expression that prevents the entire subexpression from being
/// mutable.  For example, in a sequence like "x.v.v = 42" we want to complain
/// about "x" being a let property if "v.v" are both mutable.
///
/// This returns the base subexpression that looks immutable (or that can't be
/// analyzed any further) along with a decl extracted from it if we could.
///
static std::pair<Expr*, ValueDecl*>
resolveImmutableBase(Expr *expr, ConstraintSystem &CS) {
  expr = expr->getValueProvidingExpr();

  // Provide specific diagnostics for assignment to subscripts whose base expr
  // is known to be an rvalue.
  if (auto *SE = dyn_cast<SubscriptExpr>(expr)) {
    // If we found a decl for the subscript, check to see if it is a set-only
    // subscript decl.
    SubscriptDecl *member = nullptr;
    if (SE->hasDecl())
      member = dyn_cast_or_null<SubscriptDecl>(SE->getDecl().getDecl());
    
    if (!member) {
      auto loc = CS.getConstraintLocator(SE,ConstraintLocator::SubscriptMember);
      member = dyn_cast_or_null<SubscriptDecl>(findResolvedMemberRef(loc, CS));
    }

    // If it isn't settable, return it.
    if (member) {
      if (!member->isSettable() ||
          !member->isSetterAccessibleFrom(CS.DC))
        return { expr, member };
    }

    // If it is settable, then the base must be the problem, recurse.
    return resolveImmutableBase(SE->getBase(), CS);
  }

  // Look through property references.
  if (auto *UDE = dyn_cast<UnresolvedDotExpr>(expr)) {
    // If we found a decl for the UDE, check it.
    auto loc = CS.getConstraintLocator(UDE, ConstraintLocator::Member);
    auto *member = dyn_cast_or_null<VarDecl>(findResolvedMemberRef(loc, CS));

    // If the member isn't settable, then it is the problem: return it.
    if (member) {
      if (!member->isSettable(nullptr) ||
          !member->isSetterAccessibleFrom(CS.DC))
        return { expr, member };
    }

    // If we weren't able to resolve a member or if it is mutable, then the
    // problem must be with the base, recurse.
    return resolveImmutableBase(UDE->getBase(), CS);
  }

  if (auto *MRE = dyn_cast<MemberRefExpr>(expr)) {
    // If the member isn't settable, then it is the problem: return it.
    if (auto member = dyn_cast<AbstractStorageDecl>(MRE->getMember().getDecl()))
      if (!member->isSettable(nullptr) ||
          !member->isSetterAccessibleFrom(CS.DC))
        return { expr, member };

    // If we weren't able to resolve a member or if it is mutable, then the
    // problem must be with the base, recurse.
    return resolveImmutableBase(MRE->getBase(), CS);
  }

  if (auto *DRE = dyn_cast<DeclRefExpr>(expr))
    return { expr, DRE->getDecl() };

  // Look through x!
  if (auto *FVE = dyn_cast<ForceValueExpr>(expr))
    return resolveImmutableBase(FVE->getSubExpr(), CS);
  
  // Look through x?
  if (auto *BOE = dyn_cast<BindOptionalExpr>(expr))
    return resolveImmutableBase(BOE->getSubExpr(), CS);
  
  return { expr, nullptr };
}

static void diagnoseSubElementFailure(Expr *destExpr,
                                      SourceLoc loc,
                                      ConstraintSystem &CS,
                                      Diag<StringRef> diagID,
                                      Diag<Type> unknownDiagID) {
  auto &TC = CS.getTypeChecker();
  
  // Walk through the destination expression, resolving what the problem is.  If
  // we find a node in the lvalue path that is problematic, this returns it.
  auto immInfo = resolveImmutableBase(destExpr, CS);

  // Otherwise, we cannot resolve this because the available setter candidates
  // are all mutating and the base must be mutating.  If we dug out a
  // problematic decl, we can produce a nice tailored diagnostic.
  if (auto *VD = dyn_cast_or_null<VarDecl>(immInfo.second)) {
    std::string message = "'";
    message += VD->getName().str().str();
    message += "'";
 
    if (VD->isImplicit())
      message += " is immutable";
    else if (VD->isLet())
      message += " is a 'let' constant";
    else if (VD->hasAccessorFunctions() && !VD->getSetter())
      message += " is a get-only property";
    else if (!VD->isSetterAccessibleFrom(CS.DC))
      message += " setter is inaccessible";
    else {
      message += " is immutable";
    }
    TC.diagnose(loc, diagID, message)
      .highlight(immInfo.first->getSourceRange());
    
    // If this is a simple variable marked with a 'let', emit a note to fixit
    // hint it to 'var'.
    VD->emitLetToVarNoteIfSimple(CS.DC);
    return;
  }

  // If the underlying expression was a read-only subscript, diagnose that.
  if (auto *SD = dyn_cast_or_null<SubscriptDecl>(immInfo.second)) {
    StringRef message;
    if (!SD->getSetter())
      message = "subscript is get-only";
    else if (!SD->isSetterAccessibleFrom(CS.DC))
      message = "subscript setter is inaccessible";
    else
      message = "subscript is immutable";

    TC.diagnose(loc, diagID, message)
      .highlight(immInfo.first->getSourceRange());
    return;
  }

  // If the expression is the result of a call, it is an rvalue, not a mutable
  // lvalue.
  if (auto *AE = dyn_cast<ApplyExpr>(immInfo.first)) {
    std::string name = "call";
    if (isa<PrefixUnaryExpr>(AE) || isa<PostfixUnaryExpr>(AE))
      name = "unary operator";
    else if (isa<BinaryExpr>(AE))
      name = "binary operator";
    else if (isa<CallExpr>(AE))
      name = "function call";
    else if (isa<DotSyntaxCallExpr>(AE) || isa<DotSyntaxBaseIgnoredExpr>(AE))
      name = "method call";

    if (auto *DRE =
          dyn_cast<DeclRefExpr>(AE->getFn()->getValueProvidingExpr()))
      name = std::string("'") + DRE->getDecl()->getName().str().str() + "'";

    TC.diagnose(loc, diagID, name + " returns immutable value")
      .highlight(AE->getSourceRange());
    return;
  }

  TC.diagnose(loc, unknownDiagID, destExpr->getType())
    .highlight(immInfo.first->getSourceRange());
}

namespace {
  /// Each match in an ApplyExpr is evaluated for how close of a match it is.
  /// The result is captured in this enum value, where the earlier entries are
  /// most specific.
  enum CandidateCloseness {
    CC_ExactMatch,            ///< This is a perfect match for the arguments.
    CC_NonLValueInOut,       ///< First argument is inout but no lvalue present.
    CC_OneArgumentMismatch,   ///< All arguments except one match.
    CC_SelfMismatch,          ///< Self argument mismatches.
    CC_ArgumentMismatch,      ///< Argument list mismatch.
    CC_ArgumentLabelMismatch, ///< Argument label mismatch.
    CC_ArgumentCountMismatch, ///< This candidate has wrong # arguments.
    CC_GeneralMismatch        ///< Something else is wrong.
  };

  /// This is a candidate for a callee, along with an uncurry level.
  ///
  /// The uncurry level specifies how far much of a curried value has already
  /// been applied.  For example, in a funcdecl of:
  ///     func f(a:Int)(b:Double) -> Int
  /// Uncurry level of 0 indicates that we're looking at the "a" argument, an
  /// uncurry level of 1 indicates that we're looking at the "b" argument.
  struct UncurriedCandidate {
    ValueDecl *decl;
    unsigned level;

    AnyFunctionType *getUncurriedFunctionType() const {
      auto type = decl->getType();

      // If this is an operator func decl in a type context, the 'self' isn't
      // actually going to be applied.
      if (auto *fd = dyn_cast<FuncDecl>(decl))
        if (fd->isOperator() && fd->getDeclContext()->isTypeContext())
          type = type->castTo<AnyFunctionType>()->getResult();

      for (unsigned i = 0, e = level; i != e; ++i) {
        auto funcTy = type->getAs<AnyFunctionType>();
        if (!funcTy) return nullptr;
        type = funcTy->getResult();
      }

      return type->getAs<AnyFunctionType>();
    }

    /// Given a function candidate with an uncurry level, return the parameter
    /// type at the specified uncurry level.  If there is an error getting to
    /// the specified input, this returns a null Type.
    Type getArgumentType() const {
      if (auto *funcTy = getUncurriedFunctionType())
        return funcTy->getInput();
      return Type();
    }

    /// Given a function candidate with an uncurry level, return the parameter
    /// type at the specified uncurry level.  If there is an error getting to
    /// the specified input, this returns a null Type.
    Type getResultType() const {
      if (auto *funcTy = getUncurriedFunctionType())
        return funcTy->getResult();
      return Type();
    }
  };


  /// This struct represents an analyzed function pointer to determine the
  /// candidates that could be called, or the one concrete decl that will be
  /// called if not ambiguous.
  class CalleeCandidateInfo {
    ConstraintSystem *CS;
  public:
    std::string declName;

    /// This is the list of candidates identified.
    SmallVector<UncurriedCandidate, 4> candidates;

    /// This tracks how close the match is.
    CandidateCloseness closeness;
    
    /// Analyze a function expr and break it into a candidate set.  On failure,
    /// this leaves the candidate list empty.
    CalleeCandidateInfo(Expr *Fn, ConstraintSystem *CS) : CS(CS) {
      collectCalleeCandidates(Fn);
    }

    /// Analyze a locator for a SubscriptExpr for its candidate set.
    CalleeCandidateInfo(ConstraintLocator *locator, ConstraintSystem *CS);

    CalleeCandidateInfo(ArrayRef<OverloadChoice> candidates,
                        unsigned UncurryLevel, ConstraintSystem *CS);

    typedef const std::function<CandidateCloseness(UncurriedCandidate)>
    &ClosenessPredicate;

    /// After the candidate list is formed, it can be filtered down to discard
    /// obviously mismatching candidates and compute a "closeness" for the
    /// resultant set.
    void filterList(ArrayRef<TupleTypeElt> actualArgs);
    void filterList(Type actualArgsType) {
      return filterList(decomposeArgumentType(actualArgsType));
    }
    void filterList(ClosenessPredicate predicate);
    void filterContextualMemberList(Expr *argExpr);

    bool empty() const { return candidates.empty(); }
    unsigned size() const { return candidates.size(); }
    UncurriedCandidate operator[](unsigned i) const {
      return candidates[i];
    }
    
    /// Given a set of parameter lists from an overload group, and a list of
    /// arguments, emit a diagnostic indicating any partially matching
    /// overloads.
    void suggestPotentialOverloads(StringRef functionName, SourceLoc loc,
                                   bool isCallExpr = false);

  private:
    void collectCalleeCandidates(Expr *fnExpr);
  };
}


/// Given a candidate list, this computes the narrowest closeness to the match
/// we're looking for and filters out any worse matches.  The predicate
/// indicates how close a given candidate is to the desired match.
void CalleeCandidateInfo::filterList(ClosenessPredicate predicate) {
  closeness = CC_GeneralMismatch;

  // If we couldn't find anything, give up.
  if (candidates.empty())
    return;

  // Now that we have the candidate list, figure out what the best matches from
  // the candidate list are, and remove all the ones that aren't at that level.
  SmallVector<CandidateCloseness, 4> closenessList;
  closenessList.reserve(candidates.size());
  for (auto decl : candidates) {
    closenessList.push_back(predicate(decl));
    closeness = std::min(closeness, closenessList.back());
  }

  // Now that we know the minimum closeness, remove all the elements that aren't
  // as close.
  unsigned NextElt = 0;
  for (unsigned i = 0, e = candidates.size(); i != e; ++i) {
    // If this decl in the result list isn't a close match, ignore it.
    if (closeness != closenessList[i])
      continue;

    // Otherwise, preserve it.
    candidates[NextElt++] = candidates[i];
  }

  candidates.erase(candidates.begin()+NextElt, candidates.end());
}


/// Determine how close an argument list is to an already decomposed argument
/// list.
static CandidateCloseness
evaluateCloseness(Type candArgListType, ArrayRef<TupleTypeElt> actualArgs) {
  auto candArgs = decomposeArgumentType(candArgListType);

  // FIXME: This isn't handling varargs, and isn't handling default values
  // either.
  if (actualArgs.size() != candArgs.size()) {
    // If the candidate is varargs, and if there are more arguments specified
    // than required, consider this a general mismatch.
    // TODO: we could catalog the remaining entries if they *do* match up.
    if (auto *TT = candArgListType->getAs<TupleType>()) {
      if (!TT->getElements().empty()) {
        if (TT->getElements().back().isVararg() &&
            actualArgs.size() >= TT->getElements().size()-1)
          return CC_GeneralMismatch;
      }
    }
    
    return CC_ArgumentCountMismatch;
  }
  
  // Count the number of mismatched arguments.
  unsigned mismatchingArgs = 0;
  for (unsigned i = 0, e = actualArgs.size(); i != e; ++i) {
    // Check to see if the keyword arguments line up.  If not, this is a bad
    // issue, which is considered to be more severe than a type mismatch: if
    // the labels are wrong, the user may be passing a value to the wrong
    // argument entirely - a type mismatch would be the wrong thing to report.
    if (actualArgs[i].getName() != candArgs[i].getName())
      return CC_ArgumentLabelMismatch;

    auto argType = actualArgs[i].getType();

    // If the argument has an unresolved type, then we're not actually matching
    // against it.
    if (argType->is<UnresolvedType>())
      continue;
    if (auto *LVT = argType->getAs<LValueType>())
      if (LVT->getObjectType()->is<UnresolvedType>())
        continue;
    
    // FIXME: Right now, a "matching" overload is one with a parameter whose
    // type is identical to one of the argument types. We can obviously do
    // something more sophisticated with this.
    // FIXME: We definitely need to handle archetypes for same-type constraints.
    if (!argType->getRValueType()->isEqual(candArgs[i].getType()))
      ++mismatchingArgs;
  }

  
  // If the arguments match up exactly, then we have an exact match.  This
  // handles the no-argument cases as well.
  if (mismatchingArgs == 0)
    return CC_ExactMatch;
  
  // Check to see if the first argument expects an inout argument, but is not
  // an lvalue.
  if (candArgs[0].getType()->is<InOutType>() &&
      !actualArgs[0].getType()->isLValueType())
    return CC_NonLValueInOut;

  // If we have exactly one argument mismatching, classify it specially, so that
  // close matches are prioritized against obviously wrong ones.
  if (mismatchingArgs == 1)
    return actualArgs.size() != 1 ? CC_OneArgumentMismatch :CC_ArgumentMismatch;

  return CC_GeneralMismatch;
}

void CalleeCandidateInfo::collectCalleeCandidates(Expr *fn) {
  fn = fn->getValueProvidingExpr();

  // Treat a call to a load of a variable as a call to that variable, it is just
  // the lvalue'ness being removed.
  if (auto load = dyn_cast<LoadExpr>(fn)) {
    if (isa<DeclRefExpr>(load->getSubExpr()))
      return collectCalleeCandidates(load->getSubExpr());
  }

  if (auto declRefExpr = dyn_cast<DeclRefExpr>(fn)) {
    candidates.push_back({ declRefExpr->getDecl(), 0 });
    declName = declRefExpr->getDecl()->getNameStr().str();
    return;
  }

  if (auto declRefExpr = dyn_cast<OtherConstructorDeclRefExpr>(fn)) {
    auto decl = declRefExpr->getDecl();
    candidates.push_back({ decl, 0 });
    
    if (auto fTy = decl->getType()->getAs<AnyFunctionType>())
      declName = fTy->getInput()->getRValueInstanceType()->getString()+".init";
    else
      declName = "init";
    return;
  }

  if (auto overloadedDRE = dyn_cast<OverloadedDeclRefExpr>(fn)) {
    for (auto cand : overloadedDRE->getDecls()) {
      candidates.push_back({ cand, 0 });
    }

    if (!candidates.empty())
      declName = candidates[0].decl->getNameStr().str();
    return;
  }

  if (auto TE = dyn_cast<TypeExpr>(fn)) {
    // It's always a metatype type, so use the instance type name.
    auto instanceType =TE->getType()->castTo<MetatypeType>()->getInstanceType();
    
    // TODO: figure out right value for isKnownPrivate
    if (!instanceType->getAs<TupleType>()) {
      auto ctors = CS->TC.lookupConstructors(CS->DC, instanceType);
      for (auto ctor : ctors)
        if (ctor->hasType())
          candidates.push_back({ ctor, 1 });
    }

    declName = instanceType->getString();
    return;
  }

  if (auto *DSBI = dyn_cast<DotSyntaxBaseIgnoredExpr>(fn)) {
    collectCalleeCandidates(DSBI->getRHS());
    return;
  }

  if (auto AE = dyn_cast<ApplyExpr>(fn)) {
    collectCalleeCandidates(AE->getFn());

    // If we found a candidate list with a recursive walk, try adjust the curry
    // level for the applied subexpression in this call.
    if (!candidates.empty()) {
      for (auto &C : candidates)
        C.level += 1;
      return;
    }
  }

  if (auto *OVE = dyn_cast<OpenExistentialExpr>(fn)) {
    collectCalleeCandidates(OVE->getSubExpr());
    return;
  }

  if (auto *CFCE = dyn_cast<CovariantFunctionConversionExpr>(fn)) {
    collectCalleeCandidates(CFCE->getSubExpr());
    return;
  }


  // Otherwise, we couldn't tell structurally what is going on here, so try to
  // dig something out of the constraint system.
  unsigned uncurryLevel = 0;

  // The candidate list of an unresolved_dot_expr is the candidate list of the
  // base uncurried by one level, and we refer to the name of the member, not to
  // the name of any base.
  if (auto UDE = dyn_cast<UnresolvedDotExpr>(fn)) {
    declName = UDE->getName().str().str();
    uncurryLevel = 1;

    // If we actually resolved the member to use, return it.
    auto loc = CS->getConstraintLocator(UDE, ConstraintLocator::Member);
    if (auto *member = findResolvedMemberRef(loc, *CS)) {
      candidates.push_back({ member, uncurryLevel });
      return;
    }
    // Otherwise, look for a disjunction constraint explaining what the set is.
  }
  
  // Calls to super.init() are automatically uncurried one level.
  if (auto *UCE = dyn_cast<UnresolvedConstructorExpr>(fn)) {
    uncurryLevel = 1;

    auto selfTy = UCE->getSubExpr()->getType()->getLValueOrInOutObjectType();
    if (selfTy->hasTypeVariable())
      declName = "init";
    else
      declName = selfTy.getString() + ".init";
  }
  
  if (isa<MemberRefExpr>(fn))
    uncurryLevel = 1;

  // Scan to see if we have a disjunction constraint for this callee.
  for (auto &constraint : CS->getConstraints()) {
    if (constraint.getKind() != ConstraintKind::Disjunction) continue;
    
    auto locator = constraint.getLocator();
    if (!locator || locator->getAnchor() != fn) continue;
    
    for (auto *bindOverload : constraint.getNestedConstraints()) {
      if (bindOverload->getKind() != ConstraintKind::BindOverload)
        continue;
      auto c = bindOverload->getOverloadChoice();
      if (c.isDecl())
        candidates.push_back({ c.getDecl(), uncurryLevel });
    }

    // If we found some candidates, then we're done.
    if (candidates.empty()) continue;
    
    if (declName.empty())
      declName = candidates[0].decl->getNameStr().str();
    return;
  }
}

/// After the candidate list is formed, it can be filtered down to discard
/// obviously mismatching candidates and compute a "closeness" for the
/// resultant set.
void CalleeCandidateInfo::filterList(ArrayRef<TupleTypeElt> actualArgs) {
  // Now that we have the candidate list, figure out what the best matches from
  // the candidate list are, and remove all the ones that aren't at that level.
  filterList([&](UncurriedCandidate candidate) -> CandidateCloseness {
    auto inputType = candidate.getArgumentType();
    // If this isn't a function or isn't valid at this uncurry level, treat it
    // as a general mismatch.
    if (!inputType) return CC_GeneralMismatch;
    return evaluateCloseness(inputType, actualArgs);
  });
}

void CalleeCandidateInfo::filterContextualMemberList(Expr *argExpr) {
  auto URT = CS->getASTContext().TheUnresolvedType;

  // If the argument is not present then we expect members without arguments.
  if (!argExpr) {
    return filterList([&](UncurriedCandidate candidate) -> CandidateCloseness {
      auto inputType = candidate.getArgumentType();
      // If this candidate has no arguments, then we're a match.
      if (!inputType) return CC_ExactMatch;
      
      // Otherwise, if this is a function candidate with an argument, we
      // mismatch argument count.
      return CC_ArgumentCountMismatch;
    });
  }
  
  // Build an argument list type to filter against based on the expression we
  // have.  This really just provides us a structure to match against.
  // Normally, an argument list is a TupleExpr or a ParenExpr, though sometimes
  // the ParenExpr goes missing.
  auto *argTuple = dyn_cast<TupleExpr>(argExpr);
  if (!argTuple) {
    // If we have a single argument, look through the paren expr.
    if (auto *PE = dyn_cast<ParenExpr>(argExpr))
      argExpr = PE->getSubExpr();
    
    Type argType = URT;
    // If the argument has an & specified, then we expect an lvalue.
    if (isa<InOutExpr>(argExpr))
      argType = LValueType::get(argType);
    
    return filterList(TupleTypeElt(argType));
  }
  
  // If we have a tuple expression, form a tuple type.
  SmallVector<TupleTypeElt, 4> ArgElts;
  for (unsigned i = 0, e = argTuple->getNumElements(); i != e; ++i) {
    // If the argument has an & specified, then we expect an lvalue.
    Type argType = URT;
    if (isa<InOutExpr>(argTuple->getElement(i)))
      argType = LValueType::get(argType);

    
    ArgElts.push_back({ argType, argTuple->getElementName(i) });
  }

  return filterList(ArgElts);

}


CalleeCandidateInfo::CalleeCandidateInfo(ConstraintLocator *locator,
                                         ConstraintSystem *CS) : CS(CS) {
  if (auto decl = findResolvedMemberRef(locator, *CS)) {
    // If the decl is fully resolved, add it.
    candidates.push_back({ decl, 0 });
  } else {
    // Otherwise, look for a disjunction between different candidates.
    for (auto &constraint : CS->getConstraints()) {
      if (constraint.getLocator() != locator) continue;
      
      // Okay, we found our constraint.  Check to see if it is a disjunction.
      if (constraint.getKind() != ConstraintKind::Disjunction) continue;
      
      for (auto *bindOverload : constraint.getNestedConstraints()) {
        auto c = bindOverload->getOverloadChoice();
        if (c.isDecl())
          candidates.push_back({ c.getDecl(), 0 });
      }
    }
  }
}

CalleeCandidateInfo::CalleeCandidateInfo(ArrayRef<OverloadChoice> overloads,
                                         unsigned uncurryLevel,
                                         ConstraintSystem *CS) : CS(CS) {
  for (auto cand : overloads) {
    if (cand.isDecl())
      candidates.push_back({ cand.getDecl(), uncurryLevel });
  }
}


/// Given a set of parameter lists from an overload group, and a list of
/// arguments, emit a diagnostic indicating any partially matching overloads.
void CalleeCandidateInfo::
suggestPotentialOverloads(StringRef functionName, SourceLoc loc,
                          bool isCallExpr) {
  
  // If the candidate list is has no near matches to the actual types, don't
  // print out a candidate list, it will just be noise.
  if (closeness == CC_GeneralMismatch) {
    
    // FIXME: This is arbitrary, we should use the same policy for operators.
    if (!isCallExpr)
      return;
  }
  
  std::string suggestionText = "";
  std::set<std::string> dupes;
  
  // FIXME2: For (T,T) & (Self, Self), emit this as two candidates, one using
  // the LHS and one using the RHS type for T's.
  for (auto cand : candidates) {
    Type paramListType;

    if (auto *SD = dyn_cast<SubscriptDecl>(cand.decl)) {
      paramListType = SD->getIndicesType();
    } else {
      paramListType = cand.getArgumentType();
    }
    if (paramListType.isNull())
      continue;
    
    // If we've already seen this (e.g. decls overridden on the result type),
    // ignore this one.
    auto name = getTypeListString(paramListType);
    if (!dupes.insert(name).second)
      continue;
    
    if (!suggestionText.empty())
      suggestionText += ", ";
    suggestionText += name;
  }
  
  if (suggestionText.empty())
    return;
  
  if (dupes.size() == 1) {
    CS->TC.diagnose(loc, diag::suggest_expected_match, suggestionText);
  } else {
    CS->TC.diagnose(loc, diag::suggest_partial_overloads, functionName,
                    suggestionText);
  }
}

/// Flags that can be used to control name lookup.
enum TCCFlags {
  /// Allow the result of the subexpression to be an lvalue.  If this is not
  /// specified, any lvalue will be forced to be loaded into an rvalue.
  TCC_AllowLValue = 0x01,
  
  /// Re-type-check the given subexpression even if the expression has already
  /// been checked already.  The client is asserting that infinite recursion is
  /// not possible because it has relaxed a constraint on the system.
  TCC_ForceRecheck = 0x02
};

typedef OptionSet<TCCFlags> TCCOptions;

inline TCCOptions operator|(TCCFlags flag1, TCCFlags flag2) {
  return TCCOptions(flag1) | flag2;
}


namespace {
/// If a constraint system fails to converge on a solution for a given
/// expression, this class can produce a reasonable diagnostic for the failure
/// by analyzing the remnants of the failed constraint system. (Specifically,
/// left-over inactive, active and failed constraints.)
/// This class does not tune its diagnostics for a specific expression kind,
/// for that, you'll want to use an instance of the FailureDiagnosis class.
class FailureDiagnosis :public ASTVisitor<FailureDiagnosis, /*exprresult*/bool>{
  friend class ASTVisitor<FailureDiagnosis, /*exprresult*/bool>;
  
  Expr *expr = nullptr;
  ConstraintSystem *const CS;
public:
  FailureDiagnosis(Expr *expr, ConstraintSystem *cs) : expr(expr), CS(cs) {
    assert(expr && CS);
  }

  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(ArgTypes &&...Args) {
    return CS->TC.diagnose(std::forward<ArgTypes>(Args)...);
  }

  /// Attempt to diagnose a failure without taking into account the specific
  /// kind of expression that could not be type checked.
  bool diagnoseConstraintFailure();

  /// Unless we've already done this, retypecheck the specified child of the
  /// current expression on its own, without including any contextual
  /// constraints or the parent expr nodes.  This is more likely to succeed than
  /// type checking the original expression.
  ///
  /// This mention may only be used on immediate children of the current expr
  /// node, because ClosureExpr parameters need to be treated specially.
  ///
  /// This can return a new expression (for e.g. when a UnresolvedDeclRef gets
  /// resolved) and returns null when the subexpression fails to typecheck.
  ///
  Expr *typeCheckChildIndependently(Expr *subExpr, Type convertType = Type(),
                          ContextualTypePurpose convertTypePurpose = CTP_Unused,
                                    TCCOptions options = TCCOptions(),
                                    ExprTypeCheckListener *listener = nullptr);
  Expr *typeCheckChildIndependently(Expr *subExpr, TCCOptions options) {
    return typeCheckChildIndependently(subExpr, Type(), CTP_Unused, options);
  }

  Type getTypeOfTypeCheckedChildIndependently(Expr *subExpr,
                                            TCCOptions options = TCCOptions()) {
    auto e = typeCheckChildIndependently(subExpr, options);
    return e ? e->getType() : Type();
  }

  /// This is the same as typeCheckChildIndependently, but works on an arbitrary
  /// subexpression of the current node because it handles ClosureExpr parents
  /// of the specified node.
  Expr *typeCheckArbitrarySubExprIndependently(Expr *subExpr,
                                             TCCOptions options = TCCOptions());

  /// Special magic to handle inout exprs and tuples in argument lists.
  Expr *typeCheckArgumentChildIndependently(Expr *argExpr, Type argType,
                                        const CalleeCandidateInfo &candidates);

  /// Attempt to diagnose a specific failure from the info we've collected from
  /// the failed constraint system.
  bool diagnoseExprFailure();

  /// Emit an ambiguity diagnostic about the specified expression.
  void diagnoseAmbiguity(Expr *E);

  /// Attempt to produce a diagnostic for a mismatch between an expression's
  /// type and its assumed contextual type.
  bool diagnoseContextualConversionError();


private:
    
  /// Produce a diagnostic for a general member-lookup failure (irrespective of
  /// the exact expression kind).
  bool diagnoseGeneralMemberFailure(Constraint *constraint);
  
  /// Given a result of name lookup that had no viable results, diagnose the
  /// unviable ones.
  void diagnoseUnviableLookupResults(MemberLookupResult &lookupResults,
                                     Type baseObjTy, Expr *baseExpr,
                                     DeclName memberName, SourceLoc nameLoc,
                                     SourceLoc loc);
  
  /// Produce a diagnostic for a general overload resolution failure
  /// (irrespective of the exact expression kind).
  bool diagnoseGeneralOverloadFailure(Constraint *constraint);
  
  /// Produce a diagnostic for a general conversion failure (irrespective of the
  /// exact expression kind).
  bool diagnoseGeneralConversionFailure(Constraint *constraint);
     
  bool visitExpr(Expr *E);
  bool visitIdentityExpr(IdentityExpr *E);
  bool visitTupleExpr(TupleExpr *E);
  
  bool visitUnresolvedMemberExpr(UnresolvedMemberExpr *E);
  bool visitArrayExpr(ArrayExpr *E);
  bool visitDictionaryExpr(DictionaryExpr *E);

  bool visitForceValueExpr(ForceValueExpr *FVE);
  bool visitBindOptionalExpr(BindOptionalExpr *BOE);

  bool visitBinaryExpr(BinaryExpr *BE);
  bool visitUnaryExpr(ApplyExpr *AE);
  bool visitPrefixUnaryExpr(PrefixUnaryExpr *PUE) {
    return visitUnaryExpr(PUE);
  }
  bool visitPostfixUnaryExpr(PostfixUnaryExpr *PUE) {
    return visitUnaryExpr(PUE);
  }
  
  bool visitSubscriptExpr(SubscriptExpr *SE);
  bool visitCallExpr(CallExpr *CE);
  bool visitAssignExpr(AssignExpr *AE);
  bool visitInOutExpr(InOutExpr *IOE);
  bool visitCoerceExpr(CoerceExpr *CE);
  bool visitIfExpr(IfExpr *IE);
  bool visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E);
  bool visitClosureExpr(ClosureExpr *CE);
};
} // end anonymous namespace.



static bool isMemberConstraint(Constraint *C) {
  return C->getClassification() == ConstraintClassification::Member;
}

static bool isOverloadConstraint(Constraint *C) {
  if (C->getKind() == ConstraintKind::BindOverload)
    return true;

  if (C->getKind() != ConstraintKind::Disjunction)
    return false;
  
  return C->getNestedConstraints().front()->getKind() ==
    ConstraintKind::BindOverload;
}

/// Return true if this constraint is a conversion or requirement between two
/// types.
static bool isConversionConstraint(const Constraint *C) {
  return C->getClassification() == ConstraintClassification::Relational;
}

/// Return true if this member constraint is a low priority for diagnostics, so
/// low that we would only like to issue an error message about it if there is
/// nothing else interesting we can scrape out of the constraint system.
static bool isLowPriorityConstraint(Constraint *C) {
  // If the member constraint is a ".Element" lookup to find the element type of
  // a generator in a foreach loop, then it is very low priority: We will get a
  // better and more useful diagnostic from the failed conversion to
  // SequenceType that will fail as well.
  if (C->getKind() == ConstraintKind::TypeMember) {
    if (auto *loc = C->getLocator())
      for (auto Elt : loc->getPath())
        if (Elt.getKind() ==  ConstraintLocator::GeneratorElementType)
          return true;
  }

  return false;
}

/// Attempt to diagnose a failure without taking into account the specific
/// kind of expression that could not be type checked.
bool FailureDiagnosis::diagnoseConstraintFailure() {
  // This is the priority order in which we handle constraints.  Things earlier
  // in the list are considered to have higher specificity (and thus, higher
  // priority) than things lower in the list.
  enum ConstraintRanking {
    CR_MemberConstraint,
    CR_OverloadConstraint,
    CR_ConversionConstraint,
    CR_OtherConstraint
  };

  // Start out by classifying all the constraints.
  typedef std::pair<Constraint*, ConstraintRanking> RCElt;
  std::vector<RCElt> rankedConstraints;

  // This is a predicate that classifies constraints according to our
  // priorities.
  std::function<void (Constraint*)> classifyConstraint = [&](Constraint *C) {
    if (isLowPriorityConstraint(C))
      return rankedConstraints.push_back({C, CR_OtherConstraint});

    if (isMemberConstraint(C))
      return rankedConstraints.push_back({C, CR_MemberConstraint});

    if (isOverloadConstraint(C))
      return rankedConstraints.push_back({C, CR_OverloadConstraint});

    if (isConversionConstraint(C))
      return rankedConstraints.push_back({C, CR_ConversionConstraint});

    // We occassionally end up with disjunction constraints containing an
    // original constraint along with one considered with a fix.  If we find
    // this situation, add the original one to our list for diagnosis.
    if (C->getKind() == ConstraintKind::Disjunction) {
      Constraint *Orig = nullptr;
      bool AllOthersHaveFixes = true;
      for (auto DC : C->getNestedConstraints()) {
        // If this is a constraint inside of the disjunction with a fix, ignore
        // it.
        if (DC->getFix())
          continue;

        // If we already found a candidate without a fix, we can't do this.
        if (Orig) {
          AllOthersHaveFixes = false;
          break;
        }

        // Remember this as the exemplar to use.
        Orig = DC;
      }

      if (Orig && AllOthersHaveFixes)
        return classifyConstraint(Orig);
    }

    return rankedConstraints.push_back({C, CR_OtherConstraint});
  };
  
  // Look at the failed constraint and the general constraint list.  Processing
  // the failed constraint first slightly biases it in the ranking ahead of
  // other failed constraints at the same level.
  if (CS->failedConstraint)
    classifyConstraint(CS->failedConstraint);
  for (auto &C : CS->getConstraints())
    classifyConstraint(&C);

  // Okay, now that we've classified all the constraints, sort them by their
  // priority and priviledge the favored constraints.
  std::stable_sort(rankedConstraints.begin(), rankedConstraints.end(),
                   [&] (RCElt LHS, RCElt RHS) {
    // Rank things by their kind as the highest priority.
    if (LHS.second < RHS.second)
      return true;
    if (LHS.second > RHS.second)
      return false;
    // Next priority is favored constraints.
    if (LHS.first->isFavored() != RHS.first->isFavored())
      return LHS.first->isFavored();
    return false;
  });
 
  // Now that we have a sorted precedence of constraints to diagnose, charge
  // through them.
  for (auto elt : rankedConstraints) {
    auto C = elt.first;
    if (isMemberConstraint(C) && diagnoseGeneralMemberFailure(C))
      return true;

    if (isOverloadConstraint(C) && diagnoseGeneralOverloadFailure(C))
      return true;

    if (isConversionConstraint(C) && diagnoseGeneralConversionFailure(C))
      return true;

    // TODO: There can be constraints that aren't handled here!  When this
    // happens, we end up diagnosing them as ambiguities that don't make sense.
    // This isn't as bad as it seems though, because most of these will be
    // diagnosed by expr diagnostics.
  }
  
  // Otherwise, all the constraints look ok, diagnose this as an ambiguous
  // expression.
  return false;
}


bool FailureDiagnosis::diagnoseGeneralMemberFailure(Constraint *constraint) {
  assert(isMemberConstraint(constraint));
  
  auto memberName = constraint->getMember();
  
  // Get the referenced expression from the failed constraint.
  auto anchor = expr;
  SourceRange range = anchor->getSourceRange();
  if (auto locator = constraint->getLocator()) {
    locator = simplifyLocator(*CS, locator, range);
    if (locator->getAnchor())
      anchor = locator->getAnchor();
  }

  // Retypecheck the anchor type, which is the base of the member expression.
  anchor = typeCheckArbitrarySubExprIndependently(anchor, TCC_AllowLValue);
  if (!anchor) return true;
  
  auto baseTy = anchor->getType();
  auto baseObjTy = baseTy->getRValueType();

  // If the base type is an IUO, look through it.  Odds are, the code is not
  // trying to find a member of it.
  if (auto objTy = CS->lookThroughImplicitlyUnwrappedOptionalType(baseObjTy))
    baseObjTy = objTy;

  
  if (auto moduleTy = baseObjTy->getAs<ModuleType>()) {
    diagnose(anchor->getLoc(), diag::no_member_of_module,
             moduleTy->getModule()->getName(), memberName)
      .highlight(anchor->getSourceRange()).highlight(range);
    return true;
  }
  
  // If the base of this property access is a function that takes an empty
  // argument list, then the most likely problem is that the user wanted to
  // call the function, e.g. in "a.b.c" where they had to write "a.b().c".
  // Produce a specific diagnostic + fixit for this situation.
  if (auto baseFTy = baseObjTy->getAs<AnyFunctionType>()) {
    if (baseFTy->getInput()->isVoid()) {
      SourceLoc insertLoc = anchor->getEndLoc();
    
      if (auto *DRE = dyn_cast<DeclRefExpr>(anchor)) {
        diagnose(anchor->getLoc(), diag::did_not_call_function,
                 DRE->getDecl()->getName())
          .fixItInsertAfter(insertLoc, "()");
        return true;
      }
      
      if (auto *DSCE = dyn_cast<DotSyntaxCallExpr>(anchor))
        if (auto *DRE = dyn_cast<DeclRefExpr>(DSCE->getFn())) {
          diagnose(anchor->getLoc(), diag::did_not_call_method,
                   DRE->getDecl()->getName())
            .fixItInsertAfter(insertLoc, "()");
          return true;
        }
      
      diagnose(anchor->getLoc(), diag::did_not_call_function_value)
      .fixItInsertAfter(insertLoc, "()");
      return true;
    }
  }

  if (baseObjTy->is<TupleType>()) {
    diagnose(anchor->getLoc(), diag::could_not_find_tuple_member,
             baseObjTy, memberName)
      .highlight(anchor->getSourceRange()).highlight(range);
    return true;
  }
  
  MemberLookupResult result = CS->performMemberLookup(baseObjTy, *constraint);

  switch (result.OverallResult) {
  case MemberLookupResult::Unsolved:
    // Diagnose 'super.init', which can only appear inside another initializer,
    // specially.
    if (memberName.isSimpleName(CS->TC.Context.Id_init) &&
        !baseObjTy->is<MetatypeType>()) {
      if (auto ctorRef = dyn_cast<UnresolvedConstructorExpr>(anchor)) {
        if (isa<SuperRefExpr>(ctorRef->getSubExpr())) {
          diagnose(anchor->getLoc(),
                   diag::super_initializer_not_in_initializer);
          return true;
        }
        
        // Suggest inserting '.dynamicType' to construct another object of the
        // same dynamic type.
        SourceLoc fixItLoc = ctorRef->getConstructorLoc().getAdvancedLoc(-1);
        
        // Place the '.dynamicType' right before the init.
        diagnose(anchor->getLoc(), diag::init_not_instance_member)
        .fixItInsert(fixItLoc, ".dynamicType");
        return true;
      }
    }
      
    // If we couldn't resolve a specific type for the base expression, then we
    // cannot produce a specific diagnostic.
    return false;

  case MemberLookupResult::ErrorAlreadyDiagnosed:
    // If an error was already emitted, then we're done, don't emit anything
    // redundant.
    return true;
      
  case MemberLookupResult::HasResults:
    break;
  }
  
  // We know that this is a failing lookup, so we should have no viable
  // candidates here.
  if (!result.ViableCandidates.empty())
    return false;

  diagnoseUnviableLookupResults(result, baseObjTy, anchor, memberName,
                                range.Start, anchor->getLoc());
  return true;
}


/// Given a result of name lookup that had no viable results, diagnose the
/// unviable ones.
void FailureDiagnosis::
diagnoseUnviableLookupResults(MemberLookupResult &result, Type baseObjTy,
                              Expr *baseExpr,
                              DeclName memberName, SourceLoc nameLoc,
                              SourceLoc loc) {
  SourceRange baseRange = baseExpr ? baseExpr->getSourceRange() : SourceRange();
  
  // If we found no results at all, mention that fact.
  if (result.UnviableCandidates.empty()) {
    // TODO: This should handle tuple member lookups, like x.1231 as well.
    if (auto MTT = baseObjTy->getAs<MetatypeType>())
      diagnose(loc, diag::could_not_find_type_member,
               MTT->getInstanceType(), memberName)
        .highlight(baseRange).highlight(nameLoc);
    else
      diagnose(loc, diag::could_not_find_value_member,
               baseObjTy, memberName)
        .highlight(baseRange).highlight(nameLoc);
    return;
  }
  
  // Otherwise, we have at least one (and potentially many) viable candidates
  // sort them out.  If all of the candidates have the same problem (commonly
  // because there is exactly one candidate!) diagnose this.
  bool sameProblem = true;
  auto firstProblem = result.UnviableCandidates[0].second;
  for (auto cand : result.UnviableCandidates)
    sameProblem &= cand.second == firstProblem;
  
  auto instanceTy = baseObjTy;
  if (auto *MTT = instanceTy->getAs<AnyMetatypeType>())
    instanceTy = MTT->getInstanceType();
  
  if (sameProblem) {
    switch (firstProblem) {
    case MemberLookupResult::UR_LabelMismatch:
      break;
    case MemberLookupResult::UR_UnavailableInExistential:
      diagnose(loc, diag::could_not_use_member_on_existential,
               instanceTy, memberName)
        .highlight(baseRange).highlight(nameLoc);
      return;
    case MemberLookupResult::UR_InstanceMemberOnType:
      diagnose(loc, diag::could_not_use_instance_member_on_type,
               instanceTy, memberName)
        .highlight(baseRange).highlight(nameLoc);
      return;
    case MemberLookupResult::UR_TypeMemberOnInstance:
      diagnose(loc, diag::could_not_use_type_member_on_instance,
               baseObjTy, memberName)
        .highlight(baseRange).highlight(nameLoc);
      return;
        
    case MemberLookupResult::UR_MutatingMemberOnRValue:
    case MemberLookupResult::UR_MutatingGetterOnRValue:
      auto diagIDsubelt = diag::cannot_pass_rvalue_mutating_subelement;
      auto diagIDmember = diag::cannot_pass_rvalue_mutating;
      if (firstProblem == MemberLookupResult::UR_MutatingGetterOnRValue) {
        diagIDsubelt = diag::cannot_pass_rvalue_mutating_getter_subelement;
        diagIDmember = diag::cannot_pass_rvalue_mutating_getter;
      }
      assert(baseExpr && "Cannot have a mutation failure without a base");
      diagnoseSubElementFailure(baseExpr, loc, *CS,
                                diagIDsubelt, diagIDmember);
      return;
    }
  }

  // FIXME: Emit candidate set....
  
  
  // Otherwise, we don't have a specific issue to diagnose.  Just say the vague
  // 'cannot use' diagnostic.
  if (!baseObjTy->isEqual(instanceTy))
    diagnose(loc, diag::could_not_use_type_member,
             instanceTy, memberName)
    .highlight(baseRange).highlight(nameLoc);
  else
    diagnose(loc, diag::could_not_use_value_member,
             baseObjTy, memberName)
    .highlight(baseRange).highlight(nameLoc);
  return;
}

// In the absense of a better conversion constraint failure, point out the
// inability to find an appropriate overload.
bool FailureDiagnosis::diagnoseGeneralOverloadFailure(Constraint *constraint) {
  Constraint *bindOverload = constraint;
  if (constraint->getKind() == ConstraintKind::Disjunction)
    bindOverload = constraint->getNestedConstraints().front();

  auto overloadChoice = bindOverload->getOverloadChoice();
  std::string overloadName = overloadChoice.getDecl()->getNameStr();

  if (auto *CD = dyn_cast<ConstructorDecl>(overloadChoice.getDecl()))
    if (auto *SD = CD->getImplicitSelfDecl())
      overloadName = SD->getType()->getInOutObjectType().getString() + ".init";

  // Get the referenced expression from the failed constraint.
  auto anchor = expr;
  if (auto locator = bindOverload->getLocator()) {
    anchor = simplifyLocatorToAnchor(*CS, locator);
    if (!anchor)
      anchor = locator->getAnchor();
  }

  // The anchor for the constraint is almost always an OverloadedDeclRefExpr or
  // UnresolvedDotExpr.  Look at the parent node in the AST to find the Apply to
  // give a better diagnostic.
  Expr *call = expr->getParentMap()[anchor];
  // We look through some simple things that get in between the overload set
  // and the apply.
  while (call &&
         (isa<IdentityExpr>(call) ||
          isa<TryExpr>(call) || isa<ForceTryExpr>(call))) {
    call = expr->getParentMap()[call];
  }
  
  // FIXME: This is only needed because binops don't respect contextual types.
  if (isa<ApplyExpr>(call))
    return false;

  // This happens, for example, with ambiguous OverloadedDeclRefExprs. We should
  // just implement visitOverloadedDeclRefExprs and nuke this.
  
  // If we couldn't resolve an argument, then produce a generic "ambiguity"
  // diagnostic.
  diagnose(anchor->getLoc(), diag::ambiguous_member_overload_set,
           overloadName)
    .highlight(anchor->getSourceRange());

  if (constraint->getKind() == ConstraintKind::Disjunction) {
    for (auto elt : constraint->getNestedConstraints()) {
      if (elt->getKind() != ConstraintKind::BindOverload) continue;
      auto candidate = elt->getOverloadChoice().getDecl();
      diagnose(candidate, diag::found_candidate);
    }
  }

  return true;
}

static bool isUnresolvedOrTypeVarType(Type ty) {
  return ty->is<TypeVariableType>() || ty->is<UnresolvedType>();
}

bool FailureDiagnosis::diagnoseGeneralConversionFailure(Constraint *constraint){
  auto anchor = expr;
  if (auto locator = constraint->getLocator()) {
    anchor = simplifyLocatorToAnchor(*CS, locator);
    if (!anchor)
      anchor = locator->getAnchor();
  }

  Type fromType = CS->simplifyType(constraint->getFirstType());
  if (fromType->is<TypeVariableType>()) {
    TCCOptions options;
    
    // If we know we're removing a contextual constraint, then we can force a
    // type check of the subexpr because we know we're eliminating that
    // constraint.
    if (CS->getContextualTypePurpose() != CTP_Unused)
      options |= TCC_ForceRecheck;
      
    auto sub = typeCheckArbitrarySubExprIndependently(anchor, options);
    if (!sub) return true;
    fromType = sub->getType();
  }

  fromType = fromType->getRValueType();
  auto toType = CS->simplifyType(constraint->getSecondType());

  // If the second type is a type variable, the expression itself is
  // ambiguous.  Bail out so the general ambiguity diagnosing logic can handle
  // it.
  if (isUnresolvedOrTypeVarType(fromType) ||
      isUnresolvedOrTypeVarType(toType) ||
      // FIXME: Why reject unbound generic types here?
      fromType->is<UnboundGenericType>())
    return false;
  
  // Try to simplify irrelevant details of function types.  For example, if
  // someone passes a "() -> Float" function to a "() throws -> Int"
  // parameter, then uttering the "throws" may confuse them into thinking that
  // that is the problem, even though there is a clear subtype relation.
  if (auto srcFT = fromType->getAs<FunctionType>())
    if (auto destFT = toType->getAs<FunctionType>()) {
      auto destExtInfo = destFT->getExtInfo();

      if (!srcFT->isNoEscape()) destExtInfo = destExtInfo.withNoEscape(false);
      if (!srcFT->throws()) destExtInfo = destExtInfo.withThrows(false);
      if (destExtInfo != destFT->getExtInfo())
        toType = FunctionType::get(destFT->getInput(),
                                           destFT->getResult(), destExtInfo);

      // If this is a function conversion that discards throwability or
      // noescape, emit a specific diagnostic about that.
      if (srcFT->throws() && !destFT->throws()) {
        diagnose(expr->getLoc(), diag::throws_functiontype_mismatch,
                 fromType, toType)
        .highlight(expr->getSourceRange());
        return true;
      }

      if (srcFT->isNoEscape() && !destFT->isNoEscape()) {
        diagnose(expr->getLoc(), diag::noescape_functiontype_mismatch,
                 fromType, toType)
        .highlight(expr->getSourceRange());
        return true;
      }
    }

  // If this is a callee that mismatches an expected return type, we can emit a
  // very nice and specific error.  In this case, what we'll generally see is
  // a failed conversion constraint of "A -> B" to "_ -> C", where the error is
  // that B isn't convertible to C.
  if (CS->getContextualTypePurpose() == CTP_CalleeResult) {
    if (auto destFT = toType->getAs<FunctionType>()) {
      auto srcFT = fromType->getAs<FunctionType>();
      
      // If the constraint failure is from "T" to "_ -> C" where T isn't a
      // function type at all, then there is a more basic error here.
      if (!srcFT) {
        diagnose(expr->getLoc(), diag::cannot_call_non_function_value,
                 fromType)
          .highlight(expr->getSourceRange());
        return true;
      }
      
      if (!isUnresolvedOrTypeVarType(srcFT->getResult())) {
        // Otherwise, the error is that the result types mismatch.
        diagnose(expr->getLoc(), diag::invalid_callee_result_type,
                 srcFT->getResult(), destFT->getResult())
          .highlight(expr->getSourceRange());
        return true;
      }
    }
  }
  
  if (auto PT = toType->getAs<ProtocolType>()) {
    // Check for "=" converting to BooleanType.  The user probably meant ==.
    if (auto *AE = dyn_cast<AssignExpr>(expr->getValueProvidingExpr()))
      if (PT->getDecl()->isSpecificProtocol(KnownProtocolKind::BooleanType)) {
        diagnose(AE->getEqualLoc(), diag::use_of_equal_instead_of_equality)
        .fixItReplace(AE->getEqualLoc(), "==")
        .highlight(AE->getDest()->getLoc())
        .highlight(AE->getSrc()->getLoc());
        return true;
      }
 
    if (isa<NilLiteralExpr>(expr->getValueProvidingExpr())) {
      diagnose(expr->getLoc(), diag::cannot_use_nil_with_this_type, toType)
        .highlight(expr->getSourceRange());
      return true;
    }

    // Emit a conformance error through conformsToProtocol.  If this succeeds,
    // then keep searching.
    if (CS->TC.conformsToProtocol(fromType, PT->getDecl(), CS->DC,
                                  ConformanceCheckFlags::InExpression,
                                  nullptr, expr->getLoc()))
      return false;
    return true;
  }
  

  diagnose(anchor->getLoc(), diag::types_not_convertible,
           constraint->getKind() == ConstraintKind::Subtype,
           fromType, toType)
    .highlight(anchor->getSourceRange());

  // Check to see if this constraint came from a cast instruction. If so,
  // and if this conversion constraint is different than the types being cast,
  // produce a note that talks about the overall expression.
  if (constraint->getLocator() && constraint->getLocator()->getAnchor() ==expr){
    if (auto ECE = dyn_cast<ExplicitCastExpr>(expr))
      if (!toType->isEqual(ECE->getCastTypeLoc().getType()))
        diagnose(expr->getLoc(), diag::in_cast_expr_types,
                 ECE->getSubExpr()->getType()->getRValueType(),
                 ECE->getCastTypeLoc().getType()->getRValueType())
        .highlight(ECE->getSubExpr()->getSourceRange())
        .highlight(ECE->getCastTypeLoc().getSourceRange());
  }

  return true;
}

namespace {
  class ExprTypeSaver {
    llvm::DenseMap<Expr*, Type> ExprTypes;
    llvm::DenseMap<TypeLoc*, std::pair<Type, bool>> TypeLocTypes;
    llvm::DenseMap<Pattern*, Type> PatternTypes;
  public:

    void save(Expr *E) {
      struct TypeSaver : public ASTWalker {
        ExprTypeSaver *TS;
        TypeSaver(ExprTypeSaver *TS) : TS(TS) {}
        
        std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
          TS->ExprTypes[expr] = expr->getType();
          return { true, expr };
        }
        
        bool walkToTypeLocPre(TypeLoc &TL) override {
          if (TL.getTypeRepr() && TL.getType())
            TS->TypeLocTypes[&TL] = { TL.getType(), TL.wasValidated() };
          return true;
        }
        
        std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
          if (P->hasType())
            TS->PatternTypes[P] = P->getType();
          return { true, P };
        }
        
        // Don't walk into statements.  This handles the BraceStmt in
        // non-single-expr closures, so we don't walk into their body.
        std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
          return { false, S };
        }
      };
      
      E->walk(TypeSaver(this));
    }
    
    void restore(Expr *E) {
      for (auto exprElt : ExprTypes)
        exprElt.first->setType(exprElt.second);
      
      for (auto typelocElt : TypeLocTypes)
        typelocElt.first->setType(typelocElt.second.first,
                                  typelocElt.second.second);
      
      for (auto patternElt : PatternTypes)
        patternElt.first->setType(patternElt.second);
      
      // Done, don't do redundant work on destruction.
      ExprTypes.clear();
      TypeLocTypes.clear();
      PatternTypes.clear();
    }
    
    // On destruction, if a type got wiped out, reset it from null to its
    // original type.  This is helpful because type checking a subexpression
    // can lead to replacing the nodes in that subexpression.  However, the
    // failed ConstraintSystem still has locators pointing to the old nodes,
    // and if expr-specific diagnostics fail to turn up anything useful to say,
    // we go digging through failed constraints, and expect their locators to
    // still be meaningful.
    ~ExprTypeSaver() {
      for (auto exprElt : ExprTypes)
        if (!exprElt.first->getType())
          exprElt.first->setType(exprElt.second);
      
      for (auto typelocElt : TypeLocTypes)
        if (!typelocElt.first->getType())
          typelocElt.first->setType(typelocElt.second.first,
                                    typelocElt.second.second);
      
      for (auto patternElt : PatternTypes)
        if (!patternElt.first->hasType())
          patternElt.first->setType(patternElt.second);
    }
  };
}

/// \brief "Nullify" an expression tree's type data, to make it suitable for
/// re-typecheck operations.
static void eraseTypeData(Expr *expr) {
  /// Private class to "cleanse" an expression tree of types. This is done in the
  /// case of a typecheck failure, where we may want to re-typecheck partially-
  /// typechecked subexpressions in a context-free manner.
  class TypeNullifier : public ASTWalker {
  public:
    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // Preserve module expr type data to prevent further lookups.
      if (auto *declRef = dyn_cast<DeclRefExpr>(expr))
        if (isa<ModuleDecl>(declRef->getDecl()))
          return { false, expr };
      
      // Don't strip type info off OtherConstructorDeclRefExpr, because CSGen
      // doesn't know how to reconstruct it.
      if (isa<OtherConstructorDeclRefExpr>(expr))
        return { false, expr };

      // TypeExpr's are relabeled by CSGen.
      if (isa<TypeExpr>(expr))
        return { false, expr };

      // If a literal has a Builtin.Int or Builtin.FP type on it already,
      // then sema has already expanded out a call to
      //   Init.init(<builtinliteral>)
      // and we don't want it to make
      //   Init.init(Init.init(<builtinliteral>))
      // preserve the type info to prevent this from happening.
      if (isa<LiteralExpr>(expr))
        return { false, expr };
      
      expr->setType(nullptr);
      return { true, expr };
    }
    
    // If we find a TypeLoc (e.g. in an as? expr) with a type variable, rewrite
    // it.
    bool walkToTypeLocPre(TypeLoc &TL) override {
      if (TL.getTypeRepr())
        TL.setType(Type(), /*was validated*/false);
      return true;
    }
    
    std::pair<bool, Pattern*> walkToPatternPre(Pattern *pattern) override {
      pattern->setType(nullptr);
      return { true, pattern };
    }
    
    // Don't walk into statements.  This handles the BraceStmt in
    // non-single-expr closures, so we don't walk into their body.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return { false, S };
    }
  };
  
  expr->walk(TypeNullifier());
}


/// Erase an expression tree's open existentials after a re-typecheck operation.
///
/// This is done in the case of a typecheck failure, after we re-typecheck
/// partially-typechecked subexpressions in a context-free manner.
///
static void eraseOpenedExistentials(Expr *&expr) {

  class ExistentialEraser : public ASTWalker {
    llvm::SmallDenseMap<OpaqueValueExpr *, Expr *, 4> OpenExistentials;

  public:
    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      if (auto openExistentialExpr = dyn_cast<OpenExistentialExpr>(expr)) {
        auto archetypeVal = openExistentialExpr->getOpaqueValue();
        auto base = openExistentialExpr->getExistentialValue();
        bool inserted = OpenExistentials.insert({archetypeVal, base}).second;
        assert(inserted);
        (void) inserted;
        return { true, openExistentialExpr->getSubExpr() };
      }
      
      if (auto opaqueValueExpr = dyn_cast<OpaqueValueExpr>(expr)) {
        auto value = OpenExistentials.find(opaqueValueExpr);
        assert(value != OpenExistentials.end());
        return { true, value->second };
      }
      
      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      Type type = expr->getType();
      if (!type || !type->hasOpenedExistential())
        return expr;

      type = type.transform([&](Type type) -> Type {
        if (auto archetype = type->getAs<ArchetypeType>())
          if (auto existentialType = archetype->getOpenedExistentialType())
            return existentialType;

        return type;
      });
      expr->setType(type);

      return expr;
    }
    
    // Don't walk into statements.  This handles the BraceStmt in
    // non-single-expr closures, so we don't walk into their body.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return { false, S };
    }
  };

  expr->walk(ExistentialEraser());
}

/// Rewrite any type variables & archetypes in the specified type with
/// UnresolvedType.
static Type replaceArchetypesAndTypeVarsWithUnresolved(Type ty) {
  if (!ty) return ty;

 auto &ctx = ty->getASTContext();

  return ty.transform([&](Type type) -> Type {
    if (type->is<TypeVariableType>())
      return ctx.TheUnresolvedType;
    if (type->is<ArchetypeType>())
      return ctx.TheUnresolvedType;
    return type;
  });
}

/// Unless we've already done this, retypecheck the specified subexpression on
/// its own, without including any contextual constraints or parent expr
/// nodes.  This is more likely to succeed than type checking the original
/// expression.
///
/// This can return a new expression (for e.g. when a UnresolvedDeclRef gets
/// resolved) and returns null when the subexpression fails to typecheck.
Expr *FailureDiagnosis::
typeCheckChildIndependently(Expr *subExpr, Type convertType,
                            ContextualTypePurpose convertTypePurpose,
                            TCCOptions options,
                            ExprTypeCheckListener *listener) {
  
  // If this sub-expression is currently being diagnosed, refuse to recheck the
  // expression (which may lead to infinite recursion).  If the client is
  // telling us that it knows what it is doing, then believe it.
  if (!options.contains(TCC_ForceRecheck)) {
    if (Expr *res = CS->TC.isExprBeingDiagnosed(subExpr))
      return res;
  
    CS->TC.addExprForDiagnosis(subExpr, subExpr);
  }
  
  // If we have a conversion type, but it has type variables (from the current
  // ConstraintSystem), then we can't use it.
  if (convertType) {
    if (convertType->hasTypeVariable() || convertType->hasArchetype())
      convertType = replaceArchetypesAndTypeVarsWithUnresolved(convertType);
    
    // If the conversion type contains no info, drop it.
    if (convertType->is<UnresolvedType>() ||
        (convertType->is<MetatypeType>() && convertType->hasUnresolvedType())) {
      convertType = Type();
      convertTypePurpose = CTP_Unused;
    }
  }

  ExprTypeSaver SavedTypeData;
  SavedTypeData.save(subExpr);
  
  // Store off the sub-expression, in case a new one is provided via the
  // type check operation.
  Expr *preCheckedExpr = subExpr;

  eraseTypeData(subExpr);
      
  // Disable structural checks, because we know that the overall expression
  // has type constraint problems, and we don't want to know about any
  // syntactic issues in a well-typed subexpression (which might be because
  // the context is missing).
  TypeCheckExprOptions TCEOptions = TypeCheckExprFlags::DisableStructuralChecks;

  // Claim that the result is discarded to preserve the lvalue type of
  // the expression.
  if (options.contains(TCC_AllowLValue))
    TCEOptions |= TypeCheckExprFlags::IsDiscarded;

  // If there is no contextual type available, tell typeCheckExpression that it
  // is ok to produce an ambiguous result, it can just fill in holes with
  // UnresolvedType and we'll deal with it.
  if (!convertType)
    TCEOptions |= TypeCheckExprFlags::AllowUnresolvedTypeVariables;

  bool hadError = CS->TC.typeCheckExpression(subExpr, CS->DC, convertType,
                                             convertTypePurpose, TCEOptions,
                                             listener);

  // This is a terrible hack to get around the fact that typeCheckExpression()
  // might change subExpr to point to a new OpenExistentialExpr. In that case,
  // since the caller passed subExpr by value here, they would be left
  // holding on to an expression containing open existential types but
  // no OpenExistentialExpr, which breaks invariants enforced by the
  // ASTChecker.
  eraseOpenedExistentials(subExpr);
  
  // If recursive type checking failed, then an error was emitted.  Return
  // null to indicate this to the caller.
  if (hadError)
    return nullptr;

  // If we type checked the result but failed to get a usable output from it,
  // just pretend as though nothing happened.
  if (subExpr->getType()->is<ErrorType>()) {
    subExpr = preCheckedExpr;
    SavedTypeData.restore(subExpr);
  }
  
  CS->TC.addExprForDiagnosis(preCheckedExpr, subExpr);
  return subExpr;
}

/// This is the same as typeCheckChildIndependently, but works on an arbitrary
/// subexpression of the current node because it handles ClosureExpr parents
/// of the specified node.
Expr *FailureDiagnosis::
typeCheckArbitrarySubExprIndependently(Expr *subExpr, TCCOptions options) {
  if (subExpr == expr)
    return typeCheckChildIndependently(subExpr, options);
  
  // Construct a parent map for the expr tree we're investigating.
  auto parentMap = expr->getParentMap();
  
  ClosureExpr *NearestClosure = nullptr;
  
  // Walk the parents of the specified expression, handling any ClosureExprs.
  for (Expr *node = parentMap[subExpr]; node; node = parentMap[node]) {
    auto *CE = dyn_cast<ClosureExpr>(node);
    if (!CE) continue;
    
    // Keep track of the innermost closure we see that we're jumping into.
    if (!NearestClosure)
      NearestClosure = CE;
    
    // If we have a ClosureExpr parent of the specified node, check to make sure
    // none of its arguments are type variables.  If so, these type variables
    // would be accessible to name lookup of the subexpression and may thus leak
    // in.  Reset them to UnresolvedTypes for safe measures.
    CE->getParams()->forEachVariable([&](VarDecl *VD) {
      if (VD->getType()->hasTypeVariable() || VD->getType()->is<ErrorType>())
        VD->overwriteType(CS->getASTContext().TheUnresolvedType);
    });
  }

  // When we're type checking a single-expression closure, we need to reset the
  // DeclContext to this closure for the recursive type checking.  Otherwise,
  // if there is a closure in the subexpression, we can violate invariants.
  auto newDC = NearestClosure ? NearestClosure : CS->DC;
  llvm::SaveAndRestore<DeclContext*> SavedDC(CS->DC, newDC);
  
  // Otherwise, we're ok to type check the subexpr.
  return typeCheckChildIndependently(subExpr, options);
}


bool FailureDiagnosis::diagnoseContextualConversionError() {
  // If the constraint system has a contextual type, then we can test to see if
  // this is the problem that prevents us from solving the system.
  Type contextualType = CS->getContextualType();
  if (!contextualType)
    return false;

  // Try re-type-checking the expression without the contextual type to see if
  // it can work without it.  If so, the contextual type is the problem.  We
  // force a recheck, because "expr" is likely in our table with the extra
  // contextual constraint that we know we are relaxing.
  auto exprType = getTypeOfTypeCheckedChildIndependently(expr,TCC_ForceRecheck);
  
  // If it failed an diagnosed something, then we're done.
  if (!exprType) return true;

  // Try to find the contextual type in a variety of ways.  If the constraint
  // system had a contextual type specified, we use it - it will have a purpose
  // indicator which allows us to give a very "to the point" diagnostic.
  Diag<Type, Type> diagID;
  Diag<Type, Type> diagIDProtocol;
  Diag<Type> nilDiag;

  // If this is conversion failure due to a return statement with an argument
  // that cannot be coerced to the result type of the function, emit a
  // specific error.
  switch (CS->getContextualTypePurpose()) {
  case CTP_Unused:
  case CTP_CannotFail:
    llvm_unreachable("These contextual type purposes cannot fail with a "
                     "conversion type specified!");
  case CTP_CalleeResult:
    llvm_unreachable("CTP_CalleeResult does not actually install a "
                     "contextual type");
  case CTP_Initialization:
    diagID = diag::cannot_convert_initializer_value;
    diagIDProtocol = diag::cannot_convert_initializer_value_protocol;
    nilDiag = diag::cannot_convert_initializer_value_nil;
    break;
  case CTP_ReturnStmt:
    // Special case the "conversion to void" case.
    if (contextualType->isVoid()) {
      diagnose(expr->getLoc(), diag::cannot_return_value_from_void_func)
        .highlight(expr->getSourceRange());
      return true;
    }

    diagID = diag::cannot_convert_to_return_type;
    diagIDProtocol = diag::cannot_convert_to_return_type_protocol;
    nilDiag = diag::cannot_convert_to_return_type_nil;
    break;
  case CTP_ThrowStmt:
    // TODO: Better diagnostic for 'throw nil'.

    if (exprType->is<UnresolvedType>() || exprType->hasTypeVariable())
      return false;
      
    // The conversion destination of throw is always ErrorType (at the moment)
    // if this ever expands, this should be a specific form like () is for
    // return.
    diagnose(expr->getLoc(), diag::cannot_convert_thrown_type, exprType)
      .highlight(expr->getSourceRange());
    return true;
      
  case CTP_EnumCaseRawValue:
    diagID = diag::cannot_convert_raw_initializer_value;
    diagIDProtocol = diag::cannot_convert_raw_initializer_value;
    nilDiag = diag::cannot_convert_raw_initializer_value_nil;
    break;
  case CTP_DefaultParameter:
    diagID = diag::cannot_convert_default_arg_value;
    diagIDProtocol = diag::cannot_convert_default_arg_value_protocol;
    nilDiag = diag::cannot_convert_default_arg_value_nil;
    break;

  case CTP_CallArgument:
    diagID = diag::cannot_convert_argument_value;
    diagIDProtocol = diag::cannot_convert_argument_value_protocol;
    nilDiag = diag::cannot_convert_argument_value_nil;
    break;
  case CTP_ClosureResult:
    diagID = diag::cannot_convert_closure_result;
    diagIDProtocol = diag::cannot_convert_closure_result_protocol;
    nilDiag = diag::cannot_convert_closure_result_nil;
    break;
  case CTP_ArrayElement:
    diagID = diag::cannot_convert_array_element;
    diagIDProtocol = diag::cannot_convert_array_element_protocol;
    nilDiag = diag::cannot_convert_array_element_nil;
    break;
  case CTP_DictionaryKey:
    diagID = diag::cannot_convert_dict_key;
    diagIDProtocol = diag::cannot_convert_dict_key_protocol;
    nilDiag = diag::cannot_convert_dict_key_nil;
    break;
  case CTP_DictionaryValue:
    diagID = diag::cannot_convert_dict_value;
    diagIDProtocol = diag::cannot_convert_dict_value_protocol;
    nilDiag = diag::cannot_convert_dict_value_nil;
    break;
  case CTP_CoerceOperand:
    diagID = diag::cannot_convert_coerce;
    diagIDProtocol = diag::cannot_convert_coerce_protocol;
    nilDiag = diag::cannot_convert_coerce_nil;
    break;
  case CTP_AssignSource:
    diagID = diag::cannot_convert_assign;
    diagIDProtocol = diag::cannot_convert_assign_protocol;
    nilDiag = diag::cannot_convert_assign_nil;
    break;
  }

  // If we're diagnostic an issue with 'nil', produce a specific diagnostic,
  // instead of uttering NilLiteralConvertible.
  if (isa<NilLiteralExpr>(expr->getValueProvidingExpr())) {
    diagnose(expr->getLoc(), nilDiag, contextualType);
    return true;
  }
  
  // If we don't have a type for the expression, then we cannot use it in
  // conversion constraint diagnostic generation.
  if (exprType->is<UnresolvedType>() || exprType->hasTypeVariable()) {
    // We can't do anything smart.
    return false;
  }
  
  // When complaining about conversion to a protocol type, complain about
  // conformance instead of "conversion".
  if (contextualType->is<ProtocolType>() ||
      contextualType->is<ProtocolCompositionType>())
    diagID = diagIDProtocol;
  
  // Try to simplify irrelevant details of function types.  For example, if
  // someone passes a "() -> Float" function to a "() throws -> Int"
  // parameter, then uttering the "throws" may confuse them into thinking that
  // that is the problem, even though there is a clear subtype relation.
  if (auto srcFT = exprType->getAs<FunctionType>())
    if (auto destFT = contextualType->getAs<FunctionType>()) {
      auto destExtInfo = destFT->getExtInfo();
      
      if (!srcFT->isNoEscape()) destExtInfo = destExtInfo.withNoEscape(false);
      if (!srcFT->throws()) destExtInfo = destExtInfo.withThrows(false);
      if (destExtInfo != destFT->getExtInfo())
        contextualType = FunctionType::get(destFT->getInput(),
                                           destFT->getResult(), destExtInfo);

      // If this is a function conversion that discards throwability or
      // noescape, emit a specific diagnostic about that.
      if (srcFT->throws() && !destFT->throws())
        diagID = diag::throws_functiontype_mismatch;
      else if (srcFT->isNoEscape() && !destFT->isNoEscape())
        diagID = diag::noescape_functiontype_mismatch;
    }
  
  diagnose(expr->getLoc(), diagID, exprType, contextualType)
    .highlight(expr->getSourceRange());
  return true;
}


/// When an assignment to an expression is detected and the destination is
/// invalid, emit a detailed error about the condition.
void ConstraintSystem::diagnoseAssignmentFailure(Expr *dest, Type destTy,
                                                 SourceLoc equalLoc) {
  auto &TC = getTypeChecker();

  // Diagnose obvious assignments to literals.
  if (isa<LiteralExpr>(dest->getValueProvidingExpr())) {
    TC.diagnose(equalLoc, diag::cannot_assign_to_literal);
    return;
  }

  Diag<StringRef> diagID;
  if (isa<DeclRefExpr>(dest))
    diagID = diag::assignment_lhs_is_immutable_variable;
  else if (isa<ForceValueExpr>(dest))
    diagID = diag::assignment_bang_has_immutable_subcomponent;
  else if (isa<UnresolvedDotExpr>(dest) || isa<MemberRefExpr>(dest))
    diagID = diag::assignment_lhs_is_immutable_property;
  else if (isa<SubscriptExpr>(dest))
    diagID = diag::assignment_subscript_has_immutable_base;
  else {
    diagID = diag::assignment_lhs_is_immutable_variable;
  }
  
  diagnoseSubElementFailure(dest, equalLoc, *this, diagID,
                            diag::assignment_lhs_not_lvalue);
}


/// Special magic to handle inout exprs and tuples in argument lists.
Expr *FailureDiagnosis::
typeCheckArgumentChildIndependently(Expr *argExpr, Type argType,
                                    const CalleeCandidateInfo &candidates) {
  // Grab one of the candidates (if present) and get its input list to help
  // identify operators that have implicit inout arguments.
  Type exampleInputType;
  if (!candidates.empty()) {
    exampleInputType = candidates[0].getArgumentType();

    // If we found a single candidate, and have no contextually known argument
    // type information, use that one candidate as the type information for
    // subexpr checking.
    //
    // TODO: If all candidates have the same type for some argument, we could pass
    // down partial information.
    if (candidates.size() == 1 && !argType)
      argType = candidates[0].getArgumentType();
  }

  // FIXME: This should all just be a matter of getting type type of the
  // sub-expression, but this doesn't work well when typeCheckChildIndependently
  // is over-conservative w.r.t. TupleExprs.
  auto *TE = dyn_cast<TupleExpr>(argExpr);
  if (!TE) {
    // If the argument isn't a tuple, it is some scalar value for a
    // single-argument call.
    TCCOptions options;
    if (exampleInputType && exampleInputType->is<InOutType>())
      options |= TCC_AllowLValue;

    // If the argtype is a tuple type with default arguments, or a labeled tuple
    // with a single element, pull the scalar element type for the subexpression
    // out.  If we can't do that and the tuple has default arguments, we have to
    // punt on passing down the type information, since type checking the
    // subexpression won't be able to find the default argument provider.
    if (argType)
      if (auto argTT = argType->getAs<TupleType>()) {
        int scalarElt = argTT->getElementForScalarInit();
        if (scalarElt != -1)
          argType = argTT->getElementType(scalarElt);
        else if (argTT->hasAnyDefaultValues())
          argType = Type();
      }
    
    auto CTPurpose = argType ? CTP_CallArgument : CTP_Unused;
    return typeCheckChildIndependently(argExpr, argType,
                                       CTPurpose, options);
  }

  // If we know the requested argType to use, use computeTupleShuffle to produce
  // the shuffle of input arguments to destination values.  It requires a
  // TupleType to compute the mapping from argExpr.  Conveniently, it doesn't
  // care about the actual types though, so we can just use 'void' for them.
  if (argType && argType->is<TupleType>()) {
    auto argTypeTT = argType->castTo<TupleType>();
    SmallVector<TupleTypeElt, 4> ArgElts;
    auto voidTy = CS->getASTContext().TheEmptyTupleType;
    
    for (unsigned i = 0, e = TE->getNumElements(); i != e; ++i)
      ArgElts.push_back({ voidTy, TE->getElementName(i) });
    auto actualArgType = TupleType::get(ArgElts, CS->getASTContext());
    if (auto *actualArgTypeTT = actualArgType->getAs<TupleType>()) {
      SmallVector<int, 4> sources;
      SmallVector<unsigned, 4> variadicArgs;
      if (!computeTupleShuffle(actualArgTypeTT, argTypeTT,
                               sources, variadicArgs)) {
        SmallVector<Expr*, 4> resultElts(TE->getNumElements(), nullptr);
        SmallVector<TupleTypeElt, 4> resultEltTys(TE->getNumElements(), voidTy);

        // If we got a correct shuffle, we can perform the analysis of all of
        // the input elements, with their expected types.
        for (unsigned i = 0, e = sources.size(); i != e; ++i) {
          // If the value is taken from a default argument, ignore it.
          if (sources[i] == TupleShuffleExpr::DefaultInitialize ||
              sources[i] == TupleShuffleExpr::Variadic ||
              sources[i] == TupleShuffleExpr::CallerDefaultInitialize)
            continue;
          
          assert(sources[i] >= 0 && "Unknown sources index");
          
          // Otherwise, it must match the corresponding expected argument type.
          unsigned inArgNo = sources[i];
          auto actualType = argTypeTT->getElementType(i);

          TCCOptions options;
          if (actualType->is<InOutType>())
            options |= TCC_AllowLValue;

          auto exprResult =
            typeCheckChildIndependently(TE->getElement(inArgNo), actualType,
                                        CTP_CallArgument, options);
          // If there was an error type checking this argument, then we're done.
          if (!exprResult)
            return nullptr;

          // If the caller expected something inout, but we didn't have
          // something of inout type, diagnose it.
          if (auto IOE =
                dyn_cast<InOutExpr>(exprResult->getSemanticsProvidingExpr())) {
            if (!actualType->is<InOutType>()) {
              diagnose(exprResult->getLoc(), diag::extra_address_of,
                       exprResult->getType()->getInOutObjectType())
                .highlight(exprResult->getSourceRange())
                .fixItRemove(IOE->getStartLoc());
              return nullptr;
            }
          }
          
          resultElts[inArgNo] = exprResult;
          resultEltTys[inArgNo] = {
            exprResult->getType(),
            TE->getElementName(inArgNo)
          };
        }
        
        if (!variadicArgs.empty()) {
          auto varargsTy = argTypeTT->getVarArgsBaseType();
          for (unsigned i = 0, e = variadicArgs.size(); i != e; ++i) {
            unsigned inArgNo = variadicArgs[i];
            
            auto expr =
              typeCheckChildIndependently(TE->getElement(inArgNo), varargsTy,
                                          CTP_CallArgument);
            // If there was an error type checking this argument, then we're done.
            if (!expr)
              return nullptr;
            resultElts[inArgNo] = expr;
            resultEltTys[inArgNo] = { expr->getType() };
          }
        }
        
        auto TT = TupleType::get(resultEltTys, CS->getASTContext());
        return TupleExpr::create(CS->getASTContext(), TE->getLParenLoc(),
                                 resultElts, TE->getElementNames(),
                                 TE->getElementNameLocs(),
                                 TE->getRParenLoc(), TE->hasTrailingClosure(),
                                 TE->isImplicit(), TT);
      }
    }
  }
  
  // Get the simplified type of each element and rebuild the aggregate.
  SmallVector<TupleTypeElt, 4> resultEltTys;
  SmallVector<Expr*, 4> resultElts;

  TupleType *exampleInputTuple = nullptr;
  if (exampleInputType)
    exampleInputTuple = exampleInputType->getAs<TupleType>();

  for (unsigned i = 0, e = TE->getNumElements(); i != e; i++) {
    TCCOptions options;
    if (exampleInputTuple && i < exampleInputTuple->getNumElements() &&
        exampleInputTuple->getElementType(i)->is<InOutType>())
      options |= TCC_AllowLValue;

    auto elExpr = typeCheckChildIndependently(TE->getElement(i), options);
    if (!elExpr) return nullptr; // already diagnosed.
    
    resultElts.push_back(elExpr);
    resultEltTys.push_back({elExpr->getType(), TE->getElementName(i)});
  }
  
  auto TT = TupleType::get(resultEltTys, CS->getASTContext());
  return TupleExpr::create(CS->getASTContext(), TE->getLParenLoc(),
                           resultElts, TE->getElementNames(),
                           TE->getElementNameLocs(),
                           TE->getRParenLoc(), TE->hasTrailingClosure(),
                           TE->isImplicit(), TT);
}

bool FailureDiagnosis::visitBinaryExpr(BinaryExpr *binop) {
  CalleeCandidateInfo calleeInfo(binop->getFn(), CS);
  assert(!calleeInfo.candidates.empty() && "unrecognized binop function kind");

  auto checkedArgExpr = typeCheckArgumentChildIndependently(binop->getArg(),
                                                            Type(),
                                                            calleeInfo);
  if (!checkedArgExpr) return true;

  // Pre-checking can turn (T,U) into a TypeExpr.  That's an artifact
  // of independent type-checking; just use the standard diagnostics
  // paths.
  auto argExpr = dyn_cast<TupleExpr>(checkedArgExpr);
  if (!argExpr) return visitExpr(binop);

  auto argTupleType = argExpr->getType()->castTo<TupleType>();
  calleeInfo.filterList(argTupleType);

  std::string overloadName = calleeInfo[0].decl->getNameStr();
  assert(!overloadName.empty());

  auto lhsExpr = argExpr->getElement(0), rhsExpr = argExpr->getElement(1);
  auto lhsType = lhsExpr->getType()->getRValueType();
  auto rhsType = rhsExpr->getType()->getRValueType();

  // If this is a comparison against nil, then we should produce a specific
  // diagnostic.
  if (isa<NilLiteralExpr>(rhsExpr->getValueProvidingExpr()) &&
      !lhsType->hasTypeVariable() && !lhsType->is<UnresolvedType>()) {
    if (overloadName == "=="  || overloadName == "!=" ||
        overloadName == "===" || overloadName == "!==" ||
        overloadName == "<"   || overloadName == ">" ||
        overloadName == "<="  || overloadName == ">=") {
      diagnose(binop->getLoc(), diag::comparison_with_nil_illegal, lhsType)
      .highlight(lhsExpr->getSourceRange());
      return true;
    }
  }

  // Otherwise, whatever the result type of the call happened to be must not
  // have been what we were looking for - diagnose this as a conversion
  // failure.
  if (calleeInfo.closeness == CC_ExactMatch)
    return false;
  
  // A common error is to apply an operator that only has an inout LHS (e.g. +=)
  // to non-lvalues (e.g. a local let).  Produce a nice diagnostic for this
  // case.
  if (calleeInfo.closeness == CC_NonLValueInOut) {
    diagnoseSubElementFailure(argExpr->getElement(0), binop->getLoc(), *CS,
                              diag::cannot_apply_lvalue_binop_to_subelement,
                              diag::cannot_apply_lvalue_binop_to_rvalue);
    return true;
  }

  if (binop->isImplicit() && overloadName == "~=") {
    // This binop was synthesized when typechecking an expression pattern.
    diagnose(lhsExpr->getLoc(),
             diag::cannot_match_expr_pattern_with_value, lhsType, rhsType)
      .highlight(lhsExpr->getSourceRange())
      .highlight(rhsExpr->getSourceRange());
    return true;
  }
  
  if (!lhsType->isEqual(rhsType)) {
    diagnose(binop->getLoc(), diag::cannot_apply_binop_to_args,
             overloadName, lhsType, rhsType)
      .highlight(lhsExpr->getSourceRange())
      .highlight(rhsExpr->getSourceRange());
  } else {
    diagnose(binop->getLoc(), diag::cannot_apply_binop_to_same_args,
             overloadName, lhsType)
      .highlight(lhsExpr->getSourceRange())
      .highlight(rhsExpr->getSourceRange());
  }
  
  calleeInfo.suggestPotentialOverloads(overloadName, binop->getLoc());
  return true;
}


bool FailureDiagnosis::visitUnaryExpr(ApplyExpr *applyExpr) {
  assert(expr->getKind() == ExprKind::PostfixUnary ||
         expr->getKind() == ExprKind::PrefixUnary);

  CalleeCandidateInfo calleeInfo(applyExpr->getFn(), CS);
  assert(!calleeInfo.candidates.empty() && "unrecognized unop function kind");

  auto argExpr = typeCheckArgumentChildIndependently(applyExpr->getArg(),
                                                     Type(), calleeInfo);
  if (!argExpr) return true;

  Type argType = argExpr->getType();
  calleeInfo.filterList(argType);

  if (calleeInfo.closeness == CC_ExactMatch) {
    // Otherwise, whatever the result type of the call happened to be must not
    // have been what we were looking for.  Lets diagnose it as a conversion
    // or ambiguity failure.
    return false;
  }

  // A common error is to apply an operator that only has inout forms (e.g. ++)
  // to non-lvalues (e.g. a local let).  Produce a nice diagnostic for this
  // case.
  if (calleeInfo.closeness == CC_NonLValueInOut) {
    // Diagnose the case when the failure.
    diagnoseSubElementFailure(argExpr, applyExpr->getFn()->getLoc(), *CS,
                              diag::cannot_apply_lvalue_unop_to_subelement,
                              diag::cannot_apply_lvalue_unop_to_rvalue);
    return true;
  }

  std::string overloadName = calleeInfo[0].decl->getNameStr();
  assert(!overloadName.empty());
  diagnose(argExpr->getLoc(), diag::cannot_apply_unop_to_arg, overloadName,
           argType);
  
  calleeInfo.suggestPotentialOverloads(overloadName, argExpr->getLoc());
  return true;
}

bool FailureDiagnosis::visitSubscriptExpr(SubscriptExpr *SE) {
  // See if the subscript got resolved.
  auto locator =
    CS->getConstraintLocator(SE, ConstraintLocator::SubscriptMember);
  CalleeCandidateInfo calleeInfo(locator, CS);

  auto indexExpr = typeCheckArgumentChildIndependently(SE->getIndex(),
                                                       Type(), calleeInfo);
  if (!indexExpr) return true;

  auto baseExpr = typeCheckChildIndependently(SE->getBase());
  if (!baseExpr) return true;

  auto indexType = indexExpr->getType();
  auto baseType = baseExpr->getType();

  auto decomposedIndexType = decomposeArgumentType(indexType);
  calleeInfo.filterList([&](UncurriedCandidate cand) -> CandidateCloseness
  {
    // Classify how close this match is.  Non-subscript decls don't match.
    auto *SD = dyn_cast<SubscriptDecl>(cand.decl);
    if (!SD) return CC_GeneralMismatch;
    
    // Check to make sure the base expr type is convertible to the expected base
    // type.
    auto selfConstraint = CC_ExactMatch;
    auto instanceTy =
      SD->getGetter()->getImplicitSelfDecl()->getType()->getInOutObjectType();
    if (!baseType->hasTypeVariable() &&
        !baseType->hasUnresolvedType() &&
        !CS->TC.isConvertibleTo(baseType, instanceTy, CS->DC)) {
      selfConstraint = CC_SelfMismatch;
    }

    // Explode out multi-index subscripts to find the best match.
    return std::max(evaluateCloseness(SD->getIndicesType(),decomposedIndexType),
                    selfConstraint);
  });

  // TODO: Is there any reason to check for CC_NonLValueInOut here?
  
  if (calleeInfo.closeness == CC_ExactMatch) {
    // Otherwise, whatever the result type of the call happened to be must not
    // have been what we were looking for.  Lets diagnose it as a conversion
    // or ambiguity failure.
    if (calleeInfo.size() == 1)
      return false;

    diagnose(SE->getLoc(), diag::ambiguous_subscript, baseType, indexType)
      .highlight(indexExpr->getSourceRange())
      .highlight(baseExpr->getSourceRange());
    
    // FIXME: suggestPotentialOverloads should do this.
    //calleeInfo.suggestPotentialOverloads("subscript", SE->getLoc());
    for (auto candidate : calleeInfo.candidates)
      diagnose(candidate.decl, diag::found_candidate);

    return true;
  }

  // If the closes matches all mismatch on self, we either have something that
  // cannot be subscripted, or an ambiguity.
  if (calleeInfo.closeness == CC_SelfMismatch) {
    diagnose(SE->getLoc(), diag::cannot_subscript_base, baseType)
      .highlight(SE->getBase()->getSourceRange());
    // FIXME: Should suggest overload set, but we're not ready for that until
    // it points to candidates and identifies the self type in the diagnostic.
    //calleeInfo.suggestPotentialOverloads("subscript", SE->getLoc());
    return true;
  }

  diagnose(SE->getLoc(), diag::cannot_subscript_with_index,
           baseType, indexType);

  calleeInfo.suggestPotentialOverloads("subscript", SE->getLoc());
  return true;
}


namespace {
  /// Type checking listener for pattern binding initializers.
  class CalleeListener : public ExprTypeCheckListener {
    Type contextualType;
  public:
    explicit CalleeListener(Type contextualType)
      : contextualType(contextualType) { }

    virtual bool builtConstraints(ConstraintSystem &cs, Expr *expr) {
      // If we have no contextual type, there is nothing to do.
      if (!contextualType) return false;

      // If the expresion is obviously something that produces a metatype,
      // then don't put a constraint on it.
      auto semExpr = expr->getValueProvidingExpr();
      if (isa<TypeExpr>(semExpr) ||isa<UnresolvedConstructorExpr>(semExpr))
        return false;
      
      // We're making the expr have a function type, whose result is the same
      // as our contextual type.
      auto inputLocator =
        cs.getConstraintLocator(expr, ConstraintLocator::FunctionResult);

      auto tv = cs.createTypeVariable(inputLocator,
                                 TVO_CanBindToLValue|TVO_PrefersSubtypeBinding);

      // In order to make this work, we pick the most general function type and
      // use a conversion constraint.  This gives us:
      //    "$T0 throws -> contextualType"
      // this allows things that are throws and not throws, and allows escape
      // and noescape functions.
      auto extInfo = FunctionType::ExtInfo().withThrows();
      auto fTy = FunctionType::get(tv, contextualType, extInfo);

      auto locator = cs.getConstraintLocator(expr);

      // Add a conversion constraint between the types.
      cs.addConstraint(ConstraintKind::Conversion, expr->getType(),
                       fTy, locator, /*isFavored*/true);
      return false;
    }
  };
}

bool FailureDiagnosis::visitCallExpr(CallExpr *callExpr) {
  // Type check the function subexpression to resolve a type for it if possible.
  // If we have a contextual type, we use it to inform the result type of the
  // function.
  CalleeListener listener(CS->getContextualType());
  auto fnExpr = typeCheckChildIndependently(callExpr->getFn(), Type(),
                                            CTP_CalleeResult, TCCOptions(),
                                            &listener);
  if (!fnExpr) return true;
  
  auto fnType = fnExpr->getType()->getRValueType();

  // If we resolved a concrete expression for the callee, and it has
  // non-function/non-metatype type, then we cannot call it!
  if (!fnType->hasUnresolvedType() && !fnType->hasTypeVariable() &&
      !fnType->is<AnyFunctionType>() && !fnType->is<MetatypeType>()) {
    
    // If the argument is a trailing ClosureExpr (i.e. {....}) and it is on a
    // different line than the callee, then the "real" issue is that the user
    // forgot to write "do" before their brace stmt.
    if (auto *PE = dyn_cast<ParenExpr>(callExpr->getArg()))
      if (PE->hasTrailingClosure() && isa<ClosureExpr>(PE->getSubExpr())) {
        auto &SM = CS->getASTContext().SourceMgr;
        if (SM.getLineNumber(callExpr->getFn()->getEndLoc()) !=
            SM.getLineNumber(PE->getStartLoc())) {
          diagnose(PE->getStartLoc(), diag::expected_do_in_statement)
            .fixItInsert(PE->getStartLoc(), "do ");
          return true;
        }
      }
    
    diagnose(callExpr->getArg()->getStartLoc(),
             diag::cannot_call_non_function_value, fnExpr->getType())
    .highlight(fnExpr->getSourceRange());
    return true;
  }
  
  // Collect a full candidate list of callees based on the partially type
  // checked function.
  CalleeCandidateInfo calleeInfo(fnExpr, CS);
  
  // Filter the candidate list based on the argument we may or may not have.
  calleeInfo.filterContextualMemberList(callExpr->getArg());
  
  
  Type argType;  // Type of the argument list, if knowable.
  if (auto FTy = fnType->getAs<AnyFunctionType>())
    argType = FTy->getInput();
  else if (auto MTT = fnType->getAs<AnyMetatypeType>()) {
    // If we are constructing a tuple with initializer syntax, the expected
    // argument list is the tuple type itself - and there is no initdecl.
    auto instanceTy = MTT->getInstanceType();
    if (instanceTy->is<TupleType>()) {
      argType = instanceTy;
    }
  }
  
  // Get the expression result of type checking the arguments to the call
  // independently, so we have some idea of what we're working with.
  //
  auto argExpr = typeCheckArgumentChildIndependently(callExpr->getArg(),
                                                     argType, calleeInfo);
  if (!argExpr)
    return true; // already diagnosed.

  calleeInfo.filterList(argExpr->getType());

  // If we found an exact match, this must be a problem with a conversion from
  // the result of the call to the expected type.  Diagnose this as a conversion
  // failure.
  if (calleeInfo.closeness == CC_ExactMatch)
    return false;

  
  bool isInitializer = isa<TypeExpr>(fnExpr);
  auto overloadName = calleeInfo.declName;

  std::string argString = getTypeListString(argExpr->getType());

  // If we couldn't get the name of the callee, then it must be something of a
  // more complex "value of function type".
  if (overloadName.empty()) {
    // If we couldn't infer the result type of the closure expr, then we have
    // some sort of ambiguity, let the ambiguity diagnostic stuff handle this.
    if (auto ffty = fnType->getAs<AnyFunctionType>())
      if (ffty->getResult()->hasTypeVariable()) {
        diagnoseAmbiguity(fnExpr);
        return true;
      }
    
    // The most common unnamed value of closure type is a ClosureExpr, so
    // special case it.
    if (isa<ClosureExpr>(fnExpr->getValueProvidingExpr())) {
      if (fnType->hasTypeVariable())
        diagnose(argExpr->getStartLoc(), diag::cannot_invoke_closure, argString)
          .highlight(fnExpr->getSourceRange());
      else
        diagnose(argExpr->getStartLoc(), diag::cannot_invoke_closure_type,
                 fnType, argString)
          .highlight(fnExpr->getSourceRange());
      
    } else if (fnType->hasTypeVariable()) {
      diagnose(argExpr->getStartLoc(), diag::cannot_call_function_value,
               argString)
        .highlight(fnExpr->getSourceRange());
    } else {
      diagnose(argExpr->getStartLoc(), diag::cannot_call_value_of_function_type,
                fnType, argString)
        .highlight(fnExpr->getSourceRange());
    }
    
    return true;
  }

  
  // If we have an argument list (i.e., a scalar, or a non-zero-element tuple)
  // then diagnose with some specificity about the arguments.
  if (isa<TupleExpr>(argExpr) &&
      cast<TupleExpr>(argExpr)->getNumElements() == 0) {
    // Emit diagnostics that say "no arguments".
    diagnose(fnExpr->getLoc(), diag::cannot_call_with_no_params,
             overloadName, isInitializer);
  } else {
    diagnose(fnExpr->getLoc(), diag::cannot_call_with_params,
             overloadName, argString, isInitializer);
  }
  
  // Did the user intend on invoking a different overload?
  calleeInfo.suggestPotentialOverloads(overloadName, fnExpr->getLoc(),
                                       /*isCallExpr*/true);
  return true;
}

bool FailureDiagnosis::visitAssignExpr(AssignExpr *assignExpr) {
  // Diagnose obvious assignments to literals.
  if (isa<LiteralExpr>(assignExpr->getDest()->getValueProvidingExpr())) {
    diagnose(assignExpr->getLoc(), diag::cannot_assign_to_literal);
    return true;
  }

  // Type check the destination first, so we can coerce the source to it.
  auto destExpr = typeCheckChildIndependently(assignExpr->getDest(),
                                              TCC_AllowLValue);
  if (!destExpr) return true;

  auto destType = destExpr->getType();
  if (destType->is<UnresolvedType>() || destType->hasTypeVariable()) {
    // If we have no useful type information from the destination, just type
    // check the source without contextual information.  If it succeeds, then we
    // win, but if it fails, we'll have to diagnose this another way.
    return !typeCheckChildIndependently(assignExpr->getSrc());
  }
  
  
  // If the result type is a non-lvalue, then we are failing because it is
  // immutable and that's not a great thing to assign to.
  if (!destType->isLValueType()) {
    CS->diagnoseAssignmentFailure(destExpr, destType, assignExpr->getLoc());
    return true;
  }

  // If the source type is already an error type, we've already posted an error.
  auto srcExpr = typeCheckChildIndependently(assignExpr->getSrc(),
                                     destType->getLValueOrInOutObjectType(),
                                             CTP_AssignSource);
  if (!srcExpr) return true;
  return false;
}

bool FailureDiagnosis::visitInOutExpr(InOutExpr *IOE) {
  // If we have a contextual type, it must be an inout type.
  auto contextualType = CS->getContextualType();
  
  if (contextualType) {
    if (!contextualType->is<InOutType>()) {
      // If the caller expected something inout, but we didn't have
      // something of inout type, diagnose it.
      diagnose(IOE->getLoc(), diag::extra_address_of, contextualType)
        .highlight(IOE->getSourceRange())
        .fixItRemove(IOE->getStartLoc());
      return true;
    }
    contextualType = contextualType->getInOutObjectType();
  }
  
  auto subExpr = typeCheckChildIndependently(IOE->getSubExpr(), contextualType,
                                             CS->getContextualTypePurpose(),
                                             TCC_AllowLValue);
  if (!subExpr) return true;

  auto subExprType = subExpr->getType();

  // The common cause is that the operand is not an lvalue.
  if (!subExprType->isLValueType()) {
    diagnoseSubElementFailure(subExpr, IOE->getLoc(), *CS,
                              diag::cannot_pass_rvalue_inout_subelement,
                              diag::cannot_pass_rvalue_inout);
    return true;
  }
  
  return false;
}

bool FailureDiagnosis::visitCoerceExpr(CoerceExpr *CE) {
  // Coerce the input to whatever type is specified by the CoerceExpr.
  if (!typeCheckChildIndependently(CE->getSubExpr(),
                                   CE->getCastTypeLoc().getType(),
                                   CTP_CoerceOperand))
    return true;

  return false;
}

bool FailureDiagnosis::visitForceValueExpr(ForceValueExpr *FVE) {
  auto argExpr = typeCheckChildIndependently(FVE->getSubExpr());
  if (!argExpr) return true;
  auto argType = argExpr->getType();

  // If the subexpression type checks as a non-optional type, then that is the
  // error.  Produce a specific diagnostic about this.
  if (!argType->is<TypeVariableType>() && !argType->is<UnresolvedType>() &&
      argType->getAnyOptionalObjectType().isNull()) {
    diagnose(FVE->getLoc(), diag::invalid_force_unwrap, argType)
      .fixItRemove(FVE->getExclaimLoc())
      .highlight(FVE->getSourceRange());
    return true;
  }
  
  return false;
}

bool FailureDiagnosis::visitBindOptionalExpr(BindOptionalExpr *BOE) {
  auto argExpr = typeCheckChildIndependently(BOE->getSubExpr());
  if (!argExpr) return true;
  auto argType = argExpr->getType();

  // If the subexpression type checks as a non-optional type, then that is the
  // error.  Produce a specific diagnostic about this.
  if (!argType->is<TypeVariableType>() && !argType->is<UnresolvedType>() &&
      argType->getAnyOptionalObjectType().isNull()) {
    diagnose(BOE->getQuestionLoc(), diag::invalid_optional_chain, argType)
      .highlight(BOE->getSourceRange())
      .fixItRemove(BOE->getQuestionLoc());
    return true;
  }

  return false;
}

bool FailureDiagnosis::visitIfExpr(IfExpr *IE) {
  // Check all of the subexpressions independently.
  auto condExpr = typeCheckChildIndependently(IE->getCondExpr());
  if (!condExpr) return true;
  auto trueExpr = typeCheckChildIndependently(IE->getThenExpr());
  if (!trueExpr) return true;

  auto falseExpr = typeCheckChildIndependently(IE->getElseExpr());
  if (!falseExpr) return true;

  // Check for "=" converting to BooleanType.  The user probably meant ==.
  if (auto *AE = dyn_cast<AssignExpr>(condExpr->getValueProvidingExpr())) {
    diagnose(AE->getEqualLoc(), diag::use_of_equal_instead_of_equality)
      .fixItReplace(AE->getEqualLoc(), "==")
      .highlight(AE->getDest()->getLoc())
      .highlight(AE->getSrc()->getLoc());
    return true;
  }

  // If the condition wasn't of boolean type, diagnose the problem.
  auto booleanType = CS->TC.getProtocol(IE->getQuestionLoc(),
                                        KnownProtocolKind::BooleanType);
  if (!booleanType) return true;

  if (!CS->TC.conformsToProtocol(condExpr->getType(), booleanType, CS->DC,
                                 ConformanceCheckFlags::InExpression,
                                 nullptr, condExpr->getLoc()))
    return true;
  
  // If the true/false values already match, it must be a contextual problem.
  if (trueExpr->getType()->isEqual(falseExpr->getType()))
    return false;
  
  // Otherwise, the true/false result types must not be matching.
  diagnose(IE->getColonLoc(), diag::if_expr_cases_mismatch,
           trueExpr->getType(), falseExpr->getType())
    .highlight(trueExpr->getSourceRange())
    .highlight(falseExpr->getSourceRange());
  return true;
}


bool FailureDiagnosis::
visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E) {
  // Don't walk the children for this node, it leads to multiple diagnostics
  // because of how sema injects this node into the type checker.
  return false;
}


bool FailureDiagnosis::visitClosureExpr(ClosureExpr *CE) {
  Type expectedResultType;
  
  // If we have a contextual type available for this closure, apply it to the
  // ParamDecls in our parameter list.  This ensures that any uses of them get
  // appropriate types.
  if (CS->getContextualType() &&
      CS->getContextualType()->is<AnyFunctionType>()) {

    auto fnType = CS->getContextualType()->castTo<AnyFunctionType>();
    Pattern *params = CE->getParams();
    TypeResolutionOptions TROptions;
    TROptions |= TR_OverrideType;
    TROptions |= TR_FromNonInferredPattern;
    TROptions |= TR_InExpression;
    TROptions |= TR_ImmediateFunctionInput;
    if (CS->TC.coercePatternToType(params, CE, fnType->getInput(), TROptions))
      return true;
    CE->setParams(params);

    expectedResultType = fnType->getResult();
  } else {
    
    // Defend against type variables from our constraint system leaking into
    // recursive constraints systems formed when checking the body of the
    // closure.  These typevars come into them when the body does name
    // lookups against the parameter decls.
    //
    // Handle this by rewriting the arguments to UnresolvedType().
    CE->getParams()->forEachVariable([&](VarDecl *VD) {
      if (VD->getType()->hasTypeVariable() || VD->getType()->is<ErrorType>())
        VD->overwriteType(CS->getASTContext().TheUnresolvedType);
    });
  }

  // If this is a complex leaf closure, there is nothing more we can do.
  if (!CE->hasSingleExpressionBody())
    return false;

  // If the closure had an expected result type, use it.
  if (CE->hasExplicitResultType())
    expectedResultType = CE->getExplicitResultTypeLoc().getType();

  // When we're type checking a single-expression closure, we need to reset the
  // DeclContext to this closure for the recursive type checking.  Otherwise,
  // if there is a closure in the subexpression, we can violate invariants.
  {
    llvm::SaveAndRestore<DeclContext*> SavedDC(CS->DC, CE);
    
    auto CTP = expectedResultType ? CTP_ClosureResult : CTP_Unused;
    
    if (!typeCheckChildIndependently(CE->getSingleExpressionBody(),
                                     expectedResultType, CTP))
      return true;
  }
  
  // If the body of the closure looked ok, then look for a contextual type
  // error.  This is necessary because FailureDiagnosis::diagnoseExprFailure
  // doesn't do this for closures.
  if (CS->getContextualType() &&
      !CS->getContextualType()->isEqual(CE->getType())) {

    auto fnType = CS->getContextualType()->getAs<AnyFunctionType>();

    // If the closure had an explicitly written return type incompatible with
    // the contextual type, diagnose that.
    if (CE->hasExplicitResultType() &&
        CE->getExplicitResultTypeLoc().getTypeRepr()) {
      auto explicitResultTy = CE->getExplicitResultTypeLoc().getType();
      if (fnType && !explicitResultTy->isEqual(fnType->getResult())) {
        auto repr = CE->getExplicitResultTypeLoc().getTypeRepr();
        diagnose(repr->getStartLoc(), diag::incorrect_explicit_closure_result,
                 explicitResultTy, fnType->getResult())
          .fixItReplace(repr->getSourceRange(),fnType->getResult().getString());
        return true;
      }
    }
  }

  // Otherwise, we can't produce a specific diagnostic.
  return false;
}

bool FailureDiagnosis::visitArrayExpr(ArrayExpr *E) {
  Type contextualElementType;
  auto elementTypePurpose = CTP_Unused;

  // FIXME: This isn't handling Array -> pointer impl conversions yet.

  // If we had a contextual type, then it either conforms to
  // ArrayLiteralConvertible or it is an invalid contextual type.
  if (auto contextualType = CS->getContextualType()) {
    auto ALC = CS->TC.getProtocol(E->getLoc(),
                                  KnownProtocolKind::ArrayLiteralConvertible);
    
    // Validate that the contextual type conforms to ArrayLiteralConvertible and
    // figure out what the contextual Key/Value types are in place.  If this
    // doesn't conform, then it is a contextual error that we'll emit later with
    // a good message.
    ProtocolConformance *Conformance = nullptr;
    if (!ALC || !CS->TC.conformsToProtocol(contextualType, ALC, CS->DC,
                                           ConformanceCheckFlags::InExpression,
                                           &Conformance))
      return visitExpr(E);
    
    Conformance->forEachTypeWitness(&CS->TC,
                                    [&](AssociatedTypeDecl *ATD,
                          const Substitution &subst, TypeDecl *d)->bool
    {
      if (ATD->getName().str() == "Element")
        contextualElementType = subst.getReplacement()->getDesugaredType();
      return false;
    });
    assert(contextualElementType &&
           "Could not find 'Element' ArrayLiteral associated types from"
           " contextual type conformance");

    elementTypePurpose = CTP_ArrayElement;
  }

  // Type check each of the subexpressions in place, passing down the contextual
  // type information if we have it.
  for (auto elt : E->getElements()) {
    if (typeCheckChildIndependently(elt, contextualElementType,
                                    elementTypePurpose) == nullptr)
      return true;
  }

  // If that didn't turn up an issue, then we don't know what to do.
  // TODO: When a contextual type is missing, we could try to diagnose cases
  // where the element types mismatch... but theoretically they should type
  // unify to Any, so that could never happen?
  return false;
}

bool FailureDiagnosis::visitDictionaryExpr(DictionaryExpr *E) {
  Type contextualKeyType, contextualValueType;
  auto keyTypePurpose = CTP_Unused, valueTypePurpose = CTP_Unused;

  // If we had a contextual type, then it either conforms to
  // DictionaryLiteralConvertible or it is an invalid contextual type.
  if (auto contextualType = CS->getContextualType()) {
    auto DLC = CS->TC.getProtocol(E->getLoc(),
                            KnownProtocolKind::DictionaryLiteralConvertible);
    
    // Validate the contextual type conforms to DictionaryLiteralConvertible
    // and figure out what the contextual Key/Value types are in place.  If this
    // doesn't conform, then it is a contextual error that we'll emit later with
    // a good message.
    ProtocolConformance *Conformance = nullptr;
    if (!DLC || !CS->TC.conformsToProtocol(contextualType, DLC, CS->DC,
                                           ConformanceCheckFlags::InExpression,
                                           &Conformance, E->getLoc()))
      return visitExpr(E);

    Conformance->forEachTypeWitness(&CS->TC,
                                    [&](AssociatedTypeDecl *ATD,
                          const Substitution &subst, TypeDecl *d)->bool
    {
      if (ATD->getName().str() == "Key")
        contextualKeyType = subst.getReplacement()->getDesugaredType();
      else if (ATD->getName().str() == "Value")
        contextualValueType = subst.getReplacement()->getDesugaredType();
      return false;
    });
    assert(contextualKeyType && contextualValueType &&
           "Could not find Key/Value DictionaryLiteral associated types from"
           " contextual type conformance");
    
    keyTypePurpose = CTP_DictionaryKey;
    valueTypePurpose = CTP_DictionaryValue;
  }
  
  // Type check each of the subexpressions in place, passing down the contextual
  // type information if we have it.
  for (auto elt : E->getElements()) {
    auto TE = dyn_cast<TupleExpr>(elt);
    if (!TE || TE->getNumElements() != 2) continue;

    if (!typeCheckChildIndependently(TE->getElement(0),
                                     contextualKeyType, keyTypePurpose))
      return true;
    if (!typeCheckChildIndependently(TE->getElement(1),
                                     contextualValueType, valueTypePurpose))
      return true;
  }

  // If that didn't turn up an issue, then we don't know what to do.
  // TODO: When a contextual type is missing, we could try to diagnose cases
  // where the element types mismatch.  There is no Any equivalent since they
  // keys need to be hashable.
  return false;
}

bool FailureDiagnosis::visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
  // If we have no contextual type, there is no way to resolve this.  Just
  // diagnose this as an ambiguity.
  if (!CS->getContextualType())
    return false;

  // OTOH, if we do have a contextual type, we can provide a more specific
  // error.  Dig out the UnresolvedValueMember constraint for this expr node.
  Constraint *memberConstraint = nullptr;
  auto checkConstraint = [&](Constraint *C) {
    if (C->getKind() == ConstraintKind::UnresolvedValueMember &&
        simplifyLocatorToAnchor(*CS, C->getLocator()) == E)
      memberConstraint = C;
  };
  
  if (CS->failedConstraint)
    checkConstraint(CS->failedConstraint);
  for (auto &C : CS->getConstraints()) {
    if (memberConstraint) break;
    checkConstraint(&C);
  }
  
  // If we can't find the member constraint in question, then we failed.
  if (!memberConstraint)
    return false;
  
  // If we succeeded, get ready to do the member lookup.
  auto baseObjTy = CS->getContextualType()->getRValueType();

  // If the base object is already a metatype type, then something weird is
  // going on.  For now, just generate a generic error.
  if (baseObjTy->is<MetatypeType>())
    return false;

  // Otherwise, we'll perform a lookup against the metatype of our contextual
  // type.
  baseObjTy = MetatypeType::get(baseObjTy);
  auto result = CS->performMemberLookup(baseObjTy, *memberConstraint);

  switch (result.OverallResult) {
  case MemberLookupResult::Unsolved:
    llvm_unreachable("base expr type should be resolved at this point");
  case MemberLookupResult::ErrorAlreadyDiagnosed:
    // If an error was already emitted, then we're done, don't emit anything
    // redundant.
    return true;
  case MemberLookupResult::HasResults:
    break;    // Interesting case. :-)
  }

  // If we have unviable candidates (e.g. because of access control or some
  // other problem) we should diagnose the problem.
  if (result.ViableCandidates.empty()) {
    diagnoseUnviableLookupResults(result, baseObjTy, /*no base expr*/nullptr,
                                  E->getName(), E->getNameLoc(),
                                  E->getLoc());
    return true;
  }

  // Dump all of our viable candidates into a CalleeCandidateInfo (with an
  // uncurry level of 1 to represent the contextual type) and sort it out.
  CalleeCandidateInfo candidateInfo(result.ViableCandidates, 1, CS);

  // Filter the candidate list based on the argument we may or may not have.
  candidateInfo.filterContextualMemberList(E->getArgument());

  // If we have multiple candidates, then we have an ambiguity.
  if (candidateInfo.size() != 1) {
    SourceRange argRange;
    if (auto arg = E->getArgument()) argRange = arg->getSourceRange();
    diagnose(E->getNameLoc(), diag::ambiguous_member_overload_set,
             E->getName().str())
      .highlight(argRange);
    candidateInfo.suggestPotentialOverloads(E->getName().str(),E->getNameLoc());
    return true;
  }
  
  auto argumentTy = candidateInfo[0].getArgumentType();

  // Depending on how we matched, produce tailored diagnostics.
  switch (candidateInfo.closeness) {
  case CC_NonLValueInOut:      // First argument is inout but no lvalue present.
  case CC_OneArgumentMismatch: // All arguments except one match.
  case CC_SelfMismatch:        // Self argument mismatches.
  case CC_ArgumentMismatch:    // Argument list mismatch.
    assert(0 && "These aren't produced by filterContextualMemberList");
    return false;

  case CC_ExactMatch: {        // This is a perfect match for the arguments.
    // If we have an exact match, then we must have an argument list.
    // If we didn't have an argument or an arg type, the expr would be valid.
    if (!argumentTy) {
      assert(!E->getArgument() && "Not an exact match");
      // If this is an exact match, return false to diagnose this as an
      // ambiguity.  It must be some other problem, such as failing to infer a
      // generic argument on the enum type.
      return false;
    }
    
    assert(E->getArgument() && argumentTy && "Exact match without argument?");
    return !typeCheckArgumentChildIndependently(E->getArgument(), argumentTy,
                                                candidateInfo);
  }

  case CC_ArgumentLabelMismatch: { // Argument labels are not correct.
    auto argExpr = typeCheckArgumentChildIndependently(E->getArgument(),
                                                       argumentTy,
                                                       candidateInfo);
    if (!argExpr) return true;

    // Construct the actual expected argument labels that our candidate
    // expected.
    assert(argumentTy &&
           "Candidate must expect an argument to have a label mismatch");
    auto arguments = decomposeArgumentType(argumentTy);
    
    // TODO: This is probably wrong for varargs, e.g. calling "print" with the
    // wrong label.
    SmallVector<Identifier, 4> expectedNames;
    for (auto &arg : arguments)
      expectedNames.push_back(arg.getName());

    return CS->diagnoseArgumentLabelError(argExpr, expectedNames,
                                          /*isSubscript*/false);
  }
    
  case CC_GeneralMismatch:        // Something else is wrong.
  case CC_ArgumentCountMismatch:  // This candidate has wrong # arguments.
    // If we have no argument, the candidates must have expected one.
    if (!E->getArgument()) {
      if (!argumentTy)
        return false; // Candidate must be incorrect for some other reason.
      
      // Pick one of the arguments that are expected as an exemplar.
      diagnose(E->getNameLoc(), diag::expected_argument_in_contextual_member,
               E->getName(), argumentTy);
      return true;
    }
     
    // If an argument value was specified, but this is a simple enumerator, then
    // we fail with a nice error message.
    auto argTy = candidateInfo[0].getArgumentType();
    if (!argTy) {
      diagnose(E->getNameLoc(), diag::unexpected_argument_in_contextual_member,
               E->getName());
      return true;
    }

    assert(E->getArgument() && argTy && "Exact match without an argument?");
    return !typeCheckArgumentChildIndependently(E->getArgument(), argTy,
                                                candidateInfo);
  }

  llvm_unreachable("all cases should be handled");
}

/// A TupleExpr propagate contextual type information down to its children and
/// can be erroneous when there is a label mismatch etc.
bool FailureDiagnosis::visitTupleExpr(TupleExpr *TE) {
  // If we know the requested argType to use, use computeTupleShuffle to produce
  // the shuffle of input arguments to destination values.  It requires a
  // TupleType to compute the mapping from argExpr.  Conveniently, it doesn't
  // care about the actual types though, so we can just use 'void' for them.
  if (!CS->getContextualType() || !CS->getContextualType()->is<TupleType>())
    return visitExpr(TE);
  
  auto contextualTT = CS->getContextualType()->castTo<TupleType>();
  
  SmallVector<TupleTypeElt, 4> ArgElts;
  auto voidTy = CS->getASTContext().TheEmptyTupleType;

  for (unsigned i = 0, e = TE->getNumElements(); i != e; ++i)
    ArgElts.push_back({ voidTy, TE->getElementName(i) });
  auto TEType = TupleType::get(ArgElts, CS->getASTContext());
  
  if (!TEType->is<TupleType>())
    return visitExpr(TE);

  SmallVector<int, 4> sources;
  SmallVector<unsigned, 4> variadicArgs;
  
  // If the shuffle is invalid, then there is a type error.  We could diagnose
  // it specifically here, but the general logic does a fine job so we let it
  // do it.
  if (computeTupleShuffle(TEType->castTo<TupleType>(), contextualTT,
                          sources, variadicArgs))
    return visitExpr(TE);

  // If we got a correct shuffle, we can perform the analysis of all of
  // the input elements, with their expected types.
  for (unsigned i = 0, e = sources.size(); i != e; ++i) {
    // If the value is taken from a default argument, ignore it.
    if (sources[i] == TupleShuffleExpr::DefaultInitialize ||
        sources[i] == TupleShuffleExpr::Variadic ||
        sources[i] == TupleShuffleExpr::CallerDefaultInitialize)
      continue;
    
    assert(sources[i] >= 0 && "Unknown sources index");
    
    // Otherwise, it must match the corresponding expected argument type.
    unsigned inArgNo = sources[i];
    auto actualType = contextualTT->getElementType(i);
    
    TCCOptions options;
    if (actualType->is<InOutType>())
      options |= TCC_AllowLValue;
    
    auto exprResult =
      typeCheckChildIndependently(TE->getElement(inArgNo), actualType,
                                  CS->getContextualTypePurpose(), options);
    // If there was an error type checking this argument, then we're done.
    if (!exprResult) return true;
    
    // If the caller expected something inout, but we didn't have
    // something of inout type, diagnose it.
    if (auto IOE =
          dyn_cast<InOutExpr>(exprResult->getSemanticsProvidingExpr())) {
      if (!actualType->is<InOutType>()) {
        diagnose(exprResult->getLoc(), diag::extra_address_of,
                 exprResult->getType()->getInOutObjectType())
        .highlight(exprResult->getSourceRange())
        .fixItRemove(IOE->getStartLoc());
        return true;
      }
    }
  }
  
  if (!variadicArgs.empty()) {
    auto varargsTy = contextualTT->getVarArgsBaseType();
    for (unsigned i = 0, e = variadicArgs.size(); i != e; ++i) {
      unsigned inArgNo = variadicArgs[i];
      
      auto expr =
        typeCheckChildIndependently(TE->getElement(inArgNo), varargsTy,
                                    CS->getContextualTypePurpose());
      // If there was an error type checking this argument, then we're done.
      if (!expr) return true;
    }
  }
  
  return false;
}

/// An IdentityExpr doesn't change its argument, but it *can* propagate its
/// contextual type information down.
bool FailureDiagnosis::visitIdentityExpr(IdentityExpr *E) {
  auto contextualType = CS->getContextualType();
  
  // If we have a paren expr and our contextual type is a ParenType, remove the
  // paren expr sugar.
  if (isa<ParenExpr>(E) && contextualType)
    if (auto *PT = dyn_cast<ParenType>(contextualType.getPointer()))
      contextualType = PT->getUnderlyingType();
  
  if (!typeCheckChildIndependently(E->getSubExpr(), contextualType,
                                   CS->getContextualTypePurpose()))
    return true;
  return false;
}

bool FailureDiagnosis::visitExpr(Expr *E) {
  // Check each of our immediate children to see if any of them are
  // independently invalid.
  bool errorInSubExpr = false;
  
  E->forEachImmediateChildExpr([&](Expr *Child) -> Expr* {
    // If we already found an error, stop checking.
    if (errorInSubExpr) return Child;

    // Otherwise just type check the subexpression independently.  If that
    // succeeds, then we stitch the result back into our expression.
    if (typeCheckChildIndependently(Child))
      return Child;

    // Otherwise, it failed, which emitted a diagnostic.  Keep track of this
    // so that we don't emit multiple diagnostics.
    errorInSubExpr = true;
    return Child;
  });
  
  // If any of the children were errors, we're done.
  if (errorInSubExpr)
    return true;
  
  // Otherwise, produce a more generic error.
  return false;
}


bool FailureDiagnosis::diagnoseExprFailure() {
  assert(CS && expr);

  // Our general approach is to do a depth first traversal of the broken
  // expression tree, type checking as we go.  If we find a subtree that cannot
  // be type checked on its own (even to an incomplete type) then that is where
  // we focus our attention.  If we do find a type, we use it to check for
  // contextual type mismatches.
  return visit(expr);
}


/// Given a specific expression and the remnants of the failed constraint
/// system, produce a specific diagnostic.
///
/// This is guaranteed to always emit an error message.
///
void ConstraintSystem::diagnoseFailureForExpr(Expr *expr) {
  // Continue simplifying any active constraints left in the system.  We can end
  // up with them because the solver bails out as soon as it sees a Failure.  We
  // don't want to leave them around in the system because later diagnostics
  // will assume they are unsolvable and may otherwise leave the system in an
  // inconsistent state.
  simplify(/*ContinueAfterFailures*/true);

  // Look through RebindSelfInConstructorExpr to avoid weird sema issues.
  if (auto *RB = dyn_cast<RebindSelfInConstructorExpr>(expr))
    expr = RB->getSubExpr();
  
  FailureDiagnosis diagnosis(expr, this);
  
  // Now, attempt to diagnose the failure from the info we've collected.
  if (diagnosis.diagnoseExprFailure())
    return;

  // If this is a contextual conversion problem, dig out some information.
  if (diagnosis.diagnoseContextualConversionError())
    return;

  // If we can diagnose a problem based on the constraints left laying around in
  // the system, do so now.
  if (diagnosis.diagnoseConstraintFailure())
    return;

  // If the expression-order diagnostics didn't find any diagnosable problems,
  // try the unavoidable failures list again, with locator substitutions in
  // place.  To make sure we emit the error if we have a failure recorded.
  for (auto failure : unavoidableFailures) {
    if (diagnoseFailure(*this, *failure, expr, true))
      return;
  }

  // If no one could find a problem with this expression or constraint system,
  // then it must be well-formed... but is ambiguous.  Handle this by diagnosic
  // various cases that come up.
  diagnosis.diagnoseAmbiguity(expr);
}

/// Emit an ambiguity diagnostic about the specified expression.
void FailureDiagnosis::diagnoseAmbiguity(Expr *E) {

  // Check out all of the type variables lurking in the system.  If any are
  // unbound archetypes, then the problem is that it couldn't be resolved.
  for (auto tv : CS->getTypeVariables()) {
    if (tv->getImpl().hasRepresentativeOrFixed())
      continue;
    
    
    // If this is a conversion to a type variable used to form an archetype,
    // Then diagnose this as a generic parameter that could not be resolved.
    auto archetype = tv->getImpl().getArchetype();
    
    // Only diagnose archetypes that don't have a parent, i.e., ones
    // that correspond to generic parameters.
    if (archetype && !archetype->getParent()) {
      diagnose(expr->getLoc(), diag::unbound_generic_parameter, archetype);
      
      // Emit a "note, archetype declared here" sort of thing.
      noteTargetOfDiagnostic(*CS, nullptr, tv->getImpl().getLocator());
      return;
    }
    continue;
  }

  // Unresolved/Anonymous ClosureExprs are common enough that we should give
  // them tailored diagnostics.
  if (auto CE = dyn_cast<ClosureExpr>(E->getValueProvidingExpr())) {
    auto CFTy = CE->getType()->getAs<AnyFunctionType>();
    
    // If this is a multi-statement closure with no explicit result type, emit
    // a note to clue the developer in.
    if (!CE->hasExplicitResultType() && CFTy &&
        (CFTy->getResult()->hasTypeVariable() ||
         CFTy->getResult()->is<UnresolvedType>())) {
      diagnose(CE->getLoc(), diag::cannot_infer_closure_result_type);
      return;
    }
    
    diagnose(E->getLoc(), diag::cannot_infer_closure_type)
      .highlight(E->getSourceRange());
    return;
  }

  // A DiscardAssignmentExpr (spelled "_") needs contextual type information to
  // infer its type. If we see one at top level, diagnose that it must be part
  // of an assignment so we don't get a generic "expression is ambiguous" error.
  if (isa<DiscardAssignmentExpr>(E)) {
    diagnose(E->getLoc(), diag::discard_expr_outside_of_assignment)
      .highlight(E->getSourceRange());
    return;
  }
  

  // Attempt to re-type-check the entire expression, while allowing ambiguity.
  auto exprType = getTypeOfTypeCheckedChildIndependently(expr);
  // If it failed an diagnosed something, then we're done.
  if (!exprType) return;

  // If we were able to find something more specific than "unknown" (perhaps
  // something like "[_:_]" for a dictionary literal), include it in the
  // diagnostic.
  if (!exprType->is<UnresolvedType>() && !exprType->is<TypeVariableType>()) {
    diagnose(E->getLoc(), diag::specific_type_of_expression_is_ambiguous,
             exprType)
      .highlight(E->getSourceRange());
    return;
  }
  
  // If there are no posted constraints or failures, then there was
  // not enough contextual information available to infer a type for the
  // expression.
  diagnose(E->getLoc(), diag::type_of_expression_is_ambiguous)
    .highlight(E->getSourceRange());
}

bool ConstraintSystem::salvage(SmallVectorImpl<Solution> &viable, Expr *expr) {
  // If there were any unavoidable failures, emit the first one we can.
  if (!unavoidableFailures.empty()) {
    for (auto failure : unavoidableFailures) {
      if (diagnoseFailure(*this, *failure, expr, false))
        return true;
    }
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
    solveRec(viable, FreeTypeVariableBinding::Disallow);

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

    // Remove solutions that require fixes; the fixes in those systems should
    // be diagnosed rather than any ambiguity.
    auto hasFixes = [](const Solution &sol) { return !sol.Fixes.empty(); };
    auto newEnd = std::remove_if(viable.begin(), viable.end(), hasFixes);
    viable.erase(newEnd, viable.end());

    // If there are multiple solutions, try to diagnose an ambiguity.
    if (viable.size() > 1) {
      if (getASTContext().LangOpts.DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log << "---Ambiguity error: "
            << viable.size() << " solutions found---\n";
        int i = 0;
        for (auto &solution : viable) {
          log << "---Ambiguous solution #" << i++ << "---\n";
          solution.dump(log);
          log << "\n";
        }
      }        

      if (diagnoseAmbiguity(*this, viable, expr)) {
        return true;
      }
    }

    // Remove the solver state.
    this->solverState = nullptr;

    // Fall through to produce diagnostics.
  }

  if (getExpressionTooComplex()) {
    TC.diagnose(expr->getLoc(), diag::expression_too_complex).
    highlight(expr->getSourceRange());
    return true;
  }

  // If all else fails, diagnose the failure by looking through the system's
  // constraints.
  diagnoseFailureForExpr(expr);
  return true;
}
