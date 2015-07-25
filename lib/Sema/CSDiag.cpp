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

  case DoesNotHaveInitOnInstance:
    out << getFirstType().getString() << " instance does not have initializers";
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

  case UnboundGenericParameter:
    out << getFirstType().getString()
        << " is an unbound generic parameter";
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

      if (auto objectLiteralExpr = dyn_cast<ObjectLiteralExpr>(anchor)) {
        targetAnchor = nullptr;
        targetPath.clear();

        anchor = objectLiteralExpr->getArg();
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

    if (auto identity = dyn_cast<IdentityExpr>(anchor)) {
      anchor = identity->getSubExpr();
      continue;
    }

    if (auto tryExpr = dyn_cast<AnyTryExpr>(anchor)) {
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
                tuple1->getNumElements(), tuple2->getNumElements())
      .highlight(range1).highlight(range2);
    return true;
  }

  case Failure::TupleUnused:
    tc.diagnose(loc, diag::invalid_tuple_element_unused,
                failure.getFirstType(),
                failure.getSecondType())
      .highlight(range1).highlight(range2);
    return true;

  case Failure::TypesNotConvertible:
  case Failure::TypesNotEqual:
  case Failure::TypesNotSubtypes:
  case Failure::TypesNotConstructible:
  case Failure::FunctionTypesMismatch: {
    // If this is conversion failure due to a return statement with an argument
    // that cannot be coerced to the result type of the function, emit a
    // specific error.
    if (expr->isReturnExpr() &&
        expr->getValueProvidingExpr() == anchor) {
      auto actualType = cs.simplifyType(expr->getType())->getRValueType();
      auto expectedType =
        AnyFunctionRef::fromFunctionDeclContext(cs.DC).getBodyResultType();

      if (expectedType->isVoid()) {
        tc.diagnose(loc, diag::cannot_return_value_from_void_func)
          .highlight(range1).highlight(range2);
      } else {
        tc.diagnose(loc, diag::cannot_convert_to_return_type,
                    actualType, expectedType)
          .highlight(range1).highlight(range2);
      }
      
      if (targetLocator && !useExprLoc)
        noteTargetOfDiagnostic(cs, failure, targetLocator);
      return true;
    }
    
    // We can do a better job of diagnosing application argument conversion
    // failures elsewhere.
    if (isa<ApplyExpr>(expr) ||
        isa<InOutExpr>(expr) ||
        isa<AssignExpr>(expr))
      return false;

    tc.diagnose(loc, diag::invalid_relation,
                failure.getKind() - Failure::TypesNotEqual,
                failure.getFirstType(), failure.getSecondType())
      .highlight(range1).highlight(range2);
    if (targetLocator && !useExprLoc)
      noteTargetOfDiagnostic(cs, failure, targetLocator);
    return true;
  }

  case Failure::DoesNotHaveMember:
  case Failure::DoesNotHaveNonMutatingMember: {
    if (auto moduleTy = failure.getFirstType()->getAs<ModuleType>()) {
      tc.diagnose(loc, diag::no_member_of_module,
                  moduleTy->getModule()->getName(),
                  failure.getName())
        .highlight(range1).highlight(range2);
      break;
    }

    // If the base of this property access is a function that takes an empty
    // argument list, then the most likely problem is that the user wanted to
    // call the function, e.g. in "a.b.c" where they had to write "a.b().c".
    // Produce a specific diagnostic + fixit for this situation.
    auto baseFTy = failure.getFirstType()->getAs<AnyFunctionType>();
    if (baseFTy &&baseFTy->getInput()->isEqual(tc.Context.TheEmptyTupleType)){
      SourceLoc insertLoc = locator->getAnchor()->getEndLoc();
      
      if (auto *UDE = dyn_cast<UnresolvedDotExpr>(locator->getAnchor())) {
        tc.diagnose(loc, diag::did_not_call_method, UDE->getName())
          .fixItInsertAfter(insertLoc, "()");
        break;
      }
      
      tc.diagnose(loc, diag::did_not_call_function)
        .fixItInsertAfter(insertLoc, "()");
      break;
    }
    
    bool IsNoMember = failure.getKind() == Failure::DoesNotHaveMember;
    tc.diagnose(loc, IsNoMember ? diag::does_not_have_member :
                                  diag::does_not_have_non_mutating_member,
                failure.getFirstType(),
                failure.getName())
      .highlight(range1).highlight(range2);
    break;
  }
  case Failure::DoesNotHaveInitOnInstance: {
    // Diagnose 'super.init', which can only appear inside another initializer,
    // specially.
    auto ctorRef = dyn_cast<UnresolvedConstructorExpr>(locator->getAnchor());
    if (isa<SuperRefExpr>(ctorRef->getSubExpr())) {
      tc.diagnose(loc, diag::super_initializer_not_in_initializer);
      break;
    }
  
    // Suggest inserting '.dynamicType' to construct another object of the same
    // dynamic type.
    SourceLoc fixItLoc;
    if (ctorRef) {
      // Place the '.dynamicType' right before the init.
      fixItLoc = ctorRef->getConstructorLoc().getAdvancedLoc(-1);
    }
    
    auto diag = tc.diagnose(loc, diag::init_not_instance_member);
    if (fixItLoc.isValid())
      diag.fixItInsert(fixItLoc, ".dynamicType");
    diag.flush();
    
    break;
  }

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
                          ConformanceCheckFlags::InExpression,
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

  case Failure::IsNotMaterializable: {
    tc.diagnose(loc, diag::cannot_bind_generic_parameter_to_type,
                failure.getFirstType())
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

  case Failure::FunctionThrowsMismatch: {
    tc.diagnose(loc, diag::throws_functiontype_mismatch,
                failure.getFirstType()->getAs<AnyFunctionType>()->throws(),
                failure.getFirstType(),
                failure.getSecondType()->getAs<AnyFunctionType>()->throws(),
                failure.getSecondType())
      .highlight(range2);
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

static bool diagnoseAmbiguity(ConstraintSystem &cs,
                              ArrayRef<Solution> solutions) {
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

static Constraint *getConstraintChoice(Constraint *constraint,
                                       ConstraintKind kind,
                                       bool takeAny = false) {
  if (constraint->getKind() != ConstraintKind::Disjunction &&
      constraint->getKind() != ConstraintKind::Conjunction)
    return nullptr;
  
  auto nestedConstraints = constraint->getNestedConstraints();
  
  for (auto nestedConstraint : nestedConstraints) {
    if (!takeAny && nestedConstraint->getKind() != kind)
      continue;
      
    // If this is a last-chance search, and we have a conjunction or
    // disjunction, look within.
    if (takeAny &&
        ((nestedConstraint->getKind() == ConstraintKind::Disjunction) ||
         (nestedConstraint->getKind() == ConstraintKind::Conjunction))) {
          return getConstraintChoice(nestedConstraint, kind, takeAny);
        }
    
    return nestedConstraint;
  }

  return nullptr;
}

/// For a given expression type, extract the appropriate type for a constraint-
/// based diagnostic.
static Type getDiagnosticTypeFromExpr(Expr *expr) {
  
  // For a forced checked cast expression or coerce expression, use the type of
  // the sub-expression.
  if (auto fcc = dyn_cast<ForcedCheckedCastExpr>(expr))
    return fcc->getSubExpr()->getType();

  if (auto coerceExpr = dyn_cast<CoerceExpr>(expr))
    return coerceExpr->getSubExpr()->getType();
  
  // For an application expression, use the argument type.
  if (auto applyExpr = dyn_cast<ApplyExpr>(expr))
    return applyExpr->getArg()->getType();
  
  // For a subscript expression, use the index type.
  if (auto subscriptExpr = dyn_cast<SubscriptExpr>(expr))
    return subscriptExpr->getIndex()->getType();
  
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

static std::pair<Type, Type>
getBoundTypesFromConstraint(ConstraintSystem *CS, Expr *expr,
                            Constraint *constraint) {
  auto type1 = getDiagnosticTypeFromExpr(expr);
  auto type2 = constraint->getSecondType();
  
  if (type1->isEqual(type2))
    if (auto firstType = constraint->getFirstType())
      type1 = firstType;
  
  if (auto typeVariableType =
      dyn_cast<TypeVariableType>(type2.getPointer())) {
    
    if (typeVariableType->getImpl().
          getRepresentative(nullptr) == typeVariableType) {
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
        } else if (impl.getLocator()) {
          auto implAnchor = impl.getLocator()->getAnchor();
          auto anchorType = implAnchor->getType();
          
          // Don't re-substitute an opened type variable for itself.
          if (anchorType.getPointer() != type1.getPointer())
            type2 = anchorType;
        }
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
  if (auto tv1 = type1->getAs<TypeVariableType>())
    type1 = substituteLiteralForTypeVariable(CS, tv1);
  
  if (auto tv2 = type2->getAs<TypeVariableType>())
    type2 = substituteLiteralForTypeVariable(CS, tv2);
  
  return std::pair<Type, Type>(type1->getLValueOrInOutObjectType(),
                               type2->getLValueOrInOutObjectType());
}

/// Determine if a type resulting from a failed typecheck operation is fully-
/// specialized, or if it still has type variable type arguments.
/// (This diverges slightly from hasTypeVariable, in that certain tyvars,
/// such as for nil literals, will be treated as specialized.)
static bool typeIsNotSpecialized(Type type) {
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

/// Conveniently unwrap a paren expression, if necessary.
static Expr *unwrapParenExpr(Expr *e) {
  if (auto parenExpr = dyn_cast<ParenExpr>(e))
    return unwrapParenExpr(parenExpr->getSubExpr());
  
  return e;
}

static SmallVector<Type, 4> decomposeArgumentType(Type ty) {
  SmallVector<Type, 4> result;

  // Assemble the parameter type list.
  if (auto parenType = dyn_cast<ParenType>(ty.getPointer())) {
    result.push_back(parenType->getUnderlyingType());
  } else if (auto tupleType = ty->getAs<TupleType>()) {
    for (auto field : tupleType->getElements())
      result.push_back(field.getType());
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


/// If an UnresolvedDotExpr has been resolved by the constraint system, return
/// the decl that it references.
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
  expr = expr->getSemanticsProvidingExpr();

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
          dyn_cast<DeclRefExpr>(AE->getFn()->getSemanticsProvidingExpr()))
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
    CC_ExactMatch,             // This is a perfect match for the arguments.
    CC_NonLValueInOut,         // First argument is inout but no lvalue present.
    CC_OneArgumentMismatch,    // All arguments except one match.
    CC_SelfMismatch,           // Self argument mismatches.
    CC_ArgumentMismatch,       // Argument list mismatch.
    CC_ArgumentCountMismatch,  // This candidate has wrong # arguments.
    CC_GeneralMismatch         // Something else is wrong.
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

    typedef const std::function<CandidateCloseness(UncurriedCandidate)>
    &ClosenessPredicate;

    /// After the candidate list is formed, it can be filtered down to discard
    /// obviously mismatching candidates and compute a "closeness" for the
    /// resultant set.
    void filterList(Type actualArgsType);
    void filterList(ClosenessPredicate predicate);

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
evaluateCloseness(Type candArgListType, ArrayRef<Type> actualArgs) {
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
    // FIXME: Right now, a "matching" overload is one with a parameter whose
    // type is identical to one of the argument types. We can obviously do
    // something more sophisticated with this.
    if (!actualArgs[i]->getRValueType()->isEqual(candArgs[i]))
      ++mismatchingArgs;
  }
  
  // If the arguments match up exactly, then we have an exact match.  This
  // handles the no-argument cases as well.
  if (mismatchingArgs == 0)
    return CC_ExactMatch;
  
  // Check to see if the first argument expects an inout argument, but is not
  // an lvalue.
  if (candArgs[0]->is<InOutType>() && !actualArgs[0]->isLValueType())
    return CC_NonLValueInOut;
  
  if (mismatchingArgs == 1)
    return actualArgs.size() != 1 ? CC_OneArgumentMismatch :CC_ArgumentMismatch;

  // TODO: Keyword argument mismatches.
  
  return CC_GeneralMismatch;
}


void CalleeCandidateInfo::collectCalleeCandidates(Expr *fn) {
  fn = fn->getSemanticsProvidingExpr();

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

  // Otherwise, we couldn't tell structurally what is going on here, so try to
  // dig something out of the constraint system.
  unsigned uncurryLevel = 0;

  // The candidate list of an unresolved_dot_expr is the candidate list of the
  // base uncurried by one level, and we refer to the name of the member, not to
  // the name of any base.
  if (auto UDE = dyn_cast<UnresolvedDotExpr>(fn)) {
    declName = UDE->getName().str().str();
    uncurryLevel = 1;
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
void CalleeCandidateInfo::filterList(Type actualArgsType) {
  // Now that we have the candidate list, figure out what the best matches from
  // the candidate list are, and remove all the ones that aren't at that level.
  auto actualArgs = decomposeArgumentType(actualArgsType);
  filterList([&](UncurriedCandidate candidate) -> CandidateCloseness {
    auto inputType = candidate.getArgumentType();
    // If this isn't a function or isn't valid at this uncurry level, treat it
    // as a general mismatch.
    if (!inputType) return CC_GeneralMismatch;
    return evaluateCloseness(inputType, actualArgs);
  });
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
  /// Don't force the subexpression to resolve to a specific type.  If the
  /// subexpr is ambiguous, don't diagnose an error.
  /// FIXME: this should always be on.
  TCC_AllowUnresolved = 0x01,

  /// Allow the result of the subexpression to be an lvalue.  If this is not
  /// specified, any lvalue will be forced to be loaded into an rvalue.
  TCC_AllowLValue = 0x02
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
  
  // Specific constraint kinds used, in conjunction with the expression node,
  // to determine the appropriate diagnostic.
  Constraint *conversionConstraint = nullptr;
  Constraint *overloadConstraint = nullptr;
  Constraint *valueMemberConstraint = nullptr;
  Constraint *argumentConstraint = nullptr;
  Constraint *conformanceConstraint = nullptr;
  
public:
  
  FailureDiagnosis(Expr *expr, ConstraintSystem *CS);

  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(ArgTypes &&...Args) {
    return CS->TC.diagnose(std::forward<ArgTypes>(Args)...);
  }

  /// Attempt to diagnose a failure without taking into account the specific
  /// kind of expression that could not be type checked.
  bool diagnoseGeneralFailure();

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
  Expr *typeCheckChildIndependently(Expr *subExpr,
                                    TCCOptions options = TCCOptions());

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
  Expr *typeCheckArgumentChildIndependently(Expr *argExpr,
                                        const CalleeCandidateInfo &candidates);

  /// Attempt to diagnose a specific failure from the info we've collected from
  /// the failed constraint system.
  bool diagnoseFailure();

private:
    
  /// Attempt to produce a diagnostic for a mismatch between an expression's
  /// type and its assumed contextual type.
  bool diagnoseContextualConversionError(Type exprResultType);
  
  /// Produce a diagnostic for a general member-lookup failure (irrespective of
  /// the exact expression kind).
  bool diagnoseGeneralValueMemberFailure();
  
  /// Produce a diagnostic for a general overload resolution failure
  /// (irrespective of the exact expression kind).
  bool diagnoseGeneralOverloadFailure();
  
  /// Produce a diagnostic for a general conversion failure (irrespective of the
  /// exact expression kind).
  bool diagnoseGeneralConversionFailure();
     
  bool visitExpr(Expr *E);

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
  bool visitForcedCheckedCastExpr(ForcedCheckedCastExpr *FCCE);
  bool visitIfExpr(IfExpr *IE);
  bool visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E);
  bool visitClosureExpr(ClosureExpr *CE);
};
} // end anonymous namespace.


FailureDiagnosis::FailureDiagnosis(Expr *expr, ConstraintSystem *cs)
  : expr(expr), CS(cs) {
  assert(expr && CS);
  
  Constraint *fallbackConstraint = nullptr;
  Constraint *disjunctionConversionConstraint = nullptr;
  for (auto & constraintRef : CS->getConstraints()) {
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
    if ((!conformanceConstraint || constraint->isFavored()) &&
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
    if (!argumentConstraint || constraint->isFavored()) {
      argumentConstraint = getConstraintChoice(constraint,
                                               ConstraintKind::
                                               ArgumentTupleConversion);
    }
    
    // Overload resolution failures are often nicely descriptive, so store
    // off the first one we find.
    if (!overloadConstraint || constraint->isFavored()) {
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
    
    // When all else fails, inspect a potential conjunction or disjunction for a
    // consituent conversion.
    if (!disjunctionConversionConstraint || constraint->isFavored()) {
      disjunctionConversionConstraint =
      getConstraintChoice(constraint, ConstraintKind::Conversion, true);
    }
  }
  
  // If no more descriptive constraint was found, use the fallback constraint.
  if (fallbackConstraint &&
      !(conversionConstraint || overloadConstraint || argumentConstraint)) {
        
        if (fallbackConstraint->getKind() == ConstraintKind::ArgumentConversion)
          argumentConstraint = fallbackConstraint;
        else
          conversionConstraint = fallbackConstraint;
      }
  
  // If there's still no conversion to diagnose, use the disjunction conversion.
  if (!conversionConstraint)
    conversionConstraint = disjunctionConversionConstraint;
  
  // If there was already a conversion failure, use it.
  if (!conversionConstraint &&
      CS->failedConstraint &&
      CS->failedConstraint->getKind() != ConstraintKind::Disjunction) {
    conversionConstraint = CS->failedConstraint;
  }
}

bool FailureDiagnosis::diagnoseGeneralValueMemberFailure() {
  
  if (!valueMemberConstraint) return false;
  
  assert(valueMemberConstraint->getKind() == ConstraintKind::ValueMember ||
         valueMemberConstraint->getKind() ==
          ConstraintKind::UnresolvedValueMember);
  
  auto memberName = valueMemberConstraint->getMember().getBaseName();
  
  // Get the referenced expression from the failed constraint.
  auto anchor = expr;
  if (auto locator = valueMemberConstraint->getLocator()) {
    anchor = simplifyLocatorToAnchor(*CS, locator);
    if (!anchor)
      anchor = locator->getAnchor();
  }

  // Retypecheck the anchor type, which is the base of the member expression.
  anchor = typeCheckArbitrarySubExprIndependently(anchor);
  if (!anchor) return true;
  
  auto type = anchor->getType();

  if (typeIsNotSpecialized(type)) {
    diagnose(anchor->getLoc(), diag::could_not_find_member, memberName)
      .highlight(anchor->getSourceRange());
  } else {
    diagnose(anchor->getLoc(), diag::could_not_find_member_type,
             type, memberName)
      .highlight(anchor->getSourceRange());
  }
  
  return true;
}

bool FailureDiagnosis::diagnoseGeneralOverloadFailure() {
  
  // If this is a return expression with available conversion constraints,
  // we can produce a better diagnostic by pointing out the return expression
  // conversion failure.
  if (expr->isReturnExpr() && (conversionConstraint || argumentConstraint))
    if (diagnoseGeneralConversionFailure())
      return true;
  
  // In the absense of a better conversion constraint failure, point out the
  // inability to find an appropriate overload.
  if (!overloadConstraint)
    return false;
  
  
  auto overloadChoice = overloadConstraint->getOverloadChoice();
  std::string overloadName = overloadChoice.getDecl()->getNameStr();

  if (auto *CD = dyn_cast<ConstructorDecl>(overloadChoice.getDecl()))
    if (auto *SD = CD->getImplicitSelfDecl())
      overloadName = SD->getType()->getInOutObjectType().getString() + ".init";

  // Get the referenced expression from the failed constraint.
  auto anchor = expr;
  if (auto locator = overloadConstraint->getLocator()) {
    anchor = simplifyLocatorToAnchor(*CS, locator);
    if (!anchor)
      anchor = locator->getAnchor();
  }

  // The anchor for the constraint is almost always an OverloadedDeclRefExpr.
  // Look at the parent node in the AST to find the Apply to give a better
  // diagnostic.
  Expr *call = expr->getParentMap()[anchor];
  // Ignore parens around the callee.
  while (call && (isa<IdentityExpr>(call) || isa<AnyTryExpr>(call)))
    call = expr->getParentMap()[call];
  
  // Do some sanity checking based on the call: e.g. make sure we're invoking
  // the overloaded decl, not using it as an argument.
  Type argType;
  if (auto *AE = dyn_cast_or_null<ApplyExpr>(call)) {
    if (AE->getFn()->getSemanticsProvidingExpr() == anchor) {
      // Type check the argument list independently to try to get a concrete
      // type (ignoring context).
      auto argExpr = typeCheckArbitrarySubExprIndependently(AE->getArg(),
                                                           TCC_AllowUnresolved);
      if (!argExpr) return true;

      argType = argExpr->getType();
    }
  }
  
  if (argType.isNull() || argType->is<TypeVariableType>()) {
    diagnose(anchor->getLoc(), diag::cannot_find_appropriate_overload,
             overloadName)
       .highlight(anchor->getSourceRange());
    return true;
  }
  
  
  // Otherwise, we have a good grasp on what is going on: we have a call of an
  // unresolve overload set.  Try to dig out the candidates.
  auto apply = cast<ApplyExpr>(call);
  CalleeCandidateInfo calleeInfo(apply->getFn(), CS);
  calleeInfo.filterList(argType);

  
  // A common error is to apply an operator that only has an inout LHS (e.g. +=)
  // to non-lvalues (e.g. a local let).  Produce a nice diagnostic for this
  // case.
  if (calleeInfo.closeness == CC_NonLValueInOut) {
    Expr *firstArg = apply->getArg();
    if (auto *tuple = dyn_cast<TupleExpr>(firstArg))
      if (tuple->getNumElements())
        firstArg = tuple->getElement(0);
    
    diagnoseSubElementFailure(firstArg, apply->getLoc(), *CS,
                              diag::cannot_apply_lvalue_binop_to_subelement,
                              diag::cannot_apply_lvalue_binop_to_rvalue);
    return true;
  }

  diagnose(apply->getFn()->getLoc(),
           diag::cannot_find_appropriate_overload_with_type,
           overloadName, getTypeListString(argType))
    .highlight(apply->getSourceRange());
  
  calleeInfo.suggestPotentialOverloads(overloadName, apply->getLoc());
  return true;
}

bool FailureDiagnosis::diagnoseGeneralConversionFailure() {
  
  // Otherwise, if we have a conversion constraint, use that as the basis for
  // the diagnostic.
  if (!conversionConstraint && !argumentConstraint)
    return false;
  
  auto constraint =
    argumentConstraint ? argumentConstraint : conversionConstraint;
  
  if (conformanceConstraint) {
    if (conformanceConstraint->getTypeVariables().size() <
        constraint->getTypeVariables().size()) {
      constraint = conformanceConstraint;
    }
  }
  
  auto anchor = expr;
  if (auto locator = constraint->getLocator()) {
    anchor = simplifyLocatorToAnchor(*CS, locator);
    if (!anchor)
      anchor = locator->getAnchor();
  }
  
  std::pair<Type, Type> types = getBoundTypesFromConstraint(CS, anchor,
                                                            constraint);
  
  if (argumentConstraint) {
    diagnose(anchor->getLoc(), diag::could_not_convert_argument, types.first)
      .highlight(anchor->getSourceRange());
    return true;
  }
    
  // If it's a type variable failing a conformance, avoid printing the type
  // variable and just print the conformance.
  if ((constraint->getKind() == ConstraintKind::ConformsTo) &&
      types.first->getAs<TypeVariableType>()) {
    diagnose(anchor->getLoc(), diag::single_expression_conformance_failure,
             types.first)
      .highlight(anchor->getSourceRange());
    return true;
  }

  Type fromType;
  if (auto sub = typeCheckArbitrarySubExprIndependently(anchor))
    fromType = sub->getType();
  else
    fromType = types.first.getPointer();

  fromType = fromType->getRValueType();
  
  auto toType = CS->getConversionType(anchor);
  if (!toType)
    toType = CS->getContextualType(anchor);
  if (!toType)
    toType = types.second.getPointer();
  
  // If the second type is a type variable, the expression itself is
  // ambiguous.
  if (fromType->is<UnboundGenericType>() || toType->is<TypeVariableType>() ||
      (fromType->is<TypeVariableType>() && toType->is<ProtocolType>())) {
    auto diagID = diag::type_of_expression_is_ambiguous;
    if (isa<ClosureExpr>(anchor))
      diagID = diag::cannot_infer_closure_type;
    
    diagnose(anchor->getLoc(), diagID)
      .highlight(anchor->getSourceRange());

    if (auto *CE = dyn_cast<ClosureExpr>(anchor)) {
      if (!CE->hasSingleExpressionBody() &&
          !CE->hasExplicitResultType() &&
          !CE->getBody()->getElements().empty()) {
        diagnose(CE->getLoc(),
                 diag::mult_stmt_closures_require_explicit_result);
      }
    }

    
    return true;
  }
  
  // Special case the diagnostic for a function result-type mismatch.
  if (anchor->isReturnExpr()) {
    if (toType->isVoid()) {
      diagnose(anchor->getLoc(), diag::cannot_return_value_from_void_func);
      return true;
    }
    
    diagnose(anchor->getLoc(), diag::cannot_convert_to_return_type,
             fromType, toType)
      .highlight(anchor->getSourceRange());
    
    return true;
  }
  
  diagnose(anchor->getLoc(), diag::invalid_relation,
           Failure::TypesNotConvertible - Failure::TypesNotEqual,
           fromType, toType)
    .highlight(anchor->getSourceRange());
  
  return true;
}

bool FailureDiagnosis::diagnoseGeneralFailure() {
  
  return diagnoseGeneralValueMemberFailure() ||
         diagnoseGeneralOverloadFailure() ||
         diagnoseGeneralConversionFailure();
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
      struct TypeRestorer : public ASTWalker {
        ExprTypeSaver *TS;
        TypeRestorer(ExprTypeSaver *TS) : TS(TS) {}
        
        std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
          auto it = TS->ExprTypes.find(expr);
          if (it != TS->ExprTypes.end())
            expr->setType(it->second);
          return { true, expr };
        }
        
        bool walkToTypeLocPre(TypeLoc &TL) override {
          auto it = TS->TypeLocTypes.find(&TL);
          if (it != TS->TypeLocTypes.end())
            TL.setType(it->second.first, it->second.second);
          return true;
        }
        
        std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
          auto it = TS->PatternTypes.find(P);
          if (it != TS->PatternTypes.end())
            P->setType(it->second);
          return { true, P };
        }
        
        // Don't walk into statements.  This handles the BraceStmt in
        // non-single-expr closures, so we don't walk into their body.
        std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
          return { false, S };
        }
      };
      
      E->walk(TypeRestorer(this));
    }
  };
}


/// Unless we've already done this, retypecheck the specified subexpression on
/// its own, without including any contextual constraints or parent expr
/// nodes.  This is more likely to succeed than type checking the original
/// expression.
///
/// This can return a new expression (for e.g. when a UnresolvedDeclRef gets
/// resolved) and returns null when the subexpression fails to typecheck.
Expr *FailureDiagnosis::typeCheckChildIndependently(Expr *subExpr,
                                                    TCCOptions options) {
  // Track if this sub-expression is currently being diagnosed.
  if (Expr *res = CS->TC.exprIsBeingDiagnosed(subExpr))
    return res;
  
  // FIXME: expressions are never removed from this set.
  CS->TC.addExprForDiagnosis(subExpr, subExpr);
  
  if (isa<ClosureExpr>(subExpr))
    options |= TCC_AllowUnresolved;
  
  // These expression types can never be checked without their enclosing
  // context, so don't try - it would just make bogus diagnostics.
  if (isa<NilLiteralExpr>(subExpr) ||
  
      // A '_' is only allowed on the LHS of an assignment, so we can't descend
      // past the AssignExpr.  This is a problem below because we're not
      // allowing ambiguous solutions for subexprs, and thus the code below is
      // forced to diagnose "discard_expr_outside_of_assignment".  This should
      // really allow ambiguous expressions and diagnose it only in MiscDiags.
      isa<DiscardAssignmentExpr>(subExpr))
    return subExpr;
  
  // TupleExpr often contains things that cannot be typechecked without
  // context (usually from a parameter list), but we do it if they already have
  // an unspecialized type.
  // FIXME: This is a total hack.
  if ((isa<TupleExpr>(subExpr) ||
      
       // InOutExpr needs contextual information otherwise we complain about it
       // not being in an argument context.
       isa<InOutExpr>(subExpr)) &&
      !typeIsNotSpecialized(subExpr->getType()))
    return subExpr;
  
  
  ExprTypeSaver SavedTypeData;
  SavedTypeData.save(subExpr);
  
  // Store off the sub-expression, in case a new one is provided via the
  // type check operation.
  Expr *preCheckedExpr = subExpr;

  CS->TC.eraseTypeData(subExpr);
      
  // Disable structural checks, because we know that the overall expression
  // has type constraint problems, and we don't want to know about any
  // syntactic issues in a well-typed subexpression (which might be because
  // the context is missing).
  TypeCheckExprOptions TCEOptions = TypeCheckExprFlags::DisableStructuralChecks;

  // Claim that the result is discarded to preserve the lvalue type of
  // the expression.
  if (options.contains(TCC_AllowLValue))
    TCEOptions |= TypeCheckExprFlags::IsDiscarded;

  if (options.contains(TCC_AllowUnresolved))
    TCEOptions |= TypeCheckExprFlags::AllowUnresolvedTypeVariables;

  bool hadError = CS->TC.typeCheckExpression(subExpr, CS->DC, Type(),
                                             Type(), TCEOptions);

  // This is a terrible hack to get around the fact that typeCheckExpression()
  // might change subExpr to point to a new OpenExistentialExpr. In that case,
  // since the caller passed subExpr by value here, they would be left
  // holding on to an expression containing open existential types but
  // no OpenExistentialExpr, which breaks invariants enforced by the
  // ASTChecker.
  CS->TC.eraseOpenedExistentials(subExpr);
      
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
  
  // Walk the parents of the specified expression, handling any ClosureExprs.
  for (Expr *node = parentMap[subExpr]; node != expr; node = parentMap[node]) {
    auto *CE = dyn_cast<ClosureExpr>(node);
    if (!CE) continue;
    
    // If we have a ClosureExpr parent of the specified node, check to make sure
    // none of its arguments are type variables.  If so, these type variables
    // would be accessible to name lookup of the subexpression and may thus leak
    // in.  Reset them to null types for safe measures.
    CE->getParams()->forEachVariable([&](VarDecl *VD) {
      if (VD->getType()->hasTypeVariable() || VD->getType()->is<ErrorType>())
        VD->overwriteType(Type());
    });
  }
  
  // Otherwise, we're ok to type check the subexpr.
  return typeCheckChildIndependently(subExpr, options);
}


bool FailureDiagnosis::diagnoseContextualConversionError(Type exprResultType) {
  TypeBase *contextualType = CS->getConversionType(expr);
  
  if (!contextualType) {
    contextualType = CS->getContextualType(expr);
    if (!contextualType) {
      return false;
    }
  }
  if (exprResultType->isEqual(contextualType))
    return false;
  
  if (exprResultType->getAs<TypeVariableType>())
    return false;
  
  
  // If this is conversion failure due to a return statement with an argument
  // that cannot be coerced to the result type of the function, emit a
  // specific error.
  if (expr->isReturnExpr()) {
    if (contextualType->isVoid()) {
      diagnose(expr->getLoc(), diag::cannot_return_value_from_void_func)
        .highlight(expr->getSourceRange());
    } else {
      diagnose(expr->getLoc(), diag::cannot_convert_to_return_type,
               exprResultType, contextualType)
        .highlight(expr->getSourceRange());
    }
    return true;
  }

  
  diagnose(expr->getLoc(), diag::invalid_relation,
           Failure::FailureKind::TypesNotConvertible -
                      Failure::FailureKind::TypesNotEqual,
           exprResultType, contextualType)
    .highlight(expr->getSourceRange());
  
  return true;
}


/// When an assignment to an expression is detected and the destination is
/// invalid, emit a detailed error about the condition.
void ConstraintSystem::diagnoseAssignmentFailure(Expr *dest, Type destTy,
                                                 SourceLoc equalLoc) {
  auto &TC = getTypeChecker();

  // Diagnose obvious assignments to literals.
  if (isa<LiteralExpr>(dest->getSemanticsProvidingExpr())) {
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
typeCheckArgumentChildIndependently(Expr *argExpr,
                                    const CalleeCandidateInfo &candidates) {
  // Grab one of the candidates (if present) and get its input list to help
  // identify operators that have implicit inout arguments.
  Type exampleInputType;
  if (!candidates.empty())
    exampleInputType = candidates[0].getArgumentType();

  // FIXME: This should all just be a matter of getting type type of the
  // sub-expression, but this doesn't work well when typeCheckChildIndependently
  // is over-conservative w.r.t. TupleExprs.
  if (auto *TE = dyn_cast<TupleExpr>(argExpr)) {
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

  TCCOptions options;
  if (exampleInputType && exampleInputType->is<InOutType>())
    options |= TCC_AllowLValue;

  return typeCheckChildIndependently(unwrapParenExpr(argExpr), options);
}

bool FailureDiagnosis::visitBinaryExpr(BinaryExpr *binop) {
  CalleeCandidateInfo calleeInfo(binop->getFn(), CS);
  assert(!calleeInfo.candidates.empty() && "unrecognized binop function kind");

  auto checkedArgExpr = typeCheckArgumentChildIndependently(binop->getArg(),
                                                            calleeInfo);
  if (!checkedArgExpr) return true;

  // Pre-checking can turn (T,U) into a TypeExpr.  That's an artifact
  // of independent type-checking; just use the standard diagnostics
  // paths.
  auto argExpr = dyn_cast<TupleExpr>(checkedArgExpr);
  if (!argExpr) return visitExpr(binop);

  auto argTupleType = argExpr->getType()->castTo<TupleType>();
  calleeInfo.filterList(argTupleType);

  if (calleeInfo.closeness == CC_ExactMatch) {
    
    // Otherwise, whatever the result type of the call happened to be must not
    // have been what we were looking for.
    auto resultTy = getTypeOfTypeCheckedChildIndependently(binop);
    if (!resultTy)
      return true;
    
    if (typeIsNotSpecialized(resultTy))
      resultTy = calleeInfo[0].getResultType();
    
    diagnose(binop->getLoc(), diag::result_type_no_match, resultTy)
      .highlight(binop->getSourceRange());
    return true;
  }
  
  // A common error is to apply an operator that only has an inout LHS (e.g. +=)
  // to non-lvalues (e.g. a local let).  Produce a nice diagnostic for this
  // case.
  if (calleeInfo.closeness == CC_NonLValueInOut) {
    diagnoseSubElementFailure(argExpr->getElement(0), binop->getLoc(), *CS,
                              diag::cannot_apply_lvalue_binop_to_subelement,
                              diag::cannot_apply_lvalue_binop_to_rvalue);
    return true;
  }

  auto argType1 = argTupleType->getElementType(0)->getRValueType();
  auto argType2 = argTupleType->getElementType(1)->getRValueType();
  std::string overloadName = calleeInfo[0].decl->getNameStr();
  assert(!overloadName.empty());
  if (!argType1->isEqual(argType2)) {
    diagnose(binop->getLoc(), diag::cannot_apply_binop_to_args,
             overloadName, argType1, argType2)
      .highlight(argExpr->getElement(0)->getSourceRange())
      .highlight(argExpr->getElement(1)->getSourceRange());
  } else {
    diagnose(binop->getLoc(), diag::cannot_apply_binop_to_same_args,
             overloadName, argType1)
      .highlight(argExpr->getElement(0)->getSourceRange())
      .highlight(argExpr->getElement(1)->getSourceRange());
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
                                                     calleeInfo);
  if (!argExpr) return true;

  auto argType = argExpr->getType();
  calleeInfo.filterList(argType);

  if (calleeInfo.closeness == CC_ExactMatch) {
    // Otherwise, whatever the result type of the call happened to be must not
    // have been what we were looking for.
    auto resultTy = getTypeOfTypeCheckedChildIndependently(applyExpr);
    if (!resultTy)
      return true;
    
    if (typeIsNotSpecialized(resultTy))
      resultTy = calleeInfo[0].getResultType();
    
    diagnose(applyExpr->getLoc(), diag::result_type_no_match, resultTy)
      .highlight(applyExpr->getSourceRange());
    return true;
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
                                                       calleeInfo);
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
    if (!typeIsNotSpecialized(baseType) &&
        !CS->TC.isConvertibleTo(baseType, instanceTy, CS->DC)) {
      selfConstraint = CC_SelfMismatch;
    }

    // Explode out multi-index subscripts to find the best match.
    return std::max(evaluateCloseness(SD->getIndicesType(),decomposedIndexType),
                    selfConstraint);
  });

  // TODO: Is there any reason to check for CC_NonLValueInOut here?
  
  if (calleeInfo.closeness == CC_ExactMatch) {
    // Otherwise, the return type of the subscript happened to not have been
    // what we were looking for.
    auto resultTy = getTypeOfTypeCheckedChildIndependently(SE);
    if (!resultTy)
      return true;
    
    if (!typeIsNotSpecialized(resultTy)) {
      // If we got a strong type back, then we know what the subscript produced.
    } else if (calleeInfo.size() == 1) {
      // If we have one candidate, the result must be what that candidate
      // produced.
      resultTy = calleeInfo[0].getResultType();
    } else {
      diagnose(SE->getLoc(), diag::result_type_no_match_ambiguous)
        .highlight(SE->getSourceRange());
      calleeInfo.suggestPotentialOverloads("subscript", SE->getLoc());
      return true;
    }
    
    // Only one choice.
    diagnose(SE->getLoc(), diag::result_type_no_match, resultTy)
      .highlight(SE->getSourceRange());
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


bool FailureDiagnosis::visitCallExpr(CallExpr *callExpr) {
  // Type check the function subexpression to resolve a type for it if possible.
  auto fnExpr = typeCheckChildIndependently(callExpr->getFn(),
                                            TCC_AllowUnresolved);
  if (!fnExpr) return true;

  CalleeCandidateInfo calleeInfo(fnExpr, CS);

  // If we resolved a concrete expression for the callee, and it has
  // non-function/non-metatype type, then we cannot call it!
  auto fnType = fnExpr->getType()->getRValueType();

  if (!typeIsNotSpecialized(fnType) &&
      !fnType->is<AnyFunctionType>() && !fnType->is<MetatypeType>()) {
    diagnose(callExpr->getArg()->getStartLoc(),
             diag::cannot_call_non_function_value, fnExpr->getType())
    .highlight(fnExpr->getSourceRange());
    return true;
  }
  
#if 0
  Type argType;  // Type of the argument list, if knowable.
  if (auto FTy = fnType->getAs<AnyFunctionType>())
    if (!typeIsNotSpecialized(FTy->getInput()))
      argType = FTy->getInput();
#endif


  // Get the expression result of type checking the arguments to the call
  // independently, so we have some idea of what we're working with.
  auto argExpr = typeCheckArgumentChildIndependently(callExpr->getArg(),
                                                     calleeInfo);
  if (!argExpr)
    return true; // already diagnosed.

  calleeInfo.filterList(argExpr->getType());

  bool isInitializer = isa<TypeExpr>(fnExpr);
  auto overloadName = calleeInfo.declName;

  std::string argString = getTypeListString(argExpr->getType());

  // If we couldn't get the name of the callee, then it must be something of a
  // more complex "value of function type".
  if (overloadName.empty()) {
    // The most common unnamed value of closure type is a ClosureExpr, so
    // special case it.
    if (auto CE = dyn_cast<ClosureExpr>(fnExpr->getSemanticsProvidingExpr())) {
      if (typeIsNotSpecialized(fnType))
        diagnose(argExpr->getStartLoc(), diag::cannot_invoke_closure, argString)
          .highlight(fnExpr->getSourceRange());
      else
        diagnose(argExpr->getStartLoc(), diag::cannot_invoke_closure_type,
                 fnType, argString)
          .highlight(fnExpr->getSourceRange());
      
      // If this is a multi-statement closure with no explicit result type, emit
      // a note to clue the developer in.
      if (!CE->hasSingleExpressionBody() &&
          !CE->hasExplicitResultType() &&
          !CE->getBody()->getElements().empty()) {
        diagnose(fnExpr->getLoc(),
                 diag::mult_stmt_closures_require_explicit_result);
      }

    } else if (typeIsNotSpecialized(fnType)) {
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
  if (isa<LiteralExpr>(assignExpr->getDest()->getSemanticsProvidingExpr())) {
    diagnose(assignExpr->getLoc(), diag::cannot_assign_to_literal);
    return true;
  }

  // If the source type is already an error type, we've already posted an error.
  auto srcExpr = typeCheckChildIndependently(assignExpr->getSrc());
  if (!srcExpr) return true;

  auto destExpr = typeCheckChildIndependently(assignExpr->getDest(),
                                              TCC_AllowLValue);
  if (!destExpr) return true;

  auto destType = destExpr->getType();
  auto srcType = srcExpr->getType();

  // If the result type is a non-lvalue, then we are failing because it is
  // immutable and that's not a great thing to assign to.
  if (!destType->isLValueType()) {
    CS->diagnoseAssignmentFailure(destExpr, destType, assignExpr->getLoc());
    return true;
  }

  diagnose(srcExpr->getLoc(), diag::cannot_assign_values, srcType,
           destType->getRValueType());
  return true;
}

bool FailureDiagnosis::visitInOutExpr(InOutExpr *IOE) {
  auto subExpr = typeCheckChildIndependently(IOE->getSubExpr(),
                                             TCC_AllowLValue);

  auto subExprType = subExpr->getType();

  // The common cause is that the operand is not an lvalue.
  if (!subExprType->isLValueType()) {
    diagnoseSubElementFailure(subExpr, IOE->getLoc(), *CS,
                              diag::cannot_pass_rvalue_inout_subelement,
                              diag::cannot_pass_rvalue_inout);
    return true;
  }
  
  return diagnoseGeneralFailure();
}

bool FailureDiagnosis::visitCoerceExpr(CoerceExpr *CE) {
  Expr *subExpr = typeCheckChildIndependently(CE->getSubExpr());
  if (!subExpr) return true;
  Type subType = subExpr->getType();

  std::pair<Type, Type> conversionTypes(nullptr, nullptr);
  if (conversionConstraint &&
      conversionConstraint->getKind() == ConstraintKind::ExplicitConversion &&
      conversionConstraint->getLocator()->getAnchor() == expr) {
    conversionTypes = getBoundTypesFromConstraint(CS, CE, conversionConstraint);
  } else {
    conversionTypes.first = subType->getLValueOrInOutObjectType();
    conversionTypes.second = CE->getType();
  }

  if (conversionTypes.first && conversionTypes.second) {
    diagnose(CE->getLoc(), diag::invalid_relation,
             Failure::TypesNotConvertible - Failure::TypesNotEqual,
             conversionTypes.first, conversionTypes.second)
      .highlight(CE->getSourceRange());
    return true;
  }

  return diagnoseGeneralFailure();
}

bool FailureDiagnosis::
visitForcedCheckedCastExpr(ForcedCheckedCastExpr *FCE) {
  Expr *subExpr = typeCheckChildIndependently(FCE->getSubExpr());
  if (!subExpr) return true;

  Type subType = subExpr->getType();

  std::pair<Type, Type> conversionTypes(nullptr, nullptr);
  if (conversionConstraint &&
      conversionConstraint->getKind() == ConstraintKind::CheckedCast &&
      conversionConstraint->getLocator()->getAnchor() == expr) {
    conversionTypes = getBoundTypesFromConstraint(CS, FCE,
                                                  conversionConstraint);
  } else {
    conversionTypes.first = subType->getLValueOrInOutObjectType();
    conversionTypes.second = FCE->getType();
  }

  if (conversionTypes.first && conversionTypes.second) {
    diagnose(FCE->getLoc(), diag::invalid_relation,
             Failure::TypesNotConvertible - Failure::TypesNotEqual,
             conversionTypes.first, conversionTypes.second)
      .highlight(FCE->getSourceRange());
    return true;
  }

  return diagnoseGeneralFailure();
}

bool FailureDiagnosis::visitForceValueExpr(ForceValueExpr *FVE) {
  auto argExpr = typeCheckChildIndependently(FVE->getSubExpr());
  if (!argExpr) return true;
  auto argType = argExpr->getType();

  // If the subexpression type checks as a non-optional type, then that is the
  // error.  Produce a specific diagnostic about this.
  if (argType->getOptionalObjectType().isNull()) {
    diagnose(FVE->getLoc(), diag::invalid_force_unwrap, argType)
      .fixItRemove(FVE->getExclaimLoc())
      .highlight(FVE->getSourceRange());
    return true;
  }
  
  return diagnoseGeneralFailure();
}

bool FailureDiagnosis::visitBindOptionalExpr(BindOptionalExpr *BOE) {
  auto argExpr = typeCheckChildIndependently(BOE->getSubExpr());
  if (!argExpr) return true;
  auto argType = argExpr->getType();

  // If the subexpression type checks as a non-optional type, then that is the
  // error.  Produce a specific diagnostic about this.
  if (argType->getOptionalObjectType().isNull()) {
    diagnose(BOE->getQuestionLoc(), diag::invalid_optional_chain, argType)
      .highlight(BOE->getSourceRange())
      .fixItRemove(BOE->getQuestionLoc());
    return true;
  }

  return diagnoseGeneralFailure();
}

bool FailureDiagnosis::visitIfExpr(IfExpr *IE) {
  // If type checking of the IfExpr failed, but each of the subexprs got their
  // own concrete types, then either the condition wasn't a boolean type, or
  // the true/false arms didn't match.  If the condition wasn't of boolean type,
  // we should have seen this already as an unavoidable failure.  That means
  // that the only reason we could be here is because of a true/false arm
  // mismatch.
  if (!typeCheckChildIndependently(IE->getCondExpr()))
    return true;

  auto trueExpr = typeCheckChildIndependently(IE->getThenExpr());
  if (!trueExpr) return true;

  auto falseExpr = typeCheckChildIndependently(IE->getElseExpr());
  if (!falseExpr) return true;

  
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
  return diagnoseGeneralFailure();
}


bool FailureDiagnosis::visitClosureExpr(ClosureExpr *CE) {
  // ClosureExprs are likely to get some clever handling in the future, but for
  // now we need to defend against type variables from our constraint system
  // leaking into recursive constraints systems formed when checking the body
  // of the closure.  These typevars come into them when the body does name
  // lookups against the parameter decls.
  //
  // Handle this by rewriting the arguments to Type().  CSGen has special
  // handling for ParamDecls from closure arguments with null types.
  //
  // TODO: If there is a partial type available for the closure, we should apply
  // whatever information we have about it to the paramdecls.
  CE->getParams()->forEachVariable([&](VarDecl *VD) {
    if (VD->getType()->hasTypeVariable() || VD->getType()->is<ErrorType>())
      VD->overwriteType(Type());
  });
  
  return visitExpr(CE);
}

bool FailureDiagnosis::visitExpr(Expr *E) {
  // Check each of our immediate children to see if any of them are
  // independently invalid.
  bool errorInSubExpr = false;
  
  E->forEachChildExpr([&](Expr *Child) {
    // If we already found an error, stop checking.
    if (errorInSubExpr) return;
    
    // Otherwise this subexpr is an error if type checking it produces an error.
    errorInSubExpr |= !typeCheckChildIndependently(Child);
  });
  
  // If any of the children were errors, we're done.
  if (errorInSubExpr)
    return true;
  
  // Otherwise, produce a more generic error.
  return diagnoseGeneralFailure();
}


bool FailureDiagnosis::diagnoseFailure() {
  assert(CS && expr);
  
  // Our general approach is to do a depth first traversal of the broken
  // expression tree, type checking as we go.  If we find a subtree that cannot
  // be type checked on its own (even to an incomplete type) then that is where
  // we focus our attention.  If we do find a type, we use it to check for
  // contextual type mismatches.
  auto subExprTy = getTypeOfTypeCheckedChildIndependently(expr);
  
  // We've already diagnosed the error.
  if (!subExprTy)
    return true;
  
  // If there is a contextual type that mismatches, diagnose it as the problem.
  if (diagnoseContextualConversionError(subExprTy))
    return true;

  return visit(expr);
}

/// Given a specific expression and the remnants of the failed constraint
/// system, produce a specific diagnostic.
bool ConstraintSystem::diagnoseFailureForExpr(Expr *expr) {
  if (auto *RB = dyn_cast<RebindSelfInConstructorExpr>(expr))
    expr = RB->getSubExpr();
  
  
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
  if (ActiveConstraints.empty() && InactiveConstraints.empty() &&
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
    if (!diagnoseFailureForExpr(expr) &&
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
  bool result = diagnoseFailureForExpr(expr);
  assert(result && "didn't diagnose any failure?"); (void)result;
    
  return true;
}
