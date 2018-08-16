//===--- CalleeCandidateInfo.cpp - Constraint Diagnostics -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements diagnostics for the type checker.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "CSDiag.h"
#include "CalleeCandidateInfo.h"
#include "TypeCheckAvailability.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/TypeMatcher.h"

using namespace swift;
using namespace constraints;

/// \brief Determine whether one type would be a valid substitution for an
/// archetype.
///
/// \param type The potential type.
///
/// \param archetype The archetype for which type may (or may not) be
/// substituted.
///
/// \param dc The context of the check.
///
/// \returns true if \c t1 is a valid substitution for \c t2.
static bool isSubstitutableFor(Type type, ArchetypeType *archetype,
                               DeclContext *dc) {
  if (archetype->requiresClass() && !type->satisfiesClassConstraint())
    return false;

  if (auto superclass = archetype->getSuperclass()) {
    if (!superclass->isExactSuperclassOf(type))
      return false;
  }

  for (auto proto : archetype->getConformsTo()) {
    if (!dc->getParentModule()->lookupConformance(
          type, proto))
      return false;
  }

  return true;
}

UncurriedCandidate::UncurriedCandidate(ValueDecl *decl, unsigned level)
: declOrExpr(decl), level(level), substituted(false) {

  if (auto *PD = dyn_cast<ParamDecl>(decl)) {
    if (PD->hasValidSignature())
      entityType = PD->getType();
    else
      entityType = PD->getASTContext().TheUnresolvedType;
  } else {
    entityType = decl->getInterfaceType();
    auto *DC = decl->getInnermostDeclContext();
    if (auto *GFT = entityType->getAs<GenericFunctionType>()) {
      auto subs = DC->getGenericEnvironmentOfContext()
      ->getForwardingSubstitutionMap();
      entityType = GFT->substGenericArgs(subs);
    } else {
      // FIXME: look through unforced IUOs here?

      entityType = DC->mapTypeIntoContext(entityType);
    }
  }
  
  // For some reason, subscripts and properties don't include their self
  // type.  Tack it on for consistency with other members.
  if (isa<AbstractStorageDecl>(decl)) {
    if (decl->getDeclContext()->isTypeContext()) {
      auto instanceTy = decl->getDeclContext()->getSelfTypeInContext();
      entityType = FunctionType::get({FunctionType::Param(instanceTy)},
                                     entityType);
    }
  }
}

ArrayRef<Identifier> UncurriedCandidate::getArgumentLabels(
                                       SmallVectorImpl<Identifier> &scratch) {
  scratch.clear();
  if (auto decl = getDecl()) {
    if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
      if (func->hasImplicitSelfDecl()) {
        if (level == 0) {
          scratch.push_back(Identifier());
          return scratch;
        }

        --level;
      }

      if (level == 0) {
        // Retrieve the argument labels of the corresponding parameter list.
        for (auto param : *func->getParameters()) {
          scratch.push_back(param->getArgumentName());
        }
        return scratch;
      }
    } else if (auto enumElt = dyn_cast<EnumElementDecl>(decl)) {
      // 'self'
      if (level == 0) {
        scratch.push_back(Identifier());
        return scratch;
      }
      
      // The associated data of the case.
      if (level == 1) {
        auto *paramList = enumElt->getParameterList();
        if (!paramList) return { };
        for (auto param : *paramList) {
          scratch.push_back(param->getArgumentName());
        }
        return scratch;
      }
    }
  }

  if (!hasParameters())
    return {};

  for (const auto &param : getParameters())
    scratch.push_back(param.getLabel());

  return scratch;
}

void UncurriedCandidate::dump() const {
  if (auto decl = getDecl())
    decl->dumpRef(llvm::errs());
  else
    llvm::errs() << "<<EXPR>>";
  llvm::errs() << " - uncurry level " << level;
  
  if (auto FT = getUncurriedFunctionType())
    llvm::errs() << " - type: " << Type(FT) << "\n";
  else
    llvm::errs() << " - type <<NONFUNCTION>>: " << entityType << "\n";
}

void CalleeCandidateInfo::dump() const {
  llvm::errs() << "CalleeCandidateInfo for '" << declName << "': closeness="
  << unsigned(closeness) << "\n";
  llvm::errs() << candidates.size() << " candidates:\n";
  for (auto c : candidates) {
    llvm::errs() << "  ";
    c.dump();
  }
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
  SmallVector<ClosenessResultTy, 4> closenessList;
  closenessList.reserve(candidates.size());
  for (auto decl : candidates) {
    auto declCloseness = predicate(decl);
    
    // If we have a decl identified, refine the match.
    if (auto VD = decl.getDecl()) {
      // If this candidate otherwise matched but was marked unavailable, then
      // treat it as unavailable, which is a very close failure.
      if (declCloseness.first == CC_ExactMatch &&
          VD->getAttrs().isUnavailable(CS.getASTContext()) &&
          !CS.TC.getLangOpts().DisableAvailabilityChecking)
        declCloseness.first = CC_Unavailable;
      
      // Likewise, if the candidate is inaccessible from the scope it is being
      // accessed from, mark it as inaccessible or a general mismatch.
      if (VD->hasAccess() && !VD->isAccessibleFrom(CS.DC)) {
        // If this was an exact match, downgrade it to inaccessible, so that
        // accessible decls that are also an exact match will take precedence.
        // Otherwise consider it to be a general mismatch so we only list it in
        // an overload set as a last resort.
        if (declCloseness.first == CC_ExactMatch)
          declCloseness.first = CC_Inaccessible;
        else
          declCloseness.first = CC_GeneralMismatch;
      }
    }
    
    closenessList.push_back(declCloseness);
    closeness = std::min(closeness, closenessList.back().first);
  }
  
  // Now that we know the minimum closeness, remove all the elements that aren't
  // as close.  Keep track of argument failure information if the entire
  // matching candidate set agrees.
  unsigned NextElt = 0;
  for (unsigned i = 0, e = candidates.size(); i != e; ++i) {
    // If this decl in the result list isn't a close match, ignore it.
    if (closeness != closenessList[i].first)
      continue;
    
    // Otherwise, preserve it.
    candidates[NextElt++] = candidates[i];
    
    if (NextElt == 1)
      failedArgument = closenessList[i].second;
    else if (failedArgument != closenessList[i].second)
      failedArgument = FailedArgumentInfo();
  }
  
  candidates.erase(candidates.begin()+NextElt, candidates.end());
}



/// Given an incompatible argument being passed to a parameter, decide whether
/// it is a "near" miss or not.  We consider something to be a near miss if it
/// is due to a common sort of problem (e.g. function type passed to wrong
/// function type, or T? passed to something expecting T) where a far miss is a
/// completely incompatible type (Int where Float is expected).  The notion of a
/// near miss is used to refine overload sets to a smaller candidate set that is
/// the most relevant options.
static bool argumentMismatchIsNearMiss(Type argType, Type paramType) {
  // If T? was passed to something expecting T, then it is a near miss.
  if (auto argOptType = argType->getOptionalObjectType())
    if (argOptType->isEqual(paramType))
      return true;
  
  // If these are both function types, then they are near misses.  We consider
  // incompatible function types to be near so that functions and non-function
  // types are considered far.
  if (argType->is<AnyFunctionType>() && paramType->is<AnyFunctionType>())
    return true;
  
  // Otherwise, this is some other sort of incompatibility.
  return false;
}

static bool isUnresolvedOrTypeVarType(Type ty) {
  return ty->isTypeVariableOrMember() || ty->is<UnresolvedType>();
}

/// Given a parameter type that may contain generic type params and an actual
/// argument type, decide whether the param and actual arg have the same shape
/// and equal fixed type portions, and return by reference each archetype and
/// the matching portion of the actual arg type where that archetype appears.
static bool findGenericSubstitutions(DeclContext *dc, Type paramType,
                                     Type actualArgType,
                                     TypeSubstitutionMap &archetypesMap) {
  // Type visitor doesn't handle unresolved types.
  if (isUnresolvedOrTypeVarType(paramType) ||
      isUnresolvedOrTypeVarType(actualArgType))
    return false;
  
  class GenericVisitor : public TypeMatcher<GenericVisitor> {
    DeclContext *dc;
    TypeSubstitutionMap &archetypesMap;
    
  public:
    GenericVisitor(DeclContext *dc, TypeSubstitutionMap &archetypesMap)
    : dc(dc), archetypesMap(archetypesMap) {}
    
    bool mismatch(TypeBase *paramType, TypeBase *argType,
                  Type sugaredFirstType) {
      return paramType->isEqual(argType);
    }
    
    bool mismatch(SubstitutableType *paramType, TypeBase *argType,
                  Type sugaredFirstType) {
      Type type = paramType;
      if (type->is<GenericTypeParamType>()) {
        assert(dc);
        type = dc->mapTypeIntoContext(paramType);
      }
      
      if (auto archetype = type->getAs<ArchetypeType>()) {
        auto existing = archetypesMap[archetype];
        if (existing)
          return existing->isEqual(argType);
        archetypesMap[archetype] = argType;
        return true;
      }
      return false;
    }
  };
  
  if (paramType->hasError())
    return false;
  
  GenericVisitor visitor(dc, archetypesMap);
  return visitor.match(paramType, actualArgType);
}

/// Determine how close an argument list is to an already decomposed argument
/// list.  If the closeness is a miss by a single argument, then this returns
/// information about that failure.
CalleeCandidateInfo::ClosenessResultTy
CalleeCandidateInfo::evaluateCloseness(UncurriedCandidate candidate,
                                       ArrayRef<AnyFunctionType::Param> actualArgs) {
  auto *dc = candidate.getDecl()
  ? candidate.getDecl()->getInnermostDeclContext()
  : nullptr;

  if (!candidate.hasParameters())
    return {CC_GeneralMismatch, {}};

  auto candArgs = candidate.getParameters();
  llvm::SmallBitVector candDefaultMap =
    computeDefaultMap(candArgs, candidate.getDecl(), candidate.level);
  
  struct OurListener : public MatchCallArgumentListener {
    CandidateCloseness result = CC_ExactMatch;
  public:
    CandidateCloseness getResult() const {
      return result;
    }
    void extraArgument(unsigned argIdx) override {
      result = CC_ArgumentCountMismatch;
    }
    void missingArgument(unsigned paramIdx) override {
      result = CC_ArgumentCountMismatch;
    }
    bool missingLabel(unsigned paramIdx) override {
      result = CC_ArgumentLabelMismatch;
      return true;
    }
    bool extraneousLabel(unsigned paramIdx) override {
      result = CC_ArgumentLabelMismatch;
      return true;
    }
    bool incorrectLabel(unsigned paramIdx) override {
      result = CC_ArgumentLabelMismatch;
      return true;
    }
    void outOfOrderArgument(unsigned argIdx, unsigned prevArgIdx) override {
      result = CC_ArgumentLabelMismatch;
    }
    bool relabelArguments(ArrayRef<Identifier> newNames) override {
      result = CC_ArgumentLabelMismatch;
      return true;
    }
  } listener;
  
  // Use matchCallArguments to determine how close the argument list is (in
  // shape) to the specified candidates parameters.  This ignores the concrete
  // types of the arguments, looking only at the argument labels etc.
  SmallVector<ParamBinding, 4> paramBindings;
  if (matchCallArguments(actualArgs, candArgs,
                         candDefaultMap,
                         hasTrailingClosure,
                         /*allowFixes:*/ true,
                         listener, paramBindings))
    // On error, get our closeness from whatever problem the listener saw.
    return { listener.getResult(), {}};
  
  // If we found a mapping, check to see if the matched up arguments agree in
  // their type and count the number of mismatched arguments.
  unsigned mismatchingArgs = 0;
  
  // Known mapping of archetypes in all arguments so far. An archetype may map
  // to another archetype if the constraint system substituted one for another.
  TypeSubstitutionMap allGenericSubstitutions;
  
  // Number of args of one generic archetype which are mismatched because
  // isSubstitutableFor() has failed. If all mismatches are of this type, we'll
  // return a different closeness for better diagnoses.
  Type nonSubstitutableArchetype = nullptr;
  unsigned nonSubstitutableArgs = 0;
  
  // The type of failure is that multiple occurrences of the same generic are
  // being passed arguments with different concrete types.
  bool genericWithDifferingConcreteTypes = false;
  
  // We classify an argument mismatch as being a "near" miss if it is a very
  // likely match due to a common sort of problem (e.g. wrong flags on a
  // function type, optional where none was expected, etc).  This allows us to
  // heuristically filter large overload sets better.
  bool mismatchesAreNearMisses = true;
  
  CalleeCandidateInfo::FailedArgumentInfo failureInfo;
  
  // Local function which extracts type from the parameter container.
  auto getParamResultType = [](const AnyFunctionType::Param &param) -> Type {
    // If parameter is marked as @autoclosure, we are
    // only interested in it's resulting type.
    if (param.isAutoClosure()) {
      if (auto fnType = param.getType()->getAs<AnyFunctionType>())
        return fnType->getResult();
    }
    
    return param.getType();
  };
  
  for (unsigned i = 0, e = paramBindings.size(); i != e; ++i) {
    // Bindings specify the arguments that source the parameter.  The only case
    // this returns a non-singular value is when there are varargs in play.
    auto &bindings = paramBindings[i];
    auto param = candArgs[i];
    auto paramType = getParamResultType(param);
    
    for (auto argNo : bindings) {
      auto argType = getParamResultType(actualArgs[argNo]);
      auto rArgType = argType->getRValueType();
      
      // FIXME: Right now, a "matching" overload is one with a parameter whose
      // type is identical to the argument type, or substitutable via handling
      // of functions with primary archetypes in one or more parameters.
      // We can still do something more sophisticated with this.
      // FIXME: Use TC.isConvertibleTo?
      
      TypeSubstitutionMap archetypesMap;
      bool matched;
      if (paramType->hasUnresolvedType())
        matched = true;
      else if (rArgType->hasTypeVariable() || rArgType->hasUnresolvedType())
        matched = false;
      else {
        auto matchType = paramType;
        // If the parameter is an inout type, and we have a proper lvalue, match
        // against the type contained therein.
        if (param.isInOut() && argType->is<LValueType>())
          matchType = matchType->getInOutObjectType();
        
        if (candidate.substituted) {
          matchType.findIf([&](Type type) -> bool {
            // If the replacement is itself an archetype, then the constraint
            // system was asserting equivalencies between different levels of
            // generics, rather than binding a generic to a concrete type (and we
            // don't/won't have a concrete type). In which case, it is the
            // replacement we are interested in, since it is the one in our current
            // context. That generic type should equal itself.
            if (auto archetype = type->getAs<ArchetypeType>()) {
              archetypesMap[archetype] = archetype;
            }
            return false;
          });
        }
        matched = findGenericSubstitutions(dc, matchType, rArgType,
                                           archetypesMap);
      }
      
      if (matched) {
        for (auto pair : archetypesMap) {
          auto archetype = pair.first->castTo<ArchetypeType>();
          auto substitution = pair.second;
          
          auto existingSubstitution = allGenericSubstitutions[archetype];
          if (!existingSubstitution) {
            // New substitution for this callee.
            allGenericSubstitutions[archetype] = substitution;
            
            // Not yet handling nested archetypes.
            if (!archetype->isPrimary())
              return { CC_ArgumentMismatch, {}};
            
            if (!isSubstitutableFor(substitution, archetype, CS.DC)) {
              // If we have multiple non-substitutable types, this is just a mismatched mess.
              if (!nonSubstitutableArchetype.isNull())
                return { CC_ArgumentMismatch, {}};
              
              if (auto argOptType = argType->getOptionalObjectType())
                mismatchesAreNearMisses &=
                  isSubstitutableFor(argOptType, archetype, CS.DC);
              else
                mismatchesAreNearMisses = false;
              
              nonSubstitutableArchetype = archetype;
              nonSubstitutableArgs = 1;
              matched = false;
            }
          } else {
            // Substitution for the same archetype as in a previous argument.
            bool isNonSubstitutableArchetype = !nonSubstitutableArchetype.isNull() &&
            nonSubstitutableArchetype->isEqual(archetype);
            if (substitution->isEqual(existingSubstitution)) {
              if (isNonSubstitutableArchetype) {
                ++nonSubstitutableArgs;
                matched = false;
              }
            } else {
              // If we have only one nonSubstitutableArg so far, then this different
              // type might be the one that we should be substituting for instead.
              // Note that failureInfo is already set correctly for that case.
              if (isNonSubstitutableArchetype && nonSubstitutableArgs == 1 &&
                  isSubstitutableFor(substitution, archetype, CS.DC)) {
                mismatchesAreNearMisses = argumentMismatchIsNearMiss(existingSubstitution, substitution);
                allGenericSubstitutions[archetype] = substitution;
              } else {
                genericWithDifferingConcreteTypes = true;
                matched = false;
              }
            }
          }
        }
      }
      
      if (matched)
        continue;

      // If the real argument is unresolved, the candidate isn't a mismatch because
      // the type could be anything, but it's still useful to save the argument number as
      // failureInfo.
      if (rArgType->hasUnresolvedType() && !mismatchingArgs) {
        failureInfo.argumentNumber = argNo;
      } else {
        if (archetypesMap.empty())
          mismatchesAreNearMisses &= argumentMismatchIsNearMiss(argType, paramType);
        ++mismatchingArgs;

        failureInfo.argumentNumber = argNo;
        failureInfo.parameterType = paramType;
        if (paramType->hasTypeParameter())
          failureInfo.declContext = dc;
      }
    }
  }
  
  if (mismatchingArgs == 0)
    return { CC_ExactMatch, failureInfo};
  
  // Check to see if the first argument expects an inout argument, but is not
  // an lvalue.
  if (candArgs[0].isInOut() &&
      !(actualArgs[0].getType()->hasLValueType() || actualArgs[0].isInOut())) {
    return { CC_NonLValueInOut, {}};
  }
  
  // If we have exactly one argument mismatching, classify it specially, so that
  // close matches are prioritized against obviously wrong ones.
  if (mismatchingArgs == 1) {
    CandidateCloseness closeness;
    if (allGenericSubstitutions.empty()) {
      closeness = mismatchesAreNearMisses ? CC_OneArgumentNearMismatch
      : CC_OneArgumentMismatch;
    } else {
      // If the failure is that different occurrences of the same generic have
      // different concrete types, substitute in all the concrete types we've found
      // into the failureInfo to improve diagnosis.
      if (genericWithDifferingConcreteTypes) {
        auto newType = failureInfo.parameterType.transform([&](Type type) -> Type {
          if (auto archetype = type->getAs<ArchetypeType>())
            if (auto replacement = allGenericSubstitutions[archetype])
              return replacement;
          return type;
        });
        failureInfo.parameterType = newType;
      }
      
      closeness = mismatchesAreNearMisses ? CC_OneGenericArgumentNearMismatch
      : CC_OneGenericArgumentMismatch;
    }
    // Return information about the single failing argument.
    return { closeness, failureInfo };
  }
  
  if (nonSubstitutableArgs == mismatchingArgs)
    return { CC_GenericNonsubstitutableMismatch, failureInfo };
  
  auto closeness = mismatchesAreNearMisses ? CC_ArgumentNearMismatch
  : CC_ArgumentMismatch;
  return { closeness, {}};
}

void CalleeCandidateInfo::collectCalleeCandidates(Expr *fn,
                                                  bool implicitDotSyntax) {
  fn = fn->getValueProvidingExpr();
  
  // Treat a call to a load of a variable as a call to that variable, it is just
  // the lvalue'ness being removed.
  if (auto load = dyn_cast<LoadExpr>(fn)) {
    if (isa<DeclRefExpr>(load->getSubExpr()))
      return collectCalleeCandidates(load->getSubExpr(),
                                     /*implicitDotSyntax=*/false);
  }
  
  // Determine the callee level for a "bare" reference to the given
  // declaration.
  auto getCalleeLevel = [implicitDotSyntax](ValueDecl *decl) -> unsigned {
    if (auto func = dyn_cast<FuncDecl>(decl)) {
      if (func->isOperator() && func->getDeclContext()->isTypeContext() &&
          !implicitDotSyntax)
        return 1;
    }
    
    return 0;
  };
  
  if (auto declRefExpr = dyn_cast<DeclRefExpr>(fn)) {
    auto decl = declRefExpr->getDecl();
    candidates.push_back({ decl, getCalleeLevel(decl) });
    declName = decl->getBaseName().userFacingName();
    return;
  }
  
  if (auto declRefExpr = dyn_cast<OtherConstructorDeclRefExpr>(fn)) {
    auto decl = declRefExpr->getDecl();
    candidates.push_back({ decl, getCalleeLevel(decl) });
    
    if (auto selfTy = decl->getDeclContext()->getSelfInterfaceType())
      declName = selfTy.getString() + ".init";
    else
      declName = "init";
    return;
  }
  
  if (auto overloadedDRE = dyn_cast<OverloadedDeclRefExpr>(fn)) {
    for (auto cand : overloadedDRE->getDecls()) {
      candidates.push_back({ cand, getCalleeLevel(cand) });
    }
    
    if (!candidates.empty())
      declName = candidates[0].getDecl()->getBaseName().userFacingName();
    return;
  }
  
  if (auto TE = dyn_cast<TypeExpr>(fn)) {
    // It's always a metatype type, so use the instance type name.
    auto instanceType = CS.getInstanceType(TE);
    
    // TODO: figure out right value for isKnownPrivate
    if (instanceType->mayHaveMembers()) {
      auto ctors = CS.TC.lookupConstructors(
                                            CS.DC, instanceType, NameLookupFlags::IgnoreAccessControl);
      for (auto ctor : ctors) {
        if (!ctor.getValueDecl()->hasInterfaceType())
          CS.getTypeChecker().validateDeclForNameLookup(ctor.getValueDecl());
        if (ctor.getValueDecl()->hasInterfaceType())
          candidates.push_back({ ctor.getValueDecl(), 1 });
      }
    }
    
    declName = instanceType->getString();
    return;
  }
  
  if (auto *DSBI = dyn_cast<DotSyntaxBaseIgnoredExpr>(fn)) {
    collectCalleeCandidates(DSBI->getRHS(), /*implicitDotSyntax=*/false);
    return;
  }
  
  if (auto AE = dyn_cast<ApplyExpr>(fn)) {
    collectCalleeCandidates(AE->getFn(),
                            /*implicitDotSyntax=*/AE->getMemberOperatorRef());
    
    // If we found a candidate list with a recursive walk, try adjust the curry
    // level for the applied subexpression in this call.
    if (!candidates.empty()) {
      // If this is a DotSyntaxCallExpr, then the callee is a method, and the
      // argument list of this apply is the base being applied to the method.
      // If we have a type for that, capture it so that we can calculate a
      // substituted type, which resolves many generic arguments.
      Type baseType;
      if (isa<SelfApplyExpr>(AE) &&
          !isUnresolvedOrTypeVarType(CS.getType(AE->getArg())))
        baseType = CS.getType(AE->getArg())->getWithoutSpecifierType();

      for (auto &C : candidates) {
        C.level += 1;

        baseType = replaceTypeVariablesWithUnresolved(baseType);

        // Compute a new substituted type if we have a base type to apply.
        if (baseType && C.level == 1 && C.getDecl()) {
          baseType = baseType
            ->getWithoutSpecifierType()
            ->getMetatypeInstanceType();

          if (baseType->isAnyObject())
            baseType = Type();

          if (baseType && !baseType->hasUnresolvedType()) {
            C.entityType = baseType->getTypeOfMember(CS.DC->getParentModule(),
                                                     C.getDecl(), nullptr);
            C.substituted = true;
          }
        }
      }
      
      return;
    }
  }
  
  if (auto *OVE = dyn_cast<OpenExistentialExpr>(fn)) {
    collectCalleeCandidates(OVE->getSubExpr(), /*implicitDotSyntax=*/false);
    return;
  }
  
  if (auto *CFCE = dyn_cast<CovariantFunctionConversionExpr>(fn)) {
    collectCalleeCandidates(CFCE->getSubExpr(), /*implicitDotSyntax=*/false);
    return;
  }
  
  
  // Otherwise, we couldn't tell structurally what is going on here, so try to
  // dig something out of the constraint system.
  unsigned uncurryLevel = 0;
  
  // The candidate list of an unresolved_dot_expr is the candidate list of the
  // base uncurried by one level, and we refer to the name of the member, not to
  // the name of any base.
  if (auto UDE = dyn_cast<UnresolvedDotExpr>(fn)) {
    declName = UDE->getName().getBaseName().userFacingName();
    uncurryLevel = 1;
    
    // If we actually resolved the member to use, return it.
    auto loc = CS.getConstraintLocator(UDE, ConstraintLocator::Member);
    if (auto *member = CS.findResolvedMemberRef(loc)) {
      candidates.push_back({ member, uncurryLevel });
      return;
    }
    
    // If we resolved the constructor member, return it.
    auto ctorLoc =
    CS.getConstraintLocator(UDE, ConstraintLocator::ConstructorMember);
    if (auto *member = CS.findResolvedMemberRef(ctorLoc)) {
      candidates.push_back({ member, uncurryLevel });
      return;
    }
    
    // If we have useful information about the type we're
    // initializing, provide it.
    if (UDE->getName().getBaseName() == DeclBaseName::createConstructor()) {
      auto selfTy = CS.getType(UDE->getBase())->getWithoutSpecifierType();
      if (!selfTy->hasTypeVariable())
        declName = selfTy->eraseDynamicSelfType().getString() + "." + declName;
    }
    
    // Otherwise, look for a disjunction constraint explaining what the set is.
  }
  
  if (isa<MemberRefExpr>(fn))
    uncurryLevel = 1;
  
  // Scan to see if we have a disjunction constraint for this callee.
  for (auto &constraint : CS.getConstraints()) {
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
      declName = candidates[0].getDecl()->getBaseName().userFacingName();
    return;
  }
  
  // Otherwise, just add the expression as a candidate.
  candidates.push_back({fn, CS.getType(fn)});
}

/// After the candidate list is formed, it can be filtered down to discard
/// obviously mismatching candidates and compute a "closeness" for the
/// resultant set.
void CalleeCandidateInfo::filterListArgs(ArrayRef<AnyFunctionType::Param> actualArgs) {
  // Now that we have the candidate list, figure out what the best matches from
  // the candidate list are, and remove all the ones that aren't at that level.
  filterList([&](UncurriedCandidate candidate) -> ClosenessResultTy {
    // If this isn't a function or isn't valid at this uncurry level, treat it
    // as a general mismatch.
    if (!candidate.hasParameters())
      return {CC_GeneralMismatch, {}};
    return evaluateCloseness(candidate, actualArgs);
  });
}

void CalleeCandidateInfo::filterContextualMemberList(Expr *argExpr) {
  auto URT = CS.getASTContext().TheUnresolvedType;
  
  // If the argument is not present then we expect members without arguments.
  if (!argExpr) {
    return filterList([&](UncurriedCandidate candidate) -> ClosenessResultTy {
      // If this candidate has no arguments, then we're a match.
      if (!candidate.hasParameters())
        return {CC_ExactMatch, {}};

      // Otherwise, if this is a function candidate with an argument, we
      // mismatch argument count.
      return { CC_ArgumentCountMismatch, {}};
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
    
    return filterListArgs(AnyFunctionType::Param(argType));
  }
  
  // If we have a tuple expression, form a tuple type.
  SmallVector<AnyFunctionType::Param, 4> ArgElts;
  for (unsigned i = 0, e = argTuple->getNumElements(); i != e; ++i) {
    // If the argument has an & specified, then we expect an lvalue.
    Type argType = URT;
    if (isa<InOutExpr>(argTuple->getElement(i)))
      argType = LValueType::get(argType);
    
    ArgElts.push_back(AnyFunctionType::Param(argType,
                                             argTuple->getElementName(i), {}));
  }
  
  return filterListArgs(ArgElts);
}

CalleeCandidateInfo::CalleeCandidateInfo(Type baseType,
                                         ArrayRef<OverloadChoice> overloads,
                                         bool hasTrailingClosure,
                                         ConstraintSystem &CS,
                                         bool selfAlreadyApplied)
: CS(CS), hasTrailingClosure(hasTrailingClosure) {
  
  // If we have a useful base type for the candidate set, we'll want to
  // substitute it into each member.  If not, ignore it.
  if (baseType && isUnresolvedOrTypeVarType(baseType))
    baseType = Type();
  
  for (auto cand : overloads) {
    if (!cand.isDecl()) continue;
    
    auto decl = cand.getDecl();
    
    // If this is a method or enum case member (not a var or subscript), then
    // the uncurry level is 1 if self has already been applied.
    unsigned uncurryLevel = 0;
    if (decl->getDeclContext()->isTypeContext() &&
        selfAlreadyApplied && !isa<SubscriptDecl>(decl))
      uncurryLevel = 1;
    
    candidates.push_back({ decl, uncurryLevel });
    
    // If we have a base type for this member, try to perform substitutions into
    // it to get a simpler and more concrete type.
    //
    if (baseType) {
      auto substType = replaceTypeVariablesWithUnresolved(baseType);
      if (substType)
        substType = substType
        ->getWithoutSpecifierType()
        ->getMetatypeInstanceType();
      
      // If this is a DeclViaUnwrappingOptional, then we're actually looking
      // through an optional to get the member, and baseType is an Optional or
      // Metatype<Optional>.
      if (cand.getKind() == OverloadChoiceKind::DeclViaUnwrappedOptional) {
        // Look through optional or IUO to get the underlying type the decl was
        // found in.
        substType = substType->getOptionalObjectType();
      } else if (cand.getKind() != OverloadChoiceKind::Decl) {
        // Otherwise, if it is a remapping we can't handle, don't try to compute
        // a substitution.
        substType = Type();
      }

      if (substType->isAnyObject())
        substType = Type();

      if (substType && selfAlreadyApplied && !substType->hasUnresolvedType())
        substType =
          substType->getTypeOfMember(CS.DC->getParentModule(), decl, nullptr);
      if (substType) {
        candidates.back().entityType = substType;
        candidates.back().substituted = true;
      }
    }
  }
  
  if (!candidates.empty())
    declName = candidates[0].getDecl()->getBaseName().userFacingName();
}

CalleeCandidateInfo &CalleeCandidateInfo::
operator=(const CalleeCandidateInfo &CCI) {
  if (this != &CCI) {
    // If the reference member (i.e., CS) is identical, just copy remaining
    // members; otherwise, reconstruct the object.
    if (&CS == &CCI.CS) {
      declName = CCI.declName;
      hasTrailingClosure = CCI.hasTrailingClosure;
      candidates = CCI.candidates;
      closeness = CCI.closeness;
      failedArgument = CCI.failedArgument;
    } else {
      this->~CalleeCandidateInfo();
      new (this) CalleeCandidateInfo(CCI);
    }
  }
  return *this;
}

/// Given a set of parameter lists from an overload group, and a list of
/// arguments, emit a diagnostic indicating any partially matching overloads.
void CalleeCandidateInfo::
suggestPotentialOverloads(SourceLoc loc, bool isResult) {
  std::string suggestionText = "";
  std::set<std::string> dupes;
  
  // FIXME2: For (T,T) & (Self, Self), emit this as two candidates, one using
  // the LHS and one using the RHS type for T's.
  for (auto cand : candidates) {
    auto type = isResult ? cand.getResultType()
                         : cand.getArgumentType(CS.getASTContext());
    if (type.isNull())
      continue;
    
    // If we've already seen this (e.g. decls overridden on the result type),
    // ignore this one.
    auto name = isResult ? type->getString() : getTypeListString(type);
    if (!dupes.insert(name).second)
      continue;
    
    if (!suggestionText.empty())
      suggestionText += ", ";
    suggestionText += name;
  }
  
  if (suggestionText.empty())
    return;
  
  if (dupes.size() == 1) {
    CS.TC.diagnose(loc, diag::suggest_expected_match, isResult, suggestionText);
  } else {
    CS.TC.diagnose(loc, diag::suggest_partial_overloads, isResult, declName,
                   suggestionText);
  }
}

/// If the candidate set has been narrowed to a single parameter or single
/// archetype that has argument type errors, diagnose that error and
/// return true.
bool CalleeCandidateInfo::diagnoseGenericParameterErrors(Expr *badArgExpr) {
  TypeChecker &TC = CS.TC;
  Type argType = CS.getType(badArgExpr);
  
  // FIXME: For protocol argument types, could add specific error
  // similar to could_not_use_member_on_existential.
  if (argType->hasTypeVariable() || argType->is<ProtocolType>() ||
      argType->is<ProtocolCompositionType>())
    return false;
  
  bool foundFailure = false;
  TypeSubstitutionMap archetypesMap;
  
  if (!findGenericSubstitutions(failedArgument.declContext,
                                failedArgument.parameterType,
                                argType, archetypesMap))
    return false;
  
  auto getGenericTypeDecl = [&](ArchetypeType *archetype) -> ValueDecl * {
    auto paramType = archetype->getInterfaceType();
    
    if (auto *GTPT = paramType->getAs<GenericTypeParamType>())
      return GTPT->getDecl();
    
    if (auto *DMT = paramType->getAs<DependentMemberType>())
      return DMT->getAssocType();
    
    return nullptr;
  };
  
  auto describeGenericType = [&](ValueDecl *genericParam,
                                 bool includeName = false) -> std::string {
    if (!genericParam)
      return "";
    
    Decl *parent = nullptr;
    if (auto *AT = dyn_cast<AssociatedTypeDecl>(genericParam)) {
      parent = AT->getProtocol();
    } else {
      auto *dc = genericParam->getDeclContext();
      parent = dc->getInnermostDeclarationDeclContext();
    }
    
    if (!parent)
      return "";
    
    llvm::SmallString<64> result;
    llvm::raw_svector_ostream OS(result);
    
    OS << Decl::getDescriptiveKindName(genericParam->getDescriptiveKind());
    
    if (includeName && genericParam->hasName())
      OS << " '" << genericParam->getBaseName() << "'";
    
    OS << " of ";
    OS << Decl::getDescriptiveKindName(parent->getDescriptiveKind());
    if (auto *decl = dyn_cast<ValueDecl>(parent)) {
      if (decl->hasName())
        OS << " '" << decl->getFullName() << "'";
    }
    
    return OS.str();
  };
  
  for (auto pair : archetypesMap) {
    auto paramArchetype = pair.first->castTo<ArchetypeType>();
    auto substitution = pair.second;
    
    // FIXME: Add specific error for not subclass, if the archetype has a superclass?
    
    for (auto proto : paramArchetype->getConformsTo()) {
      if (!CS.TC.conformsToProtocol(substitution, proto, CS.DC,
                                    ConformanceCheckFlags::InExpression)) {
        if (substitution->isEqual(argType)) {
          CS.TC.diagnose(badArgExpr->getLoc(),
                         diag::cannot_convert_argument_value_protocol,
                         substitution, proto->getDeclaredType());
        } else {
          CS.TC.diagnose(badArgExpr->getLoc(),
                         diag::cannot_convert_partial_argument_value_protocol,
                         argType, substitution, proto->getDeclaredType());
        }
        foundFailure = true;
        break;
      }
    }
    
    if (auto *argArchetype = substitution->getAs<ArchetypeType>()) {
      // Produce this diagnostic only if the names
      // of the generic parameters are the same.
      if (argArchetype->getName() != paramArchetype->getName())
        continue;
      
      auto *paramDecl = getGenericTypeDecl(paramArchetype);
      auto *argDecl = getGenericTypeDecl(argArchetype);
      
      if (!paramDecl || !argDecl)
        continue;
      
      TC.diagnose(badArgExpr->getLoc(),
                  diag::cannot_convert_argument_value_generic, argArchetype,
                  describeGenericType(argDecl), paramArchetype,
                  describeGenericType(paramDecl));
      
      TC.diagnose(paramDecl, diag::descriptive_generic_type_declared_here,
                  describeGenericType(paramDecl, true));
      
      TC.diagnose(argDecl, diag::descriptive_generic_type_declared_here,
                  describeGenericType(argDecl, true));
      
      foundFailure = true;
      break;
    }
  }
  
  return foundFailure;
}

/// Emit a diagnostic and return true if this is an error condition we can
/// handle uniformly.  This should be called after filtering the candidate
/// list.
bool CalleeCandidateInfo::diagnoseSimpleErrors(const Expr *E) {
  SourceLoc loc = E->getLoc();
  
  // Handle symbols marked as explicitly unavailable.
  if (closeness == CC_Unavailable) {
    auto decl = candidates[0].getDecl();
    assert(decl && "Only decl-based candidates may be marked unavailable");
    return diagnoseExplicitUnavailability(decl, loc, CS.DC,
                                          dyn_cast<CallExpr>(E));
  }
  
  // Handle symbols that are matches, but are not accessible from the current
  // scope.
  if (closeness == CC_Inaccessible) {
    auto decl = candidates[0].getDecl();
    assert(decl && "Only decl-based candidates may be marked inaccessible");
    if (auto *CD = dyn_cast<ConstructorDecl>(decl)) {
      CS.TC.diagnose(loc, diag::init_candidate_inaccessible,
                     CD->getResultInterfaceType(), decl->getFormalAccess());
      
    } else {
      CS.TC.diagnose(loc, diag::candidate_inaccessible, decl->getBaseName(),
                     decl->getFormalAccess());
    }
    for (auto cand : candidates) {
      if (auto decl = cand.getDecl()) {
        CS.TC.diagnose(decl, diag::decl_declared_here, decl->getFullName());
      }
    }
    
    return true;
  }
  
  return false;
}

