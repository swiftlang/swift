//===--- CSSimplify.cpp - Constraint Simplification -----------------------===//
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
// This file implements simplifications of constraints within the constraint
// system.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "swift/Basic/StringExtras.h"
#include "swift/ClangImporter/ClangModule.h"

using namespace swift;
using namespace constraints;

MatchCallArgumentListener::~MatchCallArgumentListener() { }

void MatchCallArgumentListener::extraArgument(unsigned argIdx) { }

void MatchCallArgumentListener::missingArgument(unsigned paramIdx) { }

void MatchCallArgumentListener::outOfOrderArgument(unsigned argIdx,
                                                   unsigned prevArgIdx) {
}

bool MatchCallArgumentListener::relabelArguments(ArrayRef<Identifier> newNames){
  return true;
}

/// Produce a score (smaller is better) comparing a parameter name and
/// potentially-typod argument name.
///
/// \param paramName The name of the parameter.
/// \param argName The name of the argument.
/// \param maxScore The maximum score permitted by this comparison, or
/// 0 if there is no limit.
///
/// \returns the score, if it is good enough to even consider this a match.
/// Otherwise, an empty optional.
///
static Optional<unsigned> scoreParamAndArgNameTypo(StringRef paramName,
                                                   StringRef argName,
                                                   unsigned maxScore) {
  using namespace camel_case;

  // If one of the names starts with "with", drop it and compare the rest.
  bool argStartsWith = startsWithIgnoreFirstCase(argName, "with");
  bool paramStartsWith = startsWithIgnoreFirstCase(paramName, "with");
  if (argStartsWith != paramStartsWith) {
    if (argStartsWith)
      argName = argName.substr(4);
    else
      paramName = paramName.substr(4);
  }

  // Compute the edit distance.
  unsigned dist = argName.edit_distance(paramName, /*AllowReplacements=*/true,
                                        /*MaxEditDistance=*/maxScore);

  // If the edit distance would be too long, we're done.
  if (maxScore != 0 && dist > maxScore)
    return None;

  // The distance can be zero due to the "with" transformation above.
  if (dist == 0)
    return 1;

  // Only allow about one typo for every two properly-typed characters, which
  // prevents completely-wacky suggestions in many cases.
  if (dist > (argName.size() + 1) / 3)
    return None;

  return dist;
}

SmallVector<CallArgParam, 4> constraints::decomposeArgParamType(Type type) {
  SmallVector<CallArgParam, 4> result;
  switch (type->getKind()) {
  case TypeKind::Tuple:
    for (const auto &elt : cast<TupleType>(type.getPointer())->getElements()) {
      CallArgParam argParam;
      argParam.Ty = elt.isVararg() ? elt.getVarargBaseTy() : elt.getType();
      argParam.Label = elt.getName();
      argParam.HasDefaultArgument
        = elt.getDefaultArgKind() != DefaultArgumentKind::None;
      argParam.Variadic = elt.isVararg();
      result.push_back(argParam);
    }
    break;

  case TypeKind::Paren: {
    CallArgParam argParam;
    argParam.Ty = cast<ParenType>(type.getPointer())->getUnderlyingType();
    result.push_back(argParam);
    break;
  }

  default: {
    CallArgParam argParam;
    argParam.Ty = type;
    result.push_back(argParam);
    break;
  }
  }

  return result;
}

bool constraints::
matchCallArguments(ArrayRef<CallArgParam> args,
                   ArrayRef<CallArgParam> params,
                   bool hasTrailingClosure,
                   bool allowFixes,
                   MatchCallArgumentListener &listener,
                   SmallVectorImpl<ParamBinding> &parameterBindings) {
  // Keep track of the parameter we're matching and what argument indices
  // got bound to each parameter.
  unsigned paramIdx, numParams = params.size();
  parameterBindings.clear();
  parameterBindings.resize(numParams);

  // Keep track of which arguments we have claimed from the argument tuple.
  unsigned nextArgIdx = 0, numArgs = args.size();
  SmallVector<bool, 4> claimedArgs(numArgs, false);
  SmallVector<Identifier, 4> actualArgNames;
  unsigned numClaimedArgs = 0;

  // Indicates whether any of the arguments are potentially out-of-order,
  // requiring further checking at the end.
  bool potentiallyOutOfOrder = false;

  // Local function that claims the argument at \c argNumber, returning the
  // index of the claimed argument. This is primarily a helper for
  // \c claimNextNamed.
  auto claim = [&](Identifier expectedName, unsigned argNumber,
                   bool ignoreNameClash = false)  -> unsigned {
    // Make sure we can claim this argument.
    assert(argNumber != numArgs && "Must have a valid index to claim");
    assert(!claimedArgs[argNumber] && "Argument already claimed");

    if (!actualArgNames.empty()) {
      // We're recording argument names; record this one.
      actualArgNames[argNumber] = expectedName;
    } else if (args[argNumber].Label != expectedName && !ignoreNameClash) {
      // We have an argument name mismatch. Start recording argument names.
      actualArgNames.resize(numArgs);

      // Figure out previous argument names from the parameter bindings.
      for (unsigned i = 0; i != numParams; ++i) {
        const auto &param = params[i];
        bool firstArg = true;

        for (auto argIdx : parameterBindings[i]) {
          actualArgNames[argIdx] = firstArg ? param.Label : Identifier();
          firstArg = false;
        }
      }

      // Record this argument name.
      actualArgNames[argNumber] = expectedName;
    }

    claimedArgs[argNumber] = true;
    ++numClaimedArgs;
    return argNumber;
  };

  // Local function that skips over any claimed arguments.
  auto skipClaimedArgs = [&]() {
    while (nextArgIdx != numArgs && claimedArgs[nextArgIdx])
      ++nextArgIdx;
  };

  // Local function that retrieves the next unclaimed argument with the given
  // name (which may be empty). This routine claims the argument.
  auto claimNextNamed
    = [&](Identifier name, bool ignoreNameMismatch) -> Optional<unsigned> {
    // Skip over any claimed arguments.
    skipClaimedArgs();

    // If we've claimed all of the arguments, there's nothing more to do.
    if (numClaimedArgs == numArgs)
      return None;

    // When the expected name is empty, we claim the next argument if it has
    // no name.
    if (name.empty()) {
      // Nothing to claim.
      if (nextArgIdx == numArgs ||
          claimedArgs[nextArgIdx] ||
          (args[nextArgIdx].hasLabel() && !ignoreNameMismatch))
        return None;

      return claim(name, nextArgIdx);
    }

    // If the name matches, claim this argument.
    if (nextArgIdx != numArgs &&
        (ignoreNameMismatch || args[nextArgIdx].Label == name)) {
      return claim(name, nextArgIdx);
    }

    // The name didn't match. Go hunting for an unclaimed argument whose name
    // does match.
    Optional<unsigned> claimedWithSameName;
    for (unsigned i = nextArgIdx; i != numArgs; ++i) {
      // Skip arguments where the name doesn't match.
      if (args[i].Label != name)
        continue;

      // Skip claimed arguments.
      if (claimedArgs[i]) {
        // Note that we have already claimed an argument with the same name.
        if (!claimedWithSameName)
          claimedWithSameName = i;
        continue;
      }

      // We found a match.  If the match wasn't the next one, we have
      // potentially out of order arguments.
      if (i != nextArgIdx)
        potentiallyOutOfOrder = true;

      // Claim it.
      return claim(name, i);
    }

    // If we're not supposed to attempt any fixes, we're done.
    if (!allowFixes)
      return None;

    // Several things could have gone wrong here, and we'll check for each
    // of them at some point:
    //   - The keyword argument might be redundant, in which case we can point
    //     out the issue.
    //   - The argument might be unnamed, in which case we try to fix the
    //     problem by adding the name.
    //   - The keyword argument might be a typo for an actual argument name, in
    //     which case we should find the closest match to correct to.

    // Redundant keyword arguments.
    if (claimedWithSameName) {
      // FIXME: We can provide better diagnostics here.
      return None;
    }

    // Missing a keyword argument name.
    if (nextArgIdx != numArgs && !args[nextArgIdx].hasLabel()) {
      // Claim the next argument.
      return claim(name, nextArgIdx);
    }

    // Typo correction is handled in a later pass.
    return None;
  };

  // Local function that attempts to bind the given parameter to arguments in
  // the list.
  bool haveUnfulfilledParams = false;
  auto bindNextParameter = [&](bool ignoreNameMismatch) {
    const auto &param = params[paramIdx];

    // Handle variadic parameters.
    if (param.Variadic) {
      // Claim the next argument with the name of this parameter.
      auto claimed = claimNextNamed(param.Label, ignoreNameMismatch);

      // If there was no such argument, leave the argument unf
      if (!claimed) {
        haveUnfulfilledParams = true;
        return;
      }

      // Record the first argument for the variadic.
      parameterBindings[paramIdx].push_back(*claimed);

      // Claim any additional unnamed arguments.
      while ((claimed = claimNextNamed(Identifier(), false))) {
        parameterBindings[paramIdx].push_back(*claimed);
      }

      skipClaimedArgs();
      return;
    }

    // Try to claim an argument for this parameter.
    if (auto claimed = claimNextNamed(param.Label, ignoreNameMismatch)) {
      parameterBindings[paramIdx].push_back(*claimed);
      skipClaimedArgs();
      return;
    }

    // There was no argument to claim. Leave the argument unfulfilled.
    haveUnfulfilledParams = true;
  };

  // If we have a trailing closure, it maps to the last parameter.
  if (hasTrailingClosure && numParams > 0) {
    claimedArgs[numArgs-1] = true;
    ++numClaimedArgs;
    parameterBindings[numParams-1].push_back(numArgs-1);
  }

  // Mark through the parameters, binding them to their arguments.
  for (paramIdx = 0; paramIdx != numParams; ++paramIdx) {
    if (parameterBindings[paramIdx].empty())
      bindNextParameter(false);
  }

  // If we have any unclaimed arguments, complain about those.
  if (numClaimedArgs != numArgs) {

    // Find all of the named, unclaimed arguments.
    llvm::SmallVector<unsigned, 4> unclaimedNamedArgs;
    for (nextArgIdx = 0; skipClaimedArgs(), nextArgIdx != numArgs;
         ++nextArgIdx) {
      if (args[nextArgIdx].hasLabel())
        unclaimedNamedArgs.push_back(nextArgIdx);
    }

    if (!unclaimedNamedArgs.empty()) {
      // Find all of the named, unfulfilled parameters.
      llvm::SmallVector<unsigned, 4> unfulfilledNamedParams;
      bool hasUnfulfilledUnnamedParams = false;
      for (paramIdx = 0; paramIdx != numParams; ++paramIdx ) {
        if (parameterBindings[paramIdx].empty()) {
          if (params[paramIdx].hasLabel())
            unfulfilledNamedParams.push_back(paramIdx);
          else
            hasUnfulfilledUnnamedParams = true;
        }
      }

      if (!unfulfilledNamedParams.empty()) {
        // Use typo correction to find the best matches.
        // FIXME: There is undoubtedly a good dynamic-programming algorithm
        // to find the best assignment here.
        for (auto argIdx : unclaimedNamedArgs) {
          auto argName = args[argIdx].Label;

          // Find the closest matching unfulfilled named parameter.
          unsigned bestScore = 0;
          unsigned best = 0;
          for (unsigned i = 0, n = unfulfilledNamedParams.size(); i != n; ++i) {
            unsigned param = unfulfilledNamedParams[i];
            auto paramName = params[param].Label;

            if (auto score = scoreParamAndArgNameTypo(paramName.str(),
                                                      argName.str(),
                                                      bestScore)) {
              if (*score < bestScore || bestScore == 0) {
                bestScore = *score;
                best = i;
              }
            }
          }

          // If we found a parameter to fulfill, do it.
          if (bestScore > 0) {
            // Bind this parameter to the argument.
            nextArgIdx = argIdx;
            paramIdx = unfulfilledNamedParams[best];
            bindNextParameter(true);

            // Erase this parameter from the list of unfulfilled named
            // parameters, so we don't try to fulfill it again.
            unfulfilledNamedParams.erase(unfulfilledNamedParams.begin() + best);
            if (unfulfilledNamedParams.empty())
              break;
          }
        }

        // Update haveUnfulfilledParams, because we may have fulfilled some
        // parameters above.
        haveUnfulfilledParams = hasUnfulfilledUnnamedParams ||
                                !unfulfilledNamedParams.empty();
      }
    }

    // Find all of the unfulfilled parameters, and match them up
    // semi-positionally.
    if (numClaimedArgs != numArgs) {
      // Restart at the first argument/parameter.
      nextArgIdx = 0;
      skipClaimedArgs();
      haveUnfulfilledParams = false;
      for (paramIdx = 0; paramIdx != numParams; ++paramIdx) {
        // Skip fulfilled parameters.
        if (!parameterBindings[paramIdx].empty())
          continue;

        bindNextParameter(true);
      }
    }

    // If we still haven't claimed all of the arguments, fail.
    if (numClaimedArgs != numArgs) {
      nextArgIdx = 0;
      skipClaimedArgs();
      listener.extraArgument(nextArgIdx);
      return true;
    }

    // FIXME: If we had the actual parameters and knew the body names, those
    // matches would be best.
    potentiallyOutOfOrder = true;
  }

  // If we have any unfulfilled parameters, check them now.
  if (haveUnfulfilledParams) {
    for (paramIdx = 0; paramIdx != numParams; ++paramIdx) {
      // If we have a binding for this parameter, we're done.
      if (!parameterBindings[paramIdx].empty())
        continue;

      const auto &param = params[paramIdx];

      // Variadic parameters can be unfulfilled.
      if (param.Variadic)
        continue;

      // Parameters with defaults can be unfilfilled.
      if (param.HasDefaultArgument)
        continue;

      listener.missingArgument(paramIdx);
      return true;
    }
  }

  // If any arguments were provided out-of-order, check whether we have
  // violated any of the reordering rules.
  if (potentiallyOutOfOrder) {
    // Build a mapping from arguments to parameters.
    SmallVector<unsigned, 4> argumentBindings(numArgs);
    for(paramIdx = 0; paramIdx != numParams; ++paramIdx) {
      for (auto argIdx : parameterBindings[paramIdx])
        argumentBindings[argIdx] = paramIdx;
    }

    // Walk through the arguments, determining if any were bound to parameters
    // out-of-order where it is not permitted.
    unsigned prevParamIdx = argumentBindings[0];
    for (unsigned argIdx = 1; argIdx != numArgs; ++argIdx) {
      unsigned paramIdx = argumentBindings[argIdx];

      // If this argument binds to the same parameter as the previous one or to
      // a later parameter, just update the parameter index.
      if (paramIdx >= prevParamIdx) {
        prevParamIdx = paramIdx;
        continue;
      }

      // The argument binds to a parameter that comes earlier than the
      // previous argument. This is fine so long as this parameter and all of
      // those parameters up to (and including) the previously-bound parameter
      // are either variadic or have a default argument.
      for (unsigned i = paramIdx; i != prevParamIdx + 1; ++i) {
        const auto &param = params[i];
        if (param.Variadic || param.HasDefaultArgument)
          continue;

        unsigned prevArgIdx = parameterBindings[prevParamIdx].front();
        listener.outOfOrderArgument(argIdx, prevArgIdx);
        return true;
      }
    }
  }

  // If no arguments were renamed, the call arguments match up with the
  // parameters.
  if (actualArgNames.empty())
    return false;

  // The arguments were relabeled; notify the listener.
  return listener.relabelArguments(actualArgNames);
}

// Match the argument of a call to the parameter.
static ConstraintSystem::SolutionKind
matchCallArguments(ConstraintSystem &cs, TypeMatchKind kind,
                   Type argType, Type paramType,
                   ConstraintLocatorBuilder locator) {
  /// Listener used to produce failures if they should be produced.
  class Listener : public MatchCallArgumentListener {
    ConstraintSystem &CS;
    Type ArgType;
    Type ParamType;
    ConstraintLocatorBuilder &Locator;

  public:
    Listener(ConstraintSystem &cs, Type argType, Type paramType,
             ConstraintLocatorBuilder &locator)
      : CS(cs), ArgType(argType), ParamType(paramType), Locator(locator) { }

    virtual void extraArgument(unsigned argIdx) {
      if (!CS.shouldRecordFailures())
        return;

      CS.recordFailure(CS.getConstraintLocator(Locator),
                       Failure::ExtraArgument,
                       argIdx);
    }

    virtual void missingArgument(unsigned paramIdx) {
      if (!CS.shouldRecordFailures())
        return;

      CS.recordFailure(CS.getConstraintLocator(Locator),
                       Failure::MissingArgument,
                       ParamType, paramIdx);
    }

    virtual void outOfOrderArgument(unsigned argIdx, unsigned prevArgIdx) {
      if (!CS.shouldRecordFailures())
        return;

      CS.recordFailure(CS.getConstraintLocator(Locator),
                       Failure::OutOfOrderArgument,
                       argIdx, prevArgIdx);
    }

    virtual bool relabelArguments(ArrayRef<Identifier> newNames) {
      if (!CS.shouldAttemptFixes()) {
        // FIXME: record why this failed. We have renaming.
        return true;
      }

      // Add a fix for the name. This is only responsible for recording the fix.
      auto result = CS.simplifyFixConstraint(
                      Fix::getRelabelTuple(CS, FixKind::RelabelCallTuple,
                                           newNames),
                      ArgType, ParamType,
                      TypeMatchKind::ArgumentTupleConversion,
                      ConstraintSystem::TMF_None,
                      Locator);
      return result != ConstraintSystem::SolutionKind::Solved;
    }
  };

  // Extract the arguments.
  auto args = decomposeArgParamType(argType);

  // Extract the parameters.
  auto params = decomposeArgParamType(paramType);

  // Match up the call arguments to the parameters.
  Listener listener(cs, argType, paramType, locator);
  SmallVector<ParamBinding, 4> parameterBindings;
  if (constraints::matchCallArguments(args, params,
                                      hasTrailingClosure(locator),
                                      cs.shouldAttemptFixes(), listener,
                                      parameterBindings))
    return ConstraintSystem::SolutionKind::Error;

  // Check the argument types for each of the parameters.
  unsigned subflags = ConstraintSystem::TMF_GenerateConstraints;
  TypeMatchKind subKind;
  switch (kind) {
  case TypeMatchKind::ArgumentTupleConversion:
    subKind = TypeMatchKind::ArgumentConversion;
    break;

  case TypeMatchKind::OperatorArgumentTupleConversion:
    subKind = TypeMatchKind::OperatorArgumentConversion;
    break;

  case TypeMatchKind::Conversion:
  case TypeMatchKind::ExplicitConversion:
  case TypeMatchKind::OperatorArgumentConversion:
  case TypeMatchKind::ArgumentConversion:
  case TypeMatchKind::BindType:
  case TypeMatchKind::BindToPointerType:
  case TypeMatchKind::SameType:
  case TypeMatchKind::Subtype:
    llvm_unreachable("Not an call argument constraint");
  }
  
  auto haveOneNonUserConversion =
          (subKind != TypeMatchKind::OperatorArgumentConversion);
  
  auto haveNilArgument = false;
  auto nilLiteralProto = cs.TC.getProtocol(SourceLoc(),
                                           KnownProtocolKind::
                                              NilLiteralConvertible);
  
  auto isNilLiteral = [&](Type t) -> bool {
    if (auto tyvar = t->getAs<TypeVariableType>()) {
      return tyvar->getImpl().literalConformanceProto == nilLiteralProto;
    }
    return false;
  };

  // If we're applying an operator function to a nil literal operand, we
  // disallow value-to-optional conversions from taking place so as not to
  // select an overly permissive overload.
  auto allowOptionalConversion = [&](Type t) -> bool {
    
    if (t->isLValueType())
      t = t->getLValueOrInOutObjectType();
    
    if (!t->getAnyOptionalObjectType().isNull())
      return true;
    
    if (isNilLiteral(t))
      return true;
    
    if (auto nt = t->getNominalOrBoundGenericNominal()) {
      return nt->getName() == cs.TC.Context.Id_OptionalNilComparisonType;
    }
    
    return false;
  };
  
  // Pre-scan operator arguments for nil literals.
  if (subKind == TypeMatchKind::OperatorArgumentConversion) {
    for (auto arg : args) {
      if (isNilLiteral(arg.Ty)) {
        haveNilArgument = true;
        break;
      }
    }
  }
  
  for (unsigned paramIdx = 0, numParams = parameterBindings.size();
       paramIdx != numParams; ++paramIdx){
    // Skip unfulfilled parameters. There's nothing to do for them.
    if (parameterBindings[paramIdx].empty())
      continue;

    // Determine the parameter type.
    const auto &param = params[paramIdx];
    auto paramTy = param.Ty;

    // Compare each of the bound arguments for this parameter.
    for (auto argIdx : parameterBindings[paramIdx]) {
      auto loc = locator.withPathElement(LocatorPathElt::
                                            getApplyArgToParam(argIdx,
                                                               paramIdx));
      auto argTy = args[argIdx].Ty;

      if (haveNilArgument && !allowOptionalConversion(argTy)) {
        subflags |= ConstraintSystem::TMF_ApplyingOperatorWithNil;
      }
      
      if (!haveOneNonUserConversion) {
        subflags |= ConstraintSystem::TMF_ApplyingOperatorParameter;
      }
      
      switch (cs.matchTypes(argTy,paramTy,
                            subKind, subflags,
                            loc)) {
      case ConstraintSystem::SolutionKind::Error:
        return ConstraintSystem::SolutionKind::Error;

      case ConstraintSystem::SolutionKind::Solved:
      case ConstraintSystem::SolutionKind::Unsolved:
        break;
      }
    }
  }

  return ConstraintSystem::SolutionKind::Solved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchTupleTypes(TupleType *tuple1, TupleType *tuple2,
                                  TypeMatchKind kind, unsigned flags,
                                  ConstraintLocatorBuilder locator) {
  unsigned subFlags = flags | TMF_GenerateConstraints;

  // Equality and subtyping have fairly strict requirements on tuple matching,
  // requiring element names to either match up or be disjoint.
  if (kind < TypeMatchKind::Conversion) {
    if (tuple1->getNumElements() != tuple2->getNumElements()) {
      // Record this failure.
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::TupleSizeMismatch, tuple1, tuple2);
      }

      return SolutionKind::Error;
    }

    for (unsigned i = 0, n = tuple1->getNumElements(); i != n; ++i) {
      const auto &elt1 = tuple1->getElement(i);
      const auto &elt2 = tuple2->getElement(i);

      // If the names don't match, we may have a conflict.
      if (elt1.getName() != elt2.getName()) {
        // Same-type requirements require exact name matches.
        if (kind == TypeMatchKind::SameType) {
          // Record this failure.
          if (shouldRecordFailures()) {
            recordFailure(getConstraintLocator(
                            locator.withPathElement(
                              LocatorPathElt::getNamedTupleElement(i))),
                          Failure::TupleNameMismatch, tuple1, tuple2);
          }

          return SolutionKind::Error;
        }

        // For subtyping constraints, just make sure that this name isn't
        // used at some other position.
        if (elt2.hasName()) {
          int matched = tuple1->getNamedElementId(elt2.getName());
          if (matched != -1) {
            // Record this failure.
            if (shouldRecordFailures()) {
              recordFailure(getConstraintLocator(
                              locator.withPathElement(
                                LocatorPathElt::getNamedTupleElement(i))),
                            Failure::TupleNamePositionMismatch, tuple1, tuple2);
            }

            return SolutionKind::Error;
          }
        }
      }

      // Variadic bit must match.
      if (elt1.isVararg() != elt2.isVararg()) {
        // Record this failure.
        if (shouldRecordFailures()) {
          recordFailure(getConstraintLocator(
                          locator.withPathElement(
                            LocatorPathElt::getNamedTupleElement(i))),
                        Failure::TupleVariadicMismatch, tuple1, tuple2);
        }
        
        return SolutionKind::Error;
      }

      // Compare the element types.
      switch (matchTypes(elt1.getType(), elt2.getType(), kind, subFlags,
                         locator.withPathElement(
                           LocatorPathElt::getTupleElement(i)))) {
      case SolutionKind::Error:
        return SolutionKind::Error;

      case SolutionKind::Solved:
      case SolutionKind::Unsolved:
        break;
      }
    }
    return SolutionKind::Solved;
  }

  assert(kind >= TypeMatchKind::Conversion);
  TypeMatchKind subKind;
  switch (kind) {
  case TypeMatchKind::ArgumentTupleConversion:
    subKind = TypeMatchKind::ArgumentConversion;
    break;

  case TypeMatchKind::OperatorArgumentTupleConversion:
    subKind = TypeMatchKind::OperatorArgumentConversion;
    break;

  case TypeMatchKind::OperatorArgumentConversion:
  case TypeMatchKind::ArgumentConversion:
  case TypeMatchKind::ExplicitConversion:
  case TypeMatchKind::Conversion:
    subKind = TypeMatchKind::Conversion;
    break;

  case TypeMatchKind::BindType:
  case TypeMatchKind::BindToPointerType:
  case TypeMatchKind::SameType:
  case TypeMatchKind::Subtype:
    llvm_unreachable("Not a conversion");
  }

  // Compute the element shuffles for conversions.
  SmallVector<int, 16> sources;
  SmallVector<unsigned, 4> variadicArguments;
  if (computeTupleShuffle(tuple1, tuple2, sources, variadicArguments)) {
    // FIXME: Record why the tuple shuffle couldn't be computed.
    if (shouldRecordFailures()) {
      if (tuple1->getNumElements() != tuple2->getNumElements()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::TupleSizeMismatch, tuple1, tuple2);
      }
    }
    return SolutionKind::Error;
  }

  // Check each of the elements.
  bool hasVarArg = false;
  for (unsigned idx2 = 0, n = sources.size(); idx2 != n; ++idx2) {
    // Default-initialization always allowed for conversions.
    if (sources[idx2] == TupleShuffleExpr::DefaultInitialize) {
      continue;
    }

    // Variadic arguments handled below.
    if (sources[idx2] == TupleShuffleExpr::FirstVariadic) {
      hasVarArg = true;
      continue;
    }

    assert(sources[idx2] >= 0);
    unsigned idx1 = sources[idx2];

    // Match up the types.
    const auto &elt1 = tuple1->getElement(idx1);
    const auto &elt2 = tuple2->getElement(idx2);
    switch (matchTypes(elt1.getType(), elt2.getType(), subKind, subFlags,
                       locator.withPathElement(
                         LocatorPathElt::getTupleElement(idx1)))) {
    case SolutionKind::Error:
      return SolutionKind::Error;

    case SolutionKind::Solved:
    case SolutionKind::Unsolved:
      break;
    }

  }

  // If we have variadic arguments to check, do so now.
  if (hasVarArg) {
    const auto &elt2 = tuple2->getElements().back();
    auto eltType2 = elt2.getVarargBaseTy();

    for (unsigned idx1 : variadicArguments) {
      switch (matchTypes(tuple1->getElementType(idx1), eltType2, subKind,
                         subFlags,
                         locator.withPathElement(
                           LocatorPathElt::getTupleElement(idx1)))) {
      case SolutionKind::Error:
        return SolutionKind::Error;

      case SolutionKind::Solved:
      case SolutionKind::Unsolved:
        break;
      }
    }
  }

  return SolutionKind::Solved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchScalarToTupleTypes(Type type1, TupleType *tuple2,
                                          TypeMatchKind kind, unsigned flags,
                                          ConstraintLocatorBuilder locator) {
  int scalarFieldIdx = tuple2->getElementForScalarInit();
  assert(scalarFieldIdx >= 0 && "Invalid tuple for scalar-to-tuple");
  const auto &elt = tuple2->getElement(scalarFieldIdx);
  auto scalarFieldTy = elt.isVararg()? elt.getVarargBaseTy() : elt.getType();
  return matchTypes(type1, scalarFieldTy, kind, flags,
                    locator.withPathElement(ConstraintLocator::ScalarToTuple));
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchTupleToScalarTypes(TupleType *tuple1, Type type2,
                                          TypeMatchKind kind, unsigned flags,
                                          ConstraintLocatorBuilder locator) {
  assert(tuple1->getNumElements() == 1 && "Wrong number of elements");
  assert(!tuple1->getElement(0).isVararg() && "Should not be variadic");

  return matchTypes(tuple1->getElementType(0),
                    type2, kind, flags,
                    locator.withPathElement(
                      LocatorPathElt::getTupleElement(0)));
}

/// Determine whether this is a function type accepting no arguments as input.
static bool isFunctionTypeAcceptingNoArguments(Type type) {
  // The type must be a function type.
  auto fnType = type->getAs<AnyFunctionType>();
  if (!fnType)
    return false;

  // Only tuple arguments make sense.
  auto tupleTy = fnType->getInput()->getAs<TupleType>();
  if (!tupleTy) return false;

  // Look for any non-defaulted, non-variadic elements.
  for (const auto &elt : tupleTy->getElements()) {
    // Defaulted arguments are okay.
    if (elt.getDefaultArgKind() != DefaultArgumentKind::None)
      continue;

    // Variadic arguments are okay.
    if (elt.isVararg())
      continue;

    return false;
  }

  return true;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchFunctionTypes(FunctionType *func1, FunctionType *func2,
                                     TypeMatchKind kind, unsigned flags,
                                     ConstraintLocatorBuilder locator) {
  // An @autoclosure function type can be a subtype of a
  // non-@autoclosure function type.
  if (func1->isAutoClosure() != func2->isAutoClosure()) {
    if (func2->isAutoClosure() || kind < TypeMatchKind::Subtype) {
      // If the second type is an autoclosure and the first type appears to
      // accept no arguments, try to add the ().
      if (func2->isAutoClosure() && shouldAttemptFixes() &&
          kind >= TypeMatchKind::Conversion &&
          isFunctionTypeAcceptingNoArguments(func1)) {
        unsigned subFlags = (flags | TMF_GenerateConstraints) & ~TMF_ApplyingFix;
        return simplifyFixConstraint(FixKind::NullaryCall, func1, func2, kind,
                                     subFlags, locator);
      }

      // Record this failure.
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::FunctionAutoclosureMismatch, func1, func2);
      }

      return SolutionKind::Error;
    }
  }
  
  // A non-throwing function can be a subtype of a throwing function.
  if (func1->throws() != func2->throws()) {
    // Cannot drop 'throws'.
    if (func1->throws() ||
        (func2->throws() && kind < TypeMatchKind::Subtype)) {
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::FunctionThrowsMismatch, func1, func2);
      }

      return SolutionKind::Error;
    }
  }

  // A @noreturn function type can be a subtype of a non-@noreturn function
  // type.
  if (func1->isNoReturn() != func2->isNoReturn()) {
    if (func2->isNoReturn() || kind < TypeMatchKind::SameType) {
      // Record this failure.
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::FunctionNoReturnMismatch, func1, func2);
      }

      return SolutionKind::Error;
    }
  }

  // A non-@noescape function type can be a subtype of a @noescape function
  // type.
  if (func1->isNoEscape() != func2->isNoEscape()) {
    if (func1->isNoEscape() || kind < TypeMatchKind::Subtype) {
      // Record this failure.
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::FunctionNoEscapeMismatch, func1, func2);
      }

      return SolutionKind::Error;
    }
  }

  // Determine how we match up the input/result types.
  TypeMatchKind subKind;
  switch (kind) {
  case TypeMatchKind::BindType:
  case TypeMatchKind::BindToPointerType:
  case TypeMatchKind::SameType:
    subKind = kind;
    break;

  case TypeMatchKind::Subtype:
  case TypeMatchKind::Conversion:
  case TypeMatchKind::ExplicitConversion:
  case TypeMatchKind::ArgumentConversion:
  case TypeMatchKind::ArgumentTupleConversion:
  case TypeMatchKind::OperatorArgumentTupleConversion:
  case TypeMatchKind::OperatorArgumentConversion:
    subKind = TypeMatchKind::Subtype;
    break;
  }

  unsigned subFlags = flags | TMF_GenerateConstraints;

  // Input types can be contravariant (or equal).
  SolutionKind result = matchTypes(func2->getInput(), func1->getInput(),
                                   subKind, subFlags,
                                   locator.withPathElement(
                                     ConstraintLocator::FunctionArgument));
  if (result == SolutionKind::Error)
    return SolutionKind::Error;

  // Result type can be covariant (or equal).
  switch (matchTypes(func1->getResult(), func2->getResult(), subKind,
                     subFlags,
                     locator.withPathElement(
                       ConstraintLocator::FunctionResult))) {
  case SolutionKind::Error:
    return SolutionKind::Error;

  case SolutionKind::Solved:
    result = SolutionKind::Solved;
    break;

  case SolutionKind::Unsolved:
    result = SolutionKind::Unsolved;
    break;
  }

  return result;
}

/// \brief Map a failed type-matching kind to a failure kind, generically.
static Failure::FailureKind getRelationalFailureKind(TypeMatchKind kind) {
  switch (kind) {
  case TypeMatchKind::BindType:
  case TypeMatchKind::BindToPointerType:
  case TypeMatchKind::SameType:
    return Failure::TypesNotEqual;

  case TypeMatchKind::Subtype:
    return Failure::TypesNotSubtypes;

  case TypeMatchKind::Conversion:
  case TypeMatchKind::ExplicitConversion:
  case TypeMatchKind::ArgumentConversion:
  case TypeMatchKind::OperatorArgumentConversion:
  case TypeMatchKind::ArgumentTupleConversion:
  case TypeMatchKind::OperatorArgumentTupleConversion:
    return Failure::TypesNotConvertible;
  }

  llvm_unreachable("unhandled type matching kind");
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchSuperclassTypes(Type type1, Type type2,
                                       TypeMatchKind kind, unsigned flags,
                                       ConstraintLocatorBuilder locator) {
  auto classDecl2 = type2->getClassOrBoundGenericClass();
  bool done = false;
  for (auto super1 = TC.getSuperClassOf(type1);
       !done && super1;
       super1 = TC.getSuperClassOf(super1)) {
    if (super1->getClassOrBoundGenericClass() != classDecl2)
      continue;

    return matchTypes(super1, type2, TypeMatchKind::SameType,
                      TMF_GenerateConstraints, locator);
  }

  // Record this failure.
  // FIXME: Specialize diagnostic.
  if (shouldRecordFailures()) {
    recordFailure(getConstraintLocator(locator),
                  getRelationalFailureKind(kind), type1, type2);
  }

  return SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchDeepEqualityTypes(Type type1, Type type2,
                                         ConstraintLocatorBuilder locator) {
  // Handle nominal types that are not directly generic.
  if (auto nominal1 = type1->getAs<NominalType>()) {
    auto nominal2 = type2->castTo<NominalType>();

    assert((bool)nominal1->getParent() == (bool)nominal2->getParent() &&
           "Mismatched parents of nominal types");

    if (!nominal1->getParent())
      return SolutionKind::Solved;

    // Match up the parents, exactly.
    return matchTypes(nominal1->getParent(), nominal2->getParent(),
                      TypeMatchKind::SameType, TMF_GenerateConstraints,
                      locator.withPathElement(ConstraintLocator::ParentType));
  }

  auto bound1 = type1->castTo<BoundGenericType>();
  auto bound2 = type2->castTo<BoundGenericType>();

  // Match up the parents, exactly, if there are parents.
  assert((bool)bound1->getParent() == (bool)bound2->getParent() &&
         "Mismatched parents of bound generics");
  if (bound1->getParent()) {
    switch (matchTypes(bound1->getParent(), bound2->getParent(),
                       TypeMatchKind::SameType, TMF_GenerateConstraints,
                       locator.withPathElement(ConstraintLocator::ParentType))){
    case SolutionKind::Error:
      return SolutionKind::Error;

    case SolutionKind::Solved:
    case SolutionKind::Unsolved:
      break;
    }
  }

  // Match up the generic arguments, exactly.
  auto args1 = bound1->getGenericArgs();
  auto args2 = bound2->getGenericArgs();
  assert(args1.size() == args2.size() && "Mismatched generic args");
  for (unsigned i = 0, n = args1.size(); i != n; ++i) {
    switch (matchTypes(args1[i], args2[i], TypeMatchKind::SameType,
                       TMF_GenerateConstraints,
                       locator.withPathElement(
                         LocatorPathElt::getGenericArgument(i)))) {
    case SolutionKind::Error:
      return SolutionKind::Error;

    case SolutionKind::Solved:
    case SolutionKind::Unsolved:
      break;
    }
  }

  return SolutionKind::Solved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchExistentialTypes(Type type1, Type type2,
                                        TypeMatchKind kind, unsigned flags,
                                        ConstraintLocatorBuilder locator) {
  SmallVector<ProtocolDecl *, 4> protocols;
  type2->getAnyExistentialTypeProtocols(protocols);

  for (auto proto : protocols) {
    switch (simplifyConformsToConstraint(type1, proto, locator, flags, false)) {
      case SolutionKind::Solved:
        break;

      case SolutionKind::Unsolved:
        // Add the constraint.
        addConstraint(ConstraintKind::ConformsTo, type1,
                      proto->getDeclaredType(), getConstraintLocator(locator));
        break;

      case SolutionKind::Error:
        return SolutionKind::Error;
    }
  }

  return SolutionKind::Solved;
}

/// \brief Map a type-matching kind to a constraint kind.
static ConstraintKind getConstraintKind(TypeMatchKind kind) {
  switch (kind) {
  case TypeMatchKind::BindType:
  case TypeMatchKind::BindToPointerType:
    return ConstraintKind::Bind;

  case TypeMatchKind::SameType:
    return ConstraintKind::Equal;

  case TypeMatchKind::Subtype:
    return ConstraintKind::Subtype;

  case TypeMatchKind::Conversion:
    return ConstraintKind::Conversion;

  case TypeMatchKind::ExplicitConversion:
    return ConstraintKind::ExplicitConversion;
      
  case TypeMatchKind::ArgumentConversion:
    return ConstraintKind::ArgumentConversion;

  case TypeMatchKind::ArgumentTupleConversion:
    return ConstraintKind::ArgumentTupleConversion;

  case TypeMatchKind::OperatorArgumentTupleConversion:
    return ConstraintKind::OperatorArgumentTupleConversion;

  case TypeMatchKind::OperatorArgumentConversion:
    return ConstraintKind::OperatorArgumentConversion;
  }

  llvm_unreachable("unhandled type matching kind");
}

static bool isStringCompatiblePointerBaseType(TypeChecker &TC,
                                              DeclContext *DC,
                                              Type baseType) {
  // Allow strings to be passed to pointer-to-byte or pointer-to-void types.
  if (baseType->isEqual(TC.getInt8Type(DC)))
    return true;
  if (baseType->isEqual(TC.getUInt8Type(DC)))
    return true;
  if (baseType->isEqual(TC.Context.TheEmptyTupleType))
    return true;
  
  return false;
}

/// Determine whether the given type is a value type to which we can bridge a
/// value of its corresponding class type, such as 'String' bridging from
/// NSString.
static bool allowsBridgingFromObjC(TypeChecker &tc, DeclContext *dc,
                                   Type type) {
  auto objcType = tc.getBridgedToObjC(dc, true, type);
  if (!objcType)
    return false;

  auto objcClass = objcType->getClassOrBoundGenericClass();
  if (!objcClass)
    return false;

  return true;
}

/// Determine whether this type variable represents a "non-trivial"
/// generic parameter, meaning that the generic parameter has a
/// non-Objective-C protocol requirement.
///
/// FIXME: This matches a limitation of our generics system, where we
/// cannot pass the witness tables from
///
/// \returns the archetype for the generic parameter.
static Type representsNonTrivialGenericParameter(TypeVariableType *typeVar) {
  auto locator = typeVar->getImpl().getLocator();
  if (!locator || locator->getPath().empty() ||
      locator->getPath().back().getKind() != ConstraintLocator::Archetype)
    return nullptr;

  auto archetype = locator->getPath().back().getArchetype();
  for (auto proto : archetype->getConformsTo()) {
    // AnyObject is always trivially representable as a generic parameter;
    // there is no runtime witness.
    if (proto->isSpecificProtocol(KnownProtocolKind::AnyObject))
      continue;
    
    // ObjC protocols are trivially representable as a generic parameter;
    // they are witnessed by the ObjC runtime.
    if (proto->isObjC())
      continue;
    
    // Other protocols would need Swift runtime support to self-conform.
    return archetype;
  }

  return nullptr;
}

/// Check whether the given value type is one of a few specific
/// bridged types.
static bool isArrayDictionarySetOrString(const ASTContext &ctx, Type type) {
  if (auto structDecl = type->getStructOrBoundGenericStruct()) {
    return (structDecl == ctx.getStringDecl() ||
            structDecl == ctx.getArrayDecl() ||
            structDecl == ctx.getDictionaryDecl() ||
            structDecl == ctx.getSetDecl());
  }

  return false;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchTypes(Type type1, Type type2, TypeMatchKind kind,
                             unsigned flags,
                             ConstraintLocatorBuilder locator) {
  // If we have type variables that have been bound to fixed types, look through
  // to the fixed type.
  TypeVariableType *typeVar1;
  bool isArgumentTupleConversion
          = kind == TypeMatchKind::ArgumentTupleConversion ||
            kind == TypeMatchKind::OperatorArgumentTupleConversion;
  type1 = getFixedTypeRecursive(type1, typeVar1,
                                kind == TypeMatchKind::SameType,
                                isArgumentTupleConversion);
  auto desugar1 = type1->getDesugaredType();

  TypeVariableType *typeVar2;
  type2 = getFixedTypeRecursive(type2, typeVar2,
                                kind == TypeMatchKind::SameType,
                                isArgumentTupleConversion);
  auto desugar2 = type2->getDesugaredType();

  // If the types are obviously equivalent, we're done.
  if (desugar1->isEqual(desugar2))
    return SolutionKind::Solved;

  // If either (or both) types are type variables, unify the type variables.
  if (typeVar1 || typeVar2) {
    switch (kind) {
    case TypeMatchKind::BindType:
    case TypeMatchKind::BindToPointerType:
    case TypeMatchKind::SameType: {
      if (typeVar1 && typeVar2) {
        auto rep1 = getRepresentative(typeVar1);
        auto rep2 = getRepresentative(typeVar2);
        if (rep1 == rep2) {
          // We already merged these two types, so this constraint is
          // trivially solved.
          return SolutionKind::Solved;
        }

        // If exactly one of the type variables can bind to an lvalue, we
        // can't merge these two type variables.
        if (rep1->getImpl().canBindToLValue()
              != rep2->getImpl().canBindToLValue()) {
          if (flags & TMF_GenerateConstraints) {
            if (kind == TypeMatchKind::BindToPointerType) {
              increaseScore(ScoreKind::SK_ScalarPointerConversion);
            }

            // Add a new constraint between these types. We consider the current
            // type-matching problem to the "solved" by this addition, because
            // this new constraint will be solved at a later point.
            // Obviously, this must not happen at the top level, or the
            // algorithm would not terminate.
            addConstraint(getConstraintKind(kind), rep1, rep2,
                          getConstraintLocator(locator));
            return SolutionKind::Solved;
          }

          return SolutionKind::Unsolved;
        }

        // Merge the equivalence classes corresponding to these two variables.
        mergeEquivalenceClasses(rep1, rep2);
        return SolutionKind::Solved;
      }

      // Provide a fixed type for the type variable.
      bool wantRvalue = kind == TypeMatchKind::SameType;
      if (typeVar1) {
        // If we want an rvalue, get the rvalue.
        if (wantRvalue)
          type2 = type2->getRValueType();

        // If the left-hand type variable cannot bind to an lvalue,
        // but we still have an lvalue, fail.
        if (!typeVar1->getImpl().canBindToLValue()) {
          if (type2->isLValueType()) {
            if (shouldRecordFailures()) {
              recordFailure(getConstraintLocator(locator),
                            Failure::IsForbiddenLValue, type1, type2);
            }
            return SolutionKind::Error;
          }

          // Okay. Bind below.
        }

        // A generic parameter can only be bound to an existential
        // type if the existential type is compatible with
        // Objective-C.
        Type archetype;
        if (type2->isExistentialType() && 
            (archetype = representsNonTrivialGenericParameter(typeVar1))) {
          if (shouldRecordFailures()) {
            recordFailure(getConstraintLocator(locator),
                          Failure::ExistentialGenericParameter, 
                          archetype, type2);
          }

          return SolutionKind::Error;
        }

        // Check whether the type variable must be bound to a materializable
        // type.
        if (typeVar1->getImpl().mustBeMaterializable()) {
          if (!type2->isMaterializable()) {
            if (shouldRecordFailures()) {
              // TODO: customize error message for closure vs. generic param
              recordFailure(getConstraintLocator(locator),  
                            Failure::IsNotMaterializable, type2);
            }
            return SolutionKind::Error;
          } else {
            setMustBeMaterializableRecursive(type2);
          }
        }

        // A constraint that binds any pointer to a void pointer is
        // ineffective, since any pointer can be converted to a void pointer.
        if (kind == TypeMatchKind::BindToPointerType && desugar2->isVoid() &&
            (flags & TMF_GenerateConstraints)) {
          // Create a disjunction where the favored branch doesn't constrain
          // anything but the unfavored branch binds type1 to Void. type1 only
          // gets bound to Void as a last resort.
          Constraint *trivialConstraint = Constraint::create(*this,
            ConstraintKind::Bind, typeVar1, typeVar1, DeclName(),
            getConstraintLocator(locator));
          trivialConstraint->setFavored();
          Constraint *bindingConstraint = Constraint::create(*this,
            ConstraintKind::Bind, typeVar1, type2, DeclName(),
            getConstraintLocator(locator));
          Constraint *constraints[] = { trivialConstraint, bindingConstraint };
          addConstraint(
            Constraint::createDisjunction(*this, constraints,
                                          getConstraintLocator(locator)));
          return SolutionKind::Solved;
        }

        assignFixedType(typeVar1, type2);
        return SolutionKind::Solved;
      }

      // If we want an rvalue, get the rvalue.
      if (wantRvalue)
        type1 = type1->getRValueType();

      if (!typeVar2->getImpl().canBindToLValue()) {
        if (type1->isLValueType()) {
          if (shouldRecordFailures()) {
            recordFailure(getConstraintLocator(locator),
                          Failure::IsForbiddenLValue, type1, type2);
          }
          return SolutionKind::Error;
        }
        
        // Okay. Bind below.
      }

      assignFixedType(typeVar2, type1);
      return SolutionKind::Solved;
    }

    case TypeMatchKind::Subtype:
    case TypeMatchKind::Conversion:
    case TypeMatchKind::ExplicitConversion:
    case TypeMatchKind::ArgumentConversion:
    case TypeMatchKind::ArgumentTupleConversion:
    case TypeMatchKind::OperatorArgumentTupleConversion:
    case TypeMatchKind::OperatorArgumentConversion:
      if (flags & TMF_GenerateConstraints) {
        // Add a new constraint between these types. We consider the current
        // type-matching problem to the "solved" by this addition, because
        // this new constraint will be solved at a later point.
        // Obviously, this must not happen at the top level, or the algorithm
        // would not terminate.
        addConstraint(getConstraintKind(kind), type1, type2,
                      getConstraintLocator(locator));
        return SolutionKind::Solved;
      }

      // We couldn't solve this constraint. If only one of the types is a type
      // variable, perhaps we can do something with it below.
      if (typeVar1 && typeVar2)
        return typeVar1 == typeVar2 ? SolutionKind::Solved
                                    : SolutionKind::Unsolved;
        
      break;
    }
  }

  llvm::SmallVector<RestrictionOrFix, 4> conversionsOrFixes;
  bool concrete = !typeVar1 && !typeVar2;

  // If this is an argument conversion, handle it directly. The rules are
  // different from normal conversions.
  if (kind == TypeMatchKind::ArgumentTupleConversion ||
      kind == TypeMatchKind::OperatorArgumentTupleConversion) {
    if (concrete) {
      return ::matchCallArguments(*this, kind, type1, type2, locator);
    }

    if (flags & TMF_GenerateConstraints) {
      // Add a new constraint between these types. We consider the current
      // type-matching problem to the "solved" by this addition, because
      // this new constraint will be solved at a later point.
      // Obviously, this must not happen at the top level, or the algorithm
      // would not terminate.
      addConstraint(getConstraintKind(kind), type1, type2,
                    getConstraintLocator(locator));
      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;
  }

  // Decompose parallel structure.
  unsigned subFlags = (flags | TMF_GenerateConstraints) & ~TMF_ApplyingFix;
  if (desugar1->getKind() == desugar2->getKind()) {
    switch (desugar1->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
      llvm_unreachable("Type has not been desugared completely");

#define ARTIFICIAL_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
      llvm_unreachable("artificial type in constraint");

#define BUILTIN_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    case TypeKind::Module:
      if (desugar1 == desugar2) {
        return SolutionKind::Solved;
      }

      // Record this failure.
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      getRelationalFailureKind(kind), type1, type2);
      }

      return SolutionKind::Error;

    case TypeKind::Error:
      return SolutionKind::Error;

    case TypeKind::GenericTypeParam:
    case TypeKind::DependentMember:
      llvm_unreachable("unmapped dependent type in type checker");

    case TypeKind::TypeVariable:
    case TypeKind::Archetype:
      // Nothing to do here; handle type variables and archetypes below.
      break;

    case TypeKind::Tuple: {
      // Try the tuple-to-tuple conversion.
      conversionsOrFixes.push_back(ConversionRestrictionKind::TupleToTuple);
      break;
    }

    case TypeKind::Enum:
    case TypeKind::Struct:
    case TypeKind::Class: {
      auto nominal1 = cast<NominalType>(desugar1);
      auto nominal2 = cast<NominalType>(desugar2);
      if (nominal1->getDecl() == nominal2->getDecl()) {
        conversionsOrFixes.push_back(ConversionRestrictionKind::DeepEquality);
      }

      // Check for CF <-> ObjectiveC bridging.
      if (desugar1->getKind() == TypeKind::Class &&
          kind >= TypeMatchKind::Subtype) {
        auto class1 = cast<ClassDecl>(nominal1->getDecl());
        auto class2 = cast<ClassDecl>(nominal2->getDecl());

        // CF -> Objective-C via toll-free bridging.
        if (class1->isForeign() && !class2->isForeign() &&
            class1->getAttrs().hasAttribute<ObjCBridgedAttr>()) {
          conversionsOrFixes.push_back(
            ConversionRestrictionKind::CFTollFreeBridgeToObjC);
        }

        // Objective-C -> CF via toll-free bridging.
        if (class2->isForeign() && !class1->isForeign() &&
            class2->getAttrs().hasAttribute<ObjCBridgedAttr>()) {
          conversionsOrFixes.push_back(
            ConversionRestrictionKind::ObjCTollFreeBridgeToCF);
        }
      }

      break;
    }

    case TypeKind::DynamicSelf:
      // FIXME: Deep equality? What is the rule between two DynamicSelfs?
      break;
       
    case TypeKind::Protocol:
      // Nothing to do here; try existential and user-defined conversions below.
      break;

    case TypeKind::Metatype:
    case TypeKind::ExistentialMetatype: {
      auto meta1 = cast<AnyMetatypeType>(desugar1);
      auto meta2 = cast<AnyMetatypeType>(desugar2);

      TypeMatchKind subKind = TypeMatchKind::SameType;
      // A.Type < B.Type if A < B and both A and B are classes.
      if (isa<MetatypeType>(desugar1) &&
          kind != TypeMatchKind::SameType &&
          meta1->getInstanceType()->mayHaveSuperclass() &&
          meta2->getInstanceType()->getClassOrBoundGenericClass())
        subKind = std::min(kind, TypeMatchKind::Subtype);
      // P.Type < Q.Type if P < Q, both P and Q are protocols, and P.Type
      // and Q.Type are both existential metatypes.
      else if (isa<ExistentialMetatypeType>(meta1)
          && isa<ExistentialMetatypeType>(meta2))
        subKind = std::min(kind, TypeMatchKind::Subtype);
      
      return matchTypes(meta1->getInstanceType(), meta2->getInstanceType(),
                        subKind, subFlags,
                        locator.withPathElement(
                          ConstraintLocator::InstanceType));
    }

    case TypeKind::Function:
      return matchFunctionTypes(cast<FunctionType>(desugar1),
                                cast<FunctionType>(desugar2),
                                kind, flags, locator);

    case TypeKind::PolymorphicFunction:
    case TypeKind::GenericFunction:
      llvm_unreachable("Polymorphic function type should have been opened");

    case TypeKind::ProtocolComposition:
      // Existential types handled below.
      break;

    case TypeKind::LValue:
      return matchTypes(cast<LValueType>(desugar1)->getObjectType(),
                        cast<LValueType>(desugar2)->getObjectType(),
                        TypeMatchKind::SameType, subFlags,
                        locator.withPathElement(
                          ConstraintLocator::ArrayElementType));
    
    case TypeKind::InOut:
      // If the RHS is an inout type, the LHS must be an @lvalue type.
      if (kind >= TypeMatchKind::OperatorArgumentConversion) {
        if (shouldRecordFailures())
          recordFailure(getConstraintLocator(locator),
                        Failure::IsForbiddenLValue, type1, type2);
        return SolutionKind::Error;
      }
      return matchTypes(cast<InOutType>(desugar1)->getObjectType(),
                        cast<InOutType>(desugar2)->getObjectType(),
                        TypeMatchKind::SameType, subFlags,
                  locator.withPathElement(ConstraintLocator::ArrayElementType));

    case TypeKind::UnboundGeneric:
      llvm_unreachable("Unbound generic type should have been opened");

    case TypeKind::BoundGenericClass:
    case TypeKind::BoundGenericEnum:
    case TypeKind::BoundGenericStruct: {
      auto bound1 = cast<BoundGenericType>(desugar1);
      auto bound2 = cast<BoundGenericType>(desugar2);
      
      if (bound1->getDecl() == bound2->getDecl()) {
        conversionsOrFixes.push_back(ConversionRestrictionKind::DeepEquality);
      }
      break;
    }
    }
  }

  if (concrete && kind >= TypeMatchKind::Subtype) {
    auto tuple1 = type1->getAs<TupleType>();
    auto tuple2 = type2->getAs<TupleType>();

    // Detect when the source and destination are both permit scalar
    // conversions, but the source has a name and the destination does not have
    // the same name.
    bool tuplesWithMismatchedNames = false;
    if (tuple1 && tuple2) {
      int scalar1 = tuple1->getElementForScalarInit();
      int scalar2 = tuple2->getElementForScalarInit();
      if (scalar1 >= 0 && scalar2 >= 0) {
        auto name1 = tuple1->getElement(scalar1).getName();
        auto name2 = tuple2->getElement(scalar2).getName();
        tuplesWithMismatchedNames = !name1.empty() && name1 != name2;
      }
    }

    if (tuple2 && !tuplesWithMismatchedNames) {
      // A scalar type is a trivial subtype of a one-element, non-variadic tuple
      // containing a single element if the scalar type is a subtype of
      // the type of that tuple's element.
      //
      // A scalar type can be converted to a tuple so long as there is at
      // most one non-defaulted element.
      if ((tuple2->getNumElements() == 1 &&
           !tuple2->getElement(0).isVararg()) ||
          (kind >= TypeMatchKind::Conversion &&
           tuple2->getElementForScalarInit() >= 0)) {
        conversionsOrFixes.push_back(
          ConversionRestrictionKind::ScalarToTuple);

        // FIXME: Prohibits some user-defined conversions for tuples.
        goto commit_to_conversions;
      }
    }

    if (tuple1 && !tuplesWithMismatchedNames) {
      // A single-element tuple can be a trivial subtype of a scalar.
      if (tuple1->getNumElements() == 1 &&
          !tuple1->getElement(0).isVararg()) {
        conversionsOrFixes.push_back(
          ConversionRestrictionKind::TupleToScalar);
      }
    }

    // Subclass-to-superclass conversion.
    if (type1->mayHaveSuperclass() && type2->mayHaveSuperclass() &&
        type2->getClassOrBoundGenericClass() &&
        type1->getClassOrBoundGenericClass()
          != type2->getClassOrBoundGenericClass()) {
      conversionsOrFixes.push_back(ConversionRestrictionKind::Superclass);
    }
    
    // Metatype to object conversion.
    //
    // Class and protocol metatypes are interoperable with certain Objective-C
    // runtime classes, but only when ObjC interop is enabled.
    
    if (TC.getLangOpts().EnableObjCInterop) {
      // These conversions are between concrete types that don't need further
      // resolution, so we can consider them immediately solved.
      auto addSolvedRestrictedConstraint
        = [&](ConversionRestrictionKind restriction) -> SolutionKind {
          auto constraint = Constraint::createRestricted(*this,
                                                   ConstraintKind::Subtype,
                                                   restriction,
                                                   type1, type2,
                                                   getConstraintLocator(locator));
          addConstraint(constraint);
          return SolutionKind::Solved;
        };
      
      if (auto meta1 = type1->getAs<MetatypeType>()) {
        if (meta1->getInstanceType()->mayHaveSuperclass()
            && type2->isAnyObject()) {
          increaseScore(ScoreKind::SK_UserConversion);
          return addSolvedRestrictedConstraint(
                           ConversionRestrictionKind::ClassMetatypeToAnyObject);
        }
        // Single @objc protocol value metatypes can be converted to the ObjC
        // Protocol class type.
        auto isProtocolClassType = [&](Type t) -> bool {
          if (auto classDecl = t->getClassOrBoundGenericClass())
            if (classDecl->getName() == getASTContext().Id_Protocol
                && classDecl->getModuleContext()->getName()
                    == getASTContext().Id_ObjectiveC)
              return true;
          return false;
        };
        
        if (auto protoTy = meta1->getInstanceType()->getAs<ProtocolType>()) {
          if (protoTy->getDecl()->isObjC()
              && isProtocolClassType(type2)) {
            increaseScore(ScoreKind::SK_UserConversion);
            return addSolvedRestrictedConstraint(
                    ConversionRestrictionKind::ProtocolMetatypeToProtocolClass);
          }
        }
      }
      if (auto meta1 = type1->getAs<ExistentialMetatypeType>()) {
        // Class-constrained existential metatypes can be converted to AnyObject.
        if (meta1->getInstanceType()->isClassExistentialType()
            && type2->isAnyObject()) {
          increaseScore(ScoreKind::SK_UserConversion);
          return addSolvedRestrictedConstraint(
                     ConversionRestrictionKind::ExistentialMetatypeToAnyObject);
        }
      }
    }
    
    // Implicit collection conversions.
    if (kind >= TypeMatchKind::Conversion) {      
      if (isArrayType(desugar1) && isArrayType(desugar2)) {
        conversionsOrFixes.push_back(ConversionRestrictionKind::ArrayUpcast);
      } else if (isDictionaryType(desugar1) && isDictionaryType(desugar2)) {
        conversionsOrFixes.push_back(
          ConversionRestrictionKind::DictionaryUpcast);
      } else if (isSetType(desugar1) && isSetType(desugar2)) {
        conversionsOrFixes.push_back(
          ConversionRestrictionKind::SetUpcast);
      }
    }
  }
  
  if (kind == TypeMatchKind::BindToPointerType) {
    if (desugar2->isEqual(getASTContext().TheEmptyTupleType))
      return SolutionKind::Solved;
  }

  if (concrete && kind >= TypeMatchKind::Conversion) {
    // An lvalue of type T1 can be converted to a value of type T2 so long as
    // T1 is convertible to T2 (by loading the value).
    if (type1->is<LValueType>())
      conversionsOrFixes.push_back(
        ConversionRestrictionKind::LValueToRValue);

    // An expression can be converted to an auto-closure function type, creating
    // an implicit closure.
    if (auto function2 = type2->getAs<FunctionType>()) {
      if (function2->isAutoClosure())
        return matchTypes(type1, function2->getResult(), kind, subFlags,
                          locator.withPathElement(ConstraintLocator::Load));
    }

    // Bridging from a value type to an Objective-C class type.
    // FIXME: Banned for operator parameters, like user conversions are.

    // NOTE: The plan for <rdar://problem/18311362> was to make such bridging
    // conversions illegal except when explicitly converting with the 'as'
    // operator. But using a String to subscript an [NSObject : AnyObject] is
    // sufficiently common due to bridging that disallowing such conversions is
    // not yet feasible, and a more targeted fix in the type checker is hard to
    // justify.

    if (type1->isPotentiallyBridgedValueType() &&
        type1->getAnyNominal() 
          != TC.Context.getImplicitlyUnwrappedOptionalDecl() &&
        !(flags & TMF_ApplyingOperatorParameter)) {
      
      auto isBridgeableTargetType = type2->isBridgeableObjectType();
      
      // Allow bridged conversions to CVarArgType through NSObject.
      if (!isBridgeableTargetType && type2->isExistentialType()) {
        if (auto nominalType = type2->getAs<NominalType>())
          isBridgeableTargetType = nominalType->getDecl()->getName() ==
                                      TC.Context.Id_CVarArgType;
      }
      
      if (isBridgeableTargetType && TC.getBridgedToObjC(DC, true, type1)) {
        conversionsOrFixes.push_back(ConversionRestrictionKind::BridgeToObjC);
      }
    }

    if (kind == TypeMatchKind::ExplicitConversion) {
      // Bridging from an Objective-C class type to a value type.
      // Note that specifically require a class or class-constrained archetype
      // here, because archetypes cannot be bridged.
      if (type1->mayHaveSuperclass() && type2->isPotentiallyBridgedValueType() &&
          type2->getAnyNominal() 
            != TC.Context.getImplicitlyUnwrappedOptionalDecl() &&
          allowsBridgingFromObjC(TC, DC, type2)) {
        conversionsOrFixes.push_back(ConversionRestrictionKind::BridgeFromObjC);
      }
      
      // Bridging from an ErrorType to an Objective-C NSError.
      auto errorType = TC.Context.getProtocol(KnownProtocolKind::ErrorType);
      if (TC.conformsToProtocol(type1, errorType, DC,
                                ConformanceCheckFlags::InExpression))
        if (auto NSErrorTy = TC.getNSErrorType(DC))
          if (type2->isEqual(NSErrorTy))
            conversionsOrFixes.push_back(
                                   ConversionRestrictionKind::BridgeToNSError);
    }
    
    // Pointer arguments can be converted from pointer-compatible types.
    if (kind >= TypeMatchKind::ArgumentConversion) {
      if (auto bgt2 = type2->getAs<BoundGenericType>()) {
        if (bgt2->getDecl() == getASTContext().getUnsafeMutablePointerDecl()
            || bgt2->getDecl() == getASTContext().getUnsafePointerDecl()) {
          // UnsafeMutablePointer can be converted from an inout reference to a
          // scalar or array.
          if (auto inoutType1 = dyn_cast<InOutType>(desugar1)) {
            auto inoutBaseType = inoutType1->getInOutObjectType();
            
            auto isWrappedArray = isArrayType(inoutBaseType);
            
            if (auto baseTyVar1 = dyn_cast<TypeVariableType>(inoutBaseType.
                                                                getPointer())) {
              TypeVariableType *tv1;
              auto bt1 = getFixedTypeRecursive(baseTyVar1, tv1,
                                    kind == TypeMatchKind::SameType,
                                    isArgumentTupleConversion);
              
              isWrappedArray = isArrayType(bt1);
            }
            
            if (isWrappedArray) {
              conversionsOrFixes.push_back(
                                     ConversionRestrictionKind::ArrayToPointer);
            }
            conversionsOrFixes.push_back(
                                     ConversionRestrictionKind::InoutToPointer);
          }
          
          if (!(flags & TMF_ApplyingOperatorParameter)) {
            auto bgt1 = type1->getAs<BoundGenericType>();

            // We can potentially convert from an UnsafeMutablePointer
            // of a different type, if we're a void pointer.
            if (bgt1 && bgt1->getDecl()
                  == getASTContext().getUnsafeMutablePointerDecl()) {
              // Favor an UnsafeMutablePointer-to-UnsafeMutablePointer
              // conversion.
              if (bgt1->getDecl() != bgt2->getDecl())
                increaseScore(ScoreKind::SK_ScalarPointerConversion);
              conversionsOrFixes.push_back(
                                   ConversionRestrictionKind::PointerToPointer);
            }
            
            // UnsafePointer can also be converted from an array
            // or string value, or a UnsafePointer or
            // AutoreleasingUnsafeMutablePointer.
            if (bgt2->getDecl() == getASTContext().getUnsafePointerDecl()){
              if (isArrayType(type1)) {
                conversionsOrFixes.push_back(
                                     ConversionRestrictionKind::ArrayToPointer);
              }
              
              // The pointer can be converted from a string, if the element type
              // is compatible.
              if (type1->isEqual(TC.getStringType(DC))) {
                TypeVariableType *tv = nullptr;
                auto baseTy = getFixedTypeRecursive(bgt2->getGenericArgs()[0],
                                                    tv, false, false);
                
                if (tv || isStringCompatiblePointerBaseType(TC, DC, baseTy))
                  conversionsOrFixes.push_back(
                                    ConversionRestrictionKind::StringToPointer);
              }
              
              if (bgt1 && bgt1->getDecl()
                               == getASTContext().getUnsafePointerDecl()) {
                conversionsOrFixes.push_back(
                                   ConversionRestrictionKind::PointerToPointer);
              }
              if (bgt1 && bgt1->getDecl() == getASTContext()
                    .getAutoreleasingUnsafeMutablePointerDecl()) {
                conversionsOrFixes.push_back(
                                   ConversionRestrictionKind::PointerToPointer);
              }
            }
          }
        } else if (
          bgt2->getDecl() == getASTContext()
            .getAutoreleasingUnsafeMutablePointerDecl()) {
          // AutoUnsafeMutablePointer can be converted from an inout
          // reference to a scalar.
          if (type1->is<InOutType>()) {
            conversionsOrFixes.push_back(
                                     ConversionRestrictionKind::InoutToPointer);
          }
        }
      }
    }
  }

  if (concrete && kind >= TypeMatchKind::OperatorArgumentConversion) {
    // If the RHS is an inout type, the LHS must be an @lvalue type.
    if (auto *iot = type2->getAs<InOutType>()) {
      return matchTypes(type1, LValueType::get(iot->getObjectType()),
                        kind, subFlags,
                        locator.withPathElement(
                                ConstraintLocator::ArrayElementType));
    }
  }

  // For a subtyping relation involving a conversion to an existential
  // metatype, match the child types.
  if (concrete && kind >= TypeMatchKind::Subtype) {
    if (auto meta1 = type1->getAs<MetatypeType>()) {
      if (auto meta2 = type2->getAs<ExistentialMetatypeType>()) {
        return matchTypes(meta1->getInstanceType(),
                          meta2->getInstanceType(),
                          TypeMatchKind::Subtype, subFlags,
                          locator.withPathElement(
                                  ConstraintLocator::InstanceType));
      }
    }
  }
  
  // For a subtyping relation involving two existential types or subtyping of
  // a class existential type, or a conversion from any type to an
  // existential type, check whether the first type conforms to each of the
  // protocols in the second type.
  if (concrete && kind >= TypeMatchKind::Subtype &&
      type2->isExistentialType()) {
    conversionsOrFixes.push_back(ConversionRestrictionKind::Existential);
  }

  // A value of type T can be converted to type U? if T is convertible to U.
  // A value of type T? can be converted to type U? if T is convertible to U.
  // The above conversions also apply to implicitly unwrapped optional types, except
  // that there is no implicit conversion from T? to T!.
  {
    BoundGenericType *boundGenericType2;
    
    if (concrete && kind >= TypeMatchKind::Subtype &&
        (boundGenericType2 = type2->getAs<BoundGenericType>())) {
      auto decl2 = boundGenericType2->getDecl();
      if (auto optionalKind2 = decl2->classifyAsOptionalType()) {
        assert(boundGenericType2->getGenericArgs().size() == 1);
        
        BoundGenericType *boundGenericType1 = type1->getAs<BoundGenericType>();
        if (boundGenericType1) {
          auto decl1 = boundGenericType1->getDecl();
          if (decl1 == decl2) {
            assert(boundGenericType1->getGenericArgs().size() == 1);
            conversionsOrFixes.push_back(
                                ConversionRestrictionKind::OptionalToOptional);
          } else if (optionalKind2 == OTK_Optional &&
                     decl1 == TC.Context.getImplicitlyUnwrappedOptionalDecl()) {
            assert(boundGenericType1->getGenericArgs().size() == 1);
            conversionsOrFixes.push_back(
                       ConversionRestrictionKind::ImplicitlyUnwrappedOptionalToOptional);
          } else if (optionalKind2 == OTK_ImplicitlyUnwrappedOptional &&
                     kind >= TypeMatchKind::Conversion &&
                     decl1 == TC.Context.getOptionalDecl()) {
            assert(boundGenericType1->getGenericArgs().size() == 1);
            conversionsOrFixes.push_back(
                       ConversionRestrictionKind::OptionalToImplicitlyUnwrappedOptional);
          }
        }
        
        // Do not attempt a value-to-optional conversion when resolving the
        // applicable overloads for an operator application with nil operands.
        if (!(subFlags & TMF_ApplyingOperatorWithNil)) {
          conversionsOrFixes.push_back(
              ConversionRestrictionKind::ValueToOptional);
        }
      }
    }
  }

  // A value of type T! can be (unsafely) forced to U if T
  // is convertible to U.
  {
    Type objectType1;
    if (concrete && kind >= TypeMatchKind::Conversion &&
        (objectType1 = lookThroughImplicitlyUnwrappedOptionalType(type1))) {
      conversionsOrFixes.push_back(
                          ConversionRestrictionKind::ForceUnchecked);
    }
  }
  
  // If the types disagree, but we're comparing a non-void, single-expression
  // closure result type to a void function result type, allow the conversion.
  {
    if (concrete && kind >= TypeMatchKind::Subtype && type2->isVoid()) {
      SmallVector<LocatorPathElt, 2> parts;
      locator.getLocatorParts(parts);
      
      while (!parts.empty()) {
        if (parts.back().getKind() == ConstraintLocator::ClosureResult) {
          increaseScore(SK_FunctionConversion);
          return SolutionKind::Solved;
        }
        parts.pop_back();
      }
    }
  }

commit_to_conversions:
  // When we hit this point, we're committed to the set of potential
  // conversions recorded thus far.
  //
  //
  // FIXME: One should only jump to this label in the case where we want to
  // cut off other potential conversions because we know none of them apply.
  // Gradually, those gotos should go away as we can handle more kinds of
  // conversions via disjunction constraints.

  // If we should attempt fixes, add those to the list. They'll only be visited
  // if there are no other possible solutions.
  if (shouldAttemptFixes() && !typeVar1 && !typeVar2 &&
      !(flags & TMF_ApplyingFix) && kind >= TypeMatchKind::Conversion) {
    Type objectType1 = type1->getRValueObjectType();

    // If the source type is a function type that could be applied with (),
    // try it.
    if (isFunctionTypeAcceptingNoArguments(objectType1)) {
      conversionsOrFixes.push_back(FixKind::NullaryCall);
    }

    // If we have an optional type, try to force-unwrap it.
    // FIXME: Should we also try '?'?
    if (objectType1->getOptionalObjectType()) {
      conversionsOrFixes.push_back(FixKind::ForceOptional);
    }

    // If we have a value of type AnyObject that we're trying to convert to
    // a class, force a downcast.
    // FIXME: Also allow types bridged through Objective-C classes.
    if (objectType1->isAnyObject() &&
        type2->getClassOrBoundGenericClass()) {
      conversionsOrFixes.push_back(Fix::getForcedDowncast(*this, type2));
    }

    // If we could perform a bridging cast, try it.
    if (isArrayDictionarySetOrString(TC.Context, type2) &&
        TC.getDynamicBridgedThroughObjCClass(DC, true, objectType1, type2)) {
      conversionsOrFixes.push_back(Fix::getForcedDowncast(*this, type2));
    }

    // If we're converting an lvalue to an inout type, add the missing '&'.
    if (type2->getRValueType()->is<InOutType>() && type1->is<LValueType>()) {
      conversionsOrFixes.push_back(FixKind::AddressOf);
    }
  }

  if (conversionsOrFixes.empty()) {
    // If one of the types is a type variable, we leave this unsolved.
    if (typeVar1 || typeVar2)
      return SolutionKind::Unsolved;

    // If we are supposed to record failures, do so.
    if (shouldRecordFailures()) {
      recordFailure(getConstraintLocator(locator),
                    getRelationalFailureKind(kind), type1, type2);
    }

    return SolutionKind::Error;
  }

  // Where there is more than one potential conversion, create a disjunction
  // so that we'll explore all of the options.
  if (conversionsOrFixes.size() > 1) {
    auto fixedLocator = getConstraintLocator(locator);
    SmallVector<Constraint *, 2> constraints;
    for (auto potential : conversionsOrFixes) {
      auto constraintKind = getConstraintKind(kind);

      if (auto restriction = potential.getRestriction()) {
        // Determine the constraint kind. For a deep equality constraint, only
        // perform equality.
        if (*restriction == ConversionRestrictionKind::DeepEquality)
          constraintKind = ConstraintKind::Equal;

        constraints.push_back(
          Constraint::createRestricted(*this, constraintKind, *restriction,
                                       type1, type2, fixedLocator));
        continue;
      }

      // If the first thing we found is a fix, add a "don't fix" marker.
      if (conversionsOrFixes.empty()) {
        constraints.push_back(
          Constraint::createFixed(*this, constraintKind, FixKind::None,
                                  type1, type2, fixedLocator));
      }

      auto fix = *potential.getFix();
      constraints.push_back(
        Constraint::createFixed(*this, constraintKind, fix, type1, type2,
                                fixedLocator));
    }
    addConstraint(Constraint::createDisjunction(*this, constraints,
                                                fixedLocator));
    return SolutionKind::Solved;
  }

  // For a single potential conversion, directly recurse, so that we
  // don't allocate a new constraint or constraint locator.

  // Handle restrictions.
  if (auto restriction = conversionsOrFixes[0].getRestriction()) {
    ConstraintRestrictions.push_back(
        std::make_tuple(type1, type2, *restriction));

    if (flags & TMF_UnwrappingOptional) {
      subFlags |= TMF_UnwrappingOptional;
    }
    
    return simplifyRestrictedConstraint(*restriction, type1, type2,
                                        kind, subFlags, locator);
  }

  // Handle fixes.
  auto fix = *conversionsOrFixes[0].getFix();
  return simplifyFixConstraint(fix, type1, type2, kind, subFlags, locator);
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstructionConstraint(Type valueType, 
                                                 FunctionType *fnType,
                                                 unsigned flags,
                                                 ConstraintLocator *locator) {
  // Desugar the value type.
  auto desugarValueType = valueType->getDesugaredType();

  // If we have a type variable that has been bound to a fixed type,
  // look through to that fixed type.
  auto desugarValueTypeVar = dyn_cast<TypeVariableType>(desugarValueType);
  if (desugarValueTypeVar) {
    if (auto fixed = getFixedType(desugarValueTypeVar)) {
      valueType = fixed;
      desugarValueType = fixed->getDesugaredType();
      desugarValueTypeVar = nullptr;
    }
  }

  Type argType = fnType->getInput();
  Type resultType = fnType->getResult();

  switch (desugarValueType->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("Type has not been desugared completely");

#define ARTIFICIAL_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
      llvm_unreachable("artificial type in constraint");
    
  case TypeKind::Error:
    return SolutionKind::Error;

  case TypeKind::GenericFunction:
  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    llvm_unreachable("unmapped dependent type");

  case TypeKind::TypeVariable:
    return SolutionKind::Unsolved;

  case TypeKind::Tuple: {
    // Tuple construction is simply tuple conversion.
    if (matchTypes(resultType, desugarValueType,
                   TypeMatchKind::BindType,
                   flags,
                   ConstraintLocatorBuilder(locator)
                     .withPathElement(ConstraintLocator::ApplyFunction))
          == SolutionKind::Error)
      return SolutionKind::Error;

    return matchTypes(argType, valueType, TypeMatchKind::Conversion,
                      flags|TMF_GenerateConstraints, locator);
  }

  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
  case TypeKind::Archetype:
  case TypeKind::DynamicSelf:
  case TypeKind::ProtocolComposition:
  case TypeKind::Protocol:
    // Break out to handle the actual construction below.
    break;

  case TypeKind::PolymorphicFunction:
    llvm_unreachable("Polymorphic function type should have been opened");

  case TypeKind::UnboundGeneric:
    llvm_unreachable("Unbound generic type should have been opened");

#define BUILTIN_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::ExistentialMetatype:
  case TypeKind::Metatype:
  case TypeKind::Function:
  case TypeKind::LValue:
  case TypeKind::InOut:
  case TypeKind::Module:
    // If we are supposed to record failures, do so.
    if (shouldRecordFailures()) {
      recordFailure(locator, Failure::TypesNotConstructible,
                    valueType, argType);
    }
    
    return SolutionKind::Error;
  }

  NameLookupOptions lookupOptions = defaultConstructorLookupOptions;
  if (isa<AbstractFunctionDecl>(DC))
    lookupOptions |= NameLookupFlags::KnownPrivate;
  auto ctors = TC.lookupConstructors(DC, valueType, lookupOptions);
  if (!ctors) {
    // If we are supposed to record failures, do so.
    if (shouldRecordFailures()) {
      recordFailure(locator, Failure::NoPublicInitializers, valueType);
    }
    
    return SolutionKind::Error;
  }

  auto &context = getASTContext();
  auto name = context.Id_init;
  auto applyLocator = getConstraintLocator(locator,
                                           ConstraintLocator::ApplyArgument);
  auto tv = createTypeVariable(applyLocator,
                               TVO_CanBindToLValue|TVO_PrefersSubtypeBinding);

  // The constructor will have function type T -> T2, for a fresh type
  // variable T. T2 is the result type provided via the construction
  // constraint itself.
  addValueMemberConstraint(valueType, name,
                           FunctionType::get(tv, resultType),
                           getConstraintLocator(
                             locator, 
                             ConstraintLocator::ConstructorMember));

  // The first type must be convertible to the constructor's argument type.
  addConstraint(ConstraintKind::ArgumentTupleConversion, argType, tv,
                applyLocator);

  return SolutionKind::Solved;
}

ConstraintSystem::SolutionKind ConstraintSystem::simplifyConformsToConstraint(
                                 Type type,
                                 ProtocolDecl *protocol,
                                 ConstraintLocatorBuilder locator,
                                 unsigned flags,
                                 bool allowNonConformingExistential) {
  // Dig out the fixed type to which this type refers.
  TypeVariableType *typeVar;
  type = getFixedTypeRecursive(type, typeVar, /*wantRValue=*/true);

  // If we hit a type variable without a fixed type, we can't
  // solve this yet.
  if (typeVar)
    return SolutionKind::Unsolved;

  // If existential types don't need to conform (i.e., they only need to
  // contain the protocol), check that separately.
  if (allowNonConformingExistential && type->isExistentialType()) {
    SmallVector<ProtocolDecl *, 4> protocols;
    type->getAnyExistentialTypeProtocols(protocols);

    for (auto ap : protocols) {
      // If this isn't the protocol we're looking for, continue looking.
      if (ap == protocol || ap->inheritsFrom(protocol))
        return SolutionKind::Solved;
    }
  } else {
    // Check whether this type conforms to the protocol.
    if (TC.conformsToProtocol(type, protocol, DC,
                              ConformanceCheckFlags::InExpression))
      return SolutionKind::Solved;
  }
  
  if (!type->getAnyOptionalObjectType().isNull() &&
      (protocol == TC.getProtocol(SourceLoc(),
                                 KnownProtocolKind::BooleanType))) {
    
    Fixes.push_back({FixKind::OptionalToBoolean,
      getConstraintLocator(locator)});
    
    return SolutionKind::Solved;
  }
  
  // There's nothing more we can do; fail.
  recordFailure(getConstraintLocator(locator),
                Failure::DoesNotConformToProtocol, type,
                protocol->getDeclaredType());
  return SolutionKind::Error;
}

/// Determine the kind of checked cast to perform from the given type to
/// the given type.
///
/// This routine does not attempt to check whether the cast can actually
/// succeed; that's the caller's responsibility.
static CheckedCastKind getCheckedCastKind(ConstraintSystem *cs,
                                          Type fromType,
                                          Type toType) {
  // Array downcasts are handled specially.
  if (cs->isArrayType(fromType) && cs->isArrayType(toType)) {
    return CheckedCastKind::ArrayDowncast;
  }

  // Dictionary downcasts are handled specially.
  if (cs->isDictionaryType(fromType) && cs->isDictionaryType(toType)) {
    return CheckedCastKind::DictionaryDowncast;
  }

  // Set downcasts are handled specially.
  if (cs->isSetType(fromType) && cs->isSetType(toType)) {
    return CheckedCastKind::SetDowncast;
  }

  // If we can bridge through an Objective-C class, do so.
  auto &tc = cs->getTypeChecker();
  if (tc.getDynamicBridgedThroughObjCClass(cs->DC, true, fromType, toType)) {
    return CheckedCastKind::BridgeFromObjectiveC;
  }

  return CheckedCastKind::ValueCast;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyCheckedCastConstraint(
                    Type fromType, Type toType,
                    ConstraintLocatorBuilder locator) {
  do {
    // Dig out the fixed type to which this type refers.
    TypeVariableType *typeVar1;
    fromType = getFixedTypeRecursive(fromType, typeVar1, /*wantRValue=*/true);

    // If we hit a type variable without a fixed type, we can't
    // solve this yet.
    if (typeVar1)
      return SolutionKind::Unsolved;

    // Dig out the fixed type to which this type refers.
    TypeVariableType *typeVar2;
    toType = getFixedTypeRecursive(toType, typeVar2, /*wantRValue=*/true);

    // If we hit a type variable without a fixed type, we can't
    // solve this yet.
    if (typeVar2)
      return SolutionKind::Unsolved;

    Type origFromType = fromType;
    Type origToType = toType;

    // Peel off optionals metatypes from the types, because we might cast through
    // them.
    toType = toType->lookThroughAllAnyOptionalTypes();
    fromType = fromType->lookThroughAllAnyOptionalTypes();

    // Peel off metatypes, since if we can cast two types, we can cast their
    // metatypes.
    while (auto toMetatype = toType->getAs<MetatypeType>()) {
      auto fromMetatype = fromType->getAs<MetatypeType>();
      if (!fromMetatype)
        break;
      toType = toMetatype->getInstanceType();
      fromType = fromMetatype->getInstanceType();
    }

    // Peel off a potential layer of existential<->concrete metatype conversion.
    if (auto toMetatype = toType->getAs<AnyMetatypeType>()) {
      if (auto fromMetatype = fromType->getAs<MetatypeType>()) {
        toType = toMetatype->getInstanceType();
        fromType = fromMetatype->getInstanceType();
      }
    }

    // If nothing changed, we're done.
    if (fromType.getPointer() == origFromType.getPointer() &&
        toType.getPointer() == origToType.getPointer())
      break;
  } while (true);

  auto kind = getCheckedCastKind(this, fromType, toType);
  switch (kind) {
  case CheckedCastKind::ArrayDowncast: {
    auto fromBaseType = getBaseTypeForArrayType(fromType.getPointer());
    auto toBaseType = getBaseTypeForArrayType(toType.getPointer());
    
    // FIXME: Deal with from/to base types that haven't been solved
    // down to type variables yet.

    // Check whether we need to bridge through an Objective-C class.
    if (auto classType = TC.getDynamicBridgedThroughObjCClass(DC, true,
                                                              fromBaseType,
                                                              toBaseType)) {
      // The class we're bridging through must be a subtype of the type we're
      // coming from.
      addConstraint(ConstraintKind::Subtype, classType, fromBaseType,
                    getConstraintLocator(locator));
      return SolutionKind::Solved;
    }

    addConstraint(ConstraintKind::Subtype, toBaseType, fromBaseType,
                  getConstraintLocator(locator));
    return SolutionKind::Solved;
  }
  case CheckedCastKind::DictionaryDowncast:
  case CheckedCastKind::DictionaryDowncastBridged: {
    Type fromKeyType, fromValueType;
    std::tie(fromKeyType, fromValueType) = *isDictionaryType(fromType);

    Type toKeyType, toValueType;
    std::tie(toKeyType, toValueType) = *isDictionaryType(toType);
    
    // FIXME: Deal with from/to base types that haven't been solved
    // down to type variables yet.

    // Check whether we need to bridge the key through an Objective-C class.
    if (auto bridgedKey = TC.getDynamicBridgedThroughObjCClass(DC, true,
                                                               fromKeyType,
                                                               toKeyType)) {
      toKeyType = bridgedKey;
    }

    // Perform subtype check on the possibly-bridged-through key type.
    unsigned subFlags = TMF_GenerateConstraints;
    auto result = matchTypes(toKeyType, fromKeyType, TypeMatchKind::Subtype, 
                             subFlags, locator);
    if (result == SolutionKind::Error)
      return result;

    // Check whether we need to bridge the value through an Objective-C class.
    if (auto bridgedValue = TC.getDynamicBridgedThroughObjCClass(DC, true,
                                                                 fromValueType,
                                                                 toValueType)) {
      toValueType = bridgedValue;
    }

    // Perform subtype check on the possibly-bridged-through value type.
    switch (matchTypes(toValueType, fromValueType, TypeMatchKind::Subtype, 
                       subFlags, locator)) {
    case SolutionKind::Solved:
      return result;

    case SolutionKind::Unsolved:
      return SolutionKind::Unsolved;

    case SolutionKind::Error:
      return SolutionKind::Error;
    }
  }

  case CheckedCastKind::SetDowncast:
  case CheckedCastKind::SetDowncastBridged: {
    auto fromBaseType = getBaseTypeForSetType(fromType.getPointer());
    auto toBaseType = getBaseTypeForSetType(toType.getPointer());
    // FIXME: Deal with from/to base types that haven't been solved
    // down to type variables yet.

    // Check whether we need to bridge through an Objective-C class.
    if (auto classType = TC.getDynamicBridgedThroughObjCClass(DC, true,
                                                              fromBaseType,
                                                              toBaseType)) {
      // The class we're bridging through must be a subtype of the type we're
      // coming from.
      addConstraint(ConstraintKind::Subtype, classType, fromBaseType,
                    getConstraintLocator(locator));
      return SolutionKind::Solved;
    }

    addConstraint(ConstraintKind::Subtype, toBaseType, fromBaseType,
                  getConstraintLocator(locator));
    return SolutionKind::Solved;
  }
      
  case CheckedCastKind::ValueCast: {
    // If casting among classes, and there are open
    // type variables remaining, introduce a subtype constraint to help resolve
    // them.
    if (fromType->getClassOrBoundGenericClass()
        && toType->getClassOrBoundGenericClass()
        && (fromType->hasTypeVariable() || toType->hasTypeVariable())) {
      addConstraint(ConstraintKind::Subtype, toType, fromType,
                    getConstraintLocator(locator));
    }
      
    return SolutionKind::Solved;
  }

  case CheckedCastKind::BridgeFromObjectiveC: {
    // This existential-to-concrete cast might bridge through an Objective-C
    // class type.
    Type objCClass = TC.getDynamicBridgedThroughObjCClass(DC, true,
                                                          fromType,
                                                          toType);
    assert(objCClass && "Type must be bridged");
    (void)objCClass;
    // Otherwise no constraint is necessary; as long as both objCClass and
    // fromType are Objective-C types, they can't have any open type variables,
    // and conversion between unrelated classes will be diagnosed in
    // typeCheckCheckedCast.
    return SolutionKind::Solved;
  }

  case CheckedCastKind::Coercion:
  case CheckedCastKind::Unresolved:
    llvm_unreachable("Not a valid result");
  }
}

/// Determine whether the given type is the Self type of the protocol.
static bool isProtocolSelf(Type type) {
  if (auto genericParam = type->getAs<GenericTypeParamType>())
    return genericParam->getDepth() == 0;
  
  return false;
}

/// Determine whether the given type contains a reference to the 'Self' type
/// of a protocol.
static bool containsProtocolSelf(Type type) {
  // If the type is not dependent, it doesn't refer to 'Self'.
  if (!type->isDependentType())
    return false;

  return type.findIf([](Type type) -> bool {
    return isProtocolSelf(type);
  });
}

/// Determine whether the given protocol member's signature prevents
/// it from being used in an existential reference.
static bool isUnavailableInExistential(TypeChecker &tc, ValueDecl *decl) {
  Type type = decl->getInterfaceType();

  // For a function or constructor, skip the implicit 'this'.
  if (auto afd = dyn_cast<AbstractFunctionDecl>(decl)) {
    type = type->castTo<AnyFunctionType>()->getResult();

    // Allow functions to return Self, but not have Self anywhere in
    // their argument types.
    for (unsigned i = 1, n = afd->getNumParamPatterns(); i != n; ++i) {
      // Check whether the input type contains Self anywhere.
      auto fnType = type->castTo<AnyFunctionType>();
      if (containsProtocolSelf(fnType->getInput()))
        return true;
      
      type = fnType->getResult();
    }

    // Look through one level of optional on the result type.
    if (auto valueType = type->getAnyOptionalObjectType())
      type = valueType;
    
    if (isProtocolSelf(type) || type->is<DynamicSelfType>())
      return false;

    return containsProtocolSelf(type);
  }

  return containsProtocolSelf(type);
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyOptionalObjectConstraint(const Constraint &constraint)
{
  // Resolve the optional type.
  Type optLValueTy = simplifyType(constraint.getFirstType());
  Type optTy = optLValueTy->getRValueType();
  
  if (optTy->is<TypeVariableType>()) {
    return SolutionKind::Unsolved;
  }
  
  // If the base type is not optional, the constraint fails.
  Type objectTy = optTy->getAnyOptionalObjectType();
  if (!objectTy) {
    recordFailure(constraint.getLocator(), Failure::IsNotOptional,
                  optTy);
    return SolutionKind::Error;
  }
  
  // The object type is an lvalue if the optional was.
  if (optLValueTy->is<LValueType>()) {
    objectTy = LValueType::get(objectTy);
  }

  // Equate it to the other type in the constraint.
  addConstraint(ConstraintKind::Bind, objectTy, constraint.getSecondType(),
                constraint.getLocator());
  
  return SolutionKind::Solved;
}

/// If the given type conforms to the RawRepresentable protocol,
/// return its underlying raw type.
static Type getRawRepresentableValueType(TypeChecker &tc, DeclContext *dc, 
                                         Type type) {
  auto proto = tc.Context.getProtocol(KnownProtocolKind::RawRepresentable);
  if (!proto)
    return nullptr;

  if (type->hasTypeVariable())
    return nullptr;

  ProtocolConformance *conformance = nullptr;
  if (!tc.conformsToProtocol(type, proto, dc,
                             ConformanceCheckFlags::InExpression, &conformance))
    return nullptr;

  return tc.getWitnessType(type, proto, conformance, tc.Context.Id_RawValue,
                           diag::broken_raw_representable_requirement);
}

/// Retrieve the argument labels that are provided for a member
/// reference at the given locator.
static Optional<ArrayRef<Identifier>> 
getArgumentLabels(ConstraintSystem &cs, ConstraintLocatorBuilder locator) {
  SmallVector<LocatorPathElt, 2> parts;
  Expr *anchor = locator.getLocatorParts(parts);
  if (!anchor)
    return None;

  while (!parts.empty()) {
    if (parts.back().getKind() == ConstraintLocator::Member ||
        parts.back().getKind() == ConstraintLocator::SubscriptMember) {
      parts.pop_back();
      continue;
    }

    if (parts.back().getKind() == ConstraintLocator::ConstructorMember) {
      // FIXME: Workaround for strange anchor on ConstructorMember locators.
      if (auto call = dyn_cast<CallExpr>(anchor)) {
        anchor = call->getFn()->getSemanticsProvidingExpr();

        if (auto optionalWrapper = dyn_cast<BindOptionalExpr>(anchor))
          anchor = optionalWrapper->getSubExpr();
        else if (auto forceWrapper = dyn_cast<ForceValueExpr>(anchor))
          anchor = forceWrapper->getSubExpr();
      }

      parts.pop_back();
      continue;
    }
    
    break;
  }
  
  if (!parts.empty())
    return None;

  auto known = cs.ArgumentLabels.find(cs.getConstraintLocator(anchor));
  if (known == cs.ArgumentLabels.end())
    return None;

  return known->second;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyMemberConstraint(const Constraint &constraint) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  Type baseTy = simplifyType(constraint.getFirstType());
  Type baseObjTy = baseTy->getRValueType();

  // Try to look through ImplicitlyUnwrappedOptional<T>; the result is
  // always an r-value.
  if (auto objTy = lookThroughImplicitlyUnwrappedOptionalType(baseObjTy)) {
    increaseScore(SK_ForceUnchecked);
    
    baseObjTy = objTy;
    if (baseTy->is<LValueType>())
      baseTy = LValueType::get(objTy);
    else
      baseTy = objTy;
  }

  // Dig out the instance type.
  bool isMetatype = false;
  Type instanceTy = baseObjTy;
  if (auto baseObjMeta = baseObjTy->getAs<AnyMetatypeType>()) {
    instanceTy = baseObjMeta->getInstanceType();
    isMetatype = true;
  }

  bool isUnresolvedMember;
  switch (constraint.getKind()) {
  case ConstraintKind::UnresolvedValueMember:
    isUnresolvedMember = true;
    break;
  case ConstraintKind::ValueMember:
  case ConstraintKind::TypeMember:
    isUnresolvedMember = false;
    break;
  default:
    llvm_unreachable("not a member constraint");
  }
  
  if (instanceTy->is<TypeVariableType>())
    return SolutionKind::Unsolved;

  // If the base type is a tuple type, look for the named or indexed member
  // of the tuple.
  DeclName name = constraint.getMember();
  Type memberTy = constraint.getSecondType();
  if (auto baseTuple = baseObjTy->getAs<TupleType>()) {
    // Tuples don't have compound-name members.
    if (!name.isSimpleName()) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);
      return SolutionKind::Error;
    }
    
    StringRef nameStr = name.getBaseName().str();
    int fieldIdx = -1;
    // Resolve a number reference into the tuple type.
    unsigned Value = 0;
    if (!nameStr.getAsInteger(10, Value) &&
        Value < baseTuple->getNumElements()) {
      fieldIdx = Value;
    } else {
      fieldIdx = baseTuple->getNamedElementId(name.getBaseName());
    }

    if (fieldIdx == -1) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);
      return SolutionKind::Error;
    }

    // Add an overload set that selects this field.
    OverloadChoice choice(baseTy, fieldIdx);
    addBindOverloadConstraint(memberTy, choice, constraint.getLocator());
    return SolutionKind::Solved;
  }

  // FIXME: If the base type still involves type variables, we want this
  // constraint to be unsolved. This effectively requires us to solve the
  // left-hand side of a dot expression before we look for members.

  // If we have a simple name, determine whether there are argument
  // labels we can use to restrict the set of lookup results.
  Optional<ArrayRef<Identifier>> argumentLabels;
  if (name.isSimpleName()) {
    argumentLabels = getArgumentLabels(
                       *this, 
                       ConstraintLocatorBuilder(constraint.getLocator()));

    // If we're referencing AnyObject and we have argument labels, put
    // the argument labels into the name: we don't want to look for
    // anything else, because the cost of the general search is so
    // high.
    if (baseObjTy->isAnyObject() && argumentLabels) {
      name = DeclName(TC.Context, name.getBaseName(), *argumentLabels);
      argumentLabels.reset();
    }
  }

  /// Determine whether the given declaration has compatible argument
  /// labels.
  auto hasCompatibleArgumentLabels = [&](ValueDecl *decl) -> bool {
    if (!argumentLabels)
      return true;

    auto name = decl->getFullName();
    if (name.isSimpleName())
      return true;

    for (auto argLabel : *argumentLabels) {
      if (argLabel.empty())
        continue;

      if (std::find(name.getArgumentNames().begin(),
                    name.getArgumentNames().end(),
                    argLabel) == name.getArgumentNames().end()) {
        return false;
      }
    }

    return true;
  };

  bool isExistential = instanceTy->isExistentialType();
  if (name.isSimpleName(TC.Context.Id_init)) {
    // Constructors have their own approach to name lookup.
    NameLookupOptions lookupOptions = defaultConstructorLookupOptions;
    if (isa<AbstractFunctionDecl>(DC))
      lookupOptions |= NameLookupFlags::KnownPrivate;
    auto ctors = TC.lookupConstructors(DC, baseObjTy, lookupOptions);
    if (!ctors) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);

      return SolutionKind::Error;
    }
    
    TypeBase *favoredType = nullptr;

    if (auto anchor = constraint.getLocator()->getAnchor()) {
      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        auto argExpr = applyExpr->getArg();
        favoredType = getFavoredType(argExpr);
        
        if (!favoredType) {
          optimizeConstraints(argExpr);
          favoredType = getFavoredType(argExpr);
        }        
      }
    }
    
    OverloadChoice favoredChoice;

    // Introduce a new overload set.
    retry_ctors_after_fail:
    bool labelMismatch = false;
    SmallVector<OverloadChoice, 4> choices;
    for (auto constructor : ctors) {
      // If the constructor is invalid, skip it.
      // FIXME: Note this as invalid, in case we don't find a solution,
      // so we don't let errors cascade further.
      TC.validateDecl(constructor, true);
      if (constructor->isInvalid())
        continue;

      // If the argument labels for this result are incompatible with
      // the call site, skip it.
      if (!hasCompatibleArgumentLabels(constructor)) {
        labelMismatch = true;
        continue;
      }

      // If our base is an existential type, we can't make use of any
      // constructor whose signature involves associated types.
      // FIXME: Mark this as 'unavailable'.
      if (isExistential &&
          isUnavailableInExistential(getTypeChecker(), constructor))
        continue;

      SourceLoc anchorLoc = constraint.getLocator()->getAnchor()->getLoc();
      auto unavailReason = TC.checkDeclarationAvailability(constructor,
                                                           anchorLoc, DC);
      
      // If the invocation's argument expression has a favored constraint,
      // use that information to determine whether a specific overload for
      // the initializer should be favored.
      if (favoredType && !favoredChoice.getDecl()) {
        // Only try and favor monomorphic initializers.
        if (auto fnTypeWithSelf =
            constructor->getType()->getAs<FunctionType>()) {
          
          if (auto fnType =
                  fnTypeWithSelf->getResult()->getAs<FunctionType>()) {
          
            auto argType = fnType->getInput();
            
            if (auto parenType =
                dyn_cast<ParenType>(argType.getPointer())) {
              argType = parenType->getUnderlyingType();
            }
            
            if (argType->isEqual(favoredType)) {
              favoredChoice = OverloadChoice(baseTy, constructor,
                                           /*isSpecialized=*/false, *this,
                                           unavailReason);
            }
          }
        }
      }
      
      choices.push_back(OverloadChoice(baseTy, constructor,
                                       /*isSpecialized=*/false, *this,
                                       unavailReason));
    }


    // If we rejected some possibilities due to an argument-label
    // mismatch and ended up with nothing, try again ignoring the
    // labels. This allows us to perform typo correction on the labels.
    if (choices.empty() && labelMismatch && shouldAttemptFixes()) {
      argumentLabels.reset();
      goto retry_ctors_after_fail;
    }

    // FIXME: Should we look for constructors in bridged types?

    if (choices.empty()) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);
      return SolutionKind::Error;
    }
    
    addOverloadSet(memberTy,
                   choices,
                   constraint.getLocator(),
                   favoredChoice.getDecl() ? &favoredChoice :  nullptr);
    
    return SolutionKind::Solved;
  }

  // If we want member types only, use member type lookup.
  if (constraint.getKind() == ConstraintKind::TypeMember) {
    // Types don't have compound names.
    if (!name.isSimpleName()) {
      // FIXME: Customize diagnostic to mention types and compound names.
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);

      return SolutionKind::Error;
    }

    NameLookupOptions lookupOptions = defaultMemberTypeLookupOptions;
    if (isa<AbstractFunctionDecl>(DC))
      lookupOptions |= NameLookupFlags::KnownPrivate;
    auto lookup = TC.lookupMemberType(DC, baseObjTy, name.getBaseName(),
                                      lookupOptions);
    if (!lookup) {
      // FIXME: Customize diagnostic to mention types.
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);

      return SolutionKind::Error;
    }

    // Form the overload set.
    SmallVector<OverloadChoice, 4> choices;
    for (auto result : lookup) {
      // If the result is invalid, skip it.
      // FIXME: Note this as invalid, in case we don't find a solution,
      // so we don't let errors cascade further.
      TC.validateDecl(result.first, true);
      if (result.first->isInvalid())
        continue;

      choices.push_back(OverloadChoice(baseTy, result.first,
                                       /*isSpecialized=*/false));
    }

    if (choices.empty()) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);
      return SolutionKind::Error;
    }

    auto locator = getConstraintLocator(constraint.getLocator());
    addOverloadSet(memberTy, choices, locator);
    return SolutionKind::Solved;
  }

  // Look for members within the base.
  LookupResult &lookup = lookupMember(baseObjTy, name);
  if (!lookup) {
    // Check whether we actually performed a lookup with an integer value.
    unsigned index;
    Type rawValueType;
    if (name.isSimpleName()
        && !name.getBaseName().str().getAsInteger(10, index)) {
      // ".0" on a scalar just refers to the underlying scalar value.
      if (index == 0) {
        OverloadChoice identityChoice(baseTy, OverloadChoiceKind::BaseType);
        addBindOverloadConstraint(memberTy, identityChoice,
                                  constraint.getLocator());
        return SolutionKind::Solved;
      }

      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);

      return SolutionKind::Error;
    } else if (shouldAttemptFixes() && name == TC.Context.Id_fromRaw && 
               (rawValueType = getRawRepresentableValueType(TC, DC,
                                                            instanceTy))) {
      // Replace a reference to ".fromRaw" with a reference to init(rawValue:).
      // FIXME: This is temporary.

      // Record this fix.
      increaseScore(SK_Fix);
      if (worseThanBestSolution())
        return SolutionKind::Error;

      auto locator = constraint.getLocator();
      Fixes.push_back({FixKind::FromRawToInit,getConstraintLocator(locator)});

      // Form the type that "fromRaw" would have had and bind the
      // member type to it.
      Type fromRawType = FunctionType::get(ParenType::get(TC.Context, 
                                                          rawValueType),
                                           OptionalType::get(instanceTy));
      addConstraint(ConstraintKind::Bind, memberTy, fromRawType, locator);
      
      return SolutionKind::Solved;
    } else if (shouldAttemptFixes() && name == TC.Context.Id_toRaw && 
               (rawValueType = getRawRepresentableValueType(TC, DC,
                                                            instanceTy))) {
      // Replace a call to "toRaw" with a reference to rawValue.
      // FIXME: This is temporary.

      // Record this fix.
      increaseScore(SK_Fix);
      if (worseThanBestSolution())
        return SolutionKind::Error;

      auto locator = constraint.getLocator();
      Fixes.push_back({FixKind::ToRawToRawValue,getConstraintLocator(locator)});

      // Form the type that "toRaw" would have had and bind the member
      // type to it.
      Type toRawType = FunctionType::get(TupleType::getEmpty(TC.Context),
                                         rawValueType);
      addConstraint(ConstraintKind::Bind, memberTy, toRawType, locator);

      return SolutionKind::Solved;
    } else if (shouldAttemptFixes() && 
               baseObjTy->getOptionalObjectType()) {
      // If the base type was an optional, look through it.

      // Note the fix.
      increaseScore(SK_Fix);
      if (worseThanBestSolution())
        return SolutionKind::Error;

      Fixes.push_back({FixKind::ForceOptional, constraint.getLocator()});

      // Look through one level of optional.
      addValueMemberConstraint(baseObjTy->getOptionalObjectType(),
                               constraint.getMember(),
                               constraint.getSecondType(),
                               constraint.getLocator());
      return SolutionKind::Solved;
    }
  }

  // The set of directly accessible types, which is only used when
  // we're performing dynamic lookup into an existential type.
  bool isDynamicLookup = instanceTy->isAnyObject();

  // Record the fact that we found a mutating function for diagnostics.
  bool FoundMutating = false;

  // Introduce a new overload set to capture the choices.
  SmallVector<OverloadChoice, 4> choices;

  // If the instance type is String bridged to NSString, compute
  // the type we'll look in for bridging.
  Type bridgedClass;
  Type bridgedType;
  if (instanceTy->getAnyNominal() == TC.Context.getStringDecl()) {
    if (Type classType = TC.getBridgedToObjC(DC, true, instanceTy)) {
      bridgedClass = classType;
      bridgedType = isMetatype ? MetatypeType::get(classType) : classType;
    }
  }

  // Local function that adds the given declaration if it is a
  // reasonable choice.
  auto addChoice = [&](ValueDecl *result, bool isBridged,
                       bool isUnwrappedOptional) {
    // If the result is invalid, skip it.
    // FIXME: Note this as invalid, in case we don't find a solution,
    // so we don't let errors cascade further.
    TC.validateDecl(result, true);
    if (result->isInvalid())
      return;

    // If our base is an existential type, we can't make use of any
    // member whose signature involves associated types.
    // FIXME: Mark this as 'unavailable'.
    if (isExistential && isUnavailableInExistential(getTypeChecker(), result))
      return;

    // If we are looking for a metatype member, don't include members that can
    // only be accessed on an instance of the object.
    // FIXME: Mark as 'unavailable' somehow.
    if (isMetatype && !(isa<FuncDecl>(result) || !result->isInstanceMember())) {
      return;
    }

    // If we aren't looking in a metatype, ignore static functions, static
    // variables, and enum elements.
    if (!isMetatype && !baseObjTy->is<ModuleType>() &&
        !result->isInstanceMember())
      return;

    // If we're doing dynamic lookup into a metatype of AnyObject and we've
    // found an instance member, ignore it.
    if (isDynamicLookup && isMetatype && result->isInstanceMember()) {
      // FIXME: Mark as 'unavailable' somehow.
      return;
    }

    // If we have an rvalue base of struct or enum type, make sure that the
    // result isn't mutating (only valid on lvalues).
    if (!isMetatype && !baseObjTy->hasReferenceSemantics() &&
        !baseTy->is<LValueType>() && result->isInstanceMember()) {
      if (auto *FD = dyn_cast<FuncDecl>(result))
        if (FD->isMutating()) {
          FoundMutating = true;
          return;
        }

      // Subscripts and computed properties are ok on rvalues so long
      // as the getter is nonmutating.
      if (auto storage = dyn_cast<AbstractStorageDecl>(result)) {
        if (storage->isGetterMutating()) {
          FoundMutating = true;
          return;
        }
      }
    }
    
    // If the result's type contains delayed members, we need to force them now.
    if (auto NT = dyn_cast<NominalType>(result->getType().getPointer())) {
      if (auto *NTD = dyn_cast<NominalTypeDecl>(NT->getDecl())) {
        TC.forceExternalDeclMembers(NTD);
      }
    }

    // If we're looking into an existential type, check whether this
    // result was found via dynamic lookup.
    if (isDynamicLookup) {
      assert(result->getDeclContext()->isTypeContext() && "Dynamic lookup bug");

      // We found this declaration via dynamic lookup, record it as such.
      choices.push_back(OverloadChoice::getDeclViaDynamic(baseTy, result));
      return;
    }

    // If we have a bridged type, we found this declaration via bridging.
    if (isBridged) {
      choices.push_back(OverloadChoice::getDeclViaBridge(bridgedType, result));
      return;
    }

    // If we got the choice by unwrapping an optional type, unwrap the base
    // type.
    Type ovlBaseTy = baseTy;
    if (isUnwrappedOptional) {
      ovlBaseTy = MetatypeType::get(baseTy->castTo<MetatypeType>()
        ->getInstanceType()
        ->getAnyOptionalObjectType());
      choices.push_back(OverloadChoice::getDeclViaUnwrappedOptional(ovlBaseTy,
                                                                    result));
    } else {
      SourceLoc anchorLoc = constraint.getLocator()->getAnchor()->getLoc();
      auto unavailReason = TC.checkDeclarationAvailability(result, anchorLoc,
                                                           DC);
      choices.push_back(OverloadChoice(ovlBaseTy, result,
                                       /*isSpecialized=*/false, *this,
                                       unavailReason));
    }
  };

  // Add all results from this lookup.
retry_after_fail:
  bool labelMismatch = false;
  for (auto result : lookup) {
    // If the argument labels for this result are incompatible with
    // the call site, skip it.
    if (!hasCompatibleArgumentLabels(result)) {
      labelMismatch = true;
      continue;
    }

    addChoice(result, /*isBridged=*/false, /*isUnwrappedOptional=*/false);
  }

  // If the instance type is a bridged to an Objective-C type, perform
  // a lookup into that Objective-C type.
  if (bridgedType) {
    LookupResult &bridgedLookup = lookupMember(bridgedClass, name);
    Module *foundationModule = nullptr;
    for (auto result : bridgedLookup) {
      // Ignore results from the Objective-C "Foundation"
      // module. Those core APIs are explicitly provided by the
      // Foundation module overlay.
      auto module = result->getModuleContext();
      if (foundationModule) {
        if (module == foundationModule)
          continue;
      } else if (ClangModuleUnit::hasClangModule(module) &&
                 module->getName().str() == "Foundation") {
        // Cache the foundation module name so we don't need to look
        // for it again.
        foundationModule = module;
        continue;
      }

      // If the argument labels for this result are incompatible with
      // the call site, skip it.
      if (!hasCompatibleArgumentLabels(result)) {
        labelMismatch = true;
        continue;
      }
      
      addChoice(result, /*isBridged=*/true, /*isUnwrappedOptional=*/false);
    }
  }
  
  // If we're looking into a metatype for an unresolved member lookup, look
  // through optional types.
  //
  // FIXME: The short-circuit here is lame.
  if (choices.empty() && isMetatype && isUnresolvedMember) {
    if (auto objectType = instanceTy->getAnyOptionalObjectType()) {
      LookupResult &optionalLookup = lookupMember(MetatypeType::get(objectType),
                                                  name);
      for (auto result : optionalLookup) {
        // If the argument labels for this result are incompatible with
        // the call site, skip it.
        if (!hasCompatibleArgumentLabels(result)) {
          labelMismatch = true;
          continue;
        }
        
        addChoice(result, /*bridged*/ false, /*isUnwrappedOptional=*/true);
      }
    }
  }

  // If we rejected some possibilities due to an argument-label
  // mismatch and ended up with nothing, try again ignoring the
  // labels. This allows us to perform typo correction on the labels.
  if (choices.empty() && labelMismatch && shouldAttemptFixes()) {
    argumentLabels.reset();
    goto retry_after_fail;
  }

  if (choices.empty()) {
    recordFailure(constraint.getLocator(),
                  FoundMutating ? Failure::DoesNotHaveNonMutatingMember :
                                  Failure::DoesNotHaveMember,
                  baseObjTy, name);
    return SolutionKind::Error;
  }
  auto locator = getConstraintLocator(constraint.getLocator());
  addOverloadSet(memberTy, choices, locator);
  return SolutionKind::Solved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyArchetypeConstraint(const Constraint &constraint) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  Type baseTy = constraint.getFirstType()->getRValueType();
  if (auto tv = dyn_cast<TypeVariableType>(baseTy.getPointer())) {
    auto fixed = getFixedType(tv);
    if (!fixed)
      return SolutionKind::Unsolved;

    // Continue with the fixed type.
    baseTy = fixed->getRValueType();
  }

  if (baseTy->is<ArchetypeType>()) {
    return SolutionKind::Solved;
  }

  // Record this failure.
  recordFailure(constraint.getLocator(), Failure::IsNotArchetype, baseTy);
  return SolutionKind::Error;
}

/// Simplify the given type for use in a type property constraint.
static Type simplifyForTypePropertyConstraint(ConstraintSystem &cs, Type type) {
  if (auto tv = type->getAs<TypeVariableType>()) {
    auto fixed = cs.getFixedType(tv);
    if (!fixed)
      return Type();

    // Continue with the fixed type.
    type = fixed;

    // Look through parentheses.
    while (auto paren = dyn_cast<ParenType>(type.getPointer()))
      type = paren->getUnderlyingType();
  }

  return type;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyClassConstraint(const Constraint &constraint){
  auto baseTy = simplifyForTypePropertyConstraint(*this,
                                                  constraint.getFirstType());
  if (!baseTy)
    return SolutionKind::Unsolved;

  if (baseTy->getClassOrBoundGenericClass())
    return SolutionKind::Solved;

  if (auto archetype = baseTy->getAs<ArchetypeType>()) {
    if (archetype->requiresClass())
      return SolutionKind::Solved;
  }

  // Record this failure.
  recordFailure(constraint.getLocator(), Failure::IsNotClass, baseTy);
  return SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyBridgedToObjectiveCConstraint(
  const Constraint &constraint)
{
  auto baseTy = simplifyForTypePropertyConstraint(*this,
                                                  constraint.getFirstType());
  if (!baseTy)
    return SolutionKind::Unsolved;
  
  if (TC.getBridgedToObjC(DC, true, baseTy)) {
    increaseScore(SK_UserConversion);
    return SolutionKind::Solved;
  }
  
  // Record this failure.
  recordFailure(constraint.getLocator(),
                Failure::IsNotBridgedToObjectiveC,
                baseTy);
  return SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyDynamicTypeOfConstraint(const Constraint &constraint) {
  // Solve forward.
  TypeVariableType *typeVar2;
  Type type2 = getFixedTypeRecursive(constraint.getSecondType(), typeVar2,
                                     /*wantRValue=*/ true);
  if (!typeVar2) {
    Type dynamicType2;
    if (type2->isAnyExistentialType()) {
      dynamicType2 = ExistentialMetatypeType::get(type2);
    } else {
      dynamicType2 = MetatypeType::get(type2);
    }
    return matchTypes(constraint.getFirstType(), dynamicType2,
                      TypeMatchKind::BindType, TMF_GenerateConstraints,
                      constraint.getLocator());
  }

  // Okay, can't solve forward.  See what we can do backwards.
  TypeVariableType *typeVar1;
  Type type1 = getFixedTypeRecursive(constraint.getFirstType(), typeVar1, true);

  // If we have an existential metatype, that's good enough to solve
  // the constraint.
  if (auto metatype1 = type1->getAs<ExistentialMetatypeType>())
    return matchTypes(metatype1->getInstanceType(), type2,
                      TypeMatchKind::BindType,
                      TMF_GenerateConstraints, constraint.getLocator());

  // If we have a normal metatype, we can't solve backwards unless we
  // know what kind of object it is.
  if (auto metatype1 = type1->getAs<MetatypeType>()) {
    TypeVariableType *instanceTypeVar1;
    Type instanceType1 = getFixedTypeRecursive(metatype1->getInstanceType(),
                                               instanceTypeVar1, true);
    if (!instanceTypeVar1) {
      return matchTypes(instanceType1, type2,
                        TypeMatchKind::BindType,
                        TMF_GenerateConstraints, constraint.getLocator());
    }

  // If it's definitely not either kind of metatype, then we can
  // report failure right away.
  } else if (!typeVar1) {
    recordFailure(constraint.getLocator(), Failure::IsNotMetatype, type1);
    return SolutionKind::Error;
  }

  return SolutionKind::Unsolved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyApplicableFnConstraint(const Constraint &constraint) {

  // By construction, the left hand side is a type that looks like the
  // following: $T1 -> $T2.
  Type type1 = constraint.getFirstType();
  assert(type1->is<FunctionType>());

  // Drill down to the concrete type on the right hand side.
  TypeVariableType *typeVar2;
  Type type2 = getFixedTypeRecursive(constraint.getSecondType(), typeVar2, 
                                     /*wantRValue=*/true);
  auto desugar2 = type2->getDesugaredType();

  // Try to look through ImplicitlyUnwrappedOptional<T>: the result is always an
  // r-value.
  if (auto objTy = lookThroughImplicitlyUnwrappedOptionalType(desugar2)) {
    type2 = objTy;
    desugar2 = type2->getDesugaredType();
  }

  // Force the right-hand side to be an rvalue.
  unsigned flags = TMF_GenerateConstraints;

  // If the types are obviously equivalent, we're done.
  if (type1.getPointer() == desugar2)
    return SolutionKind::Solved;

  // If right-hand side is a type variable, the constraint is unsolved.
  if (typeVar2)
    return SolutionKind::Unsolved;

  // Strip the 'ApplyFunction' off the locator.
  // FIXME: Perhaps ApplyFunction can go away entirely?
  ConstraintLocatorBuilder locator = constraint.getLocator();
  SmallVector<LocatorPathElt, 2> parts;
  Expr *anchor = locator.getLocatorParts(parts);
  assert(!parts.empty() && "Nonsensical applicable-function locator");
  assert(parts.back().getKind() == ConstraintLocator::ApplyFunction);
  assert(parts.back().getNewSummaryFlags() == 0);
  parts.pop_back();
  ConstraintLocatorBuilder outerLocator =
    getConstraintLocator(anchor, parts, locator.getSummaryFlags());

retry:
  // For a function, bind the output and convert the argument to the input.
  auto func1 = type1->castTo<FunctionType>();
  if (desugar2->getKind() == TypeKind::Function) {
    auto func2 = cast<FunctionType>(desugar2);

    // If this application is part of an operator, then we allow an implicit
    // lvalue to be compatible with inout arguments.  This is used by
    // assignment operators.
    TypeMatchKind ArgConv = TypeMatchKind::ArgumentTupleConversion;
    if (isa<PrefixUnaryExpr>(anchor) || isa<PostfixUnaryExpr>(anchor) ||
        isa<BinaryExpr>(anchor))
      ArgConv = TypeMatchKind::OperatorArgumentTupleConversion;
    
    // The argument type must be convertible to the input type.
    if (matchTypes(func1->getInput(), func2->getInput(),
                   ArgConv, flags,
                   outerLocator.withPathElement(
                     ConstraintLocator::ApplyArgument))
          == SolutionKind::Error)
      return SolutionKind::Error;

    // The result types are equivalent.
    if (matchTypes(func1->getResult(), func2->getResult(),
                   TypeMatchKind::BindType,
                   flags,
                   locator.withPathElement(ConstraintLocator::FunctionResult))
          == SolutionKind::Error)
      return SolutionKind::Error;

    // If our type constraints is for a FunctionType, move over the @noescape
    // flag.
    if (func1->isNoEscape() && !func2->isNoEscape()) {
      auto &extraExtInfo = extraFunctionAttrs[func2];
      extraExtInfo = extraExtInfo.withNoEscape();
    }
    return SolutionKind::Solved;
  }

  // For a metatype, perform a construction.
  if (auto meta2 = dyn_cast<AnyMetatypeType>(desugar2)) {
    // Construct the instance from the input arguments.
    return simplifyConstructionConstraint(meta2->getInstanceType(), func1,
                                          flags, 
                                          getConstraintLocator(outerLocator));
  }

  // If we're coming from an optional type, unwrap the optional and try again.
  if (auto objectType2 = desugar2->getOptionalObjectType()) {
    // Increase the score before we attempt a fix.
    increaseScore(SK_Fix);
    if (worseThanBestSolution())
      return SolutionKind::Error;

    Fixes.push_back({FixKind::ForceOptional,getConstraintLocator(locator)});

    type2 = objectType2;
    desugar2 = type2->getDesugaredType();
    goto retry;
  }

  // If this is a '()' call, drop the call.
  if (shouldAttemptFixes() &&
      func1->getInput()->isEqual(TupleType::getEmpty(getASTContext()))) {
    // Increase the score before we attempt a fix.
    increaseScore(SK_Fix);
    if (worseThanBestSolution())
      return SolutionKind::Error;

    // We don't bother with a 'None' case, because at this point we won't get
    // a better diagnostic from that case.
    Fixes.push_back({FixKind::RemoveNullaryCall,
                     getConstraintLocator(locator)});

    return matchTypes(func1->getResult(), type2,
                      TypeMatchKind::BindType,
                      flags | TMF_ApplyingFix | TMF_GenerateConstraints,
                      locator.withPathElement(
                        ConstraintLocator::FunctionResult));
  }

  // If we are supposed to record failures, do so.
  if (shouldRecordFailures()) {
    recordFailure(getConstraintLocator(locator),
                  Failure::FunctionTypesMismatch,
                  type1, type2);
  }

  return SolutionKind::Error;
}

/// \brief Retrieve the type-matching kind corresponding to the given
/// constraint kind.
static TypeMatchKind getTypeMatchKind(ConstraintKind kind) {
  switch (kind) {
  case ConstraintKind::Bind: return TypeMatchKind::BindType;
  case ConstraintKind::Equal: return TypeMatchKind::SameType;
  case ConstraintKind::Subtype: return TypeMatchKind::Subtype;
  case ConstraintKind::Conversion: return TypeMatchKind::Conversion;
  case ConstraintKind::ExplicitConversion:
    return TypeMatchKind::ExplicitConversion;
  case ConstraintKind::ArgumentConversion:
    return TypeMatchKind::ArgumentConversion;
  case ConstraintKind::ArgumentTupleConversion:
    return TypeMatchKind::ArgumentTupleConversion;
  case ConstraintKind::OperatorArgumentTupleConversion:
    return TypeMatchKind::OperatorArgumentTupleConversion;
  case ConstraintKind::OperatorArgumentConversion:
    return TypeMatchKind::OperatorArgumentConversion;

  case ConstraintKind::ApplicableFunction:
    llvm_unreachable("ApplicableFunction constraints don't involve "
                     "type matches");

  case ConstraintKind::DynamicTypeOf:
    llvm_unreachable("DynamicTypeOf constraints don't involve type matches");

  case ConstraintKind::BindOverload:
    llvm_unreachable("Overload binding constraints don't involve type matches");

  case ConstraintKind::ConformsTo:
  case ConstraintKind::SelfObjectOfProtocol:
    llvm_unreachable("Conformance constraints don't involve type matches");

  case ConstraintKind::CheckedCast:
    llvm_unreachable("Checked cast constraints don't involve type matches");

  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::TypeMember:
    llvm_unreachable("Member constraints don't involve type matches");

  case ConstraintKind::OptionalObject:
    llvm_unreachable("optional object constraints don't involve type matches");
      
  case ConstraintKind::Archetype:
  case ConstraintKind::Class:
  case ConstraintKind::BridgedToObjectiveC:
    llvm_unreachable("Type properties don't involve type matches");

  case ConstraintKind::Conjunction:
  case ConstraintKind::Disjunction:
    llvm_unreachable("Con/disjunction constraints don't involve type matches");
  }
}

Type ConstraintSystem::getBaseTypeForArrayType(TypeBase *type) {
  type = type->lookThroughAllAnyOptionalTypes().getPointer();
  
  if (auto bound = type->getAs<BoundGenericStructType>()) {
    if (bound->getDecl() == getASTContext().getArrayDecl()) {
      return bound->getGenericArgs()[0];
    }
  }

  type->dump();
  llvm_unreachable("attempted to extract a base type from a non-array type");
}

Type ConstraintSystem::getBaseTypeForSetType(TypeBase *type) {
  type = type->lookThroughAllAnyOptionalTypes().getPointer();

  if (auto bound = type->getAs<BoundGenericStructType>()) {
    if (bound->getDecl() == getASTContext().getSetDecl()) {
      return bound->getGenericArgs()[0];
    }
  }

  type->dump();
  llvm_unreachable("attempted to extract a base type from a non-set type");
}

Type ConstraintSystem::getTypeWhenUnavailable(Type declType) {
  if (!TC.getLangOpts().EnableExperimentalUnavailableAsOptional) {
    return declType;
  }
  // Drop lvalue-ness and make optional.
  return OptionalType::get(declType->getRValueType());
}

static Type getBaseTypeForPointer(ConstraintSystem &cs, TypeBase *type) {
  auto bgt = type->castTo<BoundGenericType>();
  assert((bgt->getDecl() == cs.getASTContext().getUnsafeMutablePointerDecl()
          || bgt->getDecl() == cs.getASTContext().getUnsafePointerDecl()
          || bgt->getDecl()
          == cs.getASTContext().getAutoreleasingUnsafeMutablePointerDecl())
         && "conversion is not to a pointer type");
  return bgt->getGenericArgs()[0];
}

/// Given that we have a conversion constraint between two types, and
/// that the given constraint-reduction rule applies between them at
/// the top level, apply it and generate any necessary recursive
/// constraints.
ConstraintSystem::SolutionKind
ConstraintSystem::simplifyRestrictedConstraint(ConversionRestrictionKind restriction,
                                               Type type1, Type type2,
                                               TypeMatchKind matchKind,
                                               unsigned flags,
                                               ConstraintLocatorBuilder locator) {
  // Add to the score based on context.
  auto addContextualScore = [&] {
    // Okay, we need to perform one or more conversions.  If this
    // conversion will cause a function conversion, score it as worse.
    // This induces conversions to occur within closures instead of
    // outside of them wherever possible.
    if (locator.isFunctionConversion()) {
      increaseScore(SK_FunctionConversion);
    }
  };
  
  // We'll apply user conversions for operator arguments at the application
  // site.
  if (matchKind == TypeMatchKind::OperatorArgumentConversion) {
    flags |= TMF_ApplyingOperatorParameter;
  }

  unsigned subFlags = flags | TMF_GenerateConstraints;

  switch (restriction) {
  // for $< in { <, <c, <oc }:
  //   T_i $< U_i ===> (T_i...) $< (U_i...)
  case ConversionRestrictionKind::TupleToTuple:
    return matchTupleTypes(type1->castTo<TupleType>(),
                           type2->castTo<TupleType>(),
                           matchKind, subFlags, locator);

  //   T <c U ===> T <c (U)
  case ConversionRestrictionKind::ScalarToTuple:
    return matchScalarToTupleTypes(type1,
                                   type2->castTo<TupleType>(),
                                   matchKind, subFlags, locator);

  // for $< in { <, <c, <oc }:
  //   T $< U ===> (T) $< U
  case ConversionRestrictionKind::TupleToScalar:
    return matchTupleToScalarTypes(type1->castTo<TupleType>(),
                                   type2,
                                   matchKind, subFlags, locator);

  case ConversionRestrictionKind::DeepEquality:
    return matchDeepEqualityTypes(type1, type2, locator);

  case ConversionRestrictionKind::Superclass:
    addContextualScore();
    return matchSuperclassTypes(type1, type2, matchKind, subFlags, locator);

  case ConversionRestrictionKind::LValueToRValue:
    return matchTypes(type1->getRValueType(), type2,
                      matchKind, subFlags, locator);

  // for $< in { <, <c, <oc }:
  //   T $< U, U : P_i ===> T $< protocol<P_i...>
  case ConversionRestrictionKind::Existential:
    addContextualScore();
    return matchExistentialTypes(type1, type2,
                                 matchKind, subFlags, locator);

  // for $< in { <, <c, <oc }:
  //   T $< U ===> T $< U?
  case ConversionRestrictionKind::ValueToOptional: {
    addContextualScore();
    increaseScore(SK_ValueToOptional);

    assert(matchKind >= TypeMatchKind::Subtype);
    auto generic2 = type2->castTo<BoundGenericType>();
    assert(generic2->getDecl()->classifyAsOptionalType());
    return matchTypes(type1, generic2->getGenericArgs()[0],
                      matchKind, (subFlags | TMF_UnwrappingOptional), locator);
  }

  // for $< in { <, <c, <oc }:
  //   T $< U ===> T? $< U?
  //   T $< U ===> T! $< U!
  //   T $< U ===> T! $< U?
  // also:
  //   T <c U ===> T? <c U!
  case ConversionRestrictionKind::OptionalToImplicitlyUnwrappedOptional:
  case ConversionRestrictionKind::ImplicitlyUnwrappedOptionalToOptional:
  case ConversionRestrictionKind::OptionalToOptional: {
    addContextualScore();
    assert(matchKind >= TypeMatchKind::Subtype);
    auto generic1 = type1->castTo<BoundGenericType>();
    auto generic2 = type2->castTo<BoundGenericType>();
    assert(generic1->getDecl()->classifyAsOptionalType());
    assert(generic2->getDecl()->classifyAsOptionalType());
    return matchTypes(generic1->getGenericArgs()[0],
                      generic2->getGenericArgs()[0],
                      matchKind, subFlags,
                      locator.withPathElement(
                        LocatorPathElt::getGenericArgument(0)));
  }

  // T <c U ===> T! <c U
  //
  // We don't want to allow this after user-defined conversions:
  //   - it gets really complex for users to understand why there's
  //     a dereference in their code
  //   - it would allow nil to be coerceable to a non-optional type
  // Fortunately, user-defined conversions only allow subtype
  // conversions on their results.
  case ConversionRestrictionKind::ForceUnchecked: {
    addContextualScore();
    assert(matchKind >= TypeMatchKind::Conversion);
    auto boundGenericType1 = type1->castTo<BoundGenericType>();
    assert(boundGenericType1->getDecl()->classifyAsOptionalType()
             == OTK_ImplicitlyUnwrappedOptional);
    assert(boundGenericType1->getGenericArgs().size() == 1);
    Type valueType1 = boundGenericType1->getGenericArgs()[0];
    increaseScore(SK_ForceUnchecked);
    return matchTypes(valueType1, type2,
                      matchKind, subFlags,
                      locator.withPathElement(
                        LocatorPathElt::getGenericArgument(0)));
  }
      
  case ConversionRestrictionKind::ClassMetatypeToAnyObject:
  case ConversionRestrictionKind::ExistentialMetatypeToAnyObject:
  case ConversionRestrictionKind::ProtocolMetatypeToProtocolClass: {
    // Nothing more to solve.
    addContextualScore();
    return SolutionKind::Solved;
  }
  
  // T <p U ===> T[] <a UnsafeMutablePointer<U>
  case ConversionRestrictionKind::ArrayToPointer: {
    addContextualScore();
    auto obj1 = type1;
    // Unwrap an inout type.
    if (auto inout1 = obj1->getAs<InOutType>()) {
      obj1 = inout1->getObjectType();
    }
    
    TypeVariableType *tv1;
    obj1 = getFixedTypeRecursive(obj1, tv1, false, false);
    
    auto t1 = obj1->getDesugaredType();
    auto t2 = type2->getDesugaredType();
    
    auto baseType1 = getBaseTypeForArrayType(t1);
    auto baseType2 = getBaseTypeForPointer(*this, t2);

    return matchTypes(baseType1, baseType2,
                      TypeMatchKind::BindToPointerType,
                      subFlags, locator);
  }
      
  // String ===> UnsafePointer<[U]Int8>
  case ConversionRestrictionKind::StringToPointer: {
    addContextualScore();

    auto baseType2 = getBaseTypeForPointer(*this, type2->getDesugaredType());
    
    // The pointer element type must be void or a byte-sized type.
    // TODO: Handle different encodings based on pointer element type, such as
    // UTF16 for [U]Int16 or UTF32 for [U]Int32. For now we only interop with
    // Int8 pointers using UTF8 encoding.
    TypeVariableType *btv2 = nullptr;
    baseType2 = getFixedTypeRecursive(baseType2, btv2, false, false);
    // If we haven't resolved the element type, generate constraints.
    if (btv2) {
      if (flags & TMF_GenerateConstraints) {
        auto int8Con = Constraint::create(*this, ConstraintKind::Bind,
                                       btv2, TC.getInt8Type(DC),
                                       DeclName(),
                                       getConstraintLocator(locator));
        auto uint8Con = Constraint::create(*this, ConstraintKind::Bind,
                                        btv2, TC.getUInt8Type(DC),
                                        DeclName(),
                                        getConstraintLocator(locator));
        auto voidCon = Constraint::create(*this, ConstraintKind::Bind,
                                        btv2, TC.Context.TheEmptyTupleType,
                                        DeclName(),
                                        getConstraintLocator(locator));
        
        Constraint *disjunctionChoices[] = {int8Con, uint8Con, voidCon};
        auto disjunction = Constraint::createDisjunction(*this,
                                                 disjunctionChoices,
                                                 getConstraintLocator(locator));
        
        addConstraint(disjunction);
        return SolutionKind::Solved;
      }

      return SolutionKind::Unsolved;
    }
    
    if (!isStringCompatiblePointerBaseType(TC, DC, baseType2)) {
      return SolutionKind::Unsolved;
    }
    return SolutionKind::Solved;
  }
      
  // T <p U ===> inout T <a UnsafeMutablePointer<U>
  case ConversionRestrictionKind::InoutToPointer: {
    addContextualScore();

    auto t2 = type2->getDesugaredType();
    
    auto baseType1 = type1->getInOutObjectType();
    auto baseType2 = getBaseTypeForPointer(*this, t2);
    
    // Set up the disjunction for the array or scalar cases.

    return matchTypes(baseType1, baseType2,
                      TypeMatchKind::BindToPointerType,
                      subFlags, locator);
  }
      
  // T <p U ===> UnsafeMutablePointer<T> <a UnsafeMutablePointer<U>
  case ConversionRestrictionKind::PointerToPointer: {
    auto t1 = type1->getDesugaredType();
    auto t2 = type2->getDesugaredType();
    
    Type baseType1 = getBaseTypeForPointer(*this, t1);
    Type baseType2 = getBaseTypeForPointer(*this, t2);
    
    return matchTypes(baseType1, baseType2,
                      TypeMatchKind::BindToPointerType,
                      subFlags, locator);
  }
    
  // T < U or T is bridged to V where V < U ===> Array<T> <c Array<U>
  case ConversionRestrictionKind::ArrayUpcast: {
    auto t1 = type1->getDesugaredType();
    auto t2 = type2->getDesugaredType();
    
    Type baseType1 = getBaseTypeForArrayType(t1);
    Type baseType2 = getBaseTypeForArrayType(t2);

    // Look through type variables in the first element type; we need to know
    // it's structure before we can decide whether this can be an array upcast.
    TypeVariableType *baseTypeVar1;
    baseType1 = getFixedTypeRecursive(baseType1, baseTypeVar1, false, false);
    if (baseTypeVar1) {
      if (flags & TMF_GenerateConstraints) {
        addConstraint(
          Constraint::createRestricted(*this, getConstraintKind(matchKind),
                                       restriction, type1, type2,
                                       getConstraintLocator(locator)));
        return SolutionKind::Solved;
      }

      return SolutionKind::Unsolved;
    }

    bool isSimpleUpcast = baseType1->isBridgeableObjectType();
    if (isSimpleUpcast) {
      increaseScore(SK_CollectionUpcastConversion);
    } else {
      // Check whether this is a bridging upcast.
      Type bridgedType1 = TC.getBridgedToObjC(DC, true, baseType1);
      if (!bridgedType1) {
        // FIXME: Record error.
        return SolutionKind::Error;
      }

      // Replace the base type so we perform the appropriate subtype check.
      baseType1 = bridgedType1;
      increaseScore(SK_CollectionBridgedConversion);
    }

    addContextualScore();
    assert(matchKind >= TypeMatchKind::Conversion);

    return matchTypes(baseType1,
                      baseType2,
                      TypeMatchKind::Subtype,
                      subFlags,
                      locator);
  }

  // K1 < K2 && V1 < V2 || K1 bridges to K2 && V1 bridges to V2 ===> 
  //   Dictionary<K1, V1> <c Dictionary<K2, V2>
  case ConversionRestrictionKind::DictionaryUpcast: {
    auto t1 = type1->getDesugaredType();    
    Type key1, value1;
    std::tie(key1, value1) = *isDictionaryType(t1);

    auto t2 = type2->getDesugaredType();
    Type key2, value2;
    std::tie(key2, value2) = *isDictionaryType(t2);

    // Look through type variables in the first key and value type; we
    // need to know their structure before we can decide whether this
    // can be an upcast.
    TypeVariableType *keyTypeVar1 = nullptr;
    TypeVariableType *valueTypeVar1 = nullptr;    
    key1 = getFixedTypeRecursive(key1, keyTypeVar1, false, false);
    if (!keyTypeVar1) {
      value1 = getFixedTypeRecursive(value1, valueTypeVar1, false, false);
    }

    if (keyTypeVar1 || valueTypeVar1) {
      if (flags & TMF_GenerateConstraints) {
        addConstraint(
          Constraint::createRestricted(*this, getConstraintKind(matchKind),
                                       restriction, type1, type2,
                                       getConstraintLocator(locator)));
        return SolutionKind::Solved;
      }

      return SolutionKind::Unsolved;
    }

    // If both the first key and value types are bridgeable object
    // types, this can only be an upcast.
    bool isUpcast = key1->isBridgeableObjectType() && 
                    value1->isBridgeableObjectType();

    if (isUpcast) {
      increaseScore(SK_CollectionUpcastConversion);
    } else {
      // This might be a bridged upcast.
      Type bridgedKey1 = TC.getBridgedToObjC(DC, true, key1);
      Type bridgedValue1;
      if (bridgedKey1) {
        bridgedValue1 = TC.getBridgedToObjC(DC, true, value1);
      }

      // Both the key and value types need to be bridged.
      if (!bridgedKey1 || !bridgedValue1) {
        // FIXME: Record failure.
        return SolutionKind::Error;
      }

      // Look through the destination key and value types. We need
      // their structure to determine whether we'll be bridging or
      // upcasting the key and value.
      TypeVariableType *keyTypeVar2 = nullptr;
      TypeVariableType *valueTypeVar2 = nullptr;    
      key2 = getFixedTypeRecursive(key2, keyTypeVar2, false, false);
      if (!keyTypeVar2) {
        value2 = getFixedTypeRecursive(value2, valueTypeVar2, false, false);
      }
      
      // If either the destination key or value type is a type
      // variable, we can't simplify this now.
      if (keyTypeVar2 || valueTypeVar2) {
        if (flags & TMF_GenerateConstraints) {
          addConstraint(
            Constraint::createRestricted(*this, getConstraintKind(matchKind),
                                         restriction, type1, type2,
                                         getConstraintLocator(locator)));
          return SolutionKind::Solved;
        }
        
        return SolutionKind::Unsolved;
      }

      // This can be a bridging upcast.
      if (key2->isBridgeableObjectType())
        key1 = bridgedKey1;
      if (value2->isBridgeableObjectType())
        value1 = bridgedValue1;
      increaseScore(SK_CollectionBridgedConversion);
    }

    addContextualScore();
    assert(matchKind >= TypeMatchKind::Conversion);

    // The source key and value types must be subtypes of the destination
    // key and value types, respectively.
    auto result = matchTypes(key1, key2, TypeMatchKind::Subtype, subFlags, 
                             locator);
    if (result == SolutionKind::Error)
      return result;

    switch (matchTypes(value1, value2, TypeMatchKind::Subtype, subFlags, 
                       locator)) {
    case SolutionKind::Solved:
      return result;

    case SolutionKind::Unsolved:
      return SolutionKind::Unsolved;

    case SolutionKind::Error:
      return SolutionKind::Error;
    }
  }

  // T1 < T2 || T1 bridges to T2 ===> Set<T1> <c Set<T2>
  case ConversionRestrictionKind::SetUpcast: {
    auto t1 = type1->getDesugaredType();    
    Type baseType1 = getBaseTypeForSetType(t1);

    auto t2 = type2->getDesugaredType();
    Type baseType2 = getBaseTypeForSetType(t2);

    // Look through type variables in the first base type; we need to know
    // their structure before we can decide whether this can be an upcast.
    TypeVariableType *baseTypeVar1 = nullptr;
    baseType1 = getFixedTypeRecursive(baseType1, baseTypeVar1, false, false);

    if (baseTypeVar1) {
      if (flags & TMF_GenerateConstraints) {
        addConstraint(
          Constraint::createRestricted(*this, getConstraintKind(matchKind),
                                       restriction, type1, type2,
                                       getConstraintLocator(locator)));
        return SolutionKind::Solved;
      }

      return SolutionKind::Unsolved;
    }

    // If the first base type is a bridgeable object type, this can only be an
    // upcast.
    bool isUpcast = baseType1->isBridgeableObjectType();

    if (isUpcast) {
      increaseScore(SK_CollectionUpcastConversion);
    } else {
      // This might be a bridged upcast.
      Type bridgedBaseType1 = TC.getBridgedToObjC(DC, true, baseType1);
      if (!bridgedBaseType1) {
        // FIXME: Record failure.
        return SolutionKind::Error;
      }

      // Look through the destination base type. We need its structure to
      // determine whether we'll be bridging or upcasting the base type.
      TypeVariableType *baseTypeVar2 = nullptr;
      baseType2 = getFixedTypeRecursive(baseType2, baseTypeVar2, false, false);
      
      // If the destination base type is a type variable, we can't simplify
      // this now.
      if (baseTypeVar2) {
        if (flags & TMF_GenerateConstraints) {
          addConstraint(
            Constraint::createRestricted(*this, getConstraintKind(matchKind),
                                         restriction, type1, type2,
                                         getConstraintLocator(locator)));
          return SolutionKind::Solved;
        }
        
        return SolutionKind::Unsolved;
      }

      // This can be a bridging upcast.
      if (baseType2->isBridgeableObjectType())
        baseType1 = bridgedBaseType1;
      increaseScore(SK_CollectionBridgedConversion);
    }

    addContextualScore();
    assert(matchKind >= TypeMatchKind::Conversion);

    // The source base type must be a subtype of the destination base type.
    return matchTypes(baseType1, baseType2, TypeMatchKind::Subtype, subFlags,
                      locator);
  }

  // T bridges to C and C < U ===> T <c U
  case ConversionRestrictionKind::BridgeToObjC: {
    auto objcClass = TC.getBridgedToObjC(DC, true, type1);
    assert(objcClass && "type is not bridged to Objective-C?");
    addContextualScore();
    increaseScore(SK_UserConversion); // FIXME: Use separate score kind?
    if (worseThanBestSolution()) {
      return SolutionKind::Error;
    }

    // If the bridged value type is generic, the generic arguments
    // must be bridged to Objective-C for bridging to succeed.
    if (auto bgt1 = type1->getAs<BoundGenericType>()) {
      unsigned argIdx = 0;
      for (auto arg: bgt1->getGenericArgs()) {
        addConstraint(
          Constraint::create(*this,
                             ConstraintKind::BridgedToObjectiveC,
                             arg, Type(), DeclName(),
                             getConstraintLocator(
                               locator.withPathElement(
                                 LocatorPathElt::getGenericArgument(
                                   argIdx++)))));
      }
    }

    return matchTypes(objcClass, type2, TypeMatchKind::Subtype, subFlags,
                      locator);
  }

  // U bridges to C and T < C ===> T <c U
  case ConversionRestrictionKind::BridgeFromObjC: {
    auto objcClass = TC.getBridgedToObjC(DC, true, type2);
    assert(objcClass && "type is not bridged to Objective-C?");
    addContextualScore();
    increaseScore(SK_UserConversion); // FIXME: Use separate score kind?
    if (worseThanBestSolution()) {
      return SolutionKind::Error;
    }

    // If the bridged value type is generic, the generic arguments
    // must match the 
    // FIXME: This should be an associated type of the protocol.
    if (auto bgt1 = type2->getAs<BoundGenericType>()) {
      if (bgt1->getDecl() == TC.Context.getArrayDecl()) {
        // [AnyObject]
        addConstraint(ConstraintKind::Bind, bgt1->getGenericArgs()[0],
                      TC.Context.getProtocol(KnownProtocolKind::AnyObject)
                        ->getDeclaredType(),
                      getConstraintLocator(
                        locator.withPathElement(
                          LocatorPathElt::getGenericArgument(0))));
      } else if (bgt1->getDecl() == TC.Context.getDictionaryDecl()) {
        // [NSObject : AnyObject]
        auto NSObjectType = TC.getNSObjectType(DC);
        if (!NSObjectType) {
          // Not a bridging case. Should we detect this earlier?
          return SolutionKind::Error;
        }

        addConstraint(ConstraintKind::Bind, bgt1->getGenericArgs()[0],
                      NSObjectType,
                      getConstraintLocator(
                        locator.withPathElement(
                          LocatorPathElt::getGenericArgument(0))));

        addConstraint(ConstraintKind::Bind, bgt1->getGenericArgs()[1],
                      TC.Context.getProtocol(KnownProtocolKind::AnyObject)
                        ->getDeclaredType(),
                      getConstraintLocator(
                        locator.withPathElement(
                          LocatorPathElt::getGenericArgument(1))));
      } else if (bgt1->getDecl() == TC.Context.getSetDecl()) {
        auto NSObjectType = TC.getNSObjectType(DC);
        if (!NSObjectType) {
          // Not a bridging case. Should we detect this earlier?
          return SolutionKind::Error;
        }
        addConstraint(ConstraintKind::Bind, bgt1->getGenericArgs()[0],
                      NSObjectType,
                      getConstraintLocator(
                        locator.withPathElement(
                          LocatorPathElt::getGenericArgument(0))));
      } else {
        llvm_unreachable("unhandled generic bridged type");
      }
    }

    return matchTypes(type1, objcClass, TypeMatchKind::Subtype, subFlags,
                      locator);
  }

  case ConversionRestrictionKind::BridgeToNSError: {
    increaseScore(SK_UserConversion); // FIXME: Use separate score kind?
    
    // The input type must be an ErrorType subtype.
    auto errorType = TC.Context.getProtocol(KnownProtocolKind::ErrorType)
      ->getDeclaredType();
    return matchTypes(type1, errorType, TypeMatchKind::Subtype, subFlags,
                      locator);
  }

  // T' < U and T a toll-free-bridged to T' ===> T' <c U
  case ConversionRestrictionKind::CFTollFreeBridgeToObjC: {
    increaseScore(SK_UserConversion); // FIXME: Use separate score kind?
    if (worseThanBestSolution()) {
      return SolutionKind::Error;
    }

    auto nativeClass = type1->getClassOrBoundGenericClass();
    auto bridgedObjCClass
      = nativeClass->getAttrs().getAttribute<ObjCBridgedAttr>()->getObjCClass();

    return matchTypes(bridgedObjCClass->getDeclaredInterfaceType(),
                      type2, TypeMatchKind::Subtype, subFlags, locator);
  }

  // T < U' and U a toll-free-bridged to U' ===> T <c U
  case ConversionRestrictionKind::ObjCTollFreeBridgeToCF: {
    increaseScore(SK_UserConversion); // FIXME: Use separate score kind?
    if (worseThanBestSolution()) {
      return SolutionKind::Error;
    }

    auto nativeClass = type2->getClassOrBoundGenericClass();
    auto bridgedObjCClass
      = nativeClass->getAttrs().getAttribute<ObjCBridgedAttr>()->getObjCClass();

    return matchTypes(type1,
                      bridgedObjCClass->getDeclaredInterfaceType(),
                      TypeMatchKind::Subtype, subFlags, locator);
  }
  }
  
  llvm_unreachable("bad conversion restriction");
}

bool ConstraintSystem::recordFix(Fix fix, ConstraintLocatorBuilder locator) {
  auto &ctx = getASTContext();
  if (ctx.LangOpts.DebugConstraintSolver) {
    auto &log = ctx.TypeCheckerDebug->getStream();
    log.indent(solverState? solverState->depth * 2 + 2 : 0)
      << "(attempting fix ";
    fix.print(log, this);
    log << " @";
    getConstraintLocator(locator)->dump(&ctx.SourceMgr, log);
    log << ")\n";
  }

  // Record the fix.
  if (fix.getKind() != FixKind::None) {
    Fixes.push_back({fix, getConstraintLocator(locator)});

    // Increase the score. If this would make the current solution worse than
    // the best solution we've seen already, stop now.
    increaseScore(SK_Fix);
    if (worseThanBestSolution())
      return true;
  }
  return false;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyFixConstraint(Fix fix,
                                        Type type1, Type type2,
                                        TypeMatchKind matchKind,
                                        unsigned flags,
                                        ConstraintLocatorBuilder locator) {
  if (recordFix(fix, locator)) {
    return SolutionKind::Error;
  }

  // Try with the fix.
  unsigned subFlags = flags | TMF_ApplyingFix | TMF_GenerateConstraints;
  switch (fix.getKind()) {
  case FixKind::None:
    return matchTypes(type1, type2, matchKind, subFlags, locator);

  case FixKind::NullaryCall: {
    // Assume that '()' was applied to the first type.

    // If the function was actually in a tuple, tuple'ify the
    // FIXME: This is yet another awful hack due to one-element tuples.
    auto funcTy = type1->getRValueType();
    Identifier nameToAdd;
    if (auto tupleTy = funcTy->getAs<TupleType>()) {
      int scalarIdx = tupleTy->getElementForScalarInit();
      nameToAdd = tupleTy->getElement(scalarIdx).getName();
      funcTy = tupleTy->getElementType(scalarIdx);
    }

    if (auto optObjectTy = funcTy->getAnyOptionalObjectType())
      funcTy = optObjectTy;

    auto resultTy = funcTy->castTo<AnyFunctionType>()->getResult();
    if (!nameToAdd.empty())
      resultTy = TupleType::get(TupleTypeElt(resultTy, nameToAdd),
                                  getASTContext());
    return matchTypes(resultTy, type2, matchKind, subFlags, locator);
  }

  case FixKind::RemoveNullaryCall:
    llvm_unreachable("Always applied directly");

  case FixKind::ForceOptional:
    // Assume that '!' was applied to the first type.
    return matchTypes(type1->getRValueType()->getOptionalObjectType(), type2,
                      matchKind, subFlags, locator);

  case FixKind::ForceDowncast:
    // These work whenever they are suggested.
    return SolutionKind::Solved;

  case FixKind::AddressOf:
    // Assume that '&' was applied to the first type, turning an lvalue into
    // an inout.
    return matchTypes(InOutType::get(type1->getRValueType()), type2,
                      matchKind, subFlags, locator);

  case FixKind::TupleToScalar:
    return matchTypes(type1->castTo<TupleType>()->getElementType(0),
                      type2, matchKind, subFlags, 
                      locator.withPathElement(
                        LocatorPathElt::getTupleElement(0)));

  case FixKind::ScalarToTuple: {
    auto tuple2 = type2->castTo<TupleType>();
    int scalarFieldIdx = tuple2->getElementForScalarInit();
    assert(scalarFieldIdx >= 0 && "Invalid tuple for scalar-to-tuple");
    const auto &elt = tuple2->getElement(scalarFieldIdx);
    auto scalarFieldTy = elt.isVararg()? elt.getVarargBaseTy() : elt.getType();
    return matchTypes(type1, scalarFieldTy, matchKind, subFlags,
                      locator.withPathElement(
                        ConstraintLocator::ScalarToTuple));
  }

  case FixKind::RelabelCallTuple:
  case FixKind::OptionalToBoolean:
    // The actual semantics are handled elsewhere.
    return SolutionKind::Solved;

  case FixKind::FromRawToInit:
  case FixKind::ToRawToRawValue:
  case FixKind::CoerceToCheckedCast:
    llvm_unreachable("handled elsewhere");
  }
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstraint(const Constraint &constraint) {
  switch (constraint.getKind()) {
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::ExplicitConversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::ArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentConversion: {
    // For relational constraints, match up the types.
    auto matchKind = getTypeMatchKind(constraint.getKind());

    // If there is a fix associated with this constraint, apply it before
    // we continue.
    if (auto fix = constraint.getFix()) {
      return simplifyFixConstraint(*fix, constraint.getFirstType(),
                                   constraint.getSecondType(), matchKind,
                                   TMF_GenerateConstraints,
                                   constraint.getLocator());
    }


    // If there is a restriction on this constraint, apply it directly rather
    // than going through the general \c matchTypes() machinery.
    if (auto restriction = constraint.getRestriction()) {
      SolutionKind result = simplifyRestrictedConstraint(*restriction,
                                                         constraint.getFirstType(),
                                                         constraint.getSecondType(),
                                                         matchKind,
                                                         0,
                                                         constraint.getLocator());

      // If we actually solved something, record what we did.
      switch(result) {
      case SolutionKind::Error:
      case SolutionKind::Unsolved:
        break;

      case SolutionKind::Solved:
        ConstraintRestrictions.push_back(
              std::make_tuple(constraint.getFirstType(),
                              constraint.getSecondType(), *restriction));
        break;
      }

      return result;
    }

    return matchTypes(constraint.getFirstType(), constraint.getSecondType(),
                      matchKind,
                      TMF_None, constraint.getLocator());
  }

  case ConstraintKind::ApplicableFunction:
    return simplifyApplicableFnConstraint(constraint);

  case ConstraintKind::DynamicTypeOf:
    return simplifyDynamicTypeOfConstraint(constraint);

  case ConstraintKind::BindOverload:
    resolveOverload(constraint.getLocator(), constraint.getFirstType(),
                    constraint.getOverloadChoice());
    return SolutionKind::Solved;

  case ConstraintKind::ConformsTo:
  case ConstraintKind::SelfObjectOfProtocol:
    return simplifyConformsToConstraint(
             constraint.getFirstType(),
             constraint.getProtocol(),
             constraint.getLocator(),
             TMF_None,
             constraint.getKind() == ConstraintKind::SelfObjectOfProtocol);

  case ConstraintKind::CheckedCast: {
    auto result = simplifyCheckedCastConstraint(constraint.getFirstType(),
                                                constraint.getSecondType(),
                                                constraint.getLocator());
    // NOTE: simplifyCheckedCastConstraint() may return Unsolved, e.g. if the
    // subexpression's type is unresolved. Don't record the fix until we
    // successfully simplify the constraint.
    if (result == SolutionKind::Solved) {
      if (auto fix = constraint.getFix()) {
        if (recordFix(*fix, constraint.getLocator())) {
          return SolutionKind::Error;
        }
      }
    }
    return result;
  }

  case ConstraintKind::OptionalObject:
    return simplifyOptionalObjectConstraint(constraint);
      
  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::TypeMember:
    return simplifyMemberConstraint(constraint);

  case ConstraintKind::Archetype:
    return simplifyArchetypeConstraint(constraint);
  
  case ConstraintKind::BridgedToObjectiveC:
    return simplifyBridgedToObjectiveCConstraint(constraint);

  case ConstraintKind::Class:
    return simplifyClassConstraint(constraint);

  case ConstraintKind::Conjunction:
    // Process all of the constraints in the conjunction.
    for (auto con : constraint.getNestedConstraints()) {
      addConstraint(con);
      if (failedConstraint)
        return SolutionKind::Error;
    }
    return SolutionKind::Solved;

  case ConstraintKind::Disjunction:
    // Disjunction constraints are never solved here.
    return SolutionKind::Unsolved;
  }
}
