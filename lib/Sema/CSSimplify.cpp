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

using namespace swift;
using namespace constraints;

static bool hasMandatoryTupleLabels(const ConstraintLocatorBuilder &locator) {
  if (Expr *e = locator.trySimplifyToExpr())
    return hasMandatoryTupleLabels(e);
  return false;
}

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
    return Nothing;

  // The distance can be zero due to the "with" transformation above.
  if (dist == 0)
    return 1;

  // Only allow about one typo for every two properly-typed characters, which
  // prevents completely-wacky suggestions in many cases.
  if (dist > (argName.size() + 1) / 3)
    return Nothing;

  return dist;
}

/// Function that determines whether the parameter at the given
/// index can be treated as a trailing closure.
///
/// FIXME: This is the wrong question to ask. We should know when something is
/// a trailing closure.
static bool paramIsTrailingClosure(ArrayRef<TupleTypeElt> paramTuple,
                                   unsigned paramIdx) {
  for (unsigned i = paramIdx, n = paramTuple.size(); i != n; ++i) {
    const auto &param = paramTuple[i];
    auto type = param.isVararg() ? param.getVarargBaseTy() : param.getType();
    type = type->getRValueInstanceType();

    // Look through optionals.
    if (auto optValue = type->getAnyOptionalObjectType()) {
      type = optValue;
    }

    auto funcTy = type->getAs<FunctionType>();
    if (!funcTy)
      return false;

    if (funcTy->isAutoClosure())
      return false;
  }

  return true;
};

ArrayRef<TupleTypeElt> constraints::decomposeArgParamType(Type type,
                                                          TupleTypeElt &scalar){
  switch (type->getKind()) {
  case TypeKind::Tuple:
      return cast<TupleType>(type.getPointer())->getFields();

  case TypeKind::Paren:
    scalar = cast<ParenType>(type.getPointer())->getUnderlyingType();
    return scalar;

  default:
    scalar = type;
    return scalar;
  }
}

bool constraints::matchCallArguments(
       ArrayRef<TupleTypeElt> argTuple,
       ArrayRef<TupleTypeElt> paramTuple,
       bool allowFixes,
       MatchCallArgumentListener &listener,
       SmallVectorImpl<ParamBinding> &parameterBindings) {
  // Keep track of the parameter we're matching and what argument indices
  // got bound to each parameter.
  unsigned paramIdx, numParams = paramTuple.size();
  parameterBindings.clear();
  parameterBindings.resize(numParams);

  // Keep track of which arguments we have claimed from the argument tuple.
  unsigned nextArgIdx = 0, numArgs = argTuple.size();
  SmallVector<bool, 4> claimedArgs(numArgs, false);
  SmallVector<Identifier, 4> actualArgNames;
  unsigned numClaimedArgs = 0;

  // Indicates whether any of the arguments are potentially out-of-order,
  // requiring further checking at the end.
  bool potentiallyOutOfOrder = false;

  // Local function that claims the argument at \c nextArgIdx, returning the
  // index of the claimed argument. This is primarily a helper for
  // \c claimNextNamed.
  auto claim = [&](Identifier expectedName, bool ignoreNameClash = false)
                  -> unsigned {
    // Make sure we can claim this argument.
    assert(nextArgIdx != numArgs && "Must have a valid index to claim");
    assert(!claimedArgs[nextArgIdx] && "Argument already claimed");

    if (!actualArgNames.empty()) {
      // We're recording argument names; record this one.
      actualArgNames[nextArgIdx] = expectedName;
    } else if (argTuple[nextArgIdx].getName() != expectedName &&
               !ignoreNameClash) {
      // We have an argument name mismatch. Start recording argument names.
      actualArgNames.resize(numArgs);

      // Figure out previous argument names from the parameter bindings.
      for (unsigned i = 0; i != numParams; ++i) {
        const auto &param = paramTuple[i];
        bool firstArg = true;

        for (auto argIdx : parameterBindings[i]) {
          actualArgNames[argIdx] = firstArg ? param.getName() : Identifier();
          firstArg = false;
        }
      }

      // Record this argument name.
      actualArgNames[nextArgIdx] = expectedName;
    }

    claimedArgs[nextArgIdx] = true;
    ++numClaimedArgs;
    return nextArgIdx++;
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
    // If we've claimed all of the arguments, there's nothing more to do.
    if (numClaimedArgs == numArgs)
      return Nothing;

    // When the expected name is empty, we claim the next argument if it has
    // no name.
    if (name.empty()) {
      // Nothing to claim.
      if (nextArgIdx == numArgs ||
          claimedArgs[nextArgIdx] ||
          (argTuple[nextArgIdx].hasName() && !ignoreNameMismatch))
        return Nothing;

      return claim(name);
    }

    // Skip over any claimed arguments.
    skipClaimedArgs();

    // If the name matches, claim this argument.
    if (nextArgIdx != numArgs &&
        (ignoreNameMismatch ||
         argTuple[nextArgIdx].getName() == name)) {
      return claim(name);
    }

    // The name didn't match. Go hunting for an unclaimed argument whose name
    // does match.
    Optional<unsigned> claimedWithSameName;
    for (unsigned i = nextArgIdx; i != numArgs; ++i) {
      // Skip arguments where the name doesn't match.
      if (argTuple[i].getName() != name)
        continue;

      // Skip claimed arguments.
      if (claimedArgs[i]) {
        // Note that we have already claimed an argument with the same name.
        if (!claimedWithSameName) {
          claimedWithSameName = i;
        }
        continue;
      }

      // We found a match. Claim it.
      nextArgIdx = i;
      return claim(name);
    }

    // Look at previous arguments to determine whether any are unclaimed and
    // have the same name.
    for (unsigned i = 0; i != nextArgIdx; ++i) {
      // Skip arguments where the name doesn't match.
      if (argTuple[i].getName() != name)
        continue;

      // Skip claimed arguments.
      if (claimedArgs[i]) {
        // Note that we have already claimed an argument with the same name.
        if (!claimedWithSameName) {
          claimedWithSameName = i;
        }

        continue;
      }

      // We found a match. Claim it and note that the arguments are potentially
      // out of order.
      potentiallyOutOfOrder = true;
      nextArgIdx = i;
      return claim(name);
    }

    // If the current parameter appears to be a trailing closure, consume as if
    // the name were always correct.
    // FIXME: This is a hack. We should know whether it's a trailing closure
    // or not.
    if (paramIsTrailingClosure(paramTuple, paramIdx)) {
      return claim(argTuple[nextArgIdx].getName(),
                   /*ignoreNameClash=*/true);
    }

    // If the next argument has a name, check whether that name applies to
    // some other parameter that hasn't yet been processed. If so, there is
    // nothing to claim.
    for (unsigned i = paramIdx + 1; i != numParams; ++i) {
      if (paramTuple[i].getName() == name)
        return Nothing;
    }

    // If we're not supposed to attempt any fixes, we're done.
    if (!allowFixes)
      return Nothing;

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
      return Nothing;
    }

    // Missing a keyword argument name.
    if (nextArgIdx != numArgs && !argTuple[nextArgIdx].hasName()) {
      // Claim the next argument.
      return claim(name);
    }

    // Typo correction is handled in a later pass.
    return Nothing;
  };

  // Local function that attempts to bind the given parameter to arguments in
  // the list.
  bool haveUnfulfilledParams = false;
  auto bindNextParameter = [&](bool ignoreNameMismatch) {
    const auto &param = paramTuple[paramIdx];

    // Handle variadic parameters.
    if (param.isVararg()) {
      // Claim the next argument with the name of this parameter.
      auto claimed = claimNextNamed(param.getName(), ignoreNameMismatch);

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
    if (auto claimed = claimNextNamed(param.getName(), ignoreNameMismatch)) {
      parameterBindings[paramIdx].push_back(*claimed);
      skipClaimedArgs();
      return;
    }

    // There was no argument to claim. Leave the argument unfulfilled.
    haveUnfulfilledParams = true;
  };

  // Mark through the parameters, binding them to their arguments.
  for (paramIdx = 0; paramIdx != numParams; ++paramIdx) {
    bindNextParameter(false);
  }

  // If we have any unclaimed arguments, complain about those.
  if (numClaimedArgs != numArgs) {
    // If we're not allowed to fix anything, or if we don't have any
    // unfulfilled parameters, fail.
    if (!haveUnfulfilledParams || !allowFixes) {
      nextArgIdx = 0;
      skipClaimedArgs();
      listener.extraArgument(nextArgIdx);
      return true;
    }

    // Find all of the named, unclaimed arguments.
    llvm::SmallVector<unsigned, 4> unclaimedNamedArgs;
    for (nextArgIdx = 0; skipClaimedArgs(), nextArgIdx != numArgs;
         ++nextArgIdx) {
      if (argTuple[nextArgIdx].hasName())
        unclaimedNamedArgs.push_back(nextArgIdx);
    }

    if (!unclaimedNamedArgs.empty()) {
      // Find all of the named, unfulfilled parameters.
      llvm::SmallVector<unsigned, 4> unfulfilledNamedParams;
      bool hasUnfulfilledUnnamedParams = false;
      for (paramIdx = 0; paramIdx != numParams; ++paramIdx ) {
        if (parameterBindings[paramIdx].empty()) {
          if (paramTuple[paramIdx].hasName())
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
          auto argName = argTuple[argIdx].getName();

          // Find the closest matching unfulfilled named parameter.
          unsigned bestScore = 0;
          unsigned best = 0;
          for (unsigned i = 0, n = unfulfilledNamedParams.size(); i != n; ++i) {
            unsigned param = unfulfilledNamedParams[i];
            auto paramName = paramTuple[param].getName();

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

      const auto &param = paramTuple[paramIdx];

      // Variadic parameters can be unfulfilled.
      if (param.isVararg())
        continue;

      // Parameters with defaults can be unfilfilled.
      if (param.getDefaultArgKind() != DefaultArgumentKind::None)
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
        const auto &param = paramTuple[i];
        if (param.isVararg() ||
            param.getDefaultArgKind() != DefaultArgumentKind::None)
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
matchCallArguments(ConstraintSystem &cs, Type argType, Type paramType,
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

  // Extract the argument tuple fields.
  TupleTypeElt argScalar;
  ArrayRef<TupleTypeElt> argTuple = decomposeArgParamType(argType, argScalar);

  // Extract the parameter tuple fields.
  TupleTypeElt paramScalar;
  ArrayRef<TupleTypeElt> paramTuple = decomposeArgParamType(paramType,
                                                            paramScalar);

  // Match up the call arguments to the parameters.
  Listener listener(cs, argType, paramType, locator);
  SmallVector<ParamBinding, 4> parameterBindings;
  if (constraints::matchCallArguments(argTuple, paramTuple,
                                      cs.shouldAttemptFixes(), listener,
                                      parameterBindings))
    return ConstraintSystem::SolutionKind::Error;

  // Check the argument types for each of the parameters.
  unsigned subflags = ConstraintSystem::TMF_GenerateConstraints;
  for (unsigned paramIdx = 0, numParams = parameterBindings.size();
       paramIdx != numParams; ++paramIdx){
    // Skip unfulfilled parameters. There's nothing to do for them.
    if (parameterBindings[paramIdx].empty())
      continue;

    // Determine the parameter type.
    const auto &param = paramTuple[paramIdx];
    auto paramTy = param.isVararg() ? param.getVarargBaseTy()
                                    : param.getType();

    // Compare each of the bound arguments for this parameter.
    for (auto argIdx : parameterBindings[paramIdx]) {
      // FIXME: Add a special locator for matching a parameter to an argument,
      // because we need both indices to diagnose well.
      switch (cs.matchTypes(argTuple[argIdx].getType(), paramTy,
                            TypeMatchKind::Conversion, subflags,
                            locator.withPathElement(
                              LocatorPathElt::getTupleElement(argIdx)))) {
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

  // If this is an argument tuple conversion, handle it directly. The rules are
  // different than for normal tuple conversions.
  if (kind == TypeMatchKind::ArgumentTupleConversion &&
      getASTContext().LangOpts.StrictKeywordArguments) {
    return ::matchCallArguments(*this, tuple1, tuple2, locator);
  }

  // Equality and subtyping have fairly strict requirements on tuple matching,
  // requiring element names to either match up or be disjoint.
  if (kind < TypeMatchKind::Conversion) {
    if (tuple1->getFields().size() != tuple2->getFields().size()) {
      // Record this failure.
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::TupleSizeMismatch, tuple1, tuple2);
      }

      return SolutionKind::Error;
    }

    for (unsigned i = 0, n = tuple1->getFields().size(); i != n; ++i) {
      const auto &elt1 = tuple1->getFields()[i];
      const auto &elt2 = tuple2->getFields()[i];

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

  // Compute the element shuffles for conversions.
  SmallVector<int, 16> sources;
  SmallVector<unsigned, 4> variadicArguments;
  if (computeTupleShuffle(tuple1, tuple2, sources, variadicArguments,
                          ::hasMandatoryTupleLabels(locator))) {
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
    const auto &elt1 = tuple1->getFields()[idx1];
    const auto &elt2 = tuple2->getFields()[idx2];
    switch (matchTypes(elt1.getType(), elt2.getType(), kind, subFlags,
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
    const auto &elt2 = tuple2->getFields().back();
    auto eltType2 = elt2.getVarargBaseTy();

    for (unsigned idx1 : variadicArguments) {
      switch (matchTypes(tuple1->getElementType(idx1), eltType2, kind, subFlags,
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

/// Check whether this is a single-element tuple whose element name is the
/// given name.
static bool isSingleElementTupleNamed(Type type, Identifier name) {
  if (auto tupleTy = type->getAs<TupleType>()) {
    if (tupleTy->getNumElements() == 1)
      return tupleTy->getFields()[0].getName() == name;
  }

  return false;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchScalarToTupleTypes(Type type1, TupleType *tuple2,
                                          TypeMatchKind kind, unsigned flags,
                                          ConstraintLocatorBuilder locator) {
  int scalarFieldIdx = tuple2->getFieldForScalarInit();
  assert(scalarFieldIdx >= 0 && "Invalid tuple for scalar-to-tuple");
  const auto &elt = tuple2->getFields()[scalarFieldIdx];

  // For an argument tuple conversion, provide a fix that adds the
  // missing label (if it needs one).
  // FIXME: Handle varargs.
  if (kind == TypeMatchKind::ArgumentTupleConversion &&
      elt.hasName() && !elt.isVararg() &&
      getASTContext().LangOpts.StrictKeywordArguments &&
      !paramIsTrailingClosure(tuple2->getFields(), scalarFieldIdx) &&
      !isSingleElementTupleNamed(type1, elt.getName())) {
    return simplifyFixConstraint(
             Fix::getRelabelTuple(*this, FixKind::ScalarToTuple, elt.getName()),
             type1, tuple2, TypeMatchKind::Conversion, flags, locator);
  }

  auto scalarFieldTy = elt.isVararg()? elt.getVarargBaseTy() : elt.getType();
  return matchTypes(type1, scalarFieldTy, kind, flags,
                    locator.withPathElement(ConstraintLocator::ScalarToTuple));
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchTupleToScalarTypes(TupleType *tuple1, Type type2,
                                          TypeMatchKind kind, unsigned flags,
                                          ConstraintLocatorBuilder locator) {
  assert(tuple1->getNumElements() == 1 && "Wrong number of elements");
  assert(!tuple1->getFields()[0].isVararg() && "Should not be variadic");

  // For an argument tuple conversion, provide a fix that removes the
  // label.
  if (kind == TypeMatchKind::ArgumentTupleConversion &&
      shouldAttemptFixes() && !(flags & TMF_ApplyingFix)) {
    return simplifyFixConstraint(
             Fix::getRelabelTuple(*this, FixKind::TupleToScalar, Identifier()),
             tuple1, type2, TypeMatchKind::Conversion, flags, locator);
  }

  return matchTypes(tuple1->getElementType(0),
                    type2, kind, flags,
                    locator.withPathElement(
                      LocatorPathElt::getTupleElement(0)));
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchFunctionTypes(FunctionType *func1, FunctionType *func2,
                                     TypeMatchKind kind, unsigned flags,
                                     ConstraintLocatorBuilder locator) {
  // An @auto_closure function type can be a subtype of a
  // non-@auto_closure function type.
  if (func1->isAutoClosure() != func2->isAutoClosure()) {
    if (func2->isAutoClosure() || kind < TypeMatchKind::Subtype) {
      // Record this failure.
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::FunctionAutoclosureMismatch, func1, func2);
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

  // Determine how we match up the input/result types.
  TypeMatchKind subKind;
  switch (kind) {
  case TypeMatchKind::BindType:
  case TypeMatchKind::SameType:
    subKind = kind;
    break;

  case TypeMatchKind::Subtype:
  case TypeMatchKind::Conversion:
  case TypeMatchKind::ArgumentTupleConversion:
  case TypeMatchKind::OperatorConversion:
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
  case TypeMatchKind::SameType:
    return Failure::TypesNotEqual;

  case TypeMatchKind::Subtype:
    return Failure::TypesNotSubtypes;

  case TypeMatchKind::Conversion:
  case TypeMatchKind::OperatorConversion:
  case TypeMatchKind::ArgumentTupleConversion:
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
    switch (simplifyConformsToConstraint(type1, proto, locator, false)) {
      case SolutionKind::Solved:
        break;

      case SolutionKind::Unsolved:
        // Add the constraint.
        addConstraint(ConstraintKind::ConformsTo, type1,
                      proto->getDeclaredType());
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
    return ConstraintKind::Bind;

  case TypeMatchKind::SameType:
    return ConstraintKind::Equal;

  case TypeMatchKind::Subtype:
    return ConstraintKind::Subtype;

  case TypeMatchKind::Conversion:
    return ConstraintKind::Conversion;

  case TypeMatchKind::ArgumentTupleConversion:
    return ConstraintKind::ArgumentTupleConversion;

  case TypeMatchKind::OperatorConversion:
    return ConstraintKind::OperatorConversion;
  }

  llvm_unreachable("unhandled type matching kind");
}

/// Determine whether we should attempt a user-defined conversion.
static bool shouldTryUserConversion(ConstraintSystem &cs, Type type) {
  // Strip the l-value qualifier if present.
  type = type->getRValueType();
  
  // If this isn't a type that can have user-defined conversions, there's
  // nothing to do.
  if (!type->getNominalOrBoundGenericNominal() && !type->is<ArchetypeType>())
    return false;

  // If there are no user-defined conversions, there's nothing to do.
  // FIXME: lame name!
  auto &ctx = cs.getASTContext();
  auto name = ctx.getIdentifier("__conversion");
  return static_cast<bool>(cs.lookupMember(type, name));
}

/// If the given type has user-defined conversions, introduce new
/// relational constraint between the result of performing the user-defined
/// conversion and an arbitrary other type.
static ConstraintSystem::SolutionKind
tryUserConversion(ConstraintSystem &cs, Type type, ConstraintKind kind,
                  Type otherType, ConstraintLocatorBuilder locator) {
  assert(kind != ConstraintKind::Construction &&
         kind != ConstraintKind::Conversion &&
         kind != ConstraintKind::ArgumentTupleConversion &&
         kind != ConstraintKind::OperatorConversion &&
         "Construction/conversion constraints create potential cycles");
  
  // If this isn't a type that can have user-defined conversions, there's
  // nothing to do.
  if (!shouldTryUserConversion(cs, type))
    return ConstraintSystem::SolutionKind::Unsolved;

  auto memberLocator = cs.getConstraintLocator(
                         locator.withPathElement(
                           ConstraintLocator::ConversionMember));
  auto inputTV = cs.createTypeVariable(
                   cs.getConstraintLocator(memberLocator,
                                           ConstraintLocator::ApplyArgument),
                   /*options=*/0);
  auto outputTV = cs.createTypeVariable(
                    cs.getConstraintLocator(memberLocator,
                                            ConstraintLocator::ApplyFunction),
                    /*options=*/0);

  auto &ctx = cs.getASTContext();
  auto name = ctx.getIdentifier("__conversion");

  // The conversion function will have function type TI -> TO, for fresh
  // type variables TI and TO.
  cs.addValueMemberConstraint(type, name,
                              FunctionType::get(inputTV, outputTV),
                              memberLocator);

  // A conversion function must accept an empty parameter list ().
  // Note: This should never fail, because the declaration checker
  // should ensure that conversions have no non-defaulted parameters.
  cs.addConstraint(ConstraintKind::ArgumentTupleConversion,
                   TupleType::getEmpty(ctx),
                   inputTV, cs.getConstraintLocator(locator));

  // Relate the output of the conversion function to the other type, using
  // the provided constraint kind.
  // If the type we're converting to is existential, we can also have an
  // existential conversion here, so introduce a disjunction.
  auto resultLocator = cs.getConstraintLocator(
                         locator.withPathElement(
                           ConstraintLocator::ConversionResult));
  if (otherType->isExistentialType()) {
    Constraint *constraints[2] = {
      Constraint::create(cs, kind, outputTV, otherType, Identifier(),
                         resultLocator),
      Constraint::createRestricted(cs, ConstraintKind::Conversion,
                                   ConversionRestrictionKind::Existential,
                                   outputTV, otherType, resultLocator)
    };
    cs.addConstraint(Constraint::createDisjunction(cs, constraints,
                                                   resultLocator));
  } else {
    cs.addConstraint(kind, outputTV, otherType, resultLocator);
  }

  // We're adding a user-defined conversion.
  cs.increaseScore(SK_UserConversion);

  return ConstraintSystem::SolutionKind::Solved;
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
  for (const auto &elt : tupleTy->getFields()) {
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
ConstraintSystem::matchTypes(Type type1, Type type2, TypeMatchKind kind,
                             unsigned flags,
                             ConstraintLocatorBuilder locator) {
  // If we have type variables that have been bound to fixed types, look through
  // to the fixed type.
  TypeVariableType *typeVar1;
  type1 = getFixedTypeRecursive(type1, typeVar1,
                                kind == TypeMatchKind::SameType);
  auto desugar1 = type1->getDesugaredType();

  TypeVariableType *typeVar2;
  type2 = getFixedTypeRecursive(type2, typeVar2,
                                kind == TypeMatchKind::SameType);
  auto desugar2 = type2->getDesugaredType();

  // If the types are obviously equivalent, we're done.
  if (desugar1 == desugar2)
    return SolutionKind::Solved;

  // If either (or both) types are type variables, unify the type variables.
  if (typeVar1 || typeVar2) {
    switch (kind) {
    case TypeMatchKind::BindType:
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
          if (type2->is<LValueType>()) {
            if (shouldRecordFailures()) {
              recordFailure(getConstraintLocator(locator),
                            Failure::IsForbiddenLValue, type1, type2);
            }
            return SolutionKind::Error;
          }

          // Okay. Bind below.
        }

        assignFixedType(typeVar1, type2);
        return SolutionKind::Solved;
      }

      // If we want an rvalue, get the rvalue.
      if (wantRvalue)
        type1 = type1->getRValueType();

      if (!typeVar2->getImpl().canBindToLValue()) {
        if (type1->is<LValueType>()) {
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
    case TypeMatchKind::ArgumentTupleConversion:
    case TypeMatchKind::OperatorConversion:
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

      // metatype<B> < metatype<A> if A < B and both A and B are classes.
      TypeMatchKind subKind = TypeMatchKind::SameType;
      if (isa<MetatypeType>(desugar1) &&
          kind != TypeMatchKind::SameType &&
          (meta1->getInstanceType()->mayHaveSuperclass() ||
           meta2->getInstanceType()->getClassOrBoundGenericClass()))
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
      if (kind >= TypeMatchKind::OperatorConversion) {
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
      int scalar1 = tuple1->getFieldForScalarInit();
      int scalar2 = tuple2->getFieldForScalarInit();
      if (scalar1 >= 0 && scalar2 >= 0) {
        auto name1 = tuple1->getFields()[scalar1].getName();
        auto name2 = tuple2->getFields()[scalar2].getName();
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
      if ((tuple2->getFields().size() == 1 &&
           !tuple2->getFields()[0].isVararg()) ||
          (kind >= TypeMatchKind::Conversion &&
           tuple2->getFieldForScalarInit() >= 0)) {
        conversionsOrFixes.push_back(
          ConversionRestrictionKind::ScalarToTuple);

        // FIXME: Prohibits some user-defined conversions for tuples.
        goto commit_to_conversions;
      }
    }

    if (tuple1 && !tuplesWithMismatchedNames) {
      // A single-element tuple can be a trivial subtype of a scalar.
      if (tuple1->getFields().size() == 1 &&
          !tuple1->getFields()[0].isVararg()) {
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
    
    // Implicit array conversions.
    if (kind >= TypeMatchKind::Conversion) {      
      if (isArrayType(desugar1) && isArrayType(desugar2)) {
        
        // Push both the upcast and bridged conversion kinds. We'll construct
        // a disjunctive constraint out of them, favoring the upcast conversion
        // should there be a tie.
        conversionsOrFixes.push_back(ConversionRestrictionKind::ArrayUpcast);
        conversionsOrFixes.push_back(ConversionRestrictionKind::ArrayBridged);
      }
    }
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
  }

  if (concrete && kind >= TypeMatchKind::OperatorConversion) {
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
        
        conversionsOrFixes.push_back(
          ConversionRestrictionKind::ValueToOptional);
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

  // A nominal type can be converted to another type via a user-defined
  // conversion function.
  if (concrete && kind >= TypeMatchKind::Conversion &&
      shouldTryUserConversion(*this, type1)) {
    conversionsOrFixes.push_back(ConversionRestrictionKind::User);
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
    if (objectType1->isAnyObject() &&
        type2->getClassOrBoundGenericClass()) {
      conversionsOrFixes.push_back(FixKind::ForceDowncast);
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
    ConstraintRestrictions.push_back({type1, type2, *restriction});
    return simplifyRestrictedConstraint(*restriction, type1, type2,
                                        kind, subFlags, locator);
  }

  // Handle fixes.
  auto fix = *conversionsOrFixes[0].getFix();
  return simplifyFixConstraint(fix, type1, type2, kind, subFlags, locator);
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstructionConstraint(Type valueType, Type argType,
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

  auto ctors = TC.lookupConstructors(valueType, DC);
  if (!ctors) {
    // If we are supposed to record failures, do so.
    if (shouldRecordFailures()) {
      recordFailure(locator, Failure::TypesNotConstructible,
                    valueType, argType);
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
  // variable T. Note that these constraints specifically require a
  // match on the result type because the constructors for enums and struct
  // types always return a value of exactly that type.
  addValueMemberConstraint(valueType, name,
                           FunctionType::get(tv, valueType),
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
    if (TC.conformsToProtocol(type, protocol, DC))
      return SolutionKind::Solved;
  }
  
  // If possible, redirect the coercion to a bridged type.
  if (TC.isBridgedDynamicConversion(DC, protocol->getDeclaredType(), type)) {
    auto bridgedType = TC.getBridgedType(DC, type);
    
    if (!bridgedType.isNull()) {
      simplifyRestrictedConstraint(ConversionRestrictionKind::User,
                                   type,
                                   bridgedType,
                                   TypeMatchKind::Conversion,
                                   TMF_GenerateConstraints,
                                   locator);
      
      return SolutionKind::Solved;
    }
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
static CheckedCastKind getCheckedCastKind(Type fromType, Type toType) {
  // Classify the from/to types.
  bool toArchetype = toType->is<ArchetypeType>();
  bool fromArchetype = fromType->is<ArchetypeType>();
  bool toExistential = toType->isExistentialType();
  bool fromExistential = fromType->isExistentialType();

  // We can only downcast to an existential if the destination protocols are
  // objc and the source type is an objc class or an existential bounded by objc
  // protocols.
  if (toExistential) {
    return CheckedCastKind::ConcreteToUnrelatedExistential;
  }

  // A downcast can:
  //   - convert an archetype to a (different) archetype type.
  if (fromArchetype && toArchetype) {
    return CheckedCastKind::ArchetypeToArchetype;
  }

  //   - convert from an existential to an archetype or conforming concrete
  //     type.
  if (fromExistential) {
    if (toArchetype) {
      return CheckedCastKind::ExistentialToArchetype;
    }

    return CheckedCastKind::ExistentialToConcrete;
  }

  //   - convert an archetype to a concrete type fulfilling its constraints.
  if (fromArchetype) {
    return CheckedCastKind::ArchetypeToConcrete;
  }

  if (toArchetype) {
    //   - convert from a superclass to an archetype.
    if (toType->castTo<ArchetypeType>()->getSuperclass()) {
      return CheckedCastKind::SuperToArchetype;
    }

    //  - convert a concrete type to an archetype for which it fulfills
    //    constraints.
    return CheckedCastKind::ConcreteToArchetype;
  }

  // The remaining case is a class downcast.
  assert(!fromArchetype && "archetypes should have been handled above");
  assert(!toArchetype && "archetypes should have been handled above");
  assert(!fromExistential && "existentials should have been handled above");
  assert(!toExistential && "existentials should have been handled above");

  return CheckedCastKind::Downcast;

}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyCheckedCastConstraint(
                    Type fromType, Type toType,
                    ConstraintLocatorBuilder locator) {
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

  switch (getCheckedCastKind(fromType, toType)) {
  case CheckedCastKind::ArchetypeToArchetype:
  case CheckedCastKind::ConcreteToUnrelatedExistential:
  case CheckedCastKind::ExistentialToArchetype:
  case CheckedCastKind::SuperToArchetype:
    return SolutionKind::Solved;

  case CheckedCastKind::ArchetypeToConcrete:
  case CheckedCastKind::ConcreteToArchetype:
    // FIXME: Check substitutability.
    return SolutionKind::Solved;

  case CheckedCastKind::Downcast:
    addConstraint(ConstraintKind::Subtype, toType, fromType,
                  getConstraintLocator(locator));
    return SolutionKind::Solved;

  case CheckedCastKind::ExistentialToConcrete:
    addConstraint(ConstraintKind::Subtype, toType, fromType);
    return SolutionKind::Solved;

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

static DeclName
getNameForValueLookup(
    ASTContext &ctx,
    const Constraint &constraint,
    Type baseObjTy,
    const llvm::DenseMap<UnresolvedDotExpr *, ApplyExpr *> &callMap) {
  auto origName = constraint.getMember();
  if (origName.isCompoundName())
    return origName;

  auto protoTy = baseObjTy->getAs<ProtocolType>();
  if (!protoTy ||
      !protoTy->getDecl()->isSpecificProtocol(KnownProtocolKind::AnyObject))
    return origName;

  // FIXME: Pulling out the expr from the locator is pretty hacky.
  const ConstraintLocator *loc = constraint.getLocator();
  if (!loc)
    return origName;

  auto UDE = dyn_cast_or_null<UnresolvedDotExpr>(loc->getAnchor());
  if (!UDE)
    return origName;

  const ApplyExpr *call = callMap.lookup(UDE);
  if (!call)
    return origName;

  // Handle the one-argument, no-argument-name case.
  if (isa<ParenExpr>(call->getArg()))
    return DeclName(ctx, origName.getBaseName(), Identifier());

  auto args = dyn_cast<TupleExpr>(call->getArg());
  if (!args)
    return origName;

  return DeclName(ctx, origName.getBaseName(), args->getElementNames());
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyMemberConstraint(const Constraint &constraint) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  Type baseTy = simplifyType(constraint.getFirstType());
  Type baseObjTy = baseTy->getRValueType();

  // Try to look through ImplicitlyUnwrappedOptional<T>; the result is always an r-value.
  if (auto objTy = lookThroughImplicitlyUnwrappedOptionalType(baseObjTy)) {
    increaseScore(SK_ForceUnchecked);
    baseTy = baseObjTy = objTy;
  }

  // Dig out the instance type.
  bool isMetatype = false;
  Type instanceTy = baseObjTy;
  if (auto baseObjMeta = baseObjTy->getAs<AnyMetatypeType>()) {
    instanceTy = baseObjMeta->getInstanceType();
    isMetatype = true;
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
        Value < baseTuple->getFields().size()) {
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

  bool isExistential = instanceTy->isExistentialType();
  if (name.isSimpleName(TC.Context.Id_init)) {
    // Constructors have their own approach to name lookup.
    auto ctors = TC.lookupConstructors(baseObjTy, DC);
    if (!ctors) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);

      return SolutionKind::Error;
    }

    // Introduce a new overload set.
    SmallVector<OverloadChoice, 4> choices;
    for (auto constructor : ctors) {
      // If the constructor is invalid, skip it.
      // FIXME: Note this as invalid, in case we don't find a solution,
      // so we don't let errors cascade further.
      TC.validateDecl(constructor, true);
      if (constructor->isInvalid())
        continue;

      // If our base is an existential type, we can't make use of any
      // constructor whose signature involves associated types.
      // FIXME: Mark this as 'unavailable'.
      if (isExistential &&
          isUnavailableInExistential(getTypeChecker(), constructor))
        continue;

      choices.push_back(OverloadChoice(baseTy, constructor,
                                       /*isSpecialized=*/false));
    }

    if (choices.empty()) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);
      return SolutionKind::Error;
    }
    
    addOverloadSet(memberTy, choices, constraint.getLocator());
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
    
    auto lookup = TC.lookupMemberType(baseObjTy, name.getBaseName(), DC);
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

  // If we're doing dynamic lookup, and this is a call, use the full name of
  // the member.
  name = getNameForValueLookup(getASTContext(), constraint, baseObjTy,
                               PossibleDynamicLookupCalls);

  // Look for members within the base.
  LookupResult &lookup = lookupMember(baseObjTy, name);
  if (!lookup) {
    // Check whether we actually performed a lookup with an integer value.
    unsigned index;
    if (name.isSimpleName()
        && !name.getBaseName().str().getAsInteger(10, index)) {
      // ".0" on a scalar just refers to the underlying scalar value.
      if (index == 0) {
        OverloadChoice identityChoice(baseTy, OverloadChoiceKind::BaseType);
        addBindOverloadConstraint(memberTy, identityChoice,
                                  constraint.getLocator());
        return SolutionKind::Solved;
      }

      // FIXME: Specialize diagnostic here?
    }

    recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                  baseObjTy, name);

    return SolutionKind::Error;
  }

  // The set of directly accessible types, which is only used when
  // we're performing dynamic lookup into an existential type.
  bool isDynamicLookup = false;
  if (auto protoTy = instanceTy->getAs<ProtocolType>()) {
    isDynamicLookup = protoTy->getDecl()->isSpecificProtocol(
                                            KnownProtocolKind::AnyObject);
  }

  // Record the fact that we found a mutating function for diagnostics.
  bool FoundMutating = false;

  // Introduce a new overload set to capture the choices.
  SmallVector<OverloadChoice, 4> choices;
  for (auto result : lookup) {
    // If the result is invalid, skip it.
    // FIXME: Note this as invalid, in case we don't find a solution,
    // so we don't let errors cascade further.
    TC.validateDecl(result, true);
    if (result->isInvalid())
      continue;

    // If our base is an existential type, we can't make use of any
    // member whose signature involves associated types.
    // FIXME: Mark this as 'unavailable'.
    if (isExistential && isUnavailableInExistential(getTypeChecker(), result))
      continue;

    // If we are looking for a metatype member, don't include members that can
    // only be accessed on an instance of the object.
    // FIXME: Mark as 'unavailable' somehow.
    if (isMetatype && !(isa<FuncDecl>(result) || !result->isInstanceMember())) {
      continue;
    }

    // If we aren't looking in a metatype, ignore static functions, static
    // variables, and enum elements.
    if (!isMetatype && !baseObjTy->is<ModuleType>() &&
        !result->isInstanceMember())
      continue;

    // If we're doing dynamic lookup into a metatype of AnyObject and we've
    // found an instance member, ignore it.
    if (isDynamicLookup && isMetatype && result->isInstanceMember()) {
      // FIXME: Mark as 'unavailable' somehow.
      continue;
    }

    // If we have an rvalue base of struct or enum type, make sure that the
    // result isn't mutating (only valid on lvalues).
    if (!isMetatype && !baseObjTy->hasReferenceSemantics() &&
        !baseTy->is<LValueType>() && result->isInstanceMember()) {
      if (auto *FD = dyn_cast<FuncDecl>(result))
        if (FD->isMutating()) {
          FoundMutating = true;
          continue;
        }

      // Subscripts are ok on rvalues so long as the getter is nonmutating.
      if (auto *SD = dyn_cast<SubscriptDecl>(result))
        if (SD->getGetter()->isMutating()) {
          FoundMutating = true;
          continue;
        }

      // Computed properties are ok on rvalues so long as the getter is
      // nonmutating.
      if (auto *VD = dyn_cast<VarDecl>(result))
        if (auto *GD = VD->getGetter())
          if (GD->isMutating()) {
            FoundMutating = true;
            continue;
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
      continue;
    }

    choices.push_back(OverloadChoice(baseTy, result, /*isSpecialized=*/false));
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
  if (auto tv = dyn_cast<TypeVariableType>(type.getPointer())) {
    auto fixed = cs.getFixedType(tv);
    if (!fixed)
      return Type();

    // Continue with the fixed type.
    type = fixed;
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
ConstraintSystem::simplifyDynamicLookupConstraint(const Constraint &constraint){
  auto baseTy = simplifyForTypePropertyConstraint(*this,
                                                  constraint.getFirstType());
  if (!baseTy)
    return SolutionKind::Unsolved;

  // Look through implicit lvalue types.
  baseTy = baseTy->getRValueType();

  // Look through one level of optional type.
  // FIXME: this is kindof specific to the use of this
  // constraint for ForceValueExpr.
  if (auto valueTy = baseTy->getAnyOptionalObjectType()) {
    valueTy = simplifyForTypePropertyConstraint(*this, valueTy);
    if (!valueTy)
      return SolutionKind::Unsolved;
    baseTy = valueTy;
  }

  if (auto protoTy = baseTy->getAs<ProtocolType>()) {
    if (protoTy->getDecl()->isSpecificProtocol(
                              KnownProtocolKind::AnyObject))
      return SolutionKind::Solved;
  }

  // Record this failure.
  recordFailure(constraint.getLocator(), Failure::IsNotArchetype, baseTy);
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

  // Try to look through ImplicitlyUnwrappedOptional<T>; the result is always an r-value.
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

    assert(func1->getResult()->is<TypeVariableType>() &&
           "the output of funct1 is a free variable by construction");

    // If this application is part of an operator, then we allow an implicit
    // lvalue to be compatible with inout arguments.  This is used by
    // assignment operators.
    TypeMatchKind ArgConv = TypeMatchKind::ArgumentTupleConversion;
    if (isa<PrefixUnaryExpr>(anchor) || isa<PostfixUnaryExpr>(anchor) ||
        isa<BinaryExpr>(anchor))
      ArgConv = TypeMatchKind::OperatorConversion;
    
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
    return SolutionKind::Solved;
  }

  // For a metatype, perform a construction.
  if (auto meta2 = dyn_cast<AnyMetatypeType>(desugar2)) {
    auto instanceTy2 = meta2->getInstanceType();

    // Bind the result type to the instance type.
    if (matchTypes(func1->getResult(), instanceTy2,
                   TypeMatchKind::BindType,
                   flags,
                   locator.withPathElement(ConstraintLocator::ApplyFunction))
        == SolutionKind::Error)
      return SolutionKind::Error;

    // Construct the instance from the input arguments.
    addConstraint(ConstraintKind::Construction, func1->getInput(), instanceTy2,
                  getConstraintLocator(outerLocator));
    return SolutionKind::Solved;
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
  case ConstraintKind::ArgumentTupleConversion:
    return TypeMatchKind::ArgumentTupleConversion;
  case ConstraintKind::OperatorConversion:
    return TypeMatchKind::OperatorConversion;

  case ConstraintKind::ApplicableFunction:
    llvm_unreachable("ApplicableFunction constraints don't involve "
                     "type matches");

  case ConstraintKind::DynamicTypeOf:
    llvm_unreachable("DynamicTypeOf constraints don't involve type matches");

  case ConstraintKind::BindOverload:
    llvm_unreachable("Overload binding constraints don't involve type matches");

  case ConstraintKind::Construction:
    llvm_unreachable("Construction constraints don't involve type matches");

  case ConstraintKind::ConformsTo:
  case ConstraintKind::SelfObjectOfProtocol:
    llvm_unreachable("Conformance constraints don't involve type matches");

  case ConstraintKind::CheckedCast:
    llvm_unreachable("Checked cast constraints don't involve type matches");

  case ConstraintKind::ValueMember:
  case ConstraintKind::TypeMember:
    llvm_unreachable("Member constraints don't involve type matches");

  case ConstraintKind::Archetype:
  case ConstraintKind::Class:
  case ConstraintKind::DynamicLookupValue:
    llvm_unreachable("Type properties don't involve type matches");

  case ConstraintKind::Conjunction:
  case ConstraintKind::Disjunction:
    llvm_unreachable("Con/disjunction constraints don't involve type matches");
  }
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

  switch (restriction) {
  // for $< in { <, <c, <oc }:
  //   T_i $< U_i ===> (T_i...) $< (U_i...)
  case ConversionRestrictionKind::TupleToTuple:
    return matchTupleTypes(type1->castTo<TupleType>(),
                           type2->castTo<TupleType>(),
                           matchKind, flags, locator);

  //   T <c U ===> T <c (U)
  case ConversionRestrictionKind::ScalarToTuple:
    return matchScalarToTupleTypes(type1,
                                   type2->castTo<TupleType>(),
                                   matchKind, flags, locator);

  // for $< in { <, <c, <oc }:
  //   T $< U ===> (T) $< U
  case ConversionRestrictionKind::TupleToScalar:
    return matchTupleToScalarTypes(type1->castTo<TupleType>(),
                                   type2,
                                   matchKind, flags, locator);

  case ConversionRestrictionKind::DeepEquality:
    return matchDeepEqualityTypes(type1, type2, locator);

  case ConversionRestrictionKind::Superclass:
    addContextualScore();
    return matchSuperclassTypes(type1, type2, matchKind, flags, locator);

  case ConversionRestrictionKind::LValueToRValue:
    return matchTypes(type1->getRValueType(), type2,
                      matchKind, flags, locator);

  // for $< in { <, <c, <oc }:
  //   T $< U, U : P_i ===> T $< protocol<P_i...>
  case ConversionRestrictionKind::Existential:
    addContextualScore();
    return matchExistentialTypes(type1, type2,
                                 matchKind, flags, locator);

  // for $< in { <, <c, <oc }:
  //   T $< U ===> T $< U?
  case ConversionRestrictionKind::ValueToOptional: {
    addContextualScore();
    assert(matchKind >= TypeMatchKind::Subtype);
    auto generic2 = type2->castTo<BoundGenericType>();
    assert(generic2->getDecl()->classifyAsOptionalType());
    return matchTypes(type1, generic2->getGenericArgs()[0],
                      matchKind, flags, locator);
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
                      matchKind, flags, locator);
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
                      matchKind, flags, locator);
  }
      
  // T < U ===> Array<T> <c Array<U>
  case ConversionRestrictionKind::ArrayUpcast: {
    increaseScore(SK_ArrayUpcastConversion);
    
    addContextualScore();
    assert(matchKind >= TypeMatchKind::Conversion);
    
    Type baseType1;
    Type baseType2;
    
    auto t1 = type1->getDesugaredType();
    auto t2 = type2->getDesugaredType();
    
    if (auto sliceType1 = dyn_cast<ArraySliceType>(t1)) {
      baseType1 = sliceType1->getBaseType();
    } else {
        auto arrayType1 = cast<BoundGenericStructType>(t1);
        baseType1 = arrayType1->getGenericArgs()[0];
    }
    
    if (auto sliceType2 = dyn_cast<ArraySliceType>(t2)) {
      baseType2 = sliceType2->getBaseType();
    } else {
      auto arrayType2 = cast<BoundGenericStructType>(t2);
      baseType2 = arrayType2->getGenericArgs()[0];
    }
    
    return matchTypes(baseType1,
                      baseType2,
                      TypeMatchKind::Subtype,
                      flags,
                      locator);
  }
      
  // T < U ===> Array<T> is bridged to Array<U>
  case ConversionRestrictionKind::ArrayBridged: {
    addContextualScore();
    assert(matchKind >= TypeMatchKind::Conversion);
    
    Type baseType1;
    Type baseType2;
    
    auto t1 = type1->getDesugaredType();
    auto t2 = type2->getDesugaredType();
    
    if (auto sliceType1 = dyn_cast<ArraySliceType>(t1)) {
      baseType1 = sliceType1->getBaseType();
    } else {
      auto arrayType1 = cast<BoundGenericStructType>(t1);
      baseType1 = arrayType1->getGenericArgs()[0];
    }
    
    if (auto sliceType2 = dyn_cast<ArraySliceType>(t2)) {
      baseType2 = sliceType2->getBaseType();
    } else {
      auto arrayType2 = cast<BoundGenericStructType>(t2);
      baseType2 = arrayType2->getGenericArgs()[0];
    }
    
    if (auto nominalType1 = dyn_cast<NominalType>(baseType1.getPointer())) {
      auto bridgedType1 = TC.getBridgedType(DC, nominalType1);

      // Allow assignments from Array<T> to Array<AnyObject> if T is a bridged
      // type.
      if (!bridgedType1.isNull()) {
        if (auto protoTy = baseType2->getAs<ProtocolType>()) {
          if(protoTy->getDecl()->isSpecificProtocol(KnownProtocolKind::
                                                        AnyObject)) {
            return SolutionKind::Solved;
          }
        }
        
        // If we're not converting to AnyObject, check if T.ObjectiveCType is U.
        // We'll save the further check for conformance with
        // _ConditionallyBridgedToObjectiveC until runtime.
        if (bridgedType1.getPointer() == baseType2.getPointer()) {
          return SolutionKind::Solved;
        }
        else {
          // Check if T's bridged type is a subtype of U.
          return matchTypes(bridgedType1,
                            baseType2,
                            TypeMatchKind::Subtype,
                            flags,
                            locator);
        }
      }
    }
    
    return ConstraintSystem::SolutionKind::Error;
  }

  // T' < U, hasMember(T, conversion, T -> T') ===> T <c U
  case ConversionRestrictionKind::User:
    addContextualScore();
    assert(matchKind >= TypeMatchKind::Conversion);
    return tryUserConversion(*this, type1,
                             ConstraintKind::Subtype,
                             type2,
                             locator);
      
  }
  
  llvm_unreachable("bad conversion restriction");
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyFixConstraint(Fix fix,
                                        Type type1, Type type2,
                                        TypeMatchKind matchKind,
                                        unsigned flags,
                                        ConstraintLocatorBuilder locator) {
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
      int scalarIdx = tupleTy->getFieldForScalarInit();
      nameToAdd = tupleTy->getFields()[scalarIdx].getName();
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
    int scalarFieldIdx = tuple2->getFieldForScalarInit();
    assert(scalarFieldIdx >= 0 && "Invalid tuple for scalar-to-tuple");
    const auto &elt = tuple2->getFields()[scalarFieldIdx];
    auto scalarFieldTy = elt.isVararg()? elt.getVarargBaseTy() : elt.getType();
    return matchTypes(type1, scalarFieldTy, matchKind, subFlags,
                      locator.withPathElement(
                        ConstraintLocator::ScalarToTuple));
  }

  case FixKind::RelabelCallTuple:
    // The actual semantics are handled elsewhere.
    return SolutionKind::Solved;
  }
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstraint(const Constraint &constraint) {
  switch (constraint.getKind()) {
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::ArgumentTupleConversion:
  case ConstraintKind::OperatorConversion: {
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
                                                         TMF_GenerateConstraints,
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

  case ConstraintKind::Construction:
    return simplifyConstructionConstraint(constraint.getSecondType(),
                                          constraint.getFirstType(),
                                          TMF_None,
                                          constraint.getLocator());

  case ConstraintKind::ConformsTo:
  case ConstraintKind::SelfObjectOfProtocol:
    return simplifyConformsToConstraint(
             constraint.getFirstType(),
             constraint.getProtocol(),
             constraint.getLocator(),
             constraint.getKind() == ConstraintKind::SelfObjectOfProtocol);

  case ConstraintKind::CheckedCast:
    return simplifyCheckedCastConstraint(constraint.getFirstType(),
                                         constraint.getSecondType(),
                                         constraint.getLocator());

  case ConstraintKind::ValueMember:
  case ConstraintKind::TypeMember:
    return simplifyMemberConstraint(constraint);

  case ConstraintKind::Archetype:
    return simplifyArchetypeConstraint(constraint);

  case ConstraintKind::Class:
    return simplifyClassConstraint(constraint);

  case ConstraintKind::DynamicLookupValue:
    return simplifyDynamicLookupConstraint(constraint);

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
