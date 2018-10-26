//===--- CSSimplify.cpp - Constraint Simplification -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements simplifications of constraints within the constraint
// system.
//
//===----------------------------------------------------------------------===//

#include "CSFix.h"
#include "ConstraintSystem.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/StringExtras.h"
#include "swift/ClangImporter/ClangModule.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Compiler.h"

using namespace swift;
using namespace constraints;

MatchCallArgumentListener::~MatchCallArgumentListener() { }

void MatchCallArgumentListener::extraArgument(unsigned argIdx) { }

void MatchCallArgumentListener::missingArgument(unsigned paramIdx) { }

bool MatchCallArgumentListener::missingLabel(unsigned paramIdx) { return true; }
bool MatchCallArgumentListener::extraneousLabel(unsigned paramIdx) {
  return true;
}
bool MatchCallArgumentListener::incorrectLabel(unsigned paramIdx) {
  return true;
}

void MatchCallArgumentListener::outOfOrderArgument(unsigned argIdx,
                                                   unsigned prevArgIdx) {
}

bool MatchCallArgumentListener::relabelArguments(ArrayRef<Identifier> newNames){
  return true;
}

/// Produce a score (smaller is better) comparing a parameter name and
/// potentially-typo'd argument name.
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

  // Compute the edit distance.
  unsigned dist = argName.edit_distance(paramName, /*AllowReplacements=*/true,
                                        /*MaxEditDistance=*/maxScore);

  // If the edit distance would be too long, we're done.
  if (maxScore != 0 && dist > maxScore)
    return None;

  // The distance can be zero due to the "with" transformation above.
  if (dist == 0)
    return 1;

  // If this is just a single character label on both sides,
  // simply return distance.
  if (paramName.size() == 1 && argName.size() == 1)
    return dist;

  // Only allow about one typo for every two properly-typed characters, which
  // prevents completely-wacky suggestions in many cases.
  if (dist > (argName.size() + 1) / 3)
    return None;

  return dist;
}

bool constraints::
areConservativelyCompatibleArgumentLabels(ValueDecl *decl,
                                          bool hasCurriedSelf,
                                          ArrayRef<Identifier> labels,
                                          bool hasTrailingClosure) {
  // Bail out conservatively if this isn't a function declaration.
  auto fn = dyn_cast<AbstractFunctionDecl>(decl);
  if (!fn) return true;
  
  auto *fTy = fn->getInterfaceType()->castTo<AnyFunctionType>();
  
  SmallVector<AnyFunctionType::Param, 8> argInfos;
  for (auto argLabel : labels) {
    argInfos.push_back(AnyFunctionType::Param(Type(), argLabel, {}));
  }

  const AnyFunctionType *levelTy = fTy;
  if (hasCurriedSelf) {
    levelTy = levelTy->getResult()->getAs<AnyFunctionType>();
    assert(levelTy && "Parameter list curry level does not match type");
  }
  
  auto params = levelTy->getParams();
  SmallBitVector defaultMap =
    computeDefaultMap(params, decl, hasCurriedSelf);

  MatchCallArgumentListener listener;
  SmallVector<ParamBinding, 8> unusedParamBindings;

  return !matchCallArguments(argInfos, params, defaultMap,
                             hasTrailingClosure,
                             /*allow fixes*/ false,
                             listener, unusedParamBindings);
}

/// Determine the default type-matching options to use when decomposing a
/// constraint into smaller constraints.
static ConstraintSystem::TypeMatchOptions getDefaultDecompositionOptions(
         ConstraintSystem::TypeMatchOptions flags) {
  return flags | ConstraintSystem::TMF_GenerateConstraints;
}

// FIXME: This should return ConstraintSystem::TypeMatchResult instead
//        to give more information to the solver about the failure.
bool constraints::
matchCallArguments(ArrayRef<AnyFunctionType::Param> args,
                   ArrayRef<AnyFunctionType::Param> params,
                   const SmallBitVector &defaultMap,
                   bool hasTrailingClosure,
                   bool allowFixes,
                   MatchCallArgumentListener &listener,
                   SmallVectorImpl<ParamBinding> &parameterBindings) {
  assert(params.size() == defaultMap.size() && "Default map does not match");

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

  auto hasDefault = [&defaultMap, &numParams](unsigned idx) -> bool {
    return idx < numParams ? defaultMap.test(idx) : false;
  };

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
    } else if (args[argNumber].getLabel() != expectedName && !ignoreNameClash) {
      // We have an argument name mismatch. Start recording argument names.
      actualArgNames.resize(numArgs);

      // Figure out previous argument names from the parameter bindings.
      for (unsigned i = 0; i != numParams; ++i) {
        const auto &param = params[i];
        bool firstArg = true;

        for (auto argIdx : parameterBindings[i]) {
          actualArgNames[argIdx] = firstArg ? param.getLabel() : Identifier();
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
    = [&](Identifier paramLabel, bool ignoreNameMismatch,
          bool forVariadic = false) -> Optional<unsigned> {
    // Skip over any claimed arguments.
    skipClaimedArgs();

    // If we've claimed all of the arguments, there's nothing more to do.
    if (numClaimedArgs == numArgs)
      return None;

    // Go hunting for an unclaimed argument whose name does match.
    Optional<unsigned> claimedWithSameName;
    for (unsigned i = nextArgIdx; i != numArgs; ++i) {
      auto argLabel = args[i].getLabel();

      if (argLabel != paramLabel) {
        // If this is an attempt to claim additional unlabeled arguments
        // for variadic parameter, we have to stop at first labeled argument.
        if (forVariadic)
          return None;

        // Otherwise we can continue trying to find argument which
        // matches parameter with or without label.
        continue;
      }

      // Skip claimed arguments.
      if (claimedArgs[i]) {
        // Note that we have already claimed an argument with the same name.
        if (!claimedWithSameName)
          claimedWithSameName = i;
        continue;
      }

      // We found a match.  If the match wasn't the next one, we have
      // potentially out of order arguments.
      if (i != nextArgIdx) {
        // Avoid claiming un-labeled defaulted parameters
        // by out-of-order un-labeled arguments or parts
        // of variadic argument sequence, because that might
        // be incorrect:
        // ```swift
        // func foo(_ a: Int, _ b: Int = 0, c: Int = 0, _ d: Int) {}
        // foo(1, c: 2, 3) // -> `3` will be claimed as '_ b:'.
        // ```
        if (argLabel.empty() && (hasDefault(i) || !forVariadic))
          continue;

        potentiallyOutOfOrder = true;
      }

      // Claim it.
      return claim(paramLabel, i);
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
    //   - The argument might have extraneous label, in which case we try to
    //     fix the problem by removing such label.
    //   - The keyword argument might be a typo for an actual argument name, in
    //     which case we should find the closest match to correct to.

    // Missing or extraneous label.
    if (nextArgIdx != numArgs && ignoreNameMismatch) {
      auto argLabel = args[nextArgIdx].getLabel();
      // Claim this argument if we are asked to ignore labeling failure,
      // only if argument doesn't have a label when parameter expected
      // it to, or vice versa.
      if (paramLabel.empty() || argLabel.empty())
        return claim(paramLabel, nextArgIdx);
    }

    // Redundant keyword arguments.
    if (claimedWithSameName) {
      // FIXME: We can provide better diagnostics here.
      return None;
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
    if (param.isVariadic()) {
      // Claim the next argument with the name of this parameter.
      auto claimed = claimNextNamed(param.getLabel(), ignoreNameMismatch);

      // If there was no such argument, leave the parameter unfulfilled.
      if (!claimed) {
        haveUnfulfilledParams = true;
        return;
      }

      // Record the first argument for the variadic.
      parameterBindings[paramIdx].push_back(*claimed);

      auto currentNextArgIdx = nextArgIdx;
      {
        nextArgIdx = *claimed;
        // Claim any additional unnamed arguments.
        while ((claimed = claimNextNamed(Identifier(), false, true))) {
          parameterBindings[paramIdx].push_back(*claimed);
        }
      }

      nextArgIdx = currentNextArgIdx;
      skipClaimedArgs();
      return;
    }

    // Try to claim an argument for this parameter.
    if (auto claimed = claimNextNamed(param.getLabel(), ignoreNameMismatch)) {
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
      if (!args[nextArgIdx].getLabel().empty())
        unclaimedNamedArgs.push_back(nextArgIdx);
    }

    if (!unclaimedNamedArgs.empty()) {
      // Find all of the named, unfulfilled parameters.
      llvm::SmallVector<unsigned, 4> unfulfilledNamedParams;
      bool hasUnfulfilledUnnamedParams = false;
      for (paramIdx = 0; paramIdx != numParams; ++paramIdx) {
        if (parameterBindings[paramIdx].empty()) {
          if (params[paramIdx].getLabel().empty())
            hasUnfulfilledUnnamedParams = true;
          else
            unfulfilledNamedParams.push_back(paramIdx);
        }
      }

      if (!unfulfilledNamedParams.empty()) {
        // Use typo correction to find the best matches.
        // FIXME: There is undoubtedly a good dynamic-programming algorithm
        // to find the best assignment here.
        for (auto argIdx : unclaimedNamedArgs) {
          auto argName = args[argIdx].getLabel();

          // Find the closest matching unfulfilled named parameter.
          unsigned bestScore = 0;
          unsigned best = 0;
          for (unsigned i = 0, n = unfulfilledNamedParams.size(); i != n; ++i) {
            unsigned param = unfulfilledNamedParams[i];
            auto paramName = params[param].getLabel();

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
            auto paramLabel = params[paramIdx].getLabel();

            parameterBindings[paramIdx].push_back(claim(paramLabel, argIdx));
            skipClaimedArgs();

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

    // If there are as many arguments as parameters but we still
    // haven't claimed all of the arguments, it could mean that
    // labels don't line up, if so let's try to claim arguments
    // with incorrect labels, and let OoO/re-labeling logic diagnose that.
    if (numArgs == numParams && numClaimedArgs != numArgs) {
      for (unsigned i = 0; i < numArgs; ++i) {
        if (claimedArgs[i] || !parameterBindings[i].empty())
          continue;

        // If parameter has a default value, we don't really
        // now if label doesn't match because it's incorrect
        // or argument belongs to some other parameter, so
        // we just leave this parameter unfulfilled.
        if (defaultMap.test(i))
          continue;

        // Looks like there was no parameter claimed at the same
        // position, it could only mean that label is completely
        // different, because typo correction has been attempted already.
        parameterBindings[i].push_back(claim(params[i].getLabel(), i));
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
      if (param.isVariadic())
        continue;

      // Parameters with defaults can be unfulfilled.
      if (hasDefault(paramIdx))
        continue;

      listener.missingArgument(paramIdx);
      return true;
    }
  }

  // If any arguments were provided out-of-order, check whether we have
  // violated any of the reordering rules.
  if (potentiallyOutOfOrder) {
    unsigned argIdx = 0;
    // Enumerate the parameters and their bindings to see if any arguments are
    // our of order
    for (auto binding : parameterBindings) {
      for (auto boundArgIdx : binding) {
        // We've found the parameter that has an out of order
        // argument, and know the indices of the argument that
        // needs to move (fromArgIdx) and the argument location
        // it should move to (toArgIdx).
        auto fromArgIdx = boundArgIdx;
        auto toArgIdx = argIdx;

        // If there is no re-ordering going on, and index is past
        // the number of parameters, it could only mean that this
        // is variadic parameter, so let's just move on.
        if (fromArgIdx == toArgIdx && toArgIdx >= params.size()) {
          assert(args[fromArgIdx].getLabel().empty());
          argIdx++;
          continue;
        }

        // First let's double check if out-of-order argument is nothing
        // more than a simple label mismatch, because in situation where
        // one argument requires label and another one doesn't, but caller
        // doesn't provide either, problem is going to be identified as
        // out-of-order argument instead of label mismatch.
        auto expectedLabel = params[toArgIdx].getLabel();
        auto argumentLabel = args[fromArgIdx].getLabel();

        if (argumentLabel != expectedLabel) {
          // - The parameter is unnamed, in which case we try to fix the
          //   problem by removing the name.
          if (expectedLabel.empty()) {
            if (listener.extraneousLabel(toArgIdx))
              return true;
          // - The argument is unnamed, in which case we try to fix the
          //   problem by adding the name.
          } else if (argumentLabel.empty()) {
            if (listener.missingLabel(toArgIdx))
              return true;
          // - The argument label has a typo at the same position.
          } else if (fromArgIdx == toArgIdx &&
                     listener.incorrectLabel(toArgIdx)) {
            return true;
          }
        }

        if (boundArgIdx == argIdx) {
          // If the argument is in the right location, just continue
          argIdx++;
          continue;
        }

        listener.outOfOrderArgument(fromArgIdx, toArgIdx);
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

/// Find the callee declaration and uncurry level for a given call
/// locator.
static std::tuple<ValueDecl *, bool, ArrayRef<Identifier>, bool>
getCalleeDeclAndArgs(ConstraintSystem &cs,
                     ConstraintLocatorBuilder callLocator,
                     SmallVectorImpl<Identifier> &argLabelsScratch) {
  ArrayRef<Identifier> argLabels;
  bool hasTrailingClosure = false;

  // Break down the call.
  SmallVector<LocatorPathElt, 2> path;
  auto callExpr = callLocator.getLocatorParts(path);
  if (!callExpr)
    return std::make_tuple(nullptr, /*hasCurriedSelf=*/false, argLabels,
                           hasTrailingClosure);

  // Our remaining path can only be 'ApplyArgument'.
  if (!path.empty() &&
      !(path.size() <= 2 &&
        path.back().getKind() == ConstraintLocator::ApplyArgument))
    return std::make_tuple(nullptr, /*hasCurriedSelf=*/false, argLabels,
                           hasTrailingClosure);

  // Dig out the callee.
  ConstraintLocator *targetLocator;
  if (auto call = dyn_cast<CallExpr>(callExpr)) {
    targetLocator = cs.getConstraintLocator(call->getDirectCallee());
    argLabels = call->getArgumentLabels();
    hasTrailingClosure = call->hasTrailingClosure();
  } else if (auto unresolved = dyn_cast<UnresolvedMemberExpr>(callExpr)) {
    targetLocator = cs.getConstraintLocator(callExpr);
    argLabels = unresolved->getArgumentLabels();
    hasTrailingClosure = unresolved->hasTrailingClosure();
  } else if (auto subscript = dyn_cast<SubscriptExpr>(callExpr)) {
    targetLocator = cs.getConstraintLocator(callExpr);
    argLabels = subscript->getArgumentLabels();
    hasTrailingClosure = subscript->hasTrailingClosure();
  } else if (auto dynSubscript = dyn_cast<DynamicSubscriptExpr>(callExpr)) {
    targetLocator = cs.getConstraintLocator(callExpr);
    argLabels = dynSubscript->getArgumentLabels();
    hasTrailingClosure = dynSubscript->hasTrailingClosure();
  } else if (auto keyPath = dyn_cast<KeyPathExpr>(callExpr)) {
    if (path.size() != 2 ||
        path[0].getKind() != ConstraintLocator::KeyPathComponent ||
        path[1].getKind() != ConstraintLocator::ApplyArgument)
      return std::make_tuple(nullptr, /*hasCurriedSelf=*/false, argLabels,
                             hasTrailingClosure);

    auto componentIndex = path[0].getValue();
    if (componentIndex >= keyPath->getComponents().size())
      return std::make_tuple(nullptr, /*hasCurriedSelf=*/false, argLabels,
                             hasTrailingClosure);

    auto &component = keyPath->getComponents()[componentIndex];
    switch (component.getKind()) {
    case KeyPathExpr::Component::Kind::Subscript:
    case KeyPathExpr::Component::Kind::UnresolvedSubscript:
      targetLocator = cs.getConstraintLocator(callExpr, path[0]);
      argLabels = component.getSubscriptLabels();
      hasTrailingClosure = false; // key paths don't support trailing closures
      break;
      
    case KeyPathExpr::Component::Kind::Invalid:
    case KeyPathExpr::Component::Kind::UnresolvedProperty:
    case KeyPathExpr::Component::Kind::Property:
    case KeyPathExpr::Component::Kind::OptionalForce:
    case KeyPathExpr::Component::Kind::OptionalChain:
    case KeyPathExpr::Component::Kind::OptionalWrap:
    case KeyPathExpr::Component::Kind::Identity:
      return std::make_tuple(nullptr, /*hasCurriedSelf=*/false, argLabels,
                             hasTrailingClosure);
    }

  } else {
    if (auto apply = dyn_cast<ApplyExpr>(callExpr)) {
      argLabels = apply->getArgumentLabels(argLabelsScratch);
      assert(!apply->hasTrailingClosure());
    } else if (auto objectLiteral = dyn_cast<ObjectLiteralExpr>(callExpr)) {
      argLabels = objectLiteral->getArgumentLabels();
      hasTrailingClosure = objectLiteral->hasTrailingClosure();
    }
    return std::make_tuple(nullptr, /*hasCurriedSelf=*/false, argLabels,
                           hasTrailingClosure);
  }

  // Find the overload choice corresponding to the callee locator.
  // FIXME: This linearly walks the list of resolved overloads, which is
  // potentially very expensive.
  Optional<OverloadChoice> choice;
  for (auto resolved = cs.getResolvedOverloadSets(); resolved;
       resolved = resolved->Previous) {
    // FIXME: Workaround null locators.
    if (!resolved->Locator) continue;

    auto resolvedLocator = resolved->Locator;
    SmallVector<LocatorPathElt, 4> resolvedPath(
                                     resolvedLocator->getPath().begin(),
                                     resolvedLocator->getPath().end());
    if (!resolvedPath.empty() &&
        (resolvedPath.back().getKind() == ConstraintLocator::SubscriptMember ||
         resolvedPath.back().getKind() == ConstraintLocator::Member ||
         resolvedPath.back().getKind() == ConstraintLocator::UnresolvedMember ||
         resolvedPath.back().getKind() ==
           ConstraintLocator::ConstructorMember)) {
      resolvedPath.pop_back();
      resolvedLocator = cs.getConstraintLocator(
                          resolvedLocator->getAnchor(),
                          resolvedPath,
                          resolvedLocator->getSummaryFlags());
    }

    SourceRange range;
    resolvedLocator = simplifyLocator(cs, resolvedLocator, range);

    if (resolvedLocator == targetLocator) {
      choice = resolved->Choice;
      break;
    }
  }
  
  // If we didn't find any matching overloads, we're done.
  if (!choice)
    return std::make_tuple(nullptr, /*hasCurriedSelf=*/false, argLabels,
                           hasTrailingClosure);

  // If there's a declaration, return it.
  if (choice->isDecl()) {
    auto decl = choice->getDecl();
    bool hasCurriedSelf = false;
    if (decl->getDeclContext()->isTypeContext()) {
      if (auto function = dyn_cast<AbstractFunctionDecl>(decl)) {
        // References to instance members on a metatype stay at level 0.
        // Everything else is level 1.
        if (!(function->isInstanceMember() &&
              cs.getFixedTypeRecursive(choice->getBaseType(),
                                       /*wantRValue=*/true)
                ->is<AnyMetatypeType>()))
          hasCurriedSelf = true;
      } else if (isa<SubscriptDecl>(decl)) {
        // Subscript level 1 == the indices.
        hasCurriedSelf = true;
      } else if (isa<EnumElementDecl>(decl)) {
        // Enum element level 1 == the payload.
        hasCurriedSelf = true;
      }
    }

    return std::make_tuple(decl, hasCurriedSelf, argLabels, hasTrailingClosure);
  }

  return std::make_tuple(nullptr, /*hasCurriedSelf=*/false, argLabels,
                         hasTrailingClosure);
}

class ArgumentFailureTracker : public MatchCallArgumentListener {
  ConstraintSystem &CS;
  ConstraintLocatorBuilder Locator;

public:
  ArgumentFailureTracker(ConstraintSystem &cs, ConstraintLocatorBuilder locator)
    : CS(cs), Locator(locator) {}

  bool missingLabel(unsigned paramIndex) override {
    return !CS.shouldAttemptFixes();
  }

  bool extraneousLabel(unsigned paramIndex) override {
    return !CS.shouldAttemptFixes();
  }

  bool incorrectLabel(unsigned paramIndex) override {
    return !CS.shouldAttemptFixes();
  }

  bool relabelArguments(ArrayRef<Identifier> newLabels) override {
    if (!CS.shouldAttemptFixes())
      return true;

    auto *anchor = Locator.getBaseLocator()->getAnchor();
    if (!anchor || !isa<CallExpr>(anchor))
      return true;

    auto *locator = CS.getConstraintLocator(anchor);
    auto *fix = RelabelArguments::create(CS, newLabels, locator);
    CS.recordFix(fix);
    return false;
  }
};

// Match the argument of a call to the parameter.
ConstraintSystem::TypeMatchResult constraints::matchCallArguments(
    ConstraintSystem &cs, ArrayRef<AnyFunctionType::Param> args,
    ArrayRef<AnyFunctionType::Param> params, ConstraintLocatorBuilder locator) {
  // Extract the parameters.
  ValueDecl *callee;
  bool hasCurriedSelf;
  ArrayRef<Identifier> argLabels;
  SmallVector<Identifier, 2> argLabelsScratch;
  bool hasTrailingClosure = false;
  std::tie(callee, hasCurriedSelf, argLabels, hasTrailingClosure) =
    getCalleeDeclAndArgs(cs, locator, argLabelsScratch);

  SmallBitVector defaultMap =
    computeDefaultMap(params, callee, hasCurriedSelf);

  // Apply labels to arguments.
  SmallVector<AnyFunctionType::Param, 8> argsWithLabels;
  argsWithLabels.append(args.begin(), args.end());
  AnyFunctionType::relabelParams(argsWithLabels, argLabels);

  // FIXME: Remove this. It's functionally identical to the real code
  // path below, except for some behavioral differences in solution ranking
  // that I don't understand.
  if (params.size() == 1 &&
      args.size() == 1 &&
      params[0].getLabel().empty() &&
      args[0].getLabel().empty() &&
      !params[0].getParameterFlags().isInOut() &&
      !args[0].getParameterFlags().isInOut() &&
      params[0].getPlainType()->isAny()) {
    auto argType = args[0].getPlainType();

    // Disallow assignment of noescape function to parameter of type
    // Any. Allowing this would allow these functions to escape.
    if (auto *fnTy = argType->getAs<AnyFunctionType>()) {
      if (fnTy->isNoEscape()) {
        auto *loc = cs.getConstraintLocator(locator);
        // Allow assigned of 'no-escape' function with recorded fix.
        if (cs.shouldAttemptFixes()) {
          if (!cs.recordFix(MarkExplicitlyEscaping::create(cs, loc)))
            return cs.getTypeMatchSuccess();
        }

        return cs.getTypeMatchFailure(locator);
      }
    }

    return cs.getTypeMatchSuccess();
  }

  // Match up the call arguments to the parameters.
  ArgumentFailureTracker listener(cs, locator);
  SmallVector<ParamBinding, 4> parameterBindings;
  if (constraints::matchCallArguments(argsWithLabels, params,
                                      defaultMap,
                                      hasTrailingClosure,
                                      cs.shouldAttemptFixes(), listener,
                                      parameterBindings))
    return cs.getTypeMatchFailure(locator);

  // If this application is part of an operator, then we allow an implicit
  // lvalue to be compatible with inout arguments.  This is used by
  // assignment operators.
  auto *anchor = locator.getAnchor();
  assert(anchor && "locator without anchor expression?");
  bool isOperator = (isa<PrefixUnaryExpr>(anchor) ||
                     isa<PostfixUnaryExpr>(anchor) || isa<BinaryExpr>(anchor));

  ConstraintKind subKind = (isOperator
                            ? ConstraintKind::OperatorArgumentConversion
                            : ConstraintKind::ArgumentConversion);
  
  for (unsigned paramIdx = 0, numParams = parameterBindings.size();
       paramIdx != numParams; ++paramIdx){
    // Skip unfulfilled parameters. There's nothing to do for them.
    if (parameterBindings[paramIdx].empty())
      continue;

    // Determine the parameter type.
    const auto &param = params[paramIdx];
    auto paramTy = param.getOldType();

    // Compare each of the bound arguments for this parameter.
    for (auto argIdx : parameterBindings[paramIdx]) {
      auto loc = locator.withPathElement(LocatorPathElt::
                                            getApplyArgToParam(argIdx,
                                                               paramIdx));
      auto argTy = argsWithLabels[argIdx].getOldType();
      cs.addConstraint(subKind, argTy, paramTy, loc, /*isFavored=*/false);
    }
  }

  return cs.getTypeMatchSuccess();
}

ConstraintSystem::TypeMatchResult
ConstraintSystem::matchTupleTypes(TupleType *tuple1, TupleType *tuple2,
                                  ConstraintKind kind, TypeMatchOptions flags,
                                  ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  // FIXME: Remove varargs logic below once we're no longer comparing
  // argument lists in CSRanking.

  // Equality and subtyping have fairly strict requirements on tuple matching,
  // requiring element names to either match up or be disjoint.
  if (kind < ConstraintKind::Conversion) {
    if (tuple1->getNumElements() != tuple2->getNumElements())
      return getTypeMatchFailure(locator);

    for (unsigned i = 0, n = tuple1->getNumElements(); i != n; ++i) {
      const auto &elt1 = tuple1->getElement(i);
      const auto &elt2 = tuple2->getElement(i);

      // If the names don't match, we may have a conflict.
      if (elt1.getName() != elt2.getName()) {
        // Same-type requirements require exact name matches.
        if (kind <= ConstraintKind::Equal)
          return getTypeMatchFailure(locator);

        // For subtyping constraints, just make sure that this name isn't
        // used at some other position.
        if (elt2.hasName() && tuple1->getNamedElementId(elt2.getName()) != -1)
          return getTypeMatchFailure(locator);
      }

      // Variadic bit must match.
      if (elt1.isVararg() != elt2.isVararg())
        return getTypeMatchFailure(locator);

      // Compare the element types.
      auto result = matchTypes(elt1.getType(), elt2.getType(), kind, subflags,
                               locator.withPathElement(
                                           LocatorPathElt::getTupleElement(i)));
      if (result.isFailure())
        return result;
    }

    return getTypeMatchSuccess();
  }

  assert(kind >= ConstraintKind::Conversion);
  ConstraintKind subKind;
  switch (kind) {
  case ConstraintKind::OperatorArgumentConversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::Conversion:
    subKind = ConstraintKind::Conversion;
    break;

  case ConstraintKind::Bind:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Equal:
  case ConstraintKind::Subtype:
  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::BindOverload:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::Defaultable:
  case ConstraintKind::Disjunction:
  case ConstraintKind::DynamicTypeOf:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OpenedExistentialOf:
  case ConstraintKind::KeyPath:
  case ConstraintKind::KeyPathApplication:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::OptionalObject:
  case ConstraintKind::SelfObjectOfProtocol:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::ValueMember:
  case ConstraintKind::BridgingConversion:
  case ConstraintKind::FunctionInput:
  case ConstraintKind::FunctionResult:
    llvm_unreachable("Not a conversion");
  }

  // Compute the element shuffles for conversions.
  SmallVector<int, 16> sources;
  SmallVector<unsigned, 4> variadicArguments;
  if (computeTupleShuffle(tuple1, tuple2, sources, variadicArguments))
    return getTypeMatchFailure(locator);

  // Check each of the elements.
  bool hasVariadic = false;
  unsigned variadicIdx = sources.size();
  for (unsigned idx2 = 0, n = sources.size(); idx2 != n; ++idx2) {
    // Default-initialization always allowed for conversions.
    if (sources[idx2] == TupleShuffleExpr::DefaultInitialize) {
      continue;
    }

    // Variadic arguments handled below.
    if (sources[idx2] == TupleShuffleExpr::Variadic) {
      assert(!hasVariadic && "Multiple variadic parameters");
      hasVariadic = true;
      variadicIdx = idx2;
      continue;
    }

    assert(sources[idx2] >= 0);
    unsigned idx1 = sources[idx2];

    // Match up the types.
    const auto &elt1 = tuple1->getElement(idx1);
    const auto &elt2 = tuple2->getElement(idx2);
    auto result = matchTypes(elt1.getType(), elt2.getType(), subKind, subflags,
                       locator.withPathElement(
                                        LocatorPathElt::getTupleElement(idx1)));
    if (result.isFailure())
      return result;
  }

  // If we have variadic arguments to check, do so now.
  if (hasVariadic) {
    const auto &elt2 = tuple2->getElements()[variadicIdx];
    auto eltType2 = elt2.getVarargBaseTy();

    for (unsigned idx1 : variadicArguments) {
      auto result = matchTypes(tuple1->getElementType(idx1), eltType2, subKind,
                         subflags,
                         locator.withPathElement(
                                        LocatorPathElt::getTupleElement(idx1)));
      if (result.isFailure())
        return result;
    }
  }

  return getTypeMatchSuccess();
}

// Returns 'false' (i.e. no error) if it is legal to match functions with the
// corresponding function type representations and the given match kind.
static bool matchFunctionRepresentations(FunctionTypeRepresentation rep1,
                                         FunctionTypeRepresentation rep2,
                                         ConstraintKind kind) {
  switch (kind) {
  case ConstraintKind::Bind:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Equal:
    return rep1 != rep2;

  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::BridgingConversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::OperatorArgumentConversion:
  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::BindOverload:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::Defaultable:
  case ConstraintKind::Disjunction:
  case ConstraintKind::DynamicTypeOf:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OpenedExistentialOf:
  case ConstraintKind::KeyPath:
  case ConstraintKind::KeyPathApplication:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::OptionalObject:
  case ConstraintKind::SelfObjectOfProtocol:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::ValueMember:
  case ConstraintKind::FunctionInput:
  case ConstraintKind::FunctionResult:
    return false;
  }

  llvm_unreachable("Unhandled ConstraintKind in switch.");
}

ConstraintSystem::TypeMatchResult
ConstraintSystem::matchFunctionTypes(FunctionType *func1, FunctionType *func2,
                                     ConstraintKind kind, TypeMatchOptions flags,
                                     ConstraintLocatorBuilder locator) {
  // An @autoclosure function type can be a subtype of a
  // non-@autoclosure function type.
  if (func1->isAutoClosure() != func2->isAutoClosure()) {
    // If the 2nd type is an autoclosure, then the first type needs wrapping in a
    // closure despite already being a function type.
    if (func2->isAutoClosure())
      return getTypeMatchFailure(locator);
    if (kind < ConstraintKind::Subtype)
      return getTypeMatchFailure(locator);

    increaseScore(SK_FunctionConversion);
  }
  
  // A non-throwing function can be a subtype of a throwing function.
  if (func1->throws() != func2->throws()) {
    // Cannot drop 'throws'.
    if (func1->throws() || kind < ConstraintKind::Subtype)
      return getTypeMatchFailure(locator);
  }

  // A non-@noescape function type can be a subtype of a @noescape function
  // type.
  if (func1->isNoEscape() != func2->isNoEscape() &&
      (func1->isNoEscape() || kind < ConstraintKind::Subtype))
    return getTypeMatchFailure(locator);

  if (matchFunctionRepresentations(func1->getExtInfo().getRepresentation(),
                                   func2->getExtInfo().getRepresentation(),
                                   kind)) {
    return getTypeMatchFailure(locator);
  }

  // Determine how we match up the input/result types.
  ConstraintKind subKind;
  switch (kind) {
  case ConstraintKind::Bind:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Equal:
    subKind = kind;
    break;

  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::OperatorArgumentConversion:
    subKind = ConstraintKind::Subtype;
    break;

  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::BindOverload:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::Defaultable:
  case ConstraintKind::Disjunction:
  case ConstraintKind::DynamicTypeOf:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OpenedExistentialOf:
  case ConstraintKind::KeyPath:
  case ConstraintKind::KeyPathApplication:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::OptionalObject:
  case ConstraintKind::SelfObjectOfProtocol:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::ValueMember:
  case ConstraintKind::BridgingConversion:
  case ConstraintKind::FunctionInput:
  case ConstraintKind::FunctionResult:
    llvm_unreachable("Not a relational constraint");
  }

  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  SmallVector<AnyFunctionType::Param, 8> func1Params;
  func1Params.append(func1->getParams().begin(), func1->getParams().end());

  SmallVector<AnyFunctionType::Param, 8> func2Params;
  func2Params.append(func2->getParams().begin(), func2->getParams().end());

  // Add a very narrow exception to SE-0110 by allowing functions that
  // take multiple arguments to be passed as an argument in places
  // that expect a function that takes a single tuple (of the same
  // arity).
  auto isSingleParam = [&](ArrayRef<AnyFunctionType::Param> params) {
    return (params.size() == 1 &&
            params[0].getLabel().empty() &&
            !params[0].isVariadic());
  };

  auto implodeParams = [&](SmallVectorImpl<AnyFunctionType::Param> &params) {
    auto input = AnyFunctionType::composeInput(getASTContext(), params,
                                               /*canonicalVararg=*/false);
    params.clear();
    params.emplace_back(input);
  };

  {
    SmallVector<LocatorPathElt, 4> path;
    locator.getLocatorParts(path);

    // Find the last path element, skipping OptionalPayload elements
    // so that we allow this exception in cases of optional injection.
    auto last = std::find_if(
        path.rbegin(), path.rend(), [](LocatorPathElt &elt) -> bool {
          return elt.getKind() != ConstraintLocator::OptionalPayload;
        });

    if (last != path.rend()) {
      if (last->getKind() == ConstraintLocator::ApplyArgToParam) {
        if (isSingleParam(func2Params)) {
          if (!isSingleParam(func1Params)) {
            implodeParams(func1Params);
          }
        } else if (getASTContext().isSwiftVersionAtLeast(4)
                   && !getASTContext().isSwiftVersionAtLeast(5)
                   && !isSingleParam(func2Params)) {
          auto *simplified = locator.trySimplifyToExpr();
          // We somehow let tuple unsplatting function conversions
          // through in some cases in Swift 4, so let's let that
          // continue to work, but only for Swift 4.
          if (simplified && isa<DeclRefExpr>(simplified)) {
            if (isSingleParam(func1Params)) {
              implodeParams(func2Params);
            }
          }
        }
      }
    }
  }

  // https://bugs.swift.org/browse/SR-6796
  // Add a super-narrow hack to allow:
  //   (()) -> T to be passed in place of () -> T
  if (getASTContext().isSwiftVersionAtLeast(4) &&
      !getASTContext().isSwiftVersionAtLeast(5)) {
    SmallVector<LocatorPathElt, 4> path;
    locator.getLocatorParts(path);

    // Find the last path element, skipping GenericArgument elements
    // so that we allow this exception in cases of optional types, and
    // skipping OptionalPayload elements so that we allow this
    // exception in cases of optional injection.
    auto last = std::find_if(
        path.rbegin(), path.rend(), [](LocatorPathElt &elt) -> bool {
          return elt.getKind() != ConstraintLocator::GenericArgument &&
                 elt.getKind() != ConstraintLocator::OptionalPayload;
        });

    if (last != path.rend()) {
      if (last->getKind() == ConstraintLocator::ApplyArgToParam) {
        if (isSingleParam(func1Params) &&
            func1Params[0].getOldType()->isVoid()) {
          if (func2Params.empty()) {
            func2Params.emplace_back(getASTContext().TheEmptyTupleType);
          }
        }
      }
    }
  }

  // Input types can be contravariant (or equal).
  auto argumentLocator = locator.withPathElement(
      ConstraintLocator::FunctionArgument);

  if (func1Params.size() != func2Params.size())
    return getTypeMatchFailure(argumentLocator);

  for (unsigned i : indices(func1Params)) {
    auto func1Param = func1Params[i];
    auto func2Param = func2Params[i];

    // Variadic bit must match.
    if (func1Param.isVariadic() != func2Param.isVariadic())
      return getTypeMatchFailure(argumentLocator);

    // Labels must match.
    //
    // FIXME: We should not end up with labels here at all, but we do
    // from invalid code in diagnostics, and as a result of code completion
    // directly building constraint systems.
    if (func1Param.getLabel() != func2Param.getLabel())
      return getTypeMatchFailure(argumentLocator);

    // FIXME: We should check value ownership too, but it's not completely
    // trivial because of inout-to-pointer conversions.

    // Compare the parameter types.
    auto result = matchTypes(func2Param.getOldType(),
                             func1Param.getOldType(),
                             subKind, subflags,
                             (func1Params.size() == 1
                              ? argumentLocator
                              : argumentLocator.withPathElement(
                                LocatorPathElt::getTupleElement(i))));
    if (result.isFailure())
      return result;
  }

  // Result type can be covariant (or equal).
  return matchTypes(func1->getResult(), func2->getResult(), subKind,
                     subflags,
                     locator.withPathElement(
                       ConstraintLocator::FunctionResult));
}

ConstraintSystem::TypeMatchResult
ConstraintSystem::matchSuperclassTypes(Type type1, Type type2,
                                       TypeMatchOptions flags,
                                       ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  auto classDecl2 = type2->getClassOrBoundGenericClass();
  for (auto super1 = type1->getSuperclass();
       super1;
       super1 = super1->getSuperclass()) {
    if (super1->getClassOrBoundGenericClass() != classDecl2)
      continue;

    return matchTypes(super1, type2, ConstraintKind::Equal,
                      subflags, locator);
  }

  return getTypeMatchFailure(locator);
}

ConstraintSystem::TypeMatchResult
ConstraintSystem::matchDeepEqualityTypes(Type type1, Type type2,
                                         ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = TMF_GenerateConstraints;

  // Handle nominal types that are not directly generic.
  if (auto nominal1 = type1->getAs<NominalType>()) {
    auto nominal2 = type2->castTo<NominalType>();

    assert((bool)nominal1->getParent() == (bool)nominal2->getParent() &&
           "Mismatched parents of nominal types");

    if (!nominal1->getParent())
      return getTypeMatchSuccess();

    // Match up the parents, exactly.
    return matchTypes(nominal1->getParent(), nominal2->getParent(),
                      ConstraintKind::Equal, subflags,
                      locator.withPathElement(ConstraintLocator::ParentType));
  }

  auto bound1 = type1->castTo<BoundGenericType>();
  auto bound2 = type2->castTo<BoundGenericType>();

  // Match up the parents, exactly, if there are parents.
  assert((bool)bound1->getParent() == (bool)bound2->getParent() &&
         "Mismatched parents of bound generics");
  if (bound1->getParent()) {
    auto result = matchTypes(bound1->getParent(), bound2->getParent(),
                             ConstraintKind::Equal, subflags,
                             locator.withPathElement(
                                                ConstraintLocator::ParentType));
    if (result.isFailure())
      return result;
  }

  // Match up the generic arguments, exactly.
  auto args1 = bound1->getGenericArgs();
  auto args2 = bound2->getGenericArgs();
  if (args1.size() != args2.size()) {
    return getTypeMatchFailure(locator);
  }
  for (unsigned i = 0, n = args1.size(); i != n; ++i) {
    auto result = matchTypes(args1[i], args2[i], ConstraintKind::Equal,
                             subflags, locator.withPathElement(
                                        LocatorPathElt::getGenericArgument(i)));

    if (result.isFailure())
      return result;
  }

  return getTypeMatchSuccess();
}

ConstraintSystem::TypeMatchResult
ConstraintSystem::matchExistentialTypes(Type type1, Type type2,
                                        ConstraintKind kind,
                                        TypeMatchOptions flags,
                                        ConstraintLocatorBuilder locator) {
  // If the first type is a type variable or member thereof, there's nothing
  // we can do now.
  if (type1->isTypeVariableOrMember()) {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::create(*this, kind, type1, type2,
                           getConstraintLocator(locator)));
      return getTypeMatchSuccess();
    }

    return getTypeMatchAmbiguous();
  }

  // FIXME: Feels like a hack.
  if (type1->is<InOutType>())
    return getTypeMatchFailure(locator);

  // FIXME; Feels like a hack...nothing actually "conforms" here, and
  // we need to disallow conversions from @noescape functions to Any.

  // Conformance to 'Any' always holds.
  if (type2->isAny()) {
    auto *fnTy = type1->getAs<FunctionType>();
    if (!fnTy || !fnTy->isNoEscape())
      return getTypeMatchSuccess();

    if (shouldAttemptFixes()) {
      auto &ctx = getASTContext();
      auto *fix = MarkExplicitlyEscaping::create(
          *this, getConstraintLocator(locator), ctx.TheAnyType);
      if (!recordFix(fix))
        return getTypeMatchSuccess();
    }

    return getTypeMatchFailure(locator);
  }

  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  // Handle existential metatypes.
  if (auto meta1 = type1->getAs<MetatypeType>()) {
    if (auto meta2 = type2->getAs<ExistentialMetatypeType>()) {
      return matchExistentialTypes(meta1->getInstanceType(),
                                   meta2->getInstanceType(), kind, subflags,
                                   locator.withPathElement(
                                     ConstraintLocator::InstanceType));
    }
  }

  if (!type2->isExistentialType())
    return getTypeMatchFailure(locator);

  auto layout = type2->getExistentialLayout();

  if (auto layoutConstraint = layout.getLayoutConstraint()) {
    if (layoutConstraint->isClass()) {
      if (kind == ConstraintKind::ConformsTo) {
        if (!type1->satisfiesClassConstraint())
          return getTypeMatchFailure(locator);
      } else {
        // Subtype relation to AnyObject also allows class-bound
        // existentials that are not @objc and therefore carry
        // witness tables.
        if (!type1->isClassExistentialType() &&
            !type1->mayHaveSuperclass())
          return getTypeMatchFailure(locator);
      }

      // Keep going.
    }
  }

  if (layout.explicitSuperclass) {
    auto subKind = std::min(ConstraintKind::Subtype, kind);
    auto result = matchTypes(type1, layout.explicitSuperclass, subKind,
                             subflags, locator);
    if (result.isFailure())
      return result;
  }

  for (auto *proto : layout.getProtocols()) {
    auto *protoDecl = proto->getDecl();

    if (auto superclass = protoDecl->getSuperclass()) {
      auto subKind = std::min(ConstraintKind::Subtype, kind);
      auto result = matchTypes(type1, superclass, subKind,
                               subflags, locator);
      if (result.isFailure())
        return result;
    }

    switch (simplifyConformsToConstraint(type1, protoDecl, kind, locator,
                                         subflags)) {
      case SolutionKind::Solved:
      case SolutionKind::Unsolved:
        break;

      case SolutionKind::Error:
        return getTypeMatchFailure(locator);
    }
  }

  return getTypeMatchSuccess();
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

/// Determine whether the first type with the given number of optionals
/// is potentially more optional than the second type with its number of
/// optionals.
static bool isPotentiallyMoreOptionalThan(Type type1, Type type2) {

  SmallVector<Type, 2> optionals1;
  Type objType1 = type1->lookThroughAllOptionalTypes(optionals1);
  auto numOptionals1 = optionals1.size();

  SmallVector<Type, 2> optionals2;
  type2->lookThroughAllOptionalTypes(optionals2);
  auto numOptionals2 = optionals2.size();

  if (numOptionals1 <= numOptionals2 && !objType1->isTypeVariableOrMember())
    return false;

  return true;
}

/// Enumerate all of the applicable optional conversion restrictions
static void enumerateOptionalConversionRestrictions(
                    Type type1, Type type2,
                    ConstraintKind kind, ConstraintLocatorBuilder locator,
                    llvm::function_ref<void(ConversionRestrictionKind)> fn) {
  // Optional-to-optional.
  if (type1->getOptionalObjectType() && type2->getOptionalObjectType())
    fn(ConversionRestrictionKind::OptionalToOptional);

  // Inject a value into an optional.
  if (isPotentiallyMoreOptionalThan(type2, type1)) {
    fn(ConversionRestrictionKind::ValueToOptional);
  }
}

/// Determine whether we can bind the given type variable to the given
/// fixed type.
static bool isBindable(TypeVariableType *typeVar, Type type) {
  return !ConstraintSystem::typeVarOccursInType(typeVar, type) &&
         !type->is<DependentMemberType>();
}

ConstraintSystem::TypeMatchResult
ConstraintSystem::matchTypesBindTypeVar(
    TypeVariableType *typeVar, Type type, ConstraintKind kind,
    TypeMatchOptions flags, ConstraintLocatorBuilder locator,
    llvm::function_ref<TypeMatchResult()> formUnsolvedResult) {
  assert(typeVar->is<TypeVariableType>() && "Expected a type variable!");
  // FIXME: Due to some SE-0110 related code farther up we can end
  // up with type variables wrapped in parens that will trip this
  // assert. For now, maintain the existing behavior.
  // assert(!type->is<TypeVariableType>() && "Expected a non-type variable!");

  // Simplify the right-hand type and perform the "occurs" check.
  typeVar = getRepresentative(typeVar);
  type = simplifyType(type, flags);
  if (!isBindable(typeVar, type))
    return formUnsolvedResult();

  // Since member lookup doesn't check requirements
  // it might sometimes return types which are not
  // visible in the current context e.g. typealias
  // defined in constrained extension, substitution
  // of which might produce error type for base, so
  // assignement should thead lightly and just fail
  // if it encounters such types.
  if (type->hasError())
    return getTypeMatchFailure(locator);

  // Equal constraints allow mixed LValue/RValue bindings, but
  // if we bind a type to a type variable that can bind to
  // LValues as part of simplifying the Equal constraint we may
  // later block a binding of the opposite "LValue-ness" to the
  // same type variable that happens as part of simplifying
  // another constraint.
  if (kind == ConstraintKind::Equal) {
    if (typeVar->getImpl().canBindToLValue())
      return formUnsolvedResult();

    type = type->getRValueType();
  }

  // If the left-hand type variable cannot bind to an lvalue,
  // but we still have an lvalue, fail.
  if (!typeVar->getImpl().canBindToLValue() && type->hasLValueType()) {
    return getTypeMatchFailure(locator);
  }

  // Disallow bindings of noescape functions to type variables that
  // represent an opened archetype. If we allowed this it would allow
  // the noescape function to potentially escape.
  if (auto *fnTy = type->getAs<FunctionType>()) {
    if (fnTy->isNoEscape() && typeVar->getImpl().getGenericParameter()) {
      if (shouldAttemptFixes()) {
        auto *fix = MarkExplicitlyEscaping::create(
            *this, getConstraintLocator(locator));
        if (recordFix(fix))
          return getTypeMatchFailure(locator);

        // Allow no-escape function to be bound with recorded fix.
      } else {
        return getTypeMatchFailure(locator);
      }
    }
  }

  // Check whether the type variable must be bound to a materializable
  // type.
  if (typeVar->getImpl().mustBeMaterializable()) {
    if (!type->isMaterializable())
      return getTypeMatchFailure(locator);

    setMustBeMaterializableRecursive(type);
  }

  // Okay. Bind below.

  // A constraint that binds any pointer to a void pointer is
  // ineffective, since any pointer can be converted to a void pointer.
  if (kind == ConstraintKind::BindToPointerType && type->isVoid()) {
    // Bind type1 to Void only as a last resort.
    addConstraint(ConstraintKind::Defaultable, typeVar, type,
                  getConstraintLocator(locator));
    return getTypeMatchSuccess();
  }

  assignFixedType(typeVar, type);

  return getTypeMatchSuccess();
}

static void attemptToFixRequirementFailure(
    ConstraintSystem &cs, Type type1, Type type2,
    SmallVectorImpl<RestrictionOrFix> &conversionsOrFixes,
    ConstraintLocatorBuilder locator) {
  using LocatorPathEltKind = ConstraintLocator::PathElementKind;

  // Can't fix not yet properly resolved types.
  if (type1->hasTypeVariable() || type2->hasTypeVariable())
    return;

  // If dependent members are present here it's because
  // base doesn't conform to associated type's protocol.
  if (type1->hasDependentMember() || type2->hasDependentMember())
    return;

  SmallVector<LocatorPathElt, 4> path;
  auto *anchor = locator.getLocatorParts(path);

  if (path.empty())
    return;

  auto &elt = path.back();
  if (elt.getKind() != LocatorPathEltKind::TypeParameterRequirement)
    return;

  // Build simplified locator which only contains anchor and requirement info.
  ConstraintLocatorBuilder requirement(cs.getConstraintLocator(anchor));
  auto *reqLoc = cs.getConstraintLocator(requirement.withPathElement(elt));

  auto reqKind = static_cast<RequirementKind>(elt.getValue2());
  switch (reqKind) {
  case RequirementKind::SameType: {
    auto *fix = SkipSameTypeRequirement::create(cs, type1, type2, reqLoc);
    conversionsOrFixes.push_back(fix);
    return;
  }

  case RequirementKind::Superclass: {
    auto *fix = SkipSuperclassRequirement::create(cs, type1, type2, reqLoc);
    conversionsOrFixes.push_back(fix);
    return;
  }

  case RequirementKind::Conformance:
  case RequirementKind::Layout:
    llvm_unreachable("conformance requirements are handled elsewhere");
  }
}

ConstraintSystem::TypeMatchResult
ConstraintSystem::matchTypes(Type type1, Type type2, ConstraintKind kind,
                             TypeMatchOptions flags,
                             ConstraintLocatorBuilder locator) {
  // If we have type variables that have been bound to fixed types, look through
  // to the fixed type.
  type1 = getFixedTypeRecursive(type1, flags, kind == ConstraintKind::Equal);
  type2 = getFixedTypeRecursive(type2, flags, kind == ConstraintKind::Equal);

  auto desugar1 = type1->getDesugaredType();
  auto desugar2 = type2->getDesugaredType();

  auto *typeVar1 = desugar1->getAs<TypeVariableType>();
  auto *typeVar2 = desugar2->getAs<TypeVariableType>();

  // If the types are obviously equivalent, we're done.
  if (desugar1->isEqual(desugar2))
    return getTypeMatchSuccess();

  // Local function that should be used to produce the return value whenever
  // this function was unable to resolve the constraint. It should be used
  // within \c matchTypes() as
  //
  //   return formUnsolvedResult();
  //
  // along any unsolved path. No other returns should produce
  // SolutionKind::Unsolved or inspect TMF_GenerateConstraints.
  auto formUnsolvedResult = [&] {
    // If we're supposed to generate constraints (i.e., this is a
    // newly-generated constraint), do so now.
    if (flags.contains(TMF_GenerateConstraints)) {
      // Add a new constraint between these types. We consider the current
      // type-matching problem to the "solved" by this addition, because
      // this new constraint will be solved at a later point.
      // Obviously, this must not happen at the top level, or the
      // algorithm would not terminate.
      addUnsolvedConstraint(Constraint::create(*this, kind, type1, type2,
                                               getConstraintLocator(locator)));
      return getTypeMatchSuccess();
    }

    return getTypeMatchAmbiguous();
  };

  // If either (or both) types are type variables, unify the type variables.
  if (typeVar1 || typeVar2) {
    // Handle the easy case of both being type variables, and being
    // identical, first.
    if (typeVar1 && typeVar2) {
      auto rep1 = getRepresentative(typeVar1);
      auto rep2 = getRepresentative(typeVar2);
      if (rep1 == rep2) {
        // We already merged these two types, so this constraint is
        // trivially solved.
        return getTypeMatchSuccess();
      }
    }

    switch (kind) {
    case ConstraintKind::Bind:
    case ConstraintKind::BindToPointerType:
    case ConstraintKind::Equal: {
      if (typeVar1 && typeVar2) {
        auto rep1 = getRepresentative(typeVar1);
        auto rep2 = getRepresentative(typeVar2);

        // If exactly one of the type variables can bind to an lvalue, we
        // can't merge these two type variables.
        if (rep1->getImpl().canBindToLValue()
              != rep2->getImpl().canBindToLValue())
          return formUnsolvedResult();

        // Merge the equivalence classes corresponding to these two variables.
        mergeEquivalenceClasses(rep1, rep2);
        return getTypeMatchSuccess();
      }

      assert((type1->is<TypeVariableType>() || type2->is<TypeVariableType>()) &&
             "Expected a type variable!");
      // FIXME: Due to some SE-0110 related code farther up we can end
      // up with type variables wrapped in parens that will trip this
      // assert. For now, maintain the existing behavior.
      // assert(
      //     (!type1->is<TypeVariableType>() || !type2->is<TypeVariableType>())
      //     && "Expected a non-type variable!");

      auto *typeVar = typeVar1 ? typeVar1 : typeVar2;
      auto type = typeVar1 ? type2 : type1;

      return matchTypesBindTypeVar(typeVar, type, kind, flags, locator,
                                   formUnsolvedResult);
    }

    case ConstraintKind::BindParam: {
      if (typeVar2 && !typeVar1) {
        // Simplify the left-hand type and perform the "occurs" check.
        typeVar2 = getRepresentative(typeVar2);
        type1 = simplifyType(type1, flags);
        if (!isBindable(typeVar2, type1))
          return formUnsolvedResult();

        if (auto *iot = type1->getAs<InOutType>()) {
          assignFixedType(typeVar2, LValueType::get(iot->getObjectType()));
        } else {
          assignFixedType(typeVar2, type1);
        }
        return getTypeMatchSuccess();
      } else if (typeVar1 && !typeVar2) {
        // Simplify the right-hand type and perform the "occurs" check.
        typeVar1 = getRepresentative(typeVar1);
        type2 = simplifyType(type2, flags);
        if (!isBindable(typeVar1, type2))
          return formUnsolvedResult();

        // If the right-hand side of the BindParam constraint
        // is `lvalue` type, we'll have to make sure that
        // left-hand side is bound to type variable which
        // is wrapped in `inout` type to preserve inout/lvalue pairing.
        if (auto *lvt = type2->getAs<LValueType>()) {
          auto *tv = createTypeVariable(typeVar1->getImpl().getLocator());
          assignFixedType(typeVar1, InOutType::get(tv));

          typeVar1 = tv;
          type2 = lvt->getObjectType();
        }

        // If we have a binding for the right-hand side
        // (argument type used in the body) don't try
        // to bind it to the left-hand side (parameter type)
        // directly, because there could be an implicit
        // conversion between them, and actual binding
        // can only come from the left-hand side.
        addUnsolvedConstraint(
            Constraint::create(*this, ConstraintKind::Equal, typeVar1, type2,
                               getConstraintLocator(locator)));
        return getTypeMatchSuccess();
      }

      return formUnsolvedResult();
    }

    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
    case ConstraintKind::ArgumentConversion:
    case ConstraintKind::OperatorArgumentConversion:
      // We couldn't solve this constraint. If only one of the types is a type
      // variable, perhaps we can do something with it below.
      if (typeVar1 && typeVar2)
        return formUnsolvedResult();
      break;

    case ConstraintKind::ApplicableFunction:
    case ConstraintKind::BindOverload:
    case ConstraintKind::BridgingConversion:
    case ConstraintKind::CheckedCast:
    case ConstraintKind::ConformsTo:
    case ConstraintKind::Defaultable:
    case ConstraintKind::Disjunction:
    case ConstraintKind::DynamicTypeOf:
    case ConstraintKind::EscapableFunctionOf:
    case ConstraintKind::OpenedExistentialOf:
    case ConstraintKind::KeyPath:
    case ConstraintKind::KeyPathApplication:
    case ConstraintKind::LiteralConformsTo:
    case ConstraintKind::OptionalObject:
    case ConstraintKind::SelfObjectOfProtocol:
    case ConstraintKind::UnresolvedValueMember:
    case ConstraintKind::ValueMember:
    case ConstraintKind::FunctionInput:
    case ConstraintKind::FunctionResult:
      llvm_unreachable("Not a relational constraint");
    }
  }

  bool isTypeVarOrMember1 = desugar1->isTypeVariableOrMember();
  bool isTypeVarOrMember2 = desugar2->isTypeVariableOrMember();

  llvm::SmallVector<RestrictionOrFix, 4> conversionsOrFixes;
  bool concrete = !isTypeVarOrMember1 && !isTypeVarOrMember2;

  // Decompose parallel structure.
  TypeMatchOptions subflags =
    getDefaultDecompositionOptions(flags) - TMF_ApplyingFix;
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
        return getTypeMatchSuccess();
      }
      return getTypeMatchFailure(locator);

    case TypeKind::Error:
    case TypeKind::Unresolved:
        return getTypeMatchFailure(locator);

    case TypeKind::GenericTypeParam:
      llvm_unreachable("unmapped dependent type in type checker");

    case TypeKind::DependentMember:
      // Nothing we can solve.
      return formUnsolvedResult();

    case TypeKind::TypeVariable:
    case TypeKind::Archetype:
      // Nothing to do here; handle type variables and archetypes below.
      break;

    case TypeKind::Tuple: {
      assert(!type2->is<LValueType>() && "Unexpected lvalue type!");
      // Try the tuple-to-tuple conversion.
      if (!type1->is<LValueType>())
        conversionsOrFixes.push_back(ConversionRestrictionKind::TupleToTuple);
      break;
    }

    case TypeKind::Enum:
    case TypeKind::Struct:
    case TypeKind::Class: {
      auto nominal1 = cast<NominalType>(desugar1);
      auto nominal2 = cast<NominalType>(desugar2);
      assert(!type2->is<LValueType>() && "Unexpected lvalue type!");
      if (!type1->is<LValueType>() &&
          nominal1->getDecl() == nominal2->getDecl()) {
        conversionsOrFixes.push_back(ConversionRestrictionKind::DeepEquality);
      }

      // Check for CF <-> ObjectiveC bridging.
      if (isa<ClassType>(desugar1) &&
          kind >= ConstraintKind::Subtype) {
        auto class1 = cast<ClassDecl>(nominal1->getDecl());
        auto class2 = cast<ClassDecl>(nominal2->getDecl());

        // CF -> Objective-C via toll-free bridging.
        assert(!type2->is<LValueType>() && "Unexpected lvalue type!");
        if (!type1->is<LValueType>() &&
            class1->getForeignClassKind() == ClassDecl::ForeignKind::CFType &&
            class2->getForeignClassKind() != ClassDecl::ForeignKind::CFType &&
            class1->getAttrs().hasAttribute<ObjCBridgedAttr>()) {
          conversionsOrFixes.push_back(
            ConversionRestrictionKind::CFTollFreeBridgeToObjC);
        }

        // Objective-C -> CF via toll-free bridging.
        assert(!type2->is<LValueType>() && "Unexpected lvalue type!");
        if (!type1->is<LValueType>() &&
            class2->getForeignClassKind() == ClassDecl::ForeignKind::CFType &&
            class1->getForeignClassKind() != ClassDecl::ForeignKind::CFType &&
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

      ConstraintKind subKind = ConstraintKind::Equal;
      // A.Type < B.Type if A < B and both A and B are classes.
      if (isa<MetatypeType>(meta1) &&
          meta1->getInstanceType()->mayHaveSuperclass() &&
          meta2->getInstanceType()->getClassOrBoundGenericClass())
        subKind = std::min(kind, ConstraintKind::Subtype);
      // P.Type < Q.Type if P < Q, both P and Q are protocols, and P.Type
      // and Q.Type are both existential metatypes.
      else if (isa<ExistentialMetatypeType>(meta1))
        subKind = std::min(kind, ConstraintKind::Subtype);
      
      return matchTypes(meta1->getInstanceType(), meta2->getInstanceType(),
                        subKind, subflags,
                        locator.withPathElement(
                          ConstraintLocator::InstanceType));
    }

    case TypeKind::Function: {
      auto func1 = cast<FunctionType>(desugar1);
      auto func2 = cast<FunctionType>(desugar2);

      // If the 2nd type is an autoclosure, then we don't actually want to
      // treat these as parallel. The first type needs wrapping in a closure
      // despite already being a function type.
      if (!func1->isAutoClosure() && func2->isAutoClosure()) {
        increaseScore(SK_FunctionConversion);
        break;
      }
      return matchFunctionTypes(func1, func2, kind, flags, locator);
    }

    case TypeKind::GenericFunction:
      llvm_unreachable("Polymorphic function type should have been opened");

    case TypeKind::ProtocolComposition:
      // Existential types handled below.
      break;

    case TypeKind::LValue:
      if (kind == ConstraintKind::BindParam)
        return getTypeMatchFailure(locator);
      return matchTypes(cast<LValueType>(desugar1)->getObjectType(),
                        cast<LValueType>(desugar2)->getObjectType(),
                        ConstraintKind::Equal, subflags,
                        locator.withPathElement(
                          ConstraintLocator::LValueConversion));
    
    case TypeKind::InOut:
      // If the RHS is an inout type, the LHS must be an @lvalue type.
      if (kind == ConstraintKind::BindParam ||
          kind >= ConstraintKind::OperatorArgumentConversion)
        return getTypeMatchFailure(locator);
      
      return matchTypes(cast<InOutType>(desugar1)->getObjectType(),
                        cast<InOutType>(desugar2)->getObjectType(),
                        ConstraintKind::Equal, subflags,
                  locator.withPathElement(ConstraintLocator::LValueConversion));

    case TypeKind::UnboundGeneric:
      llvm_unreachable("Unbound generic type should have been opened");

    case TypeKind::BoundGenericClass:
    case TypeKind::BoundGenericEnum:
    case TypeKind::BoundGenericStruct: {
      auto bound1 = cast<BoundGenericType>(desugar1);
      auto bound2 = cast<BoundGenericType>(desugar2);
      
      assert(!type2->is<LValueType>() && "Unexpected lvalue type!");
      if (!type1->is<LValueType>() && bound1->getDecl() == bound2->getDecl()) {
        conversionsOrFixes.push_back(ConversionRestrictionKind::DeepEquality);
      }
      break;
    }
    }
  }

  if (concrete && kind >= ConstraintKind::Subtype) {
    // Subclass-to-superclass conversion.
    if (type1->mayHaveSuperclass() &&
        type2->getClassOrBoundGenericClass() &&
        type1->getClassOrBoundGenericClass()
          != type2->getClassOrBoundGenericClass()) {
      conversionsOrFixes.push_back(ConversionRestrictionKind::Superclass);
    }

    // Existential-to-superclass conversion.
    if (type1->isClassExistentialType() &&
        type2->getClassOrBoundGenericClass()) {
      conversionsOrFixes.push_back(ConversionRestrictionKind::Superclass);
    }

    // Metatype-to-existential-metatype conversion.
    //
    // Equivalent to a conformance relation on the instance types.
    if (type1->is<MetatypeType>() &&
        type2->is<ExistentialMetatypeType>()) {
      conversionsOrFixes.push_back(
        ConversionRestrictionKind::MetatypeToExistentialMetatype);
    }

    // Existential-metatype-to-superclass-metatype conversion.
    if (type2->is<MetatypeType>()) {
      if (auto *meta1 = type1->getAs<ExistentialMetatypeType>()) {
        if (meta1->getInstanceType()->isClassExistentialType()) {
          conversionsOrFixes.push_back(
            ConversionRestrictionKind::ExistentialMetatypeToMetatype);
        }
      }
    }

    // Concrete value to existential conversion.
    if (!type1->is<LValueType>() &&
        type2->isExistentialType()) {

      // Penalize conversions to Any, and disallow conversions of
      // noescape functions to Any.
      if (kind >= ConstraintKind::Conversion && type2->isAny()) {
        if (auto *fnTy = type1->getAs<FunctionType>()) {
          if (fnTy->isNoEscape()) {
            if (shouldAttemptFixes()) {
              auto &ctx = getASTContext();
              auto *fix = MarkExplicitlyEscaping::create(
                  *this, getConstraintLocator(locator), ctx.TheAnyType);
              if (recordFix(fix))
                return getTypeMatchFailure(locator);

              // Allow 'no-escape' functions to be converted to 'Any'
              // with a recorded fix that helps us to properly diagnose
              // such situations.
            } else {
              return getTypeMatchFailure(locator);
            }
          }
        }

        increaseScore(ScoreKind::SK_EmptyExistentialConversion);
      }

      conversionsOrFixes.push_back(ConversionRestrictionKind::Existential);
    }

    // T -> AnyHashable.
    if (isAnyHashableType(desugar2)) {
      // Don't allow this in operator contexts or we'll end up allowing
      // 'T() == U()' for unrelated T and U that just happen to be Hashable.
      // We can remove this special case when we implement operator hiding.
      if (!type1->is<LValueType>() &&
          kind != ConstraintKind::OperatorArgumentConversion) {
        assert(!type2->is<LValueType>() && "Unexpected lvalue type!");
        conversionsOrFixes.push_back(
                              ConversionRestrictionKind::HashableToAnyHashable);
      }
    }

    // Metatype to object conversion.
    //
    // Class and protocol metatypes are interoperable with certain Objective-C
    // runtime classes, but only when ObjC interop is enabled.
    
    if (TC.getLangOpts().EnableObjCInterop) {
      // These conversions are between concrete types that don't need further
      // resolution, so we can consider them immediately solved.
      auto addSolvedRestrictedConstraint
        = [&](ConversionRestrictionKind restriction) -> TypeMatchResult {
          addRestrictedConstraint(ConstraintKind::Subtype, restriction,
                                  type1, type2, locator);
          return getTypeMatchSuccess();
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
    
    // Special implicit nominal conversions.
    if (!type1->is<LValueType>() && kind >= ConstraintKind::Subtype) {
      // Array -> Array.
      if (isArrayType(desugar1) && isArrayType(desugar2)) {
        assert(!type2->is<LValueType>() && "Unexpected lvalue type!");
        conversionsOrFixes.push_back(ConversionRestrictionKind::ArrayUpcast);
      // Dictionary -> Dictionary.
      } else if (isDictionaryType(desugar1) && isDictionaryType(desugar2)) {
        assert(!type2->is<LValueType>() && "Unexpected lvalue type!");
        conversionsOrFixes.push_back(
          ConversionRestrictionKind::DictionaryUpcast);
      // Set -> Set.
      } else if (isSetType(desugar1) && isSetType(desugar2)) {
        assert(!type2->is<LValueType>() && "Unexpected lvalue type!");
        conversionsOrFixes.push_back(
          ConversionRestrictionKind::SetUpcast);
      }
    }
  }
  
  if (kind == ConstraintKind::BindToPointerType) {
    if (desugar2->isEqual(getASTContext().TheEmptyTupleType))
      return getTypeMatchSuccess();
  }

  if (concrete && kind >= ConstraintKind::Conversion) {
    // An lvalue of type T1 can be converted to a value of type T2 so long as
    // T1 is convertible to T2 (by loading the value).  Note that we cannot get
    // a value of inout type as an lvalue though.
    if (type1->is<LValueType>() && !type2->is<InOutType>())
      conversionsOrFixes.push_back(
        ConversionRestrictionKind::LValueToRValue);

    // An expression can be converted to an auto-closure function type, creating
    // an implicit closure.
    if (auto function2 = type2->getAs<FunctionType>()) {
      if (function2->isAutoClosure())
        return matchTypes(
            type1, function2->getResult(), kind, subflags,
            locator.withPathElement(ConstraintLocator::AutoclosureResult));
    }

    // It is never legal to form an autoclosure that results in these
    // implicit conversions to pointer types.
    bool isAutoClosureArgument = false;
    if (auto last = locator.last())
      if (last->getKind() == ConstraintLocator::AutoclosureResult)
        isAutoClosureArgument = true;

    // Pointer arguments can be converted from pointer-compatible types.
    if (kind >= ConstraintKind::ArgumentConversion) {
      Type unwrappedType2 = type2;
      bool type2IsOptional = false;
      if (Type unwrapped = type2->getOptionalObjectType()) {
        type2IsOptional = true;
        unwrappedType2 = unwrapped;
      }
      PointerTypeKind pointerKind;
      if (Type pointeeTy =
              unwrappedType2->getAnyPointerElementType(pointerKind)) {
        switch (pointerKind) {
        case PTK_UnsafeRawPointer:
        case PTK_UnsafeMutableRawPointer:
        case PTK_UnsafePointer:
        case PTK_UnsafeMutablePointer:
          // UnsafeMutablePointer can be converted from an inout reference to a
          // scalar or array.
          if (!isAutoClosureArgument) {
            if (auto inoutType1 = dyn_cast<InOutType>(desugar1)) {
              auto inoutBaseType = inoutType1->getInOutObjectType();

              Type simplifiedInoutBaseType = getFixedTypeRecursive(
                  inoutBaseType, kind == ConstraintKind::Equal);

              // FIXME: If the base is still a type variable, we can't tell
              // what to do here. Might have to try \c ArrayToPointer and make
              // it more robust.
              if (isArrayType(simplifiedInoutBaseType)) {
                conversionsOrFixes.push_back(
                    ConversionRestrictionKind::ArrayToPointer);
              }
              conversionsOrFixes.push_back(
                  ConversionRestrictionKind::InoutToPointer);
            }
          }

          // Operators cannot use these implicit conversions.
          if (kind == ConstraintKind::ArgumentConversion) {
            // We can potentially convert from an UnsafeMutablePointer
            // of a different type, if we're a void pointer.
            Type unwrappedType1 = type1;
            bool type1IsOptional = false;
            if (Type unwrapped = type1->getOptionalObjectType()) {
              type1IsOptional = true;
              unwrappedType1 = unwrapped;
            }

            // Don't handle normal optional-related conversions here.
            if (unwrappedType1->isEqual(unwrappedType2))
              break;

            PointerTypeKind type1PointerKind;
            bool type1IsPointer{
                unwrappedType1->getAnyPointerElementType(type1PointerKind)};
            bool optionalityMatches = !type1IsOptional || type2IsOptional;
            if (type1IsPointer && optionalityMatches) {
              if (type1PointerKind == PTK_UnsafeMutablePointer) {
                // Favor an UnsafeMutablePointer-to-UnsafeMutablePointer
                // conversion.
                if (type1PointerKind != pointerKind)
                  increaseScore(ScoreKind::SK_ValueToPointerConversion);
                conversionsOrFixes.push_back(
                  ConversionRestrictionKind::PointerToPointer);
              }
              // UnsafeMutableRawPointer -> UnsafeRawPointer
              else if (type1PointerKind == PTK_UnsafeMutableRawPointer &&
                       pointerKind == PTK_UnsafeRawPointer) {
                if (type1PointerKind != pointerKind)
                  increaseScore(ScoreKind::SK_ValueToPointerConversion);
                conversionsOrFixes.push_back(
                  ConversionRestrictionKind::PointerToPointer);              
              }
            }
            // UnsafePointer and UnsafeRawPointer can also be converted from an
            // array or string value, or a UnsafePointer or
            // AutoreleasingUnsafeMutablePointer.
            if (pointerKind == PTK_UnsafePointer
                || pointerKind == PTK_UnsafeRawPointer) {
              if (!isAutoClosureArgument) {
                if (isArrayType(type1)) {
                  conversionsOrFixes.push_back(
                      ConversionRestrictionKind::ArrayToPointer);
                }

                // The pointer can be converted from a string, if the element
                // type is compatible.
                if (type1->isEqual(TC.getStringType(DC))) {
                  auto baseTy = getFixedTypeRecursive(pointeeTy, false);

                  if (baseTy->isTypeVariableOrMember() ||
                      isStringCompatiblePointerBaseType(TC, DC, baseTy))
                    conversionsOrFixes.push_back(
                        ConversionRestrictionKind::StringToPointer);
                }
              }
              
              if (type1IsPointer && optionalityMatches &&
                  (type1PointerKind == PTK_UnsafePointer ||
                   type1PointerKind == PTK_AutoreleasingUnsafeMutablePointer)) {
                conversionsOrFixes.push_back(
                                   ConversionRestrictionKind::PointerToPointer);
              }
            }
          }
          break;

        case PTK_AutoreleasingUnsafeMutablePointer:
          // PTK_AutoreleasingUnsafeMutablePointer can be converted from an
          // inout reference to a scalar.
          if (!isAutoClosureArgument && type1->is<InOutType>()) {
            conversionsOrFixes.push_back(
                                     ConversionRestrictionKind::InoutToPointer);
          }
          break;
        }
      }
    }
  }

  if (concrete && kind >= ConstraintKind::OperatorArgumentConversion) {
    // If the RHS is an inout type, the LHS must be an @lvalue type.
    if (auto *lvt = type1->getAs<LValueType>()) {
      if (auto *iot = type2->getAs<InOutType>()) {
        return matchTypes(lvt->getObjectType(), iot->getObjectType(),
                          ConstraintKind::Equal, subflags,
                          locator.withPathElement(
                                  ConstraintLocator::LValueConversion));
      }
    }
  }

  // A value of type T! can be converted to type U if T is convertible
  // to U by force-unwrapping the source value.
  // A value of type T, T?, or T! can be converted to type U? or U! if
  // T is convertible to U.
  if (concrete && !type1->is<LValueType>() && kind >= ConstraintKind::Subtype) {
    enumerateOptionalConversionRestrictions(
        type1, type2, kind, locator,
        [&](ConversionRestrictionKind restriction) {
      conversionsOrFixes.push_back(restriction);
    });
  }

  // Allow '() -> T' to '() -> ()' and '() -> Never' to '() -> T' for closure
  // literals.
  if (auto elt = locator.last()) {
    if (elt->getKind() == ConstraintLocator::ClosureResult) {
      if (concrete && kind >= ConstraintKind::Subtype &&
          (type1->isUninhabited() || type2->isVoid())) {
        increaseScore(SK_FunctionConversion);
        return getTypeMatchSuccess();
      }
    }
  }

  if (concrete && kind == ConstraintKind::BindParam) {
    if (auto *iot = dyn_cast<InOutType>(desugar1)) {
      if (auto *lvt = dyn_cast<LValueType>(desugar2)) {
        return matchTypes(iot->getObjectType(), lvt->getObjectType(),
                          ConstraintKind::Bind, subflags,
                          locator.withPathElement(
                            ConstraintLocator::LValueConversion));
      }
    }
  }

  // Attempt fixes iff it's allowed, both types are concrete and
  // we are not in the middle of attempting one already.
  bool attemptFixes =
      shouldAttemptFixes() && concrete && !flags.contains(TMF_ApplyingFix);

  // When we hit this point, we're committed to the set of potential
  // conversions recorded thus far.
  //
  // If we should attempt fixes, add those to the list. They'll only be visited
  // if there are no other possible solutions.
  if (attemptFixes && kind >= ConstraintKind::Conversion) {
    Type objectType1 = type1->getRValueType();

    // If we have an optional type, try to force-unwrap it.
    // FIXME: Should we also try '?'?
    if (objectType1->getOptionalObjectType()) {
      bool forceUnwrapPossible = true;
      if (auto declRefExpr =
            dyn_cast_or_null<DeclRefExpr>(locator.trySimplifyToExpr())) {
        if (declRefExpr->getDecl()->isImplicit()) {
          // The expression that provides the first type is implicit and never
          // spelled out in source code, e.g. $match in an expression pattern.
          // Thus we cannot force unwrap the first type
          forceUnwrapPossible = false;
        }
      }

      if (forceUnwrapPossible) {
        conversionsOrFixes.push_back(
            ForceOptional::create(*this, getConstraintLocator(locator)));
      }
    }

    // If we have a value of type AnyObject that we're trying to convert to
    // a class, force a downcast.
    // FIXME: Also allow types bridged through Objective-C classes.
    if (objectType1->isAnyObject() &&
        type2->getClassOrBoundGenericClass()) {
      conversionsOrFixes.push_back(
          ForceDowncast::create(*this, type2, getConstraintLocator(locator)));
    }

    // If we could perform a bridging cast, try it.
    if (auto bridged =
            TC.getDynamicBridgedThroughObjCClass(DC, objectType1, type2)) {
      // Note: don't perform this recovery for NSNumber;
      bool useFix = true;
      if (auto classType = bridged->getAs<ClassType>()) {
        SmallString<16> scratch;
        if (classType->getDecl()->isObjC() &&
            classType->getDecl()->getObjCRuntimeName(scratch) == "NSNumber")
          useFix = false;
      }

      if (useFix)
        conversionsOrFixes.push_back(
            ForceDowncast::create(*this, type2, getConstraintLocator(locator)));
    }

    if (type2->getRValueType()->is<InOutType>()) {
      if (type1->is<LValueType>()) {
        // If we're converting an lvalue to an inout type, add the missing '&'.
        conversionsOrFixes.push_back(
          AddAddressOf::create(*this, getConstraintLocator(locator)));
      } else {
        // If we have a concrete type that's an rvalue, "fix" it.
        conversionsOrFixes.push_back(
          TreatRValueAsLValue::create(*this, getConstraintLocator(locator)));
      }
    }
  }

  if (attemptFixes && type2->is<LValueType>()) {
    conversionsOrFixes.push_back(
        TreatRValueAsLValue::create(*this, getConstraintLocator(locator)));
  } else if (attemptFixes && kind == ConstraintKind::Bind && type1->is<LValueType>()) {
    conversionsOrFixes.push_back(
          TreatRValueAsLValue::create(*this, getConstraintLocator(locator)));
  }

  if (attemptFixes)
    attemptToFixRequirementFailure(*this, type1, type2, conversionsOrFixes,
                                   locator);

  if (conversionsOrFixes.empty()) {
    // If one of the types is a type variable or member thereof, we leave this
    // unsolved.
    if (isTypeVarOrMember1 || isTypeVarOrMember2)
      return formUnsolvedResult();

    return getTypeMatchFailure(locator);
  }

  // Where there is more than one potential conversion, create a disjunction
  // so that we'll explore all of the options.
  if (conversionsOrFixes.size() > 1) {
    auto fixedLocator = getConstraintLocator(locator);
    SmallVector<Constraint *, 2> constraints;
    for (auto potential : conversionsOrFixes) {
      auto constraintKind = kind;

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

      auto fix = *potential.getFix();
      constraints.push_back(
        Constraint::createFixed(*this, constraintKind, fix, type1, type2,
                                fixedLocator));
    }
    addDisjunctionConstraint(constraints, fixedLocator);
    return getTypeMatchSuccess();
  }

  // For a single potential conversion, directly recurse, so that we
  // don't allocate a new constraint or constraint locator.

  auto formTypeMatchResult = [&](SolutionKind kind) {
    switch (kind) {
      case SolutionKind::Error:
        return getTypeMatchFailure(locator);

      case SolutionKind::Solved:
        return getTypeMatchSuccess();

      case SolutionKind::Unsolved:
        return getTypeMatchAmbiguous();
    }
    llvm_unreachable("unhandled kind");
  };

  // Handle restrictions.
  if (auto restriction = conversionsOrFixes[0].getRestriction()) {
    return formTypeMatchResult(simplifyRestrictedConstraint(*restriction, type1,
                                                            type2, kind,
                                                            subflags, locator));
  }

  // Handle fixes.
  auto fix = *conversionsOrFixes[0].getFix();
  return formTypeMatchResult(simplifyFixConstraint(fix, type1, type2, kind,
                                                   subflags, locator));
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstructionConstraint(
    Type valueType, FunctionType *fnType, TypeMatchOptions flags,
    DeclContext *useDC,
    FunctionRefKind functionRefKind, ConstraintLocator *locator) {

  // Desugar the value type.
  auto desugarValueType = valueType->getDesugaredType();

  switch (desugarValueType->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("Type has not been desugared completely");

#define ARTIFICIAL_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
      llvm_unreachable("artificial type in constraint");
    
  case TypeKind::Unresolved:
  case TypeKind::Error:
    return SolutionKind::Error;

  case TypeKind::GenericFunction:
  case TypeKind::GenericTypeParam:
    llvm_unreachable("unmapped dependent type");

  case TypeKind::TypeVariable:
  case TypeKind::DependentMember:
    return SolutionKind::Unsolved;

  case TypeKind::Tuple: {
    // Tuple construction is simply tuple conversion.
    Type argType = AnyFunctionType::composeInput(getASTContext(),
                                                 fnType->getParams(),
                                                 /*canonicalVararg=*/false);
    Type resultType = fnType->getResult();

    if (matchTypes(resultType, desugarValueType,
                   ConstraintKind::Bind,
                   flags,
                   ConstraintLocatorBuilder(locator)
                     .withPathElement(ConstraintLocator::ApplyFunction))
        .isFailure())
      return SolutionKind::Error;

    return matchTypes(argType, valueType, ConstraintKind::Conversion,
                      getDefaultDecompositionOptions(flags), locator);
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
    return SolutionKind::Error;
  }

  auto fnLocator = getConstraintLocator(locator,
                                        ConstraintLocator::ApplyFunction);
  auto memberType = createTypeVariable(fnLocator);

  // The constructor will have function type T -> T2, for a fresh type
  // variable T. T2 is the result type provided via the construction
  // constraint itself.
  addValueMemberConstraint(MetatypeType::get(valueType, TC.Context),
                           DeclBaseName::createConstructor(),
                           memberType,
                           useDC, functionRefKind,
                           /*outerAlternatives=*/{},
                           getConstraintLocator(
                             fnLocator, 
                             ConstraintLocator::ConstructorMember));

  // FIXME: Once TVO_PrefersSubtypeBinding is replaced with something
  // better, we won't need the second type variable at all.
  {
    auto argType = createTypeVariable(
        getConstraintLocator(locator, ConstraintLocator::ApplyArgument),
        (TVO_CanBindToLValue |
         TVO_PrefersSubtypeBinding));
    addConstraint(ConstraintKind::FunctionInput, memberType, argType, locator);
  }

  addConstraint(ConstraintKind::ApplicableFunction, fnType, memberType,
                fnLocator);

  return SolutionKind::Solved;
}

ConstraintSystem::SolutionKind ConstraintSystem::simplifyConformsToConstraint(
                                 Type type,
                                 Type protocol,
                                 ConstraintKind kind,
                                 ConstraintLocatorBuilder locator,
                                 TypeMatchOptions flags) {
  if (auto proto = protocol->getAs<ProtocolType>()) {
    return simplifyConformsToConstraint(type, proto->getDecl(), kind,
                                        locator, flags);
  }

  // Dig out the fixed type to which this type refers.
  type = getFixedTypeRecursive(type, flags, /*wantRValue=*/true);

  return matchExistentialTypes(type, protocol, kind, flags, locator);
}

ConstraintSystem::SolutionKind ConstraintSystem::simplifyConformsToConstraint(
                                 Type type,
                                 ProtocolDecl *protocol,
                                 ConstraintKind kind,
                                 ConstraintLocatorBuilder locator,
                                 TypeMatchOptions flags) {
  // Dig out the fixed type to which this type refers.
  type = getFixedTypeRecursive(type, flags, /*wantRValue=*/true);

  // If we hit a type variable without a fixed type, we can't
  // solve this yet.
  if (type->isTypeVariableOrMember()) {
    // If we're supposed to generate constraints, do so.
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::create(*this, kind, type, protocol->getDeclaredType(),
                           getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;
  }

  /// Record the given conformance as the result, adding any conditional
  /// requirements if necessary.
  auto recordConformance = [&](ProtocolConformanceRef conformance) {
    // Record the conformance.
    CheckedConformances.push_back({getConstraintLocator(locator), conformance});

    // This conformance may be conditional, in which case we need to consider
    // those requirements as constraints too.
    if (conformance.isConcrete()) {
      unsigned index = 0;
      for (const auto &req : conformance.getConditionalRequirements()) {
        addConstraint(
          req,
          locator.withPathElement(
            LocatorPathElt::getConditionalRequirementComponent(index++)));
      }
    }

    return SolutionKind::Solved;
  };

  // For purposes of argument type matching, existential types don't need to
  // conform -- they only need to contain the protocol, so check that
  // separately.
  switch (kind) {
  case ConstraintKind::SelfObjectOfProtocol:
    if (auto conformance =
          TC.containsProtocol(type, protocol, DC,
                              (ConformanceCheckFlags::InExpression|
                               ConformanceCheckFlags::SkipConditionalRequirements))) {
      return recordConformance(*conformance);
    }
    break;
  case ConstraintKind::ConformsTo:
  case ConstraintKind::LiteralConformsTo: {
    // Check whether this type conforms to the protocol.
    if (auto conformance =
          TC.conformsToProtocol(
                      type, protocol, DC,
                      (ConformanceCheckFlags::InExpression|
                       ConformanceCheckFlags::SkipConditionalRequirements))) {
      return recordConformance(*conformance);
    }
    break;
  }

  default:
    llvm_unreachable("bad constraint kind");
  }
  
  if (!shouldAttemptFixes())
    return SolutionKind::Error;

  // See if there's anything we can do to fix the conformance:
  if (auto optionalObjectType = type->getOptionalObjectType()) {
    TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);
    // The underlying type of an optional may conform to the protocol if the
    // optional doesn't; suggest forcing if that's the case.
    auto result = simplifyConformsToConstraint(
        optionalObjectType, protocol, kind,
        locator.withPathElement(LocatorPathElt::getGenericArgument(0)),
        subflags);
    if (result == SolutionKind::Solved) {
      auto *fix = ForceOptional::create(*this, getConstraintLocator(locator));
      if (recordFix(fix)) {
        return SolutionKind::Error;
      }
    }
    return result;
  }

  // Let's not try to fix missing conformance for Void
  // and Never because that doesn't really make sense.
  if (type->isVoid() || type->isUninhabited())
    return SolutionKind::Error;

  // If this is a generic requirement let's try to record that
  // conformance is missing and consider this a success, which
  // makes it much easier to diagnose problems like that.
  {
    SmallVector<LocatorPathElt, 4> path;
    auto *anchor = locator.getLocatorParts(path);

    if (!path.empty() && path.back().getKind() ==
        ConstraintLocator::PathElementKind::TypeParameterRequirement) {
      auto typeRequirement = path.back();
      // Let's strip all of the unnecessary information from locator,
      // diagnostics only care about anchor - to lookup type,
      // and what was the requirement# which is not satisfied.
      ConstraintLocatorBuilder requirement(getConstraintLocator(anchor));
      auto *reqLoc =
          getConstraintLocator(requirement.withPathElement(typeRequirement));

      auto *fix = MissingConformance::create(*this, type, protocol, reqLoc);
      if (!recordFix(fix))
        return SolutionKind::Solved;
    }
  }
  
  // There's nothing more we can do; fail.
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

  return CheckedCastKind::ValueCast;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyCheckedCastConstraint(
                    Type fromType, Type toType,
                    TypeMatchOptions flags,
                    ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  /// Form an unresolved result.
  auto formUnsolved = [&] {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::create(*this, ConstraintKind::CheckedCast, fromType,
                           toType, getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;
  };

  do {
    // Dig out the fixed type this type refers to.
    fromType = getFixedTypeRecursive(fromType, flags, /*wantRValue=*/true);

    // If we hit a type variable without a fixed type, we can't
    // solve this yet.
    if (fromType->isTypeVariableOrMember())
      return formUnsolved();

    // Dig out the fixed type this type refers to.
    toType = getFixedTypeRecursive(toType, flags, /*wantRValue=*/true);

    // If we hit a type variable without a fixed type, we can't
    // solve this yet.
    if (toType->isTypeVariableOrMember())
      return formUnsolved();

    Type origFromType = fromType;
    Type origToType = toType;

    // Peel off optionals metatypes from the types, because we might cast through
    // them.
    toType = toType->lookThroughAllOptionalTypes();
    fromType = fromType->lookThroughAllOptionalTypes();

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

    // We've decomposed the types further, so adopt the subflags.
    flags = subflags;

    // If nothing changed, we're done.
    if (fromType.getPointer() == origFromType.getPointer() &&
        toType.getPointer() == origToType.getPointer())
      break;
  } while (true);

  auto kind = getCheckedCastKind(this, fromType, toType);
  switch (kind) {
  case CheckedCastKind::ArrayDowncast: {
    auto fromBaseType = *isArrayType(fromType);
    auto toBaseType = *isArrayType(toType);

    return simplifyCheckedCastConstraint(fromBaseType, toBaseType, subflags,
                                         locator);
  }
  case CheckedCastKind::DictionaryDowncast: {
    Type fromKeyType, fromValueType;
    std::tie(fromKeyType, fromValueType) = *isDictionaryType(fromType);

    Type toKeyType, toValueType;
    std::tie(toKeyType, toValueType) = *isDictionaryType(toType);

    if (simplifyCheckedCastConstraint(fromKeyType, toKeyType, subflags,
                                      locator) == SolutionKind::Error)
      return SolutionKind::Error;


    return simplifyCheckedCastConstraint(fromValueType, toValueType, subflags,
                                         locator);
  }

  case CheckedCastKind::SetDowncast: {
    auto fromBaseType = *isSetType(fromType);
    auto toBaseType = *isSetType(toType);
    return simplifyCheckedCastConstraint(fromBaseType, toBaseType, subflags,
                                         locator);
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

  case CheckedCastKind::Coercion:
  case CheckedCastKind::BridgingCoercion:
  case CheckedCastKind::Unresolved:
    llvm_unreachable("Not a valid result");
  }

  llvm_unreachable("Unhandled CheckedCastKind in switch.");
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyOptionalObjectConstraint(
                                           Type first, Type second,
                                           TypeMatchOptions flags,
                                           ConstraintLocatorBuilder locator) {
  // Resolve the optional type.
  Type optLValueTy = getFixedTypeRecursive(first, flags, /*wantRValue=*/false);
  Type optTy = optLValueTy->getRValueType();
  if (optTy.getPointer() != optLValueTy.getPointer())
    optTy = getFixedTypeRecursive(optTy, /*wantRValue=*/false);

  if (optTy->isTypeVariableOrMember()) {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::create(*this, ConstraintKind::OptionalObject, optLValueTy,
                           second, getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;
  }
  
  // If the base type is not optional, the constraint fails.
  Type objectTy = optTy->getOptionalObjectType();
  if (!objectTy)
    return SolutionKind::Error;
  
  // The object type is an lvalue if the optional was.
  if (optLValueTy->is<LValueType>())
    objectTy = LValueType::get(objectTy);

  // Equate it to the other type in the constraint.
  addConstraint(ConstraintKind::Bind, objectTy, second, locator);
  return SolutionKind::Solved;
}

/// \brief Attempt to simplify a function input or result constraint.
ConstraintSystem::SolutionKind
ConstraintSystem::simplifyFunctionComponentConstraint(
                                        ConstraintKind kind,
                                        Type first, Type second,
                                        TypeMatchOptions flags,
                                        ConstraintLocatorBuilder locator) {
  auto simplified = simplifyType(first);

  unsigned unwrapCount = 0;
  if (shouldAttemptFixes()) {
    while (auto objectTy = simplified->getOptionalObjectType()) {
      simplified = objectTy;

      // Track how many times we do this so that we can record a fix for each.
      ++unwrapCount;
    }
  }

  if (simplified->isTypeVariableOrMember()) {
    if (!flags.contains(TMF_GenerateConstraints))
      return SolutionKind::Unsolved;

    addUnsolvedConstraint(
      Constraint::create(*this, kind, simplified, second,
                         getConstraintLocator(locator)));
  } else if (auto *funcTy = simplified->getAs<FunctionType>()) {
    // Equate it to the other type in the constraint.
    Type type;
    ConstraintLocator::PathElementKind locKind;

    if (kind == ConstraintKind::FunctionInput) {
      type = AnyFunctionType::composeInput(getASTContext(),
                                           funcTy->getParams(),
                                           /*canonicalVararg=*/false);
      locKind = ConstraintLocator::FunctionArgument;
    } else if (kind == ConstraintKind::FunctionResult) {
      type = funcTy->getResult();
      locKind = ConstraintLocator::FunctionResult;
    } else {
      llvm_unreachable("Bad function component constraint kind");
    }

    addConstraint(ConstraintKind::Bind, type, second,
                  locator.withPathElement(locKind));
  } else {
    return SolutionKind::Error;
  }

  if (unwrapCount > 0) {
    auto *fix = ForceOptional::create(*this, getConstraintLocator(locator));
    while (unwrapCount-- > 0) {
      if (recordFix(fix))
        return SolutionKind::Error;
    }
  }

  return SolutionKind::Solved;
}

/// Retrieve the argument labels that are provided for a member
/// reference at the given locator.
static Optional<ConstraintSystem::ArgumentLabelState>
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

    if (parts.back().getKind() == ConstraintLocator::ApplyFunction) {
      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        anchor = applyExpr->getSemanticFn();
      }
      parts.pop_back();
      continue;
    }

    if (parts.back().getKind() == ConstraintLocator::ConstructorMember) {
      // FIXME: Workaround for strange anchor on ConstructorMember locators.

      if (auto optionalWrapper = dyn_cast<BindOptionalExpr>(anchor))
        anchor = optionalWrapper->getSubExpr();
      else if (auto forceWrapper = dyn_cast<ForceValueExpr>(anchor))
        anchor = forceWrapper->getSubExpr();

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


/// Return true if the specified type or a super-class/super-protocol has the
/// @dynamicMemberLookup attribute on it.  This implementation is not
/// particularly fast in the face of deep class hierarchies or lots of protocol
/// conformances, but this is fine because it doesn't get invoked in the normal
/// name lookup path (only when lookup is about to fail).
static bool hasDynamicMemberLookupAttribute(CanType ty,
                    llvm::DenseMap<CanType, bool> &IsDynamicMemberLookupCache) {
  auto it = IsDynamicMemberLookupCache.find(ty);
  if (it != IsDynamicMemberLookupCache.end()) return it->second;
  
  auto calculate = [&]()-> bool {
    // If this is a protocol composition, check to see if any of the protocols
    // have the attribute on them.
    if (auto protocolComp = ty->getAs<ProtocolCompositionType>()) {
      for (auto p : protocolComp->getMembers())
        if (hasDynamicMemberLookupAttribute(p->getCanonicalType(),
                                            IsDynamicMemberLookupCache))
          return true;
      return false;
    }
    
    // Otherwise this has to be a nominal type.
    auto nominal = ty->getAnyNominal();
    if (!nominal) return false;  // Dynamic lookups don't exist on tuples, etc.
    
    // If any of the protocols this type conforms to has the attribute, then
    // yes.
    for (auto p : nominal->getAllProtocols())
      if (p->getAttrs().hasAttribute<DynamicMemberLookupAttr>())
        return true;
    
    // Walk superclasses, if present.
    llvm::SmallPtrSet<const NominalTypeDecl*, 8> visitedDecls;
    while (1) {
      // If we found a circular parent class chain, reject this.
      if (!visitedDecls.insert(nominal).second)
        return false;
      
      // If this type has the attribute on it, then yes!
      if (nominal->getAttrs().hasAttribute<DynamicMemberLookupAttr>())
        return true;
      
      // If this is a class with a super class, check super classes as well.
      if (auto *cd = dyn_cast<ClassDecl>(nominal)) {
        if (auto superClass = cd->getSuperclassDecl()) {
          nominal = superClass;
          continue;
        }
      }
      
      return false;
    }
  };
  
  auto result = calculate();
  
  // Cache this if we can.
  if (!ty->hasTypeVariable())
    IsDynamicMemberLookupCache[ty] = result;
  
  return result;
}


/// Given a ValueMember, UnresolvedValueMember, or TypeMember constraint,
/// perform a lookup into the specified base type to find a candidate list.
/// The list returned includes the viable candidates as well as the unviable
/// ones (along with reasons why they aren't viable).
///
/// If includeInaccessibleMembers is set to true, this burns compile time to
/// try to identify and classify inaccessible members that may be being
/// referenced.
MemberLookupResult ConstraintSystem::
performMemberLookup(ConstraintKind constraintKind, DeclName memberName,
                    Type baseTy, FunctionRefKind functionRefKind,
                    ConstraintLocator *memberLocator,
                    bool includeInaccessibleMembers) {
  Type baseObjTy = baseTy->getRValueType();
  Type instanceTy = baseObjTy;

  if (auto baseObjMeta = baseObjTy->getAs<AnyMetatypeType>()) {
    instanceTy = baseObjMeta->getInstanceType();
  }

  if (instanceTy->isTypeVariableOrMember() ||
      instanceTy->is<UnresolvedType>()) {
    MemberLookupResult result;
    result.OverallResult = MemberLookupResult::Unsolved;
    return result;
  }

  // Okay, start building up the result list.
  MemberLookupResult result;
  result.OverallResult = MemberLookupResult::HasResults;
  
  // If we're looking for a subscript, consider key path operations.
  if (memberName.isSimpleName() &&
      memberName.getBaseName().getKind() == DeclBaseName::Kind::Subscript) {
    result.ViableCandidates.push_back(
        OverloadChoice(baseTy, OverloadChoiceKind::KeyPathApplication));
  }

  // If the base type is a tuple type, look for the named or indexed member
  // of the tuple.
  if (auto baseTuple = baseObjTy->getAs<TupleType>()) {
    // Tuples don't have compound-name members.
    if (!memberName.isSimpleName() || memberName.isSpecial())
      return result;  // No result.

    StringRef nameStr = memberName.getBaseIdentifier().str();
    int fieldIdx = -1;
    // Resolve a number reference into the tuple type.
    unsigned Value = 0;
    if (!nameStr.getAsInteger(10, Value) &&
        Value < baseTuple->getNumElements()) {
      fieldIdx = Value;
    } else {
      fieldIdx = baseTuple->getNamedElementId(memberName.getBaseIdentifier());
    }
    
    if (fieldIdx == -1)
      return result;    // No result.
    
    // Add an overload set that selects this field.
    result.ViableCandidates.push_back(OverloadChoice(baseTy, fieldIdx));
    return result;
  }

  if (auto *selfTy = instanceTy->getAs<DynamicSelfType>())
    instanceTy = selfTy->getSelfType();

  if (!instanceTy->mayHaveMembers())
    return result;

  // If we have a simple name, determine whether there are argument
  // labels we can use to restrict the set of lookup results.
  Optional<ArgumentLabelState> argumentLabels;
  if (memberName.isSimpleName()) {
    argumentLabels = getArgumentLabels(*this,
                                       ConstraintLocatorBuilder(memberLocator));

    // If we're referencing AnyObject and we have argument labels, put
    // the argument labels into the name: we don't want to look for
    // anything else, because the cost of the general search is so
    // high.
    if (baseObjTy->isAnyObject() && argumentLabels) {
      memberName = DeclName(TC.Context, memberName.getBaseName(),
                            argumentLabels->Labels);
      argumentLabels.reset();
    }
  }

  /// Determine whether the given declaration has compatible argument
  /// labels.
  auto hasCompatibleArgumentLabels = [&argumentLabels](Type baseObjTy,
                                                       ValueDecl *decl) -> bool {
    if (!argumentLabels)
      return true;

    // This is a member lookup, which generally means that the call arguments
    // (if we have any) will apply to the second level of parameters, with
    // the member lookup binding the first level.  But there are cases where
    // we can get an unapplied declaration reference back.
    bool hasCurriedSelf;
    if (baseObjTy->is<ModuleType>()) {
      hasCurriedSelf = false;
    } else if (baseObjTy->is<AnyMetatypeType>() && decl->isInstanceMember()) {
      hasCurriedSelf = false;
    } else {
      hasCurriedSelf = true;
    }

    return areConservativelyCompatibleArgumentLabels(decl, hasCurriedSelf,
                                          argumentLabels->Labels,
                                          argumentLabels->HasTrailingClosure);
  };

  // Look for members within the base.
  LookupResult &lookup = lookupMember(instanceTy, memberName);

  // If this is true, we're using type construction syntax (Foo()) rather
  // than an explicit call to `init` (Foo.init()).
  bool isImplicitInit = false;
  TypeBase *favoredType = nullptr;
  if (memberName.isSimpleName(DeclBaseName::createConstructor())) {
    SmallVector<LocatorPathElt, 2> parts;
    if (auto *anchor = memberLocator->getAnchor()) {
      auto path = memberLocator->getPath();
      if (!path.empty())
        if (path.back().getKind() == ConstraintLocator::ConstructorMember)
          isImplicitInit = true;

      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        auto argExpr = applyExpr->getArg();
        favoredType = getFavoredType(argExpr);

        if (!favoredType) {
          optimizeConstraints(argExpr);
          favoredType = getFavoredType(argExpr);
        }
      }
    }
  }

  // If the instance type is String bridged to NSString, compute
  // the type we'll look in for bridging.
  Type bridgedType;
  if (baseObjTy->getAnyNominal() == TC.Context.getStringDecl()) {
    if (Type classType = TC.Context.getBridgedToObjC(DC, instanceTy)) {
      bridgedType = classType;
    }
  }
  bool labelMismatch = false;

  // Local function that adds the given declaration if it is a
  // reasonable choice.
  auto addChoice = [&](OverloadChoice candidate) {
    auto decl = candidate.getDecl();
    
    // If the result is invalid, skip it.
    TC.validateDecl(decl);
    if (decl->isInvalid()) {
      result.markErrorAlreadyDiagnosed();
      return;
    }

    // FIXME: Deal with broken recursion
    if (!decl->hasInterfaceType())
      return;

    // Dig out the instance type and figure out what members of the instance type
    // we are going to see.
    auto baseTy = candidate.getBaseType();
    auto baseObjTy = baseTy->getRValueType();

    bool hasInstanceMembers = false;
    bool hasInstanceMethods = false;
    bool hasStaticMembers = false;
    Type instanceTy = baseObjTy;
    if (baseObjTy->is<ModuleType>()) {
      hasStaticMembers = true;
    } else if (auto baseObjMeta = baseObjTy->getAs<AnyMetatypeType>()) {
      instanceTy = baseObjMeta->getInstanceType();
      if (baseObjMeta->is<ExistentialMetatypeType>()) {
        // An instance of an existential metatype is a concrete type conforming
        // to the existential, say Self. Instance members of the concrete type
        // have type Self -> T -> U, but we don't know what Self is at compile
        // time so we cannot refer to them. Static methods are fine, on the other
        // hand -- we already know that they do not have Self or associated type
        // requirements, since otherwise we would not be able to refer to the
        // existential metatype in the first place.
        hasStaticMembers = true;
      } else if (instanceTy->isExistentialType()) {
        // A protocol metatype has instance methods with type P -> T -> U, but
        // not instance properties or static members -- the metatype value itself
        // doesn't give us a witness so there's no static method to bind.
        hasInstanceMethods = true;
      } else {
        // Metatypes of nominal types and archetypes have instance methods and
        // static members, but not instance properties.
        // FIXME: partial application of properties
        hasInstanceMethods = true;
        hasStaticMembers = true;
      }

      // If we're at the root of an unevaluated context, we can
      // reference instance members on the metatype.
      if (memberLocator &&
          UnevaluatedRootExprs.count(memberLocator->getAnchor())) {
        hasInstanceMembers = true;
      }
    } else {
      // Otherwise, we can access all instance members.
      hasInstanceMembers = true;
      hasInstanceMethods = true;
    }

    // If the argument labels for this result are incompatible with
    // the call site, skip it.
    if (!hasCompatibleArgumentLabels(baseObjTy, decl)) {
      labelMismatch = true;
      result.addUnviable(candidate, MemberLookupResult::UR_LabelMismatch);
      return;
    }

    // If our base is an existential type, we can't make use of any
    // member whose signature involves associated types.
    if (instanceTy->isExistentialType()) {
      if (auto *proto = decl->getDeclContext()->getSelfProtocolDecl()) {
        if (!proto->isAvailableInExistential(decl)) {
          result.addUnviable(candidate,
                             MemberLookupResult::UR_UnavailableInExistential);
          return;
        }
      }
    }

    // If the invocation's argument expression has a favored type,
    // use that information to determine whether a specific overload for
    // the candidate should be favored.
    if (isa<ConstructorDecl>(decl) && favoredType &&
        result.FavoredChoice == ~0U) {
      auto *ctor = cast<ConstructorDecl>(decl);

      // Only try and favor monomorphic initializers.
      if (!ctor->isGenericContext()) {
        auto args = ctor->getMethodInterfaceType()
                        ->castTo<FunctionType>()->getParams();
        auto argType = AnyFunctionType::composeInput(getASTContext(), args,
                                                     /*canonicalVarargs=*/false);
        if (argType->isEqual(favoredType))
          if (!decl->getAttrs().isUnavailable(getASTContext()))
            result.FavoredChoice = result.ViableCandidates.size();
      }
    }

    // See if we have an instance method, instance member or static method,
    // and check if it can be accessed on our base type.
    if (decl->isInstanceMember()) {
      if ((isa<FuncDecl>(decl) && !hasInstanceMethods) ||
          (!isa<FuncDecl>(decl) && !hasInstanceMembers)) {
        result.addUnviable(candidate,
                           MemberLookupResult::UR_InstanceMemberOnType);
        return;
      }

    // If the underlying type of a typealias is fully concrete, it is legal
    // to access the type with a protocol metatype base.
    } else if (instanceTy->isExistentialType() &&
               isa<TypeAliasDecl>(decl) &&
               !cast<TypeAliasDecl>(decl)->getInterfaceType()->hasTypeParameter()) {

      /* We're OK */

    } else {
      if (!hasStaticMembers) {
        result.addUnviable(candidate,
                           MemberLookupResult::UR_TypeMemberOnInstance);
        return;
      }
    }

    // If we have an rvalue base, make sure that the result isn't 'mutating'
    // (only valid on lvalues).
    if (!baseTy->is<AnyMetatypeType>() &&
        !baseTy->is<LValueType>() &&
        decl->isInstanceMember()) {
      if (auto *FD = dyn_cast<FuncDecl>(decl))
        if (FD->isMutating()) {
          result.addUnviable(candidate,
                             MemberLookupResult::UR_MutatingMemberOnRValue);
          return;
        }

      // Subscripts and computed properties are ok on rvalues so long
      // as the getter is nonmutating.
      if (auto storage = dyn_cast<AbstractStorageDecl>(decl)) {
        if (storage->isGetterMutating()) {
          result.addUnviable(candidate,
                             MemberLookupResult::UR_MutatingGetterOnRValue);
          return;
        }
      }
    }

    // Otherwise, we're good, add the candidate to the list.
    result.addViable(candidate);
  };

  // Local function that turns a ValueDecl into a properly configured
  // OverloadChoice.
  auto getOverloadChoice = [&](ValueDecl *cand, bool isBridged,
                               bool isUnwrappedOptional) -> OverloadChoice {
    // If we're looking into an existential type, check whether this
    // result was found via dynamic lookup.
    if (instanceTy->isAnyObject()) {
      assert(cand->getDeclContext()->isTypeContext() && "Dynamic lookup bug");
      
      // We found this declaration via dynamic lookup, record it as such.
      return OverloadChoice::getDeclViaDynamic(baseTy, cand, functionRefKind);
    }
    
    // If we have a bridged type, we found this declaration via bridging.
    if (isBridged)
      return OverloadChoice::getDeclViaBridge(bridgedType, cand,
                                              functionRefKind);
    
    // If we got the choice by unwrapping an optional type, unwrap the base
    // type.
    if (isUnwrappedOptional) {
      auto ovlBaseTy = MetatypeType::get(baseTy->castTo<MetatypeType>()
                                             ->getInstanceType()
                                             ->getOptionalObjectType());
      return OverloadChoice::getDeclViaUnwrappedOptional(ovlBaseTy, cand,
                                                         functionRefKind);
    }

    return OverloadChoice(baseTy, cand, functionRefKind);
  };
  
  // Add all results from this lookup.
retry_after_fail:
  labelMismatch = false;
  for (auto result : lookup)
    addChoice(getOverloadChoice(result.getValueDecl(),
                                /*isBridged=*/false,
                                /*isUnwrappedOptional=*/false));

  // Backward compatibility hack. In Swift 4, `init` and init were
  // the same name, so you could write "foo.init" to look up a
  // method or property named `init`.
  if (!TC.Context.isSwiftVersionAtLeast(5) &&
      memberName.getBaseName() == DeclBaseName::createConstructor() &&
      !isImplicitInit) {
    auto &compatLookup = lookupMember(instanceTy,
                                      TC.Context.getIdentifier("init"));
    for (auto result : compatLookup)
      addChoice(getOverloadChoice(result.getValueDecl(),
                                  /*isBridged=*/false,
                                  /*isUnwrappedOptional=*/false));
  }

  // If the instance type is a bridged to an Objective-C type, perform
  // a lookup into that Objective-C type.
  if (bridgedType) {
    LookupResult &bridgedLookup = lookupMember(bridgedType, memberName);
    ModuleDecl *foundationModule = nullptr;
    for (auto result : bridgedLookup) {
      // Ignore results from the Objective-C "Foundation"
      // module. Those core APIs are explicitly provided by the
      // Foundation module overlay.
      auto module = result.getValueDecl()->getModuleContext();
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
      
      addChoice(getOverloadChoice(result.getValueDecl(),
                                  /*isBridged=*/true,
                                  /*isUnwrappedOptional=*/false));
    }
  }

  // If we're looking into a metatype for an unresolved member lookup, look
  // through optional types.
  //
  // FIXME: The short-circuit here is lame.
  if (result.ViableCandidates.empty() &&
      baseObjTy->is<AnyMetatypeType>() &&
      constraintKind == ConstraintKind::UnresolvedValueMember) {
    if (auto objectType = instanceTy->getOptionalObjectType()) {
      if (objectType->mayHaveMembers()) {
        LookupResult &optionalLookup = lookupMember(objectType, memberName);
        for (auto result : optionalLookup)
          addChoice(getOverloadChoice(result.getValueDecl(),
                                      /*bridged*/false,
                                      /*isUnwrappedOptional=*/true));
      }
    }
  }
  
  // If we're about to fail lookup, but we are looking for members in a type
  // that has the @dynamicMemberLookup attribute, then we resolve the reference
  // to the subscript(dynamicMember:) member, and pass the member name as a
  // string.
  if (result.ViableCandidates.empty() &&
      constraintKind == ConstraintKind::ValueMember &&
      memberName.isSimpleName() && !memberName.isSpecial()) {
    auto name = memberName.getBaseIdentifier();
    if (hasDynamicMemberLookupAttribute(instanceTy->getCanonicalType(),
                                        IsDynamicMemberLookupCache)) {
      auto &ctx = getASTContext();
      // Recursively look up the subscript(dynamicMember:)'s in this type.
      auto subscriptName =
        DeclName(ctx, DeclBaseName::createSubscript(), ctx.Id_dynamicMember);

      auto subscripts = performMemberLookup(constraintKind,
                                            subscriptName,
                                            baseTy, functionRefKind,
                                            memberLocator,
                                            includeInaccessibleMembers);
        
      // Reflect the candidates found as DynamicMemberLookup results.
      for (auto candidate : subscripts.ViableCandidates) {
        auto decl = cast<SubscriptDecl>(candidate.getDecl());
        if (isAcceptableDynamicMemberLookupSubscript(decl, DC, TC))
          result.addViable(OverloadChoice::getDynamicMemberLookup(baseTy,
                                                                  decl, name));
      }
      for (auto candidate : subscripts.UnviableCandidates) {
        auto decl = candidate.first.getDecl();
        auto choice = OverloadChoice::getDynamicMemberLookup(baseTy, decl,name);
        result.addUnviable(choice, candidate.second);
      }
    }
  }

  // If we rejected some possibilities due to an argument-label
  // mismatch and ended up with nothing, try again ignoring the
  // labels. This allows us to perform typo correction on the labels.
  if (result.ViableCandidates.empty() && labelMismatch && shouldAttemptFixes()){
    argumentLabels.reset();
    goto retry_after_fail;
  }

  // If we have no viable or unviable candidates, and we're generating,
  // diagnostics, rerun the query with inaccessible members included, so we can
  // include them in the unviable candidates list.
  if (result.ViableCandidates.empty() && result.UnviableCandidates.empty() &&
      includeInaccessibleMembers) {
    NameLookupOptions lookupOptions = defaultMemberLookupOptions;
    
    // Ignore access control so we get candidates that might have been missed
    // before.
    lookupOptions |= NameLookupFlags::IgnoreAccessControl;
    // This is only used for diagnostics, so always use KnownPrivate.
    lookupOptions |= NameLookupFlags::KnownPrivate;
    
    auto lookup = TC.lookupMember(DC, instanceTy,
                                  memberName, lookupOptions);
    for (auto entry : lookup) {
      auto *cand = entry.getValueDecl();

      // If the result is invalid, skip it.
      TC.validateDecl(cand);
      if (cand->isInvalid()) {
        result.markErrorAlreadyDiagnosed();
        return result;
      }

      // FIXME: Deal with broken recursion
      if (!cand->hasInterfaceType())
        continue;

      result.addUnviable(getOverloadChoice(cand, /*isBridged=*/false,
                                           /*isUnwrappedOptional=*/false),
                         MemberLookupResult::UR_Inaccessible);
    }
  }
  
  return result;
}

ConstraintSystem::SolutionKind ConstraintSystem::simplifyMemberConstraint(
    ConstraintKind kind, Type baseTy, DeclName member, Type memberTy,
    DeclContext *useDC, FunctionRefKind functionRefKind,
    ArrayRef<OverloadChoice> outerAlternatives, TypeMatchOptions flags,
    ConstraintLocatorBuilder locatorB) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  baseTy = simplifyType(baseTy, flags);
  Type baseObjTy = baseTy->getRValueType();

  auto locator = getConstraintLocator(locatorB);
  MemberLookupResult result =
    performMemberLookup(kind, member, baseTy, functionRefKind, locator,
                        /*includeInaccessibleMembers*/false);
  
  switch (result.OverallResult) {
  case MemberLookupResult::Unsolved:
    // If requested, generate a constraint.
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::createMemberOrOuterDisjunction(*this, kind, baseTy, memberTy, member, useDC,
                                                   functionRefKind, outerAlternatives, locator));
      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;

  case MemberLookupResult::ErrorAlreadyDiagnosed:
    return SolutionKind::Error;

  case MemberLookupResult::HasResults:
    // Keep going!
    break;
  }

  // If we found viable candidates, then we're done!
  if (!result.ViableCandidates.empty()) {
    addOverloadSet(memberTy, result.ViableCandidates, useDC, locator,
                   result.getFavoredChoice(), outerAlternatives);

    return SolutionKind::Solved;
  }
  
  
  // If we found some unviable results, then fail, but without recovery.
  if (!result.UnviableCandidates.empty())
    return SolutionKind::Error;
  

  // If the lookup found no hits at all (either viable or unviable), diagnose it
  // as such and try to recover in various ways.
  if (shouldAttemptFixes() && baseObjTy->getOptionalObjectType()) {
    // If the base type was an optional, look through it.

    // If the base type is optional because we haven't chosen to force an
    // implicit optional, don't try to fix it. The IUO will be forced instead.
    if (auto dotExpr = dyn_cast<UnresolvedDotExpr>(locator->getAnchor())) {
      auto baseExpr = dotExpr->getBase();
      auto resolvedOverload = getResolvedOverloadSets();
      while (resolvedOverload) {
        if (resolvedOverload->Locator->getAnchor() == baseExpr) {
          if (resolvedOverload->Choice.isImplicitlyUnwrappedValueOrReturnValue())
            return SolutionKind::Error;
          break;
        }
        resolvedOverload = resolvedOverload->Previous;
      }
    }

    // The result of the member access can either be the expected member type
    // (for '!' or optional members with '?'), or the original member type with
    // one extra level of optionality ('?' with non-optional members).
    auto innerTV =
        createTypeVariable(locator, TVO_CanBindToLValue);
    Type optTy = getTypeChecker().getOptionalType(
        locator->getAnchor()->getSourceRange().Start, innerTV);
    SmallVector<Constraint *, 2> optionalities;
    auto nonoptionalResult = Constraint::createFixed(
        *this, ConstraintKind::Bind,
        UnwrapOptionalBase::create(*this, member, locator), innerTV, memberTy,
        locator);
    auto optionalResult = Constraint::createFixed(
        *this, ConstraintKind::Bind,
        UnwrapOptionalBase::createWithOptionalResult(*this, member, locator),
        optTy, memberTy, locator);
    optionalities.push_back(nonoptionalResult);
    optionalities.push_back(optionalResult);
    addDisjunctionConstraint(optionalities, locator);

    // Look through one level of optional.
    addValueMemberConstraint(baseObjTy->getOptionalObjectType(), member,
                             innerTV, useDC, functionRefKind, outerAlternatives,
                             locator);
    return SolutionKind::Solved;
  }
  return SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyDefaultableConstraint(
                                            Type first, Type second,
                                            TypeMatchOptions flags,
                                            ConstraintLocatorBuilder locator) {
  first = getFixedTypeRecursive(first, flags, true);

  if (first->isTypeVariableOrMember()) {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::create(*this, ConstraintKind::Defaultable, first, second,
                           getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;
  }

  // Otherwise, any type is fine.
  return SolutionKind::Solved;
}


ConstraintSystem::SolutionKind
ConstraintSystem::simplifyDynamicTypeOfConstraint(
                                        Type type1, Type type2,
                                        TypeMatchOptions flags,
                                        ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  // Local function to form an unsolved result.
  auto formUnsolved = [&] {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::create(*this, ConstraintKind::DynamicTypeOf, type1, type2,
                           getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;
  };

  // Solve forward.
  type2 = getFixedTypeRecursive(type2, flags, /*wantRValue=*/true);
  if (!type2->isTypeVariableOrMember()) {
    Type dynamicType2;
    if (type2->isAnyExistentialType()) {
      dynamicType2 = ExistentialMetatypeType::get(type2);
    } else {
      dynamicType2 = MetatypeType::get(type2);
    }
    return matchTypes(type1, dynamicType2, ConstraintKind::Bind, subflags,
                      locator);
  }

  // Okay, can't solve forward.  See what we can do backwards.
  type1 = getFixedTypeRecursive(type1, flags, /*wantRValue=*/true);
  if (type1->isTypeVariableOrMember())
    return formUnsolved();

  // If we have an existential metatype, that's good enough to solve
  // the constraint.
  if (auto metatype1 = type1->getAs<ExistentialMetatypeType>())
    return matchTypes(metatype1->getInstanceType(), type2,
                      ConstraintKind::Bind,
                      subflags, locator);

  // If we have a normal metatype, we can't solve backwards unless we
  // know what kind of object it is.
  if (auto metatype1 = type1->getAs<MetatypeType>()) {
    Type instanceType1 = getFixedTypeRecursive(metatype1->getInstanceType(),
                                               true);
    if (instanceType1->isTypeVariableOrMember())
      return formUnsolved();

    return matchTypes(instanceType1, type2, ConstraintKind::Bind, subflags,
                      locator);
  }

  // It's definitely not either kind of metatype, so we can
  // report failure right away.
  return SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyBridgingConstraint(Type type1,
                                             Type type2,
                                             TypeMatchOptions flags,
                                             ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  /// Form an unresolved result.
  auto formUnsolved = [&] {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::create(*this, ConstraintKind::BridgingConversion, type1,
                           type2, getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }
    
    return SolutionKind::Unsolved;
  };

  // Local function to look through optional types. It produces the
  // fully-unwrapped type and a count of the total # of optional types that were
  // unwrapped.
  auto unwrapType = [&](Type type) -> std::pair<Type, unsigned> {
    unsigned count = 0;
    while (Type objectType = type->getOptionalObjectType()) {
      ++count;

      TypeMatchOptions unusedOptions;
      type = getFixedTypeRecursive(objectType, unusedOptions, /*wantRValue=*/true);
    }

    return { type, count };
  };

  type1 = getFixedTypeRecursive(type1, flags, /*wantRValue=*/true);
  type2 = getFixedTypeRecursive(type2, flags, /*wantRValue=*/true);

  if (type1->isTypeVariableOrMember() || type2->isTypeVariableOrMember())
    return formUnsolved();

  Type unwrappedFromType;
  unsigned numFromOptionals;
  std::tie(unwrappedFromType, numFromOptionals) = unwrapType(type1);

  Type unwrappedToType;
  unsigned numToOptionals;
  std::tie(unwrappedToType, numToOptionals) = unwrapType(type2);

  if (unwrappedFromType->isTypeVariableOrMember() ||
      unwrappedToType->isTypeVariableOrMember())
    return formUnsolved();

  // Update the score.
  increaseScore(SK_UserConversion); // FIXME: Use separate score kind?
  if (worseThanBestSolution()) {
    return SolutionKind::Error;
  }

  // Local function to count the optional injections that will be performed
  // after the bridging conversion.
  auto countOptionalInjections = [&] {
    if (numToOptionals > numFromOptionals)
      increaseScore(SK_ValueToOptional, numToOptionals - numFromOptionals);
  };

  // Anything can be explicitly converted to AnyObject using the universal
  // bridging conversion. This allows both extraneous optionals in the source
  // (because optionals themselves can be boxed for AnyObject) and in the
  // destination (we'll perform the extra injections at the end).
  if (unwrappedToType->isAnyObject()) {
    countOptionalInjections();
    return SolutionKind::Solved;
  }

  // The source cannot be more optional than the destination, because bridging
  // conversions don't allow us to implicitly check for a value in the optional.
  if (numFromOptionals > numToOptionals) {
    return SolutionKind::Error;
  }

  // Explicit bridging from a value type to an Objective-C class type.
  if (unwrappedFromType->isPotentiallyBridgedValueType() &&
      (unwrappedToType->isBridgeableObjectType() ||
       (unwrappedToType->isExistentialType() &&
        !unwrappedToType->isAny()))) {
    countOptionalInjections();
    if (Type classType = TC.Context.getBridgedToObjC(DC, unwrappedFromType)) {
      return matchTypes(classType, unwrappedToType, ConstraintKind::Conversion,
                        subflags, locator);
    }
  }

  // Bridging from an Objective-C class type to a value type.
  // Note that specifically require a class or class-constrained archetype
  // here, because archetypes cannot be bridged.
  if (unwrappedFromType->mayHaveSuperclass() &&
      unwrappedToType->isPotentiallyBridgedValueType()) {
    Type bridgedValueType;
    if (auto objcClass = TC.Context.getBridgedToObjC(DC, unwrappedToType,
                                                     &bridgedValueType)) {
      // Bridging NSNumber to NSValue is one-way, since there are multiple Swift
      // value types that bridge to those object types. It requires a checked
      // cast to get back.
      if (TC.Context.isObjCClassWithMultipleSwiftBridgedTypes(objcClass))
        return SolutionKind::Error;

      // If the bridged value type is generic, the generic arguments
      // must either match or be bridged.
      // FIXME: This should be an associated type of the protocol.
      if (auto fromBGT = unwrappedToType->getAs<BoundGenericType>()) {
        if (fromBGT->getDecl() == TC.Context.getArrayDecl()) {
          // [AnyObject]
          addConstraint(ConstraintKind::Bind, fromBGT->getGenericArgs()[0],
                        TC.Context.getAnyObjectType(),
                        getConstraintLocator(
                          locator.withPathElement(
                                       LocatorPathElt::getGenericArgument(0))));
        } else if (fromBGT->getDecl() == TC.Context.getDictionaryDecl()) {
          // [NSObject : AnyObject]
          auto NSObjectType = TC.getNSObjectType(DC);
          if (!NSObjectType) {
            // Not a bridging case. Should we detect this earlier?
            return SolutionKind::Error;
          }

          addConstraint(ConstraintKind::Bind, fromBGT->getGenericArgs()[0],
                        NSObjectType,
                        getConstraintLocator(
                          locator.withPathElement(
                            LocatorPathElt::getGenericArgument(0))));

          addConstraint(ConstraintKind::Bind, fromBGT->getGenericArgs()[1],
                        TC.Context.getAnyObjectType(),
                        getConstraintLocator(
                          locator.withPathElement(
                            LocatorPathElt::getGenericArgument(1))));
        } else if (fromBGT->getDecl() == TC.Context.getSetDecl()) {
          auto NSObjectType = TC.getNSObjectType(DC);
          if (!NSObjectType) {
            // Not a bridging case. Should we detect this earlier?
            return SolutionKind::Error;
          }
          addConstraint(ConstraintKind::Bind, fromBGT->getGenericArgs()[0],
                        NSObjectType,
                        getConstraintLocator(
                          locator.withPathElement(
                            LocatorPathElt::getGenericArgument(0))));
        } else {
          // Nothing special to do; matchTypes will match generic arguments.
        }
      }

      // Make sure we have the bridged value type.
      if (matchTypes(unwrappedToType, bridgedValueType, ConstraintKind::Equal,
                     subflags, locator).isFailure())
        return SolutionKind::Error;

      countOptionalInjections();
      return matchTypes(unwrappedFromType, objcClass, ConstraintKind::Subtype,
                        subflags, locator);
    }
  }

  // Bridging the elements of an array.
  if (auto fromElement = isArrayType(unwrappedFromType)) {
    if (auto toElement = isArrayType(unwrappedToType)) {
      countOptionalInjections();
      return simplifyBridgingConstraint(
                                      *fromElement, *toElement, subflags,
                                      locator.withPathElement(
                                        LocatorPathElt::getGenericArgument(0)));
    }
  }

  // Bridging the keys/values of a dictionary.
  if (auto fromKeyValue = isDictionaryType(unwrappedFromType)) {
    if (auto toKeyValue = isDictionaryType(unwrappedToType)) {
      addExplicitConversionConstraint(fromKeyValue->first, toKeyValue->first,
                                      /*allowFixes=*/false,
                                      locator.withPathElement(
                                        LocatorPathElt::getGenericArgument(0)));
      addExplicitConversionConstraint(fromKeyValue->second, toKeyValue->second,
                                      /*allowFixes=*/false,
                                      locator.withPathElement(
                                        LocatorPathElt::getGenericArgument(0)));
      countOptionalInjections();
      return SolutionKind::Solved;
    }
  }

  // Bridging the elements of a set.
  if (auto fromElement = isSetType(unwrappedFromType)) {
    if (auto toElement = isSetType(unwrappedToType)) {
      countOptionalInjections();
      return simplifyBridgingConstraint(
                                      *fromElement, *toElement, subflags,
                                      locator.withPathElement(
                                        LocatorPathElt::getGenericArgument(0)));
    }
  }

  return SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyEscapableFunctionOfConstraint(
                                        Type type1, Type type2,
                                        TypeMatchOptions flags,
                                        ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  // Local function to form an unsolved result.
  auto formUnsolved = [&] {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::create(*this, ConstraintKind::EscapableFunctionOf,
                           type1, type2, getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;
  };


  type2 = getFixedTypeRecursive(type2, flags, /*wantRValue=*/true);
  if (auto fn2 = type2->getAs<FunctionType>()) {
    // Solve forward by binding the other type variable to the escapable
    // variation of this type.
    auto fn1 = fn2->withExtInfo(fn2->getExtInfo().withNoEscape(false));
    return matchTypes(type1, fn1, ConstraintKind::Bind, subflags, locator);
  }
  if (!type2->isTypeVariableOrMember())
    // We definitely don't have a function, so bail.
    return SolutionKind::Error;
  
  type1 = getFixedTypeRecursive(type1, flags, /*wantRValue=*/true);
  if (auto fn1 = type1->getAs<FunctionType>()) {
    // We should have the escaping end of the relation.
    if (fn1->getExtInfo().isNoEscape())
      return SolutionKind::Error;
    
    // Solve backward by binding the other type variable to the noescape
    // variation of this type.
    auto fn2 = fn1->withExtInfo(fn1->getExtInfo().withNoEscape(true));
    return matchTypes(type2, fn2, ConstraintKind::Bind, subflags, locator);
  }
  if (!type1->isTypeVariableOrMember())
    // We definitely don't have a function, so bail.
    return SolutionKind::Error;

  return formUnsolved();
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyOpenedExistentialOfConstraint(
                                        Type type1, Type type2,
                                        TypeMatchOptions flags,
                                        ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);
  type2 = getFixedTypeRecursive(type2, flags, /*wantRValue=*/true);
  if (type2->isAnyExistentialType()) {
    // We have the existential side. Produce an opened archetype and bind
    // type1 to it.
    bool isMetatype = false;
    auto instanceTy = type2;
    if (auto metaTy = type2->getAs<ExistentialMetatypeType>()) {
      isMetatype = true;
      instanceTy = metaTy->getInstanceType();
    }
    assert(instanceTy->isExistentialType());
    Type openedTy = ArchetypeType::getOpened(instanceTy);
    if (isMetatype)
      openedTy = MetatypeType::get(openedTy, TC.Context);
    return matchTypes(type1, openedTy, ConstraintKind::Bind, subflags, locator);
  }
  if (!type2->isTypeVariableOrMember())
    // We definitely don't have an existential, so bail.
    return SolutionKind::Error;
  
  // If type1 is constrained to anything concrete, the constraint fails.
  // It can only be bound to a type we opened for it.
  type1 = getFixedTypeRecursive(type1, flags, /*wantRValue=*/true);
  if (!type1->isTypeVariableOrMember())
    return SolutionKind::Error;
  
  if (flags.contains(TMF_GenerateConstraints)) {
    addUnsolvedConstraint(
      Constraint::create(*this, ConstraintKind::OpenedExistentialOf,
                         type1, type2, getConstraintLocator(locator)));
    return SolutionKind::Solved;
  }
  return SolutionKind::Unsolved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyKeyPathConstraint(Type keyPathTy,
                                            Type rootTy,
                                            Type valueTy,
                                            TypeMatchOptions flags,
                                            ConstraintLocatorBuilder locator) {
  auto subflags = getDefaultDecompositionOptions(flags);
  // The constraint ought to have been anchored on a KeyPathExpr.
  auto keyPath = cast<KeyPathExpr>(locator.getBaseLocator()->getAnchor());
  
  // Gather overload choices for any key path components associated with this
  // key path.
  SmallVector<OverloadChoice, 4> choices;
  choices.resize(keyPath->getComponents().size());
  for (auto resolvedItem = resolvedOverloadSets; resolvedItem;
       resolvedItem = resolvedItem->Previous) {
    auto locator = resolvedItem->Locator;
    if (locator->getAnchor() == keyPath
        && locator->getPath().size() <= 2
        && locator->getPath()[0].getKind() == ConstraintLocator::KeyPathComponent) {
      choices[locator->getPath()[0].getValue()] = resolvedItem->Choice;
    }
  }
  
  keyPathTy = getFixedTypeRecursive(keyPathTy, /*want rvalue*/ true);
  auto tryMatchRootAndValueFromKeyPathType =
    [&](BoundGenericType *bgt, bool allowPartial) -> SolutionKind {
      Type boundRoot, boundValue;
      
      // We can get root and value from a concrete key path type.
      if (bgt->getDecl() == getASTContext().getKeyPathDecl()
          || bgt->getDecl() == getASTContext().getWritableKeyPathDecl()
          || bgt->getDecl() == getASTContext().getReferenceWritableKeyPathDecl()) {
        boundRoot = bgt->getGenericArgs()[0];
        boundValue = bgt->getGenericArgs()[1];
      } else if (bgt->getDecl() == getASTContext().getPartialKeyPathDecl()) {
        if (allowPartial) {
          // We can still get the root from a PartialKeyPath.
          boundRoot = bgt->getGenericArgs()[0];
          boundValue = Type();
        } else {
          return SolutionKind::Error;
        }
      } else {
        // We can't bind anything from this type.
        return SolutionKind::Solved;
      }
      if (matchTypes(boundRoot, rootTy,
                ConstraintKind::Bind, subflags, locator).isFailure())
        return SolutionKind::Error;

      if (boundValue
          && matchTypes(boundValue, valueTy,
                ConstraintKind::Bind, subflags, locator).isFailure())
        return SolutionKind::Error;
      
      return SolutionKind::Solved;
    };

  // If we're fixed to a bound generic type, trying harvesting context from it.
  // However, we don't want a solution that fixes the expression type to
  // PartialKeyPath; we'd rather that be represented using an upcast conversion.
  auto keyPathBGT = keyPathTy->getAs<BoundGenericType>();
  if (keyPathBGT) {
    if (tryMatchRootAndValueFromKeyPathType(keyPathBGT, /*allowPartial*/false)
          == SolutionKind::Error)
      return SolutionKind::Error;
  }

  // If the expression has contextual type information, try using that too.
  if (auto contextualTy = getContextualType(keyPath)) {
    if (auto contextualBGT = contextualTy->getAs<BoundGenericType>()) {
      if (tryMatchRootAndValueFromKeyPathType(contextualBGT,
                                              /*allowPartial*/true)
            == SolutionKind::Error)
        return SolutionKind::Error;
    }
  }
  
  // See if we resolved overloads for all the components involved.
  enum {
    ReadOnly,
    Writable,
    ReferenceWritable
  } capability = Writable;

  for (unsigned i : indices(keyPath->getComponents())) {
    auto &component = keyPath->getComponents()[i];
    
    switch (component.getKind()) {
    case KeyPathExpr::Component::Kind::Invalid:
    case KeyPathExpr::Component::Kind::Identity:
      break;
      
    case KeyPathExpr::Component::Kind::Property:
    case KeyPathExpr::Component::Kind::Subscript:
    case KeyPathExpr::Component::Kind::UnresolvedProperty:
    case KeyPathExpr::Component::Kind::UnresolvedSubscript: {
      // If no choice was made, leave the constraint unsolved.
      if (choices[i].isInvalid()) {
        if (flags.contains(TMF_GenerateConstraints)) {
          addUnsolvedConstraint(Constraint::create(*this,
                                                   ConstraintKind::KeyPath,
                                                   keyPathTy, rootTy, valueTy,
                                                   locator.getBaseLocator()));
          return SolutionKind::Solved;
        }
        return SolutionKind::Unsolved;
      }
      
      // Discarded unsupported non-decl member lookups.
      if (!choices[i].isDecl()) {
        return SolutionKind::Error;
      }
      auto storage = dyn_cast<AbstractStorageDecl>(choices[i].getDecl());
      if (!storage) {
        return SolutionKind::Error;
      }
      
      // See whether key paths can store to this component. (Key paths don't
      // get any special power from being formed in certain contexts, such
      // as the ability to assign to `let`s in initialization contexts, so
      // we pass null for the DC to `isSettable` here.)
      if (!getASTContext().isSwiftVersionAtLeast(5)) {
        // As a source-compatibility measure, continue to allow
        // WritableKeyPaths to be formed in the same conditions we did
        // in previous releases even if we should not be able to set
        // the value in this context.
        if (!storage->isSettable(DC)) {
          // A non-settable component makes the key path read-only, unless
          // a reference-writable component shows up later.
          capability = ReadOnly;
          continue;
        }
      } else if (!storage->isSettable(nullptr)
                 || !storage->isSetterAccessibleFrom(DC)) {
        // A non-settable component makes the key path read-only, unless
        // a reference-writable component shows up later.
        capability = ReadOnly;
        continue;
      }
      // A nonmutating setter indicates a reference-writable base.
      if (!storage->isSetterMutating()) {
        capability = ReferenceWritable;
        continue;
      }
      // Otherwise, the key path maintains its current capability.
      break;
    }
    
    case KeyPathExpr::Component::Kind::OptionalChain:
      // Optional chains force the entire key path to be read-only.
      capability = ReadOnly;
      goto done;
    
    case KeyPathExpr::Component::Kind::OptionalForce:
      // Forcing an optional preserves its lvalue-ness.
      break;
    
    case KeyPathExpr::Component::Kind::OptionalWrap:
      // An optional chain should already have forced the entire key path to
      // be read-only.
      assert(capability == ReadOnly);
      break;
    }
  }
done:

  // Resolve the type.
  NominalTypeDecl *kpDecl;
  switch (capability) {
  case ReadOnly:
    kpDecl = getASTContext().getKeyPathDecl();
    break;

  case Writable:
    kpDecl = getASTContext().getWritableKeyPathDecl();
    break;

  case ReferenceWritable:
    kpDecl = getASTContext().getReferenceWritableKeyPathDecl();
    break;
  }
  
  // FIXME: Allow the type to be upcast if the type system has a concrete
  // KeyPath type assigned to the expression already.
  if (keyPathBGT) {
    if (keyPathBGT->getDecl() == getASTContext().getKeyPathDecl())
      kpDecl = getASTContext().getKeyPathDecl();
    else if (keyPathBGT->getDecl() == getASTContext().getWritableKeyPathDecl()
             && capability >= Writable)
      kpDecl = getASTContext().getWritableKeyPathDecl();
  }
  
  auto resolvedKPTy = BoundGenericType::get(kpDecl, nullptr,
                                            {rootTy, valueTy});
  return matchTypes(resolvedKPTy, keyPathTy, ConstraintKind::Bind,
                    subflags, locator);
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyKeyPathApplicationConstraint(
                                        Type keyPathTy,
                                        Type rootTy,
                                        Type valueTy,
                                        TypeMatchOptions flags,
                                        ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);
  keyPathTy = getFixedTypeRecursive(keyPathTy, flags, /*wantRValue=*/true);
  
  auto unsolved = [&]() -> SolutionKind {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(Constraint::create(*this,
                    ConstraintKind::KeyPathApplication,
                    keyPathTy, rootTy, valueTy, getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }
    return SolutionKind::Unsolved;
  };
  
  if (auto clas = keyPathTy->getAs<NominalType>()) {
    if (clas->getDecl() == getASTContext().getAnyKeyPathDecl()) {
      // Read-only keypath, whose projected value is upcast to `Any?`.
      // The root type can be anything.
      Type resultTy = ProtocolCompositionType::get(getASTContext(), {},
                                                  /*explicit AnyObject*/ false);
      resultTy = OptionalType::get(resultTy);
      return matchTypes(resultTy, valueTy, ConstraintKind::Bind,
                        subflags, locator);
    }
  }
  
  if (auto bgt = keyPathTy->getAs<BoundGenericType>()) {
    // We have the key path type. Match it to the other ends of the constraint.
    auto kpRootTy = bgt->getGenericArgs()[0];
    
    // Try to match the root type.
    rootTy = getFixedTypeRecursive(rootTy, flags, /*wantRValue=*/false);

    auto matchRoot = [&](ConstraintKind kind) -> bool {
      auto rootMatches = matchTypes(rootTy, kpRootTy, kind,
                                    subflags, locator);
      switch (rootMatches) {
      case SolutionKind::Error:
        return false;
      case SolutionKind::Solved:
        return true;
      case SolutionKind::Unsolved:
        llvm_unreachable("should have generated constraints");
      }
      llvm_unreachable("unhandled match");
    };

    if (bgt->getDecl() == getASTContext().getPartialKeyPathDecl()) {
      // Read-only keypath, whose projected value is upcast to `Any`.
      auto resultTy = ProtocolCompositionType::get(getASTContext(), {},
                                                  /*explicit AnyObject*/ false);

      if (!matchRoot(ConstraintKind::Conversion))
        return SolutionKind::Error;

      return matchTypes(resultTy, valueTy,
                        ConstraintKind::Bind, subflags, locator);
    }

    if (bgt->getGenericArgs().size() < 2)
      return SolutionKind::Error;
    auto kpValueTy = bgt->getGenericArgs()[1];

    /// Solve for an rvalue base.
    auto solveRValue = [&]() -> ConstraintSystem::SolutionKind {
      // An rvalue base can be converted to a supertype.
      return matchTypes(kpValueTy, valueTy,
                        ConstraintKind::Bind, subflags, locator);
    };
    /// Solve for a base whose lvalueness is to be determined.
    auto solveUnknown = [&]() -> ConstraintSystem::SolutionKind {
      if (matchTypes(kpValueTy, valueTy, ConstraintKind::Equal, subflags,
                     locator).isFailure())
        return SolutionKind::Error;
      return unsolved();
    };
    /// Solve for an lvalue base.
    auto solveLValue = [&]() -> ConstraintSystem::SolutionKind {
      return matchTypes(LValueType::get(kpValueTy), valueTy,
                        ConstraintKind::Bind, subflags, locator);
    };
  
    if (bgt->getDecl() == getASTContext().getKeyPathDecl()) {
      // Read-only keypath.
      if (!matchRoot(ConstraintKind::Conversion))
        return SolutionKind::Error;

      return solveRValue();
    }
    if (bgt->getDecl() == getASTContext().getWritableKeyPathDecl()) {
      // Writable keypath. The result can be an lvalue if the root was.
      // We can't convert the base without giving up lvalue-ness, though.
      if (!matchRoot(ConstraintKind::Equal))
        return SolutionKind::Error;

      if (rootTy->is<LValueType>())
        return solveLValue();
      if (rootTy->isTypeVariableOrMember())
        // We don't know whether the value is an lvalue yet.
        return solveUnknown();
      return solveRValue();
    }
    if (bgt->getDecl() == getASTContext().getReferenceWritableKeyPathDecl()) {
      if (!matchRoot(ConstraintKind::Conversion))
        return SolutionKind::Error;

      // Reference-writable keypath. The result can always be an lvalue.
      return solveLValue();
    }
    // Otherwise, we don't have a key path type at all.
    return SolutionKind::Error;
  }
  if (!keyPathTy->isTypeVariableOrMember())
    return SolutionKind::Error;
  
  return unsolved();
}

// SWIFT_ENABLE_TENSORFLOW
/// Returns the function declaration corresponding to a @dynamicCallable
/// attribute required method (if it exists) implemented by a type. Otherwise,
/// return nullptr.
static FuncDecl *
lookupDynamicCallableMethod(Type type, ConstraintSystem &CS,
                            const ConstraintLocatorBuilder &locator,
                            StringRef methodName, StringRef argumentName,
                            bool hasKeywordArgs, bool &error) {
  auto &ctx = CS.getASTContext();
  auto decl = type->getAnyNominal();
  auto option = DeclName(ctx, DeclBaseName(ctx.getIdentifier(methodName)),
                         { ctx.getIdentifier(argumentName) });
  auto matches = CS.performMemberLookup(ConstraintKind::ValueMember,
                                        option, type,
                                        FunctionRefKind::SingleApply,
                                        CS.getConstraintLocator(locator),
                                        /*includeInaccessibleMembers*/ false);
  // Filter valid candidates.
  auto candidates = matches.ViableCandidates;
  auto filter = [&](OverloadChoice choice) {
    auto cand = cast<FuncDecl>(choice.getDecl());
    return !isValidDynamicCallableMethod(cand, decl, CS.TC, hasKeywordArgs);
  };
  candidates.erase(std::remove_if(candidates.begin(), candidates.end(), filter),
                   candidates.end());

  // If there is one candidate, return it. Otherwise, return nullptr.
  auto size = candidates.size();
  if (size == 1) return cast<FuncDecl>(candidates.front().getDecl());
  // If there are >1 candidates, it is an overload error.
  else if (size > 1) error = true;
  return nullptr;
}

// SWIFT_ENABLE_TENSORFLOW
/// Looks up and returns the @dynamicCallable required methods (if they exist)
/// implemented by a type. This function should not be called directly: instead,
/// call `getDynamicCallableMethods` which performs caching.
static DynamicCallableMethods
lookupDynamicCallableMethods(Type type, ConstraintSystem &CS,
                             const ConstraintLocatorBuilder &locator,
                             bool &error) {
  DynamicCallableMethods methods;
  methods.argumentsMethod =
    lookupDynamicCallableMethod(type, CS, locator, "dynamicallyCall",
                                "withArguments", /*hasKeywordArgs*/ false,
                                error);
  methods.keywordArgumentsMethod =
    lookupDynamicCallableMethod(type, CS, locator, "dynamicallyCall",
                                "withKeywordArguments", /*hasKeywordArgs*/ true,
                                error);
  return methods;
}

// SWIFT_ENABLE_TENSORFLOW
/// Returns the @dynamicCallable required methods (if they exist) implemented by
/// a type.
/// This function may be slow for deep class hierarchies and multiple protocol
/// conformances, but it is invoked only after other constraint simplification
/// rules fail.
static DynamicCallableMethods
getDynamicCallableMethods(Type type, ConstraintSystem &CS,
                          const ConstraintLocatorBuilder &locator,
                          bool &error) {
  auto canType = type->getCanonicalType();
  auto it = CS.DynamicCallableCache.find(canType);
  if (it != CS.DynamicCallableCache.end()) return it->second;

  auto calculate = [&]() -> DynamicCallableMethods {
    // If this is a protocol composition, check if any of the protocols have the
    // attribute.
    if (auto protocolComp = canType->getAs<ProtocolCompositionType>()) {
      DynamicCallableMethods methods;
      for (auto protocolType : protocolComp->getMembers()) {
        auto tmp = getDynamicCallableMethods(protocolType, CS, locator, error);
        if (error) return methods;
        if (tmp.argumentsMethod) {
          if (methods.argumentsMethod) {
            error = true;
            return methods;
          }
          methods.argumentsMethod = tmp.argumentsMethod;
        }
        if (tmp.keywordArgumentsMethod) {
          if (methods.keywordArgumentsMethod) {
            error = true;
            return methods;
          }
          methods.keywordArgumentsMethod = tmp.keywordArgumentsMethod;
        }
      }
      return methods;
    }

    // Otherwise, this must be a nominal type.
    // Dynamic calling doesn't work for tuples, etc.
    auto nominal = canType->getAnyNominal();
    if (!nominal) return DynamicCallableMethods();

    // If this type conforms to a protocol which has the attribute, then
    // look up the methods.
    for (auto p : nominal->getAllProtocols())
      if (p->getAttrs().hasAttribute<DynamicCallableAttr>())
        return lookupDynamicCallableMethods(type, CS, locator, error);

    // Walk superclasses, if present.
    llvm::SmallPtrSet<const NominalTypeDecl*, 8> visitedDecls;
    while (1) {
      // If we found a circular parent class chain, reject this.
      if (!visitedDecls.insert(nominal).second)
        return DynamicCallableMethods();

      // If this type has the attribute on it, then look up the methods.
      if (nominal->getAttrs().hasAttribute<DynamicCallableAttr>())
        return lookupDynamicCallableMethods(type, CS, locator, error);

      // If this type is a class with a superclass, check superclasses.
      if (auto *cd = dyn_cast<ClassDecl>(nominal)) {
        if (auto superClass = cd->getSuperclassDecl()) {
          nominal = superClass;
          continue;
        }
      }

      return DynamicCallableMethods();
    }
  };

  return CS.DynamicCallableCache[canType] = calculate();
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyApplicableFnConstraint(
                                           Type type1,
                                           Type type2,
                                           TypeMatchOptions flags,
                                           ConstraintLocatorBuilder locator) {

  // By construction, the left hand side is a type that looks like the
  // following: $T1 -> $T2.
  assert(type1->is<FunctionType>());

  // Drill down to the concrete type on the right hand side.
  type2 = getFixedTypeRecursive(type2, flags, /*wantRValue=*/true);
  auto desugar2 = type2->getDesugaredType();

  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  // If the types are obviously equivalent, we're done.
  if (type1.getPointer() == desugar2)
    return SolutionKind::Solved;

  // Local function to form an unsolved result.
  auto formUnsolved = [&] {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::create(*this, ConstraintKind::ApplicableFunction, type1,
                           type2, getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }
    
    return SolutionKind::Unsolved;

  };

  // If right-hand side is a type variable, the constraint is unsolved.
  if (desugar2->isTypeVariableOrMember())
    return formUnsolved();

  // Strip the 'ApplyFunction' off the locator.
  // FIXME: Perhaps ApplyFunction can go away entirely?
  SmallVector<LocatorPathElt, 2> parts;
  Expr *anchor = locator.getLocatorParts(parts);
  assert(!parts.empty() && "Nonsensical applicable-function locator");
  assert(parts.back().getKind() == ConstraintLocator::ApplyFunction);
  assert(parts.back().getNewSummaryFlags() == 0);
  parts.pop_back();
  ConstraintLocatorBuilder outerLocator =
    getConstraintLocator(anchor, parts, locator.getSummaryFlags());

  unsigned unwrapCount = 0;
  if (shouldAttemptFixes()) {
    // If we have an optional type, try forcing it to see if that
    // helps. Note that we only deal with function and metatype types
    // below, so there is no reason not to attempt to strip these off
    // immediately.
    while (auto objectType2 = desugar2->getOptionalObjectType()) {
      type2 = objectType2;
      desugar2 = type2->getDesugaredType();

      // Track how many times we do this so that we can record a fix for each.
      ++unwrapCount;
    }
  }

  // For a function, bind the output and convert the argument to the input.
  auto func1 = type1->castTo<FunctionType>();
  if (auto func2 = dyn_cast<FunctionType>(desugar2)) {
    // The argument type must be convertible to the input type.
    if (::matchCallArguments(
            *this, func1->getParams(), func2->getParams(),
            outerLocator.withPathElement(ConstraintLocator::ApplyArgument))
            .isFailure())
      return SolutionKind::Error;

    // The result types are equivalent.
    if (matchTypes(func1->getResult(),
                   func2->getResult(),
                   ConstraintKind::Bind,
                   subflags,
                   locator.withPathElement(
                     ConstraintLocator::FunctionResult)).isFailure())
      return SolutionKind::Error;

    // Record any fixes we attempted to get to the correct solution.
    auto *fix = ForceOptional::create(*this, getConstraintLocator(locator));
    while (unwrapCount-- > 0) {
      if (recordFix(fix))
        return SolutionKind::Error;
    }

    return SolutionKind::Solved;
  }

  // For a metatype, perform a construction.
  if (auto meta2 = dyn_cast<AnyMetatypeType>(desugar2)) {
    auto instance2 = getFixedTypeRecursive(meta2->getInstanceType(), true);
    if (instance2->isTypeVariableOrMember())
      return formUnsolved();

    // Construct the instance from the input arguments.
    auto simplified = simplifyConstructionConstraint(instance2, func1, subflags,
                                          /*FIXME?*/ DC,
                                          FunctionRefKind::SingleApply,
                                          getConstraintLocator(outerLocator));

    // Record any fixes we attempted to get to the correct solution.
    if (simplified == SolutionKind::Solved) {
      auto *fix = ForceOptional::create(*this, getConstraintLocator(locator));
      while (unwrapCount-- > 0) {
        if (recordFix(fix))
          return SolutionKind::Error;
      }
    }

    return simplified;
  }

  // SWIFT_ENABLE_TENSORFLOW
  // Handle @dynamicCallable applications.
  bool error = false;
  auto methods = getDynamicCallableMethods(desugar2, *this, locator, error);
  if (error) return SolutionKind::Error;

  if (methods.isValid()) {
    auto &ctx = getASTContext();
    auto func1 = type1->castTo<FunctionType>();

    // Determine whether to call the positional arguments method or the
    // keyword arguments method.
    bool useKwargsMethod = methods.argumentsMethod == nullptr;
    if (!useKwargsMethod) {
      for (auto param : func1->getParams()) {
        if (param.hasLabel()) {
          useKwargsMethod = true;
          break;
        }
      }
    }

    auto method = useKwargsMethod
      ? methods.keywordArgumentsMethod
      : methods.argumentsMethod;

    if (!method) {
      assert(useKwargsMethod &&
             "Undefined method implies kwargs method is missing");
      // Get source loc of @dynamicCallable nominal decl to emit a diagnostic.
      auto decl = desugar2->getAnyNominal();
      if (auto protocolComp = desugar2->getAs<ProtocolCompositionType>()) {
        for (auto protocol : protocolComp->getMembers())
          if (DynamicCallableCache.count(protocol->getCanonicalType()))
            decl = protocol->getAnyNominal();
      }
      TC.diagnose(decl->getLoc(), diag::missing_dynamic_callable_kwargs_method,
                  desugar2);
      return SolutionKind::Error;
    }

    auto memberType = getTypeOfMemberReference(desugar2, method, DC,
                                               /*isDynamicResult*/ false,
                                               FunctionRefKind::DoubleApply,
                                               locator).second;
    auto methodType = memberType->castTo<AnyFunctionType>();
    auto argType = methodType->getParams()[0].getPlainType();

    // Attempts to solve an argument conversion constraint from each dynamic
    // call parameter to the specified type. Returns true if the constraint can
    // be solved.
    auto argTypesMatch = [&](Type argElementType) {
      // Allow argument type to default to `Any`.
      addConstraint(ConstraintKind::Defaultable, argElementType, ctx.TheAnyType,
                    locator);
      // Constraint each dynamic call parameter to argument type.
      for (auto param : func1->getParams()) {
        auto paramType = param.getPlainType();
        auto locatorBuilder =
          outerLocator.withPathElement(ConstraintLocator::ApplyArgument);
        if (matchTypes(paramType, argElementType,
                       ConstraintKind::ArgumentConversion, subflags,
                       locatorBuilder).isFailure()) {
          return false;
        }
      }
      return true;
    };

    // Add constraints on argument type.
    if (!useKwargsMethod) {
      auto arrayLitProto =
        ctx.getProtocol(KnownProtocolKind::ExpressibleByArrayLiteral);
      auto conformance =
        TC.conformsToProtocol(argType, arrayLitProto, DC,
                              ConformanceCheckFlags::InExpression);
      Type arrayElementType =
        ProtocolConformanceRef::getTypeWitnessByName(
          argType, *conformance, ctx.Id_ArrayLiteralElement, &TC)
        ->getDesugaredType();

      if (!argTypesMatch(arrayElementType))
        return SolutionKind::Error;
    } else {
      auto dictLitProto =
        ctx.getProtocol(KnownProtocolKind::ExpressibleByDictionaryLiteral);
      auto conformance =
        TC.conformsToProtocol(argType, dictLitProto, DC,
                              ConformanceCheckFlags::InExpression);
      Type dictValueType =
        ProtocolConformanceRef::getTypeWitnessByName(
          argType, *conformance, ctx.Id_Value, &TC)
        ->getDesugaredType();

      if (!argTypesMatch(dictValueType))
        return SolutionKind::Error;
    }

    // Add constraints on result type.
    auto locatorBuilder =
      locator.withPathElement(ConstraintLocator::FunctionResult);
    if (matchTypes(func1->getResult(), methodType->getResult(),
                   ConstraintKind::Bind, subflags,
                   locatorBuilder).isFailure())
      return SolutionKind::Error;

    return SolutionKind::Solved;
  } // end @dynamicCallable handling

  return SolutionKind::Error;
}

static Type getBaseTypeForPointer(ConstraintSystem &cs, TypeBase *type) {
  if (Type unwrapped = type->getOptionalObjectType())
    type = unwrapped.getPointer();

  auto pointeeTy = type->getAnyPointerElementType();
  assert(pointeeTy);
  return pointeeTy;
}

void ConstraintSystem::addRestrictedConstraint(
                             ConstraintKind kind,
                             ConversionRestrictionKind restriction,
                             Type first, Type second,
                             ConstraintLocatorBuilder locator) {
  (void)simplifyRestrictedConstraint(restriction, first, second, kind,
                                     TMF_GenerateConstraints, locator);
}

/// Given that we have a conversion constraint between two types, and
/// that the given constraint-reduction rule applies between them at
/// the top level, apply it and generate any necessary recursive
/// constraints.
ConstraintSystem::SolutionKind
ConstraintSystem::simplifyRestrictedConstraintImpl(
                                         ConversionRestrictionKind restriction,
                                         Type type1, Type type2,
                                         ConstraintKind matchKind,
                                         TypeMatchOptions flags,
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

  // Local function to form an unsolved result.
  auto formUnsolved = [&] {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::createRestricted(
          *this, matchKind, restriction, type1, type2,
          getConstraintLocator(locator)));

      return SolutionKind::Solved;
    }
    
    return SolutionKind::Unsolved;
  };

  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  switch (restriction) {
  // for $< in { <, <c, <oc }:
  //   T_i $< U_i ===> (T_i...) $< (U_i...)
  case ConversionRestrictionKind::TupleToTuple:
    return matchTupleTypes(type1->castTo<TupleType>(),
                           type2->castTo<TupleType>(),
                           matchKind, subflags, locator);

  case ConversionRestrictionKind::DeepEquality:
    return matchDeepEqualityTypes(type1, type2, locator);

  case ConversionRestrictionKind::Superclass:
    addContextualScore();
    return matchSuperclassTypes(type1, type2, subflags, locator);

  case ConversionRestrictionKind::LValueToRValue:
    return matchTypes(type1->getRValueType(), type2,
                      matchKind, subflags, locator);

  // for $< in { <, <c, <oc }:
  //   T $< U, U : P_i ===> T $< protocol<P_i...>
  case ConversionRestrictionKind::Existential:
    addContextualScore();
    return matchExistentialTypes(type1, type2,
                                 ConstraintKind::SelfObjectOfProtocol,
                                 subflags, locator);

  // for $< in { <, <c, <oc }:
  //   for P protocol, Q protocol,
  //     P : Q ===> T.Protocol $< Q.Type
  //   for P protocol, Q protocol,
  //     P $< Q ===> P.Type $< Q.Type
  case ConversionRestrictionKind::MetatypeToExistentialMetatype:
    addContextualScore();

    return matchExistentialTypes(
             type1->castTo<MetatypeType>()->getInstanceType(),
             type2->castTo<ExistentialMetatypeType>()->getInstanceType(),
             ConstraintKind::ConformsTo,
             subflags,
             locator.withPathElement(ConstraintLocator::InstanceType));

  // for $< in { <, <c, <oc }:
  //   for P protocol, C class, D class,
  //     (P & C) : D ===> (P & C).Type $< D.Type
  case ConversionRestrictionKind::ExistentialMetatypeToMetatype: {
    addContextualScore();

    auto instance1 = type1->castTo<ExistentialMetatypeType>()->getInstanceType();
    auto instance2 = type2->castTo<MetatypeType>()->getInstanceType();
    auto superclass1 = instance1->getSuperclass();

    if (!superclass1)
      return SolutionKind::Error;

    return matchTypes(
             superclass1,
             instance2,
             ConstraintKind::Subtype,
             subflags,
             locator.withPathElement(ConstraintLocator::InstanceType));

  }
  // for $< in { <, <c, <oc }:
  //   T $< U ===> T $< U?
  case ConversionRestrictionKind::ValueToOptional: {
    addContextualScore();
    increaseScore(SK_ValueToOptional);

    assert(matchKind >= ConstraintKind::Subtype);
    if (type2->isTypeVariableOrMember())
      return formUnsolved();

    if (auto generic2 = type2->getAs<BoundGenericType>()) {
      if (generic2->getDecl()->isOptionalDecl()) {
        return matchTypes(type1, generic2->getGenericArgs()[0],
                          matchKind, subflags,
                          locator.withPathElement(
                            ConstraintLocator::OptionalPayload));
      }
    }

    return SolutionKind::Error;
  }

  // for $< in { <, <c, <oc }:
  //   T $< U ===> T? $< U?
  //   T $< U ===> T! $< U!
  //   T $< U ===> T! $< U?
  // also:
  //   T <c U ===> T? <c U!
  case ConversionRestrictionKind::OptionalToOptional: {
    addContextualScore();

    if (type1->isTypeVariableOrMember() || type2->isTypeVariableOrMember())
      return formUnsolved();

    assert(matchKind >= ConstraintKind::Subtype);
    if (auto generic1 = type1->getAs<BoundGenericType>()) {
      if (auto generic2 = type2->getAs<BoundGenericType>()) {
        if (generic1->getDecl()->isOptionalDecl() &&
            generic2->getDecl()->isOptionalDecl())
          return matchTypes(generic1->getGenericArgs()[0],
                            generic2->getGenericArgs()[0],
                            matchKind, subflags,
                            locator.withPathElement(
                              LocatorPathElt::getGenericArgument(0)));
      }
    }

    return SolutionKind::Error;
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
    // Unwrap an inout type.
    auto obj1 = type1->getInOutObjectType();

    obj1 = getFixedTypeRecursive(obj1, false);
    
    auto t2 = type2->getDesugaredType();

    auto baseType1 = getFixedTypeRecursive(*isArrayType(obj1), false);
    auto baseType2 = getBaseTypeForPointer(*this, t2);

    increaseScore(ScoreKind::SK_ValueToPointerConversion);
    return matchTypes(baseType1, baseType2,
                      ConstraintKind::BindToPointerType,
                      subflags, locator);
  }
      
  // String ===> UnsafePointer<[U]Int8>
  case ConversionRestrictionKind::StringToPointer: {
    addContextualScore();

    auto baseType2 = getBaseTypeForPointer(*this, type2->getDesugaredType());
    
    // The pointer element type must be void or a byte-sized type.
    // TODO: Handle different encodings based on pointer element type, such as
    // UTF16 for [U]Int16 or UTF32 for [U]Int32. For now we only interop with
    // Int8 pointers using UTF8 encoding.
    baseType2 = getFixedTypeRecursive(baseType2, false);
    // If we haven't resolved the element type, generate constraints.
    if (baseType2->isTypeVariableOrMember()) {
      if (flags.contains(TMF_GenerateConstraints)) {
        increaseScore(ScoreKind::SK_ValueToPointerConversion);

        auto int8Con = Constraint::create(*this, ConstraintKind::Bind,
                                       baseType2, TC.getInt8Type(DC),
                                       getConstraintLocator(locator));
        auto uint8Con = Constraint::create(*this, ConstraintKind::Bind,
                                        baseType2, TC.getUInt8Type(DC),
                                        getConstraintLocator(locator));
        auto voidCon = Constraint::create(*this, ConstraintKind::Bind,
                                        baseType2, TC.Context.TheEmptyTupleType,
                                        getConstraintLocator(locator));
        
        Constraint *disjunctionChoices[] = {int8Con, uint8Con, voidCon};
        addDisjunctionConstraint(disjunctionChoices, locator);
        return SolutionKind::Solved;
      }

      return SolutionKind::Unsolved;
    }
    
    if (!isStringCompatiblePointerBaseType(TC, DC, baseType2)) {
      return SolutionKind::Error;
    }

    increaseScore(ScoreKind::SK_ValueToPointerConversion);
    return SolutionKind::Solved;
  }
      
  // T <p U ===> inout T <a UnsafeMutablePointer<U>
  case ConversionRestrictionKind::InoutToPointer: {
    addContextualScore();

    auto t2 = type2->getDesugaredType();
    
    auto baseType1 = type1->getInOutObjectType();
    auto baseType2 = getBaseTypeForPointer(*this, t2);
    
    // Set up the disjunction for the array or scalar cases.

    increaseScore(ScoreKind::SK_ValueToPointerConversion);
    return matchTypes(baseType1, baseType2,
                      ConstraintKind::BindToPointerType,
                      subflags, locator);
  }
      
  // T <p U ===> UnsafeMutablePointer<T> <a UnsafeMutablePointer<U>
  case ConversionRestrictionKind::PointerToPointer: {
    auto t1 = type1->getDesugaredType();
    auto t2 = type2->getDesugaredType();
    
    Type baseType1 = getBaseTypeForPointer(*this, t1);
    Type baseType2 = getBaseTypeForPointer(*this, t2);
    
    return matchTypes(baseType1, baseType2,
                      ConstraintKind::BindToPointerType,
                      subflags, locator);
  }
    
  // T < U or T is bridged to V where V < U ===> Array<T> <c Array<U>
  case ConversionRestrictionKind::ArrayUpcast: {
    Type baseType1 = *isArrayType(type1);
    Type baseType2 = *isArrayType(type2);

    increaseScore(SK_CollectionUpcastConversion);
    return matchTypes(baseType1,
                      baseType2,
                      matchKind,
                      subflags,
                      locator.withPathElement(
                        ConstraintLocator::PathElement::getGenericArgument(0)));
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

    auto subMatchKind = matchKind; // TODO: Restrict this?
    increaseScore(SK_CollectionUpcastConversion);
    // The source key and value types must be subtypes of the destination
    // key and value types, respectively.
    auto result = matchTypes(key1, key2, subMatchKind, subflags,
                             locator.withPathElement(
                    ConstraintLocator::PathElement::getGenericArgument(0)));
    if (result.isFailure())
      return result;

    switch (matchTypes(value1, value2, subMatchKind, subflags,
                       locator.withPathElement(
                  ConstraintLocator::PathElement::getGenericArgument(1)))) {
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
    Type baseType1 = *isSetType(type1);
    Type baseType2 = *isSetType(type2);

    increaseScore(SK_CollectionUpcastConversion);
    return matchTypes(baseType1,
                      baseType2,
                      matchKind,
                      subflags,
                      locator.withPathElement(
                    ConstraintLocator::PathElement::getGenericArgument(0)));
  }

  // T1 <c T2 && T2 : Hashable ===> T1 <c AnyHashable
  case ConversionRestrictionKind::HashableToAnyHashable: {
    // We never want to do this if the LHS is already AnyHashable.
    if (isAnyHashableType(
            type1->getRValueType()->lookThroughAllOptionalTypes())) {
      return SolutionKind::Error;
    }

    addContextualScore();
    increaseScore(SK_UserConversion); // FIXME: Use separate score kind?
    if (worseThanBestSolution()) {
      return SolutionKind::Error;
    }

    auto hashableProtocol =
      TC.Context.getProtocol(KnownProtocolKind::Hashable);
    if (!hashableProtocol)
      return SolutionKind::Error;

    auto constraintLocator = getConstraintLocator(locator);
    auto tv = createTypeVariable(constraintLocator, TVO_PrefersSubtypeBinding);
    
    addConstraint(ConstraintKind::ConformsTo, tv,
                  hashableProtocol->getDeclaredType(), constraintLocator);

    return matchTypes(type1, tv, ConstraintKind::Conversion, subflags,
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
                      type2, ConstraintKind::Subtype, subflags, locator);
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
                      ConstraintKind::Subtype, subflags, locator);
  }
  }
  
  llvm_unreachable("bad conversion restriction");
}

// Restrictions where CSApply can figure out the correct action from the shape of
// the types, rather than needing a record of the choice made.
static bool recordRestriction(ConversionRestrictionKind restriction) {
  switch(restriction) {
    case ConversionRestrictionKind::TupleToTuple:
    case ConversionRestrictionKind::LValueToRValue:
      return false;
    default:
      return true;
  }
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyRestrictedConstraint(
                                       ConversionRestrictionKind restriction,
                                       Type type1, Type type2,
                                       ConstraintKind matchKind,
                                       TypeMatchOptions flags,
                                       ConstraintLocatorBuilder locator) {
  switch (simplifyRestrictedConstraintImpl(restriction, type1, type2,
                                           matchKind, flags, locator)) {
  case SolutionKind::Solved:
    if (recordRestriction(restriction))
      ConstraintRestrictions.push_back(std::make_tuple(type1, type2, restriction));
    return SolutionKind::Solved;

  case SolutionKind::Unsolved:
    return SolutionKind::Unsolved;

  case SolutionKind::Error:
    return SolutionKind::Error;
  }

  llvm_unreachable("Unhandled SolutionKind in switch.");
}

static bool isAugmentingFix(ConstraintFix *fix) {
  switch (fix->getKind()) {
    case FixKind::TreatRValueAsLValue:
      return false;
    default:
      return true;
  }
}

bool ConstraintSystem::recordFix(ConstraintFix *fix) {
  auto &ctx = getASTContext();
  if (ctx.LangOpts.DebugConstraintSolver) {
    auto &log = ctx.TypeCheckerDebug->getStream();
    log.indent(solverState ? solverState->depth * 2 + 2 : 0)
      << "(attempting fix ";
    fix->print(log);
    log << ")\n";
  }

  // Record the fix.

  // Increase the score. If this would make the current solution worse than
  // the best solution we've seen already, stop now.
  increaseScore(SK_Fix);
  if (worseThanBestSolution())
    return true;

  if (isAugmentingFix(fix)) {
    // Always useful, unless duplicate of exactly the same fix and location.
    // This situation might happen when the same fix kind is applicable to
    // different overload choices.
    auto *loc = fix->getLocator();
    auto existingFix = llvm::find_if(Fixes, [&](const ConstraintFix *e) {
      // If we already have a fix like this recorded, let's not do it again,
      return e->getKind() == fix->getKind() && e->getLocator() == loc;
    });
    if (existingFix == Fixes.end())
      Fixes.push_back(fix);
  } else {
    // Only useful to record if no pre-existing fix in the subexpr tree.
    llvm::SmallDenseSet<Expr *> fixExprs;
    for (auto fix : Fixes)
      fixExprs.insert(fix->getAnchor());
    bool found = false;
    fix->getAnchor()->forEachChildExpr([&](Expr *subExpr) -> Expr * {
      found |= fixExprs.count(subExpr) > 0;
      return subExpr;
    });
    if (!found)
      Fixes.push_back(fix);
  }
  return false;
}

ConstraintSystem::SolutionKind ConstraintSystem::simplifyFixConstraint(
    ConstraintFix *fix, Type type1, Type type2, ConstraintKind matchKind,
    TypeMatchOptions flags, ConstraintLocatorBuilder locator) {
  // Try with the fix.
  TypeMatchOptions subflags =
    getDefaultDecompositionOptions(flags) | TMF_ApplyingFix;
  switch (fix->getKind()) {
  case FixKind::ForceOptional: {
    // Assume that we've unwrapped the first type.
    auto result =
        matchTypes(type1->getRValueType()->getOptionalObjectType(), type2,
                   matchKind, subflags, locator);
    if (result == SolutionKind::Solved)
      if (recordFix(fix))
        return SolutionKind::Error;

    return result;
  }

  case FixKind::UnwrapOptionalBase:
  case FixKind::UnwrapOptionalBaseWithOptionalResult: {
    if (recordFix(fix))
      return SolutionKind::Error;

    // First type already appropriately set.
    return matchTypes(type1, type2, matchKind, subflags, locator);
  }

  case FixKind::ForceDowncast:
    // These work whenever they are suggested.
    if (recordFix(fix))
      return SolutionKind::Error;

    return SolutionKind::Solved;

  case FixKind::AddressOf: {
    // Assume that '&' was applied to the first type, turning an lvalue into
    // an inout.
    auto result = matchTypes(InOutType::get(type1->getRValueType()), type2,
                             matchKind, subflags, locator);
    if (result == SolutionKind::Solved)
      if (recordFix(fix))
        return SolutionKind::Error;

    return result;
  }

  case FixKind::TreatRValueAsLValue: {
    if (type2->is<LValueType>() || type2->is<InOutType>())
      type1 = LValueType::get(type1);
    else
      type2 = LValueType::get(type2);
    auto result = matchTypes(type1, type2,
                             matchKind, subflags, locator);
    if (result == SolutionKind::Solved)
      if (recordFix(fix))
        return SolutionKind::Error;

    return result;
  }

  case FixKind::SkipSameTypeRequirement: {
    return recordFix(fix) ? SolutionKind::Error : SolutionKind::Solved;
  }

  case FixKind::ExplicitlyEscaping:
  case FixKind::CoerceToCheckedCast:
  case FixKind::RelabelArguments:
  case FixKind::AddConformance:
    llvm_unreachable("handled elsewhere");
  }

  llvm_unreachable("Unhandled FixKind in switch.");
}

ConstraintSystem::SolutionKind
ConstraintSystem::addConstraintImpl(ConstraintKind kind, Type first,
                                    Type second,
                                    ConstraintLocatorBuilder locator,
                                    bool isFavored) {
  assert(first && "Missing first type");
  assert(second && "Missing second type");

  TypeMatchOptions subflags = TMF_GenerateConstraints;
  switch (kind) {
  case ConstraintKind::Equal:
  case ConstraintKind::Bind:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::OperatorArgumentConversion:
    return matchTypes(first, second, kind, subflags, locator);

  case ConstraintKind::BridgingConversion:
    return simplifyBridgingConstraint(first, second, subflags, locator);

  case ConstraintKind::ApplicableFunction:
    return simplifyApplicableFnConstraint(first, second, subflags, locator);

  case ConstraintKind::DynamicTypeOf:
    return simplifyDynamicTypeOfConstraint(first, second, subflags, locator);

  case ConstraintKind::EscapableFunctionOf:
    return simplifyEscapableFunctionOfConstraint(first, second,
                                                 subflags, locator);

  case ConstraintKind::OpenedExistentialOf:
    return simplifyOpenedExistentialOfConstraint(first, second,
                                                 subflags, locator);

  case ConstraintKind::ConformsTo:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::SelfObjectOfProtocol:
    return simplifyConformsToConstraint(first, second, kind, locator,
                                        subflags);

  case ConstraintKind::CheckedCast:
    return simplifyCheckedCastConstraint(first, second, subflags, locator);

  case ConstraintKind::OptionalObject:
    return simplifyOptionalObjectConstraint(first, second, subflags, locator);

  case ConstraintKind::Defaultable:
    return simplifyDefaultableConstraint(first, second, subflags, locator);

  case ConstraintKind::FunctionInput:
  case ConstraintKind::FunctionResult:
    return simplifyFunctionComponentConstraint(kind, first, second,
                                               subflags, locator);

  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::BindOverload:
  case ConstraintKind::Disjunction:
  case ConstraintKind::KeyPath:
  case ConstraintKind::KeyPathApplication:
    llvm_unreachable("Use the correct addConstraint()");
  }

  llvm_unreachable("Unhandled ConstraintKind in switch.");
}

void
ConstraintSystem::addKeyPathApplicationRootConstraint(Type root, ConstraintLocatorBuilder locator) {
  // If this is a subscript with a KeyPath expression, add a constraint that
  // connects the subscript's root type to the root type of the KeyPath.
  SmallVector<LocatorPathElt, 4> path;
  Expr *anchor = locator.getLocatorParts(path);
  
  auto subscript = dyn_cast_or_null<SubscriptExpr>(anchor);
  if (!subscript)
    return;
  
  assert(path.size() == 1 &&
         path[0].getKind() == ConstraintLocator::SubscriptMember);
  auto indexTuple = dyn_cast<TupleExpr>(subscript->getIndex());
  if (!indexTuple || indexTuple->getNumElements() != 1)
    return;
  
  auto keyPathExpr = dyn_cast<KeyPathExpr>(indexTuple->getElement(0));
  if (!keyPathExpr)
    return;
  
  auto typeVar = getType(keyPathExpr)->getAs<TypeVariableType>();
  if (!typeVar)
    return;

  llvm::SetVector<Constraint *> constraints;
  CG.gatherConstraints(
      typeVar, constraints, ConstraintGraph::GatheringKind::EquivalenceClass,
      [&keyPathExpr](Constraint *constraint) -> bool {
        return constraint->getKind() == ConstraintKind::KeyPath &&
               constraint->getLocator()->getAnchor() == keyPathExpr;
      });

  for (auto constraint : constraints) {
    auto keyPathRootTy = constraint->getSecondType();
    addConstraint(ConstraintKind::Subtype, root->getWithoutSpecifierType(),
                  keyPathRootTy, locator);
  }
}

void
ConstraintSystem::addKeyPathApplicationConstraint(Type keypath,
                                              Type root, Type value,
                                              ConstraintLocatorBuilder locator,
                                              bool isFavored) {
  addKeyPathApplicationRootConstraint(root, locator);
  
  switch (simplifyKeyPathApplicationConstraint(keypath, root, value,
                                               TMF_GenerateConstraints,
                                               locator)) {
  case SolutionKind::Error:
    if (shouldAddNewFailingConstraint()) {
      auto c = Constraint::create(*this, ConstraintKind::KeyPathApplication,
                                  keypath, root, value,
                                  getConstraintLocator(locator));
      if (isFavored) c->setFavored();
      addNewFailingConstraint(c);
    }
    return;
  
  case SolutionKind::Solved:
    return;
    
  case SolutionKind::Unsolved:
    llvm_unreachable("should have generated constraints");
  }
}

void
ConstraintSystem::addKeyPathConstraint(Type keypath,
                                       Type root, Type value,
                                       ConstraintLocatorBuilder locator,
                                       bool isFavored) {
  switch (simplifyKeyPathConstraint(keypath, root, value,
                                    TMF_GenerateConstraints,
                                    locator)) {
  case SolutionKind::Error:
    if (shouldAddNewFailingConstraint()) {
      auto c = Constraint::create(*this, ConstraintKind::KeyPath,
                                  keypath, root, value,
                                  getConstraintLocator(locator));
      if (isFavored) c->setFavored();
      addNewFailingConstraint(c);
    }
    return;
  
  case SolutionKind::Solved:
    return;
    
  case SolutionKind::Unsolved:
    llvm_unreachable("should have generated constraints");
  }
}

void ConstraintSystem::addConstraint(Requirement req,
                                     ConstraintLocatorBuilder locator,
                                     bool isFavored) {
  bool conformsToAnyObject = false;
  Optional<ConstraintKind> kind;
  switch (req.getKind()) {
  case RequirementKind::Conformance:
    kind = ConstraintKind::ConformsTo;
    break;
  case RequirementKind::Superclass:
    conformsToAnyObject = true;
    kind = ConstraintKind::Subtype;
    break;
  case RequirementKind::SameType:
    kind = ConstraintKind::Equal;
    break;
  case RequirementKind::Layout:
    // Only a class constraint can be modeled as a constraint, and only that can
    // appear outside of a @_specialize at the moment anyway.
    if (req.getLayoutConstraint()->isClass()) {
      conformsToAnyObject = true;
      break;
    }
    return;
  }

  auto firstType = req.getFirstType();
  if (kind) {
    addConstraint(*kind, req.getFirstType(), req.getSecondType(), locator,
                  isFavored);
  }

  if (conformsToAnyObject) {
    auto anyObject = getASTContext().getAnyObjectType();
    addConstraint(ConstraintKind::ConformsTo, firstType, anyObject, locator);
  }
}

void ConstraintSystem::addConstraint(ConstraintKind kind, Type first,
                                     Type second,
                                     ConstraintLocatorBuilder locator,
                                     bool isFavored) {
  switch (addConstraintImpl(kind, first, second, locator, isFavored)) {
  case SolutionKind::Error:
    // Add a failing constraint, if needed.
    if (shouldAddNewFailingConstraint()) {
      auto c = Constraint::create(*this, kind, first, second,
                                  getConstraintLocator(locator));
      if (isFavored) c->setFavored();
      addNewFailingConstraint(c);
    }
    return;

  case SolutionKind::Unsolved:
    llvm_unreachable("should have generated constraints");

  case SolutionKind::Solved:
    return;
  }
}

void ConstraintSystem::addExplicitConversionConstraint(
                                           Type fromType, Type toType,
                                           bool allowFixes,
                                           ConstraintLocatorBuilder locator) {
  SmallVector<Constraint *, 3> constraints;

  auto locatorPtr = getConstraintLocator(locator);

  // Coercion (the common case).
  Constraint *coerceConstraint =
    Constraint::create(*this, ConstraintKind::Conversion,
                       fromType, toType, locatorPtr);
  coerceConstraint->setFavored();
  constraints.push_back(coerceConstraint);

  // The source type can be explicitly converted to the destination type.
  Constraint *bridgingConstraint =
  Constraint::create(*this, ConstraintKind::BridgingConversion,
                     fromType, toType, locatorPtr);
  constraints.push_back(bridgingConstraint);

  if (allowFixes && shouldAttemptFixes()) {
    Constraint *downcastConstraint =
        Constraint::createFixed(*this, ConstraintKind::CheckedCast,
                                CoerceToCheckedCast::create(*this, locatorPtr),
                                fromType, toType, locatorPtr);
    constraints.push_back(downcastConstraint);
  }

  addDisjunctionConstraint(constraints, locator,
                           allowFixes ? RememberChoice
                                      : ForgetChoice);
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstraint(const Constraint &constraint) {
  switch (constraint.getKind()) {
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::OperatorArgumentConversion: {
    // Relational constraints.
    auto matchKind = constraint.getKind();

    // If there is a fix associated with this constraint, apply it.
    if (auto fix = constraint.getFix()) {
      return simplifyFixConstraint(fix, constraint.getFirstType(),
                                   constraint.getSecondType(), matchKind, None,
                                   constraint.getLocator());
    }

    // If there is a restriction on this constraint, apply it directly rather
    // than going through the general \c matchTypes() machinery.
    if (auto restriction = constraint.getRestriction()) {
      return simplifyRestrictedConstraint(*restriction,
                                          constraint.getFirstType(),
                                          constraint.getSecondType(),
                                          matchKind, None,
                                          constraint.getLocator());
    }

    return matchTypes(constraint.getFirstType(), constraint.getSecondType(),
                      matchKind, None, constraint.getLocator());
  }

  case ConstraintKind::BridgingConversion:
    return simplifyBridgingConstraint(constraint.getFirstType(),
                                      constraint.getSecondType(),
                                      None, constraint.getLocator());

  case ConstraintKind::ApplicableFunction:
    return simplifyApplicableFnConstraint(constraint.getFirstType(),
                                          constraint.getSecondType(),
                                          None, constraint.getLocator());

  case ConstraintKind::DynamicTypeOf:
    return simplifyDynamicTypeOfConstraint(constraint.getFirstType(),
                                           constraint.getSecondType(),
                                           None,
                                           constraint.getLocator());

  case ConstraintKind::EscapableFunctionOf:
    return simplifyEscapableFunctionOfConstraint(constraint.getFirstType(),
                                                 constraint.getSecondType(),
                                                 None,
                                                 constraint.getLocator());

  case ConstraintKind::OpenedExistentialOf:
    return simplifyOpenedExistentialOfConstraint(constraint.getFirstType(),
                                                 constraint.getSecondType(),
                                                 None,
                                                 constraint.getLocator());

  case ConstraintKind::KeyPath:
    return simplifyKeyPathConstraint(
      constraint.getFirstType(), constraint.getSecondType(),
      constraint.getThirdType(),
      None, constraint.getLocator());

  case ConstraintKind::KeyPathApplication:
    return simplifyKeyPathApplicationConstraint(
      constraint.getFirstType(), constraint.getSecondType(),
      constraint.getThirdType(),
      None, constraint.getLocator());

  case ConstraintKind::BindOverload:
    resolveOverload(constraint.getLocator(), constraint.getFirstType(),
                    constraint.getOverloadChoice(),
                    constraint.getOverloadUseDC());
    return SolutionKind::Solved;

  case ConstraintKind::ConformsTo:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::SelfObjectOfProtocol:
    return simplifyConformsToConstraint(
             constraint.getFirstType(),
             constraint.getSecondType(),
             constraint.getKind(),
             constraint.getLocator(),
             None);

  case ConstraintKind::CheckedCast: {
    auto result = simplifyCheckedCastConstraint(constraint.getFirstType(),
                                                constraint.getSecondType(),
                                                None,
                                                constraint.getLocator());
    // NOTE: simplifyCheckedCastConstraint() may return Unsolved, e.g. if the
    // subexpression's type is unresolved. Don't record the fix until we
    // successfully simplify the constraint.
    if (result == SolutionKind::Solved) {
      if (auto *fix = constraint.getFix()) {
        if (recordFix(fix)) {
          return SolutionKind::Error;
        }
      }
    }
    return result;
  }

  case ConstraintKind::OptionalObject:
    return simplifyOptionalObjectConstraint(constraint.getFirstType(),
                                            constraint.getSecondType(),
                                            TMF_GenerateConstraints,
                                            constraint.getLocator());
      
  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
    return simplifyMemberConstraint(constraint.getKind(),
                                    constraint.getFirstType(),
                                    constraint.getMember(),
                                    constraint.getSecondType(),
                                    constraint.getMemberUseDC(),
                                    constraint.getFunctionRefKind(),
                                    /*outerAlternatives=*/{},
                                    TMF_GenerateConstraints,
                                    constraint.getLocator());

  case ConstraintKind::Defaultable:
    return simplifyDefaultableConstraint(constraint.getFirstType(),
                                         constraint.getSecondType(),
                                         TMF_GenerateConstraints,
                                         constraint.getLocator());

  case ConstraintKind::FunctionInput:
  case ConstraintKind::FunctionResult:
    return simplifyFunctionComponentConstraint(constraint.getKind(),
                                               constraint.getFirstType(),
                                               constraint.getSecondType(),
                                               TMF_GenerateConstraints,
                                               constraint.getLocator());

  case ConstraintKind::Disjunction:
    // Disjunction constraints are never solved here.
    return SolutionKind::Unsolved;
  }

  llvm_unreachable("Unhandled ConstraintKind in switch.");
}

void ConstraintSystem::simplifyDisjunctionChoice(Constraint *choice) {
  // Simplify this term in the disjunction.
  switch (simplifyConstraint(*choice)) {
  case ConstraintSystem::SolutionKind::Error:
    if (!failedConstraint)
      failedConstraint = choice;
    solverState->retireConstraint(choice);
    break;

  case ConstraintSystem::SolutionKind::Solved:
    solverState->retireConstraint(choice);
    break;

  case ConstraintSystem::SolutionKind::Unsolved:
    InactiveConstraints.push_back(choice);
    CG.addConstraint(choice);
    break;
  }

  // Record this as a generated constraint.
  solverState->addGeneratedConstraint(choice);
}
