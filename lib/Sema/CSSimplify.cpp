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
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/StringExtras.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Sema/IDETypeChecking.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Compiler.h"

using namespace swift;
using namespace constraints;

MatchCallArgumentListener::~MatchCallArgumentListener() { }

bool MatchCallArgumentListener::extraArgument(unsigned argIdx) { return true; }

Optional<unsigned>
MatchCallArgumentListener::missingArgument(unsigned paramIdx) {
  return None;
}

bool MatchCallArgumentListener::missingLabel(unsigned paramIdx) { return true; }
bool MatchCallArgumentListener::extraneousLabel(unsigned paramIdx) {
  return true;
}
bool MatchCallArgumentListener::incorrectLabel(unsigned paramIdx) {
  return true;
}

bool MatchCallArgumentListener::outOfOrderArgument(unsigned argIdx,
                                                   unsigned prevArgIdx) {
  return true;
}

bool MatchCallArgumentListener::relabelArguments(ArrayRef<Identifier> newNames){
  return true;
}

bool MatchCallArgumentListener::trailingClosureMismatch(
    unsigned paramIdx, unsigned argIdx) {
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

bool constraints::doesMemberRefApplyCurriedSelf(Type baseTy,
                                                const ValueDecl *decl) {
  assert(decl->getDeclContext()->isTypeContext() &&
         "Expected a member reference");

  // For a reference to an instance method on a metatype, we want to keep the
  // curried self.
  if (decl->isInstanceMember()) {
    assert(baseTy);
    if (isa<AbstractFunctionDecl>(decl) &&
        baseTy->getRValueType()->is<AnyMetatypeType>())
      return false;
  }

  // Otherwise the reference applies self.
  return true;
}

static bool areConservativelyCompatibleArgumentLabels(
    OverloadChoice choice, SmallVectorImpl<FunctionType::Param> &args,
    bool hasTrailingClosure) {
  ValueDecl *decl = nullptr;
  switch (choice.getKind()) {
  case OverloadChoiceKind::Decl:
  case OverloadChoiceKind::DeclViaBridge:
  case OverloadChoiceKind::DeclViaDynamic:
  case OverloadChoiceKind::DeclViaUnwrappedOptional:
    decl = choice.getDecl();
    break;

  case OverloadChoiceKind::BaseType:
  // KeyPath application is not filtered in `performMemberLookup`.
  case OverloadChoiceKind::KeyPathApplication:
  case OverloadChoiceKind::DynamicMemberLookup:
  case OverloadChoiceKind::KeyPathDynamicMemberLookup:
  case OverloadChoiceKind::TupleIndex:
    return true;
  }

  if (!decl->hasParameterList())
    return true;

  // This is a member lookup, which generally means that the call arguments
  // (if we have any) will apply to the second level of parameters, with
  // the member lookup applying the curried self at the first level. But there
  // are cases where we can get an unapplied declaration reference back.
  auto hasAppliedSelf =
      decl->hasCurriedSelf() &&
      doesMemberRefApplyCurriedSelf(choice.getBaseType(), decl);

  auto *fnType = decl->getInterfaceType()->castTo<AnyFunctionType>();
  if (hasAppliedSelf) {
    fnType = fnType->getResult()->getAs<AnyFunctionType>();
    assert(fnType && "Parameter list curry level does not match type");
  }

  auto params = fnType->getParams();
  ParameterListInfo paramInfo(params, decl, hasAppliedSelf);

  MatchCallArgumentListener listener;
  SmallVector<ParamBinding, 8> unusedParamBindings;

  return !matchCallArguments(args, params, paramInfo, hasTrailingClosure,
                             /*allow fixes*/ false, listener,
                             unusedParamBindings);
}

Expr *constraints::getArgumentLabelTargetExpr(Expr *fn) {
  // Dig out the function, looking through, parentheses, ?, and !.
  do {
    fn = fn->getSemanticsProvidingExpr();

    if (auto force = dyn_cast<ForceValueExpr>(fn)) {
      fn = force->getSubExpr();
      continue;
    }

    if (auto bind = dyn_cast<BindOptionalExpr>(fn)) {
      fn = bind->getSubExpr();
      continue;
    }

    return fn;
  } while (true);
}

/// Determine the default type-matching options to use when decomposing a
/// constraint into smaller constraints.
static ConstraintSystem::TypeMatchOptions getDefaultDecompositionOptions(
         ConstraintSystem::TypeMatchOptions flags) {
  return flags | ConstraintSystem::TMF_GenerateConstraints;
}

/// Determine whether the given parameter can accept a trailing closure.
static bool acceptsTrailingClosure(const AnyFunctionType::Param &param) {
  Type paramTy = param.getPlainType();
  if (!paramTy)
    return true;

  paramTy = paramTy->lookThroughAllOptionalTypes();
  return paramTy->isTypeParameter() ||
      paramTy->is<ArchetypeType>() ||
      paramTy->is<AnyFunctionType>() ||
      paramTy->isTypeVariableOrMember() ||
      paramTy->is<UnresolvedType>() ||
      paramTy->isAny();
}

// FIXME: This should return ConstraintSystem::TypeMatchResult instead
//        to give more information to the solver about the failure.
bool constraints::
matchCallArguments(SmallVectorImpl<AnyFunctionType::Param> &args,
                   ArrayRef<AnyFunctionType::Param> params,
                   const ParameterListInfo &paramInfo,
                   bool hasTrailingClosure,
                   bool allowFixes,
                   MatchCallArgumentListener &listener,
                   SmallVectorImpl<ParamBinding> &parameterBindings) {
  assert(params.size() == paramInfo.size() && "Default map does not match");

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
        if (argLabel.empty() &&
            (paramInfo.hasDefaultArgument(i) || !forVariadic))
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

      // If the argument is itself variadic, we're forwarding varargs
      // with a VarargExpansionExpr; don't collect any more arguments.
      if (args[*claimed].isVariadic()) {
        skipClaimedArgs();
        return;
      }

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
    unsigned lastParamIdx = numParams - 1;
    bool lastAcceptsTrailingClosure =
        acceptsTrailingClosure(params[lastParamIdx]);

    // If the last parameter is defaulted, this might be
    // an attempt to use a trailing closure with previous
    // parameter that accepts a function type e.g.
    //
    // func foo(_: () -> Int, _ x: Int = 0) {}
    // foo { 42 }
    if (!lastAcceptsTrailingClosure && numParams > 1 &&
        paramInfo.hasDefaultArgument(lastParamIdx)) {
      auto paramType = params[lastParamIdx - 1].getPlainType();
      // If the parameter before defaulted last accepts.
      if (paramType->is<AnyFunctionType>()) {
        lastAcceptsTrailingClosure = true;
        lastParamIdx -= 1;
      }
    }

    bool isExtraClosure = false;
    // If there is no suitable last parameter to accept the trailing closure,
    // notify the listener and bail if we need to.
    if (!lastAcceptsTrailingClosure) {
      if (numArgs > numParams) {
        // Argument before the trailing closure.
        unsigned prevArg = numArgs - 2;
        auto &arg = args[prevArg];
        // If the argument before trailing closure matches
        // last parameter, this is just a special case of
        // an extraneous argument.
        const auto param = params[numParams - 1];
        if (param.hasLabel() && param.getLabel() == arg.getLabel()) {
          isExtraClosure = true;
          if (listener.extraArgument(numArgs - 1))
            return true;
        }
      }

      if (!isExtraClosure &&
          listener.trailingClosureMismatch(lastParamIdx, numArgs - 1))
        return true;
    }

    // Claim the parameter/argument pair.
    claimedArgs[numArgs-1] = true;
    ++numClaimedArgs;
    // Let's claim the trailing closure unless it's an extra argument.
    if (!isExtraClosure)
      parameterBindings[lastParamIdx].push_back(numArgs - 1);
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
        if (paramInfo.hasDefaultArgument(i))
          continue;

        // Looks like there was no parameter claimed at the same
        // position, it could only mean that label is completely
        // different, because typo correction has been attempted already.
        parameterBindings[i].push_back(claim(params[i].getLabel(), i));
      }
    }

    // If we still haven't claimed all of the arguments,
    // fail if there is no recovery.
    if (numClaimedArgs != numArgs) {
      for (auto index : indices(claimedArgs)) {
        if (claimedArgs[index])
          continue;

        if (listener.extraArgument(index))
          return true;
      }
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
      if (paramInfo.hasDefaultArgument(paramIdx))
        continue;

      if (auto newArgIdx = listener.missingArgument(paramIdx)) {
        parameterBindings[paramIdx].push_back(*newArgIdx);
        continue;
      }

      return true;
    }
  }

  // If any arguments were provided out-of-order, check whether we have
  // violated any of the reordering rules.
  if (potentiallyOutOfOrder) {
    // If we've seen label failures and now there is an out-of-order
    // parameter (or even worse - OoO parameter with label re-naming),
    // we most likely have no idea what would be the best
    // diagnostic for this situation, so let's just try to re-label.
    auto isOutOfOrderArgument = [&](bool hadLabelMismatch, unsigned argIdx,
                                    unsigned prevArgIdx) {
      if (hadLabelMismatch)
        return false;

      auto newLabel = args[argIdx].getLabel();
      auto oldLabel = args[prevArgIdx].getLabel();

      unsigned actualIndex = prevArgIdx;
      for (; actualIndex != argIdx; ++actualIndex) {
        // Looks like new position (excluding defaulted parameters),
        // has a valid label.
        if (newLabel == params[actualIndex].getLabel())
          break;

        // If we are moving the the position with a different label
        // and there is no default value for it, can't diagnose the
        // problem as a simple re-ordering.
        if (!paramInfo.hasDefaultArgument(actualIndex))
          return false;
      }

      for (unsigned i = actualIndex + 1, n = params.size(); i != n; ++i) {
        if (oldLabel == params[i].getLabel())
          break;

        if (!paramInfo.hasDefaultArgument(i))
          return false;
      }

      return true;
    };

    unsigned argIdx = 0;
    // Enumerate the parameters and their bindings to see if any arguments are
    // our of order
    bool hadLabelMismatch = false;
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
            hadLabelMismatch = true;
            if (listener.extraneousLabel(toArgIdx))
              return true;
          // - The argument is unnamed, in which case we try to fix the
          //   problem by adding the name.
          } else if (argumentLabel.empty()) {
            hadLabelMismatch = true;
            if (listener.missingLabel(toArgIdx))
              return true;
          // - The argument label has a typo at the same position.
          } else if (fromArgIdx == toArgIdx) {
            hadLabelMismatch = true;
            if (listener.incorrectLabel(toArgIdx))
                return true;
          }
        }

        if (boundArgIdx == argIdx) {
          // If the argument is in the right location, just continue
          argIdx++;
          continue;
        }

        // This situation looks like out-of-order argument but it's hard
        // to say exactly without considering other factors, because it
        // could be invalid labeling too.
        if (isOutOfOrderArgument(hadLabelMismatch, fromArgIdx, toArgIdx))
          return listener.outOfOrderArgument(fromArgIdx, toArgIdx);

        SmallVector<Identifier, 8> expectedLabels;
        llvm::transform(params, std::back_inserter(expectedLabels),
                        [](const AnyFunctionType::Param &param) {
                          return param.getLabel();
                        });
        return listener.relabelArguments(expectedLabels);
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
static std::tuple<ValueDecl *, bool, ArrayRef<Identifier>, bool,
                  ConstraintLocator *>
getCalleeDeclAndArgs(ConstraintSystem &cs,
                     ConstraintLocatorBuilder callBuilder) {
  auto formUnknownCallee =
      []() -> std::tuple<ValueDecl *, bool, ArrayRef<Identifier>, bool,
                         ConstraintLocator *> {
    return std::make_tuple(/*decl*/ nullptr, /*hasAppliedSelf*/ false,
                           /*argLabels*/ ArrayRef<Identifier>(),
                           /*hasTrailingClosure*/ false,
                           /*calleeLocator*/ nullptr);
  };

  auto *callLocator = cs.getConstraintLocator(callBuilder);
  auto *callExpr = callLocator->getAnchor();

  // Break down the call.
  if (!callExpr)
    return formUnknownCallee();

  // Our remaining path can only be 'ApplyArgument'.
  auto path = callLocator->getPath();
  if (!path.empty() &&
      !(path.size() <= 2 &&
        path.back().getKind() == ConstraintLocator::ApplyArgument))
    return formUnknownCallee();

  // Dig out the callee information.
  auto argInfo = cs.getArgumentInfo(callLocator);
  if (!argInfo)
    return formUnknownCallee();

  auto argLabels = argInfo->Labels;
  auto hasTrailingClosure = argInfo->HasTrailingClosure;
  auto calleeLocator = cs.getCalleeLocator(callLocator);

  // Find the overload choice corresponding to the callee locator.
  // FIXME: This linearly walks the list of resolved overloads, which is
  // potentially very expensive.
  auto selectedOverload = cs.findSelectedOverloadFor(calleeLocator);

  // If we didn't find any matching overloads, we're done. Just return the
  // argument info.
  if (!selectedOverload)
    return std::make_tuple(/*decl*/ nullptr, /*hasAppliedSelf*/ false,
                           argLabels, hasTrailingClosure,
                           /*calleeLocator*/ nullptr);

  // Return the found declaration, assuming there is one.
  auto choice = selectedOverload->Choice;
  return std::make_tuple(choice.getDeclOrNull(), hasAppliedSelf(cs, choice),
                         argLabels, hasTrailingClosure, calleeLocator);
}

class ArgumentFailureTracker : public MatchCallArgumentListener {
  ConstraintSystem &CS;
  SmallVectorImpl<AnyFunctionType::Param> &Arguments;
  ArrayRef<AnyFunctionType::Param> Parameters;
  SmallVectorImpl<ParamBinding> &Bindings;
  ConstraintLocatorBuilder Locator;

  unsigned NumSynthesizedArgs = 0;
  SmallVector<std::pair<unsigned, AnyFunctionType::Param>, 4> ExtraArguments;

public:
  ArgumentFailureTracker(ConstraintSystem &cs,
                         SmallVectorImpl<AnyFunctionType::Param> &args,
                         ArrayRef<AnyFunctionType::Param> params,
                         SmallVectorImpl<ParamBinding> &bindings,
                         ConstraintLocatorBuilder locator)
      : CS(cs), Arguments(args), Parameters(params), Bindings(bindings),
        Locator(locator) {}

  ~ArgumentFailureTracker() override {
    if (NumSynthesizedArgs > 0) {
      ArrayRef<AnyFunctionType::Param> argRef(Arguments);

      auto *fix =
          AddMissingArguments::create(CS, argRef.take_back(NumSynthesizedArgs),
                                      CS.getConstraintLocator(Locator));

      // Not having an argument is the same impact as having a type mismatch.
      (void)CS.recordFix(fix, /*impact=*/NumSynthesizedArgs * 2);
    }
  }

  Optional<unsigned> missingArgument(unsigned paramIdx) override {
    if (!CS.shouldAttemptFixes())
      return None;

    const auto &param = Parameters[paramIdx];

    unsigned newArgIdx = Arguments.size();
    auto *argLoc = CS.getConstraintLocator(
        Locator
            .withPathElement(LocatorPathElt::ApplyArgToParam(
                newArgIdx, paramIdx, param.getParameterFlags()))
            .withPathElement(LocatorPathElt::SynthesizedArgument(newArgIdx)));

    auto *argType =
        CS.createTypeVariable(argLoc, TVO_CanBindToInOut | TVO_CanBindToLValue |
                                      TVO_CanBindToNoEscape | TVO_CanBindToHole);

    Arguments.push_back(param.withType(argType));
    ++NumSynthesizedArgs;

    return newArgIdx;
  }

  bool extraArgument(unsigned argIdx) override {
    if (!CS.shouldAttemptFixes())
      return true;

    ExtraArguments.push_back(std::make_pair(argIdx, Arguments[argIdx]));
    return false;
  }

  bool missingLabel(unsigned paramIndex) override {
    return !CS.shouldAttemptFixes();
  }

  bool extraneousLabel(unsigned paramIndex) override {
    return !CS.shouldAttemptFixes();
  }

  bool incorrectLabel(unsigned paramIndex) override {
    return !CS.shouldAttemptFixes();
  }

  bool outOfOrderArgument(unsigned argIdx, unsigned prevArgIdx) override {
    if (CS.shouldAttemptFixes()) {
      // If some of the arguments are missing/extraneous, no reason to
      // record a fix for this, increase the score so there is a way
      // to identify that there is something going on besides just missing
      // arguments.
      if (NumSynthesizedArgs || !ExtraArguments.empty()) {
        CS.increaseScore(SK_Fix);
        return false;
      }

      auto *fix = MoveOutOfOrderArgument::create(
          CS, argIdx, prevArgIdx, Bindings, CS.getConstraintLocator(Locator));
      return CS.recordFix(fix);
    }

    return true;
  }

  bool relabelArguments(ArrayRef<Identifier> newLabels) override {
    if (!CS.shouldAttemptFixes())
      return true;

    // TODO(diagnostics): If re-labeling is mixed with extra arguments,
    // let's produce a fix only for extraneous arguments for now,
    // because they'd share a locator path which (currently) means
    // one fix would overwrite another.
    if (!ExtraArguments.empty()) {
      CS.increaseScore(SK_Fix);
      return false;
    }

    auto *anchor = Locator.getBaseLocator()->getAnchor();
    if (!anchor)
      return true;

    unsigned numExtraneous = 0;
    unsigned numRenames = 0;
    unsigned numOutOfOrder = 0;

    for (unsigned i : indices(newLabels)) {
      // It's already known how many arguments are missing,
      // it would be accounted for in the impact.
      if (i >= Arguments.size())
        continue;

      auto argLabel = Arguments[i].getLabel();
      auto paramLabel = newLabels[i];

      if (argLabel == paramLabel)
        continue;

      if (!argLabel.empty()) {
        // Instead of this being a label mismatch which requires
        // re-labeling, this could be an out-of-order argument
        // instead which has a completely different impact.
        if (llvm::count(newLabels, argLabel) == 1) {
          ++numOutOfOrder;
        } else if (paramLabel.empty()) {
          ++numExtraneous;
        } else {
          ++numRenames;
        }
      }
    }

    auto *locator = CS.getConstraintLocator(Locator);
    auto *fix = RelabelArguments::create(CS, newLabels, locator);
    // Re-labeling fixes with extraneous/incorrect labels should be
    // lower priority vs. other fixes on same/different overload(s)
    // where labels did line up correctly.
    //
    // If there are not only labeling problems but also some of the
    // arguments are missing, let's account of that in the impact.
    auto impact = 1 + numOutOfOrder + numExtraneous * 2 + numRenames * 3 +
                  NumSynthesizedArgs * 2;
    return CS.recordFix(fix, impact);
  }

  bool trailingClosureMismatch(unsigned paramIdx, unsigned argIdx) override {
    if (!CS.shouldAttemptFixes())
      return true;

    auto argType = Arguments[argIdx].getPlainType();
    argType.visit([&](Type type) {
      if (auto *typeVar = type->getAs<TypeVariableType>())
        CS.recordPotentialHole(typeVar);
    });

    const auto &param = Parameters[paramIdx];

    auto *argLoc = CS.getConstraintLocator(
        Locator.withPathElement(LocatorPathElt::ApplyArgToParam(
            argIdx, paramIdx, param.getParameterFlags())));

    auto *fix = AllowInvalidUseOfTrailingClosure::create(
        CS, argType, param.getPlainType(), argLoc);
    return CS.recordFix(fix, /*impact=*/3);
  }

  ArrayRef<std::pair<unsigned, AnyFunctionType::Param>>
  getExtraneousArguments() const {
    return ExtraArguments;
  }
};

// Match the argument of a call to the parameter.
ConstraintSystem::TypeMatchResult constraints::matchCallArguments(
    ConstraintSystem &cs, FunctionType *contextualType,
    ArrayRef<AnyFunctionType::Param> args,
    ArrayRef<AnyFunctionType::Param> params, ConstraintKind subKind,
    ConstraintLocatorBuilder locator) {
  // Extract the parameters.
  ValueDecl *callee;
  bool hasAppliedSelf;
  ArrayRef<Identifier> argLabels;
  bool hasTrailingClosure = false;
  ConstraintLocator *calleeLocator;
  std::tie(callee, hasAppliedSelf, argLabels, hasTrailingClosure,
           calleeLocator) =
    getCalleeDeclAndArgs(cs, locator);

  ParameterListInfo paramInfo(params, callee, hasAppliedSelf);

  // Apply labels to arguments.
  SmallVector<AnyFunctionType::Param, 8> argsWithLabels;
  argsWithLabels.append(args.begin(), args.end());
  AnyFunctionType::relabelParams(argsWithLabels, argLabels);

  // Special case when a single tuple argument if used
  // instead of N distinct arguments e.g.:
  //
  // func foo(_ x: Int, _ y: Int) {}
  // foo((1, 2)) // expected 2 arguments, got a single tuple with 2 elements.
  if (cs.shouldAttemptFixes() && argsWithLabels.size() == 1 &&
      llvm::count_if(indices(params), [&](unsigned paramIdx) {
        return !paramInfo.hasDefaultArgument(paramIdx);
      }) > 1) {
    const auto &arg = argsWithLabels.front();
    auto argTuple = arg.getPlainType()->getRValueType()->getAs<TupleType>();
    // Don't explode a tuple in cases where first parameter is a tuple as
    // well. That is a regular "missing argument case" even if their arity
    // is different e.g.
    //
    // func foo(_: (Int, Int), _: Int) {}
    // foo((1, 2)) // call is missing an argument for parameter #1
    if (argTuple && argTuple->getNumElements() == params.size() &&
        !params.front().getPlainType()->is<TupleType>()) {
      argsWithLabels.pop_back();
      // Let's make sure that labels associated with tuple elements
      // line up with what is expected by argument list.
      for (const auto &arg : argTuple->getElements()) {
        argsWithLabels.push_back(
            AnyFunctionType::Param(arg.getType(), arg.getName()));
      }

      (void)cs.recordFix(
          AddMissingArguments::create(cs, argsWithLabels,
                                      cs.getConstraintLocator(locator)),
          /*impact=*/argsWithLabels.size() * 2);
    }
  }

  // Match up the call arguments to the parameters.
  SmallVector<ParamBinding, 4> parameterBindings;
  {
    ArgumentFailureTracker listener(cs, argsWithLabels, params,
                                    parameterBindings, locator);
    if (constraints::matchCallArguments(
            argsWithLabels, params, paramInfo, hasTrailingClosure,
            cs.shouldAttemptFixes(), listener, parameterBindings))
      return cs.getTypeMatchFailure(locator);

    auto extraArguments = listener.getExtraneousArguments();
    if (!extraArguments.empty()) {
      if (RemoveExtraneousArguments::isMinMaxNameShadowing(cs, locator))
        return cs.getTypeMatchFailure(locator);

      // First let's see whether this is a situation where a single
      // parameter is a tuple, but N distinct arguments were passed in.
      if (AllowTupleSplatForSingleParameter::attempt(
              cs, argsWithLabels, params, parameterBindings, locator)) {
        // Let's produce a generic "extraneous arguments"
        // diagnostic otherwise.
        auto *fix = RemoveExtraneousArguments::create(
            cs, contextualType, extraArguments,
            cs.getConstraintLocator(locator));

        if (cs.recordFix(fix, /*impact=*/extraArguments.size() * 5))
          return cs.getTypeMatchFailure(locator);
      }
    }
  }

  // If this application is part of an operator, then we allow an implicit
  // lvalue to be compatible with inout arguments.  This is used by
  // assignment operators.
  auto *anchor = locator.getAnchor();
  assert(anchor && "locator without anchor expression?");

  auto isSynthesizedArgument = [](const AnyFunctionType::Param &arg) -> bool {
    if (auto *typeVar = arg.getPlainType()->getAs<TypeVariableType>()) {
      auto *locator = typeVar->getImpl().getLocator();
      return locator->isLastElement<LocatorPathElt::SynthesizedArgument>();
    }

    return false;
  };

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
      auto loc = locator.withPathElement(LocatorPathElt::ApplyArgToParam(
          argIdx, paramIdx, param.getParameterFlags()));
      const auto &argument = argsWithLabels[argIdx];
      auto argTy = argument.getOldType();

      bool matchingAutoClosureResult = param.isAutoClosure();
      if (param.isAutoClosure() && !isSynthesizedArgument(argument)) {
        auto &ctx = cs.getASTContext();
        auto *fnType = paramTy->castTo<FunctionType>();
        auto *argExpr = getArgumentExpr(locator.getAnchor(), argIdx);

        // If the argument is not marked as @autoclosure or
        // this is Swift version >= 5 where forwarding is not allowed,
        // argument would always be wrapped into an implicit closure
        // at the end, so we can safely match against result type.
        if (ctx.isSwiftVersionAtLeast(5) || !isAutoClosureArgument(argExpr)) {
          // In Swift >= 5 mode there is no @autoclosure forwarding,
          // so let's match result types.
          paramTy = fnType->getResult();
        } else {
          // Matching @autoclosure argument to @autoclosure parameter
          // directly would mean introducting a function conversion
          // in Swift <= 4 mode.
          cs.increaseScore(SK_FunctionConversion);
          matchingAutoClosureResult = false;
        }
      }

      // If the parameter has a function builder type and the argument is a
      // closure, apply the function builder transformation.
      if (Type functionBuilderType
              = paramInfo.getFunctionBuilderType(paramIdx)) {
        Expr *arg = getArgumentExpr(locator.getAnchor(), argIdx);
        if (auto closure = dyn_cast_or_null<ClosureExpr>(arg)) {
          auto result =
              cs.applyFunctionBuilder(closure, functionBuilderType,
                                      calleeLocator, loc);
          if (result.isFailure())
            return result;
        }
      }

      // If argument comes for declaration it should loose
      // `@autoclosure` flag, because in context it's used
      // as a function type represented by autoclosure.
      //
      // Special case here are synthesized arguments because
      // they mirror parameter flags to ease diagnosis.
      assert(!argsWithLabels[argIdx].isAutoClosure() ||
             isSynthesizedArgument(argument));

      cs.addConstraint(
          subKind, argTy, paramTy,
          matchingAutoClosureResult
              ? loc.withPathElement(ConstraintLocator::AutoclosureResult)
              : loc,
          /*isFavored=*/false);
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
                                           LocatorPathElt::TupleElement(i)));
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

  case ConstraintKind::OpaqueUnderlyingType:
  case ConstraintKind::Bind:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Equal:
  case ConstraintKind::Subtype:
  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::DynamicCallableApplicableFunction:
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
  case ConstraintKind::OneWayEqual:
    llvm_unreachable("Not a conversion");
  }

  // Compute the element shuffles for conversions.
  SmallVector<unsigned, 16> sources;
  if (computeTupleShuffle(tuple1, tuple2, sources))
    return getTypeMatchFailure(locator);

  // Check each of the elements.
  for (unsigned idx2 = 0, n = sources.size(); idx2 != n; ++idx2) {
    unsigned idx1 = sources[idx2];

    // Match up the types.
    const auto &elt1 = tuple1->getElement(idx1);
    const auto &elt2 = tuple2->getElement(idx2);
    auto result = matchTypes(elt1.getType(), elt2.getType(), subKind, subflags,
                       locator.withPathElement(
                                        LocatorPathElt::TupleElement(idx1)));
    if (result.isFailure())
      return result;
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

  case ConstraintKind::OpaqueUnderlyingType:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::BridgingConversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::OperatorArgumentConversion:
  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::DynamicCallableApplicableFunction:
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
  case ConstraintKind::OneWayEqual:
    return false;
  }

  llvm_unreachable("Unhandled ConstraintKind in switch.");
}

/// Check whether given parameter list represents a single tuple
/// or type variable which could be later resolved to tuple.
/// This is useful for SE-0110 related fixes in `matchFunctionTypes`.
static bool isSingleTupleParam(ASTContext &ctx,
                               ArrayRef<AnyFunctionType::Param> params) {
  if (params.size() != 1)
    return false;

  const auto &param = params.front();
  if (param.isVariadic() || param.isInOut() || param.hasLabel())
    return false;

  auto paramType = param.getPlainType();

  // Support following case which was allowed until 5:
  //
  // func bar(_: (Int, Int) -> Void) {}
  // let foo: ((Int, Int)?) -> Void = { _ in }
  //
  // bar(foo) // Ok
  if (!ctx.isSwiftVersionAtLeast(5))
    paramType = paramType->lookThroughAllOptionalTypes();

  // Parameter type should either a tuple or something that can become a
  // tuple later on.
  return (paramType->is<TupleType>() || paramType->isTypeVariableOrMember());
}

static ConstraintFix *fixRequirementFailure(ConstraintSystem &cs, Type type1,
                                            Type type2, Expr *anchor,
                                            ArrayRef<LocatorPathElt> path);

static ConstraintFix *fixRequirementFailure(ConstraintSystem &cs, Type type1,
                                            Type type2,
                                            ConstraintLocatorBuilder locator) {
  SmallVector<LocatorPathElt, 4> path;
  if (auto *anchor = locator.getLocatorParts(path)) {
    return fixRequirementFailure(cs, type1, type2, anchor, path);
  }
  return nullptr;
}

static unsigned
assessRequirementFailureImpact(ConstraintSystem &cs, Type requirementType,
                               ConstraintLocatorBuilder locator) {
  auto *anchor = locator.getAnchor();
  if (!anchor)
    return 1;

  // If this requirement is associated with an overload choice let's
  // tie impact to how many times this requirement type is mentioned.
  if (auto *ODRE = dyn_cast<OverloadedDeclRefExpr>(anchor)) {
    if (!(requirementType && requirementType->is<TypeVariableType>()))
      return 1;

    unsigned choiceImpact = 0;
    if (auto *choice = cs.findSelectedOverloadFor(ODRE)) {
      auto *typeVar = requirementType->castTo<TypeVariableType>();
      choice->ImpliedType.visit([&](Type type) {
        if (type->isEqual(typeVar))
          ++choiceImpact;
      });
    }

    return choiceImpact == 0 ? 1 : choiceImpact;
  }

  // If this requirement is associated with a member reference and it
  // was possible to check it before overload choice is bound, that means
  // types came from the context (most likely Self, or associated type(s))
  // and failing this constraint makes member unrelated/inaccessible, so
  // the impact has to be adjusted accordingly in order for this fix not to
  // interfere with other overload choices.
  //
  // struct S<T> {}
  // extension S where T == AnyObject { func foo() {} }
  //
  // func bar(_ s: S<Int>) { s.foo() }
  //
  // In this case `foo` is only accessible if T == `AnyObject`, which makes
  // fix for same-type requirement higher impact vs. requirement associated
  // with method itself e.g. `func foo<U>() -> U where U : P {}` because
  // `foo` is accessible from any `S` regardless of what `T` is.
  if (isa<UnresolvedDotExpr>(anchor) || isa<UnresolvedMemberExpr>(anchor)) {
    auto *calleeLoc = cs.getCalleeLocator(cs.getConstraintLocator(locator));
    if (!cs.findSelectedOverloadFor(calleeLoc))
      return 10;
  }

  return 1;
}

/// Attempt to fix missing arguments by introducing type variables
/// and inferring their types from parameters.
static bool fixMissingArguments(ConstraintSystem &cs, Expr *anchor,
                                SmallVectorImpl<AnyFunctionType::Param> &args,
                                ArrayRef<AnyFunctionType::Param> params,
                                unsigned numMissing,
                                ConstraintLocatorBuilder locator) {
  assert(args.size() < params.size());

  auto &ctx = cs.getASTContext();
  // If there are N parameters but a single closure argument
  // (which might be anonymous), it's most likely used as a
  // tuple e.g. `$0.0`.
  Optional<TypeBase *> argumentTuple;
  if (isa<ClosureExpr>(anchor) && isSingleTupleParam(ctx, args)) {
    auto argType = args.back().getPlainType();
    // Let's unpack argument tuple into N arguments, this corresponds
    // to something like `foo { (bar: (Int, Int)) in }` where `foo`
    // has a single parameter of type `(Int, Int) -> Void`.
    if (auto *tuple = argType->getAs<TupleType>()) {
      args.pop_back();
      for (const auto &elt : tuple->getElements()) {
        args.push_back(AnyFunctionType::Param(elt.getType(), elt.getName(),
                                              elt.getParameterFlags()));
      }
    } else if (auto *typeVar = argType->getAs<TypeVariableType>()) {
      auto isParam = [](const Expr *expr) {
        if (auto *DRE = dyn_cast<DeclRefExpr>(expr)) {
          if (auto *decl = DRE->getDecl())
            return isa<ParamDecl>(decl);
        }
        return false;
      };

      // Something like `foo { x in }` or `foo { $0 }`
      anchor->forEachChildExpr([&](Expr *expr) -> Expr * {
        if (auto *UDE = dyn_cast<UnresolvedDotExpr>(expr)) {
          if (!isParam(UDE->getBase()))
            return expr;

          auto name = UDE->getName().getBaseIdentifier();
          unsigned index = 0;
          if (!name.str().getAsInteger(10, index) ||
              llvm::any_of(params, [&](const AnyFunctionType::Param &param) {
                return param.getLabel() == name;
              })) {
            argumentTuple.emplace(typeVar);
            args.pop_back();
            return nullptr;
          }
        }
        return expr;
      });
    }
  }

  for (unsigned i = args.size(), n = params.size(); i != n; ++i) {
    auto *argLoc = cs.getConstraintLocator(
        anchor, LocatorPathElt::SynthesizedArgument(i));
    args.push_back(params[i].withType(cs.createTypeVariable(argLoc,
                                                      TVO_CanBindToNoEscape)));
  }

  ArrayRef<AnyFunctionType::Param> argsRef(args);
  auto *fix = AddMissingArguments::create(cs, argsRef.take_back(numMissing),
                                          cs.getConstraintLocator(locator));

  if (cs.recordFix(fix))
    return true;

  // If the argument was a single "tuple", let's bind newly
  // synthesized arguments to it.
  if (argumentTuple) {
    cs.addConstraint(ConstraintKind::Bind, *argumentTuple,
                     FunctionType::composeInput(ctx, args,
                                                /*canonicalVararg=*/false),
                     cs.getConstraintLocator(anchor));
  }

  return false;
}

static bool fixExtraneousArguments(ConstraintSystem &cs,
                                   FunctionType *contextualType,
                                   ArrayRef<AnyFunctionType::Param> args,
                                   int numExtraneous,
                                   ConstraintLocatorBuilder locator) {
  SmallVector<std::pair<unsigned, AnyFunctionType::Param>, 4> extraneous;

  for (unsigned i = args.size() - numExtraneous, n = args.size(); i != n; ++i) {
    extraneous.push_back({i, args[i]});
    if (auto *typeVar = args[i].getPlainType()->getAs<TypeVariableType>()) {
      cs.recordPotentialHole(typeVar);
    }
  }

  return cs.recordFix(
      RemoveExtraneousArguments::create(cs, contextualType, extraneous,
                                        cs.getConstraintLocator(locator)),
      /*impact=*/numExtraneous * 2);
}

ConstraintSystem::TypeMatchResult
ConstraintSystem::matchFunctionTypes(FunctionType *func1, FunctionType *func2,
                                     ConstraintKind kind, TypeMatchOptions flags,
                                     ConstraintLocatorBuilder locator) {
  // A non-throwing function can be a subtype of a throwing function.
  if (func1->throws() != func2->throws()) {
    // Cannot drop 'throws'.
    if (func1->throws() || kind < ConstraintKind::Subtype) {
      if (!shouldAttemptFixes())
        return getTypeMatchFailure(locator);

      auto *fix = DropThrowsAttribute::create(*this, func1, func2,
                                              getConstraintLocator(locator));
      if (recordFix(fix))
        return getTypeMatchFailure(locator);
    }
  }

  // A non-@noescape function type can be a subtype of a @noescape function
  // type.
  if (func1->isNoEscape() != func2->isNoEscape() &&
      (func1->isNoEscape() || kind < ConstraintKind::Subtype)) {
    if (!shouldAttemptFixes())
      return getTypeMatchFailure(locator);

    auto *fix = MarkExplicitlyEscaping::create(
        *this, getConstraintLocator(locator), func2);

    if (recordFix(fix))
      return getTypeMatchFailure(locator);
  }

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
  case ConstraintKind::OpaqueUnderlyingType:
    subKind = ConstraintKind::Subtype;
    break;

  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::DynamicCallableApplicableFunction:
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
  case ConstraintKind::OneWayEqual:
    llvm_unreachable("Not a relational constraint");
  }

  // Input types can be contravariant (or equal).
  auto argumentLocator =
      locator.withPathElement(ConstraintLocator::FunctionArgument);

  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  SmallVector<AnyFunctionType::Param, 8> func1Params;
  func1Params.append(func1->getParams().begin(), func1->getParams().end());

  SmallVector<AnyFunctionType::Param, 8> func2Params;
  func2Params.append(func2->getParams().begin(), func2->getParams().end());

  // Add a very narrow exception to SE-0110 by allowing functions that
  // take multiple arguments to be passed as an argument in places
  // that expect a function that takes a single tuple (of the same
  // arity);
  auto canImplodeParams = [&](ArrayRef<AnyFunctionType::Param> params) {
    if (params.size() == 1)
      return false;

    for (auto param : params)
      if (param.isVariadic() || param.isInOut() || param.isAutoClosure())
        return false;

    return true;
  };

  auto implodeParams = [&](SmallVectorImpl<AnyFunctionType::Param> &params) {
    auto input = AnyFunctionType::composeInput(getASTContext(), params,
                                               /*canonicalVararg=*/false);

    params.clear();
    // If fixes are disabled let's do an easy thing and implode
    // tuple directly into parameters list.
    if (!shouldAttemptFixes()) {
      params.emplace_back(input);
      return;
    }

    // Synthesize new argument and bind it to tuple formed from existing
    // arguments, this makes it easier to diagnose cases where we attempt
    // a single tuple element formed when no arguments were present.
    auto argLoc = argumentLocator.withPathElement(
        LocatorPathElt::SynthesizedArgument(0));
    auto *typeVar = createTypeVariable(getConstraintLocator(argLoc),
                                       TVO_CanBindToNoEscape);
    params.emplace_back(typeVar);
    assignFixedType(typeVar, input);
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

    auto &ctx = getASTContext();
    if (last != path.rend()) {
      if (last->getKind() == ConstraintLocator::ApplyArgToParam) {
        if (isSingleTupleParam(ctx, func2Params) &&
            canImplodeParams(func1Params)) {
          implodeParams(func1Params);
        } else if (!ctx.isSwiftVersionAtLeast(5) &&
                   isSingleTupleParam(ctx, func1Params) &&
                   canImplodeParams(func2Params)) {
          auto *simplified = locator.trySimplifyToExpr();
          // We somehow let tuple unsplatting function conversions
          // through in some cases in Swift 4, so let's let that
          // continue to work, but only for Swift 4.
          if (simplified &&
              (isa<DeclRefExpr>(simplified) ||
               isa<OverloadedDeclRefExpr>(simplified) ||
               isa<UnresolvedDeclRefExpr>(simplified))) {
            implodeParams(func2Params);
          }
        }
      }
    }

    if (shouldAttemptFixes()) {
      auto *anchor = locator.trySimplifyToExpr();
      if (anchor && isa<ClosureExpr>(anchor) &&
          isSingleTupleParam(ctx, func2Params) &&
          canImplodeParams(func1Params)) {
        auto *fix = AllowClosureParamDestructuring::create(
            *this, func2, getConstraintLocator(anchor));
        if (recordFix(fix))
          return getTypeMatchFailure(argumentLocator);

        implodeParams(func1Params);
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
        if (isSingleTupleParam(getASTContext(), func1Params) &&
            func1Params[0].getOldType()->isVoid()) {
          if (func2Params.empty()) {
            func2Params.emplace_back(getASTContext().TheEmptyTupleType);
          }
        }
      }
    }
  }

  int diff = func1Params.size() - func2Params.size();
  if (diff != 0) {
    if (!shouldAttemptFixes())
      return getTypeMatchFailure(argumentLocator);

    auto *anchor = locator.trySimplifyToExpr();
    if (!anchor)
      return getTypeMatchFailure(argumentLocator);

    // If there are missing arguments, let's add them
    // using parameter as a template.
    if (diff < 0) {
      if (fixMissingArguments(*this, anchor, func1Params, func2Params,
                              abs(diff), locator))
        return getTypeMatchFailure(argumentLocator);
    } else {
      // If there are extraneous arguments, let's remove
      // them from the list.
      if (fixExtraneousArguments(*this, func2, func1Params, diff, locator))
        return getTypeMatchFailure(argumentLocator);

      // Drop all of the extraneous arguments.
      auto numParams = func2Params.size();
      func1Params.erase(func1Params.begin() + numParams, func1Params.end());
    }
  }

  bool hasLabelingFailures = false;
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
    if (func1Param.getLabel() != func2Param.getLabel()) {
      if (!shouldAttemptFixes())
        return getTypeMatchFailure(argumentLocator);

      // If we are allowed to attempt fixes, let's ignore labeling
      // failures, and create a fix to re-label arguments if types
      // line up correctly.
      hasLabelingFailures = true;
    }

    // FIXME: We should check value ownership too, but it's not completely
    // trivial because of inout-to-pointer conversions.

    // Compare the parameter types.
    auto result = matchTypes(func2Param.getOldType(),
                             func1Param.getOldType(),
                             subKind, subflags,
                             (func1Params.size() == 1
                              ? argumentLocator
                              : argumentLocator.withPathElement(
                                LocatorPathElt::TupleElement(i))));
    if (result.isFailure())
      return result;
  }

  if (hasLabelingFailures) {
    SmallVector<Identifier, 4> correctLabels;
    for (const auto &param : func2Params)
      correctLabels.push_back(param.getLabel());

    auto *fix = RelabelArguments::create(*this, correctLabels,
                                         getConstraintLocator(argumentLocator));
    if (recordFix(fix))
      return getTypeMatchFailure(argumentLocator);
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
  SmallPtrSet<ClassDecl *, 4> superclasses1;
  for (auto super1 = type1->getSuperclass();
       super1;
       super1 = super1->getSuperclass()) {
    auto superclass1 = super1->getClassOrBoundGenericClass();
    if (superclass1 != classDecl2) {
      // Break if we have circular inheritance.
      if (superclass1 && !superclasses1.insert(superclass1).second)
        break;

      continue;
    }

    return matchTypes(super1, type2, ConstraintKind::Bind,
                      subflags, locator);
  }

  return getTypeMatchFailure(locator);
}

static ConstraintSystem::TypeMatchResult matchDeepTypeArguments(
    ConstraintSystem &cs, ConstraintSystem::TypeMatchOptions subflags,
    ArrayRef<Type> args1, ArrayRef<Type> args2,
    ConstraintLocatorBuilder locator,
    llvm::function_ref<void(unsigned)> recordMismatch = [](unsigned) {}) {
  if (args1.size() != args2.size()) {
    return cs.getTypeMatchFailure(locator);
  }

  auto allMatch = cs.getTypeMatchSuccess();
  for (unsigned i = 0, n = args1.size(); i != n; ++i) {
    auto result = cs.matchTypes(
        args1[i], args2[i], ConstraintKind::Bind, subflags,
        locator.withPathElement(LocatorPathElt::GenericArgument(i)));

    if (result.isFailure()) {
      recordMismatch(i);
      allMatch = result;
    }
  }

  return allMatch;
}

ConstraintSystem::TypeMatchResult
ConstraintSystem::matchDeepEqualityTypes(Type type1, Type type2,
                                         ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = TMF_GenerateConstraints;

  // Handle opaque archetypes.
  if (auto arch1 = type1->getAs<ArchetypeType>()) {
    auto arch2 = type2->castTo<ArchetypeType>();
    auto opaque1 = cast<OpaqueTypeArchetypeType>(arch1->getRoot());
    auto opaque2 = cast<OpaqueTypeArchetypeType>(arch2->getRoot());
    assert(arch1->getInterfaceType()->getCanonicalType(
                      opaque1->getGenericEnvironment()->getGenericSignature())
        == arch2->getInterfaceType()->getCanonicalType(
                      opaque2->getGenericEnvironment()->getGenericSignature()));
    assert(opaque1->getDecl() == opaque2->getDecl());
    
    auto args1 = opaque1->getSubstitutions().getReplacementTypes();
    auto args2 = opaque2->getSubstitutions().getReplacementTypes();

    if (!shouldAttemptFixes()) {
      // Match up the replacement types of the respective substitution maps.
      return matchDeepTypeArguments(*this, subflags, args1, args2, locator);
    }

    unsigned numMismatches = 0;
    auto result =
        matchDeepTypeArguments(*this, subflags, args1, args2, locator,
                               [&numMismatches](unsigned) { ++numMismatches; });

    if (numMismatches > 0) {
      auto *anchor = locator.getAnchor();
      // TODO(diagnostics): Only assignments are supported at the moment.
      if (!(anchor && isa<AssignExpr>(anchor)))
        return getTypeMatchFailure(locator);

      auto *fix = IgnoreAssignmentDestinationType::create(
          *this, type1, type2, getConstraintLocator(locator));

      if (recordFix(fix, /*impact=*/numMismatches))
        return getTypeMatchFailure(locator);

      return getTypeMatchSuccess();
    }

    return result;
  }

  // Handle protocol compositions.
  if (auto existential1 = type1->getAs<ProtocolCompositionType>()) {
    if (auto existential2 = type2->getAs<ProtocolCompositionType>()) {
      auto layout1 = existential1->getExistentialLayout();
      auto layout2 = existential2->getExistentialLayout();

      // Explicit AnyObject and protocols must match exactly.
      if (layout1.hasExplicitAnyObject != layout2.hasExplicitAnyObject)
        return getTypeMatchFailure(locator);

      if (layout1.getProtocols().size() != layout2.getProtocols().size())
        return getTypeMatchFailure(locator);

      for (unsigned i: indices(layout1.getProtocols())) {
        if (!layout1.getProtocols()[i]->isEqual(layout2.getProtocols()[i]))
          return getTypeMatchFailure(locator);
      }

      // This is the only interesting case. We might have type variables
      // on either side of the superclass constraint, so make sure we
      // recursively call matchTypes() here.
      if (layout1.explicitSuperclass || layout2.explicitSuperclass) {
        if (!layout1.explicitSuperclass || !layout2.explicitSuperclass)
          return getTypeMatchFailure(locator);

        auto result = matchTypes(layout1.explicitSuperclass,
                                 layout2.explicitSuperclass,
                                 ConstraintKind::Bind, subflags,
                                 locator.withPathElement(
                                   ConstraintLocator::ExistentialSuperclassType));
        if (result.isFailure())
          return result;
      }

      return getTypeMatchSuccess();
    }
  }
  // Handle nominal types that are not directly generic.
  if (auto nominal1 = type1->getAs<NominalType>()) {
    auto nominal2 = type2->castTo<NominalType>();

    assert((bool)nominal1->getParent() == (bool)nominal2->getParent() &&
           "Mismatched parents of nominal types");

    if (!nominal1->getParent())
      return getTypeMatchSuccess();

    // Match up the parents, exactly.
    return matchTypes(nominal1->getParent(), nominal2->getParent(),
                      ConstraintKind::Bind, subflags,
                      locator.withPathElement(ConstraintLocator::ParentType));
  }

  auto bound1 = type1->castTo<BoundGenericType>();
  auto bound2 = type2->castTo<BoundGenericType>();

  // Match up the parents, exactly, if there are parents.
  assert((bool)bound1->getParent() == (bool)bound2->getParent() &&
         "Mismatched parents of bound generics");
  if (bound1->getParent()) {
    auto result = matchTypes(bound1->getParent(), bound2->getParent(),
                             ConstraintKind::Bind, subflags,
                             locator.withPathElement(
                                                ConstraintLocator::ParentType));
    if (result.isFailure())
      return result;
  }

  auto args1 = bound1->getGenericArgs();
  auto args2 = bound2->getGenericArgs();

  // Match up the generic arguments, exactly.

  if (shouldAttemptFixes()) {
    // Optionals have a lot of special diagnostics and only one
    // generic argument so if we' re dealing with one, don't produce generic
    // arguments mismatch fixes.
    // TODO(diagnostics): Move Optional diagnostics over to the
    // new framework.
    if (bound1->getDecl()->isOptionalDecl())
      return matchDeepTypeArguments(*this, subflags, args1, args2, locator);

    SmallVector<unsigned, 4> mismatches;
    auto result = matchDeepTypeArguments(
        *this, subflags, args1, args2, locator,
        [&mismatches](unsigned position) { mismatches.push_back(position); });

    if (mismatches.empty())
      return result;

    if (auto last = locator.last()) {
      if (last->is<LocatorPathElt::AnyRequirement>()) {
        if (auto *fix = fixRequirementFailure(*this, type1, type2, locator)) {
          if (recordFix(fix))
            return getTypeMatchFailure(locator);

          increaseScore(SK_Fix, mismatches.size());
          return getTypeMatchSuccess();
        }
      }
    }

    auto *fix = GenericArgumentsMismatch::create(
        *this, type1, type2, mismatches, getConstraintLocator(locator));

    if (!recordFix(fix)) {
      // Increase the solution's score for each mismtach this fixes.
      increaseScore(SK_Fix, mismatches.size() - 1);
      return getTypeMatchSuccess();
    }
    return result;
  }
  return matchDeepTypeArguments(*this, subflags, args1, args2, locator);
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
  // we need to disallow conversions from types containing @noescape
  // functions to Any.

  // Conformance to 'Any' always holds.
  if (type2->isAny()) {
    if (!type1->isNoEscape())
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
        if (!type1->satisfiesClassConstraint()) {
          if (shouldAttemptFixes()) {
            if (auto last = locator.last()) {
              // If solver is in diagnostic mode and type1 is a hole, or if this
              // is a superclass requirement, let's consider `AnyObject`
              // conformance solved. The actual superclass requirement
              // will also fail (because type can't satisfy it), and it's
              // more interesting for diagnostics.
              auto req = last->getAs<LocatorPathElt::AnyRequirement>();
              if (type1->isHole() || (req &&
                  req->getRequirementKind() == RequirementKind::Superclass))
                return getTypeMatchSuccess();

              auto *fix = fixRequirementFailure(*this, type1, type2, locator);
              if (fix && !recordFix(fix)) {
                recordFixedRequirement(type1, RequirementKind::Layout, type2);
                return getTypeMatchSuccess();
              }
            }
          }

          return getTypeMatchFailure(locator);
        }
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

      case SolutionKind::Error: {
        if (!shouldAttemptFixes())
          return getTypeMatchFailure(locator);

        // Determine whether this conformance mismatch is
        // associate with argument to a call, and if so
        // produce a tailored fix.
        if (auto last = locator.last()) {
          if (last->is<LocatorPathElt::ApplyArgToParam>()) {
            auto *fix = AllowArgumentMismatch::create(
                *this, type1, proto, getConstraintLocator(locator));

            // Impact is 2 here because there are two failures
            // 1 - missing conformance and 2 - incorrect argument type.
            //
            // This would make sure that arguments with incorrect
            // conformances are not prioritized over general argument
            // mismatches.
            if (recordFix(fix, /*impact=*/2))
              return getTypeMatchFailure(locator);

            break;
          }
        } else { // There are no elements in the path
          auto *anchor = locator.getAnchor();
          if (!(anchor && isa<AssignExpr>(anchor)))
            return getTypeMatchFailure(locator);
        }

        auto *fix = MissingConformance::forContextual(
            *this, type1, proto, getConstraintLocator(locator));

        if (recordFix(fix))
          return getTypeMatchFailure(locator);

        break;
      }
    }
  }

  return getTypeMatchSuccess();
}

static bool isStringCompatiblePointerBaseType(ASTContext &ctx,
                                              Type baseType) {
  // Allow strings to be passed to pointer-to-byte or pointer-to-void types.
  if (baseType->isEqual(TypeChecker::getInt8Type(ctx)))
    return true;
  if (baseType->isEqual(TypeChecker::getUInt8Type(ctx)))
    return true;
  if (baseType->isEqual(ctx.TheEmptyTupleType))
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

  // Attempt to fix situations where type variable can't be bound
  // to a particular type e.g. `l-value` or `inout`.
  auto fixReferenceMismatch = [&](TypeVariableType *typeVar,
                                  Type type) -> bool {
    if (auto last = locator.last()) {
      if (last->is<LocatorPathElt::ContextualType>()) {
        auto *fix = IgnoreContextualType::create(*this, typeVar, type,
                                                 getConstraintLocator(locator));
        return !recordFix(fix);
      }
    }

    return false;
  };

  // If the left-hand type variable cannot bind to an lvalue,
  // but we still have an lvalue, fail.
  if (!typeVar->getImpl().canBindToLValue() && type->hasLValueType()) {
    if (shouldAttemptFixes() && fixReferenceMismatch(typeVar, type))
      return getTypeMatchSuccess();

    return getTypeMatchFailure(locator);
  }

  // If the left-hand type variable cannot bind to an inout,
  // but we still have an inout, fail.
  if (!typeVar->getImpl().canBindToInOut() && type->is<InOutType>()) {
    if (shouldAttemptFixes() && fixReferenceMismatch(typeVar, type))
      return getTypeMatchSuccess();

    return getTypeMatchFailure(locator);
  }

  // If the left-hand type variable cannot bind to a non-escaping type,
  // but we still have a non-escaping type, fail.
  if (!typeVar->getImpl().canBindToNoEscape() && type->isNoEscape()) {
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

  // We do not allow keypaths to go through AnyObject. Let's create a fix
  // so this can be diagnosed later.
  if (auto loc = typeVar->getImpl().getLocator()) {
    auto locPath = loc->getPath();

    if (!locPath.empty() &&
        locPath.back().getKind() == ConstraintLocator::KeyPathRoot &&
        type->isAnyObject()) {
      auto *fix = AllowAnyObjectKeyPathRoot::create(
          *this, getConstraintLocator(locator));

      if (recordFix(fix))
        return getTypeMatchFailure(locator);
    }
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

  // When binding a fixed type to a type variable that cannot contain
  // lvalues or noescape types, any type variables within the fixed
  // type cannot contain lvalues or noescape types either.
  if (type->hasTypeVariable()) {
    type.visit([&](Type t) {
      if (auto *tvt = dyn_cast<TypeVariableType>(t.getPointer())) {
        if (!typeVar->getImpl().canBindToLValue()) {
          tvt->getImpl().setCanBindToLValue(getSavedBindings(),
                                            /*enabled=*/false);
        }
        if (!typeVar->getImpl().canBindToNoEscape()) {
          tvt->getImpl().setCanBindToNoEscape(getSavedBindings(),
                                              /*enabled=*/false);
        }
      }
    });
  }

  assignFixedType(typeVar, type);

  return getTypeMatchSuccess();
}

static ConstraintFix *fixRequirementFailure(ConstraintSystem &cs, Type type1,
                                            Type type2, Expr *anchor,
                                            ArrayRef<LocatorPathElt> path) {
  // Can't fix not yet properly resolved types.
  if (type1->isTypeVariableOrMember() || type2->isTypeVariableOrMember())
    return nullptr;

  auto req = path.back().castTo<LocatorPathElt::AnyRequirement>();
  if (req.isConditionalRequirement()) {
    // path is - ... -> open generic -> type req # -> cond req #,
    // to identify type requirement we only need `open generic -> type req #`
    // part, because that's how fixes for type requirements are recorded.
    auto reqPath = path.drop_back();
    // If underlying conformance requirement has been fixed,
    // then there is no reason to fix up conditional requirements.
    if (cs.hasFixFor(cs.getConstraintLocator(anchor, reqPath)))
      return nullptr;
  }

  auto *reqLoc = cs.getConstraintLocator(anchor, path);

  switch (req.getRequirementKind()) {
  case RequirementKind::SameType: {
    return SkipSameTypeRequirement::create(cs, type1, type2, reqLoc);
  }

  case RequirementKind::Superclass: {
    return SkipSuperclassRequirement::create(cs, type1, type2, reqLoc);
  }

  case RequirementKind::Layout:
  case RequirementKind::Conformance:
    return MissingConformance::forRequirement(cs, type1, type2, reqLoc);
  }
  llvm_unreachable("covered switch");
}

static ConstraintFix *fixPropertyWrapperFailure(
    ConstraintSystem &cs, Type baseTy, ConstraintLocator *locator,
    llvm::function_ref<bool(ResolvedOverloadSetListItem *, VarDecl *, Type)>
        attemptFix,
    Optional<Type> toType = None) {

  Expr *baseExpr = nullptr;
  if (auto *anchor = locator->getAnchor()) {
    if (auto *UDE = dyn_cast<UnresolvedDotExpr>(anchor))
      baseExpr = UDE->getBase();
    else if (auto *SE = dyn_cast<SubscriptExpr>(anchor))
      baseExpr = SE->getBase();
    else if (auto *MRE = dyn_cast<MemberRefExpr>(anchor))
      baseExpr = MRE->getBase();
    else if (auto *anchor = simplifyLocatorToAnchor(locator))
      baseExpr = anchor;
  }

  if (!baseExpr)
    return nullptr;

  auto resolvedOverload = cs.findSelectedOverloadFor(baseExpr);
  if (!resolvedOverload)
    return nullptr;

  enum class Fix : uint8_t {
    StorageWrapper,
    PropertyWrapper,
    WrappedValue,
  };

  auto applyFix = [&](Fix fix, VarDecl *decl, Type type) -> ConstraintFix * {
    if (!decl->hasInterfaceType() || !type)
      return nullptr;

    if (baseTy->isEqual(type))
      return nullptr;

    if (!attemptFix(resolvedOverload, decl, type))
      return nullptr;

    switch (fix) {
    case Fix::StorageWrapper:
    case Fix::PropertyWrapper:
      return UsePropertyWrapper::create(cs, decl, fix == Fix::StorageWrapper,
                                        baseTy, toType.getValueOr(type),
                                        locator);

    case Fix::WrappedValue:
      return UseWrappedValue::create(cs, decl, baseTy, toType.getValueOr(type),
                                     locator);
    }
    llvm_unreachable("Unhandled Fix type in switch");
  };

  if (auto storageWrapper = cs.getStorageWrapperInformation(resolvedOverload)) {
    if (auto *fix = applyFix(Fix::StorageWrapper, storageWrapper->first,
                             storageWrapper->second))
      return fix;
  }

  if (auto wrapper = cs.getPropertyWrapperInformation(resolvedOverload)) {
    if (auto *fix =
            applyFix(Fix::PropertyWrapper, wrapper->first, wrapper->second))
      return fix;
  }

  if (auto wrappedProperty =
          cs.getWrappedPropertyInformation(resolvedOverload)) {
    if (auto *fix = applyFix(Fix::WrappedValue, wrappedProperty->first,
                             wrappedProperty->second))
      return fix;
  }

  return nullptr;
}

static bool canBridgeThroughCast(ConstraintSystem &cs, Type fromType,
                                 Type toType) {
  // If we have a value of type AnyObject that we're trying to convert to
  // a class, force a downcast.
  // FIXME: Also allow types bridged through Objective-C classes.
  if (fromType->isAnyObject() && toType->getClassOrBoundGenericClass())
    return true;

  auto bridged = TypeChecker::getDynamicBridgedThroughObjCClass(cs.DC,
                                                                fromType, toType);
  if (!bridged)
    return false;

  // Note: don't perform this recovery for NSNumber;
  if (auto classType = bridged->getAs<ClassType>()) {
    SmallString<16> scratch;
    if (classType->getDecl()->isObjC() &&
        classType->getDecl()->getObjCRuntimeName(scratch) == "NSNumber")
      return false;
  }

  return true;
}

static bool
repairViaBridgingCast(ConstraintSystem &cs, Type fromType, Type toType,
                      SmallVectorImpl<RestrictionOrFix> &conversionsOrFixes,
                      ConstraintLocatorBuilder locator) {
  auto objectType1 = fromType->getOptionalObjectType();
  auto objectType2 = toType->getOptionalObjectType();

  if (objectType1 && !objectType2) {
    auto *anchor = locator.trySimplifyToExpr();
    if (!anchor)
      return false;

    if (auto *overload = cs.findSelectedOverloadFor(anchor)) {
      auto *decl = overload->Choice.getDeclOrNull();
      if (decl && decl->isImplicitlyUnwrappedOptional())
        fromType = objectType1;
    }
  }

  if (!canBridgeThroughCast(cs, fromType, toType))
    return false;

  conversionsOrFixes.push_back(ForceDowncast::create(
      cs, fromType, toType, cs.getConstraintLocator(locator)));
  return true;
}

static bool
repairViaOptionalUnwrap(ConstraintSystem &cs, Type fromType, Type toType,
                        ConstraintKind matchKind,
                        SmallVectorImpl<RestrictionOrFix> &conversionsOrFixes,
                        ConstraintLocatorBuilder locator) {
  fromType = fromType->getWithoutSpecifierType();

  if (!fromType->getOptionalObjectType() || toType->is<TypeVariableType>())
    return false;

  // If we have an optional type, try to force-unwrap it.
  // FIXME: Should we also try '?'?
  auto *anchor = locator.trySimplifyToExpr();
  if (!anchor)
    return false;

  // `OptionalEvaluationExpr` doesn't add a new level of
  // optionality but it could be hiding concrete types
  // behind itself which we can use to better understand
  // how many levels of optionality have to be unwrapped.
  if (auto *OEE = dyn_cast<OptionalEvaluationExpr>(anchor)) {
    auto type = cs.getType(OEE->getSubExpr());
    // If the type of sub-expression is optional, type of the
    // `OptionalEvaluationExpr` could be safely ignored because
    // it doesn't add any type information.
    if (type->getOptionalObjectType())
      fromType = type;

    // If this is a conversion from optional chain to some
    // other type e.g. contextual type or a parameter type,
    // let's use `Bind` to match object types because
    // object type of the optinal chain is a type variable.
    if (matchKind >= ConstraintKind::Conversion)
      matchKind = ConstraintKind::Bind;
  }

  if (auto *DRE = dyn_cast<DeclRefExpr>(anchor)) {
    if (DRE->getDecl()->isImplicit()) {
      // The expression that provides the first type is implicit and never
      // spelled out in source code, e.g. $match in an expression pattern.
      // Thus we cannot force unwrap the first type
      return false;
    }
  }

  if (auto *optTryExpr = dyn_cast<OptionalTryExpr>(anchor)) {
    auto subExprType = cs.getType(optTryExpr->getSubExpr());
    const bool isSwift5OrGreater =
        cs.getASTContext().LangOpts.isSwiftVersionAtLeast(5);

    if (subExprType->getOptionalObjectType()) {
      if (isSwift5OrGreater) {
        // For 'try?' expressions, a ForceOptional fix converts 'try?'
        // to 'try!'. If the sub-expression is optional, then a force-unwrap
        // won't change anything in Swift 5+ because 'try?' already avoids
        // adding an additional layer of Optional there.
        return false;
      }
    } else {
      // In cases when sub-expression isn't optional, 'try?'
      // always adds one level of optinality regardless of
      // language mode, so we can safely try to bind its
      // object type to contextual type without risk of
      // causing more optionality mismatches down the road.
      matchKind = ConstraintKind::Bind;
    }
  }

  auto getObjectTypeAndUnwraps = [](Type type) -> std::pair<Type, unsigned> {
    SmallVector<Type, 2> optionals;
    Type objType = type->lookThroughAllOptionalTypes(optionals);
    return std::make_pair(objType, optionals.size());
  };

  Type fromObjectType, toObjectType;
  unsigned fromUnwraps, toUnwraps;

  std::tie(fromObjectType, fromUnwraps) = getObjectTypeAndUnwraps(fromType);
  std::tie(toObjectType, toUnwraps) = getObjectTypeAndUnwraps(toType);

  // If `from` is not less optional than `to`, force unwrap is
  // not going to help here. In case of object type of `from`
  // is a type variable, let's assume that it might be optional.
  if (fromUnwraps <= toUnwraps && !fromObjectType->is<TypeVariableType>())
    return false;

  auto result =
      cs.matchTypes(fromObjectType, toObjectType, matchKind,
                    ConstraintSystem::TypeMatchFlags::TMF_ApplyingFix, locator);
  if (!result.isSuccess())
    return false;

  conversionsOrFixes.push_back(ForceOptional::create(
      cs, fromType, toType, cs.getConstraintLocator(locator)));
  return true;
}

/// Attempt to repair typing failures and record fixes if needed.
/// \return true if at least some of the failures has been repaired
/// successfully, which allows type matcher to continue.
bool ConstraintSystem::repairFailures(
    Type lhs, Type rhs, ConstraintKind matchKind,
    SmallVectorImpl<RestrictionOrFix> &conversionsOrFixes,
    ConstraintLocatorBuilder locator) {
  SmallVector<LocatorPathElt, 4> path;
  auto *anchor = locator.getLocatorParts(path);

  // If there is a missing explicit call it could be:
  //
  // a). Contextual e.g. `let _: R = foo`
  // b). Argument is a function value passed to parameter
  //     which expects its result type e.g. `foo(bar)`
  // c). Assigment destination type matches return type of
  //     of the function value e.g. `foo = bar` or `foo = .bar`
  auto repairByInsertingExplicitCall = [&](Type srcType, Type dstType) -> bool {
    auto fnType = srcType->getAs<FunctionType>();
    if (!fnType)
      return false;

    // If argument is a function type and all of its parameters have
    // default values, let's see whether error is related to missing
    // explicit call.
    if (fnType->getNumParams() > 0) {
      auto *anchor = simplifyLocatorToAnchor(getConstraintLocator(locator));
      if (!anchor)
        return false;

      auto *overload = findSelectedOverloadFor(anchor);
      if (!(overload && overload->Choice.isDecl()))
        return false;

      const auto &choice = overload->Choice;
      ParameterListInfo info(fnType->getParams(), choice.getDecl(),
                             hasAppliedSelf(*this, choice));

      if (llvm::any_of(indices(fnType->getParams()),
                       [&info](const unsigned idx) {
                         return !info.hasDefaultArgument(idx);
                       }))
        return false;
    }

    auto resultType = fnType->getResult();
    // If this is situation like `x = { ... }` where closure results in
    // `Void`, let's not suggest to call the closure, because it's most
    // likely not intended.
    if (anchor && isa<AssignExpr>(anchor)) {
      auto *assignment = cast<AssignExpr>(anchor);
      if (isa<ClosureExpr>(assignment->getSrc()) && resultType->isVoid())
        return false;
    }

    // If left-hand side is a function type but right-hand
    // side isn't, let's check it would be possible to fix
    // this by forming an explicit call.
    auto convertTo = dstType->lookThroughAllOptionalTypes();
    // Right-hand side can't be - a function, a type variable or dependent
    // member, or `Any` (if function conversion to `Any` didn't succeed there
    // is something else going on e.g. problem with escapiness).
    if (convertTo->is<FunctionType>() || convertTo->isTypeVariableOrMember() ||
        convertTo->isAny())
      return false;

    auto result = matchTypes(resultType, dstType, ConstraintKind::Conversion,
                             TypeMatchFlags::TMF_ApplyingFix, locator);

    if (result.isSuccess()) {
      conversionsOrFixes.push_back(
          InsertExplicitCall::create(*this, getConstraintLocator(locator)));
      return true;
    }

    return false;
  };

  auto repairByAnyToAnyObjectCast = [&](Type lhs, Type rhs) -> bool {
    if (!(lhs->isAny() && rhs->isAnyObject()))
      return false;

    conversionsOrFixes.push_back(MissingConformance::forContextual(
        *this, lhs, rhs, getConstraintLocator(locator)));
    return true;
  };

  auto repairByTreatingRValueAsLValue = [&](Type lhs, Type rhs) -> bool {
    if (!lhs->is<LValueType>() &&
        (rhs->is<LValueType>() || rhs->is<InOutType>())) {
      // Conversion from l-value to inout in an operator argument
      // position (which doesn't require explicit `&`) decays into
      // a `Bind` of involved object types, same goes for explicit
      // `&` conversion from l-value to inout type.
      auto kind = (isa<InOutExpr>(anchor) ||
                   (rhs->is<InOutType>() &&
                    matchKind == ConstraintKind::OperatorArgumentConversion))
                      ? ConstraintKind::Bind
                      : matchKind;

      auto result = matchTypes(lhs, rhs->getWithoutSpecifierType(), kind,
                               TMF_ApplyingFix, locator);

      if (result.isSuccess()) {
        conversionsOrFixes.push_back(
            TreatRValueAsLValue::create(*this, getConstraintLocator(locator)));
        return true;
      }
    }

    return false;
  };

  auto hasConversionOrRestriction = [&](ConversionRestrictionKind kind) {
    return llvm::any_of(conversionsOrFixes,
                        [kind](const RestrictionOrFix correction) {
                          if (auto restriction = correction.getRestriction())
                            return restriction == kind;
                          return false;
                        });
  };

  if (path.empty()) {
    if (!anchor)
      return false;

    if (auto *coercion = dyn_cast<CoerceExpr>(anchor)) {
      // Let's check whether the sub-expression is an optional type which
      // is possible to unwrap (either by force or `??`) to satisfy the cast,
      // otherwise we'd have to fallback to force downcast.
      if (repairViaOptionalUnwrap(*this, lhs, rhs, matchKind,
                                  conversionsOrFixes,
                                  getConstraintLocator(coercion->getSubExpr())))
        return true;

      // Repair a coercion ('as') with a forced checked cast ('as!').
      if (auto *coerceToCheckCastFix = CoerceToCheckedCast::attempt(
              *this, lhs, rhs, getConstraintLocator(locator))) {
        conversionsOrFixes.push_back(coerceToCheckCastFix);
        return true;
      }
    }

    // This could be:
    // - `InOutExpr` used with r-value e.g. `foo(&x)` where `x` is a `let`.
    // - `ForceValueExpr` e.g. `foo.bar! = 42` where `bar` or `foo` are
    //   immutable or a subscript e.g. `foo["bar"]! = 42`.
    if (repairByTreatingRValueAsLValue(lhs, rhs))
      return true;

    // If method reference forms a value type of the key path,
    // there is going to be a constraint to match result of the
    // member lookup to the generic parameter `V` of *KeyPath<R, V>
    // type associated with key path expression, which we need to
    // fix-up here.
    if (isa<KeyPathExpr>(anchor)) {
      auto *fnType = lhs->getAs<FunctionType>();
      if (fnType && fnType->getResult()->isEqual(rhs))
        return true;

      conversionsOrFixes.push_back(IgnoreContextualType::create(
          *this, lhs, rhs, getConstraintLocator(locator)));
      return true;
    }

    if (auto *AE = dyn_cast<AssignExpr>(anchor)) {
      if (repairByInsertingExplicitCall(lhs, rhs))
        return true;

      if (isa<InOutExpr>(AE->getSrc())) {
        conversionsOrFixes.push_back(
            RemoveAddressOf::create(*this, lhs, rhs,
                                    getConstraintLocator(locator)));
        return true;
      }

      if (repairByAnyToAnyObjectCast(lhs, rhs))
        return true;

      if (repairViaBridgingCast(*this, lhs, rhs, conversionsOrFixes, locator))
        return true;

      // If destination is `AnyObject` it means that source doesn't conform.
      if (rhs->getWithoutSpecifierType()
              ->lookThroughAllOptionalTypes()
              ->isAnyObject()) {
        conversionsOrFixes.push_back(IgnoreAssignmentDestinationType::create(
            *this, lhs, rhs, getConstraintLocator(locator)));
        return true;
      }

      // An attempt to assign `Int?` to `String?`.
      if (hasConversionOrRestriction(
              ConversionRestrictionKind::OptionalToOptional)) {
        conversionsOrFixes.push_back(IgnoreAssignmentDestinationType::create(
            *this, lhs, rhs, getConstraintLocator(locator)));
        return true;
      }

      // If we are trying to assign e.g. `Array<Int>` to `Array<Float>` let's
      // give solver a chance to determine which generic parameters are
      // mismatched and produce a fix for that.
      if (hasConversionOrRestriction(ConversionRestrictionKind::DeepEquality))
        return false;

      // If the situation has to do with protocol composition types and
      // destination doesn't have one of the conformances e.g. source is
      // `X & Y` but destination is only `Y` or vice versa, there is a
      // tailored "missing conformance" fix for that.
      if (hasConversionOrRestriction(ConversionRestrictionKind::Existential))
        return false;

      if (hasConversionOrRestriction(
              ConversionRestrictionKind::MetatypeToExistentialMetatype) ||
          hasConversionOrRestriction(
              ConversionRestrictionKind::ExistentialMetatypeToMetatype) ||
          hasConversionOrRestriction(ConversionRestrictionKind::Superclass)) {
        conversionsOrFixes.push_back(IgnoreAssignmentDestinationType::create(
            *this, lhs, rhs, getConstraintLocator(locator)));
        return true;
      }

      if (hasConversionOrRestriction(
              ConversionRestrictionKind::ValueToOptional)) {
        lhs = lhs->lookThroughAllOptionalTypes();
        rhs = rhs->lookThroughAllOptionalTypes();

        // If both object types are functions, let's allow the solver to
        // structurally compare them before trying to fix anything.
        if (lhs->is<FunctionType>() && rhs->is<FunctionType>())
          return false;

        // If either object type is a bound generic or existential it means
        // that follow-up to value-to-optional is going to be:
        //
        // 1. "deep equality" check, which is handled by generic argument(s)
        //    mismatch fix, or
        // 2. "existential" check, which is handled by a missing conformance
        //    fix.
        if ((lhs->is<BoundGenericType>() && rhs->is<BoundGenericType>()) ||
            rhs->isAnyExistentialType())
          return false;
      }

      auto *destExpr = AE->getDest();
      // Literal expression as well as call/operator application can't be
      // used as an assignment destination because resulting type is immutable.
      if (isa<ApplyExpr>(destExpr) || isa<LiteralExpr>(destExpr)) {
        conversionsOrFixes.push_back(
            TreatRValueAsLValue::create(*this, getConstraintLocator(locator)));
        return true;
      }

      // If destination has a function type, it might either be
      // a property with a function type or a method reference,
      // e.g. `foo.bar = 42` neither can be used if the destination
      // is not l-value.
      if (!getType(destExpr)->is<LValueType>() && rhs->is<FunctionType>()) {
        conversionsOrFixes.push_back(
            TreatRValueAsLValue::create(*this, getConstraintLocator(locator)));
        return true;
      }

      if (repairViaOptionalUnwrap(*this, lhs, rhs, matchKind,
                                  conversionsOrFixes, locator))
        return true;

      // Let's try to match source and destination types one more
      // time to see whether they line up, if they do - the problem is
      // related to immutability, otherwise it's a type mismatch.
      auto result = matchTypes(lhs, rhs, ConstraintKind::Conversion,
                               TMF_ApplyingFix, locator);

      if (getType(destExpr)->is<LValueType>() || result.isFailure()) {
        conversionsOrFixes.push_back(IgnoreAssignmentDestinationType::create(
            *this, lhs, rhs, getConstraintLocator(locator)));
      } else {
        conversionsOrFixes.push_back(
            TreatRValueAsLValue::create(*this, getConstraintLocator(locator)));
      }

      return true;
    }

    return false;
  }

  auto elt = path.back();
  switch (elt.getKind()) {
  case ConstraintLocator::LValueConversion: {
    auto CTP = getContextualTypePurpose();
    // Special case for `CTP_CallArgument` set by CSDiag
    // while type-checking each argument because we yet
    // to cover argument-to-parameter conversions in the
    // new framework.
    if (CTP != CTP_CallArgument) {
      // Ignore l-value conversion element since it has already
      // played its role.
      path.pop_back();
      // If this is a contextual mismatch between l-value types e.g.
      // `@lvalue String vs. @lvalue Int`, let's pretend that it's okay.
      if (!path.empty() && path.back().is<LocatorPathElt::ContextualType>()) {
        auto *locator = getConstraintLocator(anchor, path.back());
        conversionsOrFixes.push_back(
            IgnoreContextualType::create(*this, lhs, rhs, locator));
        break;
      }
    }

    LLVM_FALLTHROUGH;
  }

  case ConstraintLocator::ApplyArgToParam: {
    auto loc = getConstraintLocator(locator);

    if (hasFixFor(loc, FixKind::AllowInvalidUseOfTrailingClosure))
      return true;

    if (repairByInsertingExplicitCall(lhs, rhs))
      break;

    bool isPatternMatching = isArgumentOfPatternMatchingOperator(loc);
    // Let's not suggest force downcasts in pattern-matching context.
    if (!isPatternMatching &&
        repairViaBridgingCast(*this, lhs, rhs, conversionsOrFixes, locator))
      break;

    // If this is an argument to `===` or `!==` there are tailored
    // diagnostics available for it as part of argument-to-parameter
    // conversion fix, so let's not try any restrictions or other fixes.
    if (isArgumentOfReferenceEqualityOperator(loc)) {
      conversionsOrFixes.push_back(
          AllowArgumentMismatch::create(*this, lhs, rhs, loc));
      break;
    }

    // Argument is a r-value but parameter expects an l-value e.g.
    //
    // func foo(_ x: inout Int) {}
    // let x: Int = 42
    // foo(x) // `x` can't be converted to `inout Int`.
    //
    // This has to happen before checking for optionality mismatch
    // because otherwise `Int? arg conv inout Int` is going to get
    // fixed as 2 fixes - "force unwrap" + r-value -> l-value mismatch.
    if (repairByTreatingRValueAsLValue(lhs, rhs))
      break;

    // If the problem is related to missing unwrap, there is a special
    // fix for that.
    if (lhs->getOptionalObjectType() && !rhs->getOptionalObjectType()) {
      // If this is an attempt to check whether optional conforms to a
      // particular protocol, let's do that before attempting to force
      // unwrap the optional.
      if (hasConversionOrRestriction(ConversionRestrictionKind::Existential))
        break;

      auto result = matchTypes(lhs->getOptionalObjectType(), rhs, matchKind,
                               TMF_ApplyingFix, locator);

      if (result.isSuccess()) {
        conversionsOrFixes.push_back(
            ForceOptional::create(*this, lhs, rhs, loc));
        break;
      }
    }

    // There is no subtyping between object types of inout argument/parameter.
    if (elt.getKind() == ConstraintLocator::LValueConversion) {
      auto result = matchTypes(lhs, rhs, ConstraintKind::Conversion,
                               TMF_ApplyingFix, locator);

      ConstraintFix *fix = nullptr;
      if (result.isFailure()) {
        // If this is a "destination" argument to a mutating operator
        // like `+=`, let's consider it contextual and only attempt
        // to fix type mismatch on the "source" right-hand side of
        // such operators.
        if (isOperatorArgument(loc) &&
            loc->findLast<LocatorPathElt::ApplyArgToParam>()->getArgIdx() == 0)
          break;

        fix = AllowArgumentMismatch::create(*this, lhs, rhs, loc);
      } else {
        fix = AllowInOutConversion::create(*this, lhs, rhs, loc);
      }

      conversionsOrFixes.push_back(fix);
      break;
    }

    if (elt.getKind() != ConstraintLocator::ApplyArgToParam)
      break;

    if (auto *fix = fixPropertyWrapperFailure(
            *this, lhs, loc,
            [&](ResolvedOverloadSetListItem *overload, VarDecl *decl,
                Type newBase) {
              // FIXME: There is currently no easy way to avoid attempting
              // fixes, matchTypes do not propagate `TMF_ApplyingFix` flag.
              llvm::SaveAndRestore<ConstraintSystemOptions> options(
                  Options, Options - ConstraintSystemFlags::AllowFixes);

              TypeMatchOptions flags;
              return matchTypes(newBase, rhs, ConstraintKind::Subtype, flags,
                                getConstraintLocator(locator))
                  .isSuccess();
            },
            rhs)) {
      conversionsOrFixes.push_back(fix);
      break;
    }

    // If argument in l-value type and parameter is `inout` or a pointer,
    // let's see if it's generic parameter matches and suggest adding explicit
    // `&`.
    if (lhs->is<LValueType>() &&
        (rhs->is<InOutType>() || rhs->getAnyPointerElementType())) {
      auto baseType = rhs->is<InOutType>() ? rhs->getInOutObjectType()
                                           : rhs->getAnyPointerElementType();

      // Let's use `BindToPointer` constraint here to match up base types
      // of implied `inout` argument and `inout` or pointer parameter.
      // This helps us to avoid implicit conversions associated with
      // `ArgumentConversion` constraint.
      auto result = matchTypes(lhs->getRValueType(), baseType,
                               ConstraintKind::BindToPointerType,
                               TypeMatchFlags::TMF_ApplyingFix, locator);

      if (result.isSuccess()) {
        conversionsOrFixes.push_back(AddAddressOf::create(
            *this, lhs, rhs, getConstraintLocator(locator)));
        break;
      }
    }

    // If the argument is inout and the parameter is not inout or a pointer,
    // suggest removing the &.
    if (lhs->is<InOutType>() && !rhs->is<InOutType>()) {
      auto objectType = rhs->lookThroughAllOptionalTypes();
      if (!objectType->getAnyPointerElementType()) {
        auto result = matchTypes(lhs->getInOutObjectType(), rhs,
                                 ConstraintKind::ArgumentConversion,
                                 TypeMatchFlags::TMF_ApplyingFix, locator);

        if (result.isSuccess()) {
          conversionsOrFixes.push_back(RemoveAddressOf::create(
              *this, lhs, rhs, getConstraintLocator(locator)));
          break;
        }
      }
    }

    // If parameter type is `Any` the problem might be related to
    // invalid escapiness of the argument.
    if (rhs->isAny())
      break;

    // If there are any other argument mismatches already detected
    // for this call, we can consider overload unrelated.
    if (llvm::any_of(getFixes(), [&](const ConstraintFix *fix) {
          auto *locator = fix->getLocator();
          return locator->findLast<LocatorPathElt::ApplyArgToParam>()
                     ? locator->getAnchor() == anchor
                     : false;
        }))
      break;

    // If this is something like `[A] argument conv [B]` where `A` and `B`
    // are unrelated types, let's give `matchTypes` a chance to consider
    // element types.
    if (hasConversionOrRestriction(ConversionRestrictionKind::DeepEquality))
      break;

    // If there right-hand side is an existential value, let's allow conformance
    // check to happen before trying to do anything else for arguments.
    if (hasConversionOrRestriction(ConversionRestrictionKind::Existential))
      break;

    // If there implicit 'something-to-pointer' conversions involved,
    // such conversions are going to be diagnosed by specialized fix
    // which deals with generic argument mismatches.
    if (hasConversionOrRestriction(ConversionRestrictionKind::ArrayToPointer) ||
        hasConversionOrRestriction(ConversionRestrictionKind::InoutToPointer) ||
        hasConversionOrRestriction(
            ConversionRestrictionKind::PointerToPointer) ||
        matchKind == ConstraintKind::BindToPointerType)
      break;

    // If this is a ~= operator implicitly generated by pattern matching
    // let's not try to fix right-hand side of the operator because it's
    // a correct contextual type.
    if (isPatternMatching &&
        elt.castTo<LocatorPathElt::ApplyArgToParam>().getParamIdx() == 1)
      break;

    if (auto *fix = ExpandArrayIntoVarargs::attempt(*this, lhs, rhs, locator)) {
      conversionsOrFixes.push_back(fix);
      break;
    }

    if (auto *fix = ExplicitlyConstructRawRepresentable::attempt(
            *this, lhs, rhs, locator)) {
      conversionsOrFixes.push_back(fix);
      break;
    }

    if (auto *fix = UseValueTypeOfRawRepresentative::attempt(*this, lhs, rhs,
                                                             locator)) {
      conversionsOrFixes.push_back(fix);
      break;
    }

    // If parameter is a collection but argument is not, let's try
    // to try and match collection element type to the argument to
    // produce better diagnostics e.g.:
    //
    // ```
    // func foo<T>(_: [T]) {}
    // foo(1) // expected '[Int]', got 'Int'
    // ```
    if (isCollectionType(rhs)) {
      std::function<Type(Type)> getArrayOrSetType = [&](Type type) -> Type {
        if (auto eltTy = isArrayType(type))
          return getArrayOrSetType(*eltTy);

        if (auto eltTy = isSetType(type))
          return getArrayOrSetType(*eltTy);

        return type;
      };

      // Let's ignore any optional types associated with element e.g. `[T?]`
      auto rhsEltTy = getArrayOrSetType(rhs)->lookThroughAllOptionalTypes();
      (void)matchTypes(lhs, rhsEltTy, ConstraintKind::Equal, TMF_ApplyingFix,
                       locator);
    }

    // If either type has a hole, consider this fixed.
    if (lhs->hasHole() || rhs->hasHole())
      return true;

    if (repairViaOptionalUnwrap(*this, lhs, rhs, matchKind, conversionsOrFixes,
                                locator))
      break;

    conversionsOrFixes.push_back(
        AllowArgumentMismatch::create(*this, lhs, rhs, loc));
    break;
  }

  case ConstraintLocator::FunctionArgument: {
    auto *argLoc = getConstraintLocator(
        locator.withPathElement(LocatorPathElt::SynthesizedArgument(0)));

    // Let's drop the last element which points to a single argument
    // and see if this is a contextual mismatch.
    path.pop_back();
    if (path.empty() ||
        !(path.back().getKind() == ConstraintLocator::ApplyArgToParam ||
          path.back().getKind() == ConstraintLocator::ContextualType))
      return false;

    auto arg = llvm::find_if(getTypeVariables(),
                             [&argLoc](const TypeVariableType *typeVar) {
                               return typeVar->getImpl().getLocator() == argLoc;
                             });

    // What we have here is a form or tuple splat with no arguments
    // demonstrated by following example:
    //
    // func foo<T: P>(_: T, _: (T.Element) -> Int) {}
    // foo { 42 }
    //
    // In cases like this `T.Element` might be resolved to `Void`
    // which means that we have to try a single empty tuple argument
    // as a narrow exception to SE-0110, see `matchFunctionTypes`.
    //
    // But if `T.Element` didn't get resolved to `Void` we'd like
    // to diagnose this as a missing argument which can't be ignored.
    if (arg != getTypeVariables().end()) {
      conversionsOrFixes.push_back(
          AddMissingArguments::create(*this, {FunctionType::Param(*arg)},
                                      getConstraintLocator(anchor, path)));
    }

    if ((lhs->is<InOutType>() && !rhs->is<InOutType>()) ||
        (!lhs->is<InOutType>() && rhs->is<InOutType>())) {
      // We want to call matchTypes with the default decomposition options
      // in case there are type variables that we couldn't bind due to the
      // inout attribute mismatch.
      auto result = matchTypes(lhs->getInOutObjectType(),
                               rhs->getInOutObjectType(), matchKind,
                               getDefaultDecompositionOptions(TMF_ApplyingFix),
                               locator);

      if (result.isSuccess()) {
        conversionsOrFixes.push_back(AllowInOutConversion::create(*this, lhs,
            rhs, getConstraintLocator(locator)));
        break;
      }
    }

    break;
  }

  case ConstraintLocator::TypeParameterRequirement:
  case ConstraintLocator::ConditionalRequirement: {
    // If either type has a hole, consider this fixed.
    if (lhs->hasHole() || rhs->hasHole())
      return true;

    // If dependent members are present here it's because
    // base doesn't conform to associated type's protocol.
    if (lhs->hasDependentMember() || rhs->hasDependentMember())
      break;

    // If requirement is something like `T == [Int]` let's let
    // type matcher a chance to match generic parameters before
    // recording a fix, because then we'll know exactly how many
    // generic parameters did not match.
    if (hasConversionOrRestriction(ConversionRestrictionKind::DeepEquality))
      break;

    auto reqElt = elt.castTo<LocatorPathElt::AnyRequirement>();
    auto reqKind = reqElt.getRequirementKind();

    if (hasFixedRequirement(lhs, reqKind, rhs))
      return true;

    if (auto *fix = fixRequirementFailure(*this, lhs, rhs, anchor, path)) {
      recordFixedRequirement(lhs, reqKind, rhs);
      conversionsOrFixes.push_back(fix);
    }
    break;
  }

  case ConstraintLocator::ClosureResult: {
    if (repairViaOptionalUnwrap(*this, lhs, rhs, matchKind, conversionsOrFixes,
                                locator))
      return true;

    // If we could record a generic arguments mismatch instead of this fix,
    // don't record a ContextualMismatch here.
    if (hasConversionOrRestriction(ConversionRestrictionKind::DeepEquality))
      break;

    auto *fix = ContextualMismatch::create(*this, lhs, rhs,
                                           getConstraintLocator(locator));
    conversionsOrFixes.push_back(fix);
    break;
  }

  case ConstraintLocator::ContextualType: {
    // If either type is a hole, consider this fixed
    if (lhs->isHole() || rhs->isHole())
      return true;

    auto purpose = getContextualTypePurpose();
    if (rhs->isVoid() &&
        (purpose == CTP_ReturnStmt || purpose == CTP_ReturnSingleExpr)) {
      conversionsOrFixes.push_back(
          RemoveReturn::create(*this, getConstraintLocator(locator)));
      return true;
    }

    if (repairByInsertingExplicitCall(lhs, rhs))
      break;

    if (repairByAnyToAnyObjectCast(lhs, rhs))
      break;

    if (repairViaBridgingCast(*this, lhs, rhs, conversionsOrFixes, locator))
      break;

    // If both types are key path, the only differences
    // between them are mutability and/or root, value type mismatch.
    if (isKnownKeyPathType(lhs) && isKnownKeyPathType(rhs)) {
      auto *fix = KeyPathContextualMismatch::create(
          *this, lhs, rhs, getConstraintLocator(locator));
      conversionsOrFixes.push_back(fix);
    }

    if (lhs->is<FunctionType>() && !rhs->is<AnyFunctionType>() &&
        isa<ClosureExpr>(anchor)) {
      auto *fix = ContextualMismatch::create(*this, lhs, rhs,
                                             getConstraintLocator(locator));
      conversionsOrFixes.push_back(fix);
    }

    if (purpose == CTP_Initialization && lhs->is<TupleType>() &&
        rhs->is<TupleType>()) {
      auto *fix = AllowTupleTypeMismatch::create(*this, lhs, rhs,
                                                 getConstraintLocator(locator));
      conversionsOrFixes.push_back(fix);
      break;
    }

    // If either side is not yet resolved, it's too early for this fix.
    if (lhs->isTypeVariableOrMember() || rhs->isTypeVariableOrMember())
      break;

    // If contextual type is an existential value, it's handled
    // after conversion restriction is attempted.
    if (rhs->isExistentialType())
      break;

    // TODO(diagnostics): This is a problem related to `inout` mismatch,
    // in argument position, and we got here from CSDiag. Once
    // argument-to-pararameter conversion failures are implemented,
    // this check could be removed.
    if (lhs->is<InOutType>() || rhs->is<InOutType>())
      break;

    if (repairViaOptionalUnwrap(*this, lhs, rhs, matchKind, conversionsOrFixes,
                                locator))
      break;

    // If there is a deep equality, superclass restriction
    // already recorded, let's not add bother ignoring
    // contextual type, because actual fix is going to
    // be perform once restriction is applied.
    if (llvm::any_of(conversionsOrFixes,
                     [](const RestrictionOrFix &entry) -> bool {
                       return entry.IsRestriction &&
                              (entry.getRestriction() ==
                                   ConversionRestrictionKind::Superclass ||
                               entry.getRestriction() ==
                                   ConversionRestrictionKind::DeepEquality);
                     }))
      break;

    conversionsOrFixes.push_back(IgnoreContextualType::create(
        *this, lhs, rhs, getConstraintLocator(locator)));
    break;
  }

  case ConstraintLocator::FunctionResult: {
    auto *loc = getConstraintLocator(anchor, {path.begin(), path.end() - 1});
    // If this is a mismatch between contextual type and (trailing)
    // closure with explicitly specified result type let's record it
    // as contextual type mismatch.
    if (loc->isLastElement<LocatorPathElt::ContextualType>() ||
        loc->isLastElement<LocatorPathElt::ApplyArgToParam>()) {
      auto *argExpr = simplifyLocatorToAnchor(loc);
      if (argExpr && isa<ClosureExpr>(argExpr)) {
        auto *locator =
            getConstraintLocator(argExpr, ConstraintLocator::ClosureResult);

        if (repairViaOptionalUnwrap(*this, lhs, rhs, matchKind,
                                    conversionsOrFixes, locator))
          break;

        conversionsOrFixes.push_back(
            IgnoreContextualType::create(*this, lhs, rhs, locator));
        break;
      }
    }
    LLVM_FALLTHROUGH;
  }

  case ConstraintLocator::Member:
  case ConstraintLocator::DynamicLookupResult: {
    // Most likely this is an attempt to use get-only subscript as mutating,
    // or assign a value of a result of function/member ref e.g. `foo() = 42`
    // or `foo.bar = 42`, or `foo.bar()! = 42`.
    if (repairByTreatingRValueAsLValue(rhs, lhs))
      break;

    // `apply argument` -> `arg/param compare` ->
    // `@autoclosure result` -> `function result`
    if (path.size() > 3) {
      const auto &elt = path[path.size() - 2];
      if (elt.getKind() == ConstraintLocator::AutoclosureResult &&
          repairByInsertingExplicitCall(lhs, rhs))
        return true;
    }
    break;
  }

  case ConstraintLocator::AutoclosureResult: {
    if (repairByInsertingExplicitCall(lhs, rhs))
      return true;

    auto isPointerType = [](Type type) -> bool {
      return bool(
          type->lookThroughAllOptionalTypes()->getAnyPointerElementType());
    };

    // Let's see whether this is an implicit conversion to a pointer type
    // which is invalid in @autoclosure context e.g. from `inout`, Array
    // or String.
    if (!isPointerType(lhs) && isPointerType(rhs)) {
      auto result = matchTypes(
          lhs, rhs, ConstraintKind::ArgumentConversion,
          TypeMatchFlags::TMF_ApplyingFix,
          locator.withPathElement(ConstraintLocator::FunctionArgument));

      if (result.isSuccess())
        conversionsOrFixes.push_back(AllowAutoClosurePointerConversion::create(
            *this, lhs, rhs, getConstraintLocator(locator)));
    }

    // In situations like this:
    //
    // struct S<T> {}
    // func foo(_: @autoclosure () -> S<Int>) {}
    // foo(S<String>())
    //
    // Generic type conversion mismatch is a better fix which is going to
    // point to the generic arguments that did not align properly.
    if (hasConversionOrRestriction(ConversionRestrictionKind::DeepEquality))
      break;

    conversionsOrFixes.push_back(AllowArgumentMismatch::create(
        *this, lhs, rhs, getConstraintLocator(locator)));
    break;
  }

  case ConstraintLocator::TupleElement: {
    if (anchor && (isa<ArrayExpr>(anchor) || isa<DictionaryExpr>(anchor))) {
      // If we could record a generic arguments mismatch instead of this fix,
      // don't record a ContextualMismatch here.
      if (hasConversionOrRestriction(ConversionRestrictionKind::DeepEquality))
        break;

      conversionsOrFixes.push_back(CollectionElementContextualMismatch::create(
          *this, lhs, rhs, getConstraintLocator(locator)));
    }
    if (lhs->is<TupleType>() && rhs->is<TupleType>()) {
      auto *fix = AllowTupleTypeMismatch::create(*this, lhs, rhs,
                                                 getConstraintLocator(locator));
      conversionsOrFixes.push_back(fix);
    }
    break;
  }

  case ConstraintLocator::SequenceElementType: {
    // This is going to be diagnosed as `missing conformance`,
    // so no need to create duplicate fixes.
    if (rhs->isExistentialType())
      break;

    conversionsOrFixes.push_back(CollectionElementContextualMismatch::create(
        *this, lhs, rhs, getConstraintLocator(locator)));
    break;
  }

  case ConstraintLocator::SubscriptMember: {
    if (repairByTreatingRValueAsLValue(lhs, rhs))
      break;

    break;
  }

  case ConstraintLocator::Condition: {
    if (repairViaOptionalUnwrap(*this, lhs, rhs, matchKind, conversionsOrFixes,
                                locator))
      break;

    conversionsOrFixes.push_back(IgnoreContextualType::create(
        *this, lhs, rhs, getConstraintLocator(locator)));
    break;
  }

  // Unresolved member type mismatches are handled when
  // r-value adjustment constraint fails.
  case ConstraintLocator::UnresolvedMember:
    return true;

  case ConstraintLocator::RValueAdjustment: {
    if (!(anchor && isa<UnresolvedMemberExpr>(anchor)))
      break;

    if (repairViaOptionalUnwrap(*this, lhs, rhs, matchKind, conversionsOrFixes,
                                locator))
      break;

    // r-value adjustment is used to connect base type of
    // unresolved member to its output type, if there is
    // a type mismatch here it's contextual e.g.
    // `let x: E = .foo(42)`, where `.foo` is a member of `E`
    // but produces an incorrect type.
    auto *fix = IgnoreContextualType::create(*this, lhs, rhs,
                                             getConstraintLocator(locator));
    conversionsOrFixes.push_back(fix);
    break;
  }

  case ConstraintLocator::ImplicitlyUnwrappedDisjunctionChoice: {
    // If this is an attempt to use readonly IUO as a destination
    // of an assignment e.g.
    //
    // let x: Int! = 0
    // x = 42 <- `x` can be either `Int?` or `Int` but it can't be an l-value.
    if (lhs->is<LValueType>() && !rhs->is<LValueType>()) {
      auto result = matchTypes(lhs->getWithoutSpecifierType(), rhs, matchKind,
                               TMF_ApplyingFix, locator);

      if (result.isSuccess()) {
        conversionsOrFixes.push_back(
            TreatRValueAsLValue::create(*this, getConstraintLocator(locator)));
      }
    }
    break;
  }

  case ConstraintLocator::InstanceType: {
    if (lhs->hasHole() || rhs->hasHole())
      return true;

    break;
  }

  case ConstraintLocator::OptionalPayload: {
    if (repairViaOptionalUnwrap(*this, lhs, rhs, matchKind, conversionsOrFixes,
                                locator))
      return true;

    break;
  }

  default:
    break;
  }

  return !conversionsOrFixes.empty();
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

  // If both sides are dependent members without type variables, it's
  // possible that base type is incorrect e.g. `Foo.Element` where `Foo`
  // is a concrete type substituted for generic generic parameter,
  // so checking equality here would lead to incorrect behavior,
  // let's defer it until later proper check.
  if (!(desugar1->is<DependentMemberType>() &&
        desugar2->is<DependentMemberType>())) {
    // If the types are obviously equivalent, we're done.
    if (desugar1->isEqual(desugar2) && !isa<InOutType>(desugar2)) {
      return getTypeMatchSuccess();
    }
  }

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

  auto *typeVar1 = dyn_cast<TypeVariableType>(desugar1);
  auto *typeVar2 = dyn_cast<TypeVariableType>(desugar2);

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
        if (kind == ConstraintKind::Equal &&
            rep1->getImpl().canBindToLValue()
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
        auto rep2 = getRepresentative(typeVar2);
        type1 = simplifyType(type1, flags);
        if (!isBindable(typeVar2, type1))
          return formUnsolvedResult();

        if (auto *iot = type1->getAs<InOutType>()) {
          if (!rep2->getImpl().canBindToLValue())
            return getTypeMatchFailure(locator);
          assignFixedType(rep2, LValueType::get(iot->getObjectType()));
        } else {
          assignFixedType(rep2, type1);
        }
        return getTypeMatchSuccess();
      } else if (typeVar1 && !typeVar2) {
        // Simplify the right-hand type and perform the "occurs" check.
        auto rep1 = getRepresentative(typeVar1);
        type2 = simplifyType(type2, flags);
        if (!isBindable(rep1, type2))
          return formUnsolvedResult();

        if (auto *lvt = type2->getAs<LValueType>()) {
          if (!rep1->getImpl().canBindToInOut())
            return getTypeMatchFailure(locator);
          assignFixedType(rep1, InOutType::get(lvt->getObjectType()));
        } else {
          assignFixedType(rep1, type2);
        }
        return getTypeMatchSuccess();
      } if (typeVar1 && typeVar2) {
        auto rep1 = getRepresentative(typeVar1);
        auto rep2 = getRepresentative(typeVar2);

        if (!rep1->getImpl().canBindToInOut() ||
            !rep2->getImpl().canBindToLValue()) {
          // Merge the equivalence classes corresponding to these two variables.
          mergeEquivalenceClasses(rep1, rep2);
          return getTypeMatchSuccess();
        }
      }

      return formUnsolvedResult();
    }

    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
    case ConstraintKind::ArgumentConversion:
    case ConstraintKind::OperatorArgumentConversion: {
      if (typeVar1) {
        if (auto *locator = typeVar1->getImpl().getLocator()) {
          // TODO(diagnostics): Only binding here for function types, because
          // doing so for KeyPath types leaves the constraint system in an
          // unexpected state for key path diagnostics should we fail.
          if (locator->isLastElement<LocatorPathElt::KeyPathType>() &&
              type2->is<AnyFunctionType>())
            return matchTypesBindTypeVar(typeVar1, type2, kind, flags, locator,
                                         formUnsolvedResult);
        }
      }

      // If the left-hand side of a 'sequence element' constraint
      // is a dependent member type without any type variables it
      // means that conformance check has been "fixed".
      // Let's record other side of the conversion as a "hole"
      // to give the solver a chance to continue and avoid
      // producing diagnostics for both missing conformance and
      // invalid element type.
      if (shouldAttemptFixes()) {
        if (auto last = locator.last()) {
          if (last->is<LocatorPathElt::SequenceElementType>() &&
              desugar1->is<DependentMemberType>() &&
              !desugar1->hasTypeVariable()) {
            recordPotentialHole(typeVar2);
            return getTypeMatchSuccess();
          }
        }
      }

      return formUnsolvedResult();
    }

    case ConstraintKind::OpaqueUnderlyingType:
    case ConstraintKind::ApplicableFunction:
    case ConstraintKind::DynamicCallableApplicableFunction:
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
    case ConstraintKind::OneWayEqual:
      llvm_unreachable("Not a relational constraint");
    }
  }

  // If one of the types is a member type of a type variable type,
  // there's nothing we can do.
  if (desugar1->isTypeVariableOrMember() ||
      desugar2->isTypeVariableOrMember()) {
    return formUnsolvedResult();
  }

  llvm::SmallVector<RestrictionOrFix, 4> conversionsOrFixes;

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

    case TypeKind::Error:
    case TypeKind::Unresolved:
      return getTypeMatchFailure(locator);

    case TypeKind::GenericTypeParam:
      llvm_unreachable("unmapped dependent type in type checker");

    case TypeKind::TypeVariable:
      llvm_unreachable("type variables should have already been handled by now");

    case TypeKind::DependentMember: {
      // If one of the dependent member types has no type variables,
      // this comparison is effectively illformed, because dependent
      // member couldn't be simplified down to the actual type, and
      // we wouldn't be able to solve this constraint, so let's just fail.
      if (!desugar1->hasTypeVariable() || !desugar2->hasTypeVariable())
        return getTypeMatchFailure(locator);

      // Nothing we can solve yet, since we need to wait until
      // type variables will get resolved.
      return formUnsolvedResult();
    }

    case TypeKind::Module:
    case TypeKind::PrimaryArchetype:
    case TypeKind::OpenedArchetype: {
      if (shouldAttemptFixes()) {
        auto last = locator.last();
        // If this happens as part of the argument-to-parameter
        // conversion or type requirement, there is a tailored
        // fix/diagnostic.
        if (last && (last->is<LocatorPathElt::ApplyArgToParam>() ||
                     last->is<LocatorPathElt::AnyRequirement>()))
          break;
      }
      // If two module types or archetypes were not already equal, there's
      // nothing more we can do.
      return getTypeMatchFailure(locator);
    }

    case TypeKind::Tuple: {
      auto result = matchTupleTypes(cast<TupleType>(desugar1),
                                    cast<TupleType>(desugar2),
                                    kind, subflags, locator);
      if (result != SolutionKind::Error)
        return result;

      // FIXME: All cases in this switch should go down to the fix logic
      // to give repairFailures() a chance to run, but this breaks stuff
      // right now.
      break;
    }

    case TypeKind::Enum:
    case TypeKind::Struct:
    case TypeKind::Class: {
      auto nominal1 = cast<NominalType>(desugar1);
      auto nominal2 = cast<NominalType>(desugar2);
      if (nominal1->getDecl() == nominal2->getDecl())
        conversionsOrFixes.push_back(ConversionRestrictionKind::DeepEquality);

      // Check for CF <-> ObjectiveC bridging.
      if (isa<ClassType>(desugar1) &&
          kind >= ConstraintKind::Subtype) {
        auto class1 = cast<ClassDecl>(nominal1->getDecl());
        auto class2 = cast<ClassDecl>(nominal2->getDecl());

        // CF -> Objective-C via toll-free bridging.
        if (class1->getForeignClassKind() == ClassDecl::ForeignKind::CFType &&
            class2->getForeignClassKind() != ClassDecl::ForeignKind::CFType &&
            class1->getAttrs().hasAttribute<ObjCBridgedAttr>()) {
          conversionsOrFixes.push_back(
            ConversionRestrictionKind::CFTollFreeBridgeToObjC);
        }

        // Objective-C -> CF via toll-free bridging.
        if (class2->getForeignClassKind() == ClassDecl::ForeignKind::CFType &&
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

      // A.Type < B.Type if A < B and both A and B are classes.
      // P.Type < Q.Type if P < Q, both P and Q are protocols, and P.Type
      // and Q.Type are both existential metatypes
      auto subKind = std::min(kind, ConstraintKind::Subtype);
      // If instance types can't have a subtype relationship
      // it means that such types can be simply equated.
      auto instanceType1 = meta1->getInstanceType();
      auto instanceType2 = meta2->getInstanceType();
      if (isa<MetatypeType>(meta1) &&
          !(instanceType1->mayHaveSuperclass() &&
            instanceType2->getClassOrBoundGenericClass())) {
        subKind = ConstraintKind::Bind;
      }

      return matchTypes(
          instanceType1, instanceType2, subKind, subflags,
          locator.withPathElement(ConstraintLocator::InstanceType));
    }

    case TypeKind::Function: {
      auto func1 = cast<FunctionType>(desugar1);
      auto func2 = cast<FunctionType>(desugar2);

      auto result = matchFunctionTypes(func1, func2, kind, flags, locator);

      // If this is a type mismatch in assignment, we don't really care
      // (yet) was it argument or result type mismatch, let's produce a
      // diagnostic which means both function types.
      if (shouldAttemptFixes() && result.isFailure()) {
        auto *anchor = locator.getAnchor();
        if (anchor && isa<AssignExpr>(anchor))
          break;
      }

      return result;
    }

    case TypeKind::GenericFunction:
      llvm_unreachable("Polymorphic function type should have been opened");

    case TypeKind::ProtocolComposition:
      switch (kind) {
      case ConstraintKind::Equal:
      case ConstraintKind::Bind:
      case ConstraintKind::BindParam:
        // If we are matching types for equality, we might still have
        // type variables inside the protocol composition's superclass
        // constraint.
        conversionsOrFixes.push_back(ConversionRestrictionKind::DeepEquality);
        break;

      default:
        // Subtype constraints where the RHS is an existential type are
        // handled below.
        break;
      }

      break;

    case TypeKind::LValue:
      if (kind == ConstraintKind::BindParam)
        return getTypeMatchFailure(locator);
      return matchTypes(cast<LValueType>(desugar1)->getObjectType(),
                        cast<LValueType>(desugar2)->getObjectType(),
                        ConstraintKind::Bind, subflags,
                        locator.withPathElement(
                          ConstraintLocator::LValueConversion));
    
    case TypeKind::InOut:
      if (kind == ConstraintKind::BindParam)
        return getTypeMatchFailure(locator);
      
      if (kind == ConstraintKind::OperatorArgumentConversion) {
        conversionsOrFixes.push_back(
            RemoveAddressOf::create(*this, type1, type2,
                                    getConstraintLocator(locator)));
        break;
      }

      return matchTypes(cast<InOutType>(desugar1)->getObjectType(),
                        cast<InOutType>(desugar2)->getObjectType(),
                        ConstraintKind::Bind, subflags,
                  locator.withPathElement(ConstraintLocator::LValueConversion));

    case TypeKind::UnboundGeneric:
      llvm_unreachable("Unbound generic type should have been opened");

    case TypeKind::BoundGenericClass:
    case TypeKind::BoundGenericEnum:
    case TypeKind::BoundGenericStruct: {
      auto bound1 = cast<BoundGenericType>(desugar1);
      auto bound2 = cast<BoundGenericType>(desugar2);
      
      if (bound1->getDecl() == bound2->getDecl())
        conversionsOrFixes.push_back(ConversionRestrictionKind::DeepEquality);
      break;
    }

    // Opaque archetypes are globally bound, so we can match them for deep
    // equality.
    case TypeKind::OpaqueTypeArchetype: {
      auto opaque1 = cast<OpaqueTypeArchetypeType>(desugar1);
      auto opaque2 = cast<OpaqueTypeArchetypeType>(desugar2);
      
      if (opaque1->getDecl() == opaque2->getDecl()) {
        conversionsOrFixes.push_back(ConversionRestrictionKind::DeepEquality);
      }
      break;
    }
    
    // Same for nested archetypes rooted in opaque types.
    case TypeKind::NestedArchetype: {
      auto nested1 = cast<NestedArchetypeType>(desugar1);
      auto nested2 = cast<NestedArchetypeType>(desugar2);
      
      auto rootOpaque1 = dyn_cast<OpaqueTypeArchetypeType>(nested1->getRoot());
      auto rootOpaque2 = dyn_cast<OpaqueTypeArchetypeType>(nested2->getRoot());
      if (rootOpaque1 && rootOpaque2) {
        auto interfaceTy1 = nested1->getInterfaceType()
          ->getCanonicalType(rootOpaque1->getGenericEnvironment()
                                        ->getGenericSignature());
        auto interfaceTy2 = nested2->getInterfaceType()
          ->getCanonicalType(rootOpaque2->getGenericEnvironment()
                                        ->getGenericSignature());
        if (interfaceTy1 == interfaceTy2
            && rootOpaque1->getDecl() == rootOpaque2->getDecl()) {
          conversionsOrFixes.push_back(ConversionRestrictionKind::DeepEquality);
          break;
        }
      }

      // Before failing, let's give repair a chance to run in diagnostic mode.
      if (shouldAttemptFixes())
        break;

      // If the archetypes aren't rooted in an opaque type, or are rooted in
      // completely different decls, then there's nothing else we can do.
      return getTypeMatchFailure(locator);
    }
    }
  }

  if (kind >= ConstraintKind::Conversion) {
    // An lvalue of type T1 can be converted to a value of type T2 so long as
    // T1 is convertible to T2 (by loading the value).  Note that we cannot get
    // a value of inout type as an lvalue though.
    if (type1->is<LValueType>() && !type2->is<InOutType>()) {
      auto result = matchTypes(type1->getWithoutSpecifierType(), type2, kind,
                               subflags, locator);
      if (result.isSuccess() || !shouldAttemptFixes())
        return result;
    }
  }

  if (kind >= ConstraintKind::Subtype) {
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

      // Penalize conversions to Any.
      if (kind >= ConstraintKind::Conversion && type2->isAny())
        increaseScore(ScoreKind::SK_EmptyExistentialConversion);

      conversionsOrFixes.push_back(ConversionRestrictionKind::Existential);
    }

    // T -> AnyHashable.
    if (isAnyHashableType(desugar2)) {
      // Don't allow this in operator contexts or we'll end up allowing
      // 'T() == U()' for unrelated T and U that just happen to be Hashable.
      // We can remove this special case when we implement operator hiding.
      if (!type1->is<LValueType>() &&
          kind != ConstraintKind::OperatorArgumentConversion) {
        conversionsOrFixes.push_back(
                              ConversionRestrictionKind::HashableToAnyHashable);
      }
    }

    // Metatype to object conversion.
    //
    // Class and protocol metatypes are interoperable with certain Objective-C
    // runtime classes, but only when ObjC interop is enabled.

    if (getASTContext().LangOpts.EnableObjCInterop) {
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
        conversionsOrFixes.push_back(ConversionRestrictionKind::ArrayUpcast);
      // Dictionary -> Dictionary.
      } else if (isDictionaryType(desugar1) && isDictionaryType(desugar2)) {
        conversionsOrFixes.push_back(
          ConversionRestrictionKind::DictionaryUpcast);
      // Set -> Set.
      } else if (isSetType(desugar1) && isSetType(desugar2)) {
        conversionsOrFixes.push_back(
          ConversionRestrictionKind::SetUpcast);
      }
    }
  }
  
  if (kind == ConstraintKind::BindToPointerType) {
    if (desugar2->isEqual(getASTContext().TheEmptyTupleType))
      return getTypeMatchSuccess();
  }

  if (kind >= ConstraintKind::Conversion) {
    // It is never legal to form an autoclosure that results in these
    // implicit conversions to pointer types.
    bool isAutoClosureArgument = locator.isForAutoclosureResult();

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
          if (auto inoutType1 = dyn_cast<InOutType>(desugar1)) {
            if (!isAutoClosureArgument) {
              auto inoutBaseType = inoutType1->getInOutObjectType();

              auto baseIsArray = isArrayType(
                  getFixedTypeRecursive(inoutBaseType, /*wantRValue=*/true));

              // FIXME: If the base is still a type variable, we can't tell
              // what to do here. Might have to try \c ArrayToPointer and make
              // it more robust.
              if (baseIsArray)
                conversionsOrFixes.push_back(
                    ConversionRestrictionKind::ArrayToPointer);

              // Only try an inout-to-pointer conversion if we know it's not
              // an array being converted to a raw pointer type. Such
              // conversions can only use array-to-pointer.
              if (!baseIsArray || !isRawPointerKind(pointerKind))
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
                auto &ctx = getASTContext();
                if (type1->isEqual(TypeChecker::getStringType(ctx))) {
                  auto baseTy = getFixedTypeRecursive(pointeeTy, false);

                  if (baseTy->isTypeVariableOrMember() ||
                      isStringCompatiblePointerBaseType(ctx, baseTy))
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

  if (kind >= ConstraintKind::OperatorArgumentConversion) {
    // If the RHS is an inout type, the LHS must be an @lvalue type.
    if (auto *lvt = type1->getAs<LValueType>()) {
      if (auto *iot = type2->getAs<InOutType>()) {
        return matchTypes(lvt->getObjectType(), iot->getObjectType(),
                          ConstraintKind::Bind, subflags,
                          locator.withPathElement(
                                  ConstraintLocator::LValueConversion));
      }
    }
  }

  // A value of type T! can be converted to type U if T is convertible
  // to U by force-unwrapping the source value.
  // A value of type T, T?, or T! can be converted to type U? or U! if
  // T is convertible to U.
  if (!type1->is<LValueType>() && kind >= ConstraintKind::Subtype) {
    enumerateOptionalConversionRestrictions(
        type1, type2, kind, locator,
        [&](ConversionRestrictionKind restriction) {
      conversionsOrFixes.push_back(restriction);
    });
  }

  // Allow '() -> T' to '() -> ()' and '() -> Never' to '() -> T' for closure
  // literals and expressions representing an implicit return type of the single
  // expression functions.
  if (auto elt = locator.last()) {
    if (elt->isClosureResult() || elt->isResultOfSingleExprFunction()) {
      if (kind >= ConstraintKind::Subtype &&
          (type1->isUninhabited() || type2->isVoid())) {
        increaseScore(SK_FunctionConversion);
        return getTypeMatchSuccess();
      }
    }
  }

  if (kind == ConstraintKind::BindParam) {
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
  if (shouldAttemptFixes() && !flags.contains(TMF_ApplyingFix)) {
    if (repairFailures(type1, type2, kind, conversionsOrFixes, locator)) {
      if (conversionsOrFixes.empty())
        return getTypeMatchSuccess();
    }
  }

  if (conversionsOrFixes.empty())
    return getTypeMatchFailure(locator);

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
          constraintKind = ConstraintKind::Bind;

        constraints.push_back(
          Constraint::createRestricted(*this, constraintKind, *restriction,
                                       type1, type2, fixedLocator));

        if (constraints.back()->getKind() == ConstraintKind::Bind)
          constraints.back()->setFavored();

        continue;
      }

      auto fix = *potential.getFix();
      constraints.push_back(
        Constraint::createFixed(*this, constraintKind, fix, type1, type2,
                                fixedLocator));
    }

    // Sort favored constraints first.
    std::sort(constraints.begin(), constraints.end(),
              [&](Constraint *lhs, Constraint *rhs) -> bool {
                if (lhs->isFavored() == rhs->isFavored())
                  return false;

                return lhs->isFavored();
              });

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
    // If this is an attempt to construct `Void` with arguments,
    // let's diagnose it.
    if (shouldAttemptFixes()) {
      if (valueType->isVoid() && fnType->getNumParams() > 0) {
        auto contextualType = FunctionType::get({}, fnType->getResult());
        if (fixExtraneousArguments(
                *this, contextualType, fnType->getParams(),
                fnType->getNumParams(),
                getConstraintLocator(locator,
                                     ConstraintLocator::FunctionArgument)))
          return SolutionKind::Error;

        fnType = contextualType;
      }
    }

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
  case TypeKind::PrimaryArchetype:
  case TypeKind::OpenedArchetype:
  case TypeKind::NestedArchetype:
  case TypeKind::OpaqueTypeArchetype:
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
  case TypeKind::Module: {
    // If solver is in the diagnostic mode and this is an invalid base,
    // let's give solver a chance to repair it to produce a good diagnostic.
    if (shouldAttemptFixes())
      break;

    return SolutionKind::Error;
  }
  }

  auto fnLocator = getConstraintLocator(locator,
                                        ConstraintLocator::ApplyFunction);
  auto memberType = createTypeVariable(fnLocator,
                                       TVO_CanBindToNoEscape);

  // The constructor will have function type T -> T2, for a fresh type
  // variable T. T2 is the result type provided via the construction
  // constraint itself.
  addValueMemberConstraint(MetatypeType::get(valueType, getASTContext()),
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
         TVO_CanBindToInOut |
         TVO_CanBindToNoEscape |
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
  auto *typeVar = type->getAs<TypeVariableType>();

  // Dig out the fixed type to which this type refers.
  type = getFixedTypeRecursive(type, flags, /*wantRValue=*/true);
  if (shouldAttemptFixes() && type->isHole()) {
    // If the type associated with this conformance check is a "hole" in the
    // constraint system, let's consider this check a success without recording
    // a fix, because it's just a consequence of the other failure, e.g.
    //
    // func foo<T: BinaryInteger>(_: T) {}
    // foo(Foo.bar) <- if `Foo` doesn't have `bar` there is
    //                 no reason to complain about missing conformance.
    increaseScore(SK_Fix);
    return SolutionKind::Solved;
  }

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
        addConstraint(req,
                      locator.withPathElement(
                          LocatorPathElt::ConditionalRequirement(
                              index++, req.getKind())));
      }
    }

    return SolutionKind::Solved;
  };

  // For purposes of argument type matching, existential types don't need to
  // conform -- they only need to contain the protocol, so check that
  // separately.
  switch (kind) {
  case ConstraintKind::SelfObjectOfProtocol: {
    auto conformance = TypeChecker::containsProtocol(
        type, protocol, DC,
        (ConformanceCheckFlags::InExpression |
         ConformanceCheckFlags::SkipConditionalRequirements));
    if (conformance) {
      return recordConformance(conformance);
    }
  } break;
  case ConstraintKind::ConformsTo:
  case ConstraintKind::LiteralConformsTo: {
    // Check whether this type conforms to the protocol.
    auto conformance = TypeChecker::conformsToProtocol(
        type, protocol, DC,
        (ConformanceCheckFlags::InExpression |
         ConformanceCheckFlags::SkipConditionalRequirements));
    if (conformance) {
      return recordConformance(conformance);
    }
  } break;

  default:
    llvm_unreachable("bad constraint kind");
  }
  
  if (!shouldAttemptFixes())
    return SolutionKind::Error;

  auto protocolTy = protocol->getDeclaredType();

  // If this conformance has been fixed already, let's just consider this done.
  if (hasFixedRequirement(type, RequirementKind::Conformance, protocolTy))
    return SolutionKind::Solved;

  // If this is a generic requirement let's try to record that
  // conformance is missing and consider this a success, which
  // makes it much easier to diagnose problems like that.
  {
    SmallVector<LocatorPathElt, 4> path;
    auto *anchor = locator.getLocatorParts(path);

    // If this is a `nil` literal, it would be a contextual failure.
    if (auto *Nil = dyn_cast_or_null<NilLiteralExpr>(anchor)) {
      auto *fixLocator = getConstraintLocator(
          getContextualType(Nil)
              ? locator.withPathElement(LocatorPathElt::ContextualType())
              : locator);

      // Here the roles are reversed - `nil` is something we are trying to
      // convert to `type` by making sure that it conforms to a specific
      // protocol.
      auto *fix =
          ContextualMismatch::create(*this, protocolTy, type, fixLocator);

      return recordFix(fix) ? SolutionKind::Error : SolutionKind::Solved;
    }

    // If there is a missing conformance between source and destination
    // of the assignment, let's ignore current the types and instead use
    // source/destination types directly to make it possible to diagnose
    // protocol compositions.
    if (anchor && isa<AssignExpr>(anchor)) {
      auto *assignment = cast<AssignExpr>(anchor);

      // If the locator's last element points to the function result,
      // let's check whether there is a problem with function argument
      // as well, and if so, avoid producing a fix here, because
      // contextual mismatch mentions the source/destination
      // types of the assignment.
      if (auto last = locator.last()) {
        if (last->is<LocatorPathElt::FunctionResult>() &&
            hasFixFor(getConstraintLocator(anchor,
                                           LocatorPathElt::FunctionArgument())))
          return SolutionKind::Solved;
      }

      auto srcType = getType(assignment->getSrc());
      auto dstType = getType(assignment->getDest());

      auto *fix = IgnoreAssignmentDestinationType::create(
          *this, srcType, dstType, getConstraintLocator(locator));
      return recordFix(fix) ? SolutionKind::Error : SolutionKind::Solved;
    }

    if (path.empty())
      return SolutionKind::Error;

    // If this is a conformance failure related to a contextual type
    // let's record it as a "contextual mismatch" because diagnostic
    // is going to be dependent on other contextual information.
    if (path.back().is<LocatorPathElt::ContextualType>()) {
      auto *fix = ContextualMismatch::create(*this, type, protocolTy,
                                             getConstraintLocator(locator));
      return recordFix(fix) ? SolutionKind::Error : SolutionKind::Solved;
    }

    if (path.back().is<LocatorPathElt::AnyRequirement>()) {
      // If this is a requirement associated with `Self` which is bound
      // to `Any`, let's consider this "too incorrect" to continue.
      //
      // This helps us to filter out cases like operator overloads where
      // `Self` type comes from e.g. default for collection element -
      // `[1, "hello"].map { $0 + 1 }`. Main problem here is that
      // collection type couldn't be determined without unification to
      // `Any` and `+` failing for all numeric overloads is just a consequence.
      if (typeVar && type->isAny()) {
        auto *GP = typeVar->getImpl().getGenericParameter();
        if (auto *GPD = GP->getDecl()) {
          auto *DC = GPD->getDeclContext();
          if (DC->isTypeContext() && DC->getSelfInterfaceType()->isEqual(GP))
            return SolutionKind::Error;
        }
      }

      if (auto *fix =
              fixRequirementFailure(*this, type, protocolTy, anchor, path)) {
        auto impact = assessRequirementFailureImpact(*this, typeVar, locator);
        if (!recordFix(fix, impact)) {
          // Record this conformance requirement as "fixed".
          recordFixedRequirement(type, RequirementKind::Conformance,
                                 protocolTy);
          return SolutionKind::Solved;
        }
      }
    }

    // If this is an implicit Hashable conformance check generated for each
    // index argument of the keypath subscript component, we could just treat
    // it as though it conforms.
    auto *loc = getConstraintLocator(locator);
    if (loc->isResultOfKeyPathDynamicMemberLookup() ||
        loc->isKeyPathSubscriptComponent()) {
      if (protocol ==
          getASTContext().getProtocol(KnownProtocolKind::Hashable)) {
        auto *fix =
            TreatKeyPathSubscriptIndexAsHashable::create(*this, type, loc);
        if (!recordFix(fix))
          return SolutionKind::Solved;
      }
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
  

  Type objectTy = optTy->getOptionalObjectType();
  // If the base type is not optional, let's attempt a fix (if possible)
  // and assume that `!` is just not there.
  if (!objectTy) {
    // Let's see if we can apply a specific fix here.
    if (shouldAttemptFixes()) {
      if (optTy->isHole())
        return SolutionKind::Solved;

      auto *fix =
          RemoveUnwrap::create(*this, optTy, getConstraintLocator(locator));

      if (recordFix(fix))
        return SolutionKind::Error;

      // If the fix was successful let's record
      // "fixed" object type and continue.
      objectTy = optTy;
    } else {
      // If fixes are not allowed, no choice but to fail.
      return SolutionKind::Error;
    }
  }
  
  // The object type is an lvalue if the optional was.
  if (optLValueTy->is<LValueType>())
    objectTy = LValueType::get(objectTy);

  // Equate it to the other type in the constraint.
  addConstraint(ConstraintKind::Bind, objectTy, second, locator);
  return SolutionKind::Solved;
}

/// Attempt to simplify a function input or result constraint.
ConstraintSystem::SolutionKind
ConstraintSystem::simplifyFunctionComponentConstraint(
                                        ConstraintKind kind,
                                        Type first, Type second,
                                        TypeMatchOptions flags,
                                        ConstraintLocatorBuilder locator) {
  auto simplified = simplifyType(first);
  auto simplifiedCopy = simplified;

  unsigned unwrapCount = 0;
  if (shouldAttemptFixes()) {
    while (auto objectTy = simplified->getOptionalObjectType()) {
      simplified = objectTy;

      // Track how many times we do this so that we can record a fix for each.
      ++unwrapCount;
    }

    if (simplified->isHole()) {
      if (auto *typeVar = second->getAs<TypeVariableType>())
        recordPotentialHole(typeVar);
      return SolutionKind::Solved;
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
    auto *fix = ForceOptional::create(*this, simplifiedCopy, second,
                                      getConstraintLocator(locator));
    if (recordFix(fix, /*impact=*/unwrapCount))
      return SolutionKind::Error;
  }

  return SolutionKind::Solved;
}

/// Return true if the specified type or a super-class/super-protocol has the
/// @dynamicMemberLookup attribute on it.  This implementation is not
/// particularly fast in the face of deep class hierarchies or lots of protocol
/// conformances, but this is fine because it doesn't get invoked in the normal
/// name lookup path (only when lookup is about to fail).
bool swift::hasDynamicMemberLookupAttribute(Type type,
                    llvm::DenseMap<CanType, bool> &DynamicMemberLookupCache) {
  auto canType = type->getCanonicalType();
  auto it = DynamicMemberLookupCache.find(canType);
  if (it != DynamicMemberLookupCache.end()) return it->second;

  // Calculate @dynamicMemberLookup attribute for composite types with multiple
  // components (protocol composition types and archetypes).
  auto calculateForComponentTypes =
      [&](ArrayRef<Type> componentTypes) -> bool {
    for (auto componentType : componentTypes)
      if (hasDynamicMemberLookupAttribute(componentType,
                                          DynamicMemberLookupCache))
        return true;
    return false;
  };

  auto calculate = [&]() -> bool {
    // If this is an archetype type, check if any types it conforms to
    // (superclass or protocols) have the attribute.
    if (auto archetype = dyn_cast<ArchetypeType>(canType)) {
      SmallVector<Type, 2> componentTypes;
      for (auto protocolDecl : archetype->getConformsTo())
        componentTypes.push_back(protocolDecl->getDeclaredType());
      if (auto superclass = archetype->getSuperclass())
        componentTypes.push_back(superclass);
      return calculateForComponentTypes(componentTypes);
    }

    // If this is a protocol composition, check if any of its members have the
    // attribute.
    if (auto protocolComp = dyn_cast<ProtocolCompositionType>(canType))
      return calculateForComponentTypes(protocolComp->getMembers());

    // Otherwise, this must be a nominal type.
    // Dynamic member lookup doesn't work for tuples, etc.
    auto nominal = canType->getAnyNominal();
    if (!nominal) return false;

    // If this type conforms to a protocol with the attribute, then return true.
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
  // Cache the result if the type does not contain type variables.
  if (!type->hasTypeVariable())
    DynamicMemberLookupCache[canType] = result;
  return result;
}

static bool isForKeyPathSubscript(ConstraintSystem &cs,
                                  ConstraintLocator *locator) {
  if (!locator || !locator->getAnchor())
    return false;

  if (auto *SE = dyn_cast<SubscriptExpr>(locator->getAnchor())) {
    auto *indexExpr = dyn_cast<TupleExpr>(SE->getIndex());
    return indexExpr && indexExpr->getNumElements() == 1 &&
           indexExpr->getElementName(0) == cs.getASTContext().Id_keyPath;
  }
  return false;
}

/// Determine whether all of the given candidate overloads
/// found through conditional conformances of a given base type.
/// This is useful to figure out whether it makes sense to
/// perform dynamic member lookup or not.
static bool
allFromConditionalConformances(DeclContext *DC, Type baseTy,
                               ArrayRef<OverloadChoice> candidates) {
  auto *NTD = baseTy->getAnyNominal();
  if (!NTD)
    return false;

  return llvm::all_of(candidates, [&](const OverloadChoice &choice) {
    auto *decl = choice.getDeclOrNull();
    if (!decl)
      return false;

    auto *candidateDC = decl->getDeclContext();

    if (auto *extension = dyn_cast<ExtensionDecl>(candidateDC)) {
      if (extension->isConstrainedExtension())
        return true;
    }

    if (auto *protocol = candidateDC->getSelfProtocolDecl()) {
      SmallVector<ProtocolConformance *, 4> conformances;
      if (!NTD->lookupConformance(DC->getParentModule(), protocol,
                                  conformances))
        return false;

      // This is opportunistic, there should be a way to narrow the
      // list down to a particular declaration member comes from.
      return llvm::any_of(
          conformances, [](const ProtocolConformance *conformance) {
            return !conformance->getConditionalRequirements().empty();
          });
    }

    return false;
  });
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

  if (isForKeyPathSubscript(*this, memberLocator)) {
    if (baseTy->isAnyObject()) {
      result.addUnviable(
          OverloadChoice(baseTy, OverloadChoiceKind::KeyPathApplication),
          MemberLookupResult::UR_KeyPathWithAnyObjectRootType);
    } else {
      result.ViableCandidates.push_back(
          OverloadChoice(baseTy, OverloadChoiceKind::KeyPathApplication));
    }
  }

  // If the base type is a tuple type, look for the named or indexed member
  // of the tuple.
  auto &ctx = getASTContext();
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
  if (baseObjTy->isAnyObject() && memberName.isSimpleName()) {
    // If we're referencing AnyObject and we have argument labels, put
    // the argument labels into the name: we don't want to look for
    // anything else, because the cost of the general search is so
    // high.
    if (auto info = getArgumentInfo(memberLocator)) {
      memberName = DeclName(ctx, memberName.getBaseName(),
                            info->Labels);
    }
  }

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
  if (baseObjTy->getAnyNominal() == ctx.getStringDecl()) {
    if (Type classType = ctx.getBridgedToObjC(DC, instanceTy)) {
      bridgedType = classType;
    }
  }

  // Local function that adds the given declaration if it is a
  // reasonable choice.
  auto addChoice = [&](OverloadChoice candidate) {
    auto decl = candidate.getDecl();

    // In a pattern binding initializer, immediately reject all of its bound
    // variables. These would otherwise allow circular references.
    if (auto *PBI = dyn_cast<PatternBindingInitializer>(DC)) {
      if (auto *VD = dyn_cast<VarDecl>(decl)) {
        if (PBI->getBinding() == VD->getParentPatternBinding()) {
          // If this is a recursive reference to an instance variable,
          // try to see if we can give a good diagnostic by adding it as
          // an unviable candidate.
          if (!VD->isStatic()) {
            result.addUnviable(candidate,
                               MemberLookupResult::UR_InstanceMemberOnType);
          }
          return;
        }
      }
    }

    // If the result is invalid, skip it.
    if (decl->isInvalid()) {
      result.markErrorAlreadyDiagnosed();
      return;
    }

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
      if (baseObjTy->is<AnyMetatypeType>()) {
        // `AnyObject` has special semantics, so let's just let it be.
        // Otherwise adjust base type and reference kind to make it
        // look as if lookup was done on the instance, that helps
        // with diagnostics.
        auto choice = instanceTy->isAnyObject()
                          ? candidate
                          : OverloadChoice(instanceTy, decl,
                                           FunctionRefKind::SingleApply);
        // If this is an instance member referenced from metatype
        // let's add unviable result to the set because it could be
        // either curried reference or an  invalid call.
        //
        // New candidate shouldn't affect performance because such
        // choice would only be attempted when solver is in diagnostic mode.
        result.addUnviable(choice, MemberLookupResult::UR_InstanceMemberOnType);

        bool invalidMethodRef = isa<FuncDecl>(decl) && !hasInstanceMethods;
        bool invalidMemberRef = !isa<FuncDecl>(decl) && !hasInstanceMembers;
        // If this is definitely an invalid way to reference a method or member
        // on the metatype, let's stop here.
        if (invalidMethodRef || invalidMemberRef)
          return;
      }

    // If the underlying type of a typealias is fully concrete, it is legal
    // to access the type with a protocol metatype base.
    } else if (instanceTy->isExistentialType() &&
               isa<TypeAliasDecl>(decl) &&
               !cast<TypeAliasDecl>(decl)
                  ->getUnderlyingType()->getCanonicalType()
                    ->hasTypeParameter()) {

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

    // Check whether this is overload choice found via keypath
    // based dynamic member lookup. Since it's unknown upfront
    // what kind of declaration lookup is going to find, let's
    // double check here that given keypath is appropriate for it.
    if (memberLocator) {
      using KPDynamicMemberElt = LocatorPathElt::KeyPathDynamicMember;
      if (auto kpElt = memberLocator->getLastElementAs<KPDynamicMemberElt>()) {
        auto *keyPath = kpElt->getKeyPathDecl();
        if (auto *storage = dyn_cast<AbstractStorageDecl>(decl)) {
          // If this is an attempt to access read-only member via
          // writable key path, let's fail this choice early.
          auto &ctx = getASTContext();
          if (isReadOnlyKeyPathComponent(storage) &&
              (keyPath == ctx.getWritableKeyPathDecl() ||
               keyPath == ctx.getReferenceWritableKeyPathDecl())) {
            result.addUnviable(
                candidate,
                MemberLookupResult::UR_WritableKeyPathOnReadOnlyMember);
            return;
          }

          // A nonmutating setter indicates a reference-writable base,
          // on the other hand if setter is mutating there is no point
          // of attempting `ReferenceWritableKeyPath` overload.
          if (storage->isSetterMutating() &&
              keyPath == ctx.getReferenceWritableKeyPathDecl()) {
            result.addUnviable(candidate,
                               MemberLookupResult::
                                   UR_ReferenceWritableKeyPathOnMutatingMember);
            return;
          }
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

    // While looking for subscript choices it's possible to find
    // `subscript(dynamicMember: {Writable}KeyPath)` on types
    // marked as `@dynamicMemberLookup`, let's mark this candidate
    // as representing "dynamic lookup" unless it's a direct call
    // to such subscript (in that case label is expected to match).
    if (auto *subscript = dyn_cast<SubscriptDecl>(cand)) {
      if (memberLocator &&
          ::hasDynamicMemberLookupAttribute(instanceTy,
                                            DynamicMemberLookupCache) &&
          isValidKeyPathDynamicMemberLookup(subscript)) {
        auto info = getArgumentInfo(memberLocator);

        if (!(info && info->Labels.size() == 1 &&
              info->Labels[0] == getASTContext().Id_dynamicMember)) {
          return OverloadChoice::getDynamicMemberLookup(
              baseTy, subscript, ctx.getIdentifier("subscript"),
              /*isKeyPathBased=*/true);
        }
      }
    }

    return OverloadChoice(baseTy, cand, functionRefKind);
  };
  
  // Add all results from this lookup.
  for (auto result : lookup)
    addChoice(getOverloadChoice(result.getValueDecl(),
                                /*isBridged=*/false,
                                /*isUnwrappedOptional=*/false));

  // Backward compatibility hack. In Swift 4, `init` and init were
  // the same name, so you could write "foo.init" to look up a
  // method or property named `init`.
  if (!ctx.isSwiftVersionAtLeast(5) &&
      memberName.getBaseName() == DeclBaseName::createConstructor() &&
      !isImplicitInit) {
    auto &compatLookup = lookupMember(instanceTy,
                                      ctx.getIdentifier("init"));
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

  // If we're about to fail lookup because there are no viable candidates
  // or if all of the candidates come from conditional conformances (which
  // might not be applicable), and we are looking for members in a type with
  // the @dynamicMemberLookup attribute, then we resolve a reference to a
  // `subscript(dynamicMember:)` method and pass the member name as a string
  // parameter.
  if (constraintKind == ConstraintKind::ValueMember &&
      memberName.isSimpleName() && !memberName.isSpecial() &&
      ::hasDynamicMemberLookupAttribute(instanceTy, DynamicMemberLookupCache)) {
    const auto &candidates = result.ViableCandidates;

    if (candidates.empty() ||
        allFromConditionalConformances(DC, instanceTy, candidates)) {
      auto &ctx = getASTContext();

      // Recursively look up `subscript(dynamicMember:)` methods in this type.
      auto subscriptName =
          DeclName(ctx, DeclBaseName::createSubscript(), ctx.Id_dynamicMember);
      auto subscripts = performMemberLookup(
          constraintKind, subscriptName, baseTy, functionRefKind, memberLocator,
          includeInaccessibleMembers);

      // Reflect the candidates found as `DynamicMemberLookup` results.
      auto name = memberName.getBaseIdentifier();
      for (const auto &candidate : subscripts.ViableCandidates) {
        auto *SD = cast<SubscriptDecl>(candidate.getDecl());
        bool isKeyPathBased = isValidKeyPathDynamicMemberLookup(SD);

        if (isValidStringDynamicMemberLookup(SD, DC) || isKeyPathBased)
          result.addViable(OverloadChoice::getDynamicMemberLookup(
              baseTy, SD, name, isKeyPathBased));
      }

      for (auto index : indices(subscripts.UnviableCandidates)) {
        auto *SD =
            cast<SubscriptDecl>(subscripts.UnviableCandidates[index].getDecl());
        auto choice = OverloadChoice::getDynamicMemberLookup(
            baseTy, SD, name, isValidKeyPathDynamicMemberLookup(SD));
        result.addUnviable(choice, subscripts.UnviableReasons[index]);
      }
    }
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

    auto lookup =
        TypeChecker::lookupMember(DC, instanceTy, memberName, lookupOptions);
    for (auto entry : lookup) {
      auto *cand = entry.getValueDecl();

      // If the result is invalid, skip it.
      if (cand->isInvalid()) {
        result.markErrorAlreadyDiagnosed();
        return result;
      }

      result.addUnviable(getOverloadChoice(cand, /*isBridged=*/false,
                                           /*isUnwrappedOptional=*/false),
                         MemberLookupResult::UR_Inaccessible);
    }
  }
  
  return result;
}

/// Determine whether the given type refers to a non-final class (or
/// dynamic self of one).
static bool isNonFinalClass(Type type) {
  if (auto dynamicSelf = type->getAs<DynamicSelfType>())
    type = dynamicSelf->getSelfType();

  if (auto classDecl = type->getClassOrBoundGenericClass())
    return !classDecl->isFinal();

  if (auto archetype = type->getAs<ArchetypeType>())
    if (auto super = archetype->getSuperclass())
      return isNonFinalClass(super);

  return type->isExistentialType();
}

/// Determine whether given constructor reference is valid or does it require
/// any fixes e.g. when base is a protocol metatype.
static ConstraintFix *validateInitializerRef(ConstraintSystem &cs,
                                             ConstructorDecl *init,
                                             ConstraintLocator *locator) {
  auto *anchor = locator->getAnchor();
  if (!anchor)
    return nullptr;

  auto getType = [&cs](const Expr *expr) -> Type {
    return cs.simplifyType(cs.getType(expr))->getRValueType();
  };

  auto locatorEndsWith =
      [](ConstraintLocator *locator,
         ConstraintLocator::PathElementKind eltKind) -> bool {
    auto path = locator->getPath();
    return !path.empty() && path.back().getKind() == eltKind;
  };

  Expr *baseExpr = nullptr;
  Type baseType;

  // Explicit initializer reference e.g. `T.init(...)` or `T.init`.
  if (auto *UDE = dyn_cast<UnresolvedDotExpr>(anchor)) {
    baseExpr = UDE->getBase();
    baseType = getType(baseExpr);
    if (baseType->is<MetatypeType>()) {
      auto instanceType = baseType->getAs<MetatypeType>()
                              ->getInstanceType()
                              ->getWithoutParens();
      if (!cs.isTypeReference(baseExpr) && instanceType->isExistentialType()) {
        return AllowInvalidInitRef::onProtocolMetatype(
            cs, baseType, init, /*isStaticallyDerived=*/true,
            baseExpr->getSourceRange(), locator);
      }
    }
    // Initializer call e.g. `T(...)`
  } else if (auto *CE = dyn_cast<CallExpr>(anchor)) {
    baseExpr = CE->getFn();
    baseType = getType(baseExpr);
    // If this is an initializer call without explicit mention
    // of `.init` on metatype value.
    if (auto *AMT = baseType->getAs<AnyMetatypeType>()) {
      auto instanceType = AMT->getInstanceType()->getWithoutParens();
      if (!cs.isTypeReference(baseExpr)) {
        if (baseType->is<MetatypeType>() &&
            instanceType->isAnyExistentialType()) {
          return AllowInvalidInitRef::onProtocolMetatype(
              cs, baseType, init, cs.isStaticallyDerivedMetatype(baseExpr),
              baseExpr->getSourceRange(), locator);
        }

        if (!instanceType->isExistentialType() ||
            instanceType->isAnyExistentialType()) {
          return AllowInvalidInitRef::onNonConstMetatype(cs, baseType, init,
                                                         locator);
        }
      }
    }
    // Initializer reference which requires contextual base type e.g.
    // `.init(...)`.
  } else if (auto *UME = dyn_cast<UnresolvedMemberExpr>(anchor)) {
    // We need to find type variable which represents contextual base.
    auto *baseLocator = cs.getConstraintLocator(
        UME, locatorEndsWith(locator, ConstraintLocator::ConstructorMember)
                 ? ConstraintLocator::UnresolvedMember
                 : ConstraintLocator::MemberRefBase);

    // FIXME: Type variables responsible for contextual base could be cached
    // in the constraint system to speed up lookup.
    auto result = llvm::find_if(
        cs.getTypeVariables(), [&baseLocator](const TypeVariableType *typeVar) {
          return typeVar->getImpl().getLocator() == baseLocator;
        });

    assert(result != cs.getTypeVariables().end());
    baseType = cs.simplifyType(*result)->getRValueType();
    // Constraint for member base is formed as '$T.Type[.<member] = ...`
    // which means MetatypeType has to be added after finding a type variable.
    if (locatorEndsWith(baseLocator, ConstraintLocator::MemberRefBase))
      baseType = MetatypeType::get(baseType);
  }

  if (!baseType)
    return nullptr;

  if (!baseType->is<AnyMetatypeType>()) {
    bool applicable = false;
    // Special case -- in a protocol extension initializer with a class
    // constrainted Self type, 'self' has archetype type, and only
    // required initializers can be called.
    if (baseExpr && !baseExpr->isSuperExpr()) {
      auto &ctx = cs.getASTContext();
      if (auto *DRE =
              dyn_cast<DeclRefExpr>(baseExpr->getSemanticsProvidingExpr())) {
        if (DRE->getDecl()->getFullName() == ctx.Id_self) {
          if (getType(DRE)->is<ArchetypeType>())
            applicable = true;
        }
      }
    }

    if (!applicable)
      return nullptr;
  }

  auto instanceType = baseType->getMetatypeInstanceType();
  bool isStaticallyDerived = true;
  // If this is expression like `.init(...)` where base type is
  // determined by a contextual type.
  if (!baseExpr) {
    isStaticallyDerived = !(instanceType->is<DynamicSelfType>() ||
                            instanceType->is<ArchetypeType>());
  // Otherwise this is something like `T.init(...)`
  } else {
    isStaticallyDerived = cs.isStaticallyDerivedMetatype(baseExpr);
  }

  auto baseRange = baseExpr ? baseExpr->getSourceRange() : SourceRange();
  // FIXME: The "hasClangNode" check here is a complete hack.
  if (isNonFinalClass(instanceType) && !isStaticallyDerived &&
      !init->hasClangNode() &&
      !(init->isRequired() || init->getDeclContext()->getSelfProtocolDecl())) {
    return AllowInvalidInitRef::dynamicOnMetatype(cs, baseType, init, baseRange,
                                                  locator);
    // Constructors cannot be called on a protocol metatype, because there is no
    // metatype to witness it.
  } else if (baseType->is<MetatypeType>() &&
             instanceType->isExistentialType()) {
    return AllowInvalidInitRef::onProtocolMetatype(
        cs, baseType, init, isStaticallyDerived, baseRange, locator);
  }

  return nullptr;
}

static ConstraintFix *
fixMemberRef(ConstraintSystem &cs, Type baseTy,
             DeclName memberName, const OverloadChoice &choice,
             ConstraintLocator *locator,
             Optional<MemberLookupResult::UnviableReason> reason = None) {
  // Not all of the choices handled here are going
  // to refer to a declaration.
  if (auto *decl = choice.getDeclOrNull()) {
    if (auto *CD = dyn_cast<ConstructorDecl>(decl)) {
      if (auto *fix = validateInitializerRef(cs, CD, locator))
        return fix;
    }

    if (locator->isForKeyPathDynamicMemberLookup()) {
      if (auto *fix = AllowInvalidRefInKeyPath::forRef(cs, decl, locator))
        return fix;
    }
  }

  if (reason) {
    switch (*reason) {
    case MemberLookupResult::UR_InstanceMemberOnType:
    case MemberLookupResult::UR_TypeMemberOnInstance: {
      if (choice.getKind() == OverloadChoiceKind::DynamicMemberLookup ||
          choice.getKind() == OverloadChoiceKind::KeyPathDynamicMemberLookup)
        return nullptr;

      return choice.isDecl()
                 ? AllowTypeOrInstanceMember::create(
                       cs, baseTy, choice.getDecl(), memberName, locator)
                 : nullptr;
    }

    case MemberLookupResult::UR_Inaccessible:
      assert(choice.isDecl());
      return AllowInaccessibleMember::create(cs, baseTy, choice.getDecl(),
                                             memberName, locator);

    case MemberLookupResult::UR_UnavailableInExistential: {
      return choice.isDecl()
                 ? AllowMemberRefOnExistential::create(
                       cs, baseTy, choice.getDecl(), memberName, locator)
                 : nullptr;
    }

    case MemberLookupResult::UR_MutatingMemberOnRValue:
    case MemberLookupResult::UR_MutatingGetterOnRValue: {
      return choice.isDecl()
                 ? AllowMutatingMemberOnRValueBase::create(
                       cs, baseTy, choice.getDecl(), memberName, locator)
                 : nullptr;
    }

    // TODO(diagnostics): Add a new fix that is suggests to
    // add `subscript(dynamicMember: {Writable}KeyPath<T, U>)`
    // overload here, that would help if such subscript has
    // not been provided.
    case MemberLookupResult::UR_WritableKeyPathOnReadOnlyMember:
      return TreatRValueAsLValue::create(cs, cs.getConstraintLocator(locator));
    case MemberLookupResult::UR_ReferenceWritableKeyPathOnMutatingMember:
      break;
    case MemberLookupResult::UR_KeyPathWithAnyObjectRootType:
      return AllowAnyObjectKeyPathRoot::create(cs, locator);
   }
  }

  return nullptr;
}

ConstraintSystem::SolutionKind ConstraintSystem::simplifyMemberConstraint(
    ConstraintKind kind, Type baseTy, DeclName member, Type memberTy,
    DeclContext *useDC, FunctionRefKind functionRefKind,
    ArrayRef<OverloadChoice> outerAlternatives, TypeMatchOptions flags,
    ConstraintLocatorBuilder locatorB) {
  // We'd need to record original base type because it might be a type
  // variable representing another missing member.
  auto origBaseTy = baseTy;
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  baseTy = simplifyType(baseTy, flags);
  Type baseObjTy = baseTy->getRValueType();

  auto locator = getConstraintLocator(locatorB);
  MemberLookupResult result =
      performMemberLookup(kind, member, baseTy, functionRefKind, locator,
                          /*includeInaccessibleMembers*/ shouldAttemptFixes());

  auto formUnsolved = [&](bool activate = false) {
    // If requested, generate a constraint.
    if (flags.contains(TMF_GenerateConstraints)) {
      auto *memberRef = Constraint::createMemberOrOuterDisjunction(
          *this, kind, baseTy, memberTy, member, useDC, functionRefKind,
          outerAlternatives, locator);

      addUnsolvedConstraint(memberRef);

      if (activate)
        activateConstraint(memberRef);

      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;
  };

  switch (result.OverallResult) {
  case MemberLookupResult::Unsolved:
    return formUnsolved();

  case MemberLookupResult::ErrorAlreadyDiagnosed:
    return SolutionKind::Error;

  case MemberLookupResult::HasResults:
    // Keep going!
    break;
  }

  SmallVector<Constraint *, 4> candidates;
  // If we found viable candidates, then we're done!
  if (!result.ViableCandidates.empty()) {
    // If only possible choice to refer to member is via keypath
    // dynamic member dispatch, let's delay solving this constraint
    // until constraint generation phase is complete, because
    // subscript dispatch relies on presence of function application.
    if (result.ViableCandidates.size() == 1) {
      auto &choice = result.ViableCandidates.front();
      if (Phase == ConstraintSystemPhase::ConstraintGeneration &&
          choice.isKeyPathDynamicMemberLookup() &&
          member.getBaseName().isSubscript()) {
        // Let's move this constraint to the active
        // list so it could be picked up right after
        // constraint generation is done.
        return formUnsolved(/*activate=*/true);
      }
    }

    generateConstraints(
        candidates, memberTy, result.ViableCandidates, useDC, locator,
        result.getFavoredIndex(), /*requiresFix=*/false,
        [&](unsigned, const OverloadChoice &choice) {
          return fixMemberRef(*this, baseTy, member, choice, locator);
        });

    if (!outerAlternatives.empty()) {
      // If local scope has a single choice,
      // it should always be preferred.
      if (candidates.size() == 1)
        candidates.front()->setFavored();

      generateConstraints(candidates, memberTy, outerAlternatives,
                          useDC, locator);
    }
  }

  if (!result.UnviableCandidates.empty()) {
    // Generate constraints for unvailable choices if they have a fix,
    // and disable them by default, they'd get picked up in the "salvage" mode.
    generateConstraints(
        candidates, memberTy, result.UnviableCandidates, useDC, locator,
        /*favoredChoice=*/None, /*requiresFix=*/true,
        [&](unsigned idx, const OverloadChoice &choice) {
          return fixMemberRef(*this, baseTy, member, choice, locator,
                              result.UnviableReasons[idx]);
        });
  }

  if (!candidates.empty()) {
    addOverloadSet(candidates, locator);
    return SolutionKind::Solved;
  }

  // If the lookup found no hits at all (either viable or unviable), diagnose it
  // as such and try to recover in various ways.
  if (shouldAttemptFixes()) {
    auto fixMissingMember = [&](Type baseTy, Type memberTy,
                                ConstraintLocator *locator) -> SolutionKind {
      // Let's check whether there are any generic parameters associated with
      // base type, and record potential holes if so.
      simplifyType(baseTy).transform([&](Type type) -> Type {
        if (auto *typeVar = type->getAs<TypeVariableType>()) {
          if (typeVar->getImpl().hasRepresentativeOrFixed())
            return type;
          recordPotentialHole(typeVar);
        }
        return type;
      });

      auto *fix =
          DefineMemberBasedOnUse::create(*this, baseTy, member, locator);
      // Impact is higher if the base is expected to be inferred from context,
      // because a failure to find a member ultimately means that base type is
      // not a match in this case.
      auto impact =
          locator->findLast<LocatorPathElt::UnresolvedMember>() ? 2 : 1;
      if (recordFix(fix, impact))
        return SolutionKind::Error;

      // Allow member type to default to `Any` to make it possible to form
      // solutions when contextual type of the result cannot be deduced e.g.
      // `let _ = x.foo`.
      if (auto *memberTypeVar = memberTy->getAs<TypeVariableType>())
        recordPotentialHole(memberTypeVar);

      return SolutionKind::Solved;
    };

    if (baseObjTy->getOptionalObjectType()) {
      // If the base type was an optional, look through it.

      // If the base type is optional because we haven't chosen to force an
      // implicit optional, don't try to fix it. The IUO will be forced instead.
      if (auto dotExpr =
              dyn_cast_or_null<UnresolvedDotExpr>(locator->getAnchor())) {
        auto baseExpr = dotExpr->getBase();
        auto resolvedOverload = getResolvedOverloadSets();
        while (resolvedOverload) {
          if (resolvedOverload->Locator->getAnchor() == baseExpr) {
            if (resolvedOverload->Choice
                    .isImplicitlyUnwrappedValueOrReturnValue())
              return SolutionKind::Error;
            break;
          }
          resolvedOverload = resolvedOverload->Previous;
        }
      }

      // Let's check whether the problem is related to optionality of base
      // type, or there is no member with a given name.
      result =
          performMemberLookup(kind, member, baseObjTy->getOptionalObjectType(),
                              functionRefKind, locator,
                              /*includeInaccessibleMembers*/ true);

      // If uwrapped type still couldn't find anything for a given name,
      // let's fallback to a "not such member" fix.
      if (result.ViableCandidates.empty() && result.UnviableCandidates.empty())
        return fixMissingMember(origBaseTy, memberTy, locator);

      // The result of the member access can either be the expected member type
      // (for '!' or optional members with '?'), or the original member type
      // with one extra level of optionality ('?' with non-optional members).
      auto innerTV = createTypeVariable(locator,
                                        TVO_CanBindToLValue |
                                        TVO_CanBindToNoEscape);
      Type optTy = TypeChecker::getOptionalType(SourceLoc(), innerTV);
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
                               innerTV, useDC, functionRefKind,
                               outerAlternatives, locator);
      return SolutionKind::Solved;
    }

    auto solveWithNewBaseOrName = [&](Type baseType,
                                      DeclName memberName) -> SolutionKind {
      return simplifyMemberConstraint(kind, baseType, memberName, memberTy,
                                      useDC, functionRefKind, outerAlternatives,
                                      flags | TMF_ApplyingFix, locatorB);
    };

    // If this member reference is a result of a previous fix, let's not allow
    // any more fixes expect when base is optional, because it could also be
    // an IUO which requires a separate fix.
    if (flags.contains(TMF_ApplyingFix))
      return SolutionKind::Error;

    // Check if any property wrappers on the base of the member lookup have
    // matching members that we can fall back to, or if the type wraps any
    // properties that have matching members.
    if (auto *fix = fixPropertyWrapperFailure(
            *this, baseTy, locator,
            [&](ResolvedOverloadSetListItem *overload, VarDecl *decl,
                Type newBase) {
              return solveWithNewBaseOrName(newBase, member) ==
                     SolutionKind::Solved;
            })) {
      return recordFix(fix) ? SolutionKind::Error : SolutionKind::Solved;
    }

    if (auto *funcType = baseTy->getAs<FunctionType>()) {
      // We can't really suggest anything useful unless
      // function takes no arguments, otherwise it
      // would make sense to report this a missing member.
      if (funcType->getNumParams() == 0) {
        auto result = solveWithNewBaseOrName(funcType->getResult(), member);
        // If there is indeed a member with given name in result type
        // let's return, otherwise let's fall-through and report
        // this problem as a missing member.
        if (result == SolutionKind::Solved)
          return recordFix(InsertExplicitCall::create(*this, locator))
                     ? SolutionKind::Error
                     : SolutionKind::Solved;
      }
    }

    // Instead of using subscript operator spelled out `subscript` directly.
    if (member.getBaseName() == getTokenText(tok::kw_subscript)) {
      auto result =
          solveWithNewBaseOrName(baseTy, DeclBaseName::createSubscript());
      // Looks like it was indeed meant to be a subscript operator.
      if (result == SolutionKind::Solved)
        return recordFix(UseSubscriptOperator::create(*this, locator))
                   ? SolutionKind::Error
                   : SolutionKind::Solved;
    }

    // FIXME(diagnostics): This is more of a hack than anything.
    // Let's not try to suggest that there is no member related to an
    // obscure underscored type, the real problem would be somewhere
    // else. This helps to diagnose pattern matching cases.
    {
      if (auto *metatype = baseTy->getAs<MetatypeType>()) {
        auto instanceTy = metatype->getInstanceType();
        if (auto *NTD = instanceTy->getAnyNominal()) {
          if (NTD->getName() == getASTContext().Id_OptionalNilComparisonType)
            return SolutionKind::Error;
        }
      }
    }

    // FIXME(diagnostics): Errors related to `AnyObject` could be diagnosed
    // better in the future, relevant failure information has to be extracted
    // from `performMemberLookup` result, in order to figure out if it was a
    // simple labeling or # of arguments mismatch, or member with requested name
    // really doesn't exist.
    if (baseTy->isAnyObject())
      return SolutionKind::Error;

    result = performMemberLookup(kind, member, baseTy, functionRefKind, locator,
                                 /*includeInaccessibleMembers*/ true);

    // FIXME(diagnostics): If there were no viable results, but there are
    // unviable ones, we'd have to introduce fix for each specific problem.
    if (!result.UnviableCandidates.empty())
      return SolutionKind::Error;

    // Since member with given base and name doesn't exist, let's try to
    // fake its presence based on use, that makes it possible to diagnose
    // problems related to member lookup more precisely.

    // If base type is a "hole" there is no reason to record any
    // more "member not found" fixes for chained member references.
    if (baseTy->isHole()) {
      increaseScore(SK_Fix);
      if (auto *memberTypeVar = memberTy->getAs<TypeVariableType>())
        recordPotentialHole(memberTypeVar);
      return SolutionKind::Solved;
    }

    return fixMissingMember(origBaseTy, memberTy, locator);
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
ConstraintSystem::simplifyOneWayConstraint(
    ConstraintKind kind,
    Type first, Type second, TypeMatchOptions flags,
    ConstraintLocatorBuilder locator) {
  // Determine whether the second type can be fully simplified. Only then
  // will this constraint be resolved.
  Type secondSimplified = simplifyType(second);
  if (secondSimplified->hasTypeVariable()) {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::create(*this, kind, first, second,
                           getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;
  }

  // Translate this constraint into a one-way binding constraint.
  return matchTypes(first, secondSimplified, ConstraintKind::Equal, flags,
                    locator);
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
ConstraintSystem::simplifyOpaqueUnderlyingTypeConstraint(Type type1, Type type2,
                                             TypeMatchOptions flags,
                                             ConstraintLocatorBuilder locator) {
  // Open the second type, which must be an opaque archetype, to try to
  // infer the first type using its constraints.
  auto opaque2 = type2->castTo<OpaqueTypeArchetypeType>();

  // Open the generic signature of the opaque decl, and bind the "outer" generic
  // params to our context. The remaining axes of freedom on the type variable
  // corresponding to the underlying type should be the constraints on the
  // underlying return type.
  OpenedTypeMap replacements;
  openGeneric(DC, opaque2->getBoundSignature(), locator, replacements);

  auto underlyingTyVar = openType(opaque2->getInterfaceType(),
                                  replacements);
  assert(underlyingTyVar);
  
  if (auto dcSig = DC->getGenericSignatureOfContext()) {
    for (auto param : dcSig->getGenericParams()) {
      addConstraint(ConstraintKind::Bind,
                    openType(param, replacements),
                    DC->mapTypeIntoContext(param),
                    locator);
    }
  }

  addConstraint(ConstraintKind::Equal, type1, underlyingTyVar, locator);
  return getTypeMatchSuccess();
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
  auto &ctx = getASTContext();
  if (unwrappedFromType->isPotentiallyBridgedValueType() &&
      (unwrappedToType->isBridgeableObjectType() ||
       (unwrappedToType->isExistentialType() &&
        !unwrappedToType->isAny()))) {
    countOptionalInjections();
    if (Type classType = ctx.getBridgedToObjC(DC, unwrappedFromType)) {
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
    if (auto objcClass = ctx.getBridgedToObjC(DC, unwrappedToType,
                                              &bridgedValueType)) {
      // Bridging NSNumber to NSValue is one-way, since there are multiple Swift
      // value types that bridge to those object types. It requires a checked
      // cast to get back.
      if (ctx.isObjCClassWithMultipleSwiftBridgedTypes(objcClass))
        return SolutionKind::Error;

      // If the bridged value type is generic, the generic arguments
      // must either match or be bridged.
      // FIXME: This should be an associated type of the protocol.
      auto &ctx = getASTContext();
      if (auto fromBGT = unwrappedToType->getAs<BoundGenericType>()) {
        if (fromBGT->getDecl() == ctx.getArrayDecl()) {
          // [AnyObject]
          addConstraint(ConstraintKind::Bind, fromBGT->getGenericArgs()[0],
                        ctx.getAnyObjectType(),
                        getConstraintLocator(locator.withPathElement(
                            LocatorPathElt::GenericArgument(0))));
        } else if (fromBGT->getDecl() == ctx.getDictionaryDecl()) {
          // [NSObject : AnyObject]
          auto nsObjectType = ctx.getNSObjectType();
          if (!nsObjectType) {
            // Not a bridging case. Should we detect this earlier?
            return SolutionKind::Error;
          }

          addConstraint(ConstraintKind::Bind, fromBGT->getGenericArgs()[0],
                        nsObjectType,
                        getConstraintLocator(
                          locator.withPathElement(
                            LocatorPathElt::GenericArgument(0))));

          addConstraint(ConstraintKind::Bind, fromBGT->getGenericArgs()[1],
                        ctx.getAnyObjectType(),
                        getConstraintLocator(
                          locator.withPathElement(
                            LocatorPathElt::GenericArgument(1))));
        } else if (fromBGT->getDecl() == ctx.getSetDecl()) {
          auto nsObjectType = ctx.getNSObjectType();
          if (!nsObjectType) {
            // Not a bridging case. Should we detect this earlier?
            return SolutionKind::Error;
          }
          addConstraint(ConstraintKind::Bind, fromBGT->getGenericArgs()[0],
                        nsObjectType,
                        getConstraintLocator(
                          locator.withPathElement(
                            LocatorPathElt::GenericArgument(0))));
        } else {
          // Nothing special to do; matchTypes will match generic arguments.
        }
      }

      // Make sure we have the bridged value type.
      if (matchTypes(unwrappedToType, bridgedValueType, ConstraintKind::Bind,
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
          locator.withPathElement(LocatorPathElt::GenericArgument(0)));
    }
  }

  // Bridging the keys/values of a dictionary.
  if (auto fromKeyValue = isDictionaryType(unwrappedFromType)) {
    if (auto toKeyValue = isDictionaryType(unwrappedToType)) {
      addExplicitConversionConstraint(fromKeyValue->first, toKeyValue->first,
                                      /*allowFixes=*/false,
                                      locator.withPathElement(
                                        LocatorPathElt::GenericArgument(0)));
      addExplicitConversionConstraint(fromKeyValue->second, toKeyValue->second,
                                      /*allowFixes=*/false,
                                      locator.withPathElement(
                                        LocatorPathElt::GenericArgument(0)));
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
          locator.withPathElement(LocatorPathElt::GenericArgument(0)));
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
    Type openedTy = OpenedArchetypeType::get(instanceTy);
    if (isMetatype)
      openedTy = MetatypeType::get(openedTy, getASTContext());
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
ConstraintSystem::simplifyKeyPathConstraint(
    Type keyPathTy,
    Type rootTy,
    Type valueTy,
    ArrayRef<TypeVariableType *> componentTypeVars,
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
    auto path = locator->getPath();
    if (locator->getAnchor() != keyPath || path.size() > 2)
      continue;

    if (auto kpElt = path[0].getAs<LocatorPathElt::KeyPathComponent>()) {
      choices[kpElt->getIndex()] = resolvedItem->Choice;
    }
  }

  keyPathTy = getFixedTypeRecursive(keyPathTy, /*want rvalue*/ true);
  bool definitelyFunctionType = false;
  bool definitelyKeyPathType = false;

  auto tryMatchRootAndValueFromType = [&](Type type,
                                          bool allowPartial = true) -> bool {
    Type boundRoot = Type(), boundValue = Type();

    if (auto bgt = type->getAs<BoundGenericType>()) {
      definitelyKeyPathType = true;

      // We can get root and value from a concrete key path type.
      if (bgt->getDecl() == getASTContext().getKeyPathDecl() ||
          bgt->getDecl() == getASTContext().getWritableKeyPathDecl() ||
          bgt->getDecl() == getASTContext().getReferenceWritableKeyPathDecl()) {
        boundRoot = bgt->getGenericArgs()[0];
        boundValue = bgt->getGenericArgs()[1];
      } else if (bgt->getDecl() == getASTContext().getPartialKeyPathDecl()) {
        if (!allowPartial)
          return false;

        // We can still get the root from a PartialKeyPath.
        boundRoot = bgt->getGenericArgs()[0];
      }
    }

    if (auto fnTy = type->getAs<FunctionType>()) {
      definitelyFunctionType = true;

      if (fnTy->getParams().size() != 1)
        return false;

      boundRoot = fnTy->getParams()[0].getPlainType();
      boundValue = fnTy->getResult();
    }

    if (boundRoot &&
        matchTypes(boundRoot, rootTy, ConstraintKind::Bind, subflags, locator)
            .isFailure())
      return false;

    if (boundValue &&
        matchTypes(boundValue, valueTy, ConstraintKind::Bind, subflags, locator)
            .isFailure())
      return false;

    return true;
  };

  // If we're fixed to a bound generic type, trying harvesting context from it.
  // However, we don't want a solution that fixes the expression type to
  // PartialKeyPath; we'd rather that be represented using an upcast conversion.
  if (!tryMatchRootAndValueFromType(keyPathTy, /*allowPartial=*/false))
    return SolutionKind::Error;

  // If the expression has contextual type information, try using that too.
  if (auto contextualTy = getContextualType(keyPath)) {
    if (!tryMatchRootAndValueFromType(contextualTy))
      return SolutionKind::Error;
  }

  // See if we resolved overloads for all the components involved.
  enum {
    ReadOnly,
    Writable,
    ReferenceWritable
  } capability = Writable;

  bool anyComponentsUnresolved = false;

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
      // If no choice was made, leave the constraint unsolved. But when
      // generating constraints, we may already have enough information
      // to determine whether the result will be a function type vs BGT KeyPath
      // type, so continue through components to create new constraint at the
      // end.
      if (choices[i].isInvalid() || anyComponentsUnresolved) {
        if (flags.contains(TMF_GenerateConstraints)) {
          anyComponentsUnresolved = true;
          continue;
        }

        if (shouldAttemptFixes()) {
          auto typeVar =
              llvm::find_if(componentTypeVars, [&](TypeVariableType *typeVar) {
                auto *locator = typeVar->getImpl().getLocator();
                auto elt = locator->findLast<LocatorPathElt::KeyPathComponent>();
                return elt && elt->getIndex() == i;
              });

          // If one of the components haven't been resolved, let's check
          // whether it has been determined to be a "hole" and if so,
          // let's allow component validation to contiunue.
          //
          // This helps to, for example, diagnose problems with missing
          // members used as part of a key path.
          if (typeVar != componentTypeVars.end() &&
              (*typeVar)->getImpl().canBindToHole()) {
            anyComponentsUnresolved = true;
            capability = ReadOnly;
            continue;
          }
        }

        return SolutionKind::Unsolved;
      }

      // tuple elements do not change the capability of the key path
      if (choices[i].getKind() == OverloadChoiceKind::TupleIndex) {
        continue;
      }
        
      // Discarded unsupported non-decl member lookups.
      if (!choices[i].isDecl()) {
        return SolutionKind::Error;
      }

      auto storage = dyn_cast<AbstractStorageDecl>(choices[i].getDecl());

      auto *componentLoc = getConstraintLocator(
          locator.withPathElement(LocatorPathElt::KeyPathComponent(i)));

      if (auto *fix = AllowInvalidRefInKeyPath::forRef(
              *this, choices[i].getDecl(), componentLoc)) {
        if (!hasFixFor(componentLoc, FixKind::AllowTypeOrInstanceMember))
          if (!shouldAttemptFixes() || recordFix(fix))
            return SolutionKind::Error;

        // If this was a method reference let's mark it as read-only.
        if (!storage) {
          capability = ReadOnly;
          continue;
        }
      }

      if (!storage)
        return SolutionKind::Error;

      if (isReadOnlyKeyPathComponent(storage)) {
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

    case KeyPathExpr::Component::Kind::TupleElement:
      llvm_unreachable("not implemented");
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
  if (auto keyPathBGT = keyPathTy->getAs<BoundGenericType>()) {
    if (keyPathBGT->getDecl() == getASTContext().getKeyPathDecl())
      kpDecl = getASTContext().getKeyPathDecl();
    else if (keyPathBGT->getDecl() ==
                 getASTContext().getWritableKeyPathDecl() &&
             capability >= Writable)
      kpDecl = getASTContext().getWritableKeyPathDecl();
  }

  auto loc = locator.getBaseLocator();
  if (definitelyFunctionType) {
    increaseScore(SK_FunctionConversion);
    return SolutionKind::Solved;
  } else if (!anyComponentsUnresolved ||
             (definitelyKeyPathType && capability == ReadOnly)) {
    auto resolvedKPTy =
        BoundGenericType::get(kpDecl, nullptr, {rootTy, valueTy});
    return matchTypes(keyPathTy, resolvedKPTy, ConstraintKind::Bind, subflags,
                      loc);
  } else {
    addUnsolvedConstraint(Constraint::create(*this, ConstraintKind::KeyPath,
                                             keyPathTy, rootTy, valueTy,
                                             locator.getBaseLocator(),
                                             componentTypeVars));
  }
  return SolutionKind::Solved;
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

Type ConstraintSystem::simplifyAppliedOverloads(
                                TypeVariableType *fnTypeVar,
                                const FunctionType *argFnType,
                                ConstraintLocatorBuilder locator) {
  Type fnType(fnTypeVar);

  // Always work on the representation.
  fnTypeVar = getRepresentative(fnTypeVar);

  // Dig out the disjunction that describes this overload.
  unsigned numOptionalUnwraps = 0;
  auto disjunction =
      getUnboundBindOverloadDisjunction(fnTypeVar, &numOptionalUnwraps);
  if (!disjunction) return fnType;

  /// The common result type amongst all function overloads.
  Type commonResultType;
  auto updateCommonResultType = [&](Type choiceType) {
    auto markFailure = [&] {
      commonResultType = ErrorType::get(getASTContext());
    };

    auto choiceFnType = choiceType->getAs<FunctionType>();
    if (!choiceFnType)
      return markFailure();

    // For now, don't attempt to establish a common result type when there
    // are type parameters.
    Type choiceResultType = choiceFnType->getResult();
    if (choiceResultType->hasTypeParameter())
      return markFailure();

    // If we haven't seen a common result type yet, record what we found.
    if (!commonResultType) {
      commonResultType = choiceResultType;
      return;
    }

    // If we found something different, fail.
    if (!commonResultType->isEqual(choiceResultType))
      return markFailure();
  };

  auto argumentInfo = getArgumentInfo(getConstraintLocator(locator));

  // Consider each of the constraints in the disjunction.
retry_after_fail:
  bool hasUnhandledConstraints = false;
  bool labelMismatch = false;
  auto filterResult =
      filterDisjunction(disjunction, /*restoreOnFail=*/shouldAttemptFixes(),
                         [&](Constraint *constraint) {
        assert(constraint->getKind() == ConstraintKind::BindOverload);

        auto choice = constraint->getOverloadChoice();

        // Determine whether the argument labels we have conflict with those of
        // this overload choice.
        if (argumentInfo) {
          auto args = argFnType->getParams();

          SmallVector<FunctionType::Param, 8> argsWithLabels;
          argsWithLabels.append(args.begin(), args.end());
          FunctionType::relabelParams(argsWithLabels, argumentInfo->Labels);

          if (!areConservativelyCompatibleArgumentLabels(
                  choice, argsWithLabels, argumentInfo->HasTrailingClosure)) {
            labelMismatch = true;
            return false;
          }
        }

        // Determine the type that this choice will have.
        Type choiceType =
            getEffectiveOverloadType(choice, /*allowMembers=*/true,
                                     constraint->getOverloadUseDC());
        if (!choiceType) {
          hasUnhandledConstraints = true;
          return true;
        }

        // Account for any optional unwrapping/binding
        for (unsigned i : range(numOptionalUnwraps)) {
          (void)i;
          if (Type objectType = choiceType->getOptionalObjectType())
            choiceType = objectType;
        }

        // If we have a function type, we can compute a common result type.
        updateCommonResultType(choiceType);
        return true;
      });

  switch (filterResult) {
  case SolutionKind::Error:
    if (labelMismatch && shouldAttemptFixes()) {
      argumentInfo.reset();
      goto retry_after_fail;
    }

    return Type();

  case SolutionKind::Solved:
    // We should now have a type for the one remaining overload.
    fnType = getFixedTypeRecursive(fnType, /*wantRValue=*/true);
    break;

  case SolutionKind::Unsolved:
    break;
  }

  // If there was a constraint that we couldn't reason about, don't use the
  // results of any common-type computations.
  if (hasUnhandledConstraints)
    return fnType;

  // If we have a common result type, bind the expected result type to it.
  if (commonResultType && !commonResultType->is<ErrorType>()) {
    ASTContext &ctx = getASTContext();
    if (ctx.TypeCheckerOpts.DebugConstraintSolver) {
      auto &log = ctx.TypeCheckerDebug->getStream();
      log.indent(solverState ? solverState->depth * 2 + 2 : 0)
        << "(common result type for $T" << fnTypeVar->getID() << " is "
        << commonResultType.getString()
        << ")\n";
    }

    // FIXME: Could also rewrite fnType to include this result type.
    // Introduction of `Bind` constraint here could result in the disconnect
    // in the constraint system with unintended consequences because e.g.
    // in case of key path application it could disconnect one of the
    // components like subscript from the rest of the context.
    addConstraint(ConstraintKind::Equal, argFnType->getResult(),
                  commonResultType, locator);
  }

  return fnType;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyApplicableFnConstraint(
                                           Type type1,
                                           Type type2,
                                           TypeMatchOptions flags,
                                           ConstraintLocatorBuilder locator) {
  auto &ctx = getASTContext();

  // By construction, the left hand side is a type that looks like the
  // following: $T1 -> $T2.
  auto func1 = type1->castTo<FunctionType>();

  // Before stripping lvalue-ness and optional types, save the original second
  // type for handling `func callAsFunction` and `@dynamicCallable`
  // applications. This supports the following cases:
  // - Generating constraints for `mutating func callAsFunction`. The nominal
  //   type (`type2`) should be an lvalue type.
  // - Extending `Optional` itself with `func callAsFunction` or
  //   `@dynamicCallable` functionality. Optional types are stripped below if
  //   `shouldAttemptFixes()` is true.
  auto origLValueType2 =
      getFixedTypeRecursive(type2, flags, /*wantRValue=*/false);
  // Drill down to the concrete type on the right hand side.
  type2 = getFixedTypeRecursive(type2, flags, /*wantRValue=*/true);
  auto desugar2 = type2->getDesugaredType();

  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  SmallVector<LocatorPathElt, 2> parts;
  Expr *anchor = locator.getLocatorParts(parts);
  bool isOperator = (isa<PrefixUnaryExpr>(anchor) ||
                     isa<PostfixUnaryExpr>(anchor) ||
                     isa<BinaryExpr>(anchor));

  auto hasInOut = [&]() {
    for (auto param : func1->getParams())
      if (param.isInOut())
        return true;
    return false;
  };

  // If the types are obviously equivalent, we're done. This optimization
  // is not valid for operators though, where an inout parameter does not
  // have an explicit inout argument.
  if (type1.getPointer() == desugar2) {
    if (!isOperator || !hasInOut())
      return SolutionKind::Solved;
  }

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

  // Don't attempt this optimization in "diagnostic mode" because
  // in such mode we'd like to attempt all of the available
  // overloads regardless of problems related to missing or
  // extraneous labels and/or arguments.
  if (!(solverState && shouldAttemptFixes())) {
    // If the right-hand side is a type variable,
    // try to simplify the overload set.
    if (auto typeVar = desugar2->getAs<TypeVariableType>()) {
      Type newType2 = simplifyAppliedOverloads(typeVar, func1, locator);
      if (!newType2)
        return SolutionKind::Error;

      desugar2 = newType2->getDesugaredType();
    }
  }

  // If right-hand side is a type variable, the constraint is unsolved.
  if (desugar2->isTypeVariableOrMember())
    return formUnsolved();

  // Strip the 'ApplyFunction' off the locator.
  // FIXME: Perhaps ApplyFunction can go away entirely?
  assert(!parts.empty() && "Nonsensical applicable-function locator");
  assert(parts.back().getKind() == ConstraintLocator::ApplyFunction);
  assert(parts.back().getNewSummaryFlags() == 0);
  parts.pop_back();
  ConstraintLocatorBuilder outerLocator =
    getConstraintLocator(anchor, parts, locator.getSummaryFlags());

  // Handle applications of types with `callAsFunction` methods.
  // Do this before stripping optional types below, when `shouldAttemptFixes()`
  // is true.
  auto hasCallAsFunctionMethods =
      desugar2->mayHaveMembers() &&
      llvm::any_of(lookupMember(desugar2, DeclName(ctx.Id_callAsFunction)),
                   [](LookupResultEntry entry) {
                     return isa<FuncDecl>(entry.getValueDecl());
                   });
  if (hasCallAsFunctionMethods) {
    auto memberLoc = getConstraintLocator(
        outerLocator.withPathElement(ConstraintLocator::Member));
    // Add a `callAsFunction` member constraint, binding the member type to a
    // type variable.
    auto memberTy = createTypeVariable(memberLoc, /*options=*/0);
    // TODO: Revisit this if `static func callAsFunction` is to be supported.
    // Static member constraint requires `FunctionRefKind::DoubleApply`.
    addValueMemberConstraint(origLValueType2, DeclName(ctx.Id_callAsFunction),
                             memberTy, DC, FunctionRefKind::SingleApply,
                             /*outerAlternatives*/ {}, locator);
    // Add new applicable function constraint based on the member type
    // variable.
    addConstraint(ConstraintKind::ApplicableFunction, func1, memberTy,
                  locator);
    return SolutionKind::Solved;
  }

  // Record the second type before unwrapping optionals.
  auto origType2 = desugar2;
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
  if (auto func2 = dyn_cast<FunctionType>(desugar2)) {
    ConstraintKind subKind = (isOperator
                              ? ConstraintKind::OperatorArgumentConversion
                              : ConstraintKind::ArgumentConversion);

    // The argument type must be convertible to the input type.
    if (::matchCallArguments(
            *this, func2, func1->getParams(), func2->getParams(), subKind,
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

    if (unwrapCount == 0)
      return SolutionKind::Solved;

    // Record any fixes we attempted to get to the correct solution.
    auto *fix = ForceOptional::create(*this, origType2, func1,
                                      getConstraintLocator(locator));
    if (recordFix(fix, /*impact=*/unwrapCount))
      return SolutionKind::Error;

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
      if (unwrapCount == 0)
        return SolutionKind::Solved;

      auto *fix = ForceOptional::create(*this, origType2, func1,
                                        getConstraintLocator(locator));
      if (recordFix(fix, /*impact=*/unwrapCount))
        return SolutionKind::Error;
    }

    return simplified;
  }

  // Handle applications of @dynamicCallable types.
  auto result = simplifyDynamicCallableApplicableFnConstraint(
      type1, origType2, subflags, locator);

  if (shouldAttemptFixes() && result == SolutionKind::Error) {
    // Skip this fix if the type is not yet resolved or
    // it's a function type/metatype which points to argument mismatches.
    if (desugar2->is<TypeVariableType>() || desugar2->is<FunctionType>() ||
        desugar2->is<AnyMetatypeType>())
      return SolutionKind::Error;

    if (auto objectTy = desugar2->lookThroughAllOptionalTypes()) {
      if (objectTy->isAny() || objectTy->isAnyObject())
        return SolutionKind::Error;
    }

    // If there are any type variables associated with arguments/result
    // they have to be marked as "holes".
    type1.visit([&](Type subType) {
      if (auto *typeVar = subType->getAs<TypeVariableType>())
        recordPotentialHole(typeVar);
    });

    if (desugar2->isHole())
      return SolutionKind::Solved;

    auto *fix = RemoveInvalidCall::create(*this, getConstraintLocator(locator));
    // Let's make this fix as high impact so if there is a function or member
    // overload with e.g. argument-to-parameter type mismatches it would take
    // a higher priority.
    return recordFix(fix, /*impact=*/10) ? SolutionKind::Error
                                         : SolutionKind::Solved;
  }

  return result;
}

/// Looks up and returns the @dynamicCallable required methods (if they exist)
/// implemented by a type.
static llvm::DenseSet<FuncDecl *>
lookupDynamicCallableMethods(Type type, ConstraintSystem &CS,
                             const ConstraintLocatorBuilder &locator,
                             Identifier argumentName, bool hasKeywordArgs) {
  auto &ctx = CS.getASTContext();
  auto decl = type->getAnyNominal();
  auto methodName = DeclName(ctx, ctx.Id_dynamicallyCall, { argumentName });
  auto matches = CS.performMemberLookup(ConstraintKind::ValueMember,
                                        methodName, type,
                                        FunctionRefKind::SingleApply,
                                        CS.getConstraintLocator(locator),
                                        /*includeInaccessibleMembers*/ false);
  // Filter valid candidates.
  auto candidates = matches.ViableCandidates;
  auto filter = [&](OverloadChoice choice) {
    auto cand = cast<FuncDecl>(choice.getDecl());
    return !isValidDynamicCallableMethod(cand, decl, hasKeywordArgs);
  };
  candidates.erase(
      std::remove_if(candidates.begin(), candidates.end(), filter),
      candidates.end());

  llvm::DenseSet<FuncDecl *> methods;
  for (auto candidate : candidates)
    methods.insert(cast<FuncDecl>(candidate.getDecl()));
  return methods;
}

/// Looks up and returns the @dynamicCallable required methods (if they exist)
/// implemented by a type. This function should not be called directly:
/// instead, call `getDynamicCallableMethods` which performs caching.
static DynamicCallableMethods
lookupDynamicCallableMethods(Type type, ConstraintSystem &CS,
                             const ConstraintLocatorBuilder &locator) {
  auto &ctx = CS.getASTContext();
  DynamicCallableMethods methods;
  methods.argumentsMethods =
    lookupDynamicCallableMethods(type, CS, locator, ctx.Id_withArguments,
                                 /*hasKeywordArgs*/ false);
  methods.keywordArgumentsMethods =
    lookupDynamicCallableMethods(type, CS, locator,
                                 ctx.Id_withKeywordArguments,
                                 /*hasKeywordArgs*/ true);
  return methods;
}

/// Returns the @dynamicCallable required methods (if they exist) implemented
/// by a type.
/// This function may be slow for deep class hierarchies and multiple protocol
/// conformances,  but it is invoked only after other constraint simplification
/// rules fail.
static DynamicCallableMethods
getDynamicCallableMethods(Type type, ConstraintSystem &CS,
                          const ConstraintLocatorBuilder &locator) {
  auto canType = type->getCanonicalType();
  auto it = CS.DynamicCallableCache.find(canType);
  if (it != CS.DynamicCallableCache.end()) return it->second;

  // Calculate @dynamicCallable methods for composite types with multiple
  // components (protocol composition types and archetypes).
  auto calculateForComponentTypes =
      [&](ArrayRef<Type> componentTypes) -> DynamicCallableMethods {
    DynamicCallableMethods methods;
    for (auto componentType : componentTypes) {
      auto tmp = getDynamicCallableMethods(componentType, CS, locator);
      methods.argumentsMethods.insert(tmp.argumentsMethods.begin(),
                                      tmp.argumentsMethods.end());
      methods.keywordArgumentsMethods.insert(
          tmp.keywordArgumentsMethods.begin(),
          tmp.keywordArgumentsMethods.end());
    }
    return methods;
  };

  // Calculate @dynamicCallable methods.
  auto calculate = [&]() -> DynamicCallableMethods {
    // If this is an archetype type, check if any types it conforms to
    // (superclass or protocols) have the attribute.
    if (auto archetype = dyn_cast<ArchetypeType>(canType)) {
      SmallVector<Type, 2> componentTypes;
      for (auto protocolDecl : archetype->getConformsTo())
        componentTypes.push_back(protocolDecl->getDeclaredType());
      if (auto superclass = archetype->getSuperclass())
        componentTypes.push_back(superclass);
      return calculateForComponentTypes(componentTypes);
    }

    // If this is a protocol composition, check if any of its members have the
    // attribute.
    if (auto protocolComp = dyn_cast<ProtocolCompositionType>(canType))
      return calculateForComponentTypes(protocolComp->getMembers());

    // Otherwise, this must be a nominal type.
    // Dynamic calling doesn't work for tuples, etc.
    auto nominal = canType->getAnyNominal();
    if (!nominal) return DynamicCallableMethods();

    // If this type conforms to a protocol which has the attribute, then
    // look up the methods.
    for (auto p : nominal->getAllProtocols())
      if (p->getAttrs().hasAttribute<DynamicCallableAttr>())
        return lookupDynamicCallableMethods(type, CS, locator);

    // Walk superclasses, if present.
    llvm::SmallPtrSet<const NominalTypeDecl*, 8> visitedDecls;
    while (1) {
      // If we found a circular parent class chain, reject this.
      if (!visitedDecls.insert(nominal).second)
        return DynamicCallableMethods();

      // If this type has the attribute on it, then look up the methods.
      if (nominal->getAttrs().hasAttribute<DynamicCallableAttr>())
        return lookupDynamicCallableMethods(type, CS, locator);

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

  auto result = calculate();
  // Cache the result if the type does not contain type variables.
  if (!type->hasTypeVariable())
    CS.DynamicCallableCache[canType] = result;
  return result;
}

// TODO: Refactor/simplify this function.
// - It should perform less duplicate work with its caller
//   `ConstraintSystem::simplifyApplicableFnConstraint`.
// - It should generate a member constraint instead of manually forming an
//   overload set for `func dynamicallyCall` candidates.
// - It should support `mutating func dynamicallyCall`. This should fall out of
//   using member constraints with an lvalue base type.
ConstraintSystem::SolutionKind
ConstraintSystem::simplifyDynamicCallableApplicableFnConstraint(
                                            Type type1,
                                            Type type2,
                                            TypeMatchOptions flags,
                                            ConstraintLocatorBuilder locator) {
  auto &ctx = getASTContext();

  // By construction, the left hand side is a function type: $T1 -> $T2.
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
        Constraint::create(*this,
          ConstraintKind::DynamicCallableApplicableFunction, type1, type2,
          getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }
    return SolutionKind::Unsolved;
  };

  // If right-hand side is a type variable, the constraint is unsolved.
  if (desugar2->isTypeVariableOrMember())
    return formUnsolved();

  // If right-hand side is a function type, it must be a valid
  // `dynamicallyCall` method type. Bind the output and convert the argument
  // to the input.
  auto func1 = type1->castTo<FunctionType>();
  if (auto func2 = dyn_cast<FunctionType>(desugar2)) {
    // The argument type must be convertible to the input type.
    assert(func1->getParams().size() == 1 && func2->getParams().size() == 1 &&
           "Expected `dynamicallyCall` method with one parameter");
    assert((func2->getParams()[0].getLabel() == ctx.Id_withArguments ||
            func2->getParams()[0].getLabel() == ctx.Id_withKeywordArguments) &&
           "Expected 'dynamicallyCall' method argument label 'withArguments' "
           "or 'withKeywordArguments'");
    if (matchTypes(func1->getParams()[0].getPlainType(),
                   func2->getParams()[0].getPlainType(),
                   ConstraintKind::ArgumentConversion,
                   subflags,
                   locator.withPathElement(
                     ConstraintLocator::ApplyArgument)).isFailure())
      return SolutionKind::Error;

    // The result types are equivalent.
    if (matchTypes(func1->getResult(),
                   func2->getResult(),
                   ConstraintKind::Bind,
                   subflags,
                   locator.withPathElement(
                     ConstraintLocator::FunctionResult)).isFailure())
      return SolutionKind::Error;

    return SolutionKind::Solved;
  }

  // If the right-hand side is not a function type, it must be a valid
  // @dynamicCallable type. Attempt to get valid `dynamicallyCall` methods.
  auto methods = getDynamicCallableMethods(desugar2, *this, locator);
  if (!methods.isValid()) return SolutionKind::Error;

  // Determine whether to call a `withArguments` method or a
  // `withKeywordArguments` method.
  bool useKwargsMethod = methods.argumentsMethods.empty();
  useKwargsMethod |= llvm::any_of(
    func1->getParams(), [](AnyFunctionType::Param p) { return p.hasLabel(); });

  auto candidates = useKwargsMethod ?
    methods.keywordArgumentsMethods :
    methods.argumentsMethods;

  // Create a type variable for the `dynamicallyCall` method.
  auto loc = getConstraintLocator(locator);
  auto tv = createTypeVariable(loc,
                               TVO_CanBindToLValue |
                               TVO_CanBindToNoEscape);

  // Record the 'dynamicallyCall` method overload set.
  SmallVector<OverloadChoice, 4> choices;
  for (auto candidate : candidates) {
    if (candidate->isInvalid()) continue;
    choices.push_back(
      OverloadChoice(type2, candidate, FunctionRefKind::SingleApply));
  }
  if (choices.empty()) return SolutionKind::Error;
  addOverloadSet(tv, choices, DC, loc);

  // Create a type variable for the argument to the `dynamicallyCall` method.
  auto tvParam = createTypeVariable(loc, TVO_CanBindToNoEscape);
  AnyFunctionType *funcType =
    FunctionType::get({ AnyFunctionType::Param(tvParam) }, func1->getResult());
  addConstraint(ConstraintKind::DynamicCallableApplicableFunction,
                funcType, tv, locator);

  // Get argument type for the `dynamicallyCall` method.
  Type argumentType;
  if (!useKwargsMethod) {
    auto arrayLitProto =
      ctx.getProtocol(KnownProtocolKind::ExpressibleByArrayLiteral);
    addConstraint(ConstraintKind::ConformsTo, tvParam,
                  arrayLitProto->getDeclaredType(), locator);
    auto elementAssocType = arrayLitProto->getAssociatedType(
        ctx.Id_ArrayLiteralElement);
    argumentType = DependentMemberType::get(tvParam, elementAssocType);
  } else {
    auto dictLitProto =
      ctx.getProtocol(KnownProtocolKind::ExpressibleByDictionaryLiteral);
    addConstraint(ConstraintKind::ConformsTo, tvParam,
                  dictLitProto->getDeclaredType(), locator);
    auto valueAssocType = dictLitProto->getAssociatedType(ctx.Id_Value);
    argumentType = DependentMemberType::get(tvParam, valueAssocType);
  }

  // Argument type can default to `Any`.
  addConstraint(ConstraintKind::Defaultable, argumentType,
                ctx.TheAnyType, locator);

  // All dynamic call parameter types must be convertible to the argument type.
  for (auto i : indices(func1->getParams())) {
    auto param = func1->getParams()[i];
    auto paramType = param.getPlainType();
    auto locatorBuilder =
        locator.withPathElement(LocatorPathElt::TupleElement(i));
    addConstraint(ConstraintKind::ArgumentConversion, paramType,
                  argumentType, locatorBuilder);
  }

  return SolutionKind::Solved;
}

static llvm::PointerIntPair<Type, 3, unsigned>
getBaseTypeForPointer(TypeBase *type) {
  unsigned unwrapCount = 0;
  while (auto objectTy = type->getOptionalObjectType()) {
    type = objectTy.getPointer();
    ++unwrapCount;
  }

  auto pointeeTy = type->getAnyPointerElementType();
  assert(pointeeTy);
  return {pointeeTy, unwrapCount};
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
  assert(!type1->isTypeVariableOrMember() && !type2->isTypeVariableOrMember());

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

  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  auto matchPointerBaseTypes = [&](Type baseType1,
                                   Type baseType2) -> SolutionKind {
    if (restriction != ConversionRestrictionKind::PointerToPointer)
      increaseScore(ScoreKind::SK_ValueToPointerConversion);

    auto result =
        matchTypes(baseType1, baseType2, ConstraintKind::BindToPointerType,
                   subflags, locator);

    if (!(result.isFailure() && shouldAttemptFixes()))
      return result;

    BoundGenericType *ptr1 = nullptr;
    BoundGenericType *ptr2 = nullptr;

    switch (restriction) {
    case ConversionRestrictionKind::ArrayToPointer:
    case ConversionRestrictionKind::InoutToPointer: {
      ptr2 = type2->lookThroughAllOptionalTypes()->castTo<BoundGenericType>();
      ptr1 = BoundGenericType::get(ptr2->getDecl(), ptr2->getParent(),
                                   {baseType1});
      break;
    }

    case ConversionRestrictionKind::PointerToPointer:
      ptr1 = type1->castTo<BoundGenericType>();
      ptr2 = type2->castTo<BoundGenericType>();
      break;

    default:
      return SolutionKind::Error;
    }

    auto *fix = GenericArgumentsMismatch::create(*this, ptr1, ptr2, {0},
                                                 getConstraintLocator(locator));
    return recordFix(fix) ? SolutionKind::Error : SolutionKind::Solved;
  };

  switch (restriction) {
  // for $< in { <, <c, <oc }:
  //   T_i $< U_i ===> (T_i...) $< (U_i...)
  case ConversionRestrictionKind::DeepEquality:
    return matchDeepEqualityTypes(type1, type2, locator);

  case ConversionRestrictionKind::Superclass:
    addContextualScore();
    return matchSuperclassTypes(type1, type2, subflags, locator);

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

    assert(matchKind >= ConstraintKind::Subtype);
    if (auto generic1 = type1->getAs<BoundGenericType>()) {
      if (auto generic2 = type2->getAs<BoundGenericType>()) {
        if (generic1->getDecl()->isOptionalDecl() &&
            generic2->getDecl()->isOptionalDecl())
          return matchTypes(generic1->getGenericArgs()[0],
                            generic2->getGenericArgs()[0],
                            matchKind, subflags,
                            locator.withPathElement(
                              LocatorPathElt::GenericArgument(0)));
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
    auto ptr2 = getBaseTypeForPointer(t2);

    increaseScore(SK_ValueToOptional, ptr2.getInt());

    return matchPointerBaseTypes(baseType1, ptr2.getPointer());
  }

  // String ===> UnsafePointer<[U]Int8>
  case ConversionRestrictionKind::StringToPointer: {
    addContextualScore();

    auto ptr2 = getBaseTypeForPointer(type2->getDesugaredType());

    increaseScore(SK_ValueToOptional, ptr2.getInt());

    // The pointer element type must be void or a byte-sized type.
    // TODO: Handle different encodings based on pointer element type, such as
    // UTF16 for [U]Int16 or UTF32 for [U]Int32. For now we only interop with
    // Int8 pointers using UTF8 encoding.
    auto baseType2 = getFixedTypeRecursive(ptr2.getPointer(), false);
    // If we haven't resolved the element type, generate constraints.
    if (baseType2->isTypeVariableOrMember()) {
      if (flags.contains(TMF_GenerateConstraints)) {
        increaseScore(ScoreKind::SK_ValueToPointerConversion);

        auto &ctx = getASTContext();
        auto int8Con = Constraint::create(*this, ConstraintKind::Bind,
                                          baseType2,
                                          TypeChecker::getInt8Type(ctx),
                                          getConstraintLocator(locator));
        auto uint8Con = Constraint::create(*this, ConstraintKind::Bind,
                                           baseType2,
                                           TypeChecker::getUInt8Type(ctx),
                                           getConstraintLocator(locator));
        auto voidCon = Constraint::create(*this, ConstraintKind::Bind,
                                          baseType2, ctx.TheEmptyTupleType,
                                          getConstraintLocator(locator));
        
        Constraint *disjunctionChoices[] = {int8Con, uint8Con, voidCon};
        addDisjunctionConstraint(disjunctionChoices, locator);
        return SolutionKind::Solved;
      }

      return SolutionKind::Unsolved;
    }
    
    if (!isStringCompatiblePointerBaseType(getASTContext(), baseType2)) {
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
    auto ptr2 = getBaseTypeForPointer(t2);

    increaseScore(SK_ValueToOptional, ptr2.getInt());

    return matchPointerBaseTypes(baseType1, ptr2.getPointer());
  }
      
  // T <p U ===> UnsafeMutablePointer<T> <a UnsafeMutablePointer<U>
  case ConversionRestrictionKind::PointerToPointer: {
    auto t1 = type1->getDesugaredType();
    auto t2 = type2->getDesugaredType();

    auto ptr1 = getBaseTypeForPointer(t1);
    auto ptr2 = getBaseTypeForPointer(t2);

    return matchPointerBaseTypes(ptr1.getPointer(), ptr2.getPointer());
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
                          LocatorPathElt::GenericArgument(0)));
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
    auto result =
        matchTypes(key1, key2, subMatchKind, subflags,
                   locator.withPathElement(LocatorPathElt::GenericArgument(0)));
    if (result.isFailure())
      return result;

    switch (matchTypes(
        value1, value2, subMatchKind, subflags,
        locator.withPathElement(LocatorPathElt::GenericArgument(1)))) {
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
                      locator.withPathElement(LocatorPathElt::GenericArgument(0)));
  }

  // T1 <c T2 && T2 : Hashable ===> T1 <c AnyHashable
  case ConversionRestrictionKind::HashableToAnyHashable: {
    // We never want to do this if the LHS is already AnyHashable.
    type1 = simplifyType(type1);
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
      getASTContext().getProtocol(KnownProtocolKind::Hashable);
    if (!hashableProtocol)
      return SolutionKind::Error;

    auto constraintLocator = getConstraintLocator(locator);
    auto tv = createTypeVariable(constraintLocator,
                                 TVO_PrefersSubtypeBinding |
                                 TVO_CanBindToNoEscape);
    
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

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyRestrictedConstraint(
                                       ConversionRestrictionKind restriction,
                                       Type type1, Type type2,
                                       ConstraintKind matchKind,
                                       TypeMatchOptions flags,
                                       ConstraintLocatorBuilder locator) {
  switch (simplifyRestrictedConstraintImpl(restriction, type1, type2,
                                           matchKind, flags, locator)) {
  case SolutionKind::Solved: {
    // If we have an application of a non-ephemeral parameter, then record a
    // fix if we have to treat an ephemeral conversion as non-ephemeral. It's
    // important that this is solved as an independant constraint, as the
    // solving of this restriction may be required in order to evaluate it. For
    // example, when solving `foo(&.x)`, we need to first match types for the
    // inout-to-pointer conversion, which then allows us to resolve the overload
    // of `x`, which may or may not produce an ephemeral pointer.
    if (locator.isNonEphemeralParameterApplication()) {
      bool downgradeToWarning =
          !getASTContext().LangOpts.DiagnoseInvalidEphemeralnessAsError;

      auto *fix = TreatEphemeralAsNonEphemeral::create(
          *this, getConstraintLocator(locator), type1, type2, restriction,
          downgradeToWarning);
      addFixConstraint(fix, matchKind, type1, type2, locator);
    }

    ConstraintRestrictions.push_back(std::make_tuple(type1, type2, restriction));
    return SolutionKind::Solved;
  }
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

bool ConstraintSystem::recordFix(ConstraintFix *fix, unsigned impact) {
  auto &ctx = getASTContext();
  if (ctx.TypeCheckerOpts.DebugConstraintSolver) {
    auto &log = ctx.TypeCheckerDebug->getStream();
    log.indent(solverState ? solverState->depth * 2 + 2 : 0)
      << "(attempting fix ";
    fix->print(log);
    log << ")\n";
  }

  // Record the fix.

  // If this is just a warning, it shouldn't affect the solver. Otherwise,
  // increase the score.
  if (!fix->isWarning())
    increaseScore(SK_Fix, impact);

  // If we've made the current solution worse than the best solution we've seen
  // already, stop now.
  if (worseThanBestSolution())
    return true;

  if (isAugmentingFix(fix)) {
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

void ConstraintSystem::recordPotentialHole(TypeVariableType *typeVar) {
  assert(typeVar);
  typeVar->getImpl().enableCanBindToHole(getSavedBindings());
}

ConstraintSystem::SolutionKind ConstraintSystem::simplifyFixConstraint(
    ConstraintFix *fix, Type type1, Type type2, ConstraintKind matchKind,
    TypeMatchOptions flags, ConstraintLocatorBuilder locator) {

  // Local function to form an unsolved result.
  auto formUnsolved = [&] {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(Constraint::createFixed(
          *this, matchKind, fix, type1, type2, getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }
    return SolutionKind::Unsolved;
  };

  // Try with the fix.
  TypeMatchOptions subflags =
    getDefaultDecompositionOptions(flags) | TMF_ApplyingFix;
  switch (fix->getKind()) {
  case FixKind::ForceOptional: {
    SmallVector<Type, 4> unwraps1;
    type1->lookThroughAllOptionalTypes(unwraps1);

    SmallVector<Type, 4> unwraps2;
    type2->lookThroughAllOptionalTypes(unwraps2);

    auto impact = unwraps1.size() != unwraps2.size()
                      ? unwraps1.size() - unwraps2.size()
                      : 1;
    return recordFix(fix, impact) ? SolutionKind::Error : SolutionKind::Solved;
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

  case FixKind::AutoClosureForwarding: {
    if (recordFix(fix))
      return SolutionKind::Error;
    return matchTypes(type1, type2, matchKind, subflags, locator);
  }

  case FixKind::AllowTupleTypeMismatch: {
    auto lhs = type1->castTo<TupleType>();
    auto rhs = type2->castTo<TupleType>();
    // Create a new tuple type the size of the smaller tuple with elements
    // from the larger tuple whenever either side contains a type variable.
    // For example (A, $0, B, $2) and (X, Y, $1) produces: (X, $0, B).
    // This allows us to guarentee that the types will match, and all
    // type variables will get bound to something as long as we default
    // excess types in the larger tuple to Any. In the prior example,
    // when the tuples (X, Y, $1) and (X, $0, B) get matched, $0 is equated
    // to Y, $1 is equated to B, and $2 is defaulted to Any.
    auto lhsLarger = lhs->getNumElements() >= rhs->getNumElements();
    auto larger = lhsLarger ? lhs : rhs;
    auto smaller = lhsLarger ? rhs : lhs;
    llvm::SmallVector<TupleTypeElt, 4> newTupleTypes;

    for (unsigned i = 0; i < larger->getNumElements(); ++i) {
      auto largerElt = larger->getElement(i);
      if (i < smaller->getNumElements()) {
        auto smallerElt = smaller->getElement(i);
        if (largerElt.getType()->isTypeVariableOrMember() ||
            smallerElt.getType()->isTypeVariableOrMember())
          newTupleTypes.push_back(largerElt);
        else
          newTupleTypes.push_back(smallerElt);
      } else {
        if (largerElt.getType()->isTypeVariableOrMember())
          recordPotentialHole(largerElt.getType()->getAs<TypeVariableType>());
      }
    }
    auto matchingType =
        TupleType::get(newTupleTypes, getASTContext())->castTo<TupleType>();
    if (recordFix(fix))
      return SolutionKind::Error;
    return matchTupleTypes(matchingType, smaller, matchKind, subflags, locator);
  }

  case FixKind::TreatEphemeralAsNonEphemeral: {
    auto *theFix = static_cast<TreatEphemeralAsNonEphemeral *>(fix);
    // If we have a non-ephemeral locator for an ephemeral conversion, make a
    // note of the fix.
    auto conversion = theFix->getConversionKind();
    switch (isConversionEphemeral(conversion, locator)) {
    case ConversionEphemeralness::Ephemeral:
      // Record the fix with an impact of zero. This ensures that non-ephemeral
      // diagnostics don't impact solver behavior.
      if (recordFix(fix, /*impact*/ 0))
        return SolutionKind::Error;

      return SolutionKind::Solved;
    case ConversionEphemeralness::NonEphemeral:
      return SolutionKind::Solved;
    case ConversionEphemeralness::Unresolved:
      // It's possible we don't yet have enough information to know whether
      // the conversion is ephemeral or not, for example if we're dealing with
      // an overload that hasn't yet been resolved.
      return formUnsolved();
    }
  }

  case FixKind::InsertCall:
  case FixKind::RemoveReturn:
  case FixKind::RemoveAddressOf:
  case FixKind::AddMissingArguments:
  case FixKind::SkipUnhandledConstructInFunctionBuilder:
  case FixKind::UsePropertyWrapper:
  case FixKind::UseWrappedValue:
  case FixKind::ExpandArrayIntoVarargs:
  case FixKind::UseValueTypeOfRawRepresentative:
  case FixKind::ExplicitlyConstructRawRepresentable:
  case FixKind::CoerceToCheckedCast: {
    return recordFix(fix) ? SolutionKind::Error : SolutionKind::Solved;
  }

  case FixKind::TreatRValueAsLValue: {
    unsigned impact = 1;
    // If this is an attempt to use result of a function/subscript call as
    // an l-value, it has to have an increased impact because it's either
    // a function - which is completely incorrect, or it's a get-only
    // subscript, which requires changes to declaration to become mutable.
    if (auto last = locator.last()) {
      impact += (last->is<LocatorPathElt::FunctionResult>() ||
                 last->is<LocatorPathElt::SubscriptMember>())
                    ? 1
                    : 0;
    }

    return recordFix(fix, impact) ? SolutionKind::Error : SolutionKind::Solved;
  }

  case FixKind::AddConformance:
  case FixKind::SkipSameTypeRequirement:
  case FixKind::SkipSuperclassRequirement: {
    return recordFix(fix, assessRequirementFailureImpact(*this, type1,
                                                         fix->getLocator()))
               ? SolutionKind::Error
               : SolutionKind::Solved;
  }

  case FixKind::AllowArgumentTypeMismatch: {
    increaseScore(SK_Fix);
    return recordFix(fix) ? SolutionKind::Error : SolutionKind::Solved;
  }

  case FixKind::ContextualMismatch: {
    if (recordFix(fix))
      return SolutionKind::Error;

    // If type produced by expression is a function type
    // with result type matching contextual, it should have
    // been diagnosed as "missing explicit call", let's
    // increase the score to make sure that we don't impede that.
    if (auto *fnType = type1->getAs<FunctionType>()) {
      auto result = matchTypes(fnType->getResult(), type2, matchKind,
                               TMF_ApplyingFix, locator);
      if (result == SolutionKind::Solved)
        increaseScore(SK_Fix);
    }

    return SolutionKind::Solved;
  }

  case FixKind::UseSubscriptOperator:
  case FixKind::ExplicitlyEscaping:
  case FixKind::RelabelArguments:
  case FixKind::RemoveCall:
  case FixKind::RemoveUnwrap:
  case FixKind::DefineMemberBasedOnUse:
  case FixKind::AllowMemberRefOnExistential:
  case FixKind::AllowTypeOrInstanceMember:
  case FixKind::AllowInvalidPartialApplication:
  case FixKind::AllowInvalidInitRef:
  case FixKind::RemoveExtraneousArguments:
  case FixKind::AllowClosureParameterDestructuring:
  case FixKind::MoveOutOfOrderArgument:
  case FixKind::AllowInaccessibleMember:
  case FixKind::AllowAnyObjectKeyPathRoot:
  case FixKind::TreatKeyPathSubscriptIndexAsHashable:
  case FixKind::AllowInvalidRefInKeyPath:
  case FixKind::DefaultGenericArgument:
  case FixKind::GenericArgumentsMismatch:
  case FixKind::AllowMutatingMemberOnRValueBase:
  case FixKind::AllowTupleSplatForSingleParameter:
  case FixKind::AllowInvalidUseOfTrailingClosure:
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

  case ConstraintKind::OpaqueUnderlyingType:
    return simplifyOpaqueUnderlyingTypeConstraint(first, second,
                                                  subflags, locator);

  case ConstraintKind::BridgingConversion:
    return simplifyBridgingConstraint(first, second, subflags, locator);

  case ConstraintKind::ApplicableFunction:
    return simplifyApplicableFnConstraint(first, second, subflags, locator);

  case ConstraintKind::DynamicCallableApplicableFunction:
    return simplifyDynamicCallableApplicableFnConstraint(first, second,
                                                         subflags, locator);

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

  case ConstraintKind::OneWayEqual:
    return simplifyOneWayConstraint(kind, first, second, subflags, locator);

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

  assert((path.size() == 1 &&
          path[0].getKind() == ConstraintLocator::SubscriptMember) ||
         (path.size() == 2 &&
          path[1].getKind() == ConstraintLocator::KeyPathDynamicMember));
  auto indexTuple = dyn_cast<TupleExpr>(subscript->getIndex());
  if (!indexTuple || indexTuple->getNumElements() != 1)
    return;
  
  auto keyPathExpr = dyn_cast<KeyPathExpr>(indexTuple->getElement(0));
  if (!keyPathExpr)
    return;
  
  auto typeVar = getType(keyPathExpr)->getAs<TypeVariableType>();
  if (!typeVar)
    return;

  auto constraints = CG.gatherConstraints(
      typeVar, ConstraintGraph::GatheringKind::EquivalenceClass,
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
ConstraintSystem::addKeyPathConstraint(
    Type keypath,
    Type root, Type value,
    ArrayRef<TypeVariableType *> componentTypeVars,
    ConstraintLocatorBuilder locator,
    bool isFavored) {
  switch (simplifyKeyPathConstraint(keypath, root, value,
                                    componentTypeVars,
                                    TMF_GenerateConstraints,
                                    locator)) {
  case SolutionKind::Error:
    if (shouldAddNewFailingConstraint()) {
      auto c = Constraint::create(*this, ConstraintKind::KeyPath,
                                  keypath, root, value,
                                  getConstraintLocator(locator),
                                  componentTypeVars);
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
    kind = ConstraintKind::Bind;
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

Type ConstraintSystem::addJoinConstraint(
    ConstraintLocator *locator,
    ArrayRef<std::pair<Type, ConstraintLocator *>> inputs) {
  switch (inputs.size()) {
  case 0:
    return Type();

  case 1:
    return inputs.front().first;

  default:
    // Produce the join below.
    break;
  }

  // Create a type variable to capture the result of the join.
  Type resultTy = createTypeVariable(locator,
                                     (TVO_PrefersSubtypeBinding |
                                      TVO_CanBindToNoEscape));

  // Introduce conversions from each input type to the type variable.
  for (const auto &input : inputs) {
    addConstraint(
      ConstraintKind::Conversion, input.first, resultTy, input.second);
  }

  return resultTy;
}

void ConstraintSystem::addFixConstraint(ConstraintFix *fix, ConstraintKind kind,
                                        Type first, Type second,
                                        ConstraintLocatorBuilder locator,
                                        bool isFavored) {
  TypeMatchOptions subflags = TMF_GenerateConstraints;
  switch (simplifyFixConstraint(fix, first, second, kind, subflags, locator)) {
  case SolutionKind::Error:
    // Add a failing constraint, if needed.
    if (shouldAddNewFailingConstraint()) {
      auto c = Constraint::createFixed(*this, kind, fix, first, second,
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
  case ConstraintKind::OperatorArgumentConversion:
  case ConstraintKind::OpaqueUnderlyingType: {
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

  case ConstraintKind::DynamicCallableApplicableFunction:
    return simplifyDynamicCallableApplicableFnConstraint(
      constraint.getFirstType(), constraint.getSecondType(), None,
      constraint.getLocator());

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
      constraint.getThirdType(), constraint.getTypeVariables(),
      None, constraint.getLocator());

  case ConstraintKind::KeyPathApplication:
    return simplifyKeyPathApplicationConstraint(
      constraint.getFirstType(), constraint.getSecondType(),
      constraint.getThirdType(),
      None, constraint.getLocator());

  case ConstraintKind::BindOverload:
    if (auto *fix = constraint.getFix()) {
      if (recordFix(fix))
        return SolutionKind::Error;
    }

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

  case ConstraintKind::OneWayEqual:
    return simplifyOneWayConstraint(constraint.getKind(),
                                    constraint.getFirstType(),
                                    constraint.getSecondType(),
                                    TMF_GenerateConstraints,
                                    constraint.getLocator());
  }

  llvm_unreachable("Unhandled ConstraintKind in switch.");
}

void ConstraintSystem::simplifyDisjunctionChoice(Constraint *choice) {
  // Simplify this term in the disjunction.
  switch (simplifyConstraint(*choice)) {
  case ConstraintSystem::SolutionKind::Error:
    if (!failedConstraint)
      failedConstraint = choice;
    if (solverState)
      solverState->retireConstraint(choice);
    break;

  case ConstraintSystem::SolutionKind::Solved:
    if (solverState)
      solverState->retireConstraint(choice);
    break;

  case ConstraintSystem::SolutionKind::Unsolved:
    InactiveConstraints.push_back(choice);
    CG.addConstraint(choice);
    break;
  }

  // Record this as a generated constraint.
  if (solverState)
    solverState->addGeneratedConstraint(choice);
}
