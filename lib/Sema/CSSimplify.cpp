//===--- CSSimplify.cpp - Constraint Simplification -----------------------===//
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
// This file implements simplifications of constraints within the constraint
// system.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "swift/AST/ParameterList.h"
#include "swift/Basic/StringExtras.h"
#include "swift/ClangImporter/ClangModule.h"
#include "llvm/Support/Compiler.h"

using namespace swift;
using namespace constraints;

MatchCallArgumentListener::~MatchCallArgumentListener() { }

void MatchCallArgumentListener::extraArgument(unsigned argIdx) { }

void MatchCallArgumentListener::missingArgument(unsigned paramIdx) { }

void MatchCallArgumentListener::missingLabel(unsigned paramIdx) {}

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

  // Only allow about one typo for every two properly-typed characters, which
  // prevents completely-wacky suggestions in many cases.
  if (dist > (argName.size() + 1) / 3)
    return None;

  return dist;
}

bool constraints::
areConservativelyCompatibleArgumentLabels(ValueDecl *decl,
                                          unsigned parameterDepth,
                                          ArrayRef<Identifier> labels,
                                          bool hasTrailingClosure) {
  // Bail out conservatively if this isn't a function declaration.
  auto fn = dyn_cast<AbstractFunctionDecl>(decl);
  if (!fn) return true;
  assert(parameterDepth < fn->getNumParameterLists());

  ParameterList &params = *fn->getParameterList(parameterDepth);

  SmallVector<CallArgParam, 8> argInfos;
  for (auto argLabel : labels) {
    argInfos.push_back(CallArgParam());
    argInfos.back().Label = argLabel;
  }

  SmallVector<CallArgParam, 8> paramInfos;
  for (auto param : params) {
    paramInfos.push_back(CallArgParam());
    paramInfos.back().Label = param->getArgumentName();
    paramInfos.back().HasDefaultArgument = param->isDefaultArgument();
    paramInfos.back().parameterFlags = ParameterTypeFlags::fromParameterType(
        param->getInterfaceType(), param->isVariadic());
  }

  MatchCallArgumentListener listener;
  SmallVector<ParamBinding, 8> unusedParamBindings;

  return !matchCallArguments(argInfos, paramInfos, hasTrailingClosure,
                             /*allow fixes*/ false,
                             listener, unusedParamBindings);
}

/// Determine the default type-matching options to use when decomposing a
/// constraint into smaller constraints.
static ConstraintSystem::TypeMatchOptions getDefaultDecompositionOptions(
         ConstraintSystem::TypeMatchOptions flags) {
  return flags | ConstraintSystem::TMF_GenerateConstraints;
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
    if (nextArgIdx != numArgs && !args[nextArgIdx].hasLabel() &&
       ignoreNameMismatch) {
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
    if (param.isVariadic()) {
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
      for (paramIdx = 0; paramIdx != numParams; ++paramIdx) {
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
      if (param.isVariadic())
        continue;

      // Parameters with defaults can be unfulfilled.
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
    for (paramIdx = 0; paramIdx != numParams; ++paramIdx) {
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

      unsigned prevArgIdx = parameterBindings[prevParamIdx].front();

      // First let's double check if out-of-order argument is nothing
      // more than a simple label mismatch, because in situation where
      // one argument requires label and another one doesn't, but caller
      // doesn't provide either, problem is going to be identified as
      // out-of-order argument instead of label mismatch.
      auto &parameter = params[prevArgIdx];
      if (parameter.hasLabel()) {
        auto expectedLabel = parameter.Label;
        auto argumentLabel = args[argIdx].Label;

        // If there is a label but it's incorrect it can only mean
        // situation like this: expected (x, _ y) got (y, _ x).
        if (argumentLabel.empty() ||
            (expectedLabel.compare(argumentLabel) != 0 &&
             args[prevArgIdx].Label.empty())) {
          listener.missingLabel(prevArgIdx);
          return true;
        }
      }

      listener.outOfOrderArgument(argIdx, prevArgIdx);
      return true;
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
static std::tuple<ValueDecl *, unsigned, ArrayRef<Identifier>, bool>
getCalleeDeclAndArgs(ConstraintSystem &cs,
                     ConstraintLocatorBuilder callLocator,
                     SmallVectorImpl<Identifier> &argLabelsScratch) {
  ArrayRef<Identifier> argLabels;
  bool hasTrailingClosure = false;

  // Break down the call.
  SmallVector<LocatorPathElt, 2> path;
  auto callExpr = callLocator.getLocatorParts(path);
  if (!callExpr) 
    return std::make_tuple(nullptr, 0, argLabels, hasTrailingClosure);

  // Our remaining path can only be 'ApplyArgument' or 'SubscriptIndex'.
  if (!path.empty() &&
      !(path.size() == 1 &&
        (path.back().getKind() == ConstraintLocator::ApplyArgument ||
         path.back().getKind() == ConstraintLocator::SubscriptIndex)))
    return std::make_tuple(nullptr, 0, argLabels, hasTrailingClosure);

  // Dig out the callee.
  Expr *calleeExpr;
  if (auto call = dyn_cast<CallExpr>(callExpr)) {
    calleeExpr = call->getDirectCallee();
    argLabels = call->getArgumentLabels();
    hasTrailingClosure = call->hasTrailingClosure();
  } else if (auto unresolved = dyn_cast<UnresolvedMemberExpr>(callExpr)) {
    calleeExpr = callExpr;
    argLabels = unresolved->getArgumentLabels();
    hasTrailingClosure = unresolved->hasTrailingClosure();
  } else if (auto subscript = dyn_cast<SubscriptExpr>(callExpr)) {
    calleeExpr = callExpr;
    argLabels = subscript->getArgumentLabels();
    hasTrailingClosure = subscript->hasTrailingClosure();
  } else if (auto dynSubscript = dyn_cast<DynamicSubscriptExpr>(callExpr)) {
    calleeExpr = callExpr;
    argLabels = dynSubscript->getArgumentLabels();
    hasTrailingClosure = dynSubscript->hasTrailingClosure();
  } else {
    if (auto apply = dyn_cast<ApplyExpr>(callExpr)) {
      argLabels = apply->getArgumentLabels(argLabelsScratch);
      assert(!apply->hasTrailingClosure());
    } else if (auto objectLiteral = dyn_cast<ObjectLiteralExpr>(callExpr)) {
      argLabels = objectLiteral->getArgumentLabels();
      hasTrailingClosure = objectLiteral->hasTrailingClosure();
    }
    return std::make_tuple(nullptr, 0, argLabels, hasTrailingClosure);
  }

  // Determine the target locator.
  // FIXME: Check whether the callee is of an expression kind that
  // could describe a declaration. This is an optimization.
  ConstraintLocator *targetLocator = cs.getConstraintLocator(calleeExpr);

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
    return std::make_tuple(nullptr, 0, argLabels, hasTrailingClosure);

  // If there's a declaration, return it.
  if (choice->isDecl()) {
    auto decl = choice->getDecl();
    unsigned level = 0;
    if (decl->getDeclContext()->isTypeContext()) {
      if (auto function = dyn_cast<AbstractFunctionDecl>(decl)) {
        // References to instance members on a metatype stay at level 0.
        // Everything else is level 1.
        if (!(function->isInstanceMember() &&
              cs.getFixedTypeRecursive(choice->getBaseType(),
                                       /*wantRValue=*/true)
                ->is<AnyMetatypeType>()))
          level = 1;
      } else if (isa<SubscriptDecl>(decl)) {
        // Subscript level 1 == the indices.
        level = 1;
      }
    }

    return std::make_tuple(decl, level, argLabels, hasTrailingClosure);
  }

  return std::make_tuple(nullptr, 0, argLabels, hasTrailingClosure);
}

// Match the argument of a call to the parameter.
static ConstraintSystem::SolutionKind
matchCallArguments(ConstraintSystem &cs, ConstraintKind kind,
                   Type argType, Type paramType,
                   ConstraintLocatorBuilder locator) {

  if (paramType->isAny()) {
    if (argType->is<InOutType>())
      return ConstraintSystem::SolutionKind::Error;

    // If the param type is Any, the function can only have one argument.
    // Check if exactly one argument was passed to this function, otherwise
    // we obviously have a mismatch.
    if (auto tupleArgType = dyn_cast<TupleType>(argType.getPointer())) {
      // Total hack: In Swift 3 mode, argument labels are ignored when calling
      // function type with a single Any parameter.
      if (tupleArgType->getNumElements() != 1 ||
          (!cs.getASTContext().isSwiftVersion3() &&
           tupleArgType->getElement(0).hasName())) {
        return ConstraintSystem::SolutionKind::Error;
      }
    }
    return ConstraintSystem::SolutionKind::Solved;
  }

  // Extract the parameters.
  ValueDecl *callee;
  unsigned calleeLevel;
  ArrayRef<Identifier> argLabels;
  SmallVector<Identifier, 2> argLabelsScratch;
  bool hasTrailingClosure = false;
  std::tie(callee, calleeLevel, argLabels, hasTrailingClosure) =
    getCalleeDeclAndArgs(cs, locator, argLabelsScratch);
  auto params = decomposeParamType(paramType, callee, calleeLevel);

  if (callee && cs.getASTContext().isSwiftVersion3()
      && argType->is<TupleType>()) {
    // Hack: In Swift 3 mode, accept `foo(x, y)` for `foo((x, y))` when the
    // callee is a function-typed property or an enum constructor whose
    // argument is a single unlabeled type parameter.
    if (auto *prop = dyn_cast<VarDecl>(callee)) {
      auto *fnType = prop->getInterfaceType()->getAs<AnyFunctionType>();
      if (fnType && fnType->getInput()->isTypeParameter())
        argType = ParenType::get(cs.getASTContext(), argType);
    } else if (auto *enumCtor = dyn_cast<EnumElementDecl>(callee)) {
      if (enumCtor->getArgumentInterfaceType()->isTypeParameter())
        argType = ParenType::get(cs.getASTContext(), argType);
    }
  }

  // Extract the arguments.
  auto args = decomposeArgType(argType, argLabels);

  // Match up the call arguments to the parameters.
  MatchCallArgumentListener listener;
  SmallVector<ParamBinding, 4> parameterBindings;
  if (constraints::matchCallArguments(args, params, hasTrailingClosure,
                                      cs.shouldAttemptFixes(), listener,
                                      parameterBindings))
    return ConstraintSystem::SolutionKind::Error;

  // Check the argument types for each of the parameters.
  ConstraintSystem::TypeMatchOptions subflags =
    ConstraintSystem::TMF_GenerateConstraints;
  ConstraintKind subKind;
  switch (kind) {
  case ConstraintKind::ArgumentTupleConversion:
    subKind = ConstraintKind::ArgumentConversion;
    break;

  case ConstraintKind::OperatorArgumentTupleConversion:
    subKind = ConstraintKind::OperatorArgumentConversion;
    break;

  case ConstraintKind::Conversion:
  case ConstraintKind::BridgingConversion:
  case ConstraintKind::OperatorArgumentConversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::Bind:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Equal:
  case ConstraintKind::Subtype:
  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::BindOverload:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::Layout:
  case ConstraintKind::Defaultable:
  case ConstraintKind::Disjunction:
  case ConstraintKind::DynamicTypeOf:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OpenedExistentialOf:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::OptionalObject:
  case ConstraintKind::SelfObjectOfProtocol:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::ValueMember:
    llvm_unreachable("Not a call argument constraint");
  }
  
  auto haveOneNonUserConversion =
          (subKind != ConstraintKind::OperatorArgumentConversion);
  
  
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
                                  ConstraintKind kind, TypeMatchOptions flags,
                                  ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  // Equality and subtyping have fairly strict requirements on tuple matching,
  // requiring element names to either match up or be disjoint.
  if (kind < ConstraintKind::Conversion) {
    if (tuple1->getNumElements() != tuple2->getNumElements())
      return SolutionKind::Error;

    for (unsigned i = 0, n = tuple1->getNumElements(); i != n; ++i) {
      const auto &elt1 = tuple1->getElement(i);
      const auto &elt2 = tuple2->getElement(i);

      // If the names don't match, we may have a conflict.
      if (elt1.getName() != elt2.getName()) {
        // Same-type requirements require exact name matches.
        if (kind <= ConstraintKind::Equal)
          return SolutionKind::Error;

        // For subtyping constraints, just make sure that this name isn't
        // used at some other position.
        if (elt2.hasName() && tuple1->getNamedElementId(elt2.getName()) != -1)
          return SolutionKind::Error;
      }

      // Variadic bit must match.
      if (elt1.isVararg() != elt2.isVararg())
        return SolutionKind::Error;

      // Compare the element types.
      switch (matchTypes(elt1.getType(), elt2.getType(), kind, subflags,
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

  assert(kind >= ConstraintKind::Conversion);
  ConstraintKind subKind;
  switch (kind) {
  case ConstraintKind::ArgumentTupleConversion:
    subKind = ConstraintKind::ArgumentConversion;
    break;

  case ConstraintKind::OperatorArgumentTupleConversion:
    subKind = ConstraintKind::OperatorArgumentConversion;
    break;

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
  case ConstraintKind::Layout:
  case ConstraintKind::Defaultable:
  case ConstraintKind::Disjunction:
  case ConstraintKind::DynamicTypeOf:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OpenedExistentialOf:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::OptionalObject:
  case ConstraintKind::SelfObjectOfProtocol:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::ValueMember:
  case ConstraintKind::BridgingConversion:
    llvm_unreachable("Not a conversion");
  }

  // Compute the element shuffles for conversions.
  SmallVector<int, 16> sources;
  SmallVector<unsigned, 4> variadicArguments;
  if (computeTupleShuffle(tuple1, tuple2, sources, variadicArguments))
    return SolutionKind::Error;

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
    switch (matchTypes(elt1.getType(), elt2.getType(), subKind, subflags,
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
  if (hasVariadic) {
    const auto &elt2 = tuple2->getElements()[variadicIdx];
    auto eltType2 = elt2.getVarargBaseTy();

    for (unsigned idx1 : variadicArguments) {
      switch (matchTypes(tuple1->getElementType(idx1), eltType2, subKind,
                         subflags,
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
                                          ConstraintKind kind, TypeMatchOptions flags,
                                          ConstraintLocatorBuilder locator) {
  int scalarFieldIdx = tuple2->getElementForScalarInit();
  assert(scalarFieldIdx >= 0 && "Invalid tuple for scalar-to-tuple");
  const auto &elt = tuple2->getElement(scalarFieldIdx);
  auto scalarFieldTy = elt.isVararg()? elt.getVarargBaseTy() : elt.getType();
  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);
  return matchTypes(type1, scalarFieldTy, kind, subflags,
                    locator.withPathElement(ConstraintLocator::ScalarToTuple));
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
  case ConstraintKind::ArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentConversion:
  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::BindOverload:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::Layout:
  case ConstraintKind::Defaultable:
  case ConstraintKind::Disjunction:
  case ConstraintKind::DynamicTypeOf:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OpenedExistentialOf:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::OptionalObject:
  case ConstraintKind::SelfObjectOfProtocol:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::ValueMember:
    return false;
  }

  llvm_unreachable("Unhandled ConstraintKind in switch.");
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchFunctionTypes(FunctionType *func1, FunctionType *func2,
                                     ConstraintKind kind, TypeMatchOptions flags,
                                     ConstraintLocatorBuilder locator) {
  // An @autoclosure function type can be a subtype of a
  // non-@autoclosure function type.
  if (func1->isAutoClosure() != func2->isAutoClosure() &&
      kind < ConstraintKind::Subtype)
    return SolutionKind::Error;
  
  // A non-throwing function can be a subtype of a throwing function.
  if (func1->throws() != func2->throws()) {
    // Cannot drop 'throws'.
    if (func1->throws() || (func2->throws() && kind < ConstraintKind::Subtype))
      return SolutionKind::Error;
  }

  // A non-@noescape function type can be a subtype of a @noescape function
  // type.
  if (func1->isNoEscape() != func2->isNoEscape() &&
      (func1->isNoEscape() || kind < ConstraintKind::Subtype))
    return SolutionKind::Error;

  if (matchFunctionRepresentations(func1->getExtInfo().getRepresentation(),
                                   func2->getExtInfo().getRepresentation(),
                                   kind)) {
    return SolutionKind::Error;
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
  case ConstraintKind::ArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentConversion:
    subKind = ConstraintKind::Subtype;
    break;

  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::BindOverload:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::Layout:
  case ConstraintKind::Defaultable:
  case ConstraintKind::Disjunction:
  case ConstraintKind::DynamicTypeOf:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OpenedExistentialOf:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::OptionalObject:
  case ConstraintKind::SelfObjectOfProtocol:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::ValueMember:
  case ConstraintKind::BridgingConversion:
    llvm_unreachable("Not a relational constraint");
  }

  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  increaseScore(ScoreKind::SK_FunctionConversion);

  // Input types can be contravariant (or equal).
  SolutionKind result = matchTypes(func2->getInput(), func1->getInput(),
                                   subKind, subflags,
                                   locator.withPathElement(
                                     ConstraintLocator::FunctionArgument));
  if (result == SolutionKind::Error)
    return SolutionKind::Error;

  // Result type can be covariant (or equal).
  return matchTypes(func1->getResult(), func2->getResult(), subKind,
                     subflags,
                     locator.withPathElement(
                       ConstraintLocator::FunctionResult));
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchSuperclassTypes(Type type1, Type type2,
                                       ConstraintKind kind,
                                       TypeMatchOptions flags,
                                       ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  auto classDecl2 = type2->getClassOrBoundGenericClass();
  bool done = false;
  for (auto super1 = TC.getSuperClassOf(type1);
       !done && super1;
       super1 = TC.getSuperClassOf(super1)) {
    if (super1->getClassOrBoundGenericClass() != classDecl2)
      continue;

    return matchTypes(super1, type2, ConstraintKind::Equal,
                      subflags, locator);
  }

  return SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchDeepEqualityTypes(Type type1, Type type2,
                                         ConstraintLocatorBuilder locator) {
  TypeMatchOptions subflags = TMF_GenerateConstraints;

  // Handle nominal types that are not directly generic.
  if (auto nominal1 = type1->getAs<NominalType>()) {
    auto nominal2 = type2->castTo<NominalType>();

    assert((bool)nominal1->getParent() == (bool)nominal2->getParent() &&
           "Mismatched parents of nominal types");

    if (!nominal1->getParent())
      return SolutionKind::Solved;

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
    switch (matchTypes(bound1->getParent(), bound2->getParent(),
                       ConstraintKind::Equal, subflags,
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
  if (args1.size() != args2.size()) {
    return SolutionKind::Error;
  }
  for (unsigned i = 0, n = args1.size(); i != n; ++i) {
    switch (matchTypes(args1[i], args2[i], ConstraintKind::Equal,
                       subflags,
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
                                        ConstraintKind kind,
                                        TypeMatchOptions flags,
                                        ConstraintLocatorBuilder locator) {
  // FIXME: Fees like a hack.
  if (type1->is<InOutType>())
    return SolutionKind::Error;

  // Conformance to 'Any' always holds.
  if (type2->isAny())
    return SolutionKind::Solved;

  // If the first type is a type variable or member thereof, there's nothing
  // we can do now.
  if (type1->isTypeVariableOrMember()) {
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::createRestricted(*this, kind,
                                     ConversionRestrictionKind::Existential,
                                     type1, type2,
                                     getConstraintLocator(locator)));
      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;
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

  if (!type2->isAnyExistentialType())
    return SolutionKind::Error;

  SmallVector<ProtocolDecl *, 4> protocols;
  type2->getAnyExistentialTypeProtocols(protocols);

  for (auto proto : protocols) {
    switch (simplifyConformsToConstraint(type1, proto, kind, locator,
                                         subflags)) {
      case SolutionKind::Solved:
      case SolutionKind::Unsolved:
        break;

      case SolutionKind::Error:
        return SolutionKind::Error;
    }
  }

  return SolutionKind::Solved;
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

/// Determine whether this is an implicitly unwrapped optional type.
static OptionalTypeKind classifyAsOptionalType(Type type) {
  if (auto boundGeneric = type->getAs<BoundGenericType>())
    return boundGeneric->getDecl()->classifyAsOptionalType();

  return OTK_None;
}

/// Determine whether the first type with the given number of optionals
/// is potentially more optional than the second type with its number of
/// optionals.
static bool isPotentiallyMoreOptionalThan(Type objType1,
                                          unsigned numOptionals1,
                                          Type objType2,
                                          unsigned numOptionals2) {
  if (numOptionals1 <= numOptionals2 && !objType1->isTypeVariableOrMember())
    return false;

  return true;
}

/// Enumerate all of the applicable optional conversion restrictions
static void enumerateOptionalConversionRestrictions(
                    Type type1, Type type2,
                    ConstraintKind kind, ConstraintLocatorBuilder locator,
                    llvm::function_ref<void(ConversionRestrictionKind)> fn) {
  SmallVector<Type, 2> optionals1;
  Type objType1 = type1->lookThroughAllAnyOptionalTypes(optionals1);

  SmallVector<Type, 2> optionals2;
  Type objType2 = type2->lookThroughAllAnyOptionalTypes(optionals2);

  if (optionals1.empty() && optionals2.empty())
    return;

  // Optional-to-optional.
  if (!optionals1.empty() && !optionals2.empty()) {
    auto optionalKind1 = classifyAsOptionalType(optionals1.front());
    auto optionalKind2 = classifyAsOptionalType(optionals2.front());

    // Break cyclic conversions between T? and U! by only allowing it for
    // conversion constraints.
    if (kind >= ConstraintKind::Conversion ||
        locator.isFunctionConversion() ||
        !(optionalKind1 == OTK_Optional &&
          optionalKind2 == OTK_ImplicitlyUnwrappedOptional))
      fn(ConversionRestrictionKind::OptionalToOptional);
  }

  // Inject a value into an optional.
  if (isPotentiallyMoreOptionalThan(objType2, optionals2.size(),
                                    objType1, optionals1.size())) {
    fn(ConversionRestrictionKind::ValueToOptional);
  }

  // Unwrap an implicitly-unwrapped optional.
  if (!optionals1.empty() &&
      classifyAsOptionalType(optionals1.front())
        == OTK_ImplicitlyUnwrappedOptional &&
      kind >= ConstraintKind::Conversion &&
      isPotentiallyMoreOptionalThan(objType1, optionals1.size(),
                                    objType2, optionals2.size())) {
    fn(ConversionRestrictionKind::ForceUnchecked);
  }
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchTypes(Type type1, Type type2, ConstraintKind kind,
                             TypeMatchOptions flags,
                             ConstraintLocatorBuilder locator) {
  bool isArgumentTupleConversion
          = kind == ConstraintKind::ArgumentTupleConversion ||
            kind == ConstraintKind::OperatorArgumentTupleConversion;

  // If we're doing an argument tuple conversion, or just matching the input
  // types of two function types, we have to be careful to preserve
  // ParenType sugar.
  bool isArgumentTupleMatch = isArgumentTupleConversion;
  bool isSwiftVersion3 = getASTContext().isSwiftVersion3();

  // ... but not in Swift 3 mode, where this behavior was broken.
  if (!isSwiftVersion3)
    if (auto elt = locator.last())
      if (elt->getKind() == ConstraintLocator::FunctionArgument)
        isArgumentTupleMatch = true;

  // If we have type variables that have been bound to fixed types, look through
  // to the fixed type.
  type1 = getFixedTypeRecursive(type1, flags, kind == ConstraintKind::Equal,
                                isArgumentTupleMatch);
  type2 = getFixedTypeRecursive(type2, flags, kind == ConstraintKind::Equal,
                                isArgumentTupleMatch);

  auto desugar1 = type1->getDesugaredType();
  auto desugar2 = type2->getDesugaredType();
  TypeVariableType *typeVar1, *typeVar2;
  if (isArgumentTupleMatch &&
      !isSwiftVersion3) {
    typeVar1 = dyn_cast<TypeVariableType>(type1.getPointer());
    typeVar2 = dyn_cast<TypeVariableType>(type2.getPointer());

    // If the types are obviously equivalent, we're done.
    if (type1.getPointer() == type2.getPointer())
      return SolutionKind::Solved;
  } else {
    typeVar1 = desugar1->getAs<TypeVariableType>();
    typeVar2 = desugar2->getAs<TypeVariableType>();

    // If the types are obviously equivalent, we're done.
    if (desugar1->isEqual(desugar2))
      return SolutionKind::Solved;
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
      return SolutionKind::Solved;
    }

    return SolutionKind::Unsolved;
  };

  // If either (or both) types are type variables, unify the type variables.
  if (typeVar1 || typeVar2) {
    switch (kind) {
    case ConstraintKind::Bind:
    case ConstraintKind::BindToPointerType:
    case ConstraintKind::Equal: {
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
              != rep2->getImpl().canBindToLValue())
          return formUnsolvedResult();

        // Merge the equivalence classes corresponding to these two variables.
        mergeEquivalenceClasses(rep1, rep2);
        return SolutionKind::Solved;
      }

      // Provide a fixed type for the type variable.
      bool wantRvalue = kind == ConstraintKind::Equal;
      if (typeVar1) {
        // Simplify the right-hand type and perform the "occurs" check.
        typeVar1 = getRepresentative(typeVar1);
        type2 = simplifyType(type2, flags);
        if (typeVarOccursInType(typeVar1, type2))
          return formUnsolvedResult();

        // If we want an rvalue, get the rvalue.
        if (wantRvalue)
          type2 = type2->getRValueType();

        // If the left-hand type variable cannot bind to an lvalue,
        // but we still have an lvalue, fail.
        if (!typeVar1->getImpl().canBindToLValue() &&
            type2->isLValueType())
          return SolutionKind::Error;

        // Okay. Bind below.

        // Check whether the type variable must be bound to a materializable
        // type.
        if (typeVar1->getImpl().mustBeMaterializable()) {
          if (!type2->isMaterializable())
            return SolutionKind::Error;

          setMustBeMaterializableRecursive(type2);
        }

        // A constraint that binds any pointer to a void pointer is
        // ineffective, since any pointer can be converted to a void pointer.
        if (kind == ConstraintKind::BindToPointerType && type2->isVoid()) {
          // Bind type1 to Void only as a last resort.
          addConstraint(ConstraintKind::Defaultable, typeVar1, type2,
                        getConstraintLocator(locator));
          return SolutionKind::Solved;
        }

        assignFixedType(typeVar1, type2);

        return SolutionKind::Solved;
      }

      // Simplify the left-hand type and perform the "occurs" check.
      typeVar2 = getRepresentative(typeVar2);
      type1 = simplifyType(type1, flags);
      if (typeVarOccursInType(typeVar2, type1))
        return formUnsolvedResult();

      // If we want an rvalue, get the rvalue.
      if (wantRvalue)
        type1 = type1->getRValueType();

      if (!typeVar2->getImpl().canBindToLValue() &&
          type1->isLValueType()) {
        return SolutionKind::Error;
        
        // Okay. Bind below.
      }

      assignFixedType(typeVar2, type1);
      return SolutionKind::Solved;
    }

    case ConstraintKind::BindParam: {
      if (typeVar2 && !typeVar1) {
        // Simplify the left-hand type and perform the "occurs" check.
        typeVar2 = getRepresentative(typeVar2);
        type1 = simplifyType(type1, flags);
        if (typeVarOccursInType(typeVar2, type1))
          return formUnsolvedResult();

        if (auto *iot = type1->getAs<InOutType>()) {
          assignFixedType(typeVar2, LValueType::get(iot->getObjectType()));
        } else {
          assignFixedType(typeVar2, type1);
        }
        return SolutionKind::Solved;
      } else if (typeVar1 && !typeVar2) {
        // Simplify the right-hand type and perform the "occurs" check.
        typeVar1 = getRepresentative(typeVar1);
        type2 = simplifyType(type2, flags);
        if (typeVarOccursInType(typeVar1, type2))
          return formUnsolvedResult();

        if (auto *lvt = type2->getAs<LValueType>()) {
          assignFixedType(typeVar1, InOutType::get(lvt->getObjectType()));
        } else {
          assignFixedType(typeVar1, type2);
        }
        return SolutionKind::Solved;
      } else if (typeVar1 && typeVar2) {
        auto rep1 = getRepresentative(typeVar1);
        auto rep2 = getRepresentative(typeVar2);
        if (rep1 == rep2) {
          return SolutionKind::Solved;
        }
      }

      return formUnsolvedResult();
    }

    case ConstraintKind::ArgumentTupleConversion:
    case ConstraintKind::Conversion:
      if (typeVar1 && typeVar2) {
        auto rep1 = getRepresentative(typeVar1);
        auto rep2 = getRepresentative(typeVar2);
        if (rep1 == rep2) {
          // We already merged these two types, so this constraint is
          // trivially solved.
          return SolutionKind::Solved;
        }
      }
      LLVM_FALLTHROUGH;

    case ConstraintKind::Subtype:
      // Subtype constraints are subject for edge contraction,
      // which is inappropriate in this case, because it's going to
      // erase/lose 'inout' modifier after merging equivalence classes
      // (if inout constraints type var, see ConstraintGraph::contractEdges()),
      // since right-hand side type variable must not be materializable
      // it can simply get left-hand side as a fixed binding, otherwise fail.
      if (type1->is<InOutType>() &&
          type1->getInOutObjectType()->isTypeVariableOrMember() && typeVar2) {
        // Left-hand side type is not materializable, so we need to
        // check if it's even appropriate to have such a constraint
        // between these two types, or fail early otherwise if right-hand
        // side must be materializable.
        if (typeVar2->getImpl().mustBeMaterializable())
          return SolutionKind::Error;

        // Constraints like `inout T0 subtype T1` where (T0 must be
        // materializable) are created when closures are part of the generic
        // function parameters e.g. `func foo<T>(_ t: T, (inout T) -> Void) {}`
        // so when such function gets called e.g.
        // ```
        //  var x = 42
        //  foo(x) { $0 = 0 }
        // ```
        // it's going to try and map closure parameters type (inout T0), where
        // T0 is opened generic parameter T, to argument type (T1), which can
        // be 'inout' but it's uncertain at this stage, but since closure
        // 'declaration' `{ $0 = 0 }` is wrapped inside of a function call,
        // it has to 'map' parameters to arguments instead of converting them,
        // see `ConstraintSystem::matchFunctionTypes`.
        assignFixedType(typeVar2, type1);
        return SolutionKind::Solved;
      }
      LLVM_FALLTHROUGH;

    case ConstraintKind::ArgumentConversion:
    case ConstraintKind::OperatorArgumentTupleConversion:
    case ConstraintKind::OperatorArgumentConversion:
      // We couldn't solve this constraint. If only one of the types is a type
      // variable, perhaps we can do something with it below.
      if (typeVar1 && typeVar2) {
        if (typeVar1 == typeVar2) return SolutionKind::Solved;

        return formUnsolvedResult();
      }
        
      break;

    case ConstraintKind::ApplicableFunction:
    case ConstraintKind::BindOverload:
    case ConstraintKind::BridgingConversion:
    case ConstraintKind::CheckedCast:
    case ConstraintKind::ConformsTo:
    case ConstraintKind::Layout:
    case ConstraintKind::Defaultable:
    case ConstraintKind::Disjunction:
    case ConstraintKind::DynamicTypeOf:
    case ConstraintKind::EscapableFunctionOf:
    case ConstraintKind::OpenedExistentialOf:
    case ConstraintKind::LiteralConformsTo:
    case ConstraintKind::OptionalObject:
    case ConstraintKind::SelfObjectOfProtocol:
    case ConstraintKind::UnresolvedValueMember:
    case ConstraintKind::ValueMember:
      llvm_unreachable("Not a relational constraint");
    }
  }

  // If this is an argument conversion, handle it directly. The rules are
  // different from normal conversions.
  if (kind == ConstraintKind::ArgumentTupleConversion ||
      kind == ConstraintKind::OperatorArgumentTupleConversion) {
    if (!typeVar2) {
      return ::matchCallArguments(*this, kind, type1, type2, locator);
    }

    return formUnsolvedResult();
  }

  if (isArgumentTupleMatch &&
      !isSwiftVersion3) {
    if (!typeVar1 && !typeVar2) {
      if (isa<ParenType>(type1.getPointer()) !=
          isa<ParenType>(type2.getPointer())) {
        return SolutionKind::Error;
      }
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
        return SolutionKind::Solved;
      }

      return SolutionKind::Error;

    case TypeKind::Error:
    case TypeKind::Unresolved:
      return SolutionKind::Error;

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
      if (isa<MetatypeType>(desugar1) &&
          kind != ConstraintKind::Equal &&
          meta1->getInstanceType()->mayHaveSuperclass() &&
          meta2->getInstanceType()->getClassOrBoundGenericClass())
        subKind = std::min(kind, ConstraintKind::Subtype);
      // P.Type < Q.Type if P < Q, both P and Q are protocols, and P.Type
      // and Q.Type are both existential metatypes.
      else if (isa<ExistentialMetatypeType>(meta1)
          && isa<ExistentialMetatypeType>(meta2))
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
      if (!func1->isAutoClosure() && func2->isAutoClosure())
        break;
      return matchFunctionTypes(func1, func2, kind, flags, locator);
    }

    case TypeKind::GenericFunction:
      llvm_unreachable("Polymorphic function type should have been opened");

    case TypeKind::ProtocolComposition:
      // Existential types handled below.
      break;

    case TypeKind::LValue:
      if (kind == ConstraintKind::BindParam)
        return SolutionKind::Error;
      return matchTypes(cast<LValueType>(desugar1)->getObjectType(),
                        cast<LValueType>(desugar2)->getObjectType(),
                        ConstraintKind::Equal, subflags,
                        locator.withPathElement(
                          ConstraintLocator::ArrayElementType));
    
    case TypeKind::InOut:
      // If the RHS is an inout type, the LHS must be an @lvalue type.
      if (kind == ConstraintKind::BindParam ||
          kind >= ConstraintKind::OperatorArgumentConversion)
        return SolutionKind::Error;
      
      return matchTypes(cast<InOutType>(desugar1)->getObjectType(),
                        cast<InOutType>(desugar2)->getObjectType(),
                        ConstraintKind::Equal, subflags,
                  locator.withPathElement(ConstraintLocator::ArrayElementType));

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
      // A scalar type can be converted to an argument tuple so long as
      // there is at most one non-defaulted element.
      // For non-argument tuples, we can do the same conversion but not
      // to a tuple with varargs.
      if (!type1->is<LValueType>() &&
          ((tuple2->getNumElements() == 1 &&
            !tuple2->getElement(0).isVararg()) ||
           (kind >= ConstraintKind::Conversion &&
            tuple2->getElementForScalarInit() >= 0 &&
            (isArgumentTupleConversion ||
             !tuple2->getVarArgsBaseType())))) {
        conversionsOrFixes.push_back(
          ConversionRestrictionKind::ScalarToTuple);

        // FIXME: Prohibits some user-defined conversions for tuples.
        goto commit_to_conversions;
      }
    }

    // Subclass-to-superclass conversion.
    if (type1->mayHaveSuperclass() && type2->mayHaveSuperclass() &&
        type2->getClassOrBoundGenericClass() &&
        type1->getClassOrBoundGenericClass()
          != type2->getClassOrBoundGenericClass()) {
      assert(!type2->is<LValueType>() && "Unexpected lvalue type!");
      conversionsOrFixes.push_back(ConversionRestrictionKind::Superclass);
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
        = [&](ConversionRestrictionKind restriction) -> SolutionKind {
          addRestrictedConstraint(ConstraintKind::Subtype, restriction,
                                  type1, type2, locator);
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
    
    // Special implicit nominal conversions.
    if (!type1->is<LValueType>() &&
        kind >= ConstraintKind::Conversion) {
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
      return SolutionKind::Solved;
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
        return matchTypes(type1, function2->getResult(), kind, subflags,
                          locator.withPathElement(ConstraintLocator::Load));
    }

    // Pointer arguments can be converted from pointer-compatible types.
    if (kind >= ConstraintKind::ArgumentConversion) {
      Type unwrappedType2 = type2;
      OptionalTypeKind type2OptionalKind;
      if (Type unwrapped = type2->getAnyOptionalObjectType(type2OptionalKind))
        unwrappedType2 = unwrapped;
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
            auto inoutBaseType = inoutType1->getInOutObjectType();

            Type simplifiedInoutBaseType =
              getFixedTypeRecursive(inoutBaseType,
                                    kind == ConstraintKind::Equal,
                                    isArgumentTupleConversion);

            // FIXME: If the base is still a type variable, we can't tell
            // what to do here. Might have to try \c ArrayToPointer and make it
            // more robust.
            if (isArrayType(simplifiedInoutBaseType)) {
              conversionsOrFixes.push_back(
                                     ConversionRestrictionKind::ArrayToPointer);
            }
            conversionsOrFixes.push_back(
                                     ConversionRestrictionKind::InoutToPointer);
          }
          
          if (!flags.contains(TMF_ApplyingOperatorParameter) &&
              // Operators cannot use these implicit conversions.
              (kind == ConstraintKind::ArgumentConversion ||
               kind == ConstraintKind::ArgumentTupleConversion)) {

            // We can potentially convert from an UnsafeMutablePointer
            // of a different type, if we're a void pointer.
            Type unwrappedType1 = type1;
            OptionalTypeKind type1OptionalKind;
            if (Type unwrapped =
                  type1->getAnyOptionalObjectType(type1OptionalKind)) {
              unwrappedType1 = unwrapped;
            }

            // Don't handle normal optional-related conversions here.
            if (unwrappedType1->isEqual(unwrappedType2))
              break;

            PointerTypeKind type1PointerKind;
            bool type1IsPointer{
                unwrappedType1->getAnyPointerElementType(type1PointerKind)};
            bool optionalityMatches =
                type1OptionalKind == OTK_None || type2OptionalKind != OTK_None;
            if (type1IsPointer && optionalityMatches) {
              if (type1PointerKind == PTK_UnsafeMutablePointer) {
                // Favor an UnsafeMutablePointer-to-UnsafeMutablePointer
                // conversion.
                if (type1PointerKind != pointerKind)
                  increaseScore(ScoreKind::SK_ScalarPointerConversion);
                conversionsOrFixes.push_back(
                  ConversionRestrictionKind::PointerToPointer);
              }
              // UnsafeMutableRawPointer -> UnsafeRawPointer
              else if (type1PointerKind == PTK_UnsafeMutableRawPointer &&
                       pointerKind == PTK_UnsafeRawPointer) {
                if (type1PointerKind != pointerKind)
                  increaseScore(ScoreKind::SK_ScalarPointerConversion);
                conversionsOrFixes.push_back(
                  ConversionRestrictionKind::PointerToPointer);              
              }
            }
            // UnsafePointer and UnsafeRawPointer can also be converted from an
            // array or string value, or a UnsafePointer or
            // AutoreleasingUnsafeMutablePointer.
            if (pointerKind == PTK_UnsafePointer
                || pointerKind == PTK_UnsafeRawPointer) {
              if (isArrayType(type1)) {
                conversionsOrFixes.push_back(
                                     ConversionRestrictionKind::ArrayToPointer);
              }
              
              // The pointer can be converted from a string, if the element type
              // is compatible.
              if (type1->isEqual(TC.getStringType(DC))) {
                auto baseTy = getFixedTypeRecursive(pointeeTy, false);
                
                if (baseTy->isTypeVariableOrMember() ||
                    isStringCompatiblePointerBaseType(TC, DC, baseTy))
                  conversionsOrFixes.push_back(
                                    ConversionRestrictionKind::StringToPointer);
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
          if (type1->is<InOutType>()) {
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
    if (auto *iot = type2->getAs<InOutType>()) {
      return matchTypes(type1, LValueType::get(iot->getObjectType()),
                        kind, subflags,
                        locator.withPathElement(
                                ConstraintLocator::ArrayElementType));
    }
  }

  // Conformance of a metatype to an existential metatype is actually
  // equivalent to a conformance relationship on the instance types.
  // This applies to nested metatype levels, so if A : P then
  // A.Type : P.Type.
  if (concrete && kind >= ConstraintKind::Subtype &&
      type1->is<MetatypeType>() && type2->is<ExistentialMetatypeType>()) {
    conversionsOrFixes.push_back(
      ConversionRestrictionKind::MetatypeToExistentialMetatype);
  }
  
  // Instance type check for the above. We are going to check conformance once
  // we hit commit_to_conversions below, but we have to add a token restriction
  // to ensure we wrap the metatype value in a metatype erasure.
  if (concrete && type2->isExistentialType() &&
      !type1->is<LValueType>() &&
      kind >= ConstraintKind::Subtype) {

    // Penalize conversions to Any.
    if (kind >= ConstraintKind::Conversion && type2->isAny())
      increaseScore(ScoreKind::SK_EmptyExistentialConversion);

    conversionsOrFixes.push_back(ConversionRestrictionKind::Existential);
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
        return SolutionKind::Solved;
      }
    }
  }

  if (concrete && kind == ConstraintKind::BindParam) {
    if (auto *iot = dyn_cast<InOutType>(desugar1)) {
      if (auto *lvt = dyn_cast<LValueType>(desugar2)) {
        return matchTypes(iot->getObjectType(), lvt->getObjectType(),
                          ConstraintKind::Bind, subflags,
                          locator.withPathElement(
                            ConstraintLocator::ArrayElementType));
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
  if (shouldAttemptFixes() && !isTypeVarOrMember1 && !isTypeVarOrMember2 &&
      !flags.contains(TMF_ApplyingFix) && kind >= ConstraintKind::Conversion) {
    Type objectType1 = type1->getRValueObjectType();

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
        conversionsOrFixes.push_back(FixKind::ForceOptional);
      }
    }

    // If we have a value of type AnyObject that we're trying to convert to
    // a class, force a downcast.
    // FIXME: Also allow types bridged through Objective-C classes.
    if (objectType1->isAnyObject() &&
        type2->getClassOrBoundGenericClass()) {
      conversionsOrFixes.push_back(Fix::getForcedDowncast(*this, type2));
    }

    // Look through IUO's.
    auto type1WithoutIUO = objectType1;
    if (auto elt = type1WithoutIUO->getImplicitlyUnwrappedOptionalObjectType())
      type1WithoutIUO = elt;

    // If we could perform a bridging cast, try it.
    if (auto bridged =
          TC.getDynamicBridgedThroughObjCClass(DC, type1WithoutIUO, type2)) {
      // Note: don't perform this recovery for NSNumber;
      bool useFix = true;
      if (auto classType = bridged->getAs<ClassType>()) {
        SmallString<16> scratch;
        if (classType->getDecl()->isObjC() &&
            classType->getDecl()->getObjCRuntimeName(scratch) == "NSNumber")
          useFix = false;
      }

      if (useFix)
        conversionsOrFixes.push_back(Fix::getForcedDowncast(*this, type2));
    }

    // If we're converting an lvalue to an inout type, add the missing '&'.
    if (type2->getRValueType()->is<InOutType>() && type1->is<LValueType>()) {
      conversionsOrFixes.push_back(FixKind::AddressOf);
    }
  }

  if (conversionsOrFixes.empty()) {
    // If one of the types is a type variable or member thereof, we leave this
    // unsolved.
    if (isTypeVarOrMember1 || isTypeVarOrMember2)
      return formUnsolvedResult();

    return SolutionKind::Error;
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
    addDisjunctionConstraint(constraints, fixedLocator);
    return SolutionKind::Solved;
  }

  // For a single potential conversion, directly recurse, so that we
  // don't allocate a new constraint or constraint locator.

  // Handle restrictions.
  if (auto restriction = conversionsOrFixes[0].getRestriction()) {
    return simplifyRestrictedConstraint(*restriction, type1, type2,
                                        kind, subflags, locator);
  }

  // Handle fixes.
  auto fix = *conversionsOrFixes[0].getFix();
  return simplifyFixConstraint(fix, type1, type2, kind, subflags, locator);
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstructionConstraint(
    Type valueType, FunctionType *fnType, TypeMatchOptions flags,
    DeclContext *useDC,
    FunctionRefKind functionRefKind, ConstraintLocator *locator) {

  // Desugar the value type.
  auto desugarValueType = valueType->getDesugaredType();

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
    if (matchTypes(resultType, desugarValueType,
                   ConstraintKind::Bind,
                   flags,
                   ConstraintLocatorBuilder(locator)
                     .withPathElement(ConstraintLocator::ApplyFunction))
          == SolutionKind::Error)
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

  NameLookupOptions lookupOptions = defaultConstructorLookupOptions;
  if (isa<AbstractFunctionDecl>(useDC))
    lookupOptions |= NameLookupFlags::KnownPrivate;
  auto ctors = TC.lookupConstructors(useDC, valueType, lookupOptions);
  if (!ctors)
    return SolutionKind::Error;

  auto &context = getASTContext();
  auto name = context.Id_init;
  auto applyLocator = getConstraintLocator(locator,
                                           ConstraintLocator::ApplyArgument);
  auto fnLocator = getConstraintLocator(locator,
                                        ConstraintLocator::ApplyFunction);
  auto tv = createTypeVariable(applyLocator,
                               TVO_CanBindToLValue|TVO_PrefersSubtypeBinding);

  // The constructor will have function type T -> T2, for a fresh type
  // variable T. T2 is the result type provided via the construction
  // constraint itself.
  addValueMemberConstraint(MetatypeType::get(valueType, TC.Context), name,
                           FunctionType::get(tv, resultType),
                           useDC, functionRefKind,
                           getConstraintLocator(
                             fnLocator, 
                             ConstraintLocator::ConstructorMember));

  // The first type must be convertible to the constructor's argument type.
  addConstraint(ConstraintKind::ArgumentTupleConversion, argType, tv,
                applyLocator);

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

  // For purposes of argument type matching, existential types don't need to
  // conform -- they only need to contain the protocol, so check that
  // separately.
  switch (kind) {
  case ConstraintKind::SelfObjectOfProtocol:
    if (TC.containsProtocol(type, protocol, DC,
                            ConformanceCheckFlags::InExpression))
      return SolutionKind::Solved;
    break;
  case ConstraintKind::ConformsTo:
  case ConstraintKind::LiteralConformsTo:
    // Check whether this type conforms to the protocol.
    if (TC.conformsToProtocol(type, protocol, DC,
                              ConformanceCheckFlags::InExpression))
      return SolutionKind::Solved;
    break;
  default:
    llvm_unreachable("bad constraint kind");
  }
  
  if (!shouldAttemptFixes())
    return SolutionKind::Error;

  // See if there's anything we can do to fix the conformance:
  OptionalTypeKind optionalKind;
  if (auto optionalObjectType = type->getAnyOptionalObjectType(optionalKind)) {
    if (optionalKind == OTK_Optional) {
      TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);
      // The underlying type of an optional may conform to the protocol if the
      // optional doesn't; suggest forcing if that's the case.
      auto result = simplifyConformsToConstraint(
        optionalObjectType, protocol, kind,
        locator.withPathElement(LocatorPathElt::getGenericArgument(0)),
        subflags);
      if (result == SolutionKind::Solved) {
        if (recordFix(FixKind::ForceOptional, getConstraintLocator(locator))) {
          return SolutionKind::Error;
        }
      }
      return result;
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
  case CheckedCastKind::Swift3BridgingDowncast:
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
  Type objectTy = optTy->getAnyOptionalObjectType();
  if (!objectTy)
    return SolutionKind::Error;
  
  // The object type is an lvalue if the optional was.
  if (optLValueTy->is<LValueType>())
    objectTy = LValueType::get(objectTy);

  // Equate it to the other type in the constraint.
  addConstraint(ConstraintKind::Bind, objectTy, second, locator);
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

  // Dig out the instance type and figure out what members of the instance type
  // we are going to see.
  bool isMetatype = false;
  bool isModule = false;
  bool hasInstanceMembers = false;
  bool hasInstanceMethods = false;
  bool hasStaticMembers = false;
  Type instanceTy = baseObjTy;
  if (baseObjTy->is<ModuleType>()) {
    hasStaticMembers = true;
    isModule = true;
  } else if (auto baseObjMeta = baseObjTy->getAs<AnyMetatypeType>()) {
    instanceTy = baseObjMeta->getInstanceType();
    isMetatype = true;
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

  bool isExistential = instanceTy->isExistentialType();
  
  if (instanceTy->isTypeVariableOrMember() ||
      instanceTy->is<UnresolvedType>()) {
    MemberLookupResult result;
    result.OverallResult = MemberLookupResult::Unsolved;
    return result;
  }

  // Okay, start building up the result list.
  MemberLookupResult result;
  result.OverallResult = MemberLookupResult::HasResults;
  
  // If the base type is a tuple type, look for the named or indexed member
  // of the tuple.
  if (auto baseTuple = baseObjTy->getAs<TupleType>()) {
    // Tuples don't have compound-name members.
    if (!memberName.isSimpleName())
      return result;  // No result.
    
    StringRef nameStr = memberName.getBaseName().str();
    int fieldIdx = -1;
    // Resolve a number reference into the tuple type.
    unsigned Value = 0;
    if (!nameStr.getAsInteger(10, Value) &&
        Value < baseTuple->getNumElements()) {
      fieldIdx = Value;
    } else {
      fieldIdx = baseTuple->getNamedElementId(memberName.getBaseName());
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
  auto hasCompatibleArgumentLabels = [&](ValueDecl *decl) -> bool {
    if (!argumentLabels)
      return true;

    // This is a member lookup, which generally means that the call arguments
    // (if we have any) will apply to the second level of parameters, with
    // the member lookup binding the first level.  But there are cases where
    // we can get an unapplied declaration reference back.
    unsigned parameterDepth;
    if (isModule) {
      parameterDepth = 0;
    } else if (isMetatype && decl->isInstanceMember()) {
      parameterDepth = 0;
    } else {
      parameterDepth = 1;
    }

    return areConservativelyCompatibleArgumentLabels(decl, parameterDepth,
                                          argumentLabels->Labels,
                                          argumentLabels->HasTrailingClosure);
  };

  
  // Handle initializers, they have their own approach to name lookup.
  if (memberName.isSimpleName(TC.Context.Id_init)) {
    // The constructors are only found on the metatype.
    if (!isMetatype)
      return result;
    
    NameLookupOptions lookupOptions = defaultConstructorLookupOptions;
    if (isa<AbstractFunctionDecl>(DC))
      lookupOptions |= NameLookupFlags::KnownPrivate;
    
    // If we're doing a lookup for diagnostics, include inaccessible members,
    // the diagnostics machinery will sort it out.
    if (includeInaccessibleMembers)
      lookupOptions |= NameLookupFlags::IgnoreAccessibility;

    // If a constructor is only visible as a witness for a protocol
    // requirement, it must be an invalid override. Also, protocol
    // extensions cannot yet define designated initializers.
    lookupOptions -= NameLookupFlags::PerformConformanceCheck;

    LookupResult ctors = TC.lookupConstructors(DC, instanceTy, lookupOptions);
    if (!ctors)
      return result;    // No result.
    
    TypeBase *favoredType = nullptr;
    if (auto anchor = memberLocator->getAnchor()) {
      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        auto argExpr = applyExpr->getArg();
        favoredType = getFavoredType(argExpr);
        
        if (!favoredType) {
          optimizeConstraints(argExpr);
          favoredType = getFavoredType(argExpr);
        }        
      }
    }
    
    // Introduce a new overload set.
  retry_ctors_after_fail:
    bool labelMismatch = false;
    for (auto ctor : ctors) {
      // If the constructor is invalid, we fail entirely to avoid error cascade.
      TC.validateDecl(ctor);
      if (ctor->isInvalid())
        return result.markErrorAlreadyDiagnosed();

      // FIXME: Deal with broken recursion
      if (!ctor->getInterfaceType())
        continue;

      // If the argument labels for this result are incompatible with
      // the call site, skip it.
      if (!hasCompatibleArgumentLabels(ctor)) {
        labelMismatch = true;
        result.addUnviable(ctor, MemberLookupResult::UR_LabelMismatch);
        continue;
      }

      // If our base is an existential type, we can't make use of any
      // constructor whose signature involves associated types.
      if (isExistential) {
        if (auto *proto = ctor->getDeclContext()
                ->getAsProtocolOrProtocolExtensionContext()) {
          if (!proto->isAvailableInExistential(ctor)) {
            result.addUnviable(ctor,
                               MemberLookupResult::UR_UnavailableInExistential);
            continue;
          }
        }
      }
      
      // If the invocation's argument expression has a favored constraint,
      // use that information to determine whether a specific overload for
      // the initializer should be favored.
      if (favoredType && result.FavoredChoice == ~0U) {
        // Only try and favor monomorphic initializers.
        if (auto fnTypeWithSelf =
            ctor->getInterfaceType()->getAs<FunctionType>()) {
          
          if (auto fnType =
                  fnTypeWithSelf->getResult()->getAs<FunctionType>()) {
          
            auto argType = fnType->getInput()->getWithoutParens();
            argType = ctor.Decl->getInnermostDeclContext()
                ->mapTypeIntoContext(argType);
            if (argType->isEqual(favoredType))
              result.FavoredChoice = result.ViableCandidates.size();
          }
        }
      }
      
      result.addViable(OverloadChoice(baseTy, ctor, /*isSpecialized=*/false,
                                      functionRefKind));
    }


    // If we rejected some possibilities due to an argument-label
    // mismatch and ended up with nothing, try again ignoring the
    // labels. This allows us to perform typo correction on the labels.
    if (result.ViableCandidates.empty() && labelMismatch &&
        shouldAttemptFixes()) {
      argumentLabels.reset();
      goto retry_ctors_after_fail;
    }

    // FIXME: Should we look for constructors in bridged types?
    return result;
  }

  // Look for members within the base.
  LookupResult &lookup = lookupMember(instanceTy, memberName);

  // The set of directly accessible types, which is only used when
  // we're performing dynamic lookup into an existential type.
  bool isDynamicLookup = instanceTy->isAnyObject();

  // If the instance type is String bridged to NSString, compute
  // the type we'll look in for bridging.
  Type bridgedClass;
  Type bridgedType;
  if (instanceTy->getAnyNominal() == TC.Context.getStringDecl()) {
    if (Type classType = TC.Context.getBridgedToObjC(DC, instanceTy)) {
      bridgedClass = classType;
      bridgedType = isMetatype ? MetatypeType::get(classType) : classType;
    }
  }
  bool labelMismatch = false;

  // Local function that adds the given declaration if it is a
  // reasonable choice.
  auto addChoice = [&](ValueDecl *cand, bool isBridged,
                       bool isUnwrappedOptional) {
    // Destructors cannot be referenced manually
    if (isa<DestructorDecl>(cand)) {
      result.addUnviable(cand, MemberLookupResult::UR_DestructorInaccessible);
      return;
    }
    // If the result is invalid, skip it.
    TC.validateDecl(cand);
    if (cand->isInvalid()) {
      result.markErrorAlreadyDiagnosed();
      return;
    }

    // FIXME: Deal with broken recursion
    if (!cand->getInterfaceType())
      return;

    // If the argument labels for this result are incompatible with
    // the call site, skip it.
    if (!hasCompatibleArgumentLabels(cand)) {
      labelMismatch = true;
      result.addUnviable(cand, MemberLookupResult::UR_LabelMismatch);
      return;
    }

    // If our base is an existential type, we can't make use of any
    // member whose signature involves associated types.
    if (isExistential) {
      if (auto *proto = cand->getDeclContext()
              ->getAsProtocolOrProtocolExtensionContext()) {
        if (!proto->isAvailableInExistential(cand)) {
          result.addUnviable(cand,
                             MemberLookupResult::UR_UnavailableInExistential);
          return;
        }
      }
    }

    // See if we have an instance method, instance member or static method,
    // and check if it can be accessed on our base type.
    if (cand->isInstanceMember()) {
      if ((isa<FuncDecl>(cand) && !hasInstanceMethods) ||
          (!isa<FuncDecl>(cand) && !hasInstanceMembers)) {
        result.addUnviable(cand, MemberLookupResult::UR_InstanceMemberOnType);
        return;
      }

    // If the underlying type of a typealias is fully concrete, it is legal
    // to access the type with a protocol metatype base.
    } else if (isExistential &&
               isa<TypeAliasDecl>(cand) &&
               !cast<TypeAliasDecl>(cand)->getInterfaceType()->getCanonicalType()
                  ->hasTypeParameter()) {

      /* We're OK */

    } else {
      if (!hasStaticMembers) {
        result.addUnviable(cand, MemberLookupResult::UR_TypeMemberOnInstance);
        return;
      }
    }

    // If we have an rvalue base, make sure that the result isn't 'mutating'
    // (only valid on lvalues).
    if (!isMetatype &&
        !baseTy->is<LValueType>() && cand->isInstanceMember()) {
      if (auto *FD = dyn_cast<FuncDecl>(cand))
        if (FD->isMutating()) {
          result.addUnviable(cand,
                             MemberLookupResult::UR_MutatingMemberOnRValue);
          return;
        }

      // Subscripts and computed properties are ok on rvalues so long
      // as the getter is nonmutating.
      if (auto storage = dyn_cast<AbstractStorageDecl>(cand)) {
        if (storage->isGetterMutating()) {
          result.addUnviable(cand,
                             MemberLookupResult::UR_MutatingGetterOnRValue);
          return;
        }
      }
    }
    
    // If the result's type contains delayed members, we need to force them now.
    if (auto NT = dyn_cast<NominalType>(cand->getInterfaceType().getPointer())) {
      if (auto *NTD = dyn_cast<NominalTypeDecl>(NT->getDecl())) {
        TC.forceExternalDeclMembers(NTD);
      }
    }

    // If we're looking into an existential type, check whether this
    // result was found via dynamic lookup.
    if (isDynamicLookup) {
      assert(cand->getDeclContext()->isTypeContext() && "Dynamic lookup bug");

      // We found this declaration via dynamic lookup, record it as such.
      result.addViable(OverloadChoice::getDeclViaDynamic(baseTy, cand,
                                                         functionRefKind));
      return;
    }

    // If we have a bridged type, we found this declaration via bridging.
    if (isBridged) {
      result.addViable(OverloadChoice::getDeclViaBridge(bridgedType, cand,
                                                        functionRefKind));
      return;
    }

    // If we got the choice by unwrapping an optional type, unwrap the base
    // type.
    Type ovlBaseTy = baseTy;
    if (isUnwrappedOptional) {
      ovlBaseTy = MetatypeType::get(baseTy->castTo<MetatypeType>()
        ->getInstanceType()
        ->getAnyOptionalObjectType());
      result.addViable(
        OverloadChoice::getDeclViaUnwrappedOptional(ovlBaseTy, cand,
                                                    functionRefKind));
    } else {
      result.addViable(OverloadChoice(ovlBaseTy, cand,
                                      /*isSpecialized=*/false,
                                      functionRefKind));
    }
  };

  // Add all results from this lookup.
retry_after_fail:
  labelMismatch = false;
  for (auto result : lookup)
    addChoice(result, /*isBridged=*/false, /*isUnwrappedOptional=*/false);

  // If the instance type is a bridged to an Objective-C type, perform
  // a lookup into that Objective-C type.
  if (bridgedType) {
    LookupResult &bridgedLookup = lookupMember(bridgedClass, memberName);
    ModuleDecl *foundationModule = nullptr;
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
      
      addChoice(result, /*isBridged=*/true, /*isUnwrappedOptional=*/false);
    }
  }

  // If we're looking into a metatype for an unresolved member lookup, look
  // through optional types.
  //
  // FIXME: The short-circuit here is lame.
  if (result.ViableCandidates.empty() && isMetatype &&
      constraintKind == ConstraintKind::UnresolvedValueMember) {
    if (auto objectType = instanceTy->getAnyOptionalObjectType()) {
      if (objectType->mayHaveMembers()) {
        LookupResult &optionalLookup = lookupMember(objectType, memberName);
        for (auto result : optionalLookup)
          addChoice(result, /*bridged*/false, /*isUnwrappedOptional=*/true);
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
    
    // Ignore accessibility so we get candidates that might have been missed
    // before.
    lookupOptions |= NameLookupFlags::IgnoreAccessibility;
    // This is only used for diagnostics, so always use KnownPrivate.
    lookupOptions |= NameLookupFlags::KnownPrivate;
    
    auto lookup = TC.lookupMember(DC, instanceTy,
                                  memberName, lookupOptions);
    for (auto cand : lookup) {
      // If the result is invalid, skip it.
      TC.validateDecl(cand);
      if (cand->isInvalid()) {
        result.markErrorAlreadyDiagnosed();
        return result;
      }
      
      result.addUnviable(cand, MemberLookupResult::UR_Inaccessible);
    }
  }
  
  return result;
}


ConstraintSystem::SolutionKind
ConstraintSystem::simplifyMemberConstraint(ConstraintKind kind,
                                           Type baseTy, DeclName member,
                                           Type memberTy,
                                           DeclContext *useDC,
                                           FunctionRefKind functionRefKind,
                                           TypeMatchOptions flags,
                                           ConstraintLocatorBuilder locatorB) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  // FIXME: simplifyType() call here could be getFixedTypeRecursive?
  baseTy = simplifyType(baseTy, flags);
  Type baseObjTy = baseTy->getRValueType();

  // Try to look through ImplicitlyUnwrappedOptional<T>; the result is
  // always an l-value if the input was.
  if (auto objTy = lookThroughImplicitlyUnwrappedOptionalType(baseObjTy)) {
    increaseScore(SK_ForceUnchecked);
    
    baseObjTy = objTy;
    if (baseTy->is<LValueType>())
      baseTy = LValueType::get(objTy);
    else
      baseTy = objTy;
  }

  auto locator = getConstraintLocator(locatorB);
  MemberLookupResult result =
    performMemberLookup(kind, member, baseTy, functionRefKind, locator,
                        /*includeInaccessibleMembers*/false);
  
  switch (result.OverallResult) {
  case MemberLookupResult::Unsolved:
    // If requested, generate a constraint.
    if (flags.contains(TMF_GenerateConstraints)) {
      addUnsolvedConstraint(
        Constraint::createMember(*this, kind, baseTy, memberTy, member, useDC,
                                 functionRefKind, locator));
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
                   result.getFavoredChoice());
    
    return SolutionKind::Solved;
  }
  
  
  // If we found some unviable results, then fail, but without recovery.
  if (!result.UnviableCandidates.empty())
    return SolutionKind::Error;
  

  // If the lookup found no hits at all (either viable or unviable), diagnose it
  // as such and try to recover in various ways.

  auto instanceTy = baseObjTy;
  if (auto MTT = instanceTy->getAs<MetatypeType>())
    instanceTy = MTT->getInstanceType();
  
  // Value member lookup has some hacks too.
  if (shouldAttemptFixes() && baseObjTy->getOptionalObjectType()) {
    // If the base type was an optional, look through it.
    
    // Determine whether or not we want to provide an optional chaining fixit or
    // a force unwrap fixit.
    bool optionalChain;
    if (!getContextualType())
      optionalChain = !(Options & ConstraintSystemFlags::PreferForceUnwrapToOptional);
    else
      optionalChain = !getContextualType()->getOptionalObjectType().isNull();
    auto fixKind = optionalChain ? FixKind::OptionalChaining : FixKind::ForceOptional;

    // Note the fix.
    if (recordFix(fixKind, locator))
      return SolutionKind::Error;
    
    // Look through one level of optional.
    addValueMemberConstraint(baseObjTy->getOptionalObjectType(),
                             member, memberTy, useDC, functionRefKind, locator);
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
  // There's no bridging without ObjC interop, so we shouldn't have set up
  // bridging constraints without it.
  assert(TC.Context.LangOpts.EnableObjCInterop
         && "bridging constraint w/o ObjC interop?!");
  
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
    while (Type objectType = type->getAnyOptionalObjectType()) {
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

  // Unwrap one extra level of implicitly-unwrapped optional on the source,
  // if needed.
  if (numFromOptionals == numToOptionals + 1 &&
      !type1->getImplicitlyUnwrappedOptionalObjectType().isNull()) {
    --numFromOptionals;
    increaseScore(SK_ForceUnchecked);
    if (worseThanBestSolution()) {
      return SolutionKind::Error;
    }
  }

  // The source cannot be more optional than the destination, because bridging
  // conversions don't allow us to implicitly check for a value in the optional.
  if (numFromOptionals > numToOptionals) {
    return SolutionKind::Error;
  }

  // Explicit bridging from a value type to an Objective-C class type.
  if (unwrappedFromType->isPotentiallyBridgedValueType() &&
      unwrappedFromType->getAnyNominal()
        != TC.Context.getImplicitlyUnwrappedOptionalDecl() &&
      !flags.contains(TMF_ApplyingOperatorParameter) &&
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
      unwrappedToType->isPotentiallyBridgedValueType() &&
      unwrappedToType->getAnyNominal()
        != TC.Context.getImplicitlyUnwrappedOptionalDecl()) {
    Type bridgedValueType;
    if (auto objcClass = TC.Context.getBridgedToObjC(DC, unwrappedToType,
                                                     &bridgedValueType)) {
      // Bridging NSNumber to NSValue is one-way, since there are multiple Swift
      // value types that bridge to those object types. It requires a checked
      // cast to get back.
      // We accepted these coercions in Swift 3 mode, so we have to live with
      // them (but give a warning) in that language mode.
      if (!TC.Context.LangOpts.isSwiftVersion3()
          && TC.isObjCClassWithMultipleSwiftBridgedTypes(objcClass, DC))
        return SolutionKind::Error;

      // If the bridged value type is generic, the generic arguments
      // must either match or be bridged.
      // FIXME: This should be an associated type of the protocol.
      if (auto fromBGT = unwrappedToType->getAs<BoundGenericType>()) {
        if (fromBGT->getDecl() == TC.Context.getArrayDecl()) {
          // [AnyObject]
          addConstraint(ConstraintKind::Bind, fromBGT->getGenericArgs()[0],
                        TC.Context.getProtocol(KnownProtocolKind::AnyObject)
                          ->getDeclaredType(),
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
                        TC.Context.getProtocol(KnownProtocolKind::AnyObject)
                          ->getDeclaredType(),
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
                     subflags, locator)
            == ConstraintSystem::SolutionKind::Error)
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
    // We should have the noescape end of the relation.
    if (!fn2->getExtInfo().isNoEscape())
      return SolutionKind::Error;
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

  // Try to look through ImplicitlyUnwrappedOptional<T>: the result is always an
  // r-value.
  if (auto objTy = lookThroughImplicitlyUnwrappedOptionalType(desugar2)) {
    type2 = getFixedTypeRecursive(objTy, flags, /*wantRValue=*/true);
    desugar2 = type2->getDesugaredType();
  }

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

retry:
  // For a function, bind the output and convert the argument to the input.
  auto func1 = type1->castTo<FunctionType>();
  if (auto func2 = dyn_cast<FunctionType>(desugar2)) {
    // If this application is part of an operator, then we allow an implicit
    // lvalue to be compatible with inout arguments.  This is used by
    // assignment operators.
    ConstraintKind ArgConv = ConstraintKind::ArgumentTupleConversion;
    if (isa<PrefixUnaryExpr>(anchor) || isa<PostfixUnaryExpr>(anchor) ||
        isa<BinaryExpr>(anchor))
      ArgConv = ConstraintKind::OperatorArgumentTupleConversion;

    // The argument type must be convertible to the input type.
    if (matchTypes(func1->getInput(), func2->getInput(),
                   ArgConv, subflags,
                   outerLocator.withPathElement(
                     ConstraintLocator::ApplyArgument))
          == SolutionKind::Error)
      return SolutionKind::Error;

    // The result types are equivalent.
    if (matchTypes(func1->getResult(), func2->getResult(),
                   ConstraintKind::Bind,
                   subflags,
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
    auto instance2 = getFixedTypeRecursive(meta2->getInstanceType(), true);
    if (instance2->isTypeVariableOrMember())
      return formUnsolved();

    // Construct the instance from the input arguments.
    return simplifyConstructionConstraint(instance2, func1, subflags,
                                          /*FIXME?*/ DC,
                                          FunctionRefKind::SingleApply,
                                          getConstraintLocator(outerLocator));
  }

  if (!shouldAttemptFixes())
    return SolutionKind::Error;

  // If we're coming from an optional type, unwrap the optional and try again.
  if (auto objectType2 = desugar2->getOptionalObjectType()) {
    if (recordFix(FixKind::ForceOptional, getConstraintLocator(locator)))
      return SolutionKind::Error;

    type2 = objectType2;
    desugar2 = type2->getDesugaredType();
    goto retry;
  }

  return SolutionKind::Error;
}

static Type getBaseTypeForPointer(ConstraintSystem &cs, TypeBase *type) {
  if (Type unwrapped = type->getAnyOptionalObjectType())
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

  // We'll apply user conversions for operator arguments at the application
  // site.
  if (matchKind == ConstraintKind::OperatorArgumentConversion) {
    flags |= TMF_ApplyingOperatorParameter;
  }

  TypeMatchOptions subflags = getDefaultDecompositionOptions(flags);

  switch (restriction) {
  // for $< in { <, <c, <oc }:
  //   T_i $< U_i ===> (T_i...) $< (U_i...)
  case ConversionRestrictionKind::TupleToTuple:
    return matchTupleTypes(type1->castTo<TupleType>(),
                           type2->castTo<TupleType>(),
                           matchKind, subflags, locator);

  //   T <c U ===> T <c (U)
  case ConversionRestrictionKind::ScalarToTuple:
    return matchScalarToTupleTypes(type1, type2->castTo<TupleType>(),
                                   matchKind, subflags, locator);

  case ConversionRestrictionKind::DeepEquality:
    return matchDeepEqualityTypes(type1, type2, locator);

  case ConversionRestrictionKind::Superclass:
    addContextualScore();
    return matchSuperclassTypes(type1, type2, matchKind, subflags, locator);

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
  //   for T protocol,
  //     T : S ===> T.Protocol $< S.Type
  //   else,
  //     T : S ===> T.Type $< S.Type
  case ConversionRestrictionKind::MetatypeToExistentialMetatype:
    addContextualScore();

    return matchExistentialTypes(
             type1->castTo<MetatypeType>()->getInstanceType(),
             type2->castTo<ExistentialMetatypeType>()->getInstanceType(),
             ConstraintKind::ConformsTo,
             subflags,
             locator.withPathElement(ConstraintLocator::InstanceType));

  // for $< in { <, <c, <oc }:
  //   T $< U ===> T $< U?
  case ConversionRestrictionKind::ValueToOptional: {
    addContextualScore();
    increaseScore(SK_ValueToOptional);

    assert(matchKind >= ConstraintKind::Subtype);
    if (type2->isTypeVariableOrMember())
      return formUnsolved();

    if (auto generic2 = type2->getAs<BoundGenericType>()) {
      if (generic2->getDecl()->classifyAsOptionalType()) {
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
        if (generic1->getDecl()->classifyAsOptionalType() &&
            generic2->getDecl()->classifyAsOptionalType())
          return matchTypes(generic1->getGenericArgs()[0],
                            generic2->getGenericArgs()[0],
                            matchKind, subflags,
                            locator.withPathElement(
                              LocatorPathElt::getGenericArgument(0)));
      }
    }

    return SolutionKind::Error;
  }

  // T <c U ===> T! <c U
  //
  // We don't want to allow this after user-defined conversions:
  //   - it gets really complex for users to understand why there's
  //     a dereference in their code
  //   - it would allow nil to be coercible to a non-optional type
  // Fortunately, user-defined conversions only allow subtype
  // conversions on their results.
  case ConversionRestrictionKind::ForceUnchecked: {
    addContextualScore();
    assert(matchKind >= ConstraintKind::Conversion);

    if (type1->isTypeVariableOrMember())
      return formUnsolved();

    if (auto boundGenericType1 = type1->getAs<BoundGenericType>()) {
      if (boundGenericType1->getDecl()->classifyAsOptionalType()
            == OTK_ImplicitlyUnwrappedOptional) {
        assert(boundGenericType1->getGenericArgs().size() == 1);
        Type valueType1 = boundGenericType1->getGenericArgs()[0];
        increaseScore(SK_ForceUnchecked);
        return matchTypes(valueType1, type2,
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
    auto obj1 = type1;
    // Unwrap an inout type.
    if (auto inout1 = obj1->getAs<InOutType>()) {
      obj1 = inout1->getObjectType();
    }
    
    obj1 = getFixedTypeRecursive(obj1, false, false);
    
    auto t2 = type2->getDesugaredType();
    
    auto baseType1 = getFixedTypeRecursive(*isArrayType(obj1), false, false);
    auto baseType2 = getBaseTypeForPointer(*this, t2);

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
    baseType2 = getFixedTypeRecursive(baseType2, false, false);
    // If we haven't resolved the element type, generate constraints.
    if (baseType2->isTypeVariableOrMember()) {
      if (flags.contains(TMF_GenerateConstraints)) {
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
    if (result == SolutionKind::Error)
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
            type1->getRValueType()->lookThroughAllAnyOptionalTypes())) {
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
    ConstraintRestrictions.push_back(
      std::make_tuple(type1, type2, restriction));
    return SolutionKind::Solved;

  case SolutionKind::Unsolved:
    return SolutionKind::Unsolved;

  case SolutionKind::Error:
    return SolutionKind::Error;
  }

  llvm_unreachable("Unhandled SolutionKind in switch.");
}

bool ConstraintSystem::recordFix(Fix fix, ConstraintLocatorBuilder locator) {
  auto &ctx = getASTContext();
  if (ctx.LangOpts.DebugConstraintSolver) {
    auto &log = ctx.TypeCheckerDebug->getStream();
    log.indent(solverState ? solverState->depth * 2 + 2 : 0)
      << "(attempting fix ";
    fix.print(log, this);
    log << " @";
    getConstraintLocator(locator)->dump(&ctx.SourceMgr, log);
    log << ")\n";
  }

  // Record the fix.
  if (fix.getKind() != FixKind::None) {
    // Increase the score. If this would make the current solution worse than
    // the best solution we've seen already, stop now.
    increaseScore(SK_Fix);
    if (worseThanBestSolution())
      return true;

    Fixes.push_back({fix, getConstraintLocator(locator)});
  }
  return false;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyFixConstraint(Fix fix, Type type1, Type type2,
                                        ConstraintKind matchKind,
                                        TypeMatchOptions flags,
                                        ConstraintLocatorBuilder locator) {
  if (recordFix(fix, locator))
    return SolutionKind::Error;

  // Try with the fix.
  TypeMatchOptions subflags =
    getDefaultDecompositionOptions(flags) | TMF_ApplyingFix;
  switch (fix.getKind()) {
  case FixKind::None:
    return matchTypes(type1, type2, matchKind, subflags, locator);

  case FixKind::ForceOptional:
  case FixKind::OptionalChaining:
    // Assume that '!' was applied to the first type.
    return matchTypes(type1->getRValueObjectType()->getOptionalObjectType(),
                      type2, matchKind, subflags, locator);

  case FixKind::ForceDowncast:
    // These work whenever they are suggested.
    return SolutionKind::Solved;

  case FixKind::AddressOf:
    // Assume that '&' was applied to the first type, turning an lvalue into
    // an inout.
    return matchTypes(InOutType::get(type1->getRValueType()), type2,
                      matchKind, subflags, locator);

  case FixKind::CoerceToCheckedCast:
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
  case ConstraintKind::ArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentTupleConversion:
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
  case ConstraintKind::Layout:
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

  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::BindOverload:
  case ConstraintKind::Disjunction:
    llvm_unreachable("Use the correct addConstraint()");
  }

  llvm_unreachable("Unhandled ConstraintKind in switch.");
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

  // Bridging.
  if (getASTContext().LangOpts.EnableObjCInterop) {
    // The source type can be explicitly converted to the destination type.
    Constraint *bridgingConstraint =
      Constraint::create(*this, ConstraintKind::BridgingConversion,
                         fromType, toType, locatorPtr);
    constraints.push_back(bridgingConstraint);
  }

  if (allowFixes && shouldAttemptFixes()) {
    Constraint *downcastConstraint =
      Constraint::createFixed(*this, ConstraintKind::CheckedCast,
                              FixKind::CoerceToCheckedCast, fromType,
                              toType, locatorPtr);
    constraints.push_back(downcastConstraint);
  }

  addDisjunctionConstraint(constraints, locator,
    getASTContext().LangOpts.EnableObjCInterop && allowFixes ? RememberChoice
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
  case ConstraintKind::ArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentTupleConversion:
  case ConstraintKind::OperatorArgumentConversion: {
    // Relational constraints.
    auto matchKind = constraint.getKind();

    // If there is a fix associated with this constraint, apply it.
    if (auto fix = constraint.getFix()) {
      return simplifyFixConstraint(*fix, constraint.getFirstType(),
                                   constraint.getSecondType(), matchKind,
                                   None, constraint.getLocator());
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

  case ConstraintKind::BindOverload:
    resolveOverload(constraint.getLocator(), constraint.getFirstType(),
                    constraint.getOverloadChoice(),
                    constraint.getOverloadUseDC());
    return SolutionKind::Solved;

  case ConstraintKind::ConformsTo:
  case ConstraintKind::Layout:
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
      if (auto fix = constraint.getFix()) {
        if (recordFix(*fix, constraint.getLocator())) {
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
                                    TMF_GenerateConstraints,
                                    constraint.getLocator());

  case ConstraintKind::Defaultable:
    return simplifyDefaultableConstraint(constraint.getFirstType(),
                                         constraint.getSecondType(),
                                         TMF_GenerateConstraints,
                                         constraint.getLocator());

  case ConstraintKind::Disjunction:
    // Disjunction constraints are never solved here.
    return SolutionKind::Unsolved;
  }

  llvm_unreachable("Unhandled ConstraintKind in switch.");
}
