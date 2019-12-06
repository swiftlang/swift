//===--- DerivativeLookup.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// SWIFT_ENABLE_TENSORFLOW
//
// Utilities for looking up derivatives of functions.
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/Differentiation/DerivativeLookup.h"

namespace swift {

/// Returns the AbstractFunctionDecl corresponding to `F`. If there isn't one,
/// returns `nullptr`.
static AbstractFunctionDecl *findAbstractFunctionDecl(SILFunction *F) {
  auto *DC = F->getDeclContext();
  if (!DC)
    return nullptr;
  auto *D = DC->getAsDecl();
  if (!D)
    return nullptr;
  return dyn_cast<AbstractFunctionDecl>(D);
}

SILDifferentiabilityWitness *
getExactDifferentiabilityWitness(SILModule &module, SILFunction *original,
                                 IndexSubset *parameterIndices,
                                 IndexSubset *resultIndices) {
  for (auto *w : module.lookUpDifferentiabilityWitnessesForFunction(
           original->getName())) {
    if (w->getParameterIndices() == parameterIndices &&
        w->getResultIndices() == resultIndices)
      return w;
  }
  return nullptr;
}

bool findMinimalDerivativeConfiguration(
    AbstractFunctionDecl *original, IndexSubset *parameterIndices,
    IndexSubset *&minimalASTParameterIndices, AutoDiffConfig &minimalConfig) {
  auto configs = original->getDerivativeFunctionConfigurations();
  for (auto config : configs) {
    auto *silParameterIndices = autodiff::getLoweredParameterIndices(
        config.parameterIndices,
        original->getInterfaceType()->castTo<AnyFunctionType>());
    // If all indices in `parameterIndices` are in `daParameterIndices`, and
    // it has fewer indices than our current candidate and a primitive VJP,
    // then `attr` is our new candidate.
    //
    // NOTE(TF-642): `attr` may come from a un-partial-applied function and
    // have larger capacity than the desired indices. We expect this logic to
    // go away when `partial_apply` supports `@differentiable` callees.
    if (silParameterIndices->isSupersetOf(parameterIndices->extendingCapacity(
            original->getASTContext(), silParameterIndices->getCapacity())) &&
        // fewer parameters than before
        (!minimalConfig.parameterIndices ||
         silParameterIndices->getNumIndices() <
             minimalConfig.parameterIndices->getNumIndices())) {
      minimalASTParameterIndices = config.parameterIndices;
      minimalConfig = config;
      minimalConfig.parameterIndices = silParameterIndices;
    }
  }
  return minimalASTParameterIndices;
}

SILDifferentiabilityWitness *getOrCreateMinimalASTDifferentiabilityWitness(
    SILModule &module, SILFunction *original, IndexSubset *parameterIndices,
    IndexSubset *resultIndices) {
  // AST differentiability witnesses always have a single result.
  if (resultIndices->getCapacity() != 1 || !resultIndices->contains(0))
    return nullptr;

  // Explicit differentiability witnesses only exist on SILFunctions that come
  // from AST functions.
  auto *originalAFD = findAbstractFunctionDecl(original);
  if (!originalAFD)
    return nullptr;

  IndexSubset *minimalASTParameterIndices = nullptr;
  AutoDiffConfig minimalConfig(
      /*parameterIndices*/ nullptr, /*resultIndices*/ nullptr,
      /*derivativeGenericSignature*/ GenericSignature());
  if (!findMinimalDerivativeConfiguration(originalAFD, parameterIndices,
                                          minimalASTParameterIndices,
                                          minimalConfig)) {
    return nullptr;
  }

  auto *existingWitness = module.lookUpDifferentiabilityWitness(
      {original->getName(), minimalConfig});
  if (existingWitness)
    return existingWitness;

  assert(original->isExternalDeclaration() &&
         "SILGen should create differentiability witnesses for all function "
         "definitions with explicit differentiable attributes");

  return SILDifferentiabilityWitness::createDeclaration(
      module, SILLinkage::PublicExternal, original,
      minimalConfig.parameterIndices, minimalConfig.resultIndices,
      minimalConfig.derivativeGenericSignature);
}

} // end namespace swift
