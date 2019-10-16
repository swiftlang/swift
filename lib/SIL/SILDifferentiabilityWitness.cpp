//===--- SILDifferentiabilityWitness.cpp - Differentiability witnesses ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-differentiability-witness"

#include "swift/SIL/SILDifferentiabilityWitness.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SILDifferentiabilityWitness *SILDifferentiabilityWitness::create(
    SILModule &module, SILLinkage linkage, SILFunction *originalFunction,
    IndexSubset *parameterIndices, IndexSubset *resultIndices,
    GenericSignature *derivativeGenSig, SILFunction *jvp, SILFunction *vjp,
    bool isSerialized, DeclAttribute *attribute) {
  auto *diffWitness = new (module) SILDifferentiabilityWitness(
      module, linkage, originalFunction, parameterIndices, resultIndices,
      derivativeGenSig, jvp, vjp, isSerialized, attribute);
  // Register the differentiability witness in the module.
  assert(!module.DifferentiabilityWitnessMap.count(diffWitness->getKey()) &&
         "Cannot create duplicate differentiability witness in a module");
  module.DifferentiabilityWitnessMap[diffWitness->getKey()] = diffWitness;
  module.getDifferentiabilityWitnessList().push_back(diffWitness);
  return diffWitness;
}

SILDifferentiabilityWitnessKey SILDifferentiabilityWitness::getKey() const {
  return std::make_pair(originalFunction->getName(), getConfig());
}
