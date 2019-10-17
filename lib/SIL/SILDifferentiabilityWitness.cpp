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
  //llvm::dbgs() << "construct dw for " << originalFunction->getName() << "\n";
  //if (originalFunction)
  //  originalFunction->incrementRefCount();
  //if (jvp)
  //  jvp->incrementRefCount();
  //if (vjp)
  //  vjp->incrementRefCount();
  // Register the differentiability witness in the module.
  auto config = diffWitness->getAutoDiffConfig();
#if 0
  llvm::errs() << "SILDifferentiabilityWitness::create\n";
  config.print(llvm::errs()); llvm::errs() << "\n";
#endif
  assert(!module.DifferentiabilityWitnessMap[originalFunction->getName()].count(config) &&
         "Cannot create duplicate differentiability witness in a module");
  module.DifferentiabilityWitnessMap[originalFunction->getName()]
                                    [diffWitness->getAutoDiffConfig()] =
      diffWitness;
  module.getDifferentiabilityWitnessList().push_back(diffWitness);
#if 0
  diffWitness->dump();
#endif
  return diffWitness;
}

SILDifferentiabilityWitness::~SILDifferentiabilityWitness() {
  //llvm::dbgs() << "destruct dw for " << originalFunction->getName() << "\n";
  //if (originalFunction)
  //  originalFunction->decrementRefCount();
  //if (jvp)
  //  jvp->decrementRefCount();
  //if (vjp)
  //  vjp->decrementRefCount();
  //llvm::dbgs() << "finished destruct dw for " << originalFunction->getName() << "\n";
}

SILDifferentiabilityWitnessKey SILDifferentiabilityWitness::getKey() const {
  return std::make_pair(originalFunction->getName(), autoDiffConfig);
}
