//===--- SILDifferentiabilityWitness.cpp - Differentiability witnesses ----===//
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

#define DEBUG_TYPE "sil-differentiability-witness"

#include "swift/SIL/SILDifferentiabilityWitness.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SILDifferentiabilityWitness *SILDifferentiabilityWitness::create(
    SILModule &module, SILFunction *originalFunction,
    AutoDiffIndexSubset *parameterIndices, AutoDiffIndexSubset *resultIndices,
    GenericSignature *derivativeGenSig, SILFunction *jvp, SILFunction *vjp,
    bool isSerialized) {
  void *buf = module.allocate(sizeof(SILDifferentiabilityWitness),
                              alignof(SILDifferentiabilityWitness));
  SILDifferentiabilityWitness *dw = ::new (buf)
      SILDifferentiabilityWitness(module, originalFunction, parameterIndices,
                                  resultIndices, derivativeGenSig, jvp, vjp,
                                  isSerialized);
  return dw;
}
