//===--- GenDiffWitness.cpp - IRGen for differentiability witnesses -------===//
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
// SWIFT_ENABLE_TENSORFLOW
//
// This file implements IR generation for SIL differentiability witnesses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/PrettyStackTrace.h"
#include "swift/SIL/SILDifferentiabilityWitness.h"

#include "ConstantBuilder.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

void IRGenModule::emitSILDifferentiabilityWitness(
    SILDifferentiabilityWitness *dw) {
  PrettyStackTraceDifferentiabilityWitness _st(
      "emitting differentiability witness for", dw->getKey());

  // Don't emit declarations.
  if (dw->isDeclaration())
    return;

  ConstantInitBuilder builder(*this);
  auto diffWitnessContents = builder.beginStruct();

  // TODO(TF-894): When the differentiation transform canonicalizes all
  // differentiability witnesses to have JVP/VJP functions, remove the nullptr
  // cases and assert that JVP/VJP functions exist.
  if (dw->getJVP()) {
    diffWitnessContents.addBitCast(
        getAddrOfSILFunction(dw->getJVP(), NotForDefinition), Int8PtrTy);
  } else {
    diffWitnessContents.addNullPointer(Int8PtrTy);
  }
  if (dw->getVJP()) {
    diffWitnessContents.addBitCast(
        getAddrOfSILFunction(dw->getVJP(), NotForDefinition), Int8PtrTy);
  } else {
    diffWitnessContents.addNullPointer(Int8PtrTy);
  }

  getAddrOfDifferentiabilityWitness(
      dw, diffWitnessContents.finishAndCreateFuture());
}
