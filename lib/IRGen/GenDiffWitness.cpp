//===--- GenDiffWitness.cpp - IRGen for differentiability witnesses -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
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

  // Don't emit `public_external` witnesses.
  if (dw->getLinkage() == SILLinkage::PublicExternal)
    return;

  ConstantInitBuilder builder(*this);
  auto diffWitnessContents = builder.beginStruct();

  // TODO(TF-1211): Uncomment assertions after upstreaming differentiation
  // transform.
  // The mandatory differentiation transform canonicalizes differentiability
  // witnesses and ensures that JVPs/VJPs are populated.
  /*
  assert(dw->getJVP() &&
         "Differentiability witness definition should have JVP");
  assert(dw->getVJP() &&
         "Differentiability witness definition should have VJP");
  diffWitnessContents.addBitCast(
      getAddrOfSILFunction(dw->getJVP(), NotForDefinition), Int8PtrTy);
  diffWitnessContents.addBitCast(
      getAddrOfSILFunction(dw->getVJP(), NotForDefinition), Int8PtrTy);
  */
  llvm::Constant *jvpValue = llvm::UndefValue::get(Int8PtrTy);
  llvm::Constant *vjpValue = llvm::UndefValue::get(Int8PtrTy);
  if (auto *jvpFn = dw->getJVP())
    jvpValue = getAddrOfSILFunction(dw->getJVP(), NotForDefinition);
  if (auto *vjpFn = dw->getJVP())
    vjpValue = getAddrOfSILFunction(dw->getVJP(), NotForDefinition);
  diffWitnessContents.addBitCast(jvpValue, Int8PtrTy);
  diffWitnessContents.addBitCast(vjpValue, Int8PtrTy);

  getAddrOfDifferentiabilityWitness(
      dw, diffWitnessContents.finishAndCreateFuture());
}
