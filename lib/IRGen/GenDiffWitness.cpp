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
  llvm::errs() << "IRGEN SIL DIFF WITNESS, CLANG DECL? "
               << dw->getOriginalFunction()->getClangDecl() << ", C REFERENCES: "
               << dw->getOriginalFunction()->hasCReferences() << ", WEAK IMPORTED: "
               << dw->getOriginalFunction()->isWeakImported() << ", IS EXTERNAL DECL: "
               << dw->getOriginalFunction()->isExternalDeclaration() << ", IS DECL: "
               << dw->isDeclaration()
               << "\n";
  dw->dump();
  // Don't emit declarations.
  if (dw->isDeclaration())
    return;

  // Don't emit `public_external` witnesses.
  // Make an exception for Clang-imported functions.
  if (!dw->getOriginalFunction()->getClangDecl()) {
    if (hasPublicVisibility(dw->getLinkage()) &&
        isAvailableExternally(dw->getLinkage()))
      return;
  }

  ConstantInitBuilder builder(*this);
  auto diffWitnessContents = builder.beginStruct();

  assert(dw->getJVP() && "diff witness should be canonicalized");
  assert(dw->getVJP() && "diff witness should be canonicalized");

  diffWitnessContents.addBitCast(
      getAddrOfSILFunction(dw->getJVP(), NotForDefinition), Int8PtrTy);
  diffWitnessContents.addBitCast(
      getAddrOfSILFunction(dw->getVJP(), NotForDefinition), Int8PtrTy);

  getAddrOfDifferentiabilityWitness(
      dw, diffWitnessContents.finishAndCreateFuture());

  llvm::errs() << "IRGEN'd DIFF WITNESS!\n";
}
