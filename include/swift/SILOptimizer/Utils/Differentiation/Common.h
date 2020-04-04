//===--- Common.h - Automatic differentiation common utils ----*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Automatic differentiation common utilities.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_COMMON_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_COMMON_H

#include "swift/SIL/SILDifferentiabilityWitness.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"

namespace swift {

//===----------------------------------------------------------------------===//
// Helpers
//===----------------------------------------------------------------------===//

namespace autodiff {

/// Prints an "[AD] " prefix to `llvm::dbgs()` and returns the debug stream.
/// This is being used to print short debug messages within the AD pass.
raw_ostream &getADDebugStream();

/// Returns the underlying instruction for the given SILValue, if it exists,
/// peering through function conversion instructions.
template <class Inst> Inst *peerThroughFunctionConversions(SILValue value) {
  if (auto *inst = dyn_cast<Inst>(value))
    return inst;
  if (auto *cvi = dyn_cast<CopyValueInst>(value))
    return peerThroughFunctionConversions<Inst>(cvi->getOperand());
  if (auto *bbi = dyn_cast<BeginBorrowInst>(value))
    return peerThroughFunctionConversions<Inst>(bbi->getOperand());
  if (auto *tttfi = dyn_cast<ThinToThickFunctionInst>(value))
    return peerThroughFunctionConversions<Inst>(tttfi->getOperand());
  if (auto *cfi = dyn_cast<ConvertFunctionInst>(value))
    return peerThroughFunctionConversions<Inst>(cfi->getOperand());
  if (auto *pai = dyn_cast<PartialApplyInst>(value))
    return peerThroughFunctionConversions<Inst>(pai->getCallee());
  return nullptr;
}

} // end namespace autodiff

/// Creates arguments in the entry block based on the function type.
inline void createEntryArguments(SILFunction *f) {
  auto *entry = f->getEntryBlock();
  auto conv = f->getConventions();
  auto &ctx = f->getASTContext();
  auto moduleDecl = f->getModule().getSwiftModule();
  assert((entry->getNumArguments() == 0 || conv.getNumSILArguments() == 0) &&
         "Entry already has arguments?!");
  auto createFunctionArgument = [&](SILType type) {
    // Create a dummy parameter declaration.
    // Necessary to prevent crash during argument explosion optimization.
    auto loc = f->getLocation().getSourceLoc();
    auto *decl = new (ctx)
        ParamDecl(loc, loc, Identifier(), loc, Identifier(), moduleDecl);
    decl->setSpecifier(ParamDecl::Specifier::Default);
    entry->createFunctionArgument(type, decl);
  };
  // f->getLoweredFunctionType()->remap
  for (auto indResTy : conv.getIndirectSILResultTypes()) {
    if (indResTy.hasArchetype())
      indResTy = indResTy.mapTypeOutOfContext();
    createFunctionArgument(f->mapTypeIntoContext(indResTy).getAddressType());
    // createFunctionArgument(indResTy.getAddressType());
  }
  for (auto paramTy : conv.getParameterSILTypes()) {
    if (paramTy.hasArchetype())
      paramTy = paramTy.mapTypeOutOfContext();
    createFunctionArgument(f->mapTypeIntoContext(paramTy));
    // createFunctionArgument(paramTy);
  }
}

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_MANDATORY_DIFFERENTIATION_COMMON_H
