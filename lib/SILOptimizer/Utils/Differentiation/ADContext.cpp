//===--- ADContext.cpp - Context for Differentiation ----------*- C++ -*---===//
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
// Per-module contextual information for the Differentiation pass.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "swift/SILOptimizer/Utils/Differentiation/ADContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using llvm::DenseMap;
using llvm::SmallPtrSet;
using llvm::SmallVector;

namespace swift {
namespace autodiff {

//===----------------------------------------------------------------------===//
// Local helpers
//===----------------------------------------------------------------------===//

/// Given an operator name, such as '+', and a protocol, returns the '+'
/// operator. If the operator does not exist in the protocol, returns null.
static FuncDecl *findOperatorDeclInProtocol(DeclName operatorName,
                                            ProtocolDecl *protocol) {
  assert(operatorName.isOperator());
  // Find the operator requirement in the given protocol declaration.
  auto opLookup = protocol->lookupDirect(operatorName);
  for (auto *decl : opLookup) {
    if (!decl->isProtocolRequirement())
      continue;
    auto *fd = dyn_cast<FuncDecl>(decl);
    if (!fd || !fd->isStatic() || !fd->isOperator())
      continue;
    return fd;
  }
  // Not found.
  return nullptr;
}

//===----------------------------------------------------------------------===//
// ADContext methods
//===----------------------------------------------------------------------===//

ADContext::ADContext(SILModuleTransform &transform)
    : transform(transform), module(*transform.getModule()),
      passManager(*transform.getPassManager()) {}

FuncDecl *ADContext::getPlusDecl() const {
  if (!cachedPlusFn) {
    cachedPlusFn = findOperatorDeclInProtocol(astCtx.getIdentifier("+"),
                                              additiveArithmeticProtocol);
    assert(cachedPlusFn && "AdditiveArithmetic.+ not found");
  }
  return cachedPlusFn;
}

FuncDecl *ADContext::getPlusEqualDecl() const {
  if (!cachedPlusEqualFn) {
    cachedPlusEqualFn = findOperatorDeclInProtocol(astCtx.getIdentifier("+="),
                                                   additiveArithmeticProtocol);
    assert(cachedPlusEqualFn && "AdditiveArithmetic.+= not found");
  }
  return cachedPlusEqualFn;
}

void ADContext::cleanUp() {
  // Delete all references to generated functions.
  for (auto fnRef : generatedFunctionReferences) {
    if (auto *fnRefInst =
            peerThroughFunctionConversions<FunctionRefInst>(fnRef)) {
      fnRefInst->replaceAllUsesWithUndef();
      fnRefInst->eraseFromParent();
    }
  }
  // Delete all generated functions.
  for (auto *generatedFunction : generatedFunctions) {
    LLVM_DEBUG(getADDebugStream() << "Deleting generated function "
                                  << generatedFunction->getName() << '\n');
    generatedFunction->dropAllReferences();
    transform.notifyWillDeleteFunction(generatedFunction);
    module.eraseFunction(generatedFunction);
  }
}

DifferentiableFunctionInst *ADContext::createDifferentiableFunction(
    SILBuilder &builder, SILLocation loc, IndexSubset *parameterIndices,
    IndexSubset *resultIndices, SILValue original,
    Optional<std::pair<SILValue, SILValue>> derivativeFunctions) {
  auto *dfi = builder.createDifferentiableFunction(
      loc, parameterIndices, resultIndices, original, derivativeFunctions);
  processedDifferentiableFunctionInsts.erase(dfi);
  return dfi;
}

DifferentiableFunctionExpr *
ADContext::findDifferentialOperator(DifferentiableFunctionInst *inst) {
  return inst->getLoc().getAsASTNode<DifferentiableFunctionExpr>();
}

} // end namespace autodiff
} // end namespace swift
