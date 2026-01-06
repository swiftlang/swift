//===--- AliasAnalysis.cpp - SIL Alias Analysis ---------------------------===//
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

#define DEBUG_TYPE "sil-aa"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "llvm/Support/Debug.h"
#include "swift/SILOptimizer/OptimizerBridging.h"

using namespace swift;

// Bridging functions.
static BridgedAliasAnalysis::InitFn initFunction = nullptr;
static BridgedAliasAnalysis::DestroyFn destroyFunction = nullptr;
static BridgedAliasAnalysis::GetMemEffectFn getMemEffectsFunction = nullptr;
static BridgedAliasAnalysis::Escaping2InstFn isObjReleasedFunction = nullptr;
static BridgedAliasAnalysis::Escaping2ValIntFn isAddrVisibleFromObjFunction = nullptr;
static BridgedAliasAnalysis::MayAliasFn mayAliasFunction = nullptr;

void AliasAnalysis::initSwiftSpecificData() {
  if (initFunction)
    initFunction({this}, sizeof(swiftSpecificData));
}

AliasAnalysis::~AliasAnalysis() {
  if (destroyFunction)
    destroyFunction({this});
}

bool AliasAnalysis::canApplyDecrementRefCount(FullApplySite FAS, SILValue Ptr) {
  // Treat applications of no-return functions as decrementing ref counts. This
  // causes the apply to become a sink barrier for ref count increments.
  if (FAS.isCalleeNoReturn())
    return true;

  /// If the pointer cannot escape to the function we are done.
  bool result = isObjectReleasedByInst(Ptr, FAS.getInstruction());
  return result;
}

bool AliasAnalysis::canBuiltinDecrementRefCount(BuiltinInst *BI, SILValue Ptr) {
  return isObjectReleasedByInst(Ptr, BI);
}

namespace {

class AliasAnalysisContainer : public FunctionAnalysisBase<AliasAnalysis> {
  SILPassManager *PM = nullptr;

public:
  AliasAnalysisContainer() : FunctionAnalysisBase(SILAnalysisKind::Alias) {}

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind K) override {
    return K & InvalidationKind::Instructions;
  }

  virtual void invalidate(SILFunction *f,
                          SILAnalysis::InvalidationKind k) override {
    if (k & InvalidationKind::Effects) {
      FunctionAnalysisBase::invalidate();
    } else {
      FunctionAnalysisBase::invalidate(f, k);
    }
  }

  // Computes loop information for the given function using dominance
  // information.
  virtual std::unique_ptr<AliasAnalysis>
  newFunctionAnalysis(SILFunction *F) override {
    assert(PM && "dependent analysis not initialized");
    return std::make_unique<AliasAnalysis>(PM);
  }

  virtual void initialize(SILPassManager *PM) override {
    this->PM = PM;
  }
};

} // end anonymous namespace

SILAnalysis *swift::createAliasAnalysis(SILModule *M) {
  return new AliasAnalysisContainer();
}

//===----------------------------------------------------------------------===//
//                            Swift Bridging
//===----------------------------------------------------------------------===//

void BridgedAliasAnalysis::registerAnalysis(InitFn initFn,
                                            DestroyFn destroyFn,
                                            GetMemEffectFn getMemEffectsFn,
                                            Escaping2InstFn isObjReleasedFn,
                                            Escaping2ValIntFn isAddrVisibleFromObjFn,
                                            MayAliasFn mayAliasFn) {
  initFunction = initFn;
  destroyFunction = destroyFn;
  getMemEffectsFunction = getMemEffectsFn;
  isObjReleasedFunction = isObjReleasedFn;
  isAddrVisibleFromObjFunction = isAddrVisibleFromObjFn;
  mayAliasFunction = mayAliasFn;
}

MemoryBehavior AliasAnalysis::computeMemoryBehavior(SILInstruction *toInst, SILValue addr) {
  if (getMemEffectsFunction) {
    return (MemoryBehavior)getMemEffectsFunction({PM->getSwiftPassInvocation()},
                                                 {this},
                                                 {addr},
                                                 {toInst->asSILNode()});
  }
  return MemoryBehavior::MayHaveSideEffects;
}

bool AliasAnalysis::isObjectReleasedByInst(SILValue obj, SILInstruction *inst) {
  if (isObjReleasedFunction) {
    return isObjReleasedFunction({PM->getSwiftPassInvocation()}, {this}, {obj}, {inst->asSILNode()});
  }
  return true;
}

bool AliasAnalysis::isAddrVisibleFromObject(SILValue addr, SILValue obj) {
  if (isAddrVisibleFromObjFunction) {
    return isAddrVisibleFromObjFunction({PM->getSwiftPassInvocation()}, {this}, {addr}, {obj});
  }
  return true;
}

bool AliasAnalysis::mayAlias(SILValue lhs, SILValue rhs) {
  if (mayAliasFunction) {
    return mayAliasFunction({PM->getSwiftPassInvocation()}, {this}, {lhs}, {rhs});
  }
  return true;
}
