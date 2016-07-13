//===-------  NonAtomicRC.cpp - Use non-atomic reference counting  -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "non-atomic-rc"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/EscapeAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"

STATISTIC(NumNonAtomicRC, "Number of non-atomic RC operations");

llvm::cl::opt<bool> PerformNonAtomicOpts(
    "non-atomic-opts", llvm::cl::init(true),
    llvm::cl::desc("Enable non-atomic reference counting optimizations"));

using namespace swift;

namespace {

typedef SILAnalysis::InvalidationKind StateChanges;

class NonAtomicRCTransformer {
  SILPassManager *PM;
  SILFunction *F;
  EscapeAnalysis::ConnectionGraph *ConGraph;
  EscapeAnalysis *EA;
  RCIdentityFunctionInfo *RCIFI;

public:
  NonAtomicRCTransformer(SILPassManager *PM, SILFunction *F,
                         EscapeAnalysis::ConnectionGraph *ConGraph,
                         EscapeAnalysis *EA, RCIdentityFunctionInfo *RCIFI)
      : PM(PM), F(F), ConGraph(ConGraph), EA(EA), RCIFI(RCIFI) {}

  StateChanges process();

private:
  StateChanges processNonEscapingRefCountingInsts();
  bool isEligableRefCountingInst(SILInstruction *I);
  StateChanges tryNonAtomicRC(SILInstruction *I);
};

/// Try to strip all casts and projections as long they preserve the
/// uniqueness of the value.
static SILValue stripUniquenessPreservingCastsAndProjections(SILValue V) {
  while (V) {
    V = stripAddressProjections(V);
    auto V2 = stripCasts(V);
    if (auto *UAC = dyn_cast<UncheckedAddrCastInst>(V2)) {
      if (UAC->getType() ==
          SILType::getNativeObjectType(UAC->getModule().getASTContext())
              .getAddressType()) {
        V = UAC->getOperand();
        continue;
      }
    }
    if (V2 == V)
      return V;
    V = V2;
  }
  return V;
}

static void markAsNonAtomic(RefCountingInst *I) {
  SILValue Op = I->getOperand(0);
  I->setNonAtomic();
}

/// Is it a reference counting instruction that is eligable to
/// be promoted to a non-atomic version?
bool NonAtomicRCTransformer::isEligableRefCountingInst(SILInstruction *I) {
  return isa<RefCountingInst>(I) && !cast<RefCountingInst>(I)->isNonAtomic();
}

/// Obtain the underlying object by stripoping casts as well
/// index and address projections.
static SILValue obtainUnderlyingObject(SILValue V) {
  while (true) {
    SILValue V2 =
        stripIndexingInsts(stripUniquenessPreservingCastsAndProjections(V));
    if (V2 == V)
      return V2;
    V = V2;
  }
}

/// Check if the the parameter \V is based on a local object, e.g. it is an
/// allocation instruction or a struct/tuple constructed from the local objects.
/// Returns a found local object. If a local object was not found, returns an
/// empty SILValue.
static SILValue getLocalObject(SILValue V) {
  // It should be a local object.
  V = obtainUnderlyingObject(V);
  if (isa<AllocationInst>(V))
    return V;
  if (isa<LiteralInst>(V))
    return V;
  // Look through strong_pin instructions.
  if (auto *SPI = dyn_cast<StrongPinInst>(V))
    return getLocalObject(SPI->getOperand());
  if (auto *SI = dyn_cast<StructInst>(V)) {
    for (auto &Op : SI->getAllOperands())
      if (!getLocalObject(Op.get()))
        return SILValue();
    return V;
  }
  if (auto *TI = dyn_cast<TupleInst>(V)) {
    for (auto &Op : TI->getAllOperands())
      if (!getLocalObject(Op.get()))
        return SILValue();
    return V;
  }
  return SILValue();
}

/// Try to promote a reference counting instruction to its non-atomic
/// variant.
StateChanges NonAtomicRCTransformer::tryNonAtomicRC(SILInstruction *I) {
  assert(isa<RefCountingInst>(I));
  auto *RCInst = cast<RefCountingInst>(I);

  // For the EscapeAnalysis to be correct, it should be a local object.
  auto LocalObject = getLocalObject(RCInst->getOperand(0));
  if (!LocalObject)
    return SILAnalysis::InvalidationKind::Nothing;

  auto *Node = ConGraph->getNodeOrNull(LocalObject, EA);
  if (!Node)
    return SILAnalysis::InvalidationKind::Nothing;

  // As long as the value does not escape the function, it is fine.
  if (Node->escapesInsideFunction(false))
    return SILAnalysis::InvalidationKind::Nothing;

  // This value does not escape, which means that it is
  // thread-local.
  // Use non-atomic RC instructions for it.
  markAsNonAtomic(RCInst);
  NumNonAtomicRC++;
  DEBUG(llvm::dbgs() << "Marking the RC instruction as non-atomic:\n";
        RCInst->dumpInContext(););
  return SILAnalysis::InvalidationKind::Instructions;
}

// Perform a basic scan over a function, look for RC instructions.
// If any of these instruction have a non-escaping operand, it
// is safe to make them non-atomic.
StateChanges NonAtomicRCTransformer::processNonEscapingRefCountingInsts() {
  StateChanges Changes = SILAnalysis::InvalidationKind::Nothing;
  // Search the whole function for stack promotable allocations.
  for (SILBasicBlock &BB : *F) {
    for (auto Iter = BB.begin(); Iter != BB.end();) {
      // The allocation instruction may be moved, so increment Iter prior to
      // doing the optimization.
      SILInstruction *I = &*Iter++;
      if (isEligableRefCountingInst(I)) {
        Changes = StateChanges(Changes | tryNonAtomicRC(I));
      }
    }
  }
  return Changes;
}

StateChanges NonAtomicRCTransformer::process() {
  DEBUG(llvm::dbgs() << "\nAbout to process function:\n"; F->dump());
  auto ChangesRefCounting = processNonEscapingRefCountingInsts();

  if (ChangesRefCounting) {
    DEBUG(llvm::dbgs() << "\n\nFunction after the transformation:"; F->dump());
  }

  return StateChanges(ChangesRefCounting);
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

class NonAtomicRC : public SILFunctionTransform {
public:
  NonAtomicRC() {}

private:
  /// The entry point to the transformation.
  void run() override {
    if (!PerformNonAtomicOpts)
      return;

    DEBUG(llvm::dbgs() << "** Start NonAtomicRC for " << getFunction()->getName()
                       << " **\n");

    auto *EA = PM->getAnalysis<EscapeAnalysis>();
    auto *RCIA = PM->getAnalysis<RCIdentityAnalysis>();

    SILFunction *F = getFunction();
    auto *ConGraph = EA->getConnectionGraph(F);
    if (ConGraph) {
      NonAtomicRCTransformer Transformer(PM, F, ConGraph, EA, RCIA->get(F));
      auto Changes = Transformer.process();
      if (Changes) {
        PM->invalidateAnalysis(F, Changes);
      }
    }
    DEBUG(llvm::dbgs() << "** End NonAtomicRC for " << getFunction()->getName()
                       << " **\n");

  }

  StringRef getName() override { return "NonAtomicRC"; }
};

} // end anonymous namespace

SILTransform *swift::createNonAtomicRC() { return new NonAtomicRC(); }
