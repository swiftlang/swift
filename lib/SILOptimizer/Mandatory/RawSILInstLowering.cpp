//===--- RawSILInstLowering.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "raw-sil-inst-lowering"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/Statistic.h"

STATISTIC(NumAssignRewritten, "Number of assigns rewritten");

using namespace swift;

/// Emit the sequence that an assign instruction lowers to once we know
/// if it is an initialization or an assignment.  If it is an assignment,
/// a live-in value can be provided to optimize out the reload.
static void lowerAssignInstruction(SILBuilderWithScope &B, AssignInst *Inst) {
  LLVM_DEBUG(llvm::dbgs() << "  *** Lowering [isInit="
                          << unsigned(Inst->getInitKind())
                          << "]: " << *Inst << "\n");

  ++NumAssignRewritten;

  SILValue Src = Inst->getSrc();
  SILLocation Loc = Inst->getLoc();
  PartialInitializationKind initKind = Inst->getInitKind();

  // Unknown initKind is considered unprocessed. Just lower it as
  // NotInitialization
  if (initKind == PartialInitializationKind::Unknown) {
    initKind = PartialInitializationKind::IsNotInitialization;
  }

  if (initKind == PartialInitializationKind::IsInitialization ||
      Inst->getDest()->getType().isTrivial(Inst->getModule())) {

    // If this is an initialization, or the storage type is trivial, we
    // can just replace the assignment with a store.
    assert(initKind != PartialInitializationKind::IsReinitialization);
    B.createTrivialStoreOr(Loc, Src, Inst->getDest(),
                           StoreOwnershipQualifier::Init);
    Inst->eraseFromParent();
    return;
  }

  if (initKind == PartialInitializationKind::IsReinitialization) {
    // We have a case where a convenience initializer on a class
    // delegates to a factory initializer from a protocol extension.
    // Factory initializers give us a whole new instance, so the existing
    // instance, which has not been initialized and never will be, must be
    // freed using dealloc_partial_ref.
    SILValue Pointer =
        B.createLoad(Loc, Inst->getDest(), LoadOwnershipQualifier::Take);
    B.createStore(Loc, Src, Inst->getDest(), StoreOwnershipQualifier::Init);

    auto MetatypeTy = CanMetatypeType::get(
        Inst->getDest()->getType().getASTType(), MetatypeRepresentation::Thick);
    auto SILMetatypeTy = SILType::getPrimitiveObjectType(MetatypeTy);
    SILValue Metatype = B.createValueMetatype(Loc, SILMetatypeTy, Pointer);

    B.createDeallocPartialRef(Loc, Pointer, Metatype);
    Inst->eraseFromParent();
    return;
  }

  assert(initKind == PartialInitializationKind::IsNotInitialization);
  // Otherwise, we need to replace the assignment with a store [assign] which
  // lowers to the load/store/release dance. Note that the new value is already
  // considered to be retained (by the semantics of the storage type),
  // and we're transferring that ownership count into the destination.

  B.createStore(Inst->getLoc(), Src, Inst->getDest(),
                StoreOwnershipQualifier::Assign);
  Inst->eraseFromParent();
}

/// lowerRawSILOperations - There are a variety of raw-sil instructions like
/// 'assign' that are only used by this pass.  Now that definite initialization
/// checking is done, remove them.
static bool lowerRawSILOperations(SILFunction &Fn) {
  bool Changed = false;
  for (auto &BB : Fn) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      SILInstruction *Inst = &*I;
      ++I;

      // Lower 'assign' depending on initialization kind defined by definite
      // initialization.
      //
      // Unknown is considered unprocessed and is just NotInitialization
      // Initialization becomes an init store instruction
      // Reinititialization becomes a load, store, and a dealloc_partial_ref
      // NotInitialization becomes a take load, and an init store
      if (auto *AI = dyn_cast<AssignInst>(Inst)) {
        SILBuilderWithScope B(AI);
        lowerAssignInstruction(B, AI);
        // Assign lowering may split the block. If it did,
        // reset our iteration range to the block after the insertion.
        if (B.getInsertionBB() != &BB)
          I = E;
        Changed = true;
        continue;
      }

      // mark_uninitialized just becomes a noop, resolving to its operand.
      if (auto *MUI = dyn_cast<MarkUninitializedInst>(Inst)) {
        MUI->replaceAllUsesWith(MUI->getOperand());
        MUI->eraseFromParent();
        Changed = true;
        continue;
      }

      // mark_function_escape just gets zapped.
      if (isa<MarkFunctionEscapeInst>(Inst)) {
        Inst->eraseFromParent();
        Changed = true;
        continue;
      }
    }
  }
  return Changed;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class RawSILInstLowering : public SILFunctionTransform {
  void run() override {
    // Do not try to relower raw instructions in canonical SIL. There won't be
    // any there.
    if (getFunction()->wasDeserializedCanonical()) {
      return;
    }

    // Lower raw-sil only instructions used by this pass, like "assign".
    if (lowerRawSILOperations(*getFunction()))
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
  }
};

} // end anonymous namespace

SILTransform *swift::createRawSILInstLowering() {
  return new RawSILInstLowering();
}
