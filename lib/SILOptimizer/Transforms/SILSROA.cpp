//===--- SILSROA.cpp - Scalar Replacement of Aggregates  ------------------===//
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
// Change aggregate values into scalar values. Currently it takes every
// allocation and chops them up into their smallest non-captured pieces.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-sroa"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Debug.h"
#include <type_traits>

using namespace swift;

STATISTIC(NumEscapingAllocas, "Number of aggregate allocas not chopped up "
          "due to uses.");
STATISTIC(NumChoppedAllocas, "Number of chopped up aggregate allocas.");
STATISTIC(NumUnhandledAllocas, "Number of non struct, tuple allocas.");

namespace {

class SROAMemoryUseAnalyzer {
  // The allocation we are analyzing.
  AllocStackInst *AI;

  // Loads from AI.
  llvm::SmallVector<LoadInst *, 4> Loads;
  // Stores to AI.
  llvm::SmallVector<StoreInst *, 4> Stores;
  // Instructions which extract from aggregates.
  llvm::SmallVector<SingleValueInstruction *, 4> ExtractInsts;

  // TupleType if we are visiting a tuple.
  TupleType *TT = nullptr;
  // StructDecl if we are visiting a struct.
  StructDecl *SD = nullptr;
public:
  SROAMemoryUseAnalyzer(AllocStackInst *AI) : AI(AI) {
    assert(AI && "AI should never be null here.");
  }

  bool analyze();
  void chopUpAlloca(std::vector<AllocStackInst *> &Worklist);

private:
  SILValue createAgg(SILBuilder &B, SILLocation Loc, SILType Ty,
                     ArrayRef<SILValue> Elements);
  SILValue createAggProjection(SILBuilder &B, SILLocation Loc,
                               SILValue Operand, unsigned EltNo);
  unsigned getEltNoForProjection(SILInstruction *Inst);
  void createAllocas(llvm::SmallVector<AllocStackInst *, 4> &NewAllocations);
};

} // end anonymous namespace

SILValue 
SROAMemoryUseAnalyzer::createAgg(SILBuilder &B, SILLocation Loc,
                                 SILType Ty,
                                 ArrayRef<SILValue> Elements) {
  if (TT)
    return B.createTuple(Loc, Ty, Elements);

  assert(SD && "SD must not be null here since it or TT must be set to call"
         " this method.");
  return B.createStruct(Loc, Ty, Elements);
}

SILValue
SROAMemoryUseAnalyzer::createAggProjection(SILBuilder &B, SILLocation Loc,
                                           SILValue Operand,
                                           unsigned EltNo) {
  if (TT)
    return B.createTupleExtract(Loc, Operand, EltNo);

  assert(SD && "SD should not be null since either it or TT must be set at "
         "this point.");

  auto Properties = SD->getStoredProperties();
  unsigned Counter = 0;
  for (auto *D : Properties)
    if (Counter++ == EltNo)
      return B.createStructExtract(Loc, Operand, D);
  llvm_unreachable("Unknown field.");
}

unsigned SROAMemoryUseAnalyzer::getEltNoForProjection(SILInstruction *Inst) {
  if (TT)
    return cast<TupleElementAddrInst>(Inst)->getFieldNo();

  assert(SD && "SD should not be null since either it or TT must be set at "
         "this point.");
  StructElementAddrInst *SEA = cast<StructElementAddrInst>(Inst);
  VarDecl *Field = SEA->getField();
  unsigned EltNo = 0;
  for (auto *D : SD->getStoredProperties()) {
    if (D == Field)
      return EltNo;
    ++EltNo;
  }
  llvm_unreachable("Unknown field.");
}

bool SROAMemoryUseAnalyzer::analyze() {
  // We only know how to split structs and tuples... So if we have a scalar or a
  // different sort of aggregate, bail.
  SILType Type = AI->getType();

  TT = Type.getAs<TupleType>();
  SD = Type.getStructOrBoundGenericStruct();
  bool HasUnrefField = AI->getElementType().aggregateHasUnreferenceableStorage();

  // Check that the allocated type is a struct or a tuple and that there are
  // no unreferenced fields.
  if (HasUnrefField || (!TT && !SD)) {
    ++NumUnhandledAllocas;
    return false;
  }

  bool hasBenefit = false;

  // Go through uses of the memory allocation of AI...
  for (auto *Operand : getNonDebugUses(SILValue(AI))) {
    SILInstruction *User = Operand->getUser();
    LLVM_DEBUG(llvm::dbgs() << "    Visiting use: " << *User);

    // If we store the alloca pointer, we cannot analyze its uses so bail...
    // It is ok if we store into the alloca pointer though.
    if (auto *SI = dyn_cast<StoreInst>(User)) {
      if (SI->getDest() == AI) {
        LLVM_DEBUG(llvm::dbgs() << "        Found a store into the "
                                   "projection.\n");
        Stores.push_back(SI);
        SILValue Src = SI->getSrc();
        if (isa<StructInst>(Src) || isa<TupleInst>(Src))
          hasBenefit = true;
        continue;
      } else {
        LLVM_DEBUG(llvm::dbgs() << "        Found a store of the "
                                   "projection pointer. Escapes!.\n");
        ++NumEscapingAllocas;
        return false;
      }
    }

    // If the use is a load, keep track of it for splitting later...
    if (auto *LI = dyn_cast<LoadInst>(User)) {
      LLVM_DEBUG(llvm::dbgs() << "        Found a load of the projection.\n");
      Loads.push_back(LI);
      for (auto useIter = LI->use_begin(), End = LI->use_end();
           !hasBenefit && useIter != End; useIter++) {
        hasBenefit = (isa<StructExtractInst>(useIter->get()) ||
                      isa<TupleExtractInst>(useIter->get()));
      }
      continue;
    }

    // If the use is a struct_element_addr, add it to the worklist so we check
    // if it or one of its descendants escape.
    if (auto *ASI = dyn_cast<StructElementAddrInst>(User)) {
      LLVM_DEBUG(llvm::dbgs() << "        Found a struct subprojection!\n");
      ExtractInsts.push_back(ASI);
      hasBenefit = true;
      continue;
    }

    // If the use is a tuple_element_addr, add it to the worklist so we check
    // if it or one of its descendants escape.
    if (auto *TSI = dyn_cast<TupleElementAddrInst>(User)) {
      LLVM_DEBUG(llvm::dbgs() << "        Found a tuple subprojection!\n");
      ExtractInsts.push_back(TSI);
      hasBenefit = true;
      continue;
    }

    if (isa<DeallocStackInst>(User)) {
      // We can ignore the dealloc_stack.
      continue;
    }
    
    // Otherwise we do not understand this instruction, so bail.
    LLVM_DEBUG(llvm::dbgs() <<"        Found unknown user, pointer escapes!\n");
    ++NumEscapingAllocas;
    return false;
  }

  // Analysis was successful. We can break up this allocation!
  ++NumChoppedAllocas;
  return hasBenefit;
}

void
SROAMemoryUseAnalyzer::
createAllocas(llvm::SmallVector<AllocStackInst *, 4> &NewAllocations) {
  SILBuilderWithScope B(AI);
  SILType Type = AI->getType().getObjectType();
  // Intentionally dropping the debug info.
  // FIXME: VarInfo needs to be extended with fragment info to support this.
  SILLocation Loc = RegularLocation::getAutoGeneratedLocation();
  if (TT) {
    for (unsigned EltNo : indices(TT->getElementTypes())) {
      SILType EltTy = Type.getTupleElementType(EltNo);
      NewAllocations.push_back(B.createAllocStack(Loc, EltTy, {}));
    }
  } else {
    assert(SD && "SD should not be null since either it or TT must be set at "
           "this point.");
    SILModule &M = AI->getModule();
    for (auto *D : SD->getStoredProperties())
      NewAllocations.push_back(
          B.createAllocStack(Loc, Type.getFieldType(D, M), {}));
  }
}

void SROAMemoryUseAnalyzer::chopUpAlloca(std::vector<AllocStackInst *> &Worklist) {
  // Create allocations for this instruction.
  llvm::SmallVector<AllocStackInst *, 4> NewAllocations;
  createAllocas(NewAllocations);
  // Add the new allocations to the worklist for recursive processing.
  //
  // TODO: Change this into an assert. For some reason I am running into compile
  // issues when I try it now.
  for (auto *AI : NewAllocations)
    Worklist.push_back(AI);

  // Change any aggregate loads into field loads + aggregate structure.
  for (auto *LI : Loads) {
    SILBuilderWithScope B(LI);
    llvm::SmallVector<SILValue, 4> Elements;
    for (auto *NewAI : NewAllocations)
      Elements.push_back(B.createLoad(LI->getLoc(), NewAI,
                                      LoadOwnershipQualifier::Unqualified));
    SILValue Agg = createAgg(B, LI->getLoc(), LI->getType().getObjectType(),
                             Elements);
    LI->replaceAllUsesWith(Agg);
    LI->eraseFromParent();
  }

  // Change any aggregate stores into extracts + field stores.
  for (auto *SI : Stores) {
    SILBuilderWithScope B(SI);
    for (unsigned EltNo : indices(NewAllocations))
      B.createStore(SI->getLoc(),
                    createAggProjection(B, SI->getLoc(), SI->getSrc(), EltNo),
                    NewAllocations[EltNo],
                    StoreOwnershipQualifier::Unqualified);
    SI->eraseFromParent();
  }

  // Forward any field extracts to the new allocation.
  for (auto *Ext : ExtractInsts) {
    AllocStackInst *NewValue = NewAllocations[getEltNoForProjection(Ext)];
    Ext->replaceAllUsesWith(NewValue);
    Ext->eraseFromParent();
  }

  // Find all dealloc instructions for AI and then chop them up.
  llvm::SmallVector<DeallocStackInst *, 4> ToRemove;
  for (auto *Operand : getNonDebugUses(SILValue(AI))) {
    SILInstruction *User = Operand->getUser();
    SILBuilderWithScope B(User);

    // If the use is a DSI, add it to our memory analysis so that if we can chop
    // up allocas, we also chop up the relevant dealloc stack insts.
    if (auto *DSI = dyn_cast<DeallocStackInst>(User)) {
      LLVM_DEBUG(llvm::dbgs() << "        Found DeallocStackInst!\n");
      // Create the allocations in reverse order.
      for (auto *NewAI : swift::reversed(NewAllocations))
        B.createDeallocStack(DSI->getLoc(), SILValue(NewAI));
      ToRemove.push_back(DSI);
    }
  }

  // Remove the old DeallocStackInst instructions.
  for (auto *DSI : ToRemove) {
      DSI->eraseFromParent();
  }

  eraseFromParentWithDebugInsts(AI);
}

static bool runSROAOnFunction(SILFunction &Fn) {
  std::vector<AllocStackInst *> Worklist;
  bool Changed = false;

  // For each basic block BB in Fn...
  for (auto &BB : Fn)
    // For each instruction in BB...
    for (auto &I : BB)
      // If the instruction is an alloc stack inst, add it to the worklist.
      if (auto *AI = dyn_cast<AllocStackInst>(&I))
        if (shouldExpand(Fn.getModule(), AI->getElementType()))
          Worklist.push_back(AI);

  while (!Worklist.empty()) {
    AllocStackInst *AI = Worklist.back();
    Worklist.pop_back();

    SROAMemoryUseAnalyzer Analyzer(AI);

    if (!Analyzer.analyze())
      continue;

    Changed = true;
    Analyzer.chopUpAlloca(Worklist);
  }
  return Changed;
}

namespace {
class SILSROA : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    SILFunction *F = getFunction();

    // FIXME: We should be able to handle ownership.
    if (F->hasOwnership())
      return;

    LLVM_DEBUG(llvm::dbgs() << "***** SROA on function: " << F->getName()
                            << " *****\n");

    if (runSROAOnFunction(*F))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

};
} // end anonymous namespace


SILTransform *swift::createSROA() {
  return new SILSROA();
}
