//===--- AllocStackHoisting.cpp - Hoist alloc_stack instructions ----------===//
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

#define DEBUG_TYPE "alloc-stack-hoisting"

#include "swift/IRGen/IRGenSILPasses.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILArgument.h"

#include "IRGenModule.h"
#include "NonFixedTypeInfo.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

using namespace swift;

llvm::cl::opt<bool> SILUseStackSlotMerging(
    "sil-merge-stack-slots", llvm::cl::init(true),
    llvm::cl::desc("Merge generic alloc_stack instructions"));

/// Hoist generic alloc_stack instructions to the entry basic block and merge
/// alloc_stack instructions if their users span non-overlapping live-ranges.
///
/// This helps avoid llvm.stacksave/stackrestore intrinsic calls during code
/// generation. IRGen will only dynamic alloca instructions if the alloc_stack
/// is in the entry block but will emit a dynamic alloca and
/// llvm.stacksave/stackrestore for all other basic blocks.
///
/// Merging alloc_stack instructions saves code size and stack size.

/// An alloc_stack instructions is hoistable if it is of generic type and the
/// type parameter is not dependent on an opened type.
static bool isHoistable(AllocStackInst *Inst, irgen::IRGenModule &Mod) {
  auto SILTy = Inst->getType();
  // We don't need to hoist types that have reference semantics no dynamic
  // alloca will be generated as they are fixed size.
  if (SILTy.hasReferenceSemantics())
    return false;

  // Only hoist types that are dynamically sized (generics and resilient types).
  auto &TI = Mod.getTypeInfo(SILTy);
  if (TI.isFixedSize())
    return false;

  // Don't hoist generics with opened archetypes. We would have to hoist the
  // open archetype instruction which might not be possible.
  return Inst->getTypeDependentOperands().empty();
}

/// A partition of alloc_stack instructions.
///
/// Initially, a partition contains alloc_stack instructions of one type.
/// After merging non-overlapping alloc_stack live ranges, a partition contains
/// a set of alloc_stack instructions that can be assigned a single stack
/// location.
namespace {
class Partition {
public:
  SmallVector<AllocStackInst *, 4> Elts;

  Partition(AllocStackInst *A) : Elts(1, A) {}
  Partition() {}

  /// Assign a single alloc_stack instruction to all the alloc_stacks in the
  /// partition.
  ///
  /// This assumes that the live ranges of the alloc_stack instructions are
  /// non-overlapping.
  void assignStackLocation(SmallVectorImpl<SILInstruction *> &FunctionExits);
};
} // end anonymous namespace

/// Erases all dealloc_stack users of an alloc_stack
static void eraseDeallocStacks(AllocStackInst *AllocStack) {
  // Delete dealloc_stacks.
  SmallVector<DeallocStackInst *, 16> DeallocStacksToDelete;
  for (auto *U : AllocStack->getUses()) {
    if (auto *DeallocStack = dyn_cast<DeallocStackInst>(U->getUser()))
      DeallocStacksToDelete.push_back(DeallocStack);
  }
  for (auto *D : DeallocStacksToDelete)
    D->eraseFromParent();
}

/// Inserts a dealloc_stack at all the function exits.
static void
insertDeallocStackAtEndOf(SmallVectorImpl<SILInstruction *> &FunctionExits,
                          AllocStackInst *AllocStack) {
  // Insert dealloc_stack in the exit blocks.
  for (auto *Exit : FunctionExits) {
    SILBuilderWithScope Builder(Exit);
    Builder.createDeallocStack(AllocStack->getLoc(), AllocStack);
  }
}

/// Hack to workaround a clang LTO bug.
LLVM_ATTRIBUTE_NOINLINE
void moveAllocStackToBeginningOfBlock(AllocStackInst* AS, SILBasicBlock *BB) {
  AS->moveFront(BB);
}

/// Assign a single alloc_stack instruction to all the alloc_stacks in the
/// partition.
void Partition::assignStackLocation(
    SmallVectorImpl<SILInstruction *> &FunctionExits) {
  assert(!Elts.empty() && "Must have a least one location");
  // The assigned location is the first alloc_stack in our partition.
  auto *AssignedLoc = Elts[0];

  // Move this assigned location to the beginning of the entry block.
  auto *EntryBB = AssignedLoc->getFunction()->getEntryBlock();
  moveAllocStackToBeginningOfBlock(AssignedLoc, EntryBB);

  // Erase the dealloc_stacks.
  eraseDeallocStacks(AssignedLoc);

  // Insert a new dealloc_stack at the exit(s) of the function.
  insertDeallocStackAtEndOf(FunctionExits, AssignedLoc);

  // Rewrite all the other alloc_stacks in the partition to use the assigned
  // location.
  for (auto *AllocStack : Elts) {
    if (AssignedLoc == AllocStack) continue;
    eraseDeallocStacks(AllocStack);
    AllocStack->replaceAllUsesWith(AssignedLoc);
    AllocStack->eraseFromParent();
  }
}

/// Returns a single dealloc_stack user of the alloc_stack or nullptr otherwise.
static SILInstruction *getSingleDeallocStack(AllocStackInst *ASI) {
  return ASI->getSingleDeallocStack();
}

namespace {
/// Compute liveness for the partition to allow for an interference check
/// between two alloc_stack instructions.
///
/// For now liveness is computed and this just performs a simple check
/// whether two regions of alloc_stack instructions might overlap.
class Liveness {
public:
  Liveness(Partition &P) {}

  /// Check whether the live ranges of the two alloc_stack instructions
  /// might overlap.
  ///
  /// Currently this does not use a liveness analysis. Rather we check that for
  /// both alloc_stack we have:
  /// * a single dealloc_stack user
  /// * the dealloc_stack is in the same basic block
  /// If the alloc_stack instructions are in different basic blocks we know that
  /// the live-ranges can't overlap.
  /// If they are in the same basic block we scan the basic block to determine
  /// whether one dealloc_stack dominates the other alloc_stack. If this is the
  /// case the live ranges can't overlap.
  bool mayOverlap(AllocStackInst *A, AllocStackInst *B) {
    assert(A != B);

    // Check that we have a single dealloc_stack user in the same block.
    auto *singleDeallocA = getSingleDeallocStack(A);
    if (singleDeallocA == nullptr ||
        singleDeallocA->getParent() != A->getParent())
      return true;
    auto *singleDeallocB = getSingleDeallocStack(B);
    if (singleDeallocB == nullptr ||
        singleDeallocB->getParent() != B->getParent())
      return true;

    // Different basic blocks.
    if (A->getParent() != B->getParent())
      return false;
    bool ALive = false;
    bool BLive = false;
    for (auto &Inst : *A->getParent()) {
      if (A == &Inst) {
        ALive = true;
      } else if (singleDeallocA == &Inst) {
        ALive = false;
      } else if (B == &Inst) {
        BLive = true;
      } else if (singleDeallocB == &Inst) {
        BLive = false;
      }

      if (ALive && BLive)
        return true;
    }
    return false;
  }
};
} // end anonymous namespace

namespace {
/// Merge alloc_stack instructions.
///
/// This merges alloc_stack instructions of one type by:
/// * building partitions of alloc_stack instructions of one type
/// * merging alloc_stack instructions in each partition into one alloc_stack
///   if the live ranges spanned by the alloc_stack users are known not to
///   overlap.
class MergeStackSlots {
  /// Contains partitions of alloc_stack instructions by type.
  SmallVector<Partition, 2> PartitionByType;
  /// The function exits.
  SmallVectorImpl<SILInstruction *> &FunctionExits;

public:
  MergeStackSlots(SmallVectorImpl<AllocStackInst *> &AllocStacks,
                  SmallVectorImpl<SILInstruction *> &FuncExits);

  /// Merge alloc_stack instructions if possible and hoist them to the entry
  /// block.
  void mergeSlots();
};
} // end anonymous namespace

MergeStackSlots::MergeStackSlots(SmallVectorImpl<AllocStackInst *> &AllocStacks,
                                 SmallVectorImpl<SILInstruction *> &FuncExits)
    : FunctionExits(FuncExits) {
  // Build initial partitions based on the type.
  llvm::DenseMap<SILType, unsigned> TypeToPartitionMap;
  for (auto *AS : AllocStacks) {
    auto Ty = AS->getType();
    auto It = TypeToPartitionMap.find(Ty);
    if (It != TypeToPartitionMap.end()) {
      PartitionByType[It->second].Elts.push_back(AS);
    } else {
      PartitionByType.push_back(Partition(AS));
      TypeToPartitionMap[Ty] = PartitionByType.size() - 1;
    }
  }
}

/// Merge alloc_stack instructions if possible and hoist them to the entry
/// block.
void MergeStackSlots::mergeSlots() {
  for (auto &PartitionOfOneType : PartitionByType) {
    Liveness Live(PartitionOfOneType);

    // Partitions that are know to contain non-overlapping alloc_stack
    // live-ranges.
    SmallVector<Partition, 4> DisjointPartitions(1, Partition());

    // Look at all the alloc_stacks of one type.
    for (auto *CurAllocStack : PartitionOfOneType.Elts) {
      bool FoundAPartition = false;
      // Check if we can add it to an existing partition that we have show to be
      // non-interfering.
      for (auto &CandidateP : DisjointPartitions) {
        // If the candidate partition is empty (the very first time we look at an
        // alloc_stack) we can just add the alloc_stack.
        if (CandidateP.Elts.empty()) {
          CandidateP.Elts.push_back(CurAllocStack);
          FoundAPartition = true;
          break;
        }
        // Otherwise, we check interference of the current alloc_stack with the
        // candidate partition.
        bool InterferesWithCandidateP = false;
        for (auto *AllocStackInPartition : CandidateP.Elts) {
          if (Live.mayOverlap(AllocStackInPartition, CurAllocStack)) {
            InterferesWithCandidateP = true;
            break;
          }
        }
        // No interference add the current alloc_stack to the candidate
        // partition.
        if (!InterferesWithCandidateP) {
          CandidateP.Elts.push_back(CurAllocStack);
          FoundAPartition = true;
          break;
        }
        // Otherwise, we look at the next partition.
      }
      // If not partition was found add a new one.
      if (!FoundAPartition) {
        DisjointPartitions.push_back(Partition(CurAllocStack));
      }
    }

    // Assign stack locations to disjoint partition hoisting alloc_stacks to the
    // entry block at the same time.
    for (auto &Par : DisjointPartitions) {
      Par.assignStackLocation(FunctionExits);
    }
  }
}


namespace {
/// Hoist alloc_stack instructions to the entry block and merge them.
class HoistAllocStack {
  /// The function to process.
  SILFunction *F;
  /// The current IRGenModule.
  irgen::IRGenModule &IRGenMod;

  SmallVector<AllocStackInst *, 16> AllocStackToHoist;
  SmallVector<SILInstruction *, 8> FunctionExits;

public:
  HoistAllocStack(SILFunction *F, irgen::IRGenModule &Mod)
      : F(F), IRGenMod(Mod) {}

  /// Try to hoist generic alloc_stack instructions to the entry block.
  /// Returns true if the function was changed.
  bool run();

private:
  /// Collect generic alloc_stack instructions that can be moved to the entry
  /// block.
  void collectHoistableInstructions();

  /// Move the hoistable alloc_stack instructions to the entry block.
  void hoist();
};
} // end anonymous namespace

/// Collect generic alloc_stack instructions in the current function can be
/// hoisted.
/// We can hoist generic alloc_stack instructions if they are not dependent on a
/// another instruction that we would have to hoist.
/// A generic alloc_stack could reference an opened archetype that was not
/// opened in the entry block.
void HoistAllocStack::collectHoistableInstructions() {
  for (auto &BB : *F) {
    for (auto &Inst : BB) {
      // Terminators that are function exits are our dealloc_stack
      // insertion points.
      if (auto *Term = dyn_cast<TermInst>(&Inst)) {
        if (Term->isFunctionExiting())
          FunctionExits.push_back(Term);
        continue;
      }
      // Don't perform alloc_stack hoisting in functions with availability.
      if (auto *Apply = dyn_cast<ApplyInst>(&Inst)) {
        if (Apply->hasSemantics("availability.osversion")) {
          AllocStackToHoist.clear();
          return;
        }
      }
      auto *ASI = dyn_cast<AllocStackInst>(&Inst);
      if (!ASI) {
        continue;
      }
      if (isHoistable(ASI, IRGenMod)) {
        LLVM_DEBUG(llvm::dbgs() << "Hoisting     " << Inst);
        AllocStackToHoist.push_back(ASI);
      } else {
        LLVM_DEBUG(llvm::dbgs() << "Not hoisting " << Inst);
      }
    }
  }
}

/// Hoist the alloc_stack instructions to the entry block and sink the
/// dealloc_stack instructions to the function exists.
void HoistAllocStack::hoist() {

  if (SILUseStackSlotMerging) {
    MergeStackSlots Merger(AllocStackToHoist, FunctionExits);
    Merger.mergeSlots();
  } else {
    // Hoist alloc_stacks to the entry block and delete dealloc_stacks.
    auto *EntryBB = F->getEntryBlock();
    for (auto *AllocStack : AllocStackToHoist) {
      // Insert at the beginning of the entry block.
      AllocStack->moveFront(EntryBB);
      // Delete dealloc_stacks.
      eraseDeallocStacks(AllocStack);
    }
    // Insert dealloc_stack in the exit blocks.
    for (auto *AllocStack : AllocStackToHoist) {
      insertDeallocStackAtEndOf(FunctionExits, AllocStack);
    }
  }
}

/// Try to hoist generic alloc_stack instructions to the entry block.
/// Returns true if the function was changed.
bool HoistAllocStack::run() {
  collectHoistableInstructions();

  // Nothing to hoist?
  if (AllocStackToHoist.empty())
    return false;

  hoist();
  return true;
}

namespace {
class AllocStackHoisting : public SILFunctionTransform {
  void run() override {
    auto *F = getFunction();
    auto *Mod = getIRGenModule();
    assert(Mod && "This pass must be run as part of an IRGen pipeline");
    bool Changed = HoistAllocStack(F, *Mod).run();
    if (Changed) {
      PM->invalidateAnalysis(F, SILAnalysis::InvalidationKind::Instructions);
    }
  }
};
} // end anonymous namespace

SILTransform *irgen::createAllocStackHoisting() {
  return new AllocStackHoisting();
}
