//===--- TempLValueOpt.cpp - Optimize temporary l-values ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This pass optimizes copies from a temporary (an "l-value") to a destination.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "cow-opts"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Optimizes copies from a temporary (an "l-value") to a destination.
///
/// \code
///   %temp = alloc_stack $Ty
///   instructions_which_store_to %temp
///   copy_addr [take] %temp to %destination
///   dealloc_stack %temp
/// \endcode
///
/// is optimized to
///
/// \code
///   destroy_addr %destination
///   instructions_which_store_to %destination
/// \endcode
///
/// The name TempLValueOpt refers to the TempRValueOpt pass, which performs
/// a related transformation, just with the temporary on the "right" side.
///
/// Note that TempLValueOpt is similar to CopyForwarding::backwardPropagateCopy.
/// It's more restricted (e.g. the copy-source must be an alloc_stack).
/// That enables other patterns to be optimized, which backwardPropagateCopy
/// cannot handle.
///
/// The pass also performs a peephole optimization on copy_addr - destroy
/// sequences.
/// It replaces
///
/// \code
///   copy_addr %source to %destination
///   destroy_addr %source
/// \endcode
///
/// with
///
/// \code
///   copy_addr [take] %source to %destination
/// \endcode
///
/// This peephole optimization is also done by the DestroyHoisting pass. But
/// DestroyHoisting currently only runs on OSSA.
/// TODO: when DestroyHoisting can run later in the pipeline, check if we still
///       need this peephole here.
class TempLValueOptPass : public SILFunctionTransform {
public:
  TempLValueOptPass() {}

  void run() override;

private:
  AliasAnalysis *AA = nullptr;
  
  bool tempLValueOpt(CopyAddrInst *copyInst);
  bool combineCopyAndDestroy(CopyAddrInst *copyInst);
};

void TempLValueOptPass::run() {
  SILFunction *F = getFunction();
  if (!F->shouldOptimize())
    return;

  LLVM_DEBUG(llvm::dbgs() << "*** TempLValueOptPass on function: "
                          << F->getName() << " ***\n");

  AA = PM->getAnalysis<AliasAnalysis>();

  bool changed = false;
  for (SILBasicBlock &block : *F) {
    // First collect all copy_addr instructions upfront to avoid iterator
    // invalidation problems (the optimizations might delete the copy_addr
    // itself or any following instruction).
    llvm::SmallVector<CopyAddrInst *, 32> copyInsts;
    for (SILInstruction &inst : block) {
      if (auto *copyInst = dyn_cast<CopyAddrInst>(&inst))
        copyInsts.push_back(copyInst);
    }
    // Do the optimizations.
    for (CopyAddrInst *copyInst : copyInsts) {
      changed |=combineCopyAndDestroy(copyInst);
      changed |=tempLValueOpt(copyInst);
    }
  }

  if (changed) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
}

static SingleValueInstruction *isMovableProjection(SILValue addr) {
  if (auto *enumData = dyn_cast<InitEnumDataAddrInst>(addr))
    return enumData;
  if (auto *existentialAddr = dyn_cast<InitExistentialAddrInst>(addr))
    return existentialAddr;
    
  if (SingleValueInstruction *svi = Projection::isAddressProjection(addr)) {
    if (svi->getNumOperands() == 1)
      return svi;
  }
  return nullptr;
}

bool TempLValueOptPass::tempLValueOpt(CopyAddrInst *copyInst) {

  // An overview of the algorithm:
  //
  //   alloc_stack %temp
  //   ...
  //   first_use_of %temp // beginOfLiferange
  //   ... // no reads or writes from/to %destination
  //   copy_addr [take] %temp to %destination // copyInst
  //   ... // no further uses of %temp (copyInst is the end of %temp liferange)
  //   dealloc_stack %temp
  //
  // All projections to %destination are hoisted above the first use of %temp.
  // Then all uses of %temp are replaced by %destination.
  // In case the copyInst is not initializing %destination, a
  // 'destroy_addr %destination' is inserted before the first use of %temp.

  if (!copyInst->isTakeOfSrc())
    return false;

  auto *temporary = dyn_cast<AllocStackInst>(copyInst->getSrc());
  if (!temporary)
    return false;

  // Collect all users of the temporary into a set. Also, for simplicity,
  // require that all users are within a single basic block.
  SmallPtrSet<SILInstruction *, 8> users;
  SILBasicBlock *block = temporary->getParent();
  for (Operand *use : temporary->getUses()) {
    SILInstruction *user = use->getUser();
    if (user->getParent() != block)
      return false;
    users.insert(user);
  }

  // Collect all address projections of the destination - in case we need to
  // hoist them.
  SmallPtrSet<SILInstruction *, 4> projections;
  SILValue destination = copyInst->getDest();
  SILValue destRootAddr = destination;
  while (SingleValueInstruction *projInst = isMovableProjection(destRootAddr)) {
    // In theory, users of the temporary and address projections of the
    // destination should be completely distinct. Otherwise the copyInst would
    // be an identity copy (source == destination).
    // Just to be on the safe side, bail if it's not the case (instead of an
    // assert).
    if (users.count(projInst))
      return false;
    projections.insert(projInst);
    destRootAddr = projInst->getOperand(0);
  }
  // The root address of the destination. It's null if it's not an instruction,
  // but a block argument.
  SILInstruction *destRootInst = destRootAddr->getDefiningInstruction();

  // Iterate over the liferange of the temporary and make some validity checks.
  SILInstruction *beginOfLiferange = nullptr;
  bool endOfLiferangeReached = false;
  for (auto iter = temporary->getIterator(); iter != block->end(); ++iter) {
    SILInstruction *inst = &*iter;
    // The dealloc_stack is the last user of the temporary.
    if (isa<DeallocStackInst>(inst) && inst->getOperand(0) == temporary)
      break;

    if (users.count(inst) != 0) {
      // Check if the copyInst is the last user of the temporary (beside the
      // dealloc_stack).
      if (endOfLiferangeReached)
        return false;

      // Find the first user of the temporary to get a more precise liferange.
      // It would be too conservative to treat the alloc_stack itself as the
      // begin of the liferange.
      if (!beginOfLiferange)
        beginOfLiferange = inst;

      if (inst == copyInst)
        endOfLiferangeReached = true;
    }
    if (beginOfLiferange && !endOfLiferangeReached) {
      // If the root address of the destination is within the liferange of the
      // temporary, we cannot replace all uses of the temporary with the
      // destination (it would break the def-use dominance rule).
      if (inst == destRootInst)
        return false;
        
      // Check if the destination is not accessed within the liferange of
      // the temporary.
      // This is unlikely, because the destination is initialized at the
      // copyInst. But still, the destination could contain an initialized value
      // which is destroyed before the copyInst.
      if (AA->mayReadOrWriteMemory(inst, destination) &&
          // Needed to treat init_existential_addr as not-writing projection.
          projections.count(inst) == 0)
        return false;
    }
  }
  assert(endOfLiferangeReached);

  // Move all projections of the destination address before the liferange of
  // the temporary.
  for (auto iter = beginOfLiferange->getIterator();
       iter != copyInst->getIterator();) {
    SILInstruction *inst = &*iter++;
    if (projections.count(inst))
      inst->moveBefore(beginOfLiferange);
  }

  if (!copyInst->isInitializationOfDest()) {
    // Make sure the the destination is uninitialized before the liferange of
    // the temporary.
    SILBuilderWithScope builder(beginOfLiferange);
    builder.createDestroyAddr(copyInst->getLoc(), destination);
  }

  // Replace all uses of the temporary with the destination address.
  while (!temporary->use_empty()) {
    Operand *use = *temporary->use_begin();
    SILInstruction *user = use->getUser();
    switch (user->getKind()) {
      case SILInstructionKind::DeallocStackInst:
        user->eraseFromParent();
        break;
      default:
        use->set(destination);
    }
  }
  temporary->eraseFromParent();
  copyInst->eraseFromParent();
  return true;
}

bool TempLValueOptPass::combineCopyAndDestroy(CopyAddrInst *copyInst) {
  if (copyInst->isTakeOfSrc())
    return false;

  // Find a destroy_addr of the copy's source address.
  DestroyAddrInst *destroy = nullptr;
  for (Operand *srcUse : copyInst->getSrc()->getUses()) {
    if ((destroy = dyn_cast<DestroyAddrInst>(srcUse->getUser())))
      break;
  }
  SILBasicBlock *block = copyInst->getParent();
  if (!destroy || destroy->getParent() != block)
    return false;
  assert(destroy->getOperand() == copyInst->getSrc());

  // Check if the destroy_addr is after the copy_addr and if there are no
  // memory accesses between them.
  for (auto iter = std::next(copyInst->getIterator());
       iter != block->end(); ++iter) {
    SILInstruction *inst = &*iter;
    if (inst == destroy) {
      copyInst->setIsTakeOfSrc(IsTake);
      destroy->eraseFromParent();
      return true;
    }
    if (inst->mayReadOrWriteMemory())
      return false;
  }
  return false;
}

} // end anonymous namespace

SILTransform *swift::createTempLValueOpt() {
  return new TempLValueOptPass();
}

