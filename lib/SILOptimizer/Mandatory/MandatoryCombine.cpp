//===------- MandatoryCombiner.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
///  \file
///
///  Defines the MandatoryCombiner function transform.  The pass contains basic
///  instruction combines to be performed at the begining of both the Onone and
///  also the performance pass pipelines, after the diagnostics passes have been
///  run.  It is intended to be run before and to be independent of other
///  transforms.
///
///  The intention of this pass is to be a place for mandatory peepholes that
///  are not needed for diagnostics. Please put any such peepholes here instead
///  of in the diagnostic passes.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-mandatory-combiner"

#include "swift/Basic/LLVM.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILInstructionWorklist.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CanonicalizeInstruction.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/StackNesting.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

/// \returns whether all the values are of trivial type in the provided
///          function.
template <typename Values>
static bool areAllValuesTrivial(Values values, SILFunction &function) {
  return llvm::all_of(values, [&](SILValue value) -> bool {
    return value->getType().isTrivial(function);
  });
}

//===----------------------------------------------------------------------===//
//      CanonicalizeInstruction subclass for use in Mandatory Combiner.
//===----------------------------------------------------------------------===//

namespace {

class MandatoryCombineCanonicalize final : CanonicalizeInstruction {
public:
  using Worklist = SmallSILInstructionWorklist<256>;

private:
  Worklist &worklist;
  bool changed = false;

public:
  MandatoryCombineCanonicalize(Worklist &worklist, DeadEndBlocks &deadEndBlocks)
      : CanonicalizeInstruction(DEBUG_TYPE, deadEndBlocks), worklist(worklist) {
  }

  void notifyNewInstruction(SILInstruction *inst) override {
    worklist.add(inst);
    worklist.addUsersOfAllResultsToWorklist(inst);
    changed = true;
  }

  // Just delete the given 'inst' and record its operands. The callback isn't
  // allowed to mutate any other instructions.
  void killInstruction(SILInstruction *inst) override {
    worklist.eraseSingleInstFromFunction(*inst,
                                         /*AddOperandsToWorklist*/ true);
    changed = true;
  }

  void notifyHasNewUsers(SILValue value) override {
    if (worklist.size() < 10000) {
      worklist.addUsersToWorklist(value);
    }
    changed = true;
  }

  bool tryCanonicalize(SILInstruction *inst) {
    changed = false;
    canonicalize(inst);
    return changed;
  }
};

} // anonymous namespace

//===----------------------------------------------------------------------===//
//                        MandatoryCombiner Interface
//===----------------------------------------------------------------------===//

namespace {

class MandatoryCombiner final
    : public SILInstructionVisitor<MandatoryCombiner, SILInstruction *> {

  bool compilingWithOptimization;

  using Worklist = SmallSILInstructionWorklist<256>;

  /// The list of instructions remaining to visit, perhaps to combine.
  Worklist worklist;

  /// Whether any changes have been made.
  bool madeChange;

  /// Set to true if some alloc/dealloc_stack instruction are inserted and at
  /// the end of the run stack nesting needs to be corrected.
  bool invalidatedStackNesting = false;

  /// The number of times that the worklist has been processed.
  unsigned iteration;

  InstModCallbacks instModCallbacks;
  SmallVectorImpl<SILInstruction *> &createdInstructions;
  SmallVector<SILInstruction *, 16> instructionsPendingDeletion;
  DeadEndBlocks &deadEndBlocks;

public:
  MandatoryCombiner(bool optimized,
                    SmallVectorImpl<SILInstruction *> &createdInstructions,
                    DeadEndBlocks &deadEndBlocks)
      : compilingWithOptimization(optimized), worklist("MC"), madeChange(false),
        iteration(0),
        instModCallbacks(),
        createdInstructions(createdInstructions),
        deadEndBlocks(deadEndBlocks) {
    instModCallbacks = InstModCallbacks()
      .onDelete([&](SILInstruction *instruction) {
        worklist.erase(instruction);
        instructionsPendingDeletion.push_back(instruction);
      })
      .onCreateNewInst([&](SILInstruction *instruction) {
        worklist.add(instruction);
      })
      .onSetUseValue([this](Operand *use, SILValue newValue) {
        use->set(newValue);
        worklist.add(use->getUser());
      });
  };

  void addReachableCodeToWorklist(SILFunction &function);

  /// \return whether a change was made.
  bool doOneIteration(SILFunction &function, unsigned iteration);

  void clear() {
    iteration = 0;
    worklist.resetChecked();
    madeChange = false;
  }

  /// Applies the MandatoryCombiner to the provided function.
  ///
  /// \param function the function to which to apply the MandatoryCombiner.
  ///
  /// \return whether a change was made.
  bool runOnFunction(SILFunction &function) {
    bool changed = false;

    while (doOneIteration(function, iteration)) {
      changed = true;
      ++iteration;
    }

    if (invalidatedStackNesting) {
      StackNesting::fixNesting(&function);
    }

    return changed;
  }
      
  StoreInst *tryPromoteLoadsOf(StoreInst *store);

  /// Base visitor that does not do anything.
  SILInstruction *visitSILInstruction(SILInstruction *) { return nullptr; }
  SILInstruction *visitApplyInst(ApplyInst *instruction);
  SILInstruction *visitStoreInst(StoreInst *instruction);
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//               MandatoryCombiner Non-Visitor Utility Methods
//===----------------------------------------------------------------------===//

static llvm::cl::opt<bool> EnableCanonicalizationAndTrivialDCE(
    "sil-mandatory-combine-enable-canon-and-simple-dce", llvm::cl::Hidden,
    llvm::cl::init(false),
    llvm::cl::desc("An option for compiler developers that cause the Mandatory "
                   "Combiner to be more aggressive at eliminating trivially "
                   "dead code and canonicalizing SIL"));

void MandatoryCombiner::addReachableCodeToWorklist(SILFunction &function) {
  BasicBlockWorklist blockWorklist(function.getEntryBlock());
  SmallVector<SILInstruction *, 128> initialInstructionWorklist;

  while (SILBasicBlock *block = blockWorklist.pop()) {
    for (auto iterator = block->begin(), end = block->end(); iterator != end;) {
      auto *instruction = &*iterator;
      ++iterator;

      if (isInstructionTriviallyDead(instruction)) {
        if (EnableCanonicalizationAndTrivialDCE) {
          if (compilingWithOptimization) {
            instruction->replaceAllUsesOfAllResultsWithUndef();
            instruction->eraseFromParent();
          }
        }
        continue;
      }

      initialInstructionWorklist.push_back(instruction);
    }

    for (SILBasicBlock *succ : block->getSuccessors()) {
      blockWorklist.pushIfNotVisited(succ);
    }
  }

  worklist.addInitialGroup(initialInstructionWorklist);
}

bool MandatoryCombiner::doOneIteration(SILFunction &function,
                                       unsigned iteration) {
  madeChange = false;

  addReachableCodeToWorklist(function);
  MandatoryCombineCanonicalize mcCanonicialize(worklist, deadEndBlocks);

  while (!worklist.isEmpty()) {
    auto *instruction = worklist.pop_back_val();
    if (instruction == nullptr) {
      continue;
    }

    if (EnableCanonicalizationAndTrivialDCE) {
      if (compilingWithOptimization) {
        if (isInstructionTriviallyDead(instruction)) {
          worklist.eraseInstFromFunction(*instruction);
          madeChange = true;
          continue;
        }
      }

      if (mcCanonicialize.tryCanonicalize(instruction)) {
        madeChange = true;
        continue;
      }
    }

#ifndef NDEBUG
    std::string instructionDescription;
#endif
    LLVM_DEBUG(llvm::raw_string_ostream SS(instructionDescription);
               instruction->print(SS); instructionDescription = SS.str(););
    LLVM_DEBUG(llvm::dbgs()
               << "MC: Visiting: " << instructionDescription << '\n');

    if (auto replacement = visit(instruction)) {
      worklist.replaceInstructionWithInstruction(instruction, replacement
#ifndef NDEBUG
                                                 ,
                                                 instructionDescription
#endif
      );
      madeChange = true;
    }

    for (SILInstruction *instruction : instructionsPendingDeletion) {
      worklist.eraseInstFromFunction(*instruction);
      madeChange = true;
    }
    instructionsPendingDeletion.clear();

    // Our tracking list has been accumulating instructions created by the
    // SILBuilder during this iteration. Go through the tracking list and add
    // its contents to the worklist and then clear said list in preparation
    // for the next iteration.
    for (SILInstruction *instruction : createdInstructions) {
      if (instruction->isDeleted())
        continue;

      LLVM_DEBUG(llvm::dbgs() << "MC: add " << *instruction
                              << " from tracking list to worklist\n");
      worklist.add(instruction);
      madeChange = true;
    }
    createdInstructions.clear();
  }

  worklist.resetChecked();
  return madeChange;
}

//===----------------------------------------------------------------------===//
//                     MandatoryCombiner Visitor Methods
//===----------------------------------------------------------------------===//

SILInstruction *MandatoryCombiner::visitApplyInst(ApplyInst *instruction) {

  // Apply this pass only to partial applies all of whose arguments are
  // trivial.
  auto calledValue = instruction->getCallee();
  if (calledValue == nullptr) {
    return nullptr;
  }
  auto fullApplyCallee = calledValue->getDefiningInstruction();
  if (fullApplyCallee == nullptr) {
    return nullptr;
  }
  auto partialApply = dyn_cast<PartialApplyInst>(fullApplyCallee);
  if (partialApply == nullptr) {
    return nullptr;
  }
  auto *function = partialApply->getCalleeFunction();
  if (function == nullptr) {
    return nullptr;
  }
  ApplySite fullApplySite(instruction);
  auto fullApplyArguments = fullApplySite.getArguments();
  if (!areAllValuesTrivial(fullApplyArguments, *function)) {
    return nullptr;
  }
  auto partialApplyArguments = ApplySite(partialApply).getArguments();
  if (!areAllValuesTrivial(partialApplyArguments, *function)) {
    return nullptr;
  }

  auto callee = partialApply->getCallee();

  ApplySite partialApplySite(partialApply);

  SmallVector<SILValue, 8> argsVec;
  llvm::copy(fullApplyArguments, std::back_inserter(argsVec));
  llvm::copy(partialApplyArguments, std::back_inserter(argsVec));

  SILBuilderWithScope builder(instruction, &createdInstructions);
  ApplyInst *replacement = builder.createApply(
      /*Loc=*/instruction->getDebugLocation().getLocation(), /*Fn=*/callee,
      /*Subs=*/partialApply->getSubstitutionMap(),
      /*Args*/ argsVec,
      /*isNonThrowing=*/instruction->getApplyOptions(),
      /*SpecializationInfo=*/partialApply->getSpecializationInfo());

  worklist.replaceInstructionWithInstruction(instruction, replacement
#ifndef NDEBUG
                                             ,
                                             /*instructionDescription=*/""
#endif
  );
  if (tryDeleteDeadClosure(partialApply, instModCallbacks)) {
    invalidatedStackNesting = true;
  }
  return nullptr;
}

template<class Fn>
static bool tryRemoveDeadStore(StoreInst *store, Fn deleter) {
  DominanceInfo dominanceInfo(store->getFunction());
  // Keep track of weather we've skipped any users of store->getDest(). If we
  // have, we can't remove the dest and it's users.
  bool skippedAnyDestUsers = false;
  SmallVector<SILInstruction*, 4> instsToDelete;
  for (auto *use : store->getDest()->getUses()) {
    auto user = use->getUser();
    if (user == store)
      continue;

    // Destroys, deallocs, etc. are fine.
    if (isa<DestroyAddrInst>(user) || isa<DeallocStackInst>(user) ||
        isa<EndAccessInst>(user)) {
      instsToDelete.push_back(user);
    } else {
      // Other uses are only OK if they come before the store. But, if we find
      // any it means we can't remove the dest and its users.
      if (dominanceInfo.dominates(user, store))
        skippedAnyDestUsers = true;
      // If it comes after the store, bail.
      else
        return false;
    }
  }

  // We could remove this but it would mean tracking down and removing the
  // destroy addr which is expensive. Also, the pattern is rare:
  //   store x to [init] addr
  //   destroy_addr addr
  //   store y to [init] addr
  // So, just bail.
  if (store->getOwnershipQualifier() == StoreOwnershipQualifier::Init &&
      skippedAnyDestUsers)
    return false;

  // Make sure we consume the value if needed.
  if (store->getOwnershipQualifier() == StoreOwnershipQualifier::Assign ||
      store->getOwnershipQualifier() == StoreOwnershipQualifier::Init)
    SILBuilderWithScope(store)
      .emitDestroyValueAndFold(store->getLoc(), store->getSrc());

  // Either way, remove the store.
  deleter(store);
  // If there are any uses we skipped, don't remove the dest.
  if (!skippedAnyDestUsers) {
    llvm::for_each(instsToDelete, deleter);
    deleter(store->getDest().getDefiningInstruction());
  }
  return true;
}

StoreInst *MandatoryCombiner::tryPromoteLoadsOf(StoreInst *store) {
  // Avoid destinations such as global_addrs.
  if (store->getOwnershipQualifier() != StoreOwnershipQualifier::Trivial &&
      store->getOwnershipQualifier() != StoreOwnershipQualifier::Unqualified &&
      !isa<AllocStackInst>(store->getDest()) &&
      !isa<PartialApplyInst>(store->getDest()))
    return nullptr;

  // Try to promote store src to loads.
  DominanceInfo dominanceInfo(store->getFunction());
  LoadInst *load = nullptr;
  for (auto *use : getNonDebugUses(store->getDest())) {
    auto user = use->getUser();
    if (user == store)
      continue;

    if (isa<DeallocStackInst>(user) || isa<DestroyAddrInst>(user))
      continue;

    if (auto loadUse = dyn_cast<LoadInst>(user)) {
      // If the load comes before the store, we don't want update it but, it
      // also can't effect our optimization (unlike two loads after the store
      // could).
      if (dominanceInfo.dominates(loadUse, store))
        continue;
      if (load) {
        load = nullptr;
        break;
      }
      load = loadUse;
    } else {
      // Otherwise, bail.
      load = nullptr;
      break;
    }
  }

  // If we can promote the load, do so.
  if (load &&
      // Make sure we haven't already tried to delete this instruction.
      std::find(instructionsPendingDeletion.begin(),
                instructionsPendingDeletion.end(),
                load) == instructionsPendingDeletion.end()) {
    // Check that all uses of the load are dominated by the store src.
    // Otherwise, we can't optimize this load.
    bool loadDominatesAllUses;
    if (auto srcInst = store->getSrc().getDefiningInstruction()) {
      loadDominatesAllUses = dominatesAllUses(srcInst, load);
    } else {
      // Otherwise it's an argument so, check all uses are in blocks that
      // dominate that argument.
      loadDominatesAllUses = std::all_of(
          load->use_begin(), load->use_end(),
          [&store, &dominanceInfo](Operand *use) {
            return dominanceInfo.dominates(store->getSrc()->getParentBlock(),
                                           use->getUser()->getParentBlock());
          });
    }

    if (!loadDominatesAllUses)
      return nullptr;

    if (load->getOwnershipQualifier() == LoadOwnershipQualifier::Copy) {
      // load [copy] is just adding a copy.
      auto copy = SILBuilderWithScope(load).createCopyValue(load->getLoc(),
                                                            store->getSrc());
      load->replaceAllUsesWith(copy);
    } else if (load->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
      // load [take] is both a copy and a destroy. It's important that the copy
      // comes before the destroy even though they are unrelated because the
      // destroy_addr will be used below to figure out where we need to insert a
      // destroy_value.
      SILBuilderWithScope loadBuilder(load);
      auto copy = loadBuilder.createCopyValue(load->getLoc(), store->getSrc());
      loadBuilder.createDestroyAddr(load->getLoc(), load->getOperand());
      load->replaceAllUsesWith(copy);
    } else
      load->replaceAllUsesWith(store->getSrc());
    instModCallbacks.deleteInst(load);

    // Finally, for copy/takes we need to make sure the store doesn't consume
    // the new value that we've just used. We can't move the copy above the
    // store because the two instructions may not be in the same block so, we
    // copy the value, replace the store, and destroy the value.
    //
    // To destroy the value we look for the destroy_addrs for the destination of
    // the store and insert a destroy value next to those. Above we ensure that
    // the only uses of the dest are either replaced or destroy_addrs so, we can
    // safely add a destroy_value everywhere we see a destroy_addr.
    if (load->getOwnershipQualifier() == LoadOwnershipQualifier::Take ||
        load->getOwnershipQualifier() == LoadOwnershipQualifier::Copy) {
      for (auto *use : store->getDest()->getUses()) {
        if (isa<DestroyAddrInst>(use->getUser()))
          SILBuilderWithScope(use->getUser())
              .createDestroyValue(use->getUser()->getLoc(), store->getSrc());
      }

      SILBuilderWithScope storeBuilder(store);
      auto copy =
          storeBuilder.createCopyValue(store->getLoc(), store->getSrc());
      return storeBuilder.createStore(store->getLoc(), copy, store->getDest(),
                                      store->getOwnershipQualifier());
    }
  }
  
  return nullptr;
}

SILInstruction *MandatoryCombiner::visitStoreInst(StoreInst *store) {
  if (auto newStore = tryPromoteLoadsOf(store))
    return newStore;
  
  if (tryRemoveDeadStore(store, instModCallbacks.deleteInst))
    return nullptr;
  
  return nullptr;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class MandatoryCombine final : public SILFunctionTransform {
  bool optimized;
  SmallVector<SILInstruction *, 64> createdInstructions;

public:
  MandatoryCombine(bool optimized) : optimized(optimized) {}

  void run() override {
    auto *function = getFunction();

    // If this function is an external declaration, bail. We only want to visit
    // functions with bodies.
    if (function->isExternalDeclaration()) {
      return;
    }

    DeadEndBlocks deadEndBlocks(function);
    MandatoryCombiner combiner(optimized, createdInstructions, deadEndBlocks);
    bool madeChange = combiner.runOnFunction(*function);

    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createMandatoryCombine() {
  return new MandatoryCombine(/*optimized*/ false);
}

SILTransform *swift::createOptimizedMandatoryCombine() {
  return new MandatoryCombine(/*optimized*/ true);
}
