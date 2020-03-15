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
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILInstructionWorklist.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
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
//                        MandatoryCombiner Interface
//===----------------------------------------------------------------------===//

namespace {

class MandatoryCombiner final
    : public SILInstructionVisitor<MandatoryCombiner, SILInstruction *> {

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

public:
  MandatoryCombiner(SmallVectorImpl<SILInstruction *> &createdInstructions)
      : worklist("MC"), madeChange(false), iteration(0),
        instModCallbacks(
            [&](SILInstruction *instruction) {
              worklist.erase(instruction);
              instructionsPendingDeletion.push_back(instruction);
            },
            [&](SILInstruction *instruction) { worklist.add(instruction); },
            [this](SILValue oldValue, SILValue newValue) {
              worklist.replaceValueUsesWith(oldValue, newValue);
            }),
        createdInstructions(createdInstructions){};

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
      StackNesting().correctStackNesting(&function);
    }

    return changed;
  }

  /// Base visitor that does not do anything.
  SILInstruction *visitSILInstruction(SILInstruction *) { return nullptr; }
  SILInstruction *visitApplyInst(ApplyInst *instruction);
  SILInstruction *visitPartialApplyInst(PartialApplyInst *i);
  SILInstruction *visitLoadInst(LoadInst *i);
  SILInstruction *visitThinToThickFunctionInst(ThinToThickFunctionInst *i);
  SILInstruction *visitStoreInst(StoreInst *i);
  SILInstruction *visitConvertFunctionInst(ConvertFunctionInst *i);
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//               MandatoryCombiner Non-Visitor Utility Methods
//===----------------------------------------------------------------------===//

void MandatoryCombiner::addReachableCodeToWorklist(SILFunction &function) {
  SmallVector<SILBasicBlock *, 32> blockWorklist;
  SmallPtrSet<SILBasicBlock *, 32> blockAlreadyAddedToWorklist;
  SmallVector<SILInstruction *, 128> initialInstructionWorklist;

  {
    auto *firstBlock = &*function.begin();
    blockWorklist.push_back(firstBlock);
    blockAlreadyAddedToWorklist.insert(firstBlock);
  }

  while (!blockWorklist.empty()) {
    auto *block = blockWorklist.pop_back_val();

    for (auto iterator = block->begin(), end = block->end(); iterator != end;) {
      auto *instruction = &*iterator;
      ++iterator;

      if (isInstructionTriviallyDead(instruction)) {
        instModCallbacks.deleteInst(instruction);
        continue;
      }

      initialInstructionWorklist.push_back(instruction);
    }

    llvm::copy_if(block->getSuccessorBlocks(),
                  std::back_inserter(blockWorklist),
                  [&](SILBasicBlock *block) -> bool {
                    return blockAlreadyAddedToWorklist.insert(block).second;
                  });
  }

  worklist.addInitialGroup(initialInstructionWorklist);
}

bool MandatoryCombiner::doOneIteration(SILFunction &function,
                                       unsigned iteration) {
  madeChange = false;

  addReachableCodeToWorklist(function);

  while (!worklist.isEmpty()) {
    auto *instruction = worklist.pop_back_val();
    if (instruction == nullptr) {
      continue;
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
      LLVM_DEBUG(llvm::dbgs() << "MC: add " << *instruction
                              << " from tracking list to worklist\n");
      worklist.add(instruction);
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
      /*isNonThrowing=*/instruction->isNonThrowing(),
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

template <class InstT> static FunctionRefInst *getRemovableRef(InstT *i) {
  // If the only use of the function_ref is us, then remove it.
  auto funcRef = dyn_cast<FunctionRefInst>(i->getCallee());
  if (funcRef &&
      (funcRef->use_empty() ||
       (funcRef->getSingleUse() && funcRef->getSingleUse()->getUser() == i))) {
    return funcRef;
  }
  return nullptr;
}

SILInstruction *MandatoryCombiner::visitStoreInst(StoreInst *store) {
  // First optimization: try to promote store src to loads.
  LoadInst *load = nullptr;
  for (auto *use : getNonDebugUses(store->getDest())) {
    if (use->getUser() == store)
      continue;

    if (auto loadUse = dyn_cast<LoadInst>(use->getUser())) {
      if (load) {
        load = nullptr;
        break;
      }
      load = loadUse;
    }
  }

  // If we can promote the load, do so.
  if (load && std::find(instructionsPendingDeletion.begin(),
                        instructionsPendingDeletion.end(), load) == instructionsPendingDeletion.end()) {
    if (load->getOwnershipQualifier() == LoadOwnershipQualifier::Copy) {
      auto copy = SILBuilderWithScope(load)
        .createCopyValue(load->getLoc(), store->getSrc());
      load->replaceAllUsesWith(copy);
    } else if (load->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
      SILBuilderWithScope(load)
        .createDestroyAddr(load->getLoc(), load->getOperand());
      load->replaceAllUsesWith(store->getSrc());
    } else
        load->replaceAllUsesWith(store->getSrc());
    instModCallbacks.deleteInst(load);
  }

  // Now try to promote alloc_box/project_box.
  auto pbi = dyn_cast<ProjectBoxInst>(store->getDest());
  if (!pbi)
    return nullptr;
  
  for (auto *use : getNonDebugUses(pbi)) {
    if (use->getUser() != store)
      return nullptr;
  }

  auto abi = dyn_cast<AllocBoxInst>(pbi->getOperand());
  if (!abi)
    return nullptr;

  SmallVector<SILInstruction*, 2> instToDestroy;
  for (auto *use : abi->getUses()) {
    auto user = use->getUser();

    if (user == pbi)
      continue;

    if (!isa<DestroyValueInst>(user) && !isa<StrongReleaseInst>(user))
      return nullptr;

    instToDestroy.push_back(user);
  }
  
  // We know all we're replacing is destroy instructions so if it's trivial just
  // remove the uses.
  if (store->getOwnershipQualifier() == StoreOwnershipQualifier::Trivial) {
    // We need to collect store and pbi first.
    instModCallbacks.deleteInst(store);
    instModCallbacks.deleteInst(pbi);
    for (auto *use : abi->getUses()) {
      auto user = use->getUser();
      if (user == store || user == pbi)
        continue;
      instModCallbacks.deleteInst(user);
    }
  } else {
    abi->replaceAllUsesWith(store->getSrc());
    instModCallbacks.deleteInst(store);
    instModCallbacks.deleteInst(pbi);
  }

  instModCallbacks.deleteInst(abi);
  return nullptr;
}

SILInstruction *MandatoryCombiner::visitLoadInst(LoadInst *i) {
  // Remove trivially dead loads. We can remove loads where the only use is
  // destroy_values.
  SmallVector<SILInstruction*, 2> destroysToDestroy;
  for (auto *use : i->getUses()) {
    if (!isa<DestroyValueInst>(use->getUser()))
      return nullptr;
    destroysToDestroy.push_back(use->getUser());
  }
  std::for_each(destroysToDestroy.begin(), destroysToDestroy.end(),
                instModCallbacks.deleteInst);
  instModCallbacks.deleteInst(i);
  return nullptr;
}

SILInstruction *MandatoryCombiner::visitConvertFunctionInst(ConvertFunctionInst *convFunc) {
  if (isInstructionTriviallyDead(convFunc)) {
    // We'll let dead code elimination do the rest
    instModCallbacks.deleteInst(convFunc);
  }

  return nullptr;
}

/// Try to remove partial applies that are no longer used
SILInstruction *MandatoryCombiner::visitPartialApplyInst(PartialApplyInst *i) {
  auto *ref = getRemovableRef(i);
  if (tryDeleteDeadClosure(i, instModCallbacks, /*needKeepArgsAlive=*/false)) {
    if (ref) {
      instModCallbacks.deleteInst(ref);
    }
  }

  return nullptr;
}

/// Try to remove thing to thick instructions that are no longer used
SILInstruction *
MandatoryCombiner::visitThinToThickFunctionInst(ThinToThickFunctionInst *i) {
  auto *ref = getRemovableRef(i);
  if (tryDeleteDeadClosure(i, instModCallbacks, /*needKeepArgsAlive=*/false)) {
    if (ref) {
      instModCallbacks.deleteInst(ref);
    }
  }

  return nullptr;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class MandatoryCombine final : public SILFunctionTransform {

  SmallVector<SILInstruction *, 64> createdInstructions;

  void run() override {
    auto *function = getFunction();

    // If this function is an external declaration, bail. We only want to visit
    // functions with bodies.
    if (function->isExternalDeclaration()) {
      return;
    }

    MandatoryCombiner combiner(createdInstructions);
    bool madeChange = combiner.runOnFunction(*function);

    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }

  void handleDeleteNotification(SILNode *node) override {
    // Remove instructions that were both created and deleted from the list of
    // created instructions which will eventually be added to the worklist.

    auto *instruction = dyn_cast<SILInstruction>(node);
    if (instruction == nullptr) {
      return;
    }

    // Linear searching the tracking list doesn't hurt because usually it only
    // contains a few elements.
    auto iterator = find(createdInstructions, instruction);
    if (createdInstructions.end() != iterator) {
      createdInstructions.erase(iterator);
    }
  }

  bool needsNotifications() override { return true; }
};

} // end anonymous namespace

SILTransform *swift::createMandatoryCombine() { return new MandatoryCombine(); }
