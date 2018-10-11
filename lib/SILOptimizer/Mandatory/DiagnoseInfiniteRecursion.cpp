//==-- DiagnoseInfiniteRecursion.cpp - Find infinitely-recursive applies --==//
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
// This file implements a diagnostic pass that detects deleterious forms of
// recursive functions.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "infinite-recursion"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Debug.h"

using namespace swift;

template<typename...T, typename...U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc,
                         diag, std::forward<U>(args)...);
}

static bool hasRecursiveCallInPath(SILBasicBlock &Block,
                                   SILFunction *Target,
                                   ModuleDecl *TargetModule) {
  // Process all instructions in the block to find applies that reference
  // the parent function.  Also looks through vtables for statically
  // dispatched (witness) methods.
  for (auto &I : Block) {
    auto *AI = dyn_cast<ApplyInst>(&I);
    if (AI && AI->getCalleeFunction() && AI->getCalleeFunction() == Target)
      return true;

    if (FullApplySite FAI = FullApplySite::isa(&I)) {
      // Don't touch dynamic dispatch.
      if (isa<ObjCMethodInst>(FAI.getCallee()))
        continue;

      auto &M = FAI.getModule();
      if (auto *CMI = dyn_cast<ClassMethodInst>(FAI.getCallee())) {
        auto ClassType = CMI->getOperand()->getType();

        // FIXME: If we're not inside the module context of the method,
        // we may have to deserialize vtables.  If the serialized tables
        // are damaged, the pass will crash.
        //
        // Though, this has the added bonus of not looking into vtables
        // outside the current module.  Because we're not doing IPA, let
        // alone cross-module IPA, this is all well and good.
        auto *BGC = ClassType.getNominalOrBoundGenericNominal();
        if (BGC && BGC->getModuleContext() != TargetModule) {
          continue;
        }

        auto *F = getTargetClassMethod(M, ClassType, CMI);
        if (F == Target)
          return true;

        continue;
      }

      if (auto *WMI = dyn_cast<WitnessMethodInst>(FAI.getCallee())) {
        SILFunction *F;
        SILWitnessTable *WT;

        std::tie(F, WT) = M.lookUpFunctionInWitnessTable(
            WMI->getConformance(), WMI->getMember());
        if (F == Target)
          return true;

        continue;
      }
    }
  }
  return false;
}

/// Returns true if the block has a call to a function marked with
/// @_semantics("programtermination_point").
static bool isKnownProgramTerminationPoint(const SILBasicBlock *bb) {
  // Skip checking anything if this block doesn't end in a program terminator.
  if (!bb->getTerminator()->isProgramTerminating())
    return false;

  // Check each instruction for a call to something that's a known
  // programtermination_point
  for (auto it = bb->rbegin(); it != bb->rend(); ++it) {
    auto applySite = FullApplySite::isa(const_cast<SILInstruction *>(&*it));
    if (!applySite) continue;
    if (applySite.isCalleeKnownProgramTerminationPoint())
      return true;
  }
  return false;
}

/// Perform a DFS through the target function to find any paths to an exit node
/// that do not call into the target.
static bool hasInfinitelyRecursiveApply(SILFunction *targetFn) {
  SmallPtrSet<SILBasicBlock *, 32> visited = { targetFn->getEntryBlock() };
  SmallVector<SILBasicBlock *, 32> workList = { targetFn->getEntryBlock() };

  // Keep track of if we've found any recursive blocks at all.
  // We return true if we found any recursion and did not find any
  // non-recursive, function-exiting blocks.
  bool foundRecursion = false;
  auto *targetModule = targetFn->getModule().getSwiftModule();

  while (!workList.empty()) {
    SILBasicBlock *curBlock = workList.pop_back_val();

    // Before checking for infinite recursion, see if we're calling something
    // that's @_semantics("programtermination_point"). We explicitly don't
    // want this call to disqualify the warning for infinite recursion,
    // because they're reserved for exceptional circumstances.
    if (isKnownProgramTerminationPoint(curBlock))
      continue;

    // We're looking for functions that are recursive on _all_ paths. If this
    // block is recursive, mark that we found recursion and check the next
    // block in the work list.
    if (hasRecursiveCallInPath(*curBlock, targetFn, targetModule)) {
      foundRecursion = true;
      continue;
    }

    // If this block doesn't have a recursive call, and it exits the function,
    // then we know the function is not infinitely recursive.
    auto term = curBlock->getTerminator();
    if (term->isFunctionExiting() || term->isProgramTerminating())
      return false;

    // Otherwise, push the successors onto the stack if we haven't visited them.
    for (auto *succ : curBlock->getSuccessorBlocks()) {
      if (visited.insert(succ).second)
        workList.push_back(succ);
    }
  }
  return foundRecursion;
}

namespace {
  class DiagnoseInfiniteRecursion : public SILFunctionTransform {
  public:
    DiagnoseInfiniteRecursion() {}

  private:
    void run() override {
      SILFunction *Fn = getFunction();
      // Don't rerun diagnostics on deserialized functions.
      if (Fn->wasDeserializedCanonical())
        return;

      // Ignore empty functions and straight-line thunks.
      if (Fn->empty() || Fn->isThunk() != IsNotThunk)
        return;

      // If we can't diagnose it, there's no sense analyzing it.
      if (!Fn->hasLocation() || Fn->getLocation().getSourceLoc().isInvalid())
        return;

      if (hasInfinitelyRecursiveApply(Fn)) {
        diagnose(Fn->getModule().getASTContext(),
                 Fn->getLocation().getSourceLoc(),
                 diag::warn_infinite_recursive_function);
      }
    }
  };
} // end anonymous namespace

SILTransform *swift::createDiagnoseInfiniteRecursion() {
  return new DiagnoseInfiniteRecursion();
}
