//===---------- SILGlobalOpt.cpp - Optimize global initializers -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "globalopt"
#include "swift/Basic/DemangleWrappers.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILAnalysis/ColdBlockInfo.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
using namespace swift;

llvm::cl::opt<bool> EnableStaticInitializer("enable-static-init",
                                            llvm::cl::init(false));

namespace {
/// Optimize the placement of global initializers.
///
/// TODO:
///
/// - Use CallGraphAnalysis to move initializers to the module's public entry
///   points.
///
/// - Convert trivial initializers to static initialization. This requires
///   serializing globals.
///
/// - For global "lets", generate addressors that return by value. If we also
///  converted to a static initializer, then remove the load from the addressor.
///
/// - When the addressor is local to the module, be sure it is inlined to allow
///   constant propagation in case of statically initialized "lets".
class SILGlobalOpt {
  SILModule *Module;
  DominanceAnalysis* DA;
  bool HasChanged = false;

  // Map each global initializer to a list of call sites.
  typedef SmallVector<ApplyInst *, 4> GlobalInitCalls;
  llvm::MapVector<SILFunction*, GlobalInitCalls> GlobalInitCallMap;

  // Mark any block that this pass has determined to be inside a loop.
  llvm::DenseSet<SILBasicBlock*> LoopBlocks;
  // Mark any functions for which loops have been analyzed.
  llvm::DenseSet<SILFunction*> LoopCheckedFunctions;
  // Keep track of cold blocks.
  ColdBlockInfo ColdBlocks;

  // Whether we see a "once" call to callees that we currently don't handle.
  bool UnhandledOnceCallee = false;
  // Recored number of times a globalinit_func is called by "once".
  llvm::DenseMap<SILFunction*, unsigned> InitializerCount;
public:
  SILGlobalOpt(SILModule *M, DominanceAnalysis *DA): Module(M), DA(DA),
                                                     ColdBlocks(DA) {}

  bool run();

protected:
  void collectGlobalInitCall(ApplyInst *AI);
  bool isInLoop(SILBasicBlock *CurBB);
  void placeInitializers(SILFunction *InitF, ArrayRef<ApplyInst*> Calls);

  // Update UnhandledOnceCallee and InitializerCount by going through all "once"
  // calls.
  void collectOnceCall(BuiltinInst *AI);
  // Set the static initializer and remove "once" from addressor if a global can
  // be statically initialized.
  void optimizeInitializer(SILFunction *AddrF);
};
} // namespace

/// If this is a call to a global initializer, map it.
void SILGlobalOpt::collectGlobalInitCall(ApplyInst *AI) {
  FunctionRefInst *FR = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FR)
    return;

  SILFunction *F = FR->getReferencedFunction();
  if (!F->isGlobalInit())
    return;

  GlobalInitCallMap[F].push_back(AI);
}

/// Return the callee of a once call.
static SILFunction *getCalleeOfOnceCall(BuiltinInst *BI) {
  assert(BI->getNumOperands() == 2 && "once call should have 3 operands.");
  if (auto *TTTF = dyn_cast<ThinToThickFunctionInst>(BI->getOperand(1))) {
    if (auto *FR = dyn_cast<FunctionRefInst>(TTTF->getOperand()))
       return FR->getReferencedFunction();
  } else if (auto *FR = dyn_cast<FunctionRefInst>(BI->getOperand(1))) {
    return FR->getReferencedFunction();
  }
  return nullptr;
}

/// Update UnhandledOnceCallee and InitializerCount by going through all "once"
/// calls.
void SILGlobalOpt::collectOnceCall(BuiltinInst *BI) {
  if (UnhandledOnceCallee)
    return;

  const BuiltinInfo &Builtin = Module->getBuiltinInfo(BI->getName());
  if (Builtin.ID != BuiltinValueKind::Once)
    return;

  SILFunction *Callee = getCalleeOfOnceCall(BI);
  if (!Callee) {
    DEBUG(llvm::dbgs() << "GlobalOpt: unhandled once callee\n");
    UnhandledOnceCallee = true;
    return;
  }
  if (!Callee->getName().startswith("globalinit_"))
    return;

  // We currently disable optimizing the intializer if a globalinit_func
  // is called by "once" from multiple locations.
  if (!BI->getFunction()->isGlobalInit())
    // If a globalinit_func is called by "once" from a function that is not
    // an addressor, we set count to 2 to disable optimizing the initializer.
    InitializerCount[Callee] = 2;
  else
    InitializerCount[Callee]++;
}

/// return true if this block is inside a loop.
bool SILGlobalOpt::isInLoop(SILBasicBlock *CurBB) {
  SILFunction *F = CurBB->getParent();
  // Catch the common case in which we've already hoisted the initializer.
  if (CurBB == &F->front())
    return false;

  if (LoopCheckedFunctions.insert(F).second) {
    for (auto I = scc_begin(F); !I.isAtEnd(); ++I) {
      if (I.hasLoop())
        for (SILBasicBlock *BB : *I)
          LoopBlocks.insert(BB);
    }
  }
  return LoopBlocks.count(CurBB);
}

/// Optimize placement of initializer calls given a list of calls to the
/// same initializer. All original initialization points must be dominated by
/// the final initialization calls.
///
/// The current heuristic hoists all initialization points within a function to
/// a single dominating call in the outer loop preheader.
void SILGlobalOpt::placeInitializers(SILFunction *InitF,
                                     ArrayRef<ApplyInst*> Calls) {
  DEBUG(llvm::dbgs() << "GlobalOpt: calls to "
        << demangle_wrappers::demangleSymbolAsString(InitF->getName())
        << " : " << Calls.size() << "\n");
  // Map each initializer-containing function to its final initializer call.
  llvm::DenseMap<SILFunction*, ApplyInst*> ParentFuncs;
  for (auto *AI : Calls) {
    assert(AI->getNumArguments() == 0 && "ill-formed global init call");
    assert(cast<FunctionRefInst>(AI->getCallee())->getReferencedFunction()
           == InitF && "wrong init call");

    SILFunction *ParentF = AI->getFunction();
    DominanceInfo *DT = DA->getDomInfo(ParentF);
    auto PFI = ParentFuncs.find(ParentF);
    if (PFI != ParentFuncs.end()) {
      // Found a replacement for this init call.
      // Ensure the replacement dominates the original call site.
      ApplyInst *CommonAI = PFI->second;
      assert(cast<FunctionRefInst>(CommonAI->getCallee())
             ->getReferencedFunction() == InitF &&
             "ill-formed global init call");
      SILBasicBlock *DomBB =
        DT->findNearestCommonDominator(AI->getParent(), CommonAI->getParent());
      if (DomBB != CommonAI->getParent()) {
        CommonAI->moveBefore(DomBB->begin());
        placeFuncRef(CommonAI, DT);
      }
      AI->replaceAllUsesWith(CommonAI);
      AI->eraseFromParent();
      HasChanged = true;
    }
    else {
      // Move this call to the outermost loop preheader.
      SILBasicBlock *BB = AI->getParent();
      typedef llvm::DomTreeNodeBase<SILBasicBlock> DomTreeNode;
      DomTreeNode *Node = DT->getNode(BB);
      while (Node) {
        BB = Node->getBlock();
        if (!isInLoop(BB))
          break;
        Node = Node->getIDom();
      }
      if (BB == AI->getParent()) {
        // BB is either unreachable or not in a loop.
        DEBUG(llvm::dbgs() << "  skipping (not in a loop): " << *AI
              << "  in " << AI->getFunction()->getName() << "\n");
      }
      else {
        DEBUG(llvm::dbgs() << "  hoisting: " << *AI
              << "  in " << AI->getFunction()->getName() << "\n");
        AI->moveBefore(BB->begin());
        placeFuncRef(AI, DT);
        ParentFuncs[ParentF] = AI;
        HasChanged = true;
      }
    }
  }
}

/// Find the globalinit_func by analyzing the body of the addressor.
static SILFunction *findInitializer(SILModule *Module, SILFunction *AddrF,
                                    BuiltinInst *&CallToOnce) {
  // We only handle a single SILBasicBlock for now.
  if (AddrF->size() != 1)
    return nullptr;

  CallToOnce = nullptr;
  SILBasicBlock *BB = &AddrF->front();
  for (auto &I : *BB) {
    // Find the builtin "once" call.
    if (BuiltinInst *BI = dyn_cast<BuiltinInst>(&I)) {
      const BuiltinInfo &Builtin = Module->getBuiltinInfo(BI->getName());
      if (Builtin.ID != BuiltinValueKind::Once)
        continue;

      // Bail if we have multiple "once" calls in the addressor.
      if (CallToOnce)
        return nullptr;

      CallToOnce = BI;
    }
  }
  if (!CallToOnce)
    return nullptr;
  return getCalleeOfOnceCall(CallToOnce);
}

/// We analyze the body of globalinit_func to see if it can be statically
/// initialized. If yes, we set the initial value of the SILGlobalVariable and
/// remove the "once" call to globalinit_func from the addressor.
void SILGlobalOpt::optimizeInitializer(SILFunction *AddrF) {
  if (UnhandledOnceCallee)
    return;

  // Find the initializer and the SILGlobalVariable.
  BuiltinInst *CallToOnce;

  // If the addressor contains a single "once" call, it calls globalinit_func,
  // and the globalinit_func is called by "once" from a single location,
  // continue; otherwise bail.
  auto *InitF = findInitializer(Module, AddrF, CallToOnce);
  if (!InitF || !InitF->getName().startswith("globalinit_") ||
      InitializerCount[InitF] > 1)
    return;

  // If the globalinit_func is trivial, continue; otherwise bail.
  auto *SILG = SILGlobalVariable::getVariableOfStaticInitializer(InitF);
  if (!SILG || !SILG->isDefinition())
    return;

  DEBUG(llvm::dbgs() << "GlobalOpt: use static initializer for " <<
        SILG->getName() << '\n');

  // Remove "once" call from the addressor.
  CallToOnce->eraseFromParent();
  SILG->setInitializer(InitF);

  HasChanged = true;
}

bool SILGlobalOpt::run() {
  for (auto &F : *Module) {
    // Cache cold blocks per function.
    ColdBlockInfo ColdBlocks(DA);
    for (auto &BB : F) {
      bool IsCold = ColdBlocks.isCold(&BB);
      for (auto &I : BB)
        if (BuiltinInst *BI = dyn_cast<BuiltinInst>(&I)) {
          if (EnableStaticInitializer)
            collectOnceCall(BI);
        } else if (ApplyInst *AI = dyn_cast<ApplyInst>(&I)) {
          if (!IsCold)
            collectGlobalInitCall(AI);
        }
    }
  }
  for (auto &InitCalls : GlobalInitCallMap) {
    // Optimize the addressors if possible.
    if (EnableStaticInitializer)
      optimizeInitializer(InitCalls.first);
    placeInitializers(InitCalls.first, InitCalls.second);
  }

  return HasChanged;
}

namespace {
class SILGlobalOptPass : public SILModuleTransform
{
  void run() override {
    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    if (SILGlobalOpt(getModule(), DA).run())
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
  }

  StringRef getName() override { return "SIL Global Optimization"; }
};
} // anonymous

SILTransform *swift::createGlobalOpt() {
  return new SILGlobalOptPass();
}
