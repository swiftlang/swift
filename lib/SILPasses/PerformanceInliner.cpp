//===- PerformanceInliner.cpp - Basic cost based inlining for performance -===//
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

#define DEBUG_TYPE "sil-inliner"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/Projection.h"
#include "swift/SILAnalysis/ColdBlockInfo.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Utils/SILInliner.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/MapVector.h"
using namespace swift;

STATISTIC(NumFunctionsInlined, "Number of functions inlined");

namespace {

  // Threshold for deterministic testing of the inline heuristic.
  // It specifies an instruction cost limit where a simplified model is used
  // for the instruction costs: only builtin instructions have a cost of exactly
  // 1.
  llvm::cl::opt<int> TestThreshold("sil-inline-test-threshold",
                                        llvm::cl::init(-1), llvm::cl::Hidden);
  
  // Tracks constants in the caller and callee to get an estimation of what
  // values get constant if the callee is inlined.
  // This can be seen as a "simulation" of several optimizations: SROA, mem2reg
  // and constant propagation.
  // Note that this is only a simplified model and not correct in all cases.
  // For example aliasing information is not taken into account.
  class ConstantTracker {
    // Links between loaded and stored values.
    // The key is a load instruction, the value is the corresponding store
    // instruction which stores the loaded value. Both, key and value can also
    // be copy_addr instructions.
    llvm::DenseMap<SILInstruction *, SILInstruction *> links;
    
    // The current stored values at memory addresses.
    // The key is the base address of the memory (after skipping address
    // projections). The value are store (or copy_addr) instructions, which
    // store the current value.
    // This is only an estimation, because e.g. it does not consider potential
    // aliasing.
    llvm::DenseMap<SILValue, SILInstruction *> memoryContent;

    // The caller/callee function which is tracked.
    SILFunction *F;
    
    // The constant tracker of the caller function (null if this is the
    // tracker of the callee).
    ConstantTracker *callerTracker;
    
    // The apply instruction in the caller (null if this is the tracker of the
    // callee).
    ApplyInst *AI;
    
    // Walks through address projections and (optionally) collects them.
    // Returns the base address, i.e. the first address which is not a
    // projection.
    SILValue scanProjections(SILValue addr,
                             SmallVectorImpl<Projection> *Result = nullptr);
    
    // Get the stored value for a load. The loadInst can be either a real load
    // or a copy_addr.
    SILValue getStoredValue(SILInstruction *loadInst,
                            ProjectionPath &projStack);

    // Gets the parameter in the caller for a function argument.
    SILValue getParam(SILValue value) {
      if (SILArgument *arg = dyn_cast<SILArgument>(value)) {
        if (AI && arg->isFunctionArg() && arg->getFunction() == F) {
          // Continue at the caller.
          return AI->getArgument(arg->getIndex());
        }
      }
      return SILValue();
    }
    
    SILInstruction *getMemoryContent(SILValue addr) {
      // The memory content can be stored in this ConstantTracker or in the
      // caller's ConstantTracker.
      SILInstruction *storeInst = memoryContent[addr];
      if (storeInst)
        return storeInst;
      if (callerTracker)
        return callerTracker->getMemoryContent(addr);
      return nullptr;
    }
    
    // Gets the estimated constant of a value.
    SILInstruction *getConst(SILValue val, ProjectionPath &projStack);

  public:
    
    // Constructor for the caller function.
    ConstantTracker(SILFunction *function) : F(function), callerTracker(nullptr), AI(nullptr) { }
    
    // Constructor for the callee function.
    ConstantTracker(SILFunction *function, ConstantTracker *caller, ApplyInst *callerApply) :
       F(function), callerTracker(caller), AI(callerApply)
    { }
    
    void beginBlock() {
      // Currently we don't do any sophisticated dataflow analysis, so we keep
      // the memoryContent alive only for a single block.
      memoryContent.clear();
    }

    // Must be called for each instruction visited in dominance order.
    void trackInst(SILInstruction *inst);
    
    // Gets the estimated constant of a value.
    SILInstruction *getConst(SILValue val) {
      ProjectionPath projStack;
      return getConst(val, projStack);
    }
  };

  // Controls the decision to inline functions with @semantics, @effect and
  // global_init attributes.
  enum class InlineSelection {
    Everything,
    NoGlobalInit,
    NoSemanticsAndGlobalInit
  };

  class SILPerformanceInliner {
    /// The inline threashold.
    const unsigned InlineCostThreshold;
    /// Specifies which functions not to inline, based on @semantics and
    /// global_init attributes.
    InlineSelection WhatToInline;

    SILFunction *getEligibleFunction(ApplyInst *AI);
    
    bool isProfitableToInline(ApplyInst *AI, DominanceAnalysis *DA,
                              ConstantTracker &constTracker);
    
    void visitColdBlocks(SmallVectorImpl<ApplyInst *> &CallSitesToInline,
                               SILBasicBlock *root, DominanceInfo *DT);

  public:
    SILPerformanceInliner(unsigned threshold,
                          InlineSelection WhatToInline)
      : InlineCostThreshold(threshold),
    WhatToInline(WhatToInline) {}

    bool inlineCallsIntoFunction(SILFunction *F, DominanceAnalysis *DA);
  };
}

//===----------------------------------------------------------------------===//
//                               ConstantTracker
//===----------------------------------------------------------------------===//


void ConstantTracker::trackInst(SILInstruction *inst) {
  if (LoadInst *LI = dyn_cast<LoadInst>(inst)) {
    SILValue baseAddr = scanProjections(LI->getOperand());
    if (SILInstruction *loadLink = getMemoryContent(baseAddr))
       links[LI] = loadLink;
  } else if (StoreInst *SI = dyn_cast<StoreInst>(inst)) {
    SILValue baseAddr = scanProjections(SI->getOperand(1));
    memoryContent[baseAddr] = SI;
  } else if (CopyAddrInst *CAI = dyn_cast<CopyAddrInst>(inst)) {
    if (!CAI->isTakeOfSrc()) {
      // Treat a copy_addr as a load + store
      SILValue loadAddr = scanProjections(CAI->getOperand(0));
      if (SILInstruction *loadLink = getMemoryContent(loadAddr)) {
        links[CAI] = loadLink;
        SILValue storeAddr = scanProjections(CAI->getOperand(1));
        memoryContent[storeAddr] = CAI;
      }
    }
  }
}

SILValue ConstantTracker::scanProjections(SILValue addr,
                                          SmallVectorImpl<Projection> *Result) {
  for (;;) {
    if (Projection::isAddrProjection(addr)) {
      SILInstruction *I = cast<SILInstruction>(addr.getDef());
      if (Result) {
        Optional<Projection> P = Projection::addressProjectionForInstruction(I);
        Result->push_back(P.getValue());
      }
      addr = I->getOperand(0);
      continue;
    }
    if (SILValue param = getParam(addr)) {
      // Go to the caller.
      addr = param;
      continue;
    }
    // Return the base address = the first address which is not a projection.
    return addr;
  }
}

SILValue ConstantTracker::getStoredValue(SILInstruction *loadInst,
                                  ProjectionPath &projStack) {
  SILInstruction *store = links[loadInst];
  if (!store && callerTracker)
    store = callerTracker->links[loadInst];
  if (!store) return SILValue();

  assert(isa<LoadInst>(loadInst) || isa<CopyAddrInst>(loadInst));

  // Push the address projections of the load onto the stack.
  SmallVector<Projection, 4> loadProjections;
  scanProjections(loadInst->getOperand(0), &loadProjections);
  for (const Projection &proj : loadProjections) {
    projStack.push_back(proj);
  }
  
  //  Pop the address projections of the store from the stack.
  SmallVector<Projection, 4> storeProjections;
  scanProjections(store->getOperand(1), &storeProjections);
  for (auto iter = storeProjections.rbegin(); iter != storeProjections.rend();
       ++iter) {
    const Projection &proj = *iter;
    // The corresponding load-projection must match the store-projection.
    if (projStack.empty() || projStack.back() != proj)
      return SILValue();
    projStack.pop_back();
  }
  
  if (isa<StoreInst>(store))
    return store->getOperand(0);

  // The copy_addr instruction is both a load and a store. So we follow the link
  // again.
  assert(isa<CopyAddrInst>(store));
  return getStoredValue(store, projStack);
}

// Get the aggregate member based on the top of the projection stack.
static SILValue getMember(SILInstruction *inst, ProjectionPath &projStack) {
  if (!projStack.empty()) {
    const Projection &proj = projStack.back();
    return proj.getOperandForAggregate(inst);
  }
  return SILValue();
}

SILInstruction *ConstantTracker::getConst(SILValue val,
                                          ProjectionPath &projStack) {
  
  // Track the value up the dominator tree.
  for (;;) {
    if (SILInstruction *inst = dyn_cast<SILInstruction>(val)) {
      switch (inst->getKind()) {
      // partial_apply is not a constant but for the heuristic "as good"
      // as a constant.
      case ValueKind::PartialApplyInst:
      case ValueKind::FunctionRefInst:
      case ValueKind::IntegerLiteralInst:
      case ValueKind::FloatLiteralInst:
        return inst;
      case ValueKind::ThinToThickFunctionInst:
        val = inst->getOperand(0);
        continue;
      default:
        break;
      }
      if (auto proj = Projection::valueProjectionForInstruction(inst)) {
        // Extract a member from a struct/tuple/enum.
        projStack.push_back(proj.getValue());
        val = inst->getOperand(0);
        continue;
      } else if (SILValue member = getMember(inst, projStack)) {
        // The opposite of a projection instruction: composing a struct/tuple.
        projStack.pop_back();
        val = member;
        continue;
      } else if (SILValue loadedVal = getStoredValue(inst, projStack)) {
        // A value loaded from memory.
        val = loadedVal;
        continue;
      }
    } else if (SILValue param = getParam(val)) {
      // Continue in the caller.
      val = param;
      continue;
    }
    return nullptr;
  }
}


//===----------------------------------------------------------------------===//
//                           Performance Inliner
//===----------------------------------------------------------------------===//

// Returns the referenced function of an apply_inst if it is a direct call.
static SILFunction *getReferencedFunction(ApplyInst *AI) {
  auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FRI)
    return nullptr;
  
  return FRI->getReferencedFunction();
}

// Returns the callee of an apply_inst if it is basically inlinable.
SILFunction *SILPerformanceInliner::getEligibleFunction(ApplyInst *AI) {
 
  SILFunction *Callee = getReferencedFunction(AI);
  
  if (!Callee) {
    DEBUG(llvm::dbgs() << "        FAIL: Cannot find inlineable callee.\n");
    return nullptr;
  }

  // Don't inline functions that are marked with the @semantics or @effects
  // attribute if the inliner is asked not to inline them.
  if (Callee->hasDefinedSemantics() || Callee->hasSpecifiedEffectsInfo()) {
    if (WhatToInline == InlineSelection::NoSemanticsAndGlobalInit) {
      DEBUG(llvm::dbgs() << "        FAIL: Function " << Callee->getName()
            << " has special semantics or effects attribute.\n");
      return nullptr;
    }
  } else if (Callee->isGlobalInit()) {
    if (WhatToInline != InlineSelection::Everything) {
      DEBUG(llvm::dbgs() << "        FAIL: Function " << Callee->getName()
            << " has the global-init attribute.\n");
      return nullptr;
    }
  }

  // We can't inline external declarations.
  if (Callee->empty() || Callee->isExternalDeclaration()) {
    DEBUG(llvm::dbgs() << "        FAIL: Cannot inline external " <<
          Callee->getName() << ".\n");
    return nullptr;
  }

  // Explicitly disabled inlining.
  if (Callee->getInlineStrategy() == NoInline) {
    DEBUG(llvm::dbgs() << "        FAIL: noinline attribute on " <<
          Callee->getName() << ".\n");
    return nullptr;
  }
  
  // We don't support this yet.
  if (AI->hasSubstitutions()) {
    DEBUG(llvm::dbgs() << "        FAIL: Generic substitutions on " <<
          Callee->getName() << ".\n");
    return nullptr;
  }

  SILFunction *Caller = AI->getFunction();
  
  // Check for trivial recursions.
  if (Callee == Caller) {
    DEBUG(llvm::dbgs() << "        FAIL: Skipping recursive calls on " <<
          Callee->getName() << ".\n");
    return nullptr;
  }
  
  // A non-fragile function may not be inlined into a fragile function.
  if (Caller->isFragile() && !Callee->isFragile()) {
    DEBUG(llvm::dbgs() << "        FAIL: Can't inline fragile " <<
          Callee->getName() << ".\n");
    return nullptr;
  }
  DEBUG(llvm::dbgs() << "        Eligible callee: " <<
        Callee->getName() << "\n");
  
  return Callee;
}

// Gets the cost of an instruction by using the simplified test-model: only
// builtin instructions have a cost and that's exactly 1.
static unsigned testCost(SILInstruction *I) {
  switch (I->getKind()) {
    case ValueKind::BuiltinInst:
      return 1;
    default:
      return 0;
  }
}

/// Return true if inlining this call site is profitable.
bool SILPerformanceInliner::isProfitableToInline(ApplyInst *AI,
                                              DominanceAnalysis *DA,
                                              ConstantTracker &callerTracker) {
  /// Always inline transparent calls. This should have been done during
  /// MandatoryInlining, but generics are not currenly handled.
  if (AI->isTransparent())
    return true;
  
  SILFunction *Callee = getReferencedFunction(AI);
  
  if (Callee->getInlineStrategy() == AlwaysInline)
    return true;
  
  ConstantTracker constTracker(Callee, &callerTracker, AI);
  SILFunction *Caller = AI->getFunction();
  
  DominanceInfo *DT = DA->getDomInfo(Callee);
  if (!DT->isValid(Callee))
    DT->recalculate(*Callee);
  DominanceOrder domOrder(&Callee->front(), DT, Callee->size());
  
  // Calculate the inlining cost of the callee.
  unsigned CalleeCost = 0;
  unsigned BoostFactor = 1;
  int testThreshold = TestThreshold;

  while (SILBasicBlock *block = domOrder.getNext()) {
    constTracker.beginBlock();
    for (SILInstruction &I : *block) {
      constTracker.trackInst(&I);
      
      auto ICost = instructionInlineCost(I);
      if (ICost == InlineCost::CannotBeInlined)
        return false;
      
      if (testThreshold >= 0) {
        // We are in test-mode: use a simplified cost model.
        CalleeCost += testCost(&I);
      } else {
        // Use the regular cost model.
        CalleeCost += unsigned(ICost);
      }
      
      if (ApplyInst *AI = dyn_cast<ApplyInst>(&I)) {
        
        // Check if the callee is passed as an argument. If so, increase the
        // threshold, because inlining will (probably) eliminate the closure.
        SILInstruction *constVal = constTracker.getConst(AI->getCallee());
        if (constVal && constVal->getFunction() == Caller) {
          DEBUG(llvm::dbgs() << "        Boost: apply const function at" << *AI);
          BoostFactor = 2;
        }
      }
    }
    domOrder.pushChildren(block);
  }

  unsigned Threshold = (testThreshold >= 0 ? (unsigned)testThreshold :
                        InlineCostThreshold);
  Threshold *= BoostFactor;
  if (CalleeCost > Threshold) {
    DEBUG(llvm::dbgs() << "        FAIL: Function too big to inline, "
          "callee cost: " << CalleeCost << ", threshold: " << Threshold << "\n");
    return false;
  }
  DEBUG(llvm::dbgs() << "        Ready to inline, callee cost: " << CalleeCost
        << ", threshold: " << Threshold << "\n");
  return true;
}

/// \brief Attempt to inline all calls smaller than our threshold.
/// returns True if a function was inlined.
bool SILPerformanceInliner::inlineCallsIntoFunction(SILFunction *Caller,
                                                    DominanceAnalysis *DA) {
  bool Changed = false;
  DEBUG(llvm::dbgs() << "Visiting Function: " << Caller->getName() << "\n");

  SmallVector<ApplyInst*, 8> CallSitesToInline;

  // First step: collect all the functions we want to inline.
  // We don't change anything yet, which let's the dominance info kept alive.

  DominanceInfo *DT = DA->getDomInfo(Caller);
  if (!DT->isValid(Caller))
    DT->recalculate(*Caller);

  ConstantTracker constTracker(Caller);
  DominanceOrder domOrder(&Caller->front(), DT, Caller->size());

  // Go through all instructions and find candidates for inlining.
  // We do this in dominance order for the constTracker.
  while (SILBasicBlock *block = domOrder.getNext()) {
    constTracker.beginBlock();
    for (SILInstruction &I : *block) {
      constTracker.trackInst(&I);
      
      if (ApplyInst *AI = dyn_cast<ApplyInst>(&I)) {

        DEBUG(llvm::dbgs() << "    Check:" << *AI);

        auto *Callee = getEligibleFunction(AI);
        if (Callee) {
          if (isProfitableToInline(AI, DA, constTracker))
            CallSitesToInline.push_back(AI);
        }
      }
    }
    domOrder.pushChildrenIf(block, [&] (SILBasicBlock *child) {
      if (ColdBlockInfo::isSlowPath(block, child)) {
        // Handle cold blocks separately.
        visitColdBlocks(CallSitesToInline, child, DT);
        return false;
      }
      return true;
    });
  }

  // Calculate how many times a callee is called from this caller.
  llvm::DenseMap<SILFunction *, unsigned> CalleeCount;
  for (auto AI : CallSitesToInline) {
    SILFunction *Callee = getReferencedFunction(AI);
    assert(Callee && "apply_inst does not have a direct callee anymore");
    CalleeCount[Callee]++;
  }

  // Second step: do the actual inlining.

  for (auto AI : CallSitesToInline) {

    SILFunction *Callee = getReferencedFunction(AI);
    assert(Callee && "apply_inst does not have a direct callee anymore");
    
    const unsigned CallsToCalleeThreshold = 1024;
    if (CalleeCount[Callee] > CallsToCalleeThreshold) {
      DEBUG(llvm::dbgs() << "    FAIL: CalleeCount exceeded:" <<  *AI);
      continue;
    }

    DEBUG(llvm::dbgs() << "    Inline:" <<  *AI);
    
    SmallVector<SILValue, 8> Args;
    for (const auto &Arg : AI->getArguments())
      Args.push_back(Arg);
    
    // Notice that we will skip all of the newly inlined ApplyInsts. That's
    // okay because we will visit them in our next invocation of the inliner.
    TypeSubstitutionMap ContextSubs;
    SILInliner Inliner(*Caller, *Callee,
                       SILInliner::InlineKind::PerformanceInline,
                       ContextSubs, AI->getSubstitutions());
    Inliner.inlineFunction(AI, Args);
    DT->reset();
    NumFunctionsInlined++;
    Changed = true;
  }

  DEBUG(llvm::dbgs() << "\n");
  return Changed;
}

// Find functions in cold blocks which are forced to be inlined.
// All other functions are not inlined in cold blocks.
void SILPerformanceInliner::visitColdBlocks(SmallVectorImpl<ApplyInst *> &
                                               CallSitesToInline,
                                            SILBasicBlock *Root,
                                            DominanceInfo *DT) {
  DominanceOrder domOrder(Root, DT);
  while (SILBasicBlock *block = domOrder.getNext()) {
    for (SILInstruction &I : *block) {
      if (ApplyInst *AI = dyn_cast<ApplyInst>(&I)) {
        auto *Callee = getEligibleFunction(AI);
        if (Callee && (Callee->getInlineStrategy() == AlwaysInline ||
                       AI->isTransparent())) {
          DEBUG(llvm::dbgs() << "    mandatory inline in cold block:" <<  *AI);
          CallSitesToInline.push_back(AI);
        }
      }
    }
    domOrder.pushChildren(block);
  }
}


//===----------------------------------------------------------------------===//
//                          Performane Inliner Pass
//===----------------------------------------------------------------------===//

namespace {
class SILPerformanceInlinerPass : public SILModuleTransform {
  /// Specifies which functions not to inline, based on @semantics and
  /// global_init attributes.
  InlineSelection WhatToInline;
  std::string PassName;
public:
  SILPerformanceInlinerPass(InlineSelection WhatToInline, StringRef LevelName):
    WhatToInline(WhatToInline), PassName(LevelName) {
    PassName.append(" Performance Inliner");
  }

  void run() {
    CallGraphAnalysis* CGA = PM->getAnalysis<CallGraphAnalysis>();
    DominanceAnalysis* DA = PM->getAnalysis<DominanceAnalysis>();

    if (getOptions().InlineThreshold == 0) {
      DEBUG(llvm::dbgs() << "*** The Performance Inliner is disabled ***\n");
      return;
    }

    SILPerformanceInliner inliner(getOptions().InlineThreshold,
                                  WhatToInline);

    bool Changed = false;
    // Inline functions bottom up from the leafs.
    for (auto *F : CGA->getCallGraph().getBottomUpFunctionOrder()) {
      // If F is empty, attempt to link it. Skip it if we fail to do so.
      if (F->empty() &&
          !getModule()->linkFunction(F, SILModule::LinkingMode::LinkAll))
        continue;

      Changed |= inliner.inlineCallsIntoFunction(F, DA);
    }

    // Invalidate the call graph.
    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
  }

  StringRef getName() override { return PassName; }
};
} // end anonymous namespace

/// Create an inliner pass that does not inline functions that are marked with
/// the @semantics, @effects or global_init attributes.
SILTransform *swift::createEarlyInliner() {
  return new SILPerformanceInlinerPass(
    InlineSelection::NoSemanticsAndGlobalInit, "Early");
}

/// Create an inliner pass that does not inline functions that are marked with
/// the global_init attribute.
SILTransform *swift::createPerfInliner() {
  return new SILPerformanceInlinerPass(InlineSelection::NoGlobalInit, "Middle");
}

/// Create an inliner pass that inlines all functions that are marked with
/// the @semantics, @effects or global_init attributes.
SILTransform *swift::createLateInliner() {
  return new SILPerformanceInlinerPass(InlineSelection::Everything, "Late");
}
