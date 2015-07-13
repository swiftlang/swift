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
#include "swift/AST/Mangle.h"
using namespace swift;

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
  void optimizeInitializer(SILFunction *AddrF, GlobalInitCalls &Calls);
  // Replace loads from a global variable by the known value.
  void replaceLoadsByKnownValue(BuiltinInst *CallToOnce,
                                SILFunction *AddrF,
                                SILFunction *InitF,
                                SILGlobalVariable *SILG,
                                GlobalInitCalls &Calls);
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

/// Returns true if the block \p BB is terminated with a cond_br based on an
/// availability check.
static bool isAvailabilityCheck(SILBasicBlock *BB) {
  CondBranchInst *CBR = dyn_cast<CondBranchInst>(BB->getTerminator());
  if (!CBR)
    return false;
  
  ApplyInst *AI = dyn_cast<ApplyInst>(CBR->getCondition());
  if (!AI)
    return false;
  
  FunctionRefInst *FR = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FR)
    return false;
  
  SILFunction *F = FR->getReferencedFunction();
  if (!F->hasDefinedSemantics())
    return false;
  
  return F->getSemanticsString().startswith("availability");
}

/// Returns true if there are any availability checks along the dominator tree
/// from \p From to \p To.
static bool isAvailabilityCheckOnDomPath(SILBasicBlock *From, SILBasicBlock *To,
                                         DominanceInfo *DT) {
  if (From == To)
    return false;

  auto *Node = DT->getNode(To)->getIDom();
  for(;;) {
    SILBasicBlock *BB = Node->getBlock();
    if (isAvailabilityCheck(BB))
      return true;
    if (BB == From)
      return false;
    Node = Node->getIDom();
    assert(Node && "Should have hit To-block");
  }
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
    DominanceInfo *DT = DA->get(ParentF);
    auto PFI = ParentFuncs.find(ParentF);
    ApplyInst *HoistAI = nullptr;
    if (PFI != ParentFuncs.end()) {
      // Found a replacement for this init call.
      // Ensure the replacement dominates the original call site.
      ApplyInst *CommonAI = PFI->second;
      assert(cast<FunctionRefInst>(CommonAI->getCallee())
             ->getReferencedFunction() == InitF &&
             "ill-formed global init call");
      SILBasicBlock *DomBB =
        DT->findNearestCommonDominator(AI->getParent(), CommonAI->getParent());
      
      // We must not move initializers around availability-checks.
      if (!isAvailabilityCheckOnDomPath(DomBB, CommonAI->getParent(), DT)) {
        if (DomBB != CommonAI->getParent()) {
          CommonAI->moveBefore(DomBB->begin());
          placeFuncRef(CommonAI, DT);
          
          // Try to hoist the existing AI again if we move it to another block,
          // e.g. from a loop exit into the loop.
          HoistAI = CommonAI;
        }
        AI->replaceAllUsesWith(CommonAI);
        AI->eraseFromParent();
        HasChanged = true;
      }
    } else {
      ParentFuncs[ParentF] = AI;
      
      // It's the first time we found a call to InitF in this function, so we
      // try to hoist it out of any loop.
      HoistAI = AI;
    }
    if (HoistAI) {
      // Move this call to the outermost loop preheader.
      SILBasicBlock *BB = HoistAI->getParent();
      typedef llvm::DomTreeNodeBase<SILBasicBlock> DomTreeNode;
      DomTreeNode *Node = DT->getNode(BB);
      while (Node) {
        SILBasicBlock *DomParentBB = Node->getBlock();
        if (isAvailabilityCheck(DomParentBB)) {
          DEBUG(llvm::dbgs() << "  don't hoist above availibility check at bb" <<
                DomParentBB->getDebugID() << "\n");
          break;
        }
        BB = DomParentBB;
        if (!isInLoop(BB))
          break;
        Node = Node->getIDom();
      }
      if (BB == HoistAI->getParent()) {
        // BB is either unreachable or not in a loop.
        DEBUG(llvm::dbgs() << "  skipping (not in a loop): " << *HoistAI
              << "  in " << HoistAI->getFunction()->getName() << "\n");
      }
      else {
        DEBUG(llvm::dbgs() << "  hoisting: " << *HoistAI
              << "  in " << HoistAI->getFunction()->getName() << "\n");
        HoistAI->moveBefore(BB->begin());
        placeFuncRef(HoistAI, DT);
        HasChanged = true;
      }
    }
  }
}

/// Create an accessor function from the intializer function.
static SILFunction *genAccessorFromInit(SILFunction *InitF, VarDecl *varDecl) {
  // Generate a getter from the global init function without side-effects.
  llvm::SmallString<20> accessorBuffer;
  llvm::raw_svector_ostream accessorStream(accessorBuffer);
  Mangle::Mangler accessorMangler(accessorStream);
  accessorMangler.mangleGlobalGetterEntity(varDecl);
  auto refType = varDecl->getType().getCanonicalTypeOrNull();
  // Function takes no arguments and returns refType
  SILResultInfo ResultInfo(refType, ResultConvention::Owned);
  SILFunctionType::ExtInfo EInfo;
  EInfo = EInfo.withRepresentation(SILFunctionType::Representation::Thin);
  auto LoweredType = SILFunctionType::get(nullptr, EInfo,
      ParameterConvention::Direct_Owned, { }, ResultInfo, None,
      InitF->getASTContext());
  auto *AccessorF = InitF->getModule().getOrCreateFunction(InitF->getLocation(),
      accessorStream.str(), SILLinkage::PrivateExternal, LoweredType,
      IsBare_t::IsBare, IsTransparent_t::IsNotTransparent,
      IsFragile_t::IsFragile);

  auto *EntryBB = AccessorF->createBasicBlock();
  // Copy InitF into AccessorF
  BasicBlockCloner Cloner(InitF->begin(), EntryBB);
  Cloner.clone();
  AccessorF->setInlined();

  // Find the store instruction
  auto BB = EntryBB;
  SILValue Val;
  SILInstruction *Store;
  for (auto &I : *BB) {
    if (StoreInst *SI = dyn_cast<StoreInst>(&I)) {
      Val = SI->getSrc();
      Store = SI;
      continue;
    }

    if (ReturnInst *RI = dyn_cast<ReturnInst>(&I)) {
      SILBuilderWithScope<1> B(RI);
      B.createReturn(RI->getLoc(), Val);
      eraseUsesOfInstruction(RI);
      recursivelyDeleteTriviallyDeadInstructions(RI, true);
      recursivelyDeleteTriviallyDeadInstructions(Store, true);
      return AccessorF;
    }
  }
  InitF->getModule().getFunctionList().addNodeToList(AccessorF);
  return AccessorF;
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

/// Checks if a given global variable is assigned only once.
static bool isAssignedOnlyOnceInInitializer(SILGlobalVariable *SILG) {
  if (SILG->isLet())
    return true;
  // TODO: If we can prove that a given global variable
  // is assigned only once, during initialization, then
  // we can treat it as if it is a let.
  // If this global is internal or private, it should be
  return false;
}

/// Replace loads from a global variable by the known value.
void SILGlobalOpt::
replaceLoadsByKnownValue(BuiltinInst *CallToOnce, SILFunction *AddrF,
                         SILFunction *InitF, SILGlobalVariable *SILG,
                         GlobalInitCalls &Calls) {
  assert(isAssignedOnlyOnceInInitializer(SILG) &&
         "The value of the initializer should be known at compile-time");
  eraseUsesOfInstruction(CallToOnce);
  recursivelyDeleteTriviallyDeadInstructions(CallToOnce, true);

  // Make this addressor transparent.
  AddrF->setTransparent(IsTransparent_t::IsTransparent);
  for (int i = 0, e = Calls.size(); i < e; ++i) {
    auto &Call = Calls[i];
    SILBuilderWithScope<1> B(Call);
    SmallVector<SILValue, 1> Args;
    auto *NewAI = B.createApply(Call->getLoc(), Call->getCallee(), Args);
    SILValue(Call, 0).replaceAllUsesWith(SILValue(NewAI, 0));
    eraseUsesOfInstruction(Call);
    recursivelyDeleteTriviallyDeadInstructions(Call, true);
    Calls[i] = NewAI;
  }

  // Generate a getter from InitF which returns the value of the global.
  auto *AccessorF = genAccessorFromInit(InitF, SILG->getDecl());

  // Replace all calls of addressor by calls of accessor.
  for (int i = 0, e = Calls.size(); i < e; ++i) {
    ApplyInst *&Call = Calls[i];

    // Now find all uses of Call. They all should be loads, so that
    // we can replace it.
    bool isValid = true;
    for (auto Use : Call->getUses()) {
      if (!isa<PointerToAddressInst>(Use->getUser())) {
        isValid = false;
        break;
      }
    }

    if (!isValid)
      continue;

    SILBuilderWithScope<1> B(Call);
    SmallVector<SILValue, 1> Args;
    auto *AccessorRef = B.createFunctionRef(Call->getLoc(), AccessorF);
    auto *NewAI = B.createApply(Call->getLoc(), AccessorRef, Args);

    for (auto Use : Call->getUses()) {
      auto *PTAI = dyn_cast<PointerToAddressInst>(Use->getUser());
      assert(PTAI && "All uses should be pointer_to_address");
      for (auto PTAIUse : PTAI->getUses()) {
        LoadInst *LI = dyn_cast<LoadInst>(PTAIUse->getUser());
        if (LI) {
          //assert(LI && "All uses should be loads");
          SILValue(LI, 0).replaceAllUsesWith(SILValue(NewAI, 0));
          recursivelyDeleteTriviallyDeadInstructions(LI, true);
          continue;
        }
        StructElementAddrInst *SEAI = dyn_cast<StructElementAddrInst>(
            PTAIUse->getUser());
        if (SEAI) {
          for (auto SEAIUse : SEAI->getUses()) {
            LoadInst *LI = dyn_cast<LoadInst>(SEAIUse->getUser());
            if (LI) {
              //assert(LI && "All uses should be loads");
              auto *SEI = B.createStructExtract(SEAI->getLoc(), NewAI,
                  SEAI->getField());
              SILValue(LI, 0).replaceAllUsesWith(SILValue(SEI, 0));
              recursivelyDeleteTriviallyDeadInstructions(LI, true);
              continue;
            }
            continue;
          }
        }
      }
    }
    eraseUsesOfInstruction(Call);
    recursivelyDeleteTriviallyDeadInstructions(Call, true);
  }

  Calls.clear();
  SILG->setInitializer(InitF);
}

/// We analyze the body of globalinit_func to see if it can be statically
/// initialized. If yes, we set the initial value of the SILGlobalVariable and
/// remove the "once" call to globalinit_func from the addressor.
void SILGlobalOpt::optimizeInitializer(SILFunction *AddrF, GlobalInitCalls &Calls) {
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
  if (!isAssignedOnlyOnceInInitializer(SILG)) {
    CallToOnce->eraseFromParent();
    SILG->setInitializer(InitF);
    HasChanged = true;
    return;
  }

  replaceLoadsByKnownValue(CallToOnce, AddrF, InitF, SILG, Calls);
  HasChanged = true;
}

bool SILGlobalOpt::run() {
  for (auto &F : *Module) {

    // Don't optimize functions that are marked with the opt.never attribute.
    if (!F.shouldOptimize())
      continue;

    // Cache cold blocks per function.
    ColdBlockInfo ColdBlocks(DA);
    for (auto &BB : F) {
      bool IsCold = ColdBlocks.isCold(&BB);
      for (auto &I : BB)
        if (BuiltinInst *BI = dyn_cast<BuiltinInst>(&I)) {
          collectOnceCall(BI);
        } else if (ApplyInst *AI = dyn_cast<ApplyInst>(&I)) {
          if (!IsCold)
            collectGlobalInitCall(AI);
        }
    }
  }
  for (auto &InitCalls : GlobalInitCallMap) {
    // Optimize the addressors if possible.
    optimizeInitializer(InitCalls.first, InitCalls.second);
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
      invalidateAnalysis(SILAnalysis::PreserveKind::Nothing);
  }

  StringRef getName() override { return "SIL Global Optimization"; }
};
} // anonymous

SILTransform *swift::createGlobalOpt() {
  return new SILGlobalOptPass();
}
