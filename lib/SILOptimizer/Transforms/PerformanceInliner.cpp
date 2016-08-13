//===--- PerformanceInliner.cpp - Basic cost based performance inlining ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
#include "swift/SIL/InstructionUtils.h"
#include "swift/SILOptimizer/Analysis/ColdBlockInfo.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/FunctionOrder.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/ConstantFolding.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/MapVector.h"
#include <functional>


using namespace swift;

STATISTIC(NumFunctionsInlined, "Number of functions inlined");

llvm::cl::opt<bool> PrintShortestPathInfo(
    "print-shortest-path-info", llvm::cl::init(false),
    llvm::cl::desc("Print shortest-path information for inlining"));

//===----------------------------------------------------------------------===//
//                               ConstantTracker
//===----------------------------------------------------------------------===//

// Tracks constants in the caller and callee to get an estimation of what
// values get constant if the callee is inlined.
// This can be seen as a "simulation" of several optimizations: SROA, mem2reg
// and constant propagation.
// Note that this is only a simplified model and not correct in all cases.
// For example aliasing information is not taken into account.
class ConstantTracker {

  // Represents a value in integer constant evaluation.
  struct IntConst {
    IntConst() : isValid(false), isFromCaller(false) { }

    IntConst(const APInt &value, bool isFromCaller) :
    value(value), isValid(true), isFromCaller(isFromCaller) { }

    // The actual value.
    APInt value;

    // True if the value is valid, i.e. could be evaluated to a constant.
    bool isValid;

    // True if the value is only valid, because a constant is passed to the
    // callee. False if constant propagation could do the same job inside the
    // callee without inlining it.
    bool isFromCaller;
  };
  
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
  
  // Cache for evaluated constants.
  llvm::SmallDenseMap<BuiltinInst *, IntConst> constCache;

  // The caller/callee function which is tracked.
  SILFunction *F;
  
  // The constant tracker of the caller function (null if this is the
  // tracker of the callee).
  ConstantTracker *callerTracker;
  
  // The apply instruction in the caller (null if this is the tracker of the
  // callee).
  FullApplySite AI;
  
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
        return AI.getArgument(arg->getIndex());
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
  
  // Gets the estimated definition of a value.
  SILInstruction *getDef(SILValue val, ProjectionPath &projStack);

  // Gets the estimated integer constant result of a builtin.
  IntConst getBuiltinConst(BuiltinInst *BI, int depth);
  
public:
  
  // Constructor for the caller function.
  ConstantTracker(SILFunction *function) :
    F(function), callerTracker(nullptr), AI()
  { }
  
  // Constructor for the callee function.
  ConstantTracker(SILFunction *function, ConstantTracker *caller,
                  FullApplySite callerApply) :
     F(function), callerTracker(caller), AI(callerApply)
  { }
  
  void beginBlock() {
    // Currently we don't do any sophisticated dataflow analysis, so we keep
    // the memoryContent alive only for a single block.
    memoryContent.clear();
  }

  // Must be called for each instruction visited in dominance order.
  void trackInst(SILInstruction *inst);
  
  // Gets the estimated definition of a value.
  SILInstruction *getDef(SILValue val) {
    ProjectionPath projStack(val->getType());
    return getDef(val, projStack);
  }
  
  // Gets the estimated definition of a value if it is in the caller.
  SILInstruction *getDefInCaller(SILValue val) {
    SILInstruction *def = getDef(val);
    if (def && def->getFunction() != F)
      return def;
    return nullptr;
  }

  bool isStackAddrInCaller(SILValue val) {
    if (SILValue Param = getParam(stripAddressProjections(val))) {
      return isa<AllocStackInst>(stripAddressProjections(Param));
    }
    return false;
  }

  // Gets the estimated integer constant of a value.
  IntConst getIntConst(SILValue val, int depth = 0);

  SILBasicBlock *getTakenBlock(TermInst *term);
};

void ConstantTracker::trackInst(SILInstruction *inst) {
  if (auto *LI = dyn_cast<LoadInst>(inst)) {
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
    if (Projection::isAddressProjection(addr)) {
      SILInstruction *I = cast<SILInstruction>(addr);
      if (Result) {
        Result->push_back(Projection(I));
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

SILInstruction *ConstantTracker::getDef(SILValue val,
                                        ProjectionPath &projStack) {

  // Track the value up the dominator tree.
  for (;;) {
    if (SILInstruction *inst = dyn_cast<SILInstruction>(val)) {
      if (Projection::isObjectProjection(inst)) {
        // Extract a member from a struct/tuple/enum.
        projStack.push_back(Projection(inst));
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
      } else if (isa<ThinToThickFunctionInst>(inst)) {
        val = inst->getOperand(0);
        continue;
      }
      return inst;
    } else if (SILValue param = getParam(val)) {
      // Continue in the caller.
      val = param;
      continue;
    }
    return nullptr;
  }
}

ConstantTracker::IntConst ConstantTracker::getBuiltinConst(BuiltinInst *BI, int depth) {
  const BuiltinInfo &Builtin = BI->getBuiltinInfo();
  OperandValueArrayRef Args = BI->getArguments();
  switch (Builtin.ID) {
    default: break;

      // Fold comparison predicates.
#define BUILTIN(id, name, Attrs)
#define BUILTIN_BINARY_PREDICATE(id, name, attrs, overload) \
case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    {
      IntConst lhs = getIntConst(Args[0], depth);
      IntConst rhs = getIntConst(Args[1], depth);
      if (lhs.isValid && rhs.isValid) {
        return IntConst(constantFoldComparison(lhs.value, rhs.value,
                                               Builtin.ID),
                        lhs.isFromCaller || rhs.isFromCaller);
      }
      break;
    }

    case BuiltinValueKind::SAddOver:
    case BuiltinValueKind::UAddOver:
    case BuiltinValueKind::SSubOver:
    case BuiltinValueKind::USubOver:
    case BuiltinValueKind::SMulOver:
    case BuiltinValueKind::UMulOver: {
      IntConst lhs = getIntConst(Args[0], depth);
      IntConst rhs = getIntConst(Args[1], depth);
      if (lhs.isValid && rhs.isValid) {
        bool IgnoredOverflow;
        return IntConst(constantFoldBinaryWithOverflow(lhs.value, rhs.value,
                        IgnoredOverflow,
                        getLLVMIntrinsicIDForBuiltinWithOverflow(Builtin.ID)),
                          lhs.isFromCaller || rhs.isFromCaller);
      }
      break;
    }

    case BuiltinValueKind::SDiv:
    case BuiltinValueKind::SRem:
    case BuiltinValueKind::UDiv:
    case BuiltinValueKind::URem: {
      IntConst lhs = getIntConst(Args[0], depth);
      IntConst rhs = getIntConst(Args[1], depth);
      if (lhs.isValid && rhs.isValid && rhs.value != 0) {
        bool IgnoredOverflow;
        return IntConst(constantFoldDiv(lhs.value, rhs.value,
                                        IgnoredOverflow, Builtin.ID),
                        lhs.isFromCaller || rhs.isFromCaller);
      }
      break;
    }

    case BuiltinValueKind::And:
    case BuiltinValueKind::AShr:
    case BuiltinValueKind::LShr:
    case BuiltinValueKind::Or:
    case BuiltinValueKind::Shl:
    case BuiltinValueKind::Xor: {
      IntConst lhs = getIntConst(Args[0], depth);
      IntConst rhs = getIntConst(Args[1], depth);
      if (lhs.isValid && rhs.isValid) {
        return IntConst(constantFoldBitOperation(lhs.value, rhs.value,
                                                 Builtin.ID),
                        lhs.isFromCaller || rhs.isFromCaller);
      }
      break;
    }

    case BuiltinValueKind::Trunc:
    case BuiltinValueKind::ZExt:
    case BuiltinValueKind::SExt:
    case BuiltinValueKind::TruncOrBitCast:
    case BuiltinValueKind::ZExtOrBitCast:
    case BuiltinValueKind::SExtOrBitCast: {
      IntConst val = getIntConst(Args[0], depth);
      if (val.isValid) {
        return IntConst(constantFoldCast(val.value, Builtin), val.isFromCaller);
      }
      break;
    }
  }
  return IntConst();
}

// Tries to evaluate the integer constant of a value. The \p depth is used
// to limit the complexity.
ConstantTracker::IntConst ConstantTracker::getIntConst(SILValue val, int depth) {

  // Don't spend too much time with constant evaluation.
  if (depth >= 10)
    return IntConst();
  
  SILInstruction *I = getDef(val);
  if (!I)
    return IntConst();
  
  if (auto *IL = dyn_cast<IntegerLiteralInst>(I)) {
    return IntConst(IL->getValue(), IL->getFunction() != F);
  }
  if (auto *BI = dyn_cast<BuiltinInst>(I)) {
    if (constCache.count(BI) != 0)
      return constCache[BI];
    
    IntConst builtinConst = getBuiltinConst(BI, depth + 1);
    constCache[BI] = builtinConst;
    return builtinConst;
  }
  return IntConst();
}

// Returns the taken block of a terminator instruction if the condition turns
// out to be constant.
SILBasicBlock *ConstantTracker::getTakenBlock(TermInst *term) {
  if (CondBranchInst *CBI = dyn_cast<CondBranchInst>(term)) {
    IntConst condConst = getIntConst(CBI->getCondition());
    if (condConst.isFromCaller) {
      return condConst.value != 0 ? CBI->getTrueBB() : CBI->getFalseBB();
    }
    return nullptr;
  }
  if (SwitchValueInst *SVI = dyn_cast<SwitchValueInst>(term)) {
    IntConst switchConst = getIntConst(SVI->getOperand());
    if (switchConst.isFromCaller) {
      for (unsigned Idx = 0; Idx < SVI->getNumCases(); ++Idx) {
        auto switchCase = SVI->getCase(Idx);
        if (auto *IL = dyn_cast<IntegerLiteralInst>(switchCase.first)) {
          if (switchConst.value == IL->getValue())
            return switchCase.second;
        } else {
          return nullptr;
        }
      }
      if (SVI->hasDefault())
        return SVI->getDefaultBB();
    }
    return nullptr;
  }
  if (SwitchEnumInst *SEI = dyn_cast<SwitchEnumInst>(term)) {
    if (SILInstruction *def = getDefInCaller(SEI->getOperand())) {
      if (EnumInst *EI = dyn_cast<EnumInst>(def)) {
        for (unsigned Idx = 0; Idx < SEI->getNumCases(); ++Idx) {
          auto enumCase = SEI->getCase(Idx);
          if (enumCase.first == EI->getElement())
            return enumCase.second;
        }
        if (SEI->hasDefault())
          return SEI->getDefaultBB();
      }
    }
    return nullptr;
  }
  if (CheckedCastBranchInst *CCB = dyn_cast<CheckedCastBranchInst>(term)) {
    if (SILInstruction *def = getDefInCaller(CCB->getOperand())) {
      if (UpcastInst *UCI = dyn_cast<UpcastInst>(def)) {
        SILType castType = UCI->getOperand()->getType();
        if (CCB->getCastType().isExactSuperclassOf(castType)) {
          return CCB->getSuccessBB();
        }
        if (!castType.isBindableToSuperclassOf(CCB->getCastType())) {
          return CCB->getFailureBB();
        }
      }
    }
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
//                           Shortest path analysis
//===----------------------------------------------------------------------===//

/// Computes the length of the shortest path through a block in a scope.
///
/// A scope is either a loop or the whole function. The "length" is an
/// estimation of the execution time. For example if the shortest path of a
/// block B is N, it means that the execution time of the scope (either
/// function or a single loop iteration), when execution includes the block, is
/// at least N, regardless of what branches are taken.
///
/// The shortest path of a block is the sum of the shortest path from the scope
/// entry and the shortest path to the scope exit.
class ShortestPathAnalysis {
public:
  enum {
    /// We assume that cold block take very long to execute.
    ColdBlockLength = 1000,

    /// Our assumption on how many times a loop is executed.
    LoopCount = 10,

    /// To keep things simple we only analyse up to this number of nested loops.
    MaxNumLoopLevels = 4,

    /// The "weight" for the benefit which a single loop nest gives.
    SingleLoopWeight = 4,

    /// Pretty large but small enough to add something without overflowing.
    InitialDist = (1 << 29)
  };

  /// A weight for an inlining benefit.
  class Weight {
    /// How long does it take to execute the scope (either loop or the whole
    /// function) of the call to inline? The larger it is the less important it
    /// is to inline.
    int ScopeLength;

    /// Represents the loop nest. The larger it is the more important it is to
    /// inline
    int LoopWeight;

    friend class ShortestPathAnalysis;

  public:
    Weight(int ScopeLength, int LoopWeight) : ScopeLength(ScopeLength),
                                              LoopWeight(LoopWeight) { }

    Weight(const Weight &RHS, int AdditionalLoopWeight) :
      Weight(RHS.ScopeLength, RHS.LoopWeight + AdditionalLoopWeight) { }

    Weight() : ScopeLength(-1), LoopWeight(0) {
      assert(!isValid());
    }

    bool isValid() const { return ScopeLength >= 0; }

    /// Updates the \p Benefit by weighting the \p Importance.
    void updateBenefit(int &Benefit, int Importance) const {
      assert(isValid());
      int newBenefit = 0;

      // Use some heuristics. The basic idea is: length is bad, loops are good.
      if (ScopeLength > 320) {
        newBenefit = Importance;
      } else if (ScopeLength > 160) {
        newBenefit = Importance + LoopWeight * 4;
      } else if (ScopeLength > 80) {
        newBenefit = Importance + LoopWeight * 8;
      } else if (ScopeLength > 40) {
        newBenefit = Importance + LoopWeight * 12;
      } else if (ScopeLength > 20) {
        newBenefit = Importance + LoopWeight * 16;
      } else {
        newBenefit = Importance + 20 + LoopWeight * 16;
      }
      // We don't accumulate the benefit instead we max it.
      if (newBenefit > Benefit)
        Benefit = newBenefit;
    }

#ifndef NDEBUG
    friend raw_ostream &operator<<(raw_ostream &os, const Weight &W) {
      os << W.LoopWeight << '/' << W.ScopeLength;
      return os;
    }
#endif
  };

private:
  /// The distances for a block in it's scope (either loop or function).
  struct Distances {
    /// The shortest distance from the scope entry to the block entry.
    int DistFromEntry = InitialDist;

    /// The shortest distance from the block entry to the scope exit.
    int DistToExit = InitialDist;

    /// Additional length to account for loop iterations. It's != 0 for loop
    /// headers (or loop predecessors).
    int LoopHeaderLength = 0;
  };

  /// Holds the distance information for a block in all it's scopes, i.e. in all
  /// its containing loops and the function itself.
  struct BlockInfo {
  private:
    /// Distances for all scopes. Element 0 refers to the function, elements
    /// > 0 to loops.
    Distances Dists[MaxNumLoopLevels];
  public:
    /// The length of the block itself.
    int Length = 0;

    /// Returns the distances for the loop with nesting level \p LoopDepth.
    Distances &getDistances(int LoopDepth) {
      assert(LoopDepth >= 0 && LoopDepth < MaxNumLoopLevels);
      return Dists[LoopDepth];
    }

    /// Returns the length including the LoopHeaderLength for a given
    /// \p LoopDepth.
    int getLength(int LoopDepth) {
      return Length + getDistances(LoopDepth).LoopHeaderLength;
    }

    /// Returns the length of the shortest path of this block in the loop with
    /// nesting level \p LoopDepth.
    int getScopeLength(int LoopDepth) {
      const Distances &D = getDistances(LoopDepth);
      return D.DistFromEntry + D.DistToExit;
    }
  };

  SILFunction *F;
  SILLoopInfo *LI;
  llvm::DenseMap<const SILBasicBlock *, BlockInfo *> BlockInfos;
  std::vector<BlockInfo> BlockInfoStorage;

  BlockInfo *getBlockInfo(const SILBasicBlock *BB) {
    BlockInfo *BI = BlockInfos[BB];
    assert(BI);
    return BI;
  }

  // Utility functions to make the template solveDataFlow compilable for a block
  // list containing references _and_ a list containing pointers.

  const SILBasicBlock *getBlock(const SILBasicBlock *BB) { return BB; }
  const SILBasicBlock *getBlock(const SILBasicBlock &BB) { return &BB; }

  /// Returns the minimum distance from all predecessor blocks of \p BB.
  int getEntryDistFromPreds(const SILBasicBlock *BB, int LoopDepth);

  /// Returns the minimum distance from all successor blocks of \p BB.
  int getExitDistFromSuccs(const SILBasicBlock *BB, int LoopDepth);

  /// Computes the distances by solving the dataflow problem for all \p Blocks
  /// in a scope.
  template <typename BlockList>
  void solveDataFlow(const BlockList &Blocks, int LoopDepth) {
    bool Changed = false;

    // Compute the distances from the entry block.
    do {
      Changed = false;
      for (auto &Block : Blocks) {
        const SILBasicBlock *BB = getBlock(Block);
        BlockInfo *BBInfo = getBlockInfo(BB);
        int DistFromEntry = getEntryDistFromPreds(BB, LoopDepth);
        Distances &BBDists = BBInfo->getDistances(LoopDepth);
        if (DistFromEntry < BBDists.DistFromEntry) {
          BBDists.DistFromEntry = DistFromEntry;
          Changed = true;
        }
      }
    } while (Changed);

    // Compute the distances to the exit block.
    do {
      Changed = false;
      for (auto &Block : reverse(Blocks)) {
        const SILBasicBlock *BB = getBlock(Block);
        BlockInfo *BBInfo = getBlockInfo(BB);
        int DistToExit =
          getExitDistFromSuccs(BB, LoopDepth) + BBInfo->getLength(LoopDepth);
        Distances &BBDists = BBInfo->getDistances(LoopDepth);
        if (DistToExit < BBDists.DistToExit) {
          BBDists.DistToExit = DistToExit;
          Changed = true;
        }
      }
    } while (Changed);
  }

  /// Analyze \p Loop and all its inner loops.
  void analyzeLoopsRecursively(SILLoop *Loop, int LoopDepth);

  void printFunction(llvm::raw_ostream &OS);

  void printLoop(llvm::raw_ostream &OS, SILLoop *Loop, int LoopDepth);

  void printBlockInfo(llvm::raw_ostream &OS, SILBasicBlock *BB, int LoopDepth);

public:
  ShortestPathAnalysis(SILFunction *F, SILLoopInfo *LI) : F(F), LI(LI) { }

  bool isValid() const { return !BlockInfos.empty(); }

  /// Compute the distances. The function \p getApplyLength returns the length
  /// of a function call.
  template <typename Func>
  void analyze(ColdBlockInfo &CBI, Func getApplyLength) {
    assert(!isValid());

    BlockInfoStorage.resize(F->size());

    // First step: compute the length of the blocks.
    unsigned BlockIdx = 0;
    for (SILBasicBlock &BB : *F) {
      int Length = 0;
      if (CBI.isCold(&BB)) {
        Length = ColdBlockLength;
      } else {
        for (SILInstruction &I : BB) {
          if (auto FAS = FullApplySite::isa(&I)) {
            Length += getApplyLength(FAS);
          } else {
            Length += (int)instructionInlineCost(I);
          }
        }
      }
      BlockInfo *BBInfo = &BlockInfoStorage[BlockIdx++];
      BlockInfos[&BB] = BBInfo;
      BBInfo->Length = Length;

      // Initialize the distances for the entry and exit blocks, used when
      // computing the distances for the function itself.
      if (&BB == &F->front())
        BBInfo->getDistances(0).DistFromEntry = 0;

      if (isa<ReturnInst>(BB.getTerminator()))
        BBInfo->getDistances(0).DistToExit = Length;
      else if (isa<ThrowInst>(BB.getTerminator()))
        BBInfo->getDistances(0).DistToExit = Length + ColdBlockLength;
    }
    // Compute the distances for all loops in the function.
    for (SILLoop *Loop : *LI) {
      analyzeLoopsRecursively(Loop, 1);
    }
    // Compute the distances for the function itself.
    solveDataFlow(F->getBlocks(), 0);
  }

  /// Returns the length of the shortest path of the block \p BB in the loop
  /// with nesting level \p LoopDepth. If \p LoopDepth is 0 ir returns the
  /// shortest path in the function.
  int getScopeLength(SILBasicBlock *BB, int LoopDepth) {
    assert(BB->getParent() == F);
    if (LoopDepth >= MaxNumLoopLevels)
      LoopDepth = MaxNumLoopLevels - 1;
    return getBlockInfo(BB)->getScopeLength(LoopDepth);
  }

  /// Returns the weight of block \p BB also considering the \p CallerWeight
  /// which is the weight of the call site's block in the caller.
  Weight getWeight(SILBasicBlock *BB, Weight CallerWeight);

  void dump();
};

int ShortestPathAnalysis::getEntryDistFromPreds(const SILBasicBlock *BB,
                                                int LoopDepth) {
  int MinDist = InitialDist;
  for (SILBasicBlock *Pred : BB->getPreds()) {
    BlockInfo *PredInfo = getBlockInfo(Pred);
    Distances &PDists = PredInfo->getDistances(LoopDepth);
    int DistFromEntry = PDists.DistFromEntry + PredInfo->Length +
                          PDists.LoopHeaderLength;
    assert(DistFromEntry >= 0);
    if (DistFromEntry < MinDist)
      MinDist = DistFromEntry;
  }
  return MinDist;
}

int ShortestPathAnalysis::getExitDistFromSuccs(const SILBasicBlock *BB,
                                               int LoopDepth) {
  int MinDist = InitialDist;
  for (const SILSuccessor &Succ : BB->getSuccessors()) {
    BlockInfo *SuccInfo = getBlockInfo(Succ);
    Distances &SDists = SuccInfo->getDistances(LoopDepth);
    if (SDists.DistToExit < MinDist)
      MinDist = SDists.DistToExit;
  }
  return MinDist;
}

/// Detect an edge from the loop pre-header's predecessor to the loop exit
/// block. Such an edge "short-cuts" a loop if it is never iterated. But usually
/// it is the less frequent case and we want to ignore it.
/// E.g. it handles the case of N==0 for
///     for i in 0..<N { ... }
/// If the \p Loop has such an edge the source block of this edge is returned,
/// which is the predecessor of the loop pre-header.
static SILBasicBlock *detectLoopBypassPreheader(SILLoop *Loop) {
  SILBasicBlock *Pred = Loop->getLoopPreheader();
  if (!Pred)
    return nullptr;

  SILBasicBlock *PredPred = Pred->getSinglePredecessor();
  if (!PredPred)
    return nullptr;

  auto *CBR = dyn_cast<CondBranchInst>(PredPred->getTerminator());
  if (!CBR)
    return nullptr;

  SILBasicBlock *Succ = (CBR->getTrueBB() == Pred ? CBR->getFalseBB() :
                                                    CBR->getTrueBB());

  for (SILBasicBlock *PredOfSucc : Succ->getPreds()) {
    SILBasicBlock *Exiting = PredOfSucc->getSinglePredecessor();
    if (!Exiting)
      Exiting = PredOfSucc;
    if (Loop->contains(Exiting))
      return PredPred;
  }
  return nullptr;
}

void ShortestPathAnalysis::analyzeLoopsRecursively(SILLoop *Loop, int LoopDepth) {
  if (LoopDepth >= MaxNumLoopLevels)
    return;

  // First dive into the inner loops.
  for (SILLoop *SubLoop : Loop->getSubLoops()) {
    analyzeLoopsRecursively(SubLoop, LoopDepth + 1);
  }

  BlockInfo *HeaderInfo = getBlockInfo(Loop->getHeader());
  Distances &HeaderDists = HeaderInfo->getDistances(LoopDepth);

  // Initial values for the entry (== header) and exit-predecessor (== header as
  // well).
  HeaderDists.DistFromEntry = 0;
  HeaderDists.DistToExit = 0;

  solveDataFlow(Loop->getBlocks(), LoopDepth);

  int LoopLength = getExitDistFromSuccs(Loop->getHeader(), LoopDepth) +
  HeaderInfo->getLength(LoopDepth);
  HeaderDists.DistToExit = LoopLength;

  // If there is a loop bypass edge, add the loop length to the loop pre-pre-
  // header instead to the header. This actually let us ignore the loop bypass
  // edge in the length calculation for the loop's parent scope.
  if (SILBasicBlock *Bypass = detectLoopBypassPreheader(Loop))
    HeaderInfo = getBlockInfo(Bypass);

  // Add the full loop length (= assumed-iteration-count * length) to the loop
  // header so that it is considered in the parent scope.
  HeaderInfo->getDistances(LoopDepth - 1).LoopHeaderLength =
    LoopCount * LoopLength;
}

ShortestPathAnalysis::Weight ShortestPathAnalysis::
getWeight(SILBasicBlock *BB, Weight CallerWeight) {
  assert(BB->getParent() == F);

  SILLoop *Loop = LI->getLoopFor(BB);
  if (!Loop) {
    // We are not in a loop. So just account the length of our function scope
    // in to the length of the CallerWeight.
    return Weight(CallerWeight.ScopeLength + getScopeLength(BB, 0),
                  CallerWeight.LoopWeight);
  }
  int LoopDepth = Loop->getLoopDepth();
  // Deal with the corner case of having more than 4 nested loops.
  while (LoopDepth >= MaxNumLoopLevels) {
    --LoopDepth;
    Loop = Loop->getParentLoop();
  }
  Weight W(getScopeLength(BB, LoopDepth), SingleLoopWeight);

  // Add weights for all the loops BB is in.
  while (Loop) {
    assert(LoopDepth > 0);
    BlockInfo *HeaderInfo = getBlockInfo(Loop->getHeader());
    int InnerLoopLength = HeaderInfo->getScopeLength(LoopDepth) *
                            ShortestPathAnalysis::LoopCount;
    int OuterLoopWeight = SingleLoopWeight;
    int OuterScopeLength = HeaderInfo->getScopeLength(LoopDepth - 1);

    // Reaching the outermost loop, we use the CallerWeight to get the outer
    // length+loopweight.
    if (LoopDepth == 1) {
      // If the apply in the caller is not in a significant loop, just stop with
      // what we have now.
      if (CallerWeight.LoopWeight < 4)
        return W;

      // If this function is part of the caller's scope length take the caller's
      // scope length. Note: this is not the case e.g. if the apply is in a
      // then-branch of an if-then-else in the caller and the else-branch is
      // the short path.
      if (CallerWeight.ScopeLength > OuterScopeLength)
        OuterScopeLength = CallerWeight.ScopeLength;
      OuterLoopWeight = CallerWeight.LoopWeight;
    }
    assert(OuterScopeLength >= InnerLoopLength);

    // If the current loop is only a small part of its outer loop, we don't
    // take the outer loop that much into account. Only if the current loop is
    // actually the "main part" in the outer loop we add the full loop weight
    // for the outer loop.
    if (OuterScopeLength < InnerLoopLength * 2) {
      W.LoopWeight += OuterLoopWeight - 1;
    } else if (OuterScopeLength < InnerLoopLength * 3) {
      W.LoopWeight += OuterLoopWeight - 2;
    } else if (OuterScopeLength < InnerLoopLength * 4) {
      W.LoopWeight += OuterLoopWeight - 3;
    } else {
      return W;
    }
    --LoopDepth;
    Loop = Loop->getParentLoop();
  }
  assert(LoopDepth == 0);
  return W;
}

void ShortestPathAnalysis::dump() {
  printFunction(llvm::errs());
}

void ShortestPathAnalysis::printFunction(llvm::raw_ostream &OS) {
  OS << "SPA @" << F->getName() << "\n";
  for (SILBasicBlock &BB : *F) {
    printBlockInfo(OS, &BB, 0);
  }
  for (SILLoop *Loop : *LI) {
    printLoop(OS, Loop, 1);
  }
}

void ShortestPathAnalysis::printLoop(llvm::raw_ostream &OS, SILLoop *Loop,
                                     int LoopDepth) {
  if (LoopDepth >= MaxNumLoopLevels)
    return;
  assert(LoopDepth == (int)Loop->getLoopDepth());

  OS << "Loop bb" << Loop->getHeader()->getDebugID() << ":\n";
  for (SILBasicBlock *BB : Loop->getBlocks()) {
    printBlockInfo(OS, BB, LoopDepth);
  }
  for (SILLoop *SubLoop : Loop->getSubLoops()) {
    printLoop(OS, SubLoop, LoopDepth + 1);
  }
}

void ShortestPathAnalysis::printBlockInfo(llvm::raw_ostream &OS,
                                          SILBasicBlock *BB, int LoopDepth) {
  BlockInfo *BBInfo = getBlockInfo(BB);
  Distances &D = BBInfo->getDistances(LoopDepth);
  OS << "  bb" << BB->getDebugID() << ": length=" << BBInfo->Length << '+'
     << D.LoopHeaderLength << ", d-entry=" << D.DistFromEntry
     << ", d-exit=" << D.DistToExit << '\n';
}

//===----------------------------------------------------------------------===//
//                           Performance Inliner
//===----------------------------------------------------------------------===//

namespace {

// Controls the decision to inline functions with @_semantics, @effect and
// global_init attributes.
enum class InlineSelection {
  Everything,
  NoGlobalInit, // and no availability semantics calls
  NoSemanticsAndGlobalInit
};

using Weight = ShortestPathAnalysis::Weight;

class SILPerformanceInliner {
  /// Specifies which functions not to inline, based on @_semantics and
  /// global_init attributes.
  InlineSelection WhatToInline;

  DominanceAnalysis *DA;
  SILLoopAnalysis *LA;

  // For keys of SILFunction and SILLoop.
  llvm::DenseMap<SILFunction *, ShortestPathAnalysis *> SPAs;
  llvm::SpecificBumpPtrAllocator<ShortestPathAnalysis> SPAAllocator;

  ColdBlockInfo CBI;

  /// The following constants define the cost model for inlining. Some constants
  /// are also defined in ShortestPathAnalysis.
  enum {
    /// The base value for every call: it represents the benefit of removing the
    /// call overhead itself.
    RemovedCallBenefit = 20,

    /// The benefit if the operand of an apply gets constant, e.g. if a closure
    /// is passed to an apply instruction in the callee.
    RemovedClosureBenefit = RemovedCallBenefit + 50,

    /// The benefit if a load can (probably) eliminated because it loads from
    /// a stack location in the caller.
    RemovedLoadBenefit = RemovedCallBenefit + 5,

    /// The benefit if a store can (probably) eliminated because it stores to
    /// a stack location in the caller.
    RemovedStoreBenefit = RemovedCallBenefit + 10,

    /// The benefit if the condition of a terminator instruction gets constant
    /// due to inlining.
    RemovedTerminatorBenefit = RemovedCallBenefit + 10,

    /// The benefit if a retain/release can (probably) be eliminated after
    /// inlining.
    RefCountBenefit = RemovedCallBenefit + 20,

    /// The benefit of a onFastPath builtin.
    FastPathBuiltinBenefit = RemovedCallBenefit + 40,

    /// Approximately up to this cost level a function can be inlined without
    /// increasing the code size.
    TrivialFunctionThreshold = 18,

    /// Configuration for the caller block limit.
    BlockLimitDenominator = 10000,

    /// The assumed execution length of a function call.
    DefaultApplyLength = 10
  };

#ifndef NDEBUG
  SILFunction *LastPrintedCaller = nullptr;
  void dumpCaller(SILFunction *Caller) {
    if (Caller != LastPrintedCaller) {
      llvm::dbgs() << "\nInline into caller: " << Caller->getName() << '\n';
      LastPrintedCaller = Caller;
    }
  }
#endif

  ShortestPathAnalysis *getSPA(SILFunction *F, SILLoopInfo *LI) {
    ShortestPathAnalysis *&SPA = SPAs[F];
    if (!SPA) {
      SPA = new (SPAAllocator.Allocate()) ShortestPathAnalysis(F, LI);
    }
    return SPA;
  }

  SILFunction *getEligibleFunction(FullApplySite AI);

  bool isProfitableToInline(FullApplySite AI,
                            Weight CallerWeight,
                            ConstantTracker &constTracker,
                            int &NumCallerBlocks);

  bool isProfitableInColdBlock(FullApplySite AI, SILFunction *Callee);

  void visitColdBlocks(SmallVectorImpl<FullApplySite> &AppliesToInline,
                       SILBasicBlock *root, DominanceInfo *DT);

  void collectAppliesToInline(SILFunction *Caller,
                              SmallVectorImpl<FullApplySite> &Applies);

public:
  SILPerformanceInliner(InlineSelection WhatToInline, DominanceAnalysis *DA,
                        SILLoopAnalysis *LA)
      : WhatToInline(WhatToInline), DA(DA), LA(LA), CBI(DA) {}

  bool inlineCallsIntoFunction(SILFunction *F);
};

} // namespace

// Return true if the callee has self-recursive calls.
static bool calleeIsSelfRecursive(SILFunction *Callee) {
  for (auto &BB : *Callee)
    for (auto &I : BB)
      if (auto Apply = FullApplySite::isa(&I))
        if (Apply.getReferencedFunction() == Callee)
          return true;
  return false;
}

// Returns the callee of an apply_inst if it is basically inlineable.
SILFunction *SILPerformanceInliner::getEligibleFunction(FullApplySite AI) {

  SILFunction *Callee = AI.getReferencedFunction();

  if (!Callee) {
    return nullptr;
  }

  // Don't inline functions that are marked with the @_semantics or @effects
  // attribute if the inliner is asked not to inline them.
  if (Callee->hasSemanticsAttrs() || Callee->hasEffectsKind()) {
    if (WhatToInline == InlineSelection::NoSemanticsAndGlobalInit) {
      return nullptr;
    }
    // The "availability" semantics attribute is treated like global-init.
    if (Callee->hasSemanticsAttrs() &&
        WhatToInline != InlineSelection::Everything &&
        Callee->hasSemanticsAttrThatStartsWith("availability")) {
      return nullptr;
    }
  } else if (Callee->isGlobalInit()) {
    if (WhatToInline != InlineSelection::Everything) {
      return nullptr;
    }
  }

  // We can't inline external declarations.
  if (Callee->empty() || Callee->isExternalDeclaration()) {
    return nullptr;
  }

  // Explicitly disabled inlining.
  if (Callee->getInlineStrategy() == NoInline) {
    return nullptr;
  }
  
  if (!Callee->shouldOptimize()) {
    return nullptr;
  }

  // We don't support this yet.
  if (AI.hasSubstitutions()) {
    return nullptr;
  }

  SILFunction *Caller = AI.getFunction();

  // We don't support inlining a function that binds dynamic self because we
  // have no mechanism to preserve the original function's local self metadata.
  if (mayBindDynamicSelf(Callee)) {
    // Check if passed Self is the same as the Self of the caller.
    // In this case, it is safe to inline because both functions
    // use the same Self.
    if (AI.hasSelfArgument() && Caller->hasSelfParam()) {
      auto CalleeSelf = stripCasts(AI.getSelfArgument());
      auto CallerSelf = Caller->getSelfArgument();
      if (CalleeSelf != SILValue(CallerSelf))
        return nullptr;
    } else
      return nullptr;
  }

  // Detect self-recursive calls.
  if (Caller == Callee) {
    return nullptr;
  }

  // A non-fragile function may not be inlined into a fragile function.
  if (Caller->isFragile() &&
      !Callee->hasValidLinkageForFragileInline()) {
    if (!Callee->hasValidLinkageForFragileRef()) {
      llvm::errs() << "caller: " << Caller->getName() << "\n";
      llvm::errs() << "callee: " << Callee->getName() << "\n";
      llvm_unreachable("Should never be inlining a resilient function into "
                       "a fragile function");
    }
    return nullptr;
  }

  // Inlining self-recursive functions into other functions can result
  // in excessive code duplication since we run the inliner multiple
  // times in our pipeline
  if (calleeIsSelfRecursive(Callee)) {
    return nullptr;
  }

  return Callee;
}

/// Return true if inlining this call site is profitable.
bool SILPerformanceInliner::isProfitableToInline(FullApplySite AI,
                                              Weight CallerWeight,
                                              ConstantTracker &callerTracker,
                                              int &NumCallerBlocks) {
  SILFunction *Callee = AI.getReferencedFunction();

  if (Callee->getInlineStrategy() == AlwaysInline)
    return true;

  SILLoopInfo *LI = LA->get(Callee);
  ShortestPathAnalysis *SPA = getSPA(Callee, LI);
  assert(SPA->isValid());

  ConstantTracker constTracker(Callee, &callerTracker, AI);
  DominanceInfo *DT = DA->get(Callee);
  SILBasicBlock *CalleeEntry = &Callee->front();
  DominanceOrder domOrder(CalleeEntry, DT, Callee->size());

  // Calculate the inlining cost of the callee.
  int CalleeCost = 0;
  int Benefit = 0;
  
  // Start with a base benefit.
  int BaseBenefit = RemovedCallBenefit;
  const SILOptions &Opts = Callee->getModule().getOptions();
  
  // For some reason -Ounchecked can accept a higher base benefit without
  // increasing the code size too much.
  if (Opts.Optimization == SILOptions::SILOptMode::OptimizeUnchecked)
    BaseBenefit *= 2;

  CallerWeight.updateBenefit(Benefit, BaseBenefit);

  // Go through all blocks of the function, accumulate the cost and find
  // benefits.
  while (SILBasicBlock *block = domOrder.getNext()) {
    constTracker.beginBlock();
    Weight BlockW = SPA->getWeight(block, CallerWeight);

    for (SILInstruction &I : *block) {
      constTracker.trackInst(&I);
      
      CalleeCost += (int)instructionInlineCost(I);

      if (FullApplySite AI = FullApplySite::isa(&I)) {
        
        // Check if the callee is passed as an argument. If so, increase the
        // threshold, because inlining will (probably) eliminate the closure.
        SILInstruction *def = constTracker.getDefInCaller(AI.getCallee());
        if (def && (isa<FunctionRefInst>(def) || isa<PartialApplyInst>(def)))
          BlockW.updateBenefit(Benefit, RemovedClosureBenefit);
      } else if (auto *LI = dyn_cast<LoadInst>(&I)) {
        // Check if it's a load from a stack location in the caller. Such a load
        // might be optimized away if inlined.
        if (constTracker.isStackAddrInCaller(LI->getOperand()))
          BlockW.updateBenefit(Benefit, RemovedLoadBenefit);
      } else if (auto *SI = dyn_cast<StoreInst>(&I)) {
        // Check if it's a store to a stack location in the caller. Such a load
        // might be optimized away if inlined.
        if (constTracker.isStackAddrInCaller(SI->getDest()))
          BlockW.updateBenefit(Benefit, RemovedStoreBenefit);
      } else if (isa<StrongReleaseInst>(&I) || isa<ReleaseValueInst>(&I)) {
        SILValue Op = stripCasts(I.getOperand(0));
        if (SILArgument *Arg = dyn_cast<SILArgument>(Op)) {
          if (Arg->isFunctionArg() && Arg->getArgumentConvention() ==
              SILArgumentConvention::Direct_Guaranteed) {
            BlockW.updateBenefit(Benefit, RefCountBenefit);
          }
        }
      } else if (auto *BI = dyn_cast<BuiltinInst>(&I)) {
        if (BI->getBuiltinInfo().ID == BuiltinValueKind::OnFastPath)
          BlockW.updateBenefit(Benefit, FastPathBuiltinBenefit);
      }
    }
    // Don't count costs in blocks which are dead after inlining.
    SILBasicBlock *takenBlock = constTracker.getTakenBlock(block->getTerminator());
    if (takenBlock) {
      BlockW.updateBenefit(Benefit, RemovedTerminatorBenefit);
      domOrder.pushChildrenIf(block, [=] (SILBasicBlock *child) {
        return child->getSinglePredecessor() != block || child == takenBlock;
      });
    } else {
      domOrder.pushChildren(block);
    }
  }

  if (AI.getFunction()->isThunk()) {
    // Only inline trivial functions into thunks (which will not increase the
    // code size).
    if (CalleeCost > TrivialFunctionThreshold)
      return false;

    DEBUG(
      
      dumpCaller(AI.getFunction());
      llvm::dbgs() << "    decision {" << CalleeCost << " into thunk} " <<
          Callee->getName() << '\n';
    );
    return true;
  }

  // We reduce the benefit if the caller is too large. For this we use a
  // cubic function on the number of caller blocks. This starts to prevent
  // inlining at about 800 - 1000 caller blocks.
  int blockMinus =
    (NumCallerBlocks * NumCallerBlocks) / BlockLimitDenominator *
                        NumCallerBlocks / BlockLimitDenominator;
  Benefit -= blockMinus;

  // This is the final inlining decision.
  if (CalleeCost > Benefit) {
    return false;
  }

  NumCallerBlocks += Callee->size();

  DEBUG(
    dumpCaller(AI.getFunction());
    llvm::dbgs() << "    decision {c=" << CalleeCost << ", b=" << Benefit <<
        ", l=" << SPA->getScopeLength(CalleeEntry, 0) <<
        ", c-w=" << CallerWeight << ", bb=" << Callee->size() <<
        ", c-bb=" << NumCallerBlocks << "} " << Callee->getName() << '\n';
  );
  return true;
}

/// Return true if inlining this call site into a cold block is profitable.
bool SILPerformanceInliner::isProfitableInColdBlock(FullApplySite AI,
                                                    SILFunction *Callee) {
  if (Callee->getInlineStrategy() == AlwaysInline)
    return true;

  int CalleeCost = 0;

  for (SILBasicBlock &Block : *Callee) {
    for (SILInstruction &I : Block) {
      CalleeCost += int(instructionInlineCost(I));
      if (CalleeCost > TrivialFunctionThreshold)
        return false;
    }
  }
  DEBUG(
    dumpCaller(AI.getFunction());
    llvm::dbgs() << "    cold decision {" << CalleeCost << "} " <<
              Callee->getName() << '\n';
  );
  return true;
}

/// Record additional weight increases.
///
/// Why can't we just add the weight when we call isProfitableToInline? Because
/// the additional weight is for _another_ function than the current handled
/// callee.
static void addWeightCorrection(FullApplySite FAS,
                        llvm::DenseMap<FullApplySite, int> &WeightCorrections) {
  SILFunction *Callee = FAS.getReferencedFunction();
  if (Callee && Callee->hasSemanticsAttr("array.uninitialized")) {
    // We want to inline the argument to an array.uninitialized call, because
    // this argument is most likely a call to a function which contains the
    // buffer allocation for the array. It is essential to inline it for stack
    // promotion of the array buffer.
    SILValue BufferArg = FAS.getArgument(0);
    SILValue Base = stripValueProjections(stripCasts(BufferArg));
    if (auto BaseApply = FullApplySite::isa(Base))
      WeightCorrections[BaseApply] += 6;
  }
}

void SILPerformanceInliner::collectAppliesToInline(
    SILFunction *Caller, SmallVectorImpl<FullApplySite> &Applies) {
  DominanceInfo *DT = DA->get(Caller);
  SILLoopInfo *LI = LA->get(Caller);

  llvm::DenseMap<FullApplySite, int> WeightCorrections;

  // Compute the shortest-path analysis for the caller.
  ShortestPathAnalysis *SPA = getSPA(Caller, LI);
  SPA->analyze(CBI, [&](FullApplySite FAS) -> int {
  
    // This closure returns the length of a called function.

    // At this occasion we record additional weight increases.
    addWeightCorrection(FAS, WeightCorrections);

    if (SILFunction *Callee = getEligibleFunction(FAS)) {
      // Compute the shortest-path analysis for the callee.
      SILLoopInfo *CalleeLI = LA->get(Callee);
      ShortestPathAnalysis *CalleeSPA = getSPA(Callee, CalleeLI);
      if (!CalleeSPA->isValid()) {
        CalleeSPA->analyze(CBI, [](FullApplySite FAS) {
          // We don't compute SPA for another call-level. Functions called from
          // the callee are assumed to have DefaultApplyLength.
          return DefaultApplyLength;
        });
      }
      int CalleeLength = CalleeSPA->getScopeLength(&Callee->front(), 0);
      // Just in case the callee is a noreturn function.
      if (CalleeLength >= ShortestPathAnalysis::InitialDist)
        return DefaultApplyLength;
      return CalleeLength;
    }
    // Some unknown function.
    return DefaultApplyLength;
  });

#ifndef NDEBUG
  if (PrintShortestPathInfo) {
    SPA->dump();
  }
#endif

  ConstantTracker constTracker(Caller);
  DominanceOrder domOrder(&Caller->front(), DT, Caller->size());
  int NumCallerBlocks = (int)Caller->size();

  // Go through all instructions and find candidates for inlining.
  // We do this in dominance order for the constTracker.
  SmallVector<FullApplySite, 8> InitialCandidates;
  while (SILBasicBlock *block = domOrder.getNext()) {
    constTracker.beginBlock();
    Weight BlockWeight;

    for (auto I = block->begin(), E = block->end(); I != E; ++I) {
      constTracker.trackInst(&*I);

      if (!FullApplySite::isa(&*I))
        continue;

      FullApplySite AI = FullApplySite(&*I);

      auto *Callee = getEligibleFunction(AI);
      if (Callee) {
        if (!BlockWeight.isValid())
          BlockWeight = SPA->getWeight(block, Weight(0, 0));

        // The actual weight including a possible weight correction.
        Weight W(BlockWeight, WeightCorrections.lookup(AI));

        if (isProfitableToInline(AI, W, constTracker, NumCallerBlocks))
          InitialCandidates.push_back(AI);
      }
    }
    domOrder.pushChildrenIf(block, [&] (SILBasicBlock *child) {
      if (CBI.isSlowPath(block, child)) {
        // Handle cold blocks separately.
        visitColdBlocks(InitialCandidates, child, DT);
        return false;
      }
      return true;
    });
  }

  // Calculate how many times a callee is called from this caller.
  llvm::DenseMap<SILFunction *, unsigned> CalleeCount;
  for (auto AI : InitialCandidates) {
    SILFunction *Callee = AI.getReferencedFunction();
    assert(Callee && "apply_inst does not have a direct callee anymore");
    CalleeCount[Callee]++;
  }

  // Now copy each candidate callee that has a small enough number of
  // call sites into the final set of call sites.
  for (auto AI : InitialCandidates) {
    SILFunction *Callee = AI.getReferencedFunction();
    assert(Callee && "apply_inst does not have a direct callee anymore");

    const unsigned CallsToCalleeThreshold = 1024;
    if (CalleeCount[Callee] <= CallsToCalleeThreshold)
      Applies.push_back(AI);
  }
}

/// \brief Attempt to inline all calls smaller than our threshold.
/// returns True if a function was inlined.
bool SILPerformanceInliner::inlineCallsIntoFunction(SILFunction *Caller) {
  // Don't optimize functions that are marked with the opt.never attribute.
  if (!Caller->shouldOptimize())
    return false;

  // First step: collect all the functions we want to inline.  We
  // don't change anything yet so that the dominator information
  // remains valid.
  SmallVector<FullApplySite, 8> AppliesToInline;
  collectAppliesToInline(Caller, AppliesToInline);

  if (AppliesToInline.empty())
    return false;

  // Second step: do the actual inlining.
  for (auto AI : AppliesToInline) {
    SILFunction *Callee = AI.getReferencedFunction();
    assert(Callee && "apply_inst does not have a direct callee anymore");

    if (!Callee->shouldOptimize()) {
      continue;
    }
    
    SmallVector<SILValue, 8> Args;
    for (const auto &Arg : AI.getArguments())
      Args.push_back(Arg);

    DEBUG(
      dumpCaller(Caller);
      llvm::dbgs() << "    inline [" << Callee->size() << "->" <<
          Caller->size() << "] " << Callee->getName() << "\n";
    );

    SILOpenedArchetypesTracker OpenedArchetypesTracker(*Caller);
    Caller->getModule().registerDeleteNotificationHandler(&OpenedArchetypesTracker);
    // The callee only needs to know about opened archetypes used in
    // the substitution list.
    OpenedArchetypesTracker.registerUsedOpenedArchetypes(AI.getInstruction());

    // Notice that we will skip all of the newly inlined ApplyInsts. That's
    // okay because we will visit them in our next invocation of the inliner.
    TypeSubstitutionMap ContextSubs;
    SILInliner Inliner(*Caller, *Callee,
                       SILInliner::InlineKind::PerformanceInline, ContextSubs,
                       AI.getSubstitutions(),
                       OpenedArchetypesTracker);

    auto Success = Inliner.inlineFunction(AI, Args);
    (void) Success;
    // We've already determined we should be able to inline this, so
    // we expect it to have happened.
    assert(Success && "Expected inliner to inline this function!");

    recursivelyDeleteTriviallyDeadInstructions(AI.getInstruction(), true);

    NumFunctionsInlined++;
  }

  return true;
}

// Find functions in cold blocks which are forced to be inlined.
// All other functions are not inlined in cold blocks.
void SILPerformanceInliner::visitColdBlocks(
    SmallVectorImpl<FullApplySite> &AppliesToInline, SILBasicBlock *Root,
    DominanceInfo *DT) {
  DominanceOrder domOrder(Root, DT);
  while (SILBasicBlock *block = domOrder.getNext()) {
    for (SILInstruction &I : *block) {
      ApplyInst *AI = dyn_cast<ApplyInst>(&I);
      if (!AI)
        continue;

      auto *Callee = getEligibleFunction(AI);
      if (Callee && isProfitableInColdBlock(AI, Callee)) {
        AppliesToInline.push_back(AI);
      }
    }
    domOrder.pushChildren(block);
  }
}


//===----------------------------------------------------------------------===//
//                          Performance Inliner Pass
//===----------------------------------------------------------------------===//

namespace {
class SILPerformanceInlinerPass : public SILFunctionTransform {
  /// Specifies which functions not to inline, based on @_semantics and
  /// global_init attributes.
  InlineSelection WhatToInline;
  std::string PassName;

public:
  SILPerformanceInlinerPass(InlineSelection WhatToInline, StringRef LevelName):
    WhatToInline(WhatToInline), PassName(LevelName) {
    PassName.append(" Performance Inliner");
  }

  void run() override {
    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    SILLoopAnalysis *LA = PM->getAnalysis<SILLoopAnalysis>();

    if (getOptions().InlineThreshold == 0) {
      return;
    }

    SILPerformanceInliner Inliner(WhatToInline, DA, LA);

    assert(getFunction()->isDefinition() &&
           "Expected only functions with bodies!");

    // Inline things into this function, and if we do so invalidate
    // analyses for this function and restart the pipeline so that we
    // can further optimize this function before attempting to inline
    // in it again.
    if (Inliner.inlineCallsIntoFunction(getFunction())) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
      restartPassPipeline();
    }
  }

  StringRef getName() override { return PassName; }
};
} // end anonymous namespace

/// Create an inliner pass that does not inline functions that are marked with
/// the @_semantics, @effects or global_init attributes.
SILTransform *swift::createEarlyInliner() {
  return new SILPerformanceInlinerPass(
    InlineSelection::NoSemanticsAndGlobalInit, "Early");
}

/// Create an inliner pass that does not inline functions that are marked with
/// the global_init attribute or have an "availability" semantics attribute.
SILTransform *swift::createPerfInliner() {
  return new SILPerformanceInlinerPass(InlineSelection::NoGlobalInit, "Middle");
}

/// Create an inliner pass that inlines all functions that are marked with
/// the @_semantics, @effects or global_init attributes.
SILTransform *swift::createLateInliner() {
  return new SILPerformanceInlinerPass(InlineSelection::Everything, "Late");
}
