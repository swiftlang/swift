//===--- PerformanceInliner.cpp - Basic cost based performance inlining ---===//
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

#define DEBUG_TYPE "sil-inliner"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/Utils/PerformanceInlinerUtils.h"
#include "swift/Strings.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

STATISTIC(NumFunctionsInlined, "Number of functions inlined");

llvm::cl::opt<bool> PrintShortestPathInfo(
    "print-shortest-path-info", llvm::cl::init(false),
    llvm::cl::desc("Print shortest-path information for inlining"));

llvm::cl::opt<bool> EnableSILInliningOfGenerics(
  "sil-inline-generics", llvm::cl::init(true),
  llvm::cl::desc("Enable inlining of generics"));

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

    /// The benefit of being able to devirtualize a call.
    DevirtualizedCallBenefit = RemovedCallBenefit + 300,

    /// The benefit of being able to produce a generic
    /// specializatio for a call.
    GenericSpecializationBenefit = RemovedCallBenefit + 300,

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
                            ConstantTracker &callerTracker,
                            int &NumCallerBlocks);

  bool decideInWarmBlock(FullApplySite AI,
                         Weight CallerWeight,
                         ConstantTracker &callerTracker,
                         int &NumCallerBlocks);

  bool decideInColdBlock(FullApplySite AI, SILFunction *Callee);

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

} // end anonymous namespace

// Return true if the callee has self-recursive calls.
static bool calleeIsSelfRecursive(SILFunction *Callee) {
  for (auto &BB : *Callee)
    for (auto &I : BB)
      if (auto Apply = FullApplySite::isa(&I))
        if (Apply.getReferencedFunction() == Callee)
          return true;
  return false;
}

// Returns true if the callee contains a partial apply instruction,
// whose substitutions list would contain opened existentials after
// inlining.
static bool calleeHasPartialApplyWithOpenedExistentials(FullApplySite AI) {
  if (!AI.hasSubstitutions())
    return false;

  SILFunction *Callee = AI.getReferencedFunction();
  auto Subs = AI.getSubstitutions();

  // Bail if there are no open existentials in the list of substitutions.
  bool HasNoOpenedExistentials = true;
  for (auto Sub : Subs) {
    if (Sub.getReplacement()->hasOpenedExistential()) {
      HasNoOpenedExistentials = false;
      break;
    }
  }

  if (HasNoOpenedExistentials)
    return false;

  auto SubsMap = Callee->getGenericEnvironment()->getSubstitutionMap(Subs);

  for (auto &BB : *Callee) {
    for (auto &I : BB) {
      if (auto PAI = dyn_cast<PartialApplyInst>(&I)) {
        auto PAISubs = PAI->getSubstitutions();
        if (PAISubs.empty())
          continue;
        // Check if any of substitutions would contain open existentials
        // after inlining.
        for (auto PAISub : PAISubs) {
          if (!PAISub.getReplacement()->hasArchetype())
            continue;
          auto NewPAISub =
              PAISub.subst(AI.getModule().getSwiftModule(), SubsMap);
          if (NewPAISub.getReplacement()->hasOpenedExistential())
            return true;
        }
      }
    }
  }

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

  if (!EnableSILInliningOfGenerics && AI.hasSubstitutions()) {
    // Inlining of generics is not allowed.
    return nullptr;
  }

  // IRGen cannot handle partial_applies containing opened_existentials
  // in its substitutions list.
  if (calleeHasPartialApplyWithOpenedExistentials(AI)) {
    return nullptr;
  }

  return Callee;
}

// Returns true if it is possible to perform a generic
// specialization for a given call.
static bool canSpecializeGeneric(ApplySite AI, SILFunction *F,
                                 SubstitutionList Subs) {
  ReabstractionInfo ReInfo(AI, F, Subs);
  return ReInfo.canBeSpecialized();
}

bool SILPerformanceInliner::isProfitableToInline(FullApplySite AI,
                                                 Weight CallerWeight,
                                                 ConstantTracker &callerTracker,
                                                 int &NumCallerBlocks) {
  SILFunction *Callee = AI.getReferencedFunction();
  bool IsGeneric = !AI.getSubstitutions().empty();

  assert(EnableSILInliningOfGenerics || !IsGeneric);

  // Bail out if this generic call can be optimized by means of
  // the generic specialization, because we prefer generic specialization
  // to inlining of generics.
  if (IsGeneric && canSpecializeGeneric(AI, Callee, AI.getSubstitutions()))
    return false;

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

  SubstitutionMap CalleeSubstMap;
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

      if (FullApplySite FAI = FullApplySite::isa(&I)) {
        // Check if the callee is passed as an argument. If so, increase the
        // threshold, because inlining will (probably) eliminate the closure.
        SILInstruction *def = constTracker.getDefInCaller(FAI.getCallee());
        if (def && (isa<FunctionRefInst>(def) || isa<PartialApplyInst>(def)))
          BlockW.updateBenefit(Benefit, RemovedClosureBenefit);
        // Check if inlining the callee would allow for further
        // optimizations like devirtualization or generic specialization. 
        if (!def)
          def = dyn_cast_or_null<SILInstruction>(FAI.getCallee());

        if (!def)
          continue;

        auto Subs = FAI.getSubstitutions();

        // Bail if it is not a generic call or inlining of generics is forbidden.
        if (!EnableSILInliningOfGenerics || Subs.empty())
          continue;

        if (!isa<FunctionRefInst>(def) && !isa<ClassMethodInst>(def) &&
            !isa<WitnessMethodInst>(def))
          continue;

        SmallVector<Substitution, 32> NewSubs;
        SubstitutionMap SubstMap;

        // It is a generic call inside the callee. Check if after inlining
        // it will be possible to perform a generic specialization or
        // devirtualization of this call.

        // Create the list of substitutions as they will be after
        // inlining.
        for (auto Sub : Subs) {
          if (!Sub.getReplacement()->hasArchetype()) {
            // This substitution is a concrete type.
            NewSubs.push_back(Sub);
            continue;
          }
          // This substitution is not a concrete type.
          if (IsGeneric && CalleeSubstMap.empty()) {
            CalleeSubstMap =
                Callee->getGenericEnvironment()->getSubstitutionMap(
                    AI.getSubstitutions());
          }
          auto NewSub = Sub.subst(AI.getModule().getSwiftModule(), CalleeSubstMap);
          NewSubs.push_back(NewSub);
        }

        // Check if the call can be devirtualized.
        if (isa<ClassMethodInst>(def) || isa<WitnessMethodInst>(def) ||
            isa<SuperMethodInst>(def)) {
          // TODO: Take AI.getSubstitutions() into account.
          if (canDevirtualizeApply(FAI, nullptr)) {
            DEBUG(llvm::dbgs() << "Devirtualization will be possible after "
                                  "inlining for the call:\n";
                  FAI.getInstruction()->dumpInContext());
            BlockW.updateBenefit(Benefit, DevirtualizedCallBenefit);
          }
        }

        // Check if a generic specialization would be possible.
        if (isa<FunctionRefInst>(def)) {
          auto CalleeF = FAI.getCalleeFunction();
          if (!canSpecializeGeneric(FAI, CalleeF, NewSubs))
            continue;
          DEBUG(llvm::dbgs() << "Generic specialization will be possible after "
                                "inlining for the call:\n";
                FAI.getInstruction()->dumpInContext());
          BlockW.updateBenefit(Benefit, GenericSpecializationBenefit);
        }
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
        if (auto *Arg = dyn_cast<SILFunctionArgument>(Op)) {
          if (Arg->getArgumentConvention() ==
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
      domOrder.pushChildrenIf(block, [=](SILBasicBlock *child) {
        return child->getSinglePredecessorBlock() != block ||
               child == takenBlock;
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

/// Checks if a given generic apply should be inlined unconditionally, i.e.
/// without any complex analysis using e.g. a cost model.
/// It returns true if a function should be inlined.
/// It returns false if a function should not be inlined.
/// It returns None if the decision cannot be made without a more complex
/// analysis.
static Optional<bool> shouldInlineGeneric(FullApplySite AI) {
  assert(!AI.getSubstitutions().empty() &&
         "Expected a generic apply");

  if (!EnableSILInliningOfGenerics)
    return false;

  // If all substitutions are concrete, then there is no need to perform the
  // generic inlining. Let the generic specializer create a specialized
  // function and then decide if it is beneficial to inline it.
  if (!hasArchetypes(AI.getSubstitutions()))
    return false;

  SILFunction *Callee = AI.getReferencedFunction();

  // Do not inline @_semantics functions when compiling the stdlib,
  // because they need to be preserved, so that the optimizer
  // can properly optimize a user code later.
  auto ModuleName = Callee->getModule().getSwiftModule()->getName().str();
  if (Callee->hasSemanticsAttrThatStartsWith("array.") &&
      (ModuleName == STDLIB_NAME || ModuleName == SWIFT_ONONE_SUPPORT))
    return false;

  // Always inline generic functions which are marked as
  // AlwaysInline or transparent.
  if (Callee->getInlineStrategy() == AlwaysInline || Callee->isTransparent())
    return true;

  // It is not clear yet if this function should be decided or not.
  return None;
}

bool SILPerformanceInliner::
decideInWarmBlock(FullApplySite AI,
                  Weight CallerWeight,
                  ConstantTracker &callerTracker,
                  int &NumCallerBlocks) {
  if (!AI.getSubstitutions().empty()) {
    // Only inline generics if definitively clear that it should be done.
    auto ShouldInlineGeneric = shouldInlineGeneric(AI);
    if (ShouldInlineGeneric.hasValue())
      return ShouldInlineGeneric.getValue();
  }

  SILFunction *Callee = AI.getReferencedFunction();

  if (Callee->getInlineStrategy() == AlwaysInline)
    return true;

  return isProfitableToInline(AI, CallerWeight, callerTracker, NumCallerBlocks);
}

/// Return true if inlining this call site into a cold block is profitable.
bool SILPerformanceInliner::decideInColdBlock(FullApplySite AI,
                                              SILFunction *Callee) {
  if (!AI.getSubstitutions().empty()) {
    // Only inline generics if definitively clear that it should be done.
    auto ShouldInlineGeneric = shouldInlineGeneric(AI);
    if (ShouldInlineGeneric.hasValue())
      return ShouldInlineGeneric.getValue();

    return false;
  }

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

        if (decideInWarmBlock(AI, W, constTracker, NumCallerBlocks))
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

    SILInliner Inliner(*Caller, *Callee,
                       SILInliner::InlineKind::PerformanceInline,
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
      if (Callee && decideInColdBlock(AI, Callee)) {
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
