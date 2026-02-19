//===--- ColdBlockInfo.cpp - Hot/cold block analysis for the SIL CFG ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/SemanticAttrs.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Analysis/ColdBlockInfo.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Support/CommandLine.h"

#define DEBUG_TYPE "cold-block-info"

using namespace swift;

/// Strategy for handling blocks with zero execution counts in profile data
enum class ZeroCountStrategy {
  Conservative,  // Skip inference, let other heuristics decide (safe default)
  Optimistic     // Assume zero-count blocks are cold
};

static llvm::cl::opt<ZeroCountStrategy> ZeroCountHandling(
    "sil-zero-count-strategy", llvm::cl::init(ZeroCountStrategy::Conservative),
    llvm::cl::desc("Strategy for handling blocks with zero execution counts"),
    llvm::cl::values(
        clEnumValN(ZeroCountStrategy::Conservative, "conservative",
                   "Skip inference for zero-count blocks (default)"),
        clEnumValN(ZeroCountStrategy::Optimistic, "optimistic",
                   "Assume zero-count blocks are cold")));

bool isColdEnergy(ColdBlockInfo::Energy e);

ColdBlockInfo::ColdBlockInfo(DominanceAnalysis *DA,
                             PostDominanceAnalysis *PDA) : DA(DA), PDA(PDA) {
  LLVM_DEBUG(llvm::dbgs() << "ColdBlockInfo: constructed\n");
}

static std::string toString(SILBasicBlock const *bb) {
  std::string str = bb->getParent()->getName().str();
  str += "::bb" + std::to_string(bb->getDebugID());
  return str;
}

static StringRef toString(ColdBlockInfo::Energy e) {
  if (e == ColdBlockInfo::Energy::full())
    return "<<ALL>>";
  else if (e.empty())
    return "<<NONE>>";
  else if (e.contains(ColdBlockInfo::State::Warm))
    return "warm";
  else if (e.contains(ColdBlockInfo::State::Cold))
    return "cold";
  else
    llvm_unreachable("unhandled energy state");
}

static StringRef toString(ColdBlockInfo::State::Temperature t) {
  ColdBlockInfo::Energy e;
  e.insert(t);
  return toString(e);
}

void ColdBlockInfo::dump() const {
  unsigned warm = 0, cold = 0;
  llvm::dbgs() << "ColdBlockInfo {\n";
  for (auto pair : EnergyMap) {
    auto energy = pair.getSecond();
    isColdEnergy(energy) ? cold++ : warm++;

    llvm::dbgs() << toString(pair.getFirst())
                 << " -> " << toString(energy) << "\n";
  }
  llvm::dbgs() << "STATISTICS: warm " << warm << " | cold " << cold << "\n}\n";
}

inline bool isCriticalEdge(SILBasicBlock *predBB, SILBasicBlock *succBB) {
  return !(predBB->getSingleSuccessorBlock() == succBB
        || succBB->getSinglePredecessorBlock() == predBB);
}
static bool hasCriticalEdge(SILBasicBlock *BB) {
  return llvm::any_of(BB->getSuccessorBlocks(), [&](auto *succBB) {
    if (!isCriticalEdge(BB, succBB))
      return false;

    LLVM_DEBUG(llvm::dbgs() << "ColdBlockInfo: "
                            << toString(BB) << " -> " << toString(succBB)
                            << " is a critical edge!\n");
    return true;
  });
}

/// A cold terminator is one where it's unlikely to be reached, which are
/// function exits that are less-common. A cold terminator implies a cold block.
///   - 'unreachable', as it's never executed.
///   - 'throw', if throws prediction is enabled.
static bool isColdTerminator(const TermInst *term) {
  switch (term->getTermKind()) {
  case TermKind::AwaitAsyncContinuationInst:
  case TermKind::BranchInst:
  case TermKind::CondBranchInst:
  case TermKind::SwitchValueInst:
  case TermKind::SwitchEnumInst:
  case TermKind::SwitchEnumAddrInst:
  case TermKind::DynamicMethodBranchInst:
  case TermKind::CheckedCastBranchInst:
  case TermKind::CheckedCastAddrBranchInst:
  case TermKind::TryApplyInst:
  case TermKind::YieldInst:
  case TermKind::ReturnInst:
  case TermKind::ReturnBorrowInst:
    return false;
  case TermKind::ThrowInst:
  case TermKind::ThrowAddrInst:
  case TermKind::UnwindInst:
    return term->getModule().getOptions().EnableThrowsPrediction;
  case TermKind::UnreachableInst:
    // For now, assume it's always cold, since it's executed once.
    // Not all functions in the stdlib are properly annotated as a
    // "known program termination point", so we don't use
    // ApplySite::isCalleeKnownProgramTerminationPoint.
    return term->getModule().getOptions().EnableNoReturnCold;
  }
}

/// Peek through an extract of Bool.value.
static SILValue getCondition(SILValue C) {
  if (auto *SEI = dyn_cast<StructExtractInst>(C)) {
    if (auto *Struct = dyn_cast<StructInst>(SEI->getOperand()))
      return Struct->getFieldValue(SEI->getField());
    return SEI->getOperand();
  }
  return C;
}

constexpr unsigned RecursionDepthLimit = 3;
std::optional<bool>
ColdBlockInfo::searchForExpectedValue(SILValue Cond,
                                      unsigned recursionDepth) {
  if (recursionDepth > RecursionDepthLimit)
    return std::nullopt;

  if (auto *Arg = dyn_cast<SILArgument>(Cond)) {
    llvm::SmallVector<std::pair<SILBasicBlock *, SILValue>, 4> InValues;
    if (!Arg->getIncomingPhiValues(InValues))
      return std::nullopt;

    std::optional<bool> expectedValue;

    // Check all predecessor values which come from non-cold blocks.
    for (auto Pair : InValues) {
      auto *predBB = Pair.first;
      auto predArg = Pair.second;

      // We only want to consider values coming from a non-cold path of preds.
      if (isCold(predBB))
        continue;

      std::optional<bool> predecessorValue;

      // Look for an integer literal, otherwise, recurse.
      if (auto *IL = dyn_cast<IntegerLiteralInst>(predArg)) {
        predecessorValue = IL->getValue().getBoolValue();
      } else {
        predecessorValue = searchForExpectedValue(predArg, recursionDepth+1);
      }

      // There's at least one non-cold predecessor with an unknown value.
      if (!predecessorValue)
        return std::nullopt;

      // If this is the first non-cold predecessor, save the value.
      if (!expectedValue) {
        expectedValue = *predecessorValue;
        continue;
      }

      // Check if we have a consistent value across all non-cold predecessors.
      if (*expectedValue != *predecessorValue)
        return std::nullopt;
    }

    return expectedValue;
  }
  return std::nullopt;
}

static std::optional<bool> getExpectedValue(SILValue Cond) {
  // Handle the fully inlined Builtin.
  if (auto *BI = dyn_cast<BuiltinInst>(Cond)) {
    if (BI->getIntrinsicInfo().ID == llvm::Intrinsic::expect) {
      // peek through an extract of Bool.value.
      SILValue ExpectedValue = getCondition(BI->getArguments()[1]);
      if (auto *Literal = dyn_cast<IntegerLiteralInst>(ExpectedValue)) {
        return (Literal->getValue() == 0) ? false : true;
      }
    }
    return std::nullopt;
  }

  // Handle the @semantic functions used for branch hints.
  auto AI = dyn_cast<ApplyInst>(Cond);
  if (!AI)
    return std::nullopt;

  if (auto *F = AI->getReferencedFunctionOrNull()) {
    if (F->hasSemanticsAttrs()) {
      // fastpath/slowpath attrs are untested because the inliner luckily
      // inlines them before the downstream calls.
      if (F->hasSemanticsAttr(semantics::SLOWPATH))
        return false;
      else if (F->hasSemanticsAttr(semantics::FASTPATH))
        return true;
    }
  }
  return std::nullopt;
}

/// The minimum probability that an edge is taken to be considered "warm".
constexpr double WARM_EDGE_MINIMUM = 3.0 / 100.0;

/// Using the profile data on the terminator of this block, annotate successors
/// with cold/warm information.
///
/// \returns true if an inference was made
bool ColdBlockInfo::inferFromEdgeProfile(SILBasicBlock *BB) {
  ProfileCounter totalCount{0};
  SmallVector<ProfileCounter, 2> succCount;

  // Current analysis only accurately handles blocks with 2 successors,
  // since we only have two temperatures (cold/warm).
  if (BB->getNumSuccessors() != 2)
    return false;

  // First pass: collect all counters and check for evidence of profiling.
  // ProfileCounter has native missing data support via hasValue().
  bool hasAnyNonZeroCount = false;
  bool hasAnyMissingData = false;
  SmallVector<ProfileCounter, 2> counters;

  for (auto const &succ : BB->getSuccessors()) {
    auto counter = succ.getCount();
    counters.push_back(counter);

    if (!counter.hasValue()) {
      hasAnyMissingData = true;
    } else if (counter.getValue() > 0) {
      hasAnyNonZeroCount = true;
    }
  }

  // Handle missing profile data based on the configured strategy
  if (hasAnyMissingData) {
    if (ZeroCountHandling == ZeroCountStrategy::Optimistic) {
      // Optimistic strategy: treat missing data as zero only when we have
      // evidence of profiling (at least one non-zero count).
      // This enables optimizations when profile data is partial.
      if (!hasAnyNonZeroCount) {
        // No evidence of profiling - bail out conservatively
        LLVM_DEBUG(llvm::dbgs() << "ColdBlockInfo: no evidence of profiling for "
                                << toString(BB) << " (optimistic mode, but no non-zero counts)\n");
        return false;
      }
      // Continue to build count vector, treating missing as zero
      LLVM_DEBUG(llvm::dbgs() << "ColdBlockInfo: applying optimistic strategy for "
                              << toString(BB) << " (found non-zero counts)\n");
    } else {
      // Conservative strategy: bail out when any missing data exists.
      // This prevents marking unprofiled code as cold.
      LLVM_DEBUG(llvm::dbgs() << "ColdBlockInfo: missing profile data for "
                              << toString(BB) << " (conservative mode - skipping)\n");
      return false;
    }
  }

  // Second pass: build the count vector
  for (size_t i = 0; i < counters.size(); i++) {
    ProfileCounter count;
    if (counters[i].hasValue()) {
      count = counters[i];
    } else {
      // Only reached in Optimistic mode with evidence of profiling
      count = ProfileCounter(0);
      LLVM_DEBUG(llvm::dbgs()
        << "ColdBlockInfo: treating missing profile data as zero for "
        << toString(BB->getSuccessors()[i])
        << " (optimistic strategy)\n");
    }

    succCount.push_back(count);
    auto didSaturate = totalCount.add_saturating(count);
    ASSERT(!didSaturate && "should rescale the profile data first");
    (void)didSaturate;
  }

  TermInst::ConstSuccessorListTy succs = BB->getSuccessors();
  ASSERT(succCount.size() == succs.size());

  // Handle the case where all successors have zero execution counts
  // This can happen when: 1) the block was instrumented but never executed,
  // or 2) our optimistic strategy treated missing data as zero
  if (totalCount.getValue() < 1) {
    LLVM_DEBUG(llvm::dbgs()
               << "ColdBlockInfo: handling zero execution count for " << toString(BB)
               << " using strategy: ");

    switch (ZeroCountHandling) {
    case ZeroCountStrategy::Conservative:
      // Conservative approach: we can't infer probabilities without execution data
      // Skip this block and let other heuristics handle it
      LLVM_DEBUG(llvm::dbgs() << "conservative (skipping inference)\n");
      return false;

    case ZeroCountStrategy::Optimistic:
      // Optimistic approach: assume zero-count blocks are cold
      // Mark all successors as cold since this code path is never executed
      LLVM_DEBUG(llvm::dbgs() << "optimistic (marking all successors as cold)\n");
      for (auto const &succ : succs) {
        set(succ, ColdBlockInfo::State::Cold);
      }
      return true;
    }

    llvm_unreachable("Unknown zero count strategy");
  }

  // Record temperatures.
  for (size_t i = 0; i < succs.size(); i++) {
    double takenProbability =
        succCount[i].getValue() / (double)totalCount.getValue();

    // It's a cold edge if the profiling-based probability is below the threshold.
    auto state =
        takenProbability < WARM_EDGE_MINIMUM ? ColdBlockInfo::State::Cold
                                             : ColdBlockInfo::State::Warm;

    set(succs[i], state);

    LLVM_DEBUG(llvm::dbgs()
                 << "ColdBlockInfo: setting to " << toString(state)
                 << " (inferFromEdgeProfile): " << toString(succs[i])
                 << " has taken probability " << takenProbability << "\n");
    ASSERT(takenProbability >= 0);
    ASSERT(takenProbability <= 1);
  }

  return true;
}

void ColdBlockInfo::analyze(SILFunction *fn) {
  SWIFT_DEFER { changedMap = false; };

  LLVM_DEBUG(llvm::dbgs()
    << "ColdBlockInfo::analyze on " << fn->getName() << "\n");
  LLVM_DEBUG(llvm::dbgs() << "--> Before Stage 1\n");
  LLVM_DEBUG(dump());

  // The set of blocks for which we can skip searching for an expected
  // conditional value, as we've already determined which successor is cold.
  BasicBlockSet foundExpectedCond(fn);

  // Stage 1: Seed the graph with warm/cold blocks.
  changedMap = false;
  for (auto &BB : *fn) {
    auto *term = BB.getTerminator();

    // Check for a cold exit.
    if (isColdTerminator(term)) {
      assert(term->getNumSuccessors() == 0);
      LLVM_DEBUG(llvm::dbgs()
        << "ColdBlockInfo: resetting to cold (isColdTerminator): "
        << toString(&BB) << "\n");

      // Overwrite any existing temperatures.
      resetToCold(&BB);
      continue;
    }

    // Check profile data for successors, choosing it first over branch hints.
    if (inferFromEdgeProfile(&BB)) {
      foundExpectedCond.insert(&BB);
      continue;
    }

    // Check for an obvious _fastPath / _slowPath condition for successors.
    if (auto *CBI = dyn_cast<CondBranchInst>(term)) {
      if (auto val = getExpectedValue(getCondition(CBI->getCondition()))) {
        setExpectedCondition(CBI, val);
        foundExpectedCond.insert(&BB);
      }
    }
  }

  LLVM_DEBUG(llvm::dbgs() << "--> After Stage 1; changedMap = "
                          << changedMap << "\n");
  LLVM_DEBUG(dump());

  /// Latter stages are only for propagating coldness from other cold blocks.
  ///
  /// If we haven't changed the energy map at all in Stage 1, then we didn't
  /// find any new coldness, so stop early.
  if (!changedMap) {
    LLVM_DEBUG(llvm::dbgs()
      << "--> Stopping early in "<< fn->getName() << "\n");
    return;
  }

  // Stage 2: Propagate via dominators
  changedMap = false;
  SmallVector<SILBasicBlock *, 8> scratch;
  for (auto &BB : *fn) {
    scratch.clear();

    if (isCold(&BB)) {
      // Mark all blocks I dominate as cold.
      auto *domInfo = DA->get(fn);
      domInfo->getDescendants(&BB, scratch);
      for (auto *dominatedBB : scratch) {
        if (dominatedBB == &BB)
          continue;
        LLVM_DEBUG(llvm::dbgs() << "ColdBlockInfo: resetting to cold (dominatedBB): "
                                << toString(dominatedBB) << "\n");
        resetToCold(dominatedBB);
      }
    } else {
      // Mark myself cold if I'm post-dominated by a cold block.
      auto *pdInfo = PDA->get(fn);
      scratch.push_back(&BB);
      auto *node = pdInfo->getNode(&BB);
      bool foundCold = false;

      while (node && !foundCold) {
        node = node->getIDom();
        if (!node || pdInfo->isVirtualRoot(node))
          break;

        auto *postBB = node->getBlock();
        if (isCold(postBB)) {
          foundCold = true;
        } else {
          scratch.push_back(postBB);
        }
      }

      if (foundCold) {
        for (auto *chainBB : scratch) {
          LLVM_DEBUG(llvm::dbgs() << "ColdBlockInfo: resetting to cold (chainBB): "
                                  << toString(chainBB) << "\n");
          resetToCold(chainBB);
        }
      }
    }
  }

  LLVM_DEBUG(llvm::dbgs() << "--> After Stage 2; changedMap = "
                          << changedMap << "\n");
  LLVM_DEBUG(dump());

  /// Stage 3: Backwards propagate coldness from successors.
  changedMap = false;
  auto isColdBlock = [&](auto *bb) { return isCold(bb); };

  unsigned completedIters = 0;
  bool changed;
  do {
    changed = false;


    // We're bubbling up coldness from the leaves of the function up towards the
    // entry block, so walk the blocks in post-order to converge faster.
    for (auto *BB : llvm::post_order(fn)) {

      // Only on the first pass, search recursively for an expected value,
      // if needed, now that more temperature data has been determined.
      if (!completedIters && !foundExpectedCond.contains(BB)) {
        if (auto *CBI = dyn_cast<CondBranchInst>(BB->getTerminator())) {
          auto cond = getCondition(CBI->getCondition());
          if (auto val = searchForExpectedValue(cond)) {
            setExpectedCondition(CBI, val);
            changed = true;
          }
        }
      }

      // Nothing to propagate from.
      if (BB->getNumSuccessors() == 0)
        continue;

      // Coldness already exists here.
      if (isCold(BB))
        continue;

      if (llvm::all_of(BB->getSuccessorBlocks(), isColdBlock)) {
        resetToCold(BB);
        changed = true;
      }
    }
    completedIters++;
  } while (changed);

  LLVM_DEBUG(llvm::dbgs() << "--> Final for " << fn->getName() <<
                          " | converged after " << completedIters << " iters"
                          << " over " << fn->size() << " blocks; "
                          << " changedMap = " << changedMap << "\n");
  LLVM_DEBUG(dump());
}

inline bool isColdEnergy(ColdBlockInfo::Energy e) {
  return e.contains(ColdBlockInfo::State::Cold)
     && !e.contains(ColdBlockInfo::State::Warm);
}

bool ColdBlockInfo::isCold(const SILBasicBlock *BB) const {
  auto result = EnergyMap.find(BB);
  if (result == EnergyMap.end())
    return false;

  return isColdEnergy(result->getSecond());
}

void ColdBlockInfo::resetToCold(const SILBasicBlock *BB) {
  auto &entry = EnergyMap[BB];
  if (isColdEnergy(entry))
    return;

  entry.removeAll();
  entry.insert(State::Cold);
  changedMap = true;
}

void ColdBlockInfo::set(const SILBasicBlock *BB, State::Temperature temp) {
  auto &entry = EnergyMap[BB];
  if (entry.contains(temp))
    return;

  entry.insert(temp);
  changedMap = true;
}

void ColdBlockInfo::setExpectedCondition(CondBranchInst *CBI, ExpectedValue value) {
  if (!value)
    return;

  // This function marks both sides of the conditional-branch, assuming
  // critical edges are split. If they're NOT, then unexpected things happen.
  // For example, we'd mark bb2 below as cold, which post-dominates the warm
  // block bb1, and thus wipes out the warm annotation on bb1!
  //                bb0: [ _fastPath(trueSide) ]
  //                          │             │
  //                          │      bb1: [ warm ]
  //                          │             │
  //                          │─────────────┘
  //                          ▼
  //                  bb2: [ cold ]
  if (hasCriticalEdge(CBI->getParent()))
    return;

  if (*value) {
    set(CBI->getTrueBB(), State::Warm);
    set(CBI->getFalseBB(), State::Cold);
    LLVM_DEBUG(llvm::dbgs() << "ColdBlockInfo: "
                            << "_fastPath = " << toString(CBI->getTrueBB())
                            << " | _slowPath = " << toString(CBI->getFalseBB())
                            << "\n");
  } else {
    set(CBI->getTrueBB(), State::Cold);
    set(CBI->getFalseBB(), State::Warm);
    LLVM_DEBUG(llvm::dbgs() << "ColdBlockInfo: "
                            << "_fastPath = " << toString(CBI->getFalseBB())
                            << " | _slowPath = " << toString(CBI->getTrueBB())
                            << "\n");
  }
}
