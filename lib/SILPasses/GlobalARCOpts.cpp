//===-------- GlobalARCOpts.cpp - Global SIL ARC Optimizations ------------===//
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

#define DEBUG_TYPE "sil-global-arc-opts"
#include "swift/SILPasses/Passes.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace swift::arc;

STATISTIC(NumIncrements, "Total number of increments seen");
STATISTIC(NumRefCountOpsMoved, "Total number of increments moved");
STATISTIC(NumRefCountOpsRemoved, "Total number of increments removed");

//===----------------------------------------------------------------------===//
//                             Utility Functions
//===----------------------------------------------------------------------===//

/// Returns true if Inc and Dec are compatible reference count instructions.
///
/// In more specific terms this means that means a (strong_retain,
/// strong_release) pair or a (retain_value, release_value) pair.
static bool matchingRefCountPairType(SILInstruction *Inc, SILInstruction *Dec) {
  return (isa<StrongRetainInst>(Inc) && isa<StrongReleaseInst>(Dec)) ||
    (isa<RetainValueInst>(Inc) && isa<ReleaseValueInst>(Dec));
}

/// Is I an instruction that we recognize as a "reference count increment"
/// instruction?
static bool isRefCountIncrement(SILInstruction &I) {
  return isa<StrongRetainInst>(I) || isa<RetainValueInst>(I);
}

/// Is I an instruction that we recognize as a "reference count decrement"
/// instruction?
static bool isRefCountDecrement(SILInstruction &I) {
  return isa<StrongReleaseInst>(I) || isa<ReleaseValueInst>(I);
}

//===----------------------------------------------------------------------===//
//                           Reference Count State
//===----------------------------------------------------------------------===//

namespace {

/// Sequence of states that a value with reference semantics can go through when
/// visiting increments top down.
enum class SequenceState {
  None,                 ///< The pointer has no information associated with it.
  Incremented,          ///< The pointer is known to have been incremented.
  MightBeDecremented,   ///< The pointer has been incremented and may have been
                        ///  decremented.
  MightBeUsed           ///< The pointer has been incremented, may have been
                        ///  decremented, and may have a use that needs to be
                        ///  protected against the decrement deallocating the
                        ///  pointer.
};

class ReferenceCountState {
  /// The increment that we are tracking.
  llvm::PointerUnion<SILInstruction *, SILArgument *> Increment;

  /// Was the pointer we are tracking known incremented when we visited the
  /// current increment we are tracking? In that case we know that it is safe
  /// to move the inner retain over instructions that may decrement ref counts
  /// since the outer retain will keep the reference counted value alive.
  bool KnownSafe = false;

  /// The latest point we can move Instruction without moving it over an
  /// instruction that might be able to decrement the value with reference
  /// semantics.
  SILInstruction *InsertPt = nullptr;

  /// Current place in the sequence of the value.
  SequenceState SeqState = SequenceState::None;

public:
  ReferenceCountState() { }
  ~ReferenceCountState() { }

  /// Can we gaurantee that the given reference counted value is incremented?
  bool isIncremented() const { return SequenceState::Incremented == SeqState; }

  /// Are we tracking an instruction currently? This returns false when given an
  /// uninitialized ReferenceCountState or one tracking a SILArgument.
  bool isTrackingInstruction() const {
    return isTrackingIncrement() && Increment.is<SILInstruction *>();
  }

  /// Are we tracking an increment currently? This returns false when given an
  /// uninitialized ReferenceCountState.
  bool isTrackingIncrement() const { return !Increment.isNull(); }

  /// Return the increment we are tracking.
  SILInstruction *getInstruction() const {
    return Increment.get<SILInstruction *>();
  }

  /// Return the value with reference semantics that is the operand of our
  /// increment.
  SILValue getValue() const {
    return getInstruction()->getOperand(0).stripCasts();
  }

  /// The latest point we can move the increment without bypassing instructions
  /// that may have reference semantics.
  SILInstruction *getInsertPoint() const { return InsertPt; }

  ///
  bool isKnownSafe() const { return KnownSafe; }

  /// Initializes/reinitialized the state for I. If we reinitialize we return
  /// true.
  bool initWithInst(SILInstruction *I);

  /// Initializes the state for a function parameter with the given
  /// SILParameterInfo.
  void initWithArg(SILArgument *A);

  /// Uninitialize the current state.
  void clear();

  /// Check if PotentialDecrement can decrement the reference count associated
  /// with the value we are tracking. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool handlePotentialDecrement(SILInstruction *PotentialDecrement,
                                AliasAnalysis *AA);

  /// Check if PotentialUser could be a use of the reference counted value that
  /// requires user to be alive. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool handlePotentialUser(SILInstruction *PotentialUser,
                           AliasAnalysis *AA);

  /// Returns true if Decrement is a decrement instruction that matches the
  /// increment we are tracking. Performs all necessary state cleanups.
  bool doesDecrementMatchInstruction(SILInstruction *Decrement);
};

} // end anonymous namespace

bool ReferenceCountState::initWithInst(SILInstruction *I) {
  bool Nested = isTrackingIncrement();

  // This retain is known safe if the operand we are tracking was already
  // known incremented previously. This occurs when you have nested increments.
  KnownSafe = isIncremented();

  // Reset our state to Incremented.
  SeqState = SequenceState::Incremented;

  // Stash the incrementing instruction we are tracking.
  Increment = I;

  // Reset our insertion point to null.
  InsertPt = nullptr;

  return Nested;
}

void ReferenceCountState::initWithArg(SILArgument *Arg) {
  SeqState = SequenceState::Incremented;
  Increment = Arg;
  InsertPt = nullptr;
  KnownSafe = false;
}

void ReferenceCountState::clear() {
  SeqState = SequenceState::None;
  Increment = static_cast<SILInstruction *>(nullptr);
  InsertPt = nullptr;
  KnownSafe = false;
}

bool ReferenceCountState::handlePotentialDecrement(SILInstruction *Other,
                                                   AliasAnalysis *AA) {
  // If our state is not initialized, return false since we are not tracking
  // anything.
  if (!isTrackingInstruction())
    return false;

  // If we can prove that Other can not decrement the reference counted
  // instruction we are tracking, return false.
  if (!canDecrementRefCount(Other, getValue(), AA))
    return false;

  // Otherwise Other could potentially decrement the value we are tracking.
  // Attempt to advance the sequence. If we do advance the sequence, return
  // true. If we can not advance the sequence then this information is not
  // relevant, so return false.
  switch (SeqState) {
    case SequenceState::Incremented:
      SeqState = SequenceState::MightBeDecremented;
      InsertPt = Other;
      return true;
    case SequenceState::None:
    case SequenceState::MightBeDecremented:
    case SequenceState::MightBeUsed:
      return false;
  }
}

bool ReferenceCountState::handlePotentialUser(SILInstruction *Other,
                                              AliasAnalysis *AA) {
  // If our state is not initialized, return false since we are not tracking
  // anything.
  if (!isTrackingInstruction())
    return false;

  // If we can prove that Other can not use the pointer we are tracking,
  // return...
  if (!canUseValue(Other, getValue(), AA))
    return false;

  // Otherwise advance the sequence...
  switch (SeqState) {
    case SequenceState::MightBeDecremented:
      SeqState = SequenceState::MightBeUsed;
      return true;
    case SequenceState::Incremented:
    case SequenceState::None:
    case SequenceState::MightBeUsed:
      return false;
  }
}

bool
ReferenceCountState::doesDecrementMatchInstruction(SILInstruction *Decrement) {
  // If our state is not initialized, return false since we are not tracking
  // anything. If the ValueKind of our increments, decrement do not match, we
  // can not eliminate the pair so return false.
  if (!isTrackingInstruction() ||
      !matchingRefCountPairType(getInstruction(), Decrement))
    return false;

  // Otherwise modify the state appropriately in preparation for removing the
  // increment, decrement pair.
  switch (SeqState) {
    case SequenceState::None:
      return false;
    case SequenceState::Incremented:
    case SequenceState::MightBeDecremented:
      // Unset InsertPt so we remove retain release pairs instead of performing
      // code motion.
      InsertPt = nullptr;
      SWIFT_FALLTHROUGH;
    case SequenceState::MightBeUsed:
      return true;
  }
}

//===----------------------------------------------------------------------===//
//                             Top Down Dataflow
//===----------------------------------------------------------------------===//

#ifndef NDEBUG
/// Small helper function for reducing the debug output generated by ignoring
/// functions without interesting instructions.
static bool isInterestingInstruction(ValueKind Kind) {
  switch (Kind) {
   default:
     return false;
   case ValueKind::RetainValueInst:
   case ValueKind::ReleaseValueInst:
   case ValueKind::StrongRetainInst:
   case ValueKind::StrongReleaseInst:
     return true;
  }
}
#endif

static bool isAutoreleasePoolCall(SILInstruction &I) {
  ApplyInst *AI = dyn_cast<ApplyInst>(&I);
  if (!AI)
    return false;

  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FRI)
    return false;

  return llvm::StringSwitch<bool>(FRI->getReferencedFunction()->getName())
    .Case("objc_autoreleasePoolPush", true)
    .Case("objc_autoreleasePoolPop", true)
    .Default(false);
}

/// Analyze a single BB for refcount inc/dec instructions.
///
/// If anything was found it will be added to DecToIncStateMap.
///
/// NestingDetected will be set to indicate that the block needs to be
/// reanalyzed if code motion occurs.
static void
processBBTopDown(SILBasicBlock &BB,
                 llvm::MapVector<SILValue, ReferenceCountState> &BBState,
                 llvm::MapVector<SILInstruction *,
                                ReferenceCountState> &DecToIncStateMap,
                 bool &NestingDetected,
                 AliasAnalysis *AA) {

  NestingDetected = false;

#ifndef NDEBUG
  // If we are in debug mode, to make the output less verbose, make sure we are
  // actually processing something interesting before emitting any log output.
  bool FoundOneInst = false;
  for (auto &I : BB)
    FoundOneInst |= isInterestingInstruction(I.getKind());
  if (!FoundOneInst)
    return;
#endif

  // If the current BB is the entry BB, initialize a state corresponding to each
  // of its owned parameters.
  //
  // TODO: Handle gauranteed parameters.
  if (&BB == &*BB.getParent()->begin()) {
    auto Args = BB.getBBArgs();
    auto SignatureParams =
      BB.getParent()->getLoweredFunctionType()->getInterfaceParameters();
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
      SILArgument *A = Args[i];
      ParameterConvention P = SignatureParams[i].getConvention();

      DEBUG(llvm::dbgs() << "VISITING ARGUMENT: " << *A);

      if (P != ParameterConvention::Direct_Owned)
        continue;
      BBState[SILValue(Args[i])].initWithArg(A);
    }
  }

  // For each instruction I in BB...
  for (auto &I : BB) {

    DEBUG(llvm::dbgs() << "VISITING:\n    " << I);

    // If we have an autorelease pool call, clear all tracked state.
    if (isAutoreleasePoolCall(I))
      BBState.clear();

    // If I is a ref count increment instruction...
    if (isRefCountIncrement(I)) {
      // map its operand to a newly initialized or reinitialized ref count
      // state and continue...
      SILValue Operand = I.getOperand(0).stripCasts();
      ReferenceCountState &RefCountState = BBState[Operand];
      NestingDetected |= RefCountState.initWithInst(&I);

      DEBUG(llvm::dbgs() << "    REF COUNT INCREMENT! Known Safe: "
            << (RefCountState.isKnownSafe()?"yes":"no") << "\n");

      ++NumIncrements;
      continue;
    }

    // If we have a reference count decrement...
    if (isRefCountDecrement(I)) {
      // Look up the state associated with its operand...
      SILValue Operand = I.getOperand(0).stripCasts();
      ReferenceCountState &RefCountState = BBState[Operand];

      DEBUG(llvm::dbgs() << "    REF COUNT DECREMENT!\n");

      // If the state is already initialized to contain a reference count
      // increment of the same type (i.e. retain_value, release_value or
      // strong_retain, strong_release), then remove the state from the map
      // and add the retain/release pair to the delete list and continue.
      if (RefCountState.doesDecrementMatchInstruction(&I)) {
        // Copy the current value of ref count state into the result map.
        DecToIncStateMap[&I] = RefCountState;
        DEBUG(llvm::dbgs() << "    MATCHING INCREMENT:           "
              << *RefCountState.getInstruction());

        // Clear the ref count state in case we see more operations on this
        // ref counted value. This is for safety reasons.
        //
        // FIXME: It may be safe to continue here, but for now lets be
        // conservative.
        RefCountState.clear();
      }

      // Otherwise we continue processing the reference count decrement to
      // see if the decrement can affect any other pointers that we are
      // tracking.
    }

    // For all other (reference counted value, ref count state) we are
    // tracking...
    for (auto &OtherState : BBState) {
      // If we are tracking an argument, skip it.
      if (!OtherState.second.isTrackingInstruction())
        continue;

      // Check if the instruction we are visiting could potentially decrement
      // the reference counted value we are tracking... in a manner that could
      // cause us to change states. If we do change states continue...
      if (OtherState.second.handlePotentialDecrement(&I, AA)) {
        DEBUG(llvm::dbgs() << "    Found Potential Decrement:\n        "
              << *OtherState.second.getInstruction());
        continue;
      }

      // Otherwise check if the reference counted value we are tracking
      // could be used by the given instruction.
      SILInstruction *Other = OtherState.second.getInstruction();
      (void)Other;
      if (OtherState.second.handlePotentialUser(&I, AA))
        DEBUG(llvm::dbgs() << "    Found Potential Use:\n        " << *Other);
    }
  }
}

//===----------------------------------------------------------------------===//
//                                Code Motion
//===----------------------------------------------------------------------===//

/// A set of matching reference count increments, decrements, increment
/// insertion pts, and decrement insertion pts.
namespace {
struct ARCMatchingSet {
  SILValue Ptr;
  llvm::SmallPtrSet<SILInstruction *, 8> Increments;
  llvm::SmallPtrSet<SILInstruction *, 8> IncrementInsertPts;
  llvm::SmallPtrSet<SILInstruction *, 8> Decrements;
  llvm::SmallPtrSet<SILInstruction *, 8> DecrementInsertionPts;

  void clear() {
    Ptr = SILValue();
    Increments.clear();
    IncrementInsertPts.clear();
    Decrements.clear();
    DecrementInsertionPts.clear();
  }
};
} // end anonymous namespace

static SILInstruction *createIncrement(SILValue Ptr, SILInstruction *InsertPt) {
  SILBuilder B(InsertPt);
  if (Ptr.getType().hasReferenceSemantics())
    return B.createStrongRetain(InsertPt->getLoc(), Ptr);
  return B.createRetainValue(InsertPt->getLoc(), Ptr);
}

static SILInstruction *createDecrement(SILValue Ptr, SILInstruction *InsertPt) {
  SILBuilder B(InsertPt);

  if (Ptr.getType().hasReferenceSemantics())
    return B.createStrongRelease(InsertPt->getLoc(), Ptr);
  return B.createReleaseValue(InsertPt->getLoc(), Ptr);
}

static bool
optimizeReferenceCountMatchingSet(ARCMatchingSet &MatchSet) {
  DEBUG(llvm::dbgs() << "**** Optimizing Matching Set ****\n");

  bool Changed = false;

  // Insert the new increments.
  for (SILInstruction *InsertPt : MatchSet.IncrementInsertPts) {
    if (!InsertPt) {
      DEBUG(llvm::dbgs() << "    No insertion point, not inserting increment "
            "into new position.\n");
      continue;
    }

    Changed = true;
    SILInstruction *NewIncrement = createIncrement(MatchSet.Ptr, InsertPt);
    (void)NewIncrement;
    DEBUG(llvm::dbgs() << "    Inserting new increment: " << *NewIncrement
                       << "\n"
                          "At insertion point: " << *InsertPt << "\n");
    ++NumRefCountOpsMoved;
  }

  // Insert the new decrements.
  for (SILInstruction *InsertPt : MatchSet.DecrementInsertionPts) {
    if (!InsertPt) {
      DEBUG(llvm::dbgs() << "    No insertion point, not inserting decrement "
            "into its new position.\n");
      continue;
    }

    Changed = true;
    SILInstruction *NewDecrement = createDecrement(MatchSet.Ptr, InsertPt);
    (void)NewDecrement;
    DEBUG(llvm::dbgs() << "    Inserting new NewDecrement: " << *NewDecrement
                       << "\nAt insertion point: " << *InsertPt << "\n");
    ++NumRefCountOpsMoved;
  }

  // Add the old increments to the delete list.
  for (SILInstruction *Increment : MatchSet.Increments) {
    Changed = true;
    DEBUG(llvm::dbgs() << "    Deleting increment: " << *Increment);
    Increment->eraseFromParent();
    ++NumRefCountOpsRemoved;
  }

  // Add the old decrements to the delete list.
  for (SILInstruction *Decrement : MatchSet.Decrements) {
    Changed = true;
    DEBUG(llvm::dbgs() << "    Deleting decrement: " << *Decrement);
    Decrement->eraseFromParent();
    ++NumRefCountOpsRemoved;
  }

  return Changed;
}

static bool
computeARCMatchingSet(llvm::MapVector<SILInstruction *,
                                      ReferenceCountState> &DecToIncStateMap,
                      ARCMatchingSet &MatchSet) {
  bool MatchedPair = false;

  for (auto &Pair : DecToIncStateMap) {
    // If we do not have an instruction, this is a state with an invalidated
    // reference count. Skip it...
    if (!Pair.second.getInstruction())
      continue;

    if (!MatchSet.Ptr)
      MatchSet.Ptr = Pair.first->getOperand(0);
    assert(MatchSet.Ptr == Pair.first->getOperand(0) &&
           "If Ptr is already set, make sure it matches the ptr on the "
           "increment.");

    auto *InsertPt = Pair.second.getInsertPoint();

    // If we reached this point and do not have an insertion point (in the case
    // where we do not complete the sequence) or are known safe, remove the
    // retain release pair.
    if (Pair.second.isKnownSafe() || !InsertPt) {
      SILInstruction *Inst = Pair.second.getInstruction();

      DEBUG(llvm::dbgs() << "Removing Pair:\n    KnownSafe: "
            << (Pair.second.isKnownSafe()?"yes":"no")
            << "\n    " << *Inst << "    "
            << *Pair.first);

      MatchedPair = true;
      MatchSet.Increments.insert(Inst);
      MatchSet.Decrements.insert(Pair.first);
      continue;
    }

    if (InsertPt) {
      MatchSet.Increments.insert(Pair.second.getInstruction());
      MatchSet.IncrementInsertPts.insert(InsertPt);
    }
  }

  return MatchedPair;
}


//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static bool processFunction(SILFunction &F, AliasAnalysis *AA) {
  // GlobalARCOpts seems to be taking up a lot of compile time when running on
  // globalinit_func. Since that is not *that* interesting from an ARC
  // perspective (i.e. no ref count operations in a loop), disable it on such
  // functions temporarily in order to unblock others. This should be removed.
  if (F.getName().startswith("globalinit_func"))
    return false;

  DEBUG(llvm::dbgs() << "***** Processing " << F.getName() << " *****\n");

  llvm::MapVector<SILValue, ReferenceCountState> BBState;
  llvm::MapVector<SILInstruction *, ReferenceCountState> DecToIncStateMap;
  ARCMatchingSet MatchingSet;
  bool Changed = false;

  // For each basic block in F...
  for (auto &BB : F) {
    DEBUG(llvm::dbgs() << "\n<<< Processing New BB! >>>\n");

    // Process the basic block until we do not eliminate any inc/dec pairs
    // and see any nested increments.
    while (true) {
      BBState.clear();
      DecToIncStateMap.clear();
      MatchingSet.clear();

      bool NestingDetected;
      // Perform the top down dataflow.
      processBBTopDown(BB, BBState, DecToIncStateMap, NestingDetected, AA);

      // Use the result of the top down dataflow to initialize a set of matching
      // increments, decrements, increment insertion pts, and decrement
      // insertion pts.
      //
      // *NOTE* Since we are single basic block only and are not moving
      // releases, currently we will have match sets consisting of an increment,
      // decrement pair or an increment, increment insertion pt.
      bool RemovedPair = computeARCMatchingSet(DecToIncStateMap, MatchingSet);

      // Optimize the computed. ReferenceCountMatchingSet
      Changed |= optimizeReferenceCountMatchingSet(MatchingSet);

      // We need to rerun if we saw any nested increment/decrements and if we
      // removed any increment/decrement pairs.
      if (!NestingDetected || !RemovedPair)
        break;

      DEBUG(llvm::dbgs() << "\n<<< Made a Change! Reprocessing BB! >>>\n");
    }

    DEBUG(llvm::dbgs() << "\n");
  }
  return Changed;
}

namespace {
class GlobalARCOpts : public SILFunctionTransform {
  /// The entry point to the transformation.
  void run() {
    auto *AA = getAnalysis<AliasAnalysis>();
    if (processFunction(*getFunction(), AA))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "Global ARC Optimization"; }
};
} // end anonymous namespace


SILTransform *swift::createGlobalARCOpts() {
  return new GlobalARCOpts();
}
