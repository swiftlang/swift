//===-------- ARCOpts.cpp - Perform SIL ARC Optimizations -----------------===//
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

#define DEBUG_TYPE "sil-arc-opts"
#include "swift/SILPasses/Passes.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

static llvm::cl::opt<bool>
EnableARCOpts("enable-arc-opts", llvm::cl::Hidden, llvm::cl::init(true));

STATISTIC(NumIncrements, "Total number of increments seen");
STATISTIC(NumIncrementsRemoved, "Total number of increments removed");

//===----------------------------------------------------------------------===//
//                          Stub Analysis Functions
//===----------------------------------------------------------------------===//

/// Could Inst decrement the ref count associated with target?
static bool cannotDecrementRefCount(SILInstruction *Inst, SILValue Target) {
  // Just make sure that we do not have side effects.
  return Inst->getMemoryBehavior() !=
    SILInstruction::MemoryBehavior::MayHaveSideEffects;
}

/// Can Inst use Target in a manner that requires Target to be alive at Inst?
static bool cannotUseValue(SILInstruction *Inst, SILValue Target) {
  // Assume Inst always uses Target for now.
  return false;
}

//===----------------------------------------------------------------------===//
//                             Utility Functions
//===----------------------------------------------------------------------===//

/// Returns true if Inc and Dec are compatible reference count instructions.
///
/// In more specific terms this means that means a (strong_retain,
/// strong_release) pair or a (copy_value, destroy_value) pair.
static bool matchingRefCountPairType(SILInstruction *Inc, SILInstruction *Dec) {
  return (isa<StrongRetainInst>(Inc) && isa<StrongReleaseInst>(Dec)) ||
    (isa<CopyValueInst>(Inc) && isa<DestroyValueInst>(Dec));
}

/// Is I an instruction that we recognize as a "reference count increment"
/// instruction?
static bool isRefCountIncrement(SILInstruction &I) {
  return isa<StrongRetainInst>(I) || isa<CopyValueInst>(I);
}

/// Is I an instruction that we recognize as a "reference count decrement"
/// instruction?
static bool isRefCountDecrement(SILInstruction &I) {
  return isa<StrongReleaseInst>(I) || isa<DestroyValueInst>(I);
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
  /// The increment instruction that we are tracking.
  SILInstruction *Instruction = nullptr;

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
  SequenceState State = SequenceState::None;

public:
  ReferenceCountState() { }
  ~ReferenceCountState() { }

  /// Are we initialized for tracking a pointer?
  bool isInitialized() const { return Instruction != nullptr; }

  /// Can we gaurantee that the given reference counted value is incremented?
  bool isIncremented() const { return SequenceState::Incremented == State; }

  /// Are we tracking an instruction currently? This returns false when given
  /// an uninitialized ReferenceCountState.
  bool hasInstruction() const { return Instruction != nullptr; }

  /// Return the increment we are tracking.
  SILInstruction *getInstruction() const { return Instruction; }

  /// Return the value with reference semantics that is the operand of our
  /// increment.
  SILValue getValue() const { return getInstruction()->getOperand(0); }

  /// The latest point we can move the increment without bypassing instructions
  /// that may have reference semantics.
  SILInstruction *getInsertPoint() const { return InsertPt; }

  ///
  bool isKnownSafe() const { return KnownSafe; }

  /// Is this an increment that we can perform code motion for. This is true
  /// for strong_retain and false for copy_value.
  bool canBeMoved() const { return isa<StrongRetainInst>(Instruction); }

  /// Initializes/reinitialized the state for I. If we reinitialize we return
  /// true.
  bool init(SILInstruction *I);

  /// Uninitialize the current state.
  void clear();

  /// Check if PotentialDecrement can decrement the reference count associated
  /// with the value we are tracking. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool handlePotentialDecrement(SILInstruction *PotentialDecrement);

  /// Check if PotentialUser could be a use of the reference counted value that
  /// requires user to be alive. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool handlePotentialUser(SILInstruction *PotentialUser);

  /// Returns true if Decrement is a decrement instruction that matches the
  /// increment we are tracking. Performs all necessary state cleanups.
  bool doesDecrementMatchInstruction(SILInstruction *Decrement);
};

} // end anonymous namespace

bool ReferenceCountState::init(SILInstruction *I) {
  bool AlreadyInitialized = isInitialized();

  // This retain is known safe if the operand we are tracking was already
  // known incremented previously. This occurs when you have nested increments.
  KnownSafe = isIncremented();

  // Reset our state to Incremented.
  State = SequenceState::Incremented;

  // Stash the incrementing instruction we are tracking.
  Instruction = I;

  // Reset our insertion point to null.
  InsertPt = nullptr;

  return AlreadyInitialized;
}

void ReferenceCountState::clear() {
  State = SequenceState::None;
  Instruction = nullptr;
  InsertPt = nullptr;
  KnownSafe = false;
}

bool ReferenceCountState::handlePotentialDecrement(SILInstruction *Other) {
  // If our state is not initialized, return false since we are not tracking
  // anything.
  if (!isInitialized())
    return false;

  // If we can prove that Other can not decrement the reference counted
  // instruction we are tracking, return false.
  if (cannotDecrementRefCount(Other, getValue()))
    return false;

  // Otherwise Other could potentially decrement the value we are tracking.
  // Attempt to advance the sequence. If we do advance the sequence, return
  // true. If we can not advance the sequence then this information is not
  // relevant, so return false.
  switch (State) {
    case SequenceState::Incremented:
      State = SequenceState::MightBeDecremented;
      InsertPt = Other;
      return true;
    case SequenceState::None:
    case SequenceState::MightBeDecremented:
    case SequenceState::MightBeUsed:
      return false;
  }
}

bool ReferenceCountState::handlePotentialUser(SILInstruction *Other) {
  // If our state is not initialized, return false since we are not tracking
  // anything.
  if (!isInitialized())
    return false;

  // If we can prove that Other can not use the pointer we are tracking,
  // return...
  if (cannotUseValue(Other, getValue()))
    return false;

  // Otherwise advance the sequence...
  switch (State) {
    case SequenceState::MightBeDecremented:
      State = SequenceState::MightBeUsed;
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
  if (!isInitialized() || !matchingRefCountPairType(Instruction, Decrement))
    return false;

  // Otherwise modify the state appropriately in preparation for removing the
  // increment, decrement pair.
  switch (State) {
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
   case ValueKind::CopyValueInst:
   case ValueKind::DestroyValueInst:
   case ValueKind::StrongRetainInst:
   case ValueKind::StrongReleaseInst:
     return true;
  }
}
#endif

static bool
processBBTopDown(SILBasicBlock &BB,
                 llvm::MapVector<SILValue, ReferenceCountState> &BBState,
                 llvm::DenseMap<SILInstruction *,
                                ReferenceCountState> &DecToIncStateMap) {

  bool NestingDetected = false;

#ifndef NDEBUG
  // If we are in debug mode, to make the output less verbose, make sure we are
  // actually processing something interesting before emitting any log output.
  bool FoundOneInst = false;
  for (auto &I : BB)
    FoundOneInst |= isInterestingInstruction(I.getKind());
  if (!FoundOneInst)
    return false;
#endif

  // For each instruction I in BB...
  for (auto &I : BB) {

    DEBUG(llvm::dbgs() << "VISITING:\n    " << I);

    // If I is a ref count increment instruction...
    if (isRefCountIncrement(I)) {
      // map its operand to a newly initialized or reinitialized ref count
      // state and continue...
      SILValue Operand = I.getOperand(0);
      ReferenceCountState &RefCountState = BBState[Operand];
      NestingDetected |= RefCountState.init(&I);

      // If we have a copy value add in an additional state for its output
      // pointer.
      if (isa<CopyValueInst>(I)) {
        ReferenceCountState &OutputRefCountState = BBState[SILValue(&I, 0)];
        OutputRefCountState.init(&I);
        NestingDetected |= OutputRefCountState.init(&I);
      }

      DEBUG(llvm::dbgs() << "    REF COUNT INCREMENT! Known Safe: "
            << (RefCountState.isKnownSafe()?"yes":"no") << "\n");

      ++NumIncrements;
      continue;
    }

    // If we have a reference count decrement...
    if (isRefCountDecrement(I)) {
      // Look up the state associated with its operand...
      SILValue Operand = I.getOperand(0);
      ReferenceCountState &RefCountState = BBState[Operand];

      DEBUG(llvm::dbgs() << "    REF COUNT DECREMENT!\n");

      // If the state is already initialized to contain a reference count
      // increment of the same type (i.e. copy_value, destroy_value or
      // strong_retain, strong_release), then remove the state from the map
      // and add the retain/release pair to the delete list and continue.
      if (RefCountState.doesDecrementMatchInstruction(&I)) {
        // Copy the current value of ref count state into the result map.
        DecToIncStateMap[&I] = RefCountState;
        DEBUG(llvm::dbgs() << "    MATCHING INCREMENT:           "
              << *RefCountState.getInstruction());

        // If we have matched up a copy_value, find the other copy of its state
        // and remove it so we do not attempt to remove the copy_value twice.
        if (auto *CV =
              dyn_cast<CopyValueInst>(RefCountState.getInstruction())) {
          SILValue CVOperand = CV->getOperand();
          if (CVOperand == Operand) {
            // We matched the copy value's operand, clear the state associated
            // with its result.
            BBState[SILValue(CV, 0)].clear();
          } else {
            // We matched the copy value's result, clear the state associated
            // with its operand.
            BBState[CVOperand].clear();
          }
        }

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
      // First check if the instruction we are visiting could potentially
      // decrement the reference counted value we are tracking... in a
      // manner that could cause us to change states. If we do change states
      // continue...
      if (OtherState.second.handlePotentialDecrement(&I)) {
        DEBUG(llvm::dbgs() << "    Found Potential Decrement:\n        "
              << *OtherState.second.getInstruction());
        continue;
      }

      // Otherwise check if the reference counted value we are tracking
      // could be used by the given instruction.
      SILInstruction *Other = OtherState.second.getInstruction();
      (void)Other;
      if (OtherState.second.handlePotentialUser(&I))
        DEBUG(llvm::dbgs() << "    Found Potential Use:\n        " << *Other);
    }
  }

  // Return whether or not we saw a nested retain to signal if so to process
  // this basic block again.
  return NestingDetected;
}

//===----------------------------------------------------------------------===//
//                                Code Motion
//===----------------------------------------------------------------------===//

static bool
performCodeMotion(llvm::DenseMap<SILInstruction *,
                                 ReferenceCountState> &DecToIncStateMap) {
  llvm::SmallVector<SILInstruction *, 16> DeleteList;

  for (auto &Pair : DecToIncStateMap) {
    // If we do not have an instruction, this is a state with an invalidated
    // reference count. Skip it...
    if (!Pair.second.getInstruction())
      continue;

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

      assert(!std::count(DeleteList.begin(), DeleteList.end(), Pair.first) &&
             "Should only add an instruction to this list once.");
      assert(!std::count(DeleteList.begin(), DeleteList.end(), Inst) &&
             "Should only add an instruction to this list once.");

      // Add our deleted instructions to the delete list.
      DeleteList.push_back(Pair.first);
      DeleteList.push_back(Inst);

      // If we are going to remove a copy value, propagate the operand of the
      // copy value to all uses of the copy value.
      if (isa<CopyValueInst>(Inst))
        SILValue(Inst, 0).replaceAllUsesWith(Inst->getOperand(0));

      ++NumIncrementsRemoved;
      continue;
    }
  }

  // Delete the instructions.
  for (auto *Inst : DeleteList)
    Inst->eraseFromParent();

  // If we found instructions to delete, return true.
  return !DeleteList.empty();;
}


//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static void processFunction(SILFunction &F) {
  DEBUG(llvm::dbgs() << "***** Processing " << F.getName() << " *****\n");

  llvm::MapVector<SILValue, ReferenceCountState> BBState;
  llvm::DenseMap<SILInstruction *, ReferenceCountState> DecToIncStateMap;

  // For each basic block in F...
  for (auto &BB : F) {
    DEBUG(llvm::dbgs() << "\n<<< Processing New BB! >>>\n");
    bool Changed;

    // Process the basic block until we do not eliminate any inc/dec pairs
    // and see any nested increments.
    while (true) {
      BBState.clear();
      DecToIncStateMap.clear();

      // We need to rerun if we saw any nested increment/decrements and if we
      // removed any increment/decrement pairs.
      Changed = processBBTopDown(BB, BBState, DecToIncStateMap);
      Changed &= performCodeMotion(DecToIncStateMap);
      if (!Changed)
        break;

      DEBUG(llvm::dbgs() << "\n<<< Made a Change! Reprocessing BB! >>>\n");
    }

    DEBUG(llvm::dbgs() << "\n");
  }
}

void swift::performSILARCOpts(SILModule *M) {
  if (!EnableARCOpts)
    return;

  DEBUG(llvm::dbgs() << "*** SIL ARC OPTS ***\n");
  // For each function in the module...
  for (SILFunction &F : *M) {
    // If the function has no basic blocks, skip it...
    if (F.empty())
      continue;

    // Otherwise perform ARC optimizations.
    processFunction(F);
  }
}
