//===--- COWArrayOpt.cpp - Optimize Copy-On-Write Array Checks ------------===//
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
///
/// Optimize CoW array access by hoisting uniqueness checks.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "cowarray-opts"

#include "ArrayOpt.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/Analysis/ColdBlockInfo.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
using namespace swift;

// Do the two values \p A and \p B reference the same 'array' after potentially
// looking through a load. To identify a common array address this functions
// strips struct projections until it hits \p ArrayAddress.
bool areArraysEqual(RCIdentityFunctionInfo *RCIA, SILValue A, SILValue B,
                    SILValue ArrayAddress) {
  A = RCIA->getRCIdentityRoot(A);
  B = RCIA->getRCIdentityRoot(B);
  if (A == B)
    return true;
  // We have stripped off struct_extracts. Remove the load to look at the
  // address we are loading from.
  if (auto *ALoad = dyn_cast<LoadInst>(A))
    A = ALoad->getOperand();
  if (auto *BLoad = dyn_cast<LoadInst>(B))
    B = BLoad->getOperand();
  // Strip off struct_extract_refs until we hit array address.
  if (ArrayAddress) {
    StructElementAddrInst *SEAI = nullptr;
    while (A != ArrayAddress && (SEAI = dyn_cast<StructElementAddrInst>(A)))
      A = SEAI->getOperand();
    while (B != ArrayAddress && (SEAI = dyn_cast<StructElementAddrInst>(B)))
      B = SEAI->getOperand();
  }
  return A == B;
}

/// \return true if the given instruction releases the given value.
static bool isRelease(SILInstruction *Inst, SILValue RetainedValue,
                      SILValue ArrayAddress, RCIdentityFunctionInfo *RCIA,
                      SmallPtrSetImpl<Operand *> &MatchedReleases) {

  // Before we can match a release with a retain we need to check that we have
  // not already matched the release with a retain we processed earlier.
  // We don't want to match the release with both retains in the example below.
  //
  //   retain %a  <--|
  //   retain %a     | Match.   <-| Don't match.
  //   release %a <--|          <-|
  //
  if (auto *R = dyn_cast<ReleaseValueInst>(Inst))
    if (!MatchedReleases.count(&R->getOperandRef()))
      if (areArraysEqual(RCIA, Inst->getOperand(0), RetainedValue,
                         ArrayAddress)) {
        LLVM_DEBUG(llvm::dbgs() << "     matching with release " << *Inst);
        MatchedReleases.insert(&R->getOperandRef());
        return true;
      }

  if (auto *R = dyn_cast<StrongReleaseInst>(Inst))
    if (!MatchedReleases.count(&R->getOperandRef()))
      if (areArraysEqual(RCIA, Inst->getOperand(0), RetainedValue,
                         ArrayAddress)) {
        LLVM_DEBUG(llvm::dbgs() << "     matching with release " << *Inst);
        MatchedReleases.insert(&R->getOperandRef());
        return true;
      }

  if (auto *AI = dyn_cast<ApplyInst>(Inst)) {
    if (auto *F = AI->getReferencedFunctionOrNull()) {
      auto Params = F->getLoweredFunctionType()->getParameters();
      auto Args = AI->getArguments();
      for (unsigned ArgIdx = 0, ArgEnd = Params.size(); ArgIdx != ArgEnd;
           ++ArgIdx) {
        if (MatchedReleases.count(&AI->getArgumentRef(ArgIdx)))
          continue;
        if (!areArraysEqual(RCIA, Args[ArgIdx], RetainedValue, ArrayAddress))
          continue;
        ParameterConvention P = Params[ArgIdx].getConvention();
        if (P == ParameterConvention::Direct_Owned) {
          LLVM_DEBUG(llvm::dbgs() << "     matching with release " << *Inst);
          MatchedReleases.insert(&AI->getArgumentRef(ArgIdx));
          return true;
        }
      }
    }
  }
  LLVM_DEBUG(llvm::dbgs() << "      not a matching release " << *Inst);
  return false;
}

namespace {

/// Optimize Copy-On-Write array checks based on high-level semantics.
///
/// Performs an analysis on all Array users to ensure they do not interfere
/// with make_mutable hoisting. Ultimately, the only thing that can interfere
/// with make_mutable is a retain of the array. To ensure no retains occur
/// within the loop, it is necessary to check that the array does not escape on
/// any path reaching the loop, and that it is not directly retained within the
/// loop itself.
///
/// In some cases, a retain does exist within the loop, but is balanced by a
/// release or call to @owned. The analysis must determine whether any array
/// mutation can occur between the retain and release. To accomplish this it
/// relies on knowledge of all array operations within the loop. If the array
/// escapes in some way that cannot be tracked, the analysis must fail.
///
/// TODO: Handle this pattern:
///   retain(array)
///   call(array)
///   release(array)
/// Whenever the call is readonly, has balanced retain/release for the array,
/// and does not capture the array. Under these conditions, the call can neither
/// mutate the array nor save an alias for later mutation.
///
/// TODO: Completely eliminate make_mutable calls if all operations that the
/// guard are already guarded by either "init" or "mutate_unknown".
class COWArrayOpt {
  typedef StructUseCollector::UserList UserList;
  typedef StructUseCollector::UserOperList UserOperList;

  RCIdentityFunctionInfo *RCIA;
  SILFunction *Function;
  SILLoop *Loop;
  SILBasicBlock *Preheader;
  DominanceInfo *DomTree;
  bool HasChanged = false;

  // Keep track of cold blocks.
  ColdBlockInfo ColdBlocks;

  // Cache of the analysis whether a loop is safe wrt. make_unique hoisting by
  // looking at the operations (no uniquely identified objects).
  std::pair<bool, bool> CachedSafeLoop;

  // Set of all blocks that may reach the loop, not including loop blocks.
  llvm::SmallPtrSet<SILBasicBlock*,32> ReachingBlocks;

  /// Transient per-Array user set.
  ///
  /// Track all known array users with the exception of struct_extract users
  /// (checkSafeArrayElementUse prohibits struct_extract users from mutating the
  /// array). During analysis of retains/releases within the loop body, the
  /// users in this set are assumed to cover all possible mutating operations on
  /// the array. If the array escaped through an unknown use, the analysis must
  /// abort earlier.
  SmallPtrSet<SILInstruction*, 8> ArrayUserSet;

  /// Array loads which can be hoisted because a make_mutable of that array
  /// was hoisted previously.
  /// This is important to handle the two dimensional array case.
  SmallPtrSet<LoadInst *, 4> HoistableLoads;

  // When matching retains to releases we must not match the same release twice.
  //
  // For example we could have:
  //   retain %a // id %1
  //   retain %a // id %2
  //   release %a // id %3
  // When we match %1 with %3, we can't match %3 again when we look for a
  // matching release for %2.
  // The set refers to operands instead of instructions because an apply could
  // have several operands with release semantics.
  SmallPtrSet<Operand*, 8> MatchedReleases;

  // The address of the array passed to the current make_mutable we are
  // analyzing.
  SILValue CurrentArrayAddr;
public:
  COWArrayOpt(RCIdentityFunctionInfo *RCIA, SILLoop *L, DominanceAnalysis *DA)
      : RCIA(RCIA), Function(L->getHeader()->getParent()), Loop(L),
        Preheader(L->getLoopPreheader()), DomTree(DA->get(Function)),
        ColdBlocks(DA), CachedSafeLoop(false, false) {}

  bool run();

protected:
  bool checkUniqueArrayContainer(SILValue ArrayContainer);
  SmallPtrSetImpl<SILBasicBlock*> &getReachingBlocks();
  bool isRetainReleasedBeforeMutate(SILInstruction *RetainInst,
                                    bool IsUniquelyIdentifiedArray = true);
  bool checkSafeArrayAddressUses(UserList &AddressUsers);
  bool checkSafeArrayValueUses(UserList &ArrayValueUsers);
  bool checkSafeArrayElementUse(SILInstruction *UseInst, SILValue ArrayVal);
  bool checkSafeElementValueUses(UserOperList &ElementValueUsers);
  bool hoistMakeMutable(ArraySemanticsCall MakeMutable, bool dominatesExits);
  bool dominatesExitingBlocks(SILBasicBlock *BB);
  void hoistAddressProjections(Operand &ArrayOp);
  bool hasLoopOnlyDestructorSafeArrayOperations();
  SILValue getArrayAddressBase(SILValue V);
};
} // end anonymous namespace

/// \return true of the given container is known to be a unique copy of the
/// array with no aliases. Cases we check:
///
/// (1) An @inout argument.
///
/// (2) A local variable, which may be copied from a by-val argument,
/// initialized directly, or copied from a function return value. We don't
/// need to check how it is initialized here, because that will show up as a
/// store to the local's address. checkSafeArrayAddressUses will check that the
/// store is a simple initialization outside the loop.
bool COWArrayOpt::checkUniqueArrayContainer(SILValue ArrayContainer) {
  if (auto *Arg = dyn_cast<SILArgument>(ArrayContainer)) {
    // Check that the argument is passed as an inout type. This means there are
    // no aliases accessible within this function scope.
    auto Params = Function->getLoweredFunctionType()->getParameters();
    ArrayRef<SILArgument *> FunctionArgs = Function->begin()->getArguments();
    for (unsigned ArgIdx = 0, ArgEnd = Params.size();
         ArgIdx != ArgEnd; ++ArgIdx) {
      if (FunctionArgs[ArgIdx] != Arg)
        continue;

      if (!Params[ArgIdx].isIndirectInOut()) {
        LLVM_DEBUG(llvm::dbgs()
                   << "    Skipping Array: Not an inout argument!\n");
        return false;
      }
    }
    return true;
  }
  else if (isa<AllocStackInst>(ArrayContainer))
    return true;

  if (auto *LI = dyn_cast<LoadInst>(ArrayContainer)) {
    // A load of another array, which follows a make_mutable, also guarantees
    // a unique container. This is the case if the current array is an element
    // of the outer array in nested arrays.
    if (HoistableLoads.count(LI) != 0)
      return true;
  }

  // TODO: we should also take advantage of access markers to identify
  // unique arrays.

  LLVM_DEBUG(llvm::dbgs()
             << "    Skipping Array: Not an argument or local variable!\n");
  return false;
}

/// Lazily compute blocks that may reach the loop.
SmallPtrSetImpl<SILBasicBlock*> &COWArrayOpt::getReachingBlocks() {
  if (ReachingBlocks.empty()) {
    SmallVector<SILBasicBlock*, 8> Worklist;
    ReachingBlocks.insert(Preheader);
    Worklist.push_back(Preheader);
    while (!Worklist.empty()) {
      SILBasicBlock *BB = Worklist.pop_back_val();
      for (auto PI = BB->pred_begin(), PE = BB->pred_end(); PI != PE; ++PI) {
        if (ReachingBlocks.insert(*PI).second)
          Worklist.push_back(*PI);
      }
    }
  }
  return ReachingBlocks;
}


/// \return true if the instruction is a call to a non-mutating array semantic
/// function.
static bool isNonMutatingArraySemanticCall(SILInstruction *Inst) {
  ArraySemanticsCall Call(Inst);
  if (!Call)
    return false;

  switch (Call.getKind()) {
  case ArrayCallKind::kNone:
  case ArrayCallKind::kArrayPropsIsNativeTypeChecked:
  case ArrayCallKind::kCheckSubscript:
  case ArrayCallKind::kCheckIndex:
  case ArrayCallKind::kGetCount:
  case ArrayCallKind::kGetCapacity:
  case ArrayCallKind::kGetElement:
  case ArrayCallKind::kGetElementAddress:
    return true;
  case ArrayCallKind::kMakeMutable:
  case ArrayCallKind::kMutateUnknown:
  case ArrayCallKind::kReserveCapacityForAppend:
  case ArrayCallKind::kWithUnsafeMutableBufferPointer:
  case ArrayCallKind::kArrayInit:
  case ArrayCallKind::kArrayUninitialized:
  case ArrayCallKind::kAppendContentsOf:
  case ArrayCallKind::kAppendElement:
    return false;
  }

  llvm_unreachable("Unhandled ArrayCallKind in switch.");
}

/// \return true if the given retain instruction is followed by a release on the
/// same object prior to any potential mutating operation.
bool COWArrayOpt::isRetainReleasedBeforeMutate(SILInstruction *RetainInst,
                                               bool IsUniquelyIdentifiedArray) {
  // If a retain is found outside the loop ignore it. Otherwise, it must
  // have a matching @owned call.
  if (!Loop->contains(RetainInst))
    return true;

  LLVM_DEBUG(llvm::dbgs() << "     Looking at retain " << *RetainInst);

  // Walk forward looking for a release of ArrayLoad or element of
  // ArrayUserSet. Note that ArrayUserSet does not included uses of elements
  // within the Array. Consequently, checkSafeArrayElementUse must prove that
  // no uses of the Array value, or projections of it can lead to mutation
  // (element uses may only be retained/released).
  for (auto II = std::next(SILBasicBlock::iterator(RetainInst)),
         IE = RetainInst->getParent()->end(); II != IE; ++II) {
    if (isRelease(&*II, RetainInst->getOperand(0), CurrentArrayAddr, RCIA,
                  MatchedReleases))
      return true;

    if (isa<RetainValueInst>(II) || isa<StrongRetainInst>(II))
      continue;

    // A side effect free instruction cannot mutate the array.
    if (!II->mayHaveSideEffects())
      continue;

    // Non mutating array calls are safe.
    if (isNonMutatingArraySemanticCall(&*II))
      continue;

    if (IsUniquelyIdentifiedArray) {
      // It is okay for an identified loop to have releases in between a retain
      // and a release. We can end up here if we have two retains in a row and
      // then a release. The second retain cannot be matched with the release
      // but must be matched by a follow up instruction.
      //   retain %ptr
      //   retain %ptr
      //   release %ptr
      //   array_operation(..., @owned %ptr)
      //
      // This is not the case for a potentially aliased array because a release
      // can cause a destructor to run. The destructor in turn can cause
      // arbitrary side effects.
      if (isa<ReleaseValueInst>(II) || isa<StrongReleaseInst>(II))
        continue;

      if (ArrayUserSet.count(&*II)) // May be an array mutation.
        break;
    } else {
      // Not safe.
      break;
    }
  }
  LLVM_DEBUG(llvm::dbgs() << "    Skipping Array: retained in loop!\n"
                          << "    " << *RetainInst);
  return false;
}

/// \return true if all given users of an array address are safe to hoist
/// make_mutable across.
///
/// General calls are unsafe because they may copy the array struct which in
/// turn bumps the reference count of the array storage.
///
/// The same logic currently applies to both uses of the array struct itself and
/// uses of an aggregate containing the array.
///
/// This does not apply to addresses of elements within the array. e.g. it is
/// not safe to store to an element in the array because we may be storing an
/// alias to the array storage.
bool COWArrayOpt::checkSafeArrayAddressUses(UserList &AddressUsers) {

  for (auto *UseInst : AddressUsers) {

    if (auto *AI = dyn_cast<ApplyInst>(UseInst)) {
      if (ArraySemanticsCall(AI))
        continue;

      // Check of this escape can reach the current loop.
      if (!Loop->contains(UseInst->getParent()) &&
          !getReachingBlocks().count(UseInst->getParent())) {
        continue;
      }

      LLVM_DEBUG(llvm::dbgs() << "    Skipping Array: may escape "
                                 "through call!\n"
                              << "    " << *UseInst);
      return false;
    }

    if (auto *StInst = dyn_cast<StoreInst>(UseInst)) {
      // Allow a local array to be initialized outside the loop via a by-value
      // argument or return value. The array value may be returned by its
      // initializer or some other factory function.
      if (Loop->contains(StInst->getParent())) {
        LLVM_DEBUG(llvm::dbgs() << "    Skipping Array: store inside loop!\n"
                                << "    " << *StInst);
        return false;
      }

      SILValue InitArray = StInst->getSrc();
      if (isa<SILArgument>(InitArray) || isa<ApplyInst>(InitArray))
        continue;

      LLVM_DEBUG(llvm::dbgs() << "    Skipping Array: may escape "
                                 "through store!\n"
                              << "    " << *UseInst);
      return false;
    }

    if (isa<DeallocStackInst>(UseInst)) {
      // Handle destruction of a local array.
      continue;
    }

    if (isa<MarkDependenceInst>(UseInst)) {
      continue;
    }

    LLVM_DEBUG(llvm::dbgs() << "    Skipping Array: unknown Array use!\n"
                            << "    " << *UseInst);
    // Found an unsafe or unknown user. The Array may escape here.
    return false;
  }
  return true;
}

/// Returns true if this instruction is a safe array use if all of its users are
/// also safe array users.
static SILValue isTransitiveSafeUser(SILInstruction *I) {
  switch (I->getKind()) {
  case SILInstructionKind::StructExtractInst:
  case SILInstructionKind::TupleExtractInst:
  case SILInstructionKind::UncheckedEnumDataInst:
  case SILInstructionKind::StructInst:
  case SILInstructionKind::TupleInst:
  case SILInstructionKind::EnumInst:
  case SILInstructionKind::UncheckedRefCastInst:
  case SILInstructionKind::UncheckedBitwiseCastInst:
    return cast<SingleValueInstruction>(I);
  default:
    return nullptr;
  }
}

/// Check that the use of an Array value, the value of an aggregate containing
/// an array, or the value of an element within the array, is safe w.r.t
/// make_mutable hoisting. Retains are safe as long as they are not inside the
/// Loop.
bool COWArrayOpt::checkSafeArrayValueUses(UserList &ArrayValueUsers) {
  for (auto *UseInst : ArrayValueUsers) {
    if (auto *AI = dyn_cast<ApplyInst>(UseInst)) {
      if (ArraySemanticsCall(AI))
        continue;

      // Found an unsafe or unknown user. The Array may escape here.
      LLVM_DEBUG(llvm::dbgs() << "    Skipping Array: unsafe call!\n"
                              << "    " << *UseInst);
      return false;
    }

    /// Is this a unary transitive safe user instruction. This means that the
    /// instruction is safe only if all of its users are safe. Check this
    /// recursively.
    if (auto inst = isTransitiveSafeUser(UseInst)) {
      if (std::all_of(inst->use_begin(), inst->use_end(),
                      [this](Operand *Op) -> bool {
                        return checkSafeArrayElementUse(Op->getUser(),
                                                        Op->get());
                      }))
        continue;
      return false;
    }

    if (isa<RetainValueInst>(UseInst)) {
      if (isRetainReleasedBeforeMutate(UseInst))
        continue;
      // Found an unsafe or unknown user. The Array may escape here.
      LLVM_DEBUG(llvm::dbgs() << "    Skipping Array: found unmatched retain "
                                 "value!\n"
                              << "    " << *UseInst);
      return false;
    }

    if (isa<ReleaseValueInst>(UseInst)) {
      // Releases are always safe. This case handles the release of an array
      // buffer that is loaded from a local array struct.
      continue;
    }

    if (isa<MarkDependenceInst>(UseInst))
      continue;

    if (UseInst->isDebugInstruction())
      continue;
    
    // Found an unsafe or unknown user. The Array may escape here.
    LLVM_DEBUG(llvm::dbgs() << "    Skipping Array: unsafe Array value use!\n"
                            << "    " << *UseInst);
    return false;
  }
  return true;
}

/// Given an array value, recursively check that uses of elements within the
/// array are safe.
///
/// Consider any potentially mutating operation unsafe. Mutation would not
/// prevent make_mutable hoisting, but it would interfere with
/// isRetainReleasedBeforeMutate. Since struct_extract users are not visited by
/// StructUseCollector, they are never added to ArrayUserSet. Thus we check here
/// that no mutating struct_extract users exist.
///
/// After the lower aggregates pass, SIL contains chains of struct_extract and
/// retain_value instructions. e.g.
///   %a = load %0 : $*Array<Int>
///   %b = struct_extract %a : $Array<Int>, #Array._buffer
///   %s = struct_extract %b : $_ArrayBuffer<Int>, #_ArrayBuffer.storage
///  retain_value %s : $Optional<Builtin.NativeObject>
///
///  SILCombine typically simplifies this by bypassing the
///  struct_extract. However, for completeness this analysis has the ability to
///  follow struct_extract users.
///
/// Since this does not recurse through multi-operand instructions, no visited
/// set is necessary.
bool COWArrayOpt::checkSafeArrayElementUse(SILInstruction *UseInst,
                                           SILValue ArrayVal) {
  if ((isa<RetainValueInst>(UseInst) || isa<StrongRetainInst>(UseInst)) &&
      isRetainReleasedBeforeMutate(UseInst))
    return true;

  if (isa<ReleaseValueInst>(UseInst) || isa<StrongReleaseInst>(UseInst))
    // Releases are always safe. This case handles the release of an array
    // buffer that is loaded from a local array struct.
    return true;

  if (isa<RefTailAddrInst>(UseInst)) {
    return true;
  }

  // Look for a safe mark_dependence instruction use.
  //
  // This use looks something like:
  //
  //   %57 = load %56 : $*Builtin.BridgeObject from Array<Int>
  //   %58 = unchecked_ref_cast %57 : $Builtin.BridgeObject to
  //   $_ContiguousArray
  //   %59 = unchecked_ref_cast %58 : $__ContiguousArrayStorageBase to
  //   $Builtin.NativeObject
  //   %60 = struct_extract %53 : $UnsafeMutablePointer<Int>,
  //   #UnsafeMutablePointer
  //   %61 = pointer_to_address %60 : $Builtin.RawPointer to strict $*Int
  //   %62 = mark_dependence %61 : $*Int on %59 : $Builtin.NativeObject
  //
  // The struct_extract, unchecked_ref_cast is handled below in the
  // "Transitive SafeArrayElementUse" code.
  if (isa<MarkDependenceInst>(UseInst))
    return true;

  if (UseInst->isDebugInstruction())
    return true;
  
  // If this is an instruction which is a safe array element use if and only if
  // all of its users are safe array element uses, recursively check its uses
  // and return false if any of them are not transitive escape array element
  // uses.
  if (auto result = isTransitiveSafeUser(UseInst)) {
    return std::all_of(result->use_begin(), result->use_end(),
                       [this, &ArrayVal](Operand *UI) -> bool {
                         return checkSafeArrayElementUse(UI->getUser(),
                                                         ArrayVal);
                       });
  }

  // Found an unsafe or unknown user. The Array may escape here.
  LLVM_DEBUG(llvm::dbgs() << "    Skipping Array: unknown Element use!\n"
                          << *UseInst);
  return false;
}

/// Check that the use of an Array element is safe w.r.t. make_mutable hoisting.
///
/// This logic should be similar to checkSafeArrayElementUse
bool COWArrayOpt::checkSafeElementValueUses(UserOperList &ElementValueUsers) {
  for (auto &Pair : ElementValueUsers) {
    SILInstruction *UseInst = Pair.first;
    Operand *ArrayValOper = Pair.second;
    if (!checkSafeArrayElementUse(UseInst, ArrayValOper->get()))
      return false;
  }
  return true;
}

/// Check if a loop has only 'safe' array operations such that we can hoist the
/// uniqueness check even without having an 'identified' object.
///
/// 'Safe' array operations are:
///   * all array semantic functions
///   * stores to array elements
///   * any instruction that does not have side effects.
///   * any retain must be matched by a release before we hit a make_unique.
///
/// Note, that a release in this modus (we don't have a uniquely identified
/// object) is not safe because the destructor of what we are releasing might
/// be unsafe (creating a reference).
///
bool COWArrayOpt::hasLoopOnlyDestructorSafeArrayOperations() {
  if (CachedSafeLoop.first)
    return CachedSafeLoop.second;

  assert(!CachedSafeLoop.second &&
         "We only move to a true state below");

  // We will compute the state of this loop now.
  CachedSafeLoop.first = true;

  // We need to cleanup the MatchedRelease on return.
  auto ReturnWithCleanup = [&] (bool LoopHasSafeOperations) {
    MatchedReleases.clear();
    return LoopHasSafeOperations;
  };

  LLVM_DEBUG(llvm::dbgs() << "    checking whether loop only has safe array "
                             "operations ...\n");
  CanType SameTy;
  for (auto *BB : Loop->getBlocks()) {
    for (auto &It : *BB) {
      auto *Inst = &It;
      LLVM_DEBUG(llvm::dbgs() << "        visiting: " << *Inst);

      // Semantic calls are safe.
      ArraySemanticsCall Sem(Inst);
      if (Sem) {
        auto Kind = Sem.getKind();
        // Safe because they create new arrays.
        if (Kind == ArrayCallKind::kArrayInit ||
            Kind == ArrayCallKind::kArrayUninitialized)
          continue;
        // All array types must be the same. This is a stronger guaranteed than
        // we actually need. The requirement is that we can't create another
        // reference to the array by performing an array operation: for example,
        // storing or appending one array into a two-dimensional array.
        // Checking
        // that all types are the same make guarantees that this cannot happen.
        if (SameTy.isNull()) {
          SameTy = Sem.getSelf()->getType().getASTType();
          continue;
        }
        
        if (Sem.getSelf()->getType().getASTType() != SameTy) {
          LLVM_DEBUG(llvm::dbgs() << "    (NO) mismatching array types\n");
          return ReturnWithCleanup(false);
        }

        // Safe array semantics operation.
        continue;
      }

      // Stores to array elements.
      if (auto *SI = dyn_cast<StoreInst>(Inst)) {
        if (isAddressOfArrayElement(SI->getDest()))
          continue;
        LLVM_DEBUG(llvm::dbgs() << "     (NO) unknown store " << *SI);
        return ReturnWithCleanup(false);
      }

      // Instructions without side effects are safe.
      if (!Inst->mayHaveSideEffects())
        continue;
      if (isa<CondFailInst>(Inst))
        continue;
      if (isa<AllocationInst>(Inst) || isa<DeallocStackInst>(Inst))
        continue;

      if (isa<RetainValueInst>(Inst) || isa<StrongRetainInst>(Inst))
        if (isRetainReleasedBeforeMutate(Inst, false))
          continue;

      // If the instruction is a matched release we can ignore it.
      if (auto SRI = dyn_cast<StrongReleaseInst>(Inst))
        if (MatchedReleases.count(&SRI->getOperandRef()))
          continue;
      if (auto RVI = dyn_cast<ReleaseValueInst>(Inst))
        if (MatchedReleases.count(&RVI->getOperandRef()))
          continue;

      // Ignore fix_lifetime. It cannot increment ref counts.
      if (isa<FixLifetimeInst>(Inst))
        continue;

      LLVM_DEBUG(llvm::dbgs() << "     (NO) unknown operation " << *Inst);
      return ReturnWithCleanup(false);
    }
  }

  LLVM_DEBUG(llvm::dbgs() << "     (YES)\n");
  CachedSafeLoop.second = true;
  return ReturnWithCleanup(true);
}

/// Return the underlying Array address after stripping off all address
/// projections. Returns an invalid SILValue if the array base does not dominate
/// the loop.
SILValue COWArrayOpt::getArrayAddressBase(SILValue V) {
  while (true) {
    V = stripSinglePredecessorArgs(V);
    if (auto *RefCast = dyn_cast<UncheckedRefCastInst>(V)) {
      V = RefCast->getOperand();
      continue;
    }
    if (auto *SE = dyn_cast<StructExtractInst>(V)) {
      V = SE->getOperand();
      continue;
    }
    if (auto *IA = dyn_cast<IndexAddrInst>(V)) {
      // index_addr is the only projection which has a second operand: the index.
      // Check if the index is loop invariant.
      SILBasicBlock *IndexBlock = IA->getIndex()->getParentBlock();
      if (IndexBlock && !DomTree->dominates(IndexBlock, Preheader))
        return SILValue();
      V = IA->getBase();
      continue;
    }
    if (!Projection::isAddressProjection(V))
      break;
    auto *Inst = cast<SingleValueInstruction>(V);
    if (Inst->getNumOperands() > 1)
      break;
    V = Inst->getOperand(0);
  }
  if (auto *LI = dyn_cast<LoadInst>(V)) {
    if (HoistableLoads.count(LI) != 0)
      return V;
  }
  SILBasicBlock *ArrayAddrBaseBB = V->getParentBlock();
  if (ArrayAddrBaseBB && !DomTree->dominates(ArrayAddrBaseBB, Preheader))
    return SILValue();

  return V;
}

/// Hoist the address projection rooted in \p Op to \p InsertBefore.
/// Requires the projected value to dominate the insertion point.
///
/// Will look through single basic block predecessor arguments.
void COWArrayOpt::hoistAddressProjections(Operand &ArrayOp) {
  SILValue V = ArrayOp.get();
  SILInstruction *Prev = nullptr;
  SILInstruction *InsertPt = Preheader->getTerminator();
  while (true) {
    SILValue Incoming = stripSinglePredecessorArgs(V);

    // Forward the incoming arg from a single predecessor.
    if (V != Incoming) {
      if (V == ArrayOp.get()) {
        // If we are the operand itself set the operand to the incoming
        // argument.
        ArrayOp.set(Incoming);
        V = Incoming;
      } else {
        // Otherwise, set the previous projections operand to the incoming
        // argument.
        assert(Prev && "Must have seen a projection");
        Prev->setOperand(0, Incoming);
        V = Incoming;
      }
    }

    switch (V->getKind()) {
      case ValueKind::LoadInst:
      case ValueKind::StructElementAddrInst:
      case ValueKind::TupleElementAddrInst:
      case ValueKind::RefElementAddrInst:
      case ValueKind::RefTailAddrInst:
      case ValueKind::UncheckedRefCastInst:
      case ValueKind::StructExtractInst:
      case ValueKind::IndexAddrInst:
      case ValueKind::UncheckedTakeEnumDataAddrInst: {
        auto *Inst = cast<SingleValueInstruction>(V);
        // We are done once the current projection dominates the insert point.
        if (DomTree->dominates(Inst->getParent(), Preheader))
          return;

        assert(!isa<LoadInst>(V) || HoistableLoads.count(cast<LoadInst>(V)) != 0);

        // Move the current projection and memorize it for the next iteration.
        Prev = Inst;
        Inst->moveBefore(InsertPt);
        InsertPt = Inst;
        V = Inst->getOperand(0);
        continue;
      }
      default:
        assert(DomTree->dominates(V->getParentBlock(), Preheader) &&
               "The projected value must dominate the insertion point");
        return;
    }
  }
}

/// Check if this call to "make_mutable" is hoistable, and move it, or delete it
/// if it's already hoisted.
bool COWArrayOpt::hoistMakeMutable(ArraySemanticsCall MakeMutable,
                                   bool dominatesExits) {
  LLVM_DEBUG(llvm::dbgs() << "    Checking mutable array: " <<CurrentArrayAddr);

  // We can hoist address projections (even if they are only conditionally
  // executed).
  SILValue ArrayAddrBase = getArrayAddressBase(CurrentArrayAddr);
  if (!ArrayAddrBase) {
    LLVM_DEBUG(llvm::dbgs() << "    Skipping Array: does not dominate loop!\n");
    return false;
  }

  SmallVector<unsigned, 4> AccessPath;
  SILValue ArrayContainer =
    StructUseCollector::getAccessPath(CurrentArrayAddr, AccessPath);
  bool arrayContainerIsUnique = checkUniqueArrayContainer(ArrayContainer);

  StructUseCollector StructUses;

  // Check whether we can hoist make_mutable based on the operations that are
  // in the loop.
  // Note that in this case we don't verify that the array buffer is not aliased
  // and therefore we must be conservative if the make_mutable is executed
  // conditionally (i.e. doesn't dominate all exit blocks).
  // The test SILOptimizer/cowarray_opt.sil: dont_hoist_if_executed_conditionally
  // shows the problem.
  if (hasLoopOnlyDestructorSafeArrayOperations() && dominatesExits) {
    // Done. We can hoist the make_mutable.
    // We still need the array uses later to check if we can add loads to
    // HoistableLoads.
    StructUses.collectUses(ArrayContainer, AccessPath);
  } else {
    // There are some unsafe operations in the loop. If the array is uniquely
    // identifyable and not escaping, then we are good if all the array uses
    // are safe.
    if (!arrayContainerIsUnique) {
      LLVM_DEBUG(llvm::dbgs() << "    Skipping Array: is not unique!\n");
      return false;
    }

    // Check that the Array is not retained with this loop and it's address does
    // not escape within this function.
    StructUses.collectUses(ArrayContainer, AccessPath);
    for (auto *Oper : StructUses.Visited)
      ArrayUserSet.insert(Oper->getUser());

    if (!checkSafeArrayAddressUses(StructUses.AggregateAddressUsers) ||
        !checkSafeArrayAddressUses(StructUses.StructAddressUsers) ||
        !checkSafeArrayValueUses(StructUses.StructValueUsers) ||
        !checkSafeElementValueUses(StructUses.ElementValueUsers) ||
        !StructUses.ElementAddressUsers.empty())
      return false;
  }

  // Hoist the make_mutable.
  LLVM_DEBUG(llvm::dbgs() << "    Hoisting make_mutable: " << *MakeMutable);

  hoistAddressProjections(MakeMutable.getSelfOperand());

  assert(MakeMutable.canHoist(Preheader->getTerminator(), DomTree) &&
         "Should be able to hoist make_mutable");

  MakeMutable.hoist(Preheader->getTerminator(), DomTree);

  // Register array loads. This is needed for hoisting make_mutable calls of
  // inner arrays in the two-dimensional case.
  if (arrayContainerIsUnique &&
      StructUses.hasSingleAddressUse((ApplyInst *)MakeMutable)) {
    for (auto use : MakeMutable.getSelf()->getUses()) {
      if (auto *LI = dyn_cast<LoadInst>(use->getUser()))
        HoistableLoads.insert(LI);
    }
  }
  return true;
}

bool COWArrayOpt::dominatesExitingBlocks(SILBasicBlock *BB) {
  llvm::SmallVector<SILBasicBlock *, 8> ExitingBlocks;
  Loop->getExitingBlocks(ExitingBlocks);
  for (SILBasicBlock *Exiting : ExitingBlocks) {
    if (!DomTree->dominates(BB, Exiting))
      return false;
  }
  return true;
}

bool COWArrayOpt::run() {
  LLVM_DEBUG(llvm::dbgs() << "  Array Opts in Loop " << *Loop);

  Preheader = Loop->getLoopPreheader();
  if (!Preheader) {
    LLVM_DEBUG(llvm::dbgs() << "    Skipping Loop: No Preheader!\n");
    return false;
  }

  // Map an array to a hoisted make_mutable call for the current loop. An array
  // is only mapped to a call once the analysis has determined that no
  // make_mutable calls are required within the loop body for that array.
  llvm::SmallDenseMap<SILValue, ApplyInst*> ArrayMakeMutableMap;

  for (auto *BB : Loop->getBlocks()) {
    if (ColdBlocks.isCold(BB))
      continue;
    bool dominatesExits = dominatesExitingBlocks(BB);
    for (auto II = BB->begin(), IE = BB->end(); II != IE;) {
      // Inst may be moved by hoistMakeMutable.
      SILInstruction *Inst = &*II;
      ++II;
      ArraySemanticsCall MakeMutableCall(Inst, "array.make_mutable");
      if (!MakeMutableCall)
        continue;

      CurrentArrayAddr = MakeMutableCall.getSelf();
      auto HoistedCallEntry = ArrayMakeMutableMap.find(CurrentArrayAddr);
      if (HoistedCallEntry == ArrayMakeMutableMap.end()) {
        if (!hoistMakeMutable(MakeMutableCall, dominatesExits)) {
          ArrayMakeMutableMap[CurrentArrayAddr] = nullptr;
          continue;
        }

        ArrayMakeMutableMap[CurrentArrayAddr] = MakeMutableCall;
        HasChanged = true;
        continue;
      }

      if (!HoistedCallEntry->second)
        continue;

      LLVM_DEBUG(llvm::dbgs() << "    Removing make_mutable call: "
                              << *MakeMutableCall);
      MakeMutableCall.removeCall();
      HasChanged = true;
    }
  }
  return HasChanged;
}

namespace {

class COWArrayOptPass : public SILFunctionTransform {
  void run() override {
    // FIXME: Update for ownership.
    if (getFunction()->hasOwnership())
      return;

    LLVM_DEBUG(llvm::dbgs() << "COW Array Opts in Func "
                            << getFunction()->getName() << "\n");

    auto *DA = PM->getAnalysis<DominanceAnalysis>();
    auto *LA = PM->getAnalysis<SILLoopAnalysis>();
    auto *RCIA =
      PM->getAnalysis<RCIdentityAnalysis>()->get(getFunction());
    SILLoopInfo *LI = LA->get(getFunction());
    if (LI->empty()) {
      LLVM_DEBUG(llvm::dbgs() << "  Skipping Function: No loops.\n");
      return;
    }

    // Create a flat list of loops in loop-tree postorder (bottom-up).
    llvm::SmallVector<SILLoop *, 16> Loops;
    std::function<void (SILLoop*)> pushChildren = [&](SILLoop *L) {
      for (auto *SubLoop : *L)
        pushChildren(SubLoop);
      Loops.push_back(L);
    };
    for (auto *L : *LI)
      pushChildren(L);

    bool HasChanged = false;
    for (auto *L : Loops)
      HasChanged |= COWArrayOpt(RCIA, L, DA).run();

    if (HasChanged)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
  }

};
} // end anonymous namespace

SILTransform *swift::createCOWArrayOpts() {
  return new COWArrayOptPass();
}
