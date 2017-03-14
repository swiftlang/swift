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

#define DEBUG_TYPE "cowarray-opts"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/ColdBlockInfo.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
using namespace swift;

#ifndef NDEBUG
llvm::cl::opt<std::string>
COWViewCFGFunction("view-cfg-before-cow-for", llvm::cl::init(""),
                   llvm::cl::desc("Only print out the sil for this function"));
#endif

/// \return a sequence of integers representing the access path of this element
/// within a Struct/Ref/Tuple.
///
/// Do not form a path with an IndexAddrInst because we have no way to
/// distinguish between indexing and subelement access. The same index could
/// either refer to the next element (indexed) or a subelement.
static SILValue getAccessPath(SILValue V, SmallVectorImpl<unsigned>& Path) {
  V = stripCasts(V);
  ProjectionIndex PI(V);
  if (!PI.isValid() || V->getKind() == ValueKind::IndexAddrInst)
    return V;

  SILValue UnderlyingObject = getAccessPath(PI.Aggregate, Path);
  Path.push_back(PI.Index);
  return UnderlyingObject;
}

namespace {
/// Collect all uses of a struct given an aggregate value that contains the
/// struct and access path describing the projection of the aggregate
/// that accesses the struct.
///
/// AggregateAddressUsers records uses of the aggregate value's address. These
/// may indirectly access the struct's elements.
///
/// Projections over the aggregate that do not access the struct are ignored.
///
/// StructLoads records loads of the struct value.
/// StructAddressUsers records other uses of the struct address.
/// StructValueUsers records direct uses of the loaded struct.
///
/// Projections of the struct over its elements are all similarly recorded in
/// ElementAddressUsers, ElementLoads, and ElementValueUsers.
///
/// bb0(%arg : $*S)
/// apply %f(%arg)           // <--- Aggregate Address User
/// %struct_addr = struct_element_addr %arg : $*S, #S.element
/// apply %g(%struct_addr)   // <--- Struct Address User
/// %val = load %struct_addr // <--- Struct Load
/// apply %h(%val)           // <--- Struct Value User
/// %elt_addr = struct_element_addr %struct_addr : $*A, #A.element
/// apply %i(%elt_addr)      // <--- Element Address User
/// %elt = load %elt_addr    // <--- Element Load
/// apply %j(%elt)           // <--- Element Value User
class StructUseCollector {
public:
  typedef SmallPtrSet<Operand*, 16> VisitedSet;
  typedef SmallVector<SILInstruction*, 16> UserList;

  /// Record the users of a value or an element within that value along with the
  /// operand that directly uses the value. Multiple levels of struct_extract
  /// may exist between the operand and the user instruction.
  typedef SmallVector<std::pair<SILInstruction*, Operand*>, 16> UserOperList;

  UserList AggregateAddressUsers;
  UserList StructAddressUsers;
  UserList StructLoads;
  UserList StructValueUsers;
  UserOperList ElementAddressUsers;
  UserOperList ElementLoads;
  UserOperList ElementValueUsers;
  VisitedSet Visited;

  /// Collect all uses of the value at the given address.
  void collectUses(ValueBase *V, ArrayRef<unsigned> AccessPath) {
    // Save our old indent and increment.
    // Collect all users of the address and loads.
    collectAddressUses(V, AccessPath, nullptr);

    // Collect all uses of the Struct value.
    for (auto *DefInst : StructLoads) {
      for (auto *DefUI : DefInst->getUses()) {
        if (!Visited.insert(&*DefUI).second) {
          continue;
        }

        StructValueUsers.push_back(DefUI->getUser());
      }
    }

    // Collect all users of element values.
    for (auto &Pair : ElementLoads) {
      for (auto *DefUI : Pair.first->getUses()) {
        if (!Visited.insert(&*DefUI).second) {
          continue;
        }

        ElementValueUsers.push_back(
            std::make_pair(DefUI->getUser(), Pair.second));
      }
    }
  }

protected:

  static bool definesSingleObjectType(ValueBase *V) {
    if (auto *Arg = dyn_cast<SILArgument>(V))
      return Arg->getType().isObject();
    if (auto *Inst = dyn_cast<SILInstruction>(V))
      return Inst->hasValue() && Inst->getType().isObject();
    return false;
  }

  /// If AccessPathSuffix is non-empty, then the value is the address of an
  /// aggregate containing the Struct. If AccessPathSuffix is empty and
  /// StructVal is invalid, then the value is the address of the Struct. If
  /// StructVal is valid, the value is the address of an element within the
  /// Struct.
  void collectAddressUses(ValueBase *V, ArrayRef<unsigned> AccessPathSuffix,
                          Operand *StructVal) {
    for (auto *UI : V->getUses()) {
      // Keep the operand, not the instruction in the visited set. The same
      // instruction may theoretically have different types of uses.
      if (!Visited.insert(&*UI).second) {
        continue;
      }

      SILInstruction *UseInst = UI->getUser();
      if (StructVal) {
        // Found a use of an element.
        assert(AccessPathSuffix.empty() && "should have accessed struct");
        if (auto *LoadI = dyn_cast<LoadInst>(UseInst)) {
          ElementLoads.push_back(std::make_pair(LoadI, StructVal));
          continue;
        }

        if (isa<StructElementAddrInst>(UseInst)) {
          collectAddressUses(UseInst, AccessPathSuffix, StructVal);
          continue;
        }

        ElementAddressUsers.push_back(std::make_pair(UseInst,StructVal));
        continue;
      }

      if (AccessPathSuffix.empty()) {
        // Found a use of the struct at the given access path.
        if (auto *LoadI = dyn_cast<LoadInst>(UseInst)) {
          StructLoads.push_back(LoadI);
          continue;
        }

        if (isa<StructElementAddrInst>(UseInst)) {
          collectAddressUses(UseInst, AccessPathSuffix, &*UI);
          continue;
        }

        // Value users - this happens if we start with a value object in V.
        if (definesSingleObjectType(V)) {
          StructValueUsers.push_back(UseInst);
          continue;
        }

        StructAddressUsers.push_back(UseInst);
        continue;
      }

      ProjectionIndex PI(UseInst);
      // Do not form a path from an IndexAddrInst without otherwise
      // distinguishing it from subelement addressing.
      if (!PI.isValid() || V->getKind() == ValueKind::IndexAddrInst) {
        // Found a use of an aggregate containing the given element.
        AggregateAddressUsers.push_back(UseInst);
        continue;
      }

      if (PI.Index != AccessPathSuffix[0]) {
        // Ignore uses of disjoint elements.
        continue;
      }

      // An alloc_box returns its address as the second value.
      assert(PI.Aggregate && "Expected unary element addr inst.");

      // Recursively check for users after stripping this component from the
      // access path.
      collectAddressUses(UseInst, AccessPathSuffix.slice(1), nullptr);
    }
  }
};
} // end anonymous namespace

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
        DEBUG(llvm::dbgs() << "     matching with release " << *Inst);
        MatchedReleases.insert(&R->getOperandRef());
        return true;
      }

  if (auto *R = dyn_cast<StrongReleaseInst>(Inst))
    if (!MatchedReleases.count(&R->getOperandRef()))
      if (areArraysEqual(RCIA, Inst->getOperand(0), RetainedValue,
                         ArrayAddress)) {
        DEBUG(llvm::dbgs() << "     matching with release " << *Inst);
        MatchedReleases.insert(&R->getOperandRef());
        return true;
      }

  if (auto *AI = dyn_cast<ApplyInst>(Inst)) {
    if (auto *F = AI->getReferencedFunction()) {
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
          DEBUG(llvm::dbgs() << "     matching with release " << *Inst);
          MatchedReleases.insert(&AI->getArgumentRef(ArgIdx));
          return true;
        }
      }
    }
  }
  DEBUG(llvm::dbgs() << "      not a matching release " << *Inst);
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

  // Map an array to a hoisted make_mutable call for the current loop. An array
  // is only mapped to a call once the analysis has determined that no
  // make_mutable calls are required within the loop body for that array.
  llvm::SmallDenseMap<SILValue, ApplyInst*> ArrayMakeMutableMap;

  /// \brief Transient per-Array user set.
  ///
  /// Track all known array users with the exception of struct_extract users
  /// (checkSafeArrayElementUse prohibits struct_extract users from mutating the
  /// array). During analysis of retains/releases within the loop body, the
  /// users in this set are assumed to cover all possible mutating operations on
  /// the array. If the array escaped through an unknown use, the analysis must
  /// abort earlier.
  SmallPtrSet<SILInstruction*, 8> ArrayUserSet;

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
  COWArrayOpt(RCIdentityFunctionInfo *RCIA, SILLoop *L,
              DominanceAnalysis *DA)
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
  bool hoistMakeMutable(ArraySemanticsCall MakeMutable);
  void hoistMakeMutableAndSelfProjection(ArraySemanticsCall MakeMutable,
                                         bool HoistProjection);
  bool hasLoopOnlyDestructorSafeArrayOperations();
  bool isArrayValueReleasedBeforeMutate(
      SILValue V, llvm::SmallSet<SILInstruction *, 16> &Releases);
  bool hoistInLoopWithOnlyNonArrayValueMutatingOperations();
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
  if (SILArgument *Arg = dyn_cast<SILArgument>(ArrayContainer)) {
    // Check that the argument is passed as an inout type. This means there are
    // no aliases accessible within this function scope.
    auto Params = Function->getLoweredFunctionType()->getParameters();
    ArrayRef<SILArgument *> FunctionArgs = Function->begin()->getArguments();
    for (unsigned ArgIdx = 0, ArgEnd = Params.size();
         ArgIdx != ArgEnd; ++ArgIdx) {
      if (FunctionArgs[ArgIdx] != Arg)
        continue;

      if (!Params[ArgIdx].isIndirectInOut()) {
        DEBUG(llvm::dbgs() << "    Skipping Array: Not an inout argument!\n");
        return false;
      }
    }
    return true;
  }
  else if (isa<AllocStackInst>(ArrayContainer))
    return true;

  DEBUG(llvm::dbgs()
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
  case ArrayCallKind::kGetArrayOwner:
  case ArrayCallKind::kGetElementAddress:
    return true;
  case ArrayCallKind::kMakeMutable:
  case ArrayCallKind::kMutateUnknown:
  case ArrayCallKind::kWithUnsafeMutableBufferPointer:
  case ArrayCallKind::kArrayInit:
  case ArrayCallKind::kArrayUninitialized:
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

  DEBUG(llvm::dbgs() << "     Looking at retain " << *RetainInst);

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
  DEBUG(llvm::dbgs() << "    Skipping Array: retained in loop!\n    "
        << *RetainInst);
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

    if (isDebugInst(UseInst))
      continue;

    if (auto *AI = dyn_cast<ApplyInst>(UseInst)) {
      if (ArraySemanticsCall(AI))
        continue;

      // Check of this escape can reach the current loop.
      if (!Loop->contains(UseInst->getParent()) &&
          !getReachingBlocks().count(UseInst->getParent())) {
        continue;
      }

      DEBUG(llvm::dbgs() << "    Skipping Array: may escape through call!\n    "
            << *UseInst);
      return false;
    }

    if (auto *StInst = dyn_cast<StoreInst>(UseInst)) {
      // Allow a local array to be initialized outside the loop via a by-value
      // argument or return value. The array value may be returned by its
      // initializer or some other factory function.
      if (Loop->contains(StInst->getParent())) {
        DEBUG(llvm::dbgs() << "    Skipping Array: store inside loop!\n    "
              << *StInst);
        return false;
      }

      SILValue InitArray = StInst->getSrc();
      if (isa<SILArgument>(InitArray) || isa<ApplyInst>(InitArray))
        continue;

      DEBUG(llvm::dbgs() << "    Skipping Array: may escape through store!\n"
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

    DEBUG(llvm::dbgs() << "    Skipping Array: unknown Array use!\n    "
          << *UseInst);
    // Found an unsafe or unknown user. The Array may escape here.
    return false;
  }
  return true;
}

/// Returns true if this instruction is a safe array use if all of its users are
/// also safe array users.
static bool isTransitiveSafeUser(SILInstruction *I) {
  switch (I->getKind()) {
  case ValueKind::StructExtractInst:
  case ValueKind::TupleExtractInst:
  case ValueKind::UncheckedEnumDataInst:
  case ValueKind::StructInst:
  case ValueKind::TupleInst:
  case ValueKind::EnumInst:
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::UncheckedBitwiseCastInst:
    assert(I->hasValue() && "We assume these are unary");
    return true;
  default:
    return false;
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
      DEBUG(llvm::dbgs() << "    Skipping Array: unsafe call!\n    "
            << *UseInst);
      return false;
    }

    /// Is this a unary transitive safe user instruction. This means that the
    /// instruction is safe only if all of its users are safe. Check this
    /// recursively.
    if (isTransitiveSafeUser(UseInst)) {
      if (std::all_of(UseInst->use_begin(), UseInst->use_end(),
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
      DEBUG(llvm::dbgs() << "    Skipping Array: found unmatched retain value!\n"
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

    if (isDebugInst(UseInst))
      continue;
    
    // Found an unsafe or unknown user. The Array may escape here.
    DEBUG(llvm::dbgs() << "    Skipping Array: unsafe Array value use!\n    "
          << *UseInst);
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

  // Look for a safe mark_dependence instruction use.
  //
  // This use looks something like:
  //
  //   %57 = load %56 : $*Builtin.BridgeObject from Array<Int>
  //   %58 = unchecked_ref_cast %57 : $Builtin.BridgeObject to
  //   $_ContiguousArray
  //   %59 = unchecked_ref_cast %58 : $_ContiguousArrayStorageBase to
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

  if (isDebugInst(UseInst))
    return true;
  
  // If this is an instruction which is a safe array element use if and only if
  // all of its users are safe array element uses, recursively check its uses
  // and return false if any of them are not transitive escape array element
  // uses.
  if (isTransitiveSafeUser(UseInst)) {
    return std::all_of(UseInst->use_begin(), UseInst->use_end(),
                       [this, &ArrayVal](Operand *UI) -> bool {
                         return checkSafeArrayElementUse(UI->getUser(),
                                                         ArrayVal);
                       });
  }

  // Found an unsafe or unknown user. The Array may escape here.
  DEBUG(llvm::dbgs() << "    Skipping Array: unknown Element use!\n"
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
static bool isArrayEltStore(StoreInst *SI) {
  SILValue Dest = stripAddressProjections(SI->getDest());
  if (auto *MD = dyn_cast<MarkDependenceInst>(Dest))
    Dest = MD->getOperand(0);

  if (auto *PtrToAddr =
          dyn_cast<PointerToAddressInst>(stripAddressProjections(Dest)))
    if (auto *SEI = dyn_cast<StructExtractInst>(PtrToAddr->getOperand())) {
      ArraySemanticsCall Call(SEI->getOperand());
      if (Call && Call.getKind() == ArrayCallKind::kGetElementAddress)
        return true;
    }
  return false;
}

/// Check whether the array semantic operation could change an array value to
/// not be uniquely referenced.
///
/// Array.append for example can capture another array value.
static bool mayChangeArrayValueToNonUniqueState(ArraySemanticsCall &Call) {
  switch (Call.getKind()) {
  case ArrayCallKind::kArrayPropsIsNativeTypeChecked:
  case ArrayCallKind::kCheckSubscript:
  case ArrayCallKind::kCheckIndex:
  case ArrayCallKind::kGetCount:
  case ArrayCallKind::kGetCapacity:
  case ArrayCallKind::kGetElement:
  case ArrayCallKind::kGetArrayOwner:
  case ArrayCallKind::kGetElementAddress:
  case ArrayCallKind::kMakeMutable:
    return false;

  case ArrayCallKind::kNone:
  case ArrayCallKind::kMutateUnknown:
  case ArrayCallKind::kWithUnsafeMutableBufferPointer:
  case ArrayCallKind::kArrayInit:
  case ArrayCallKind::kArrayUninitialized:
    return true;
  }

  llvm_unreachable("Unhandled ArrayCallKind in switch.");
}

/// Check that the array value stored in \p ArrayStruct is released by \Inst.
static bool isReleaseOfArrayValueAt(AllocStackInst *ArrayStruct,
                                    SILInstruction *Inst,
                                    RCIdentityFunctionInfo *RCIA) {
  auto *SRI = dyn_cast<StrongReleaseInst>(Inst);
  if (!SRI)
   return false;
  auto Root = RCIA->getRCIdentityRoot(SRI->getOperand());
  auto *ArrayLoad = dyn_cast<LoadInst>(Root);
  if (!ArrayLoad)
    return false;

  if (ArrayLoad->getOperand() == ArrayStruct)
    return true;

  return false;
}

static bool isReleaseOfArrayValue(SILValue Array, SILInstruction *Inst,
                                  RCIdentityFunctionInfo *RCIA) {
  if (!isa<StrongReleaseInst>(Inst) && !isa<ReleaseValueInst>(Inst))
    return false;
  SILValue Root = RCIA->getRCIdentityRoot(Inst->getOperand(0));
  return Root == Array;
}

/// Check that the array value is released before a mutating operation happens.
bool COWArrayOpt::isArrayValueReleasedBeforeMutate(
    SILValue V, llvm::SmallSet<SILInstruction *, 16> &Releases) {
  AllocStackInst *ASI = nullptr;
  SILInstruction *StartInst = nullptr;
  if (V->getType().isAddress()) {
    ASI = dyn_cast<AllocStackInst>(V);
    if (!ASI)
      return false;
    StartInst = ASI;
  } else {
    StartInst = cast<SILInstruction>(V);
  }
  for (auto II = std::next(SILBasicBlock::iterator(StartInst)),
            IE = StartInst->getParent()->end();
       II != IE; ++II) {
    auto *Inst = &*II;
    // Ignore matched releases.
    if (auto SRI = dyn_cast<StrongReleaseInst>(Inst))
      if (MatchedReleases.count(&SRI->getOperandRef()))
        continue;
    if (auto RVI = dyn_cast<ReleaseValueInst>(Inst))
      if (MatchedReleases.count(&RVI->getOperandRef()))
        continue;

    if (ASI) {
      if (isReleaseOfArrayValueAt(ASI, &*II, RCIA)) {
        Releases.erase(&*II);
        return true;
      }
    } else {
      if (isReleaseOfArrayValue(V, &*II, RCIA)) {
        Releases.erase(&*II);
        return true;
      }
    }

    if (isa<RetainValueInst>(II) || isa<StrongRetainInst>(II))
      continue;

    // A side effect free instruction cannot mutate the array.
    if (!II->mayHaveSideEffects())
      continue;

    // Non mutating array calls are safe.
    if (isNonMutatingArraySemanticCall(&*II))
      continue;

    return false;
  }
  return true;
}

static SILInstruction *getInstBefore(SILInstruction *I) {
  auto It = ++I->getIterator().getReverse();
  if (I->getParent()->rend() == It)
    return nullptr;
  return &*It;
}

static SILInstruction *getInstAfter(SILInstruction *I) {
  auto It = SILBasicBlock::iterator(I);
  It = std::next(It);
  if (I->getParent()->end() == It)
    return nullptr;
  return &*It;
}

/// Strips and stores the struct_extract projections leading to the array
/// storage reference.
static SILValue
stripValueProjections(SILValue V,
                      SmallVectorImpl<SILInstruction *> &ValuePrjs) {
  while (V->getKind() == ValueKind::StructExtractInst) {
    ValuePrjs.push_back(cast<SILInstruction>(V));
    V = cast<SILInstruction>(V)->getOperand(0);
  }
  return V;
}

/// Finds the preceding check_subscript, make_mutable call or returns nil.
///
/// If we found a make_mutable call this means that check_subscript was removed
/// by the array bounds check elimination pass.
static SILInstruction *
findPrecedingCheckSubscriptOrMakeMutable(ApplyInst *GetElementAddr) {
  for (auto ReverseIt = ++GetElementAddr->getIterator().getReverse(),
            End = GetElementAddr->getParent()->rend();
       ReverseIt != End; ++ReverseIt) {
    auto Apply = dyn_cast<ApplyInst>(&*ReverseIt);
    if (!Apply)
      continue;
    ArraySemanticsCall CheckSubscript(Apply);
    if (!CheckSubscript ||
        (CheckSubscript.getKind() != ArrayCallKind::kCheckSubscript &&
         CheckSubscript.getKind() != ArrayCallKind::kMakeMutable))
      return nullptr;
    return CheckSubscript;
  }
  return nullptr;
}

/// Matches the self parameter arguments, verifies that \p Self is called and
/// stores the instructions in \p DepInsts in order.
static bool
matchSelfParameterSetup(ArraySemanticsCall Call, LoadInst *Self,
                        SmallVectorImpl<SILInstruction *> &DepInsts) {
  bool MayHaveBridgedObjectElementType = Call.mayHaveBridgedObjectElementType();
  // We only need the retain/release for the guaranteed parameter if the call
  // could release self. This can only happen if the array is backed by an
  // Objective-C array. If this is not the case we can safely hoist the call
  // without the retain/releases.
  auto *RetainArray = dyn_cast_or_null<StrongRetainInst>(getInstBefore(Call));
  if (!RetainArray && MayHaveBridgedObjectElementType)
    return false;
  auto *ReleaseArray = dyn_cast_or_null<StrongReleaseInst>(getInstAfter(Call));
  if (!ReleaseArray && MayHaveBridgedObjectElementType)
    return false;
  if (ReleaseArray && RetainArray &&
      ReleaseArray->getOperand() != RetainArray->getOperand())
    return false;

  if (ReleaseArray)
    DepInsts.push_back(ReleaseArray);
  DepInsts.push_back(Call);
  if (RetainArray)
    DepInsts.push_back(RetainArray);

  if (RetainArray) {
    auto ArrayLoad = stripValueProjections(RetainArray->getOperand(), DepInsts);
    if (ArrayLoad != Self)
      return false;
  }

  DepInsts.push_back(Self);
  return true;
}

/// Match a hoistable make_mutable call.
///
/// Precondition: The client must make sure that it is valid to actually hoist
/// the call. It must make sure that no write and no increment to the array
/// reference has happened such that hoisting is not valid.
///
/// This helper only checks that the operands computing the array reference
/// are also hoistable.
struct HoistableMakeMutable {
  SILLoop *Loop;
  bool IsHoistable;
  ApplyInst *MakeMutable;
  SmallVector<SILInstruction *, 24> DepInsts;

  HoistableMakeMutable(ArraySemanticsCall M, SILLoop *L) {
    IsHoistable = false;
    Loop = L;
    MakeMutable = M;

    // The function_ref needs to be invariant.
    if (Loop->contains(MakeMutable->getCallee()->getParentBlock()))
      return;

    // The array reference is invariant.
    if (!L->contains(M.getSelf()->getParentBlock())) {
      IsHoistable = true;
      return;
    }

    // Check whether we can hoist the dependent instructions resulting in the
    // array reference.
    if (canHoistDependentInstructions(M))
      IsHoistable = true;
  }

  /// Is this a hoistable make_mutable call.
  bool isHoistable() {
    return IsHoistable;
  }

  /// Hoist this make_mutable call and depend instructions to the preheader.
  void hoist() {
    auto *Term = Loop->getLoopPreheader()->getTerminator();
    for (auto *It : swift::reversed(DepInsts)) {
      if (It->getParent() != Term->getParent())
        It->moveBefore(Term);
    }
    MakeMutable->moveBefore(Term);
  }

private:

  /// Check whether we can hoist the dependent instructions resulting in the
  /// array reference passed to the make_mutable call.
  /// We pattern match the first dimension's array access here.
  bool canHoistDependentInstructions(ArraySemanticsCall &M) {
    // Match get_element_addr call.
    // %124 = load %3
    // %125 = struct_extract %124
    // %126 = struct_extract %125
    // %127 = struct_extract %126
    // strong_retain %127
    // %129 = apply %70(%30, %124)
    // strong_release %127
    //
    // %131 = load %73
    // %132 = unchecked_ref_cast %131
    // %133 = enum $Optional<NativeObject>, #Optional.Some!enumelt.1, %132
    // %134 = struct_extract %129
    // %135 = pointer_to_address %134 to strict $*Array<Int>
    // %136 = mark_dependence %135 on %133

    auto *MarkDependence = dyn_cast<MarkDependenceInst>(M.getSelf());
    if (!MarkDependence)
      return false;
    DepInsts.push_back(MarkDependence);

    auto *PtrToAddrArrayAddr =
        dyn_cast<PointerToAddressInst>(MarkDependence->getValue());
    if (!PtrToAddrArrayAddr)
      return false;
    DepInsts.push_back(PtrToAddrArrayAddr);

    auto *StructExtractArrayAddr =
        dyn_cast<StructExtractInst>(PtrToAddrArrayAddr->getOperand());
    if (!StructExtractArrayAddr)
      return false;
    DepInsts.push_back(StructExtractArrayAddr);

    // Check the base the array element address is dependent on.
    auto *EnumArrayAddr = dyn_cast<EnumInst>(MarkDependence->getBase());
    if (!EnumArrayAddr)
      return false;
    DepInsts.push_back(EnumArrayAddr);
    auto *UncheckedRefCast =
        dyn_cast<UncheckedRefCastInst>(EnumArrayAddr->getOperand());
    if (!UncheckedRefCast)
      return false;
    DepInsts.push_back(UncheckedRefCast);

    SILValue ArrayBuffer = stripValueProjections(UncheckedRefCast->getOperand(), DepInsts);
    auto *BaseLoad = dyn_cast<LoadInst>(ArrayBuffer);
    if (!BaseLoad ||  Loop->contains(BaseLoad->getOperand()->getParentBlock()))
      return false;
    DepInsts.push_back(BaseLoad);

    // Check the get_element_addr call.
    ArraySemanticsCall GetElementAddrCall(
        StructExtractArrayAddr->getOperand());
    if (!GetElementAddrCall ||
        GetElementAddrCall.getKind() != ArrayCallKind::kGetElementAddress)
      return false;
    if (Loop->contains(
            ((ApplyInst *)GetElementAddrCall)->getCallee()->getParentBlock()))
      return false;
    if (Loop->contains(GetElementAddrCall.getIndex()->getParentBlock()))
      return false;

    auto *GetElementAddrArrayLoad =
        dyn_cast<LoadInst>(GetElementAddrCall.getSelf());
    if (!GetElementAddrArrayLoad ||
        Loop->contains(GetElementAddrArrayLoad->getOperand()->getParentBlock()))
      return false;

    // Check the retain/release around the get_element_addr call.
    if (!matchSelfParameterSetup(GetElementAddrCall, GetElementAddrArrayLoad,
                                 DepInsts))
      return false;

    // Check check_subscript.
    // %116 = load %3
    // %118 = struct_extract %116
    // %119 = struct_extract %118
    // %120 = struct_extract %119
    // strong_retain %120
    // %122 = apply %23(%30, %69, %116)
    // strong_release %120
    //
    // Find the check_subscript call.
    auto *Check = findPrecedingCheckSubscriptOrMakeMutable(GetElementAddrCall);
    if (!Check)
      return false;

    ArraySemanticsCall CheckSubscript(Check);
    // The check_subscript call was removed.
    if (CheckSubscript.getKind() == ArrayCallKind::kMakeMutable)
      return true;

    if (Loop->contains(CheckSubscript.getIndex()->getParentBlock()) ||
        Loop->contains(CheckSubscript.getArrayPropertyIsNativeTypeChecked()
                           ->getParentBlock()))
      return false;

    auto *CheckSubscriptArrayLoad =
        dyn_cast<LoadInst>(CheckSubscript.getSelf());
    if (!CheckSubscript ||
        Loop->contains(CheckSubscriptArrayLoad->getOperand()->getParentBlock()))
      return false;
  if (Loop->contains(
            ((ApplyInst *)CheckSubscript)->getCallee()->getParentBlock()))
      return false;

    // The array must match get_element_addr's array.
    if (CheckSubscriptArrayLoad->getOperand() !=
        GetElementAddrArrayLoad->getOperand())
      return false;

    // Check the retain/release around the check_subscript call.
    if (!matchSelfParameterSetup(CheckSubscript, CheckSubscriptArrayLoad,
                                 DepInsts))
      return false;

    return true;
  }
};

/// Prove that there are not array value mutating or capturing operations in the
/// loop and hoist make_mutable.
bool COWArrayOpt::hoistInLoopWithOnlyNonArrayValueMutatingOperations() {
  // Only handle innermost loops.
  assert(Loop->getSubLoops().empty() && "Only works in innermost loops");

  DEBUG(llvm::dbgs() << "    Checking whether loop only has only non array "
                        "value mutating operations ...\n");

  // There is no current array addr value.
  CurrentArrayAddr = SILValue();

  // We need to cleanup the MatchedRelease on return.
  auto ReturnWithCleanup = [&] (bool LoopHasSafeOperations) {
    MatchedReleases.clear();
    return LoopHasSafeOperations;
  };

  llvm::SmallSet<SILValue, 16> ArrayValues;
  llvm::SmallSetVector<SILValue, 16> CreatedNonTrivialValues;
  llvm::SmallSet<SILInstruction *, 16> Releases;
  llvm::SmallVector<ArraySemanticsCall, 8> MakeMutableCalls;


  /// Make sure that no writes to an array value happens in the loop and that
  /// no array values are retained without being released before hitting a
  /// make_unique:
  ///
  /// * array semantic functions that don't change the uniqueness state to
  ///   non-unique are safe.
  /// * retains must be matched by a release before hitting a mutating operation
  /// * stores must not store an array value (only trivial stores are safe).
  ///
  auto &Module = Function->getModule();
  for (auto *BB : Loop->getBlocks()) {
    for (auto &InstIt : *BB) {
      auto *Inst = &InstIt;
      ArraySemanticsCall Sem(Inst);
      if (Sem) {
        // Give up if the array semantic function might change the uniqueness
        // state of an array value in the loop. An example of such an operation
        // would be append. We also give up for array initializations.
        if (mayChangeArrayValueToNonUniqueState(Sem))
          return ReturnWithCleanup(false);

        // Collect both the value and the pointer.
        ArrayValues.insert(Sem.getSelf());
        if (auto *LI = dyn_cast<LoadInst>(Sem.getSelf()))
          ArrayValues.insert(LI->getOperand());

        // Collect non-trivial generated values. This could be an array value.
        // We must make sure that any non-trivial generated values (== +1)
        // are release before we hit a make_unique instruction.
        ApplyInst *SemCall = Sem;
        if (Sem.getKind() == ArrayCallKind::kGetElement) {
          SILValue Elem = (Sem.hasGetElementDirectResult() ? Inst :
                           SemCall->getArgument(0));
          if (!Elem->getType().isTrivial(Module))
            CreatedNonTrivialValues.insert(Elem);
        } else if (Sem.getKind() == ArrayCallKind::kMakeMutable) {
          MakeMutableCalls.push_back(Sem);
        }

        continue;
      }

      // Instructions without side effects are safe.
      if (!Inst->mayHaveSideEffects())
        continue;
      if (isa<CondFailInst>(Inst))
        continue;
      if (isa<AllocationInst>(Inst) || isa<DeallocStackInst>(Inst))
        continue;

      // A retain must be released before make_unique.
      if (isa<RetainValueInst>(Inst) ||
          isa<StrongRetainInst>(Inst)) {
        if (!isRetainReleasedBeforeMutate(Inst, false)) {
          DEBUG(llvm::dbgs() << "    (NO) retain not released before mutate " << *Inst);
          return ReturnWithCleanup(false);
        }
        continue;
      }
      // A store is only safe if it is to an array element and the element type
      // is trivial.
      if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
        if (!isArrayEltStore(SI) ||
            !SI->getSrc()->getType().isTrivial(Module)) {
          DEBUG(llvm::dbgs()
                << "     (NO) non trivial store could store an array value "
                << *Inst);
          return ReturnWithCleanup(false);
        }
        continue;
      }
      // Releases must be matched by a retain otherwise a destructor could run.
      if (auto SRI = dyn_cast<StrongReleaseInst>(Inst)) {
        if (!MatchedReleases.count(&SRI->getOperandRef()))
          Releases.insert(Inst);
        continue;
      }
      if (auto RVI = dyn_cast<ReleaseValueInst>(Inst)) {
        if (!MatchedReleases.count(&RVI->getOperandRef()))
          Releases.insert(Inst);
        continue;
      }

      DEBUG(llvm::dbgs() << "    (NO) instruction prevents make_unique hoisting "
                         << *Inst);
      return ReturnWithCleanup(false);
    }
  }

  // Nothing to do.
  if (MakeMutableCalls.empty())
    return ReturnWithCleanup(false);

  // Verify that all created non trivial values are array values and that they
  // are released before mutation.
  for (auto &NonTrivial : CreatedNonTrivialValues) {
    if (!ArrayValues.count(NonTrivial)) {
      DEBUG(llvm::dbgs() << "    (NO) non-trivial non-array value: " << NonTrivial);
      return ReturnWithCleanup(false);
    }

    if (!isArrayValueReleasedBeforeMutate(NonTrivial, Releases)) {
      DEBUG(llvm::dbgs() << "    (NO) array value not released before mutation "
                         << NonTrivial);
      return ReturnWithCleanup(false);
    }
  }

  if (!Releases.empty()) {
    DEBUG(llvm::dbgs() << "    (NO) remaining releases not matched by retain\n");
    return ReturnWithCleanup(false);
  }

  // Collect all recursively hoistable calls.
  SmallVector<std::unique_ptr<HoistableMakeMutable>, 16> CallsToHoist;
  for (auto M : MakeMutableCalls) {
    auto Call = llvm::make_unique<HoistableMakeMutable>(M, Loop);
    if (!Call->isHoistable()) {
      DEBUG(llvm::dbgs() << "    (NO) make_mutable not hoistable"
                         << *Call->MakeMutable);
      return ReturnWithCleanup(false);
    }
    CallsToHoist.push_back(std::move(Call));
  }

  for (auto &Call: CallsToHoist)
    Call->hoist();

  DEBUG(llvm::dbgs() << "    Hoisting make_mutable in " << Function->getName()
                     << "\n");
  return ReturnWithCleanup(true);
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

  DEBUG(llvm::dbgs() << "    checking whether loop only has safe array operations ...\n");
  CanType SameTy;
  for (auto *BB : Loop->getBlocks()) {
    for (auto &It : *BB) {
      auto *Inst = &It;
      DEBUG(llvm::dbgs() << "        visiting: " << *Inst);

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
          SameTy = Sem.getSelf()->getType().getSwiftRValueType();
          continue;
        }
        
        if (Sem.getSelf()->getType().getSwiftRValueType() != SameTy) {
          DEBUG(llvm::dbgs() << "    (NO) mismatching array types\n");
          return ReturnWithCleanup(false);
        }

        // Safe array semantics operation.
        continue;
      }

      // Stores to array elements.
      if (auto *SI = dyn_cast<StoreInst>(Inst)) {
        if (isArrayEltStore(SI))
          continue;
        DEBUG(llvm::dbgs() << "     (NO) unknown store " << *SI);
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

      DEBUG(llvm::dbgs() << "     (NO) unknown operation " << *Inst);
      return ReturnWithCleanup(false);
    }
  }

  DEBUG(llvm::dbgs() << "     (YES)\n");
  CachedSafeLoop.second = true;
  return ReturnWithCleanup(true);
}

/// Hoist the make_mutable call and optionally the projection chain that feeds
/// the array self argument.
void COWArrayOpt::hoistMakeMutableAndSelfProjection(
    ArraySemanticsCall MakeMutable, bool HoistProjection) {
  // Hoist projections.
  if (HoistProjection)
    hoistAddressProjections(MakeMutable.getSelfOperand(),
      Preheader->getTerminator(), DomTree);

  assert(MakeMutable.canHoist(Preheader->getTerminator(), DomTree) &&
         "Should be able to hoist make_mutable");

  // Hoist this call to make_mutable.
  DEBUG(llvm::dbgs() << "    Hoisting make_mutable: " << *MakeMutable);
  MakeMutable.hoist(Preheader->getTerminator(), DomTree);
}

/// Check if this call to "make_mutable" is hoistable, and move it, or delete it
/// if it's already hoisted.
bool COWArrayOpt::hoistMakeMutable(ArraySemanticsCall MakeMutable) {
  DEBUG(llvm::dbgs() << "    Checking mutable array: " << CurrentArrayAddr);

  // We can hoist address projections (even if they are only conditionally
  // executed).
  auto ArrayAddrBase = stripUnaryAddressProjections(CurrentArrayAddr);
  SILBasicBlock *ArrayAddrBaseBB = ArrayAddrBase->getParentBlock();

  if (ArrayAddrBaseBB && !DomTree->dominates(ArrayAddrBaseBB, Preheader)) {
    DEBUG(llvm::dbgs() << "    Skipping Array: does not dominate loop!\n");
    return false;
  }

  SmallVector<unsigned, 4> AccessPath;
  SILValue ArrayContainer = getAccessPath(CurrentArrayAddr, AccessPath);

  // Check whether we can hoist make_mutable based on the operations that are
  // in the loop.
  if (hasLoopOnlyDestructorSafeArrayOperations()) {
    hoistMakeMutableAndSelfProjection(MakeMutable,
                                      CurrentArrayAddr != ArrayAddrBase);
    DEBUG(llvm::dbgs()
          << "    Can Hoist: loop only has 'safe' array operations!\n");
    return true;
  }

  // Check that the array is a member of an inout argument or return value.
  if (!checkUniqueArrayContainer(ArrayContainer)) {
    DEBUG(llvm::dbgs() << "    Skipping Array: is not unique!\n");
    return false;
  }

  // Check that the Array is not retained with this loop and it's address does
  // not escape within this function.
  StructUseCollector StructUses;
  StructUses.collectUses(ArrayContainer, AccessPath);
  for (auto *Oper : StructUses.Visited)
    ArrayUserSet.insert(Oper->getUser());

  if (!checkSafeArrayAddressUses(StructUses.AggregateAddressUsers) ||
      !checkSafeArrayAddressUses(StructUses.StructAddressUsers) ||
      !checkSafeArrayValueUses(StructUses.StructValueUsers) ||
      !checkSafeElementValueUses(StructUses.ElementValueUsers) ||
      !StructUses.ElementAddressUsers.empty())
    return false;

  hoistMakeMutableAndSelfProjection(MakeMutable,
                                    CurrentArrayAddr != ArrayAddrBase);
  return true;
}

bool COWArrayOpt::run() {
  DEBUG(llvm::dbgs() << "  Array Opts in Loop " << *Loop);

  Preheader = Loop->getLoopPreheader();
  if (!Preheader) {
    DEBUG(llvm::dbgs() << "    Skipping Loop: No Preheader!\n");
    return false;
  }

  // Hoist make_mutable in two dimensional arrays if there are no array value
  // mutating operations in the loop.
  if (Loop->getSubLoops().empty() &&
      hoistInLoopWithOnlyNonArrayValueMutatingOperations()) {
    return true;
  }

  for (auto *BB : Loop->getBlocks()) {
    if (ColdBlocks.isCold(BB))
      continue;
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
        if (!hoistMakeMutable(MakeMutableCall)) {
          ArrayMakeMutableMap[CurrentArrayAddr] = nullptr;
          continue;
        }

        ArrayMakeMutableMap[CurrentArrayAddr] = MakeMutableCall;
        HasChanged = true;
        continue;
      }

      if (!HoistedCallEntry->second)
        continue;

      DEBUG(llvm::dbgs() << "    Removing make_mutable call: " << *MakeMutableCall);
      MakeMutableCall.removeCall();
      HasChanged = true;
    }
  }
  return HasChanged;
}

namespace {

class COWArrayOptPass : public SILFunctionTransform {
  void run() override {
    DEBUG(llvm::dbgs() << "COW Array Opts in Func " << getFunction()->getName()
          << "\n");

    auto *DA = PM->getAnalysis<DominanceAnalysis>();
    auto *LA = PM->getAnalysis<SILLoopAnalysis>();
    auto *RCIA =
      PM->getAnalysis<RCIdentityAnalysis>()->get(getFunction());
    SILLoopInfo *LI = LA->get(getFunction());
    if (LI->empty()) {
      DEBUG(llvm::dbgs() << "  Skipping Function: No loops.\n");
      return;
    }

#ifndef NDEBUG
    if (!COWViewCFGFunction.empty() && getFunction()->getName() == COWViewCFGFunction) {
      getFunction()->dump();
      getFunction()->viewCFG();
    }
#endif

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

      if (HasChanged) {
        invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
      }
  }

  StringRef getName() override { return "SIL COW Array Optimization"; }
};
} // end anonymous namespace

SILTransform *swift::createCOWArrayOpts() {
  return new COWArrayOptPass();
}

namespace {

/// This optimization specializes loops with calls to
/// "array.props.isNative/needsElementTypeCheck".
///
/// The "array.props.isNative/needsElementTypeCheck" predicate has the property
/// that if it is true/false respectively for the array struct it is true/false
/// respectively until somebody writes a new array struct over the memory
/// location. Less abstractly, a fast native swift array does not transition to
/// a slow array (be it a cocoa array, or be it an array that needs type
/// checking) except if we store a new array to the variable that holds it.
///
/// Using this property we can hoist the predicate above a region where no such
/// store can take place.
///
///  func f(a : A[AClass]) {
///     for i in 0..a.count {
///       let b = a.props.isNative()
///        .. += _getElement(i, b)
///     }
///  }
///
///   ==>
///
///  func f(a : A[AClass]) {
///    let b = a.props.isNative
///    if (b) {
///      for i in 0..a.count {
///         .. += _getElement(i, false)
///      }
///    } else {
///      for i in 0..a.count {
///        let a = a.props.isNative
///        .. += _getElement(i, a)
///      }
///    }
///  }
///
static llvm::cl::opt<bool> ShouldSpecializeArrayProps("sil-array-props",
                                                      llvm::cl::init(true));

/// Analysis whether it is safe to specialize this loop nest based on the
/// array.props function calls it contains. It is safe to hoist array.props
/// calls if the array does not escape such that the array container could be
/// overwritten in the hoisted region.
/// This analysis also checks if we can clone the instructions in the loop nest.
class ArrayPropertiesAnalysis {
  using UserList = StructUseCollector::UserList;
  using UserOperList = StructUseCollector::UserOperList;

  SILFunction *Fun;
  SILLoop *Loop;
  SILBasicBlock *Preheader;
  DominanceInfo *DomTree;

  llvm::SmallSet<SILValue, 16> HoistableArray;

  SmallPtrSet<SILBasicBlock *, 16> ReachingBlocks;
  SmallPtrSet<SILBasicBlock *, 16> CachedExitingBlocks;
public:
  ArrayPropertiesAnalysis(SILLoop *L, DominanceAnalysis *DA)
      : Fun(L->getHeader()->getParent()), Loop(L), Preheader(nullptr),
        DomTree(DA->get(Fun)) {}

  bool run() {
    Preheader = Loop->getLoopPreheader();
    if (!Preheader) {
      DEBUG(llvm::dbgs() << "ArrayPropertiesAnalysis: Missing preheader for " << *Loop);
      return false;
    }

    // Check whether this is a 'array.props' instruction and whether we
    // can hoist it. Heuristic: We only want to hoist array.props instructions
    // if we can hoist all of them - only then can we get rid of all the
    // control-flow if we specialize. Hoisting some but not others is not as
    // beneficial. This heuristic also simplifies which regions we want to
    // specialize on. We will specialize the outermost loopnest that has
    // 'array.props' instructions in its preheader.
    bool FoundHoistable = false;
    for (auto *BB : Loop->getBlocks()) {
      for (auto &Inst : *BB) {

        // Can't clone alloc_stack instructions whose dealloc_stack is outside
        // the loop.
        if (!Loop->canDuplicate(&Inst))
          return false;

        ArraySemanticsCall ArrayPropsInst(&Inst, "array.props", true);
        if (!ArrayPropsInst)
          continue;

        if (!canHoistArrayPropsInst(ArrayPropsInst))
          return false;
        FoundHoistable = true;
      }
    }

    return FoundHoistable;
  }

private:

  /// Strip the struct load and the address projection to the location
  /// holding the array struct.
  SILValue stripArrayStructLoad(SILValue V) {
    if (auto LI = dyn_cast<LoadInst>(V)) {
      auto Val = LI->getOperand();
      // We could have two arrays in a surrounding container so we can only
      // strip off the 'array struct' project.
      // struct Container {
      //   var a1 : [ClassA]
      //   var a2 : [ClassA]
      // }
      // 'a1' and 'a2' are different arrays.
      if (auto SEAI = dyn_cast<StructElementAddrInst>(Val))
        Val = SEAI->getOperand();
      return Val;
    }
    return V;
  }

  SmallPtrSetImpl<SILBasicBlock *> &getReachingBlocks() {
    if (ReachingBlocks.empty()) {
      SmallVector<SILBasicBlock *, 8> Worklist;
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

  /// Array address uses are safe if they don't store to the array struct. We
  /// could for example store an NSArray array struct on top of the array. For
  /// example, an opaque function that uses the array's address could store a
  /// new array onto it.
  bool checkSafeArrayAddressUses(UserList &AddressUsers) {
    for (auto *UseInst : AddressUsers) {

      if (isDebugInst(UseInst))
        continue;

      if (isa<DeallocStackInst>(UseInst)) {
        // Handle destruction of a local array.
        continue;
      }

      if (auto *AI = dyn_cast<ApplyInst>(UseInst)) {
        if (ArraySemanticsCall(AI))
          continue;

        // Check if this escape can reach the current loop.
        if (!Loop->contains(UseInst->getParent()) &&
            !getReachingBlocks().count(UseInst->getParent())) {
          continue;
        }
        DEBUG(
            llvm::dbgs() << "    Skipping Array: may escape through call!\n    "
                         << *UseInst);
        return false;
      }

      if (auto *StInst = dyn_cast<StoreInst>(UseInst)) {
        // Allow a local array to be initialized outside the loop via a by-value
        // argument or return value. The array value may be returned by its
        // initializer or some other factory function.
        if (Loop->contains(StInst->getParent())) {
          DEBUG(llvm::dbgs() << "    Skipping Array: store inside loop!\n    "
                             << *StInst);
          return false;
        }
        SILValue InitArray = StInst->getSrc();
        if (isa<SILArgument>(InitArray) || isa<ApplyInst>(InitArray))
          continue;

        return false;
      }

      DEBUG(llvm::dbgs() << "    Skipping Array: unknown Array use!\n    "
                         << *UseInst);
      // Found an unsafe or unknown user. The Array may escape here.
      return false;
    }

    // Otherwise, all of our users are sane. The array does not escape.
    return true;
  }

  /// Value uses are generally safe. We can't change the state of an array
  /// through a value use.
  bool checkSafeArrayValueUses(UserList &ValueUsers) {
    return true;
  }
  bool checkSafeElementValueUses(UserOperList &ElementValueUsers) {
    return true;
  }

  // We have a safe container if the array container is passed as a function
  // argument by-value or by inout reference. In either case there can't be an
  // alias of the container. Alternatively, we can have a local variable. We
  // will check in checkSafeArrayAddressUses that all initialization stores to
  // this variable are safe (i.e the store dominates the loop etc).
  bool isSafeArrayContainer(SILValue V) {
    if (auto *Arg = dyn_cast<SILArgument>(V)) {
      // Check that the argument is passed as an inout or by value type. This
      // means there are no aliases accessible within this function scope.
      auto Params = Fun->getLoweredFunctionType()->getParameters();
      ArrayRef<SILArgument *> FunctionArgs = Fun->begin()->getArguments();
      for (unsigned ArgIdx = 0, ArgEnd = Params.size(); ArgIdx != ArgEnd;
           ++ArgIdx) {
        if (FunctionArgs[ArgIdx] != Arg)
          continue;

        if (!Params[ArgIdx].isIndirectInOut()
            && Params[ArgIdx].isFormalIndirect()) {
          DEBUG(llvm::dbgs()
                << "    Skipping Array: Not an inout or by val argument!\n");
          return false;
        }
      }
      return true;
    } else if (isa<AllocStackInst>(V))
      return true;

    DEBUG(llvm::dbgs()
          << "    Skipping Array: Not a know array container type!\n");

    return false;
  }

  SmallPtrSetImpl<SILBasicBlock *> &getLoopExitingBlocks() {
    if (!CachedExitingBlocks.empty())
      return CachedExitingBlocks;
    SmallVector<SILBasicBlock *, 16> ExitingBlocks;
    Loop->getExitingBlocks(ExitingBlocks);
    CachedExitingBlocks.insert(ExitingBlocks.begin(), ExitingBlocks.end());
    return CachedExitingBlocks;
  }

  bool isConditionallyExecuted(ArraySemanticsCall Call) {
    auto CallBB = (*Call).getParent();
    for (auto *ExitingBlk : getLoopExitingBlocks())
      if (!DomTree->dominates(CallBB, ExitingBlk))
          return true;
    return false;
  }

  bool isClassElementTypeArray(SILValue Arr) {
    auto Ty = Arr->getType();
    if (auto BGT = Ty.getAs<BoundGenericStructType>()) {
      // Check the array element type parameter.
      bool isClass = false;
      for (auto EltTy : BGT->getGenericArgs()) {
        if (!EltTy->hasReferenceSemantics())
          return false;
        isClass = true;
      }
      return isClass;
    }
    return false;
  }

  bool canHoistArrayPropsInst(ArraySemanticsCall Call) {
    // TODO: This is way conservative. If there is an unconditionally
    // executed call to the same array we can still hoist it.
    if (isConditionallyExecuted(Call))
      return false;

    SILValue Arr = Call.getSelf();

    // We don't attempt to hoist non-class element type arrays.
    if (!isClassElementTypeArray(Arr))
      return false;

    // We can strip the load that might even occur in the loop because we make
    // sure that no unsafe store to the array's address takes place.
    Arr = stripArrayStructLoad(Arr);

    // Have we already seen this array and deemed it safe?
    if (HoistableArray.count(Arr))
      return true;

    // Do we know how to hoist the arguments of this call.
    if (!Call.canHoist(Preheader->getTerminator(), DomTree))
      return false;

    SmallVector<unsigned, 4> AccessPath;
    SILValue ArrayContainer = getAccessPath(Arr, AccessPath);

    if (!isSafeArrayContainer(ArrayContainer))
      return false;

    StructUseCollector StructUses;
    StructUses.collectUses(ArrayContainer, AccessPath);

    if (!checkSafeArrayAddressUses(StructUses.AggregateAddressUsers) ||
        !checkSafeArrayAddressUses(StructUses.StructAddressUsers) ||
        !checkSafeArrayValueUses(StructUses.StructValueUsers) ||
        !checkSafeElementValueUses(StructUses.ElementValueUsers) ||
        !StructUses.ElementAddressUsers.empty())
    return false;

    HoistableArray.insert(Arr);
    return true;
  }
};
} // end anonymous namespace

namespace {
/// Clone a single exit multiple exit region starting at basic block and ending
/// in a set of basic blocks. Updates the dominator tree with the cloned blocks.
/// However, the client needs to update the dominator of the exit blocks.
class RegionCloner : public SILCloner<RegionCloner> {
  DominanceInfo &DomTree;
  SILBasicBlock *StartBB;
  SmallPtrSet<SILBasicBlock *, 16> OutsideBBs;

  friend class SILVisitor<RegionCloner>;
  friend class SILCloner<RegionCloner>;

public:
  RegionCloner(SILBasicBlock *EntryBB,
               SmallVectorImpl<SILBasicBlock *> &ExitBlocks, DominanceInfo &DT)
      : SILCloner<RegionCloner>(*EntryBB->getParent()), DomTree(DT),
        StartBB(EntryBB), OutsideBBs(ExitBlocks.begin(), ExitBlocks.end()) {}

  SILBasicBlock *cloneRegion() {
    assert (DomTree.getNode(StartBB) != nullptr && "Can't cloned dead code");
    auto CurFun = StartBB->getParent();

    // We don't want to visit blocks outside of the region. visitSILBasicBlocks
    // checks BBMap before it clones a block. So we mark exiting blocks as
    // visited by putting them in the BBMap.
    for (auto *BB : OutsideBBs)
      BBMap[BB] = BB;

    // We need to split any edge from a non cond_br basic block leading to a
    // exit block. After cloning this edge will become critical if it came from
    // inside the cloned region. The SSAUpdater can't handle critical non
    // cond_br edges.
    for (auto *BB : OutsideBBs) {
      SmallVector<SILBasicBlock *, 8> Preds(BB->getPredecessorBlocks());
      for (auto *Pred : Preds)
        if (!isa<CondBranchInst>(Pred->getTerminator()) &&
            !isa<BranchInst>(Pred->getTerminator()))
          splitEdgesFromTo(Pred, BB, &DomTree, nullptr);
    }

    // Create the cloned start basic block.
    auto *ClonedStartBB = CurFun->createBasicBlock();
    BBMap[StartBB] = ClonedStartBB;

    // Clone the arguments.
    for (auto &Arg : StartBB->getArguments()) {
      SILValue MappedArg = ClonedStartBB->createPHIArgument(
          getOpType(Arg->getType()), ValueOwnershipKind::Owned);
      ValueMap.insert(std::make_pair(Arg, MappedArg));
    }

    // Clone the instructions in this basic block and recursively clone
    // successor blocks.
    getBuilder().setInsertionPoint(ClonedStartBB);
    visitSILBasicBlock(StartBB);

    // Fix-up terminators.
    for (auto BBPair : BBMap)
      if (BBPair.first != BBPair.second) {
        getBuilder().setInsertionPoint(BBPair.second);
        visit(BBPair.first->getTerminator());
      }

    // Add dominator tree nodes for the new basic blocks.
    fixDomTreeNodes(DomTree.getNode(StartBB));

    // Update SSA form for values used outside of the copied region.
    updateSSAForm();
    return ClonedStartBB;
  }

  llvm::MapVector<SILBasicBlock *, SILBasicBlock *> &getBBMap() { return BBMap; }

protected:
  /// Clone the dominator tree from the original region to the cloned region.
  void fixDomTreeNodes(DominanceInfoNode *OrigNode) {
    auto *BB = OrigNode->getBlock();
    auto MapIt = BBMap.find(BB);
    // Outside the cloned region.
    if (MapIt == BBMap.end())
      return;

    auto *ClonedBB = MapIt->second;
    // Exit blocks (BBMap[BB] == BB) end the recursion.
    if (ClonedBB == BB)
      return;

    auto *OrigDom = OrigNode->getIDom();
    assert(OrigDom);

    if (BB == StartBB) {
      // The cloned start node shares the same dominator as the original node.
      auto *ClonedNode = DomTree.addNewBlock(ClonedBB, OrigDom->getBlock());
      (void) ClonedNode;
      assert(ClonedNode);
    } else {
      // Otherwise, map the dominator structure using the mapped block.
      auto *OrigDomBB = OrigDom->getBlock();
      assert(BBMap.count(OrigDomBB) && "Must have visited dominating block");
      auto *MappedDomBB = BBMap[OrigDomBB];
      assert(MappedDomBB);
      DomTree.addNewBlock(ClonedBB, MappedDomBB);
    }

    for (auto *Child : *OrigNode)
      fixDomTreeNodes(Child);
  }

  SILValue remapValue(SILValue V) {
    if (auto *BB = V->getParentBlock()) {
      if (!DomTree.dominates(StartBB, BB)) {
        // Must be a value that dominates the start basic block.
        assert(DomTree.dominates(BB, StartBB) &&
               "Must dominated the start of the cloned region");
        return V;
      }
    }
    return SILCloner<RegionCloner>::remapValue(V);
  }

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    SILCloner<RegionCloner>::postProcess(Orig, Cloned);
  }

  /// Update SSA form for values that are used outside the region.
  void updateSSAForValue(SILBasicBlock *OrigBB, SILValue V,
                         SILSSAUpdater &SSAUp) {
    // Collect outside uses.
    SmallVector<UseWrapper, 16> UseList;
    for (auto Use : V->getUses())
      if (OutsideBBs.count(Use->getUser()->getParent()) ||
          !BBMap.count(Use->getUser()->getParent())) {
        UseList.push_back(UseWrapper(Use));
      }
    if (UseList.empty())
      return;

    // Update SSA form.
    SSAUp.Initialize(V->getType());
    SSAUp.AddAvailableValue(OrigBB, V);
    SILValue NewVal = remapValue(V);
    SSAUp.AddAvailableValue(BBMap[OrigBB], NewVal);
    for (auto U : UseList) {
      Operand *Use = U;
      SSAUp.RewriteUse(*Use);
    }
  }

  void updateSSAForm() {
    SILSSAUpdater SSAUp;
    for (auto Entry : BBMap) {
      // Ignore exit blocks.
      if (Entry.first == Entry.second)
        continue;
      auto *OrigBB = Entry.first;

      // Update outside used phi values.
      for (auto *Arg : OrigBB->getArguments())
        updateSSAForValue(OrigBB, Arg, SSAUp);

      // Update outside used instruction values.
      for (auto &Inst : *OrigBB) {
        updateSSAForValue(OrigBB, &Inst, SSAUp);
      }
    }
  }
};
} // end anonymous namespace

namespace {
/// This class transforms a hoistable loop nest into a speculatively specialized
/// loop based on array.props calls.
class ArrayPropertiesSpecializer {
  DominanceInfo *DomTree;
  SILLoopAnalysis *LoopAnalysis;
  SILBasicBlock *HoistableLoopPreheader;

public:
  ArrayPropertiesSpecializer(DominanceInfo *DT, SILLoopAnalysis *LA,
                             SILBasicBlock *Hoistable)
      : DomTree(DT), LoopAnalysis(LA), HoistableLoopPreheader(Hoistable) {}

  void run() {
    specializeLoopNest();
  }

  SILLoop *getLoop() {
    auto *LoopInfo = LoopAnalysis->get(HoistableLoopPreheader->getParent());
    return LoopInfo->getLoopFor(
        HoistableLoopPreheader->getSingleSuccessorBlock());
  }

protected:
  void specializeLoopNest();
};
} // end anonymous namespace

static SILValue createStructExtract(SILBuilder &B, SILLocation Loc,
                                    SILValue Opd, unsigned FieldNo) {
  SILType Ty = Opd->getType();
  auto SD = Ty.getStructOrBoundGenericStruct();
  auto Properties = SD->getStoredProperties();
  unsigned Counter = 0;
  for (auto *D : Properties)
    if (Counter++ == FieldNo)
      return B.createStructExtract(Loc, Opd, D);
  llvm_unreachable("Wrong field number");
}

static Identifier getBinaryFunction(StringRef Name, SILType IntSILTy,
                                    ASTContext &C) {
  auto IntTy = IntSILTy.castTo<BuiltinIntegerType>();
  unsigned NumBits = IntTy->getWidth().getFixedWidth();
  // Name is something like: add_Int64
  std::string NameStr = Name;
  NameStr += "_Int" + llvm::utostr(NumBits);
  return C.getIdentifier(NameStr);
}

/// Create a binary and function.
static SILValue createAnd(SILBuilder &B, SILLocation Loc, SILValue Opd1,
                          SILValue Opd2) {
  auto AndFn = getBinaryFunction("and", Opd1->getType(), B.getASTContext());
  SILValue Args[] = {Opd1, Opd2};
  return B.createBuiltin(Loc, AndFn, Opd1->getType(), {}, Args);
}

/// Create a check over all array.props calls that they have the 'fast native
/// swift' array value: isNative && !needsElementTypeCheck must be true.
static SILValue
createFastNativeArraysCheck(SmallVectorImpl<ArraySemanticsCall> &ArrayProps,
                            SILBuilder &B) {
  assert(!ArrayProps.empty() && "Must have array.pros calls");

  SILType IntBoolTy = SILType::getBuiltinIntegerType(1, B.getASTContext());
  SILValue Result =
      B.createIntegerLiteral((*ArrayProps[0]).getLoc(), IntBoolTy, 1);

  for (auto Call : ArrayProps) {
    auto Loc = (*Call).getLoc();
    auto CallKind = Call.getKind();
    if (CallKind == ArrayCallKind::kArrayPropsIsNativeTypeChecked) {
      auto Val = createStructExtract(B, Loc, SILValue(Call), 0);
      Result = createAnd(B, Loc, Result, Val);
    }
  }
  return Result;
}

/// Collect all array.props calls in the cloned basic blocks stored in the map,
/// asserting that we found at least one.
static void collectArrayPropsCalls(
    llvm::MapVector<SILBasicBlock *, SILBasicBlock *> &OrigToClonedBBMap,
    SmallVectorImpl<SILBasicBlock *> &ExitBlocks,
    SmallVectorImpl<ArraySemanticsCall> &Calls) {
  for (auto &P : OrigToClonedBBMap) {
    // Collect array.props calls in all cloned blocks, excluding the exit
    // blocks.
    if (std::find(ExitBlocks.begin(), ExitBlocks.end(), P.second) ==
        ExitBlocks.end())
      for (auto &Inst : *P.second) {
        ArraySemanticsCall ArrayProps(&Inst, "array.props", true);
        if (!ArrayProps)
          continue;
        Calls.push_back(ArrayProps);
      }
  }
  assert(!Calls.empty() && "Should have a least one array.props call");
}

/// Replace an array.props call by the 'fast swift array' value.
///
/// This is true for array.props.isNative and false for
/// array.props.needsElementTypeCheck.
static void replaceArrayPropsCall(SILBuilder &B, ArraySemanticsCall C) {
  assert(C.getKind() == ArrayCallKind::kArrayPropsIsNativeTypeChecked);
  ApplyInst *AI = C;

  SILType IntBoolTy = SILType::getBuiltinIntegerType(1, B.getASTContext());

  auto BoolTy = AI->getType();
  auto C0 = B.createIntegerLiteral(AI->getLoc(), IntBoolTy, 1);
  auto BoolVal = B.createStruct(AI->getLoc(), BoolTy, {C0});

  (*C).replaceAllUsesWith(BoolVal);
    // Remove call to array.props.read/write.
  C.removeCall();
}

void ArrayPropertiesSpecializer::specializeLoopNest() {
  auto *Lp = getLoop();
  assert(Lp);

  // Split of a new empty preheader. We don't want to duplicate the whole
  // original preheader it might contain instructions that we can't clone.
  // This will be block that will contain the check whether to execute the
  // 'native swift array' loop or the original loop.
  SILBuilder B(HoistableLoopPreheader);
  auto *CheckBlock = splitBasicBlockAndBranch(B,
      HoistableLoopPreheader->getTerminator(), DomTree, nullptr);

  // Get the exit blocks of the original loop.
  auto *Header = CheckBlock->getSingleSuccessorBlock();
  assert(Header);

  // Our loop info is not really completely valid anymore since the cloner does
  // not update it. However, exit blocks of the original loop are still valid.
  SmallVector<SILBasicBlock *, 16> ExitBlocks;
  Lp->getExitBlocks(ExitBlocks);

  // Collect the exit blocks dominated by the loop - they will be dominated by
  // the check block.
  SmallVector<SILBasicBlock *, 16> ExitBlocksDominatedByPreheader;
  for (auto *ExitBlock: ExitBlocks)
    if (DomTree->dominates(CheckBlock, ExitBlock))
      ExitBlocksDominatedByPreheader.push_back(ExitBlock);

  // Split the preheader before the first instruction.
  SILBasicBlock *NewPreheader =
    splitBasicBlockAndBranch(B, &*CheckBlock->begin(), DomTree, nullptr);

  // Clone the region from the new preheader up to (not including) the exit
  // blocks. This creates a second loop nest.
  RegionCloner Cloner(NewPreheader, ExitBlocks, *DomTree);
  auto *ClonedPreheader = Cloner.cloneRegion();

  // Collect the array.props call that we will specialize on that we have
  // cloned in the cloned loop.
  SmallVector<ArraySemanticsCall, 16> ArrayPropCalls;
  collectArrayPropsCalls(Cloner.getBBMap(), ExitBlocks, ArrayPropCalls);

  // Move them to the check block.
  SmallVector<ArraySemanticsCall, 16> HoistedArrayPropCalls;
  for (auto C: ArrayPropCalls)
    HoistedArrayPropCalls.push_back(
        ArraySemanticsCall(C.copyTo(CheckBlock->getTerminator(), DomTree)));

  // Create a conditional branch on the fast condition being true.
  B.setInsertionPoint(CheckBlock->getTerminator());
  auto IsFastNativeArray =
      createFastNativeArraysCheck(HoistedArrayPropCalls, B);
  B.createCondBranch(CheckBlock->getTerminator()->getLoc(),
                     IsFastNativeArray, ClonedPreheader, NewPreheader);
  CheckBlock->getTerminator()->eraseFromParent();

  // Fixup the exit blocks. They are now dominated by the check block.
  for (auto *BB : ExitBlocksDominatedByPreheader)
    DomTree->changeImmediateDominator(DomTree->getNode(BB),
                                      DomTree->getNode(CheckBlock));

  // Replace the array.props calls uses in the cloned loop by their 'fast'
  // value.
  SILBuilder B2(ClonedPreheader->getTerminator());
  for (auto C : ArrayPropCalls)
    replaceArrayPropsCall(B2, C);

  // We have potentially cloned a loop - invalidate loop info.
  LoopAnalysis->invalidate(Header->getParent(),
                           SILAnalysis::InvalidationKind::FunctionBody);
}

namespace {
class SwiftArrayOptPass : public SILFunctionTransform {

  void run() override {
    if (!ShouldSpecializeArrayProps)
      return;

    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    SILLoopAnalysis *LA = PM->getAnalysis<SILLoopAnalysis>();
    SILLoopInfo *LI = LA->get(getFunction());

    bool HasChanged = false;

    // Check whether we can hoist 'array.props' calls out of loops, collecting
    // the preheader we can hoist to. We only hoist out of loops if 'all'
    // array.props call can be hoisted for a given loop nest.
    // We process the loop tree preorder (top-down) to hoist over the biggest
    // possible loop-nest.
    SmallVector<SILBasicBlock *, 16> HoistableLoopNests;
    std::function<void(SILLoop *)> processChildren = [&](SILLoop *L) {
      ArrayPropertiesAnalysis Analysis(L, DA);
      if (Analysis.run()) {
        // Hoist in the current loop nest.
        HasChanged = true;
        HoistableLoopNests.push_back(L->getLoopPreheader());
      } else {
        // Otherwise, try hoisting sub-loops.
        for (auto *SubLoop : *L)
          processChildren(SubLoop);
      }
    };
    for (auto *L : *LI)
      processChildren(L);

    // Specialize the identified loop nest based on the 'array.props' calls.
    if (HasChanged) {
      DEBUG(getFunction()->viewCFG());
      DominanceInfo *DT = DA->get(getFunction());

      // Process specialized loop-nests in loop-tree post-order (bottom-up).
      std::reverse(HoistableLoopNests.begin(), HoistableLoopNests.end());

      // Hoist the loop nests.
      for (auto &HoistableLoopNest : HoistableLoopNests)
        ArrayPropertiesSpecializer(DT, LA, HoistableLoopNest).run();

      // We might have cloned there might be critical edges that need splitting.
      splitAllCriticalEdges(*getFunction(), true /* only cond_br terminators*/,
                            DT, nullptr);

      DEBUG(getFunction()->viewCFG());
    }

    if (HasChanged) {
      // We preserve the dominator tree. Let's invalidate everything
      // else.
      DA->lockInvalidation();
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
      DA->unlockInvalidation();
    }
  }

  StringRef getName() override { return "SIL Swift Array Optimization"; }
};
} // end anonymous namespace

SILTransform *swift::createSwiftArrayOpts() {
  return new SwiftArrayOptPass();
}
