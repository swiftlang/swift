//===------- COWArrayOpt.cpp - Optimize Copy-On-Write Array Checks --------===//
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

#define DEBUG_TYPE "cowarray-opts"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILAnalysis/ColdBlockInfo.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/LoopAnalysis.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
using namespace swift;

/// Given a SIL value, capture its element index and the value of the aggregate
/// that immeditely contains it. (This is a simple projection over a value.)
struct AccessPathComponent {
  SILValue Aggregate;
  unsigned Index;
  AccessPathComponent(ValueBase *V) :Index(~0U) {
    switch (V->getKind()) {
    default:
      break;
    case ValueKind::StructElementAddrInst: {
      StructElementAddrInst *SEA = cast<StructElementAddrInst>(V);
      Index = SEA->getFieldNo();
      Aggregate = SEA->getOperand();
      break;
    }
    case ValueKind::RefElementAddrInst: {
      RefElementAddrInst *REA = cast<RefElementAddrInst>(V);
      Index = REA->getFieldNo();
      Aggregate = REA->getOperand();
      break;
    }
    case ValueKind::TupleElementAddrInst: {
      TupleElementAddrInst *TEA = cast<TupleElementAddrInst>(V);
      Index = TEA->getFieldNo();
      Aggregate = TEA->getOperand();
      break;
    }
    }
  }
  bool isValid() const { return Aggregate.isValid(); }
};

/// \return a sequence of integers representing the access path of this element
/// within a Struct/Ref/Tuple.
///
/// IndexedEntity is not handled because it would add complexity.
static SILValue getAccessPath(SILValue V, SmallVectorImpl<unsigned>& Path) {
  V = V.stripCasts();
  AccessPathComponent APC(V.getDef());
  if (!APC.isValid())
    return V;

  SILValue UnderlyingObject = getAccessPath(APC.Aggregate, Path);
  Path.push_back(APC.Index);
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
    // Collect all users of the address and loads.
    collectAddressUses(V, AccessPath, nullptr);

    // Collect all uses of the Struct value.
    for (auto *DefInst : StructLoads) {
      for (auto DefUI : DefInst->getUses()) {
        if (!Visited.insert(&*DefUI))
          continue;
        StructValueUsers.push_back(DefUI->getUser());
      }
    }
    // Collect all users of element values.
    for (auto &Pair : ElementLoads) {
      for (auto DefUI : Pair.first->getUses()) {
        if (!Visited.insert(&*DefUI))
          continue;
        ElementValueUsers.push_back(
          std::make_pair(DefUI->getUser(), Pair.second));
      }
    }
  }

protected:
  /// If AccessPathSuffix is non-empty, then the value is the address of an
  /// aggregate containing the Struct. If AccessPathSuffix is empty and
  /// StructVal is invalid, then the value is the address of the Struct. If
  /// StructVal is valid, the value is the address of an element within the
  /// Struct.
  void collectAddressUses(ValueBase *V, ArrayRef<unsigned> AccessPathSuffix,
                          Operand *StructVal) {
    for (auto UI : V->getUses()) {
      // Keep the operand, not the instruction in the visited set. The same
      // instruction may theoretically have different types of uses.
      if (!Visited.insert(&*UI))
        continue;

      SILInstruction *UseInst = UI->getUser();
      if (StructVal) {
        // Found a use of an element.
        assert(AccessPathSuffix.empty() && "should have accessed struct");
        if (LoadInst *LoadI = dyn_cast<LoadInst>(UseInst))
          ElementLoads.push_back(std::make_pair(LoadI, StructVal));
        else if (isa<StructElementAddrInst>(UseInst))
          collectAddressUses(UseInst, AccessPathSuffix, StructVal);
        else
          ElementAddressUsers.push_back(std::make_pair(UseInst,StructVal));
        continue;

      } else if (AccessPathSuffix.empty()) {
        // Found a use of the struct at the given access path.
        if (LoadInst *LoadI = dyn_cast<LoadInst>(UseInst))
          StructLoads.push_back(LoadI);
        else if (isa<StructElementAddrInst>(UseInst))
          collectAddressUses(UseInst, AccessPathSuffix, &*UI);
        else
          StructAddressUsers.push_back(UseInst);
        continue;
      }
      AccessPathComponent APC(UseInst);
      if (!APC.isValid()) {
        // Found a use of an aggregate containing the given element.
        AggregateAddressUsers.push_back(UseInst);
        continue;
      }
      if (APC.Index != AccessPathSuffix[0]) {
        // Ignore uses of disjoint elements.
        continue;
      }
      assert(APC.Aggregate == V && "Expected unary element addr inst.");
      // Recursively check for users after stripping this component from the
      // access path.
      collectAddressUses(UseInst, AccessPathSuffix.slice(1), nullptr);
    }
  }
};
} // namespace

/// \return true if the callee is tagged with array semantics.
static bool isArraySemanticCall(ApplyInst *AI) {
  if (FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(AI->getCallee())) {
    // All methods with "array" semantics are safe w.r.t make_mutable.
    if (FRI->getReferencedFunction()->hasDefinedSemantics() &&
        FRI->getReferencedFunction()->getSemanticsString().startswith("array"))
      return true;
  }
  return false;
}

/// \return true if the given instruction releases the given value.
static bool isRelease(SILInstruction *Inst, SILValue Value) {
  if (isa<ReleaseValueInst>(Inst) && Inst->getOperand(0) == Value)
    return true;

  if (ApplyInst *AI = dyn_cast<ApplyInst>(Inst)) {
    if (FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(AI->getCallee())) {
      SILFunction *F = FRI->getReferencedFunction();
      auto Params = F->getLoweredFunctionType()->getParameters();
      auto Args = AI->getArguments();
      for (unsigned ArgIdx = 0, ArgEnd = Params.size(); ArgIdx != ArgEnd;
           ++ArgIdx) {
        if (Args[ArgIdx] != Value)
          continue;
        ParameterConvention P = Params[ArgIdx].getConvention();
        if (P == ParameterConvention::Direct_Owned)
          return true;
      }
    }
  }
  return false;
}

namespace {
/// Optimize Copy-On-Write array checks based on high-level semantics.
///
/// Performs an analysis on all Array users to ensure they do not interfere
/// with make_mutable hoisting. Ultimately, the only thing that can interefere
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

  SILFunction *Function;
  SILLoop *Loop;
  SILBasicBlock *Preheader;
  DominanceInfo* DomTree;
  bool HasChanged = false;

  // Keep track of cold blocks.
  ColdBlockInfo ColdBlocks;

  // Set of all blocks that may reach the loop, not including loop blocks.
  llvm::SmallPtrSet<SILBasicBlock*,32> ReachingBlocks;

  // Map an array to a hoisted make_mutable call for the current loop. An array
  // is only mapped to a call once the analysis has determined that no
  // make_mutable calls are required within the loop body for that array.
  llvm::SmallDenseMap<SILValue, ApplyInst*> ArrayMakeMutableMap;

  // \brief Transient per-Array user set.
  //
  // Track all known array users with the exception of struct_extract users
  // (checkSafeArrayElementUse prohibits struct_extract users from mutating the
  // array). During analysis of retains/releases within the loop body, the
  // users in this set are assumed to cover all possible mutating operations on
  // the array. If the array escaped through an unknown use, the analysis must
  // abort earlier.
  SmallPtrSet<SILInstruction*, 8> ArrayUserSet;

public:
  COWArrayOpt(SILLoop *L, DominanceAnalysis *DA)
    : Function(L->getHeader()->getParent()), Loop(L),
      Preheader(L->getLoopPreheader()), DomTree(DA->getDomInfo(Function)),
      ColdBlocks(DA)
    {}

  bool run();

protected:
  bool checkUniqueArrayContainer(SILValue ArrayContainer);
  SmallPtrSetImpl<SILBasicBlock*> &getReachingBlocks();
  bool isRetainReleasedBeforeMutate(SILInstruction *RetainInst,
                                    SILValue ArrayVal);
  bool checkSafeArrayAddressUses(UserList &AddressUsers);
  bool checkSafeArrayValueUses(UserList &ArrayValueUsers);
  bool checkSafeArrayElementUse(SILInstruction *UseInst, SILValue ArrayVal);
  bool checkSafeElementValueUses(UserOperList &ElementValueUsers);
  bool hoistMakeMutable(ApplyInst *MakeMutable, SILValue ArrayAddr);
};
} // namespace

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
    ArrayRef<SILArgument*> FunctionArgs = Function->begin()->getBBArgs();
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

/// Lazilly compute blocks that may reach the loop.
SmallPtrSetImpl<SILBasicBlock*> &COWArrayOpt::getReachingBlocks() {
  if (ReachingBlocks.empty()) {
    SmallVector<SILBasicBlock*, 8> Worklist;
    ReachingBlocks.insert(Preheader);
    Worklist.push_back(Preheader);
    while (!Worklist.empty()) {
      SILBasicBlock *BB = Worklist.pop_back_val();
      for (auto PI = BB->pred_begin(), PE = BB->pred_end(); PI != PE; ++PI) {
        if (ReachingBlocks.insert(*PI))
          Worklist.push_back(*PI);
      }
    }
  }
  return ReachingBlocks;
}

/// \return true if the given retain instruction is followed by a release on the
/// same object prior to any potential mutating operation.
bool COWArrayOpt::isRetainReleasedBeforeMutate(SILInstruction *RetainInst,
                                               SILValue ArrayVal) {
  // If a retain is found outside the loop ignore it. Otherwise, it must
  // have a matching @owned call.
  if (!Loop->contains(RetainInst))
    return true;

  // Walk forward looking for a release of ArrayLoad or element of
  // ArrayUserSet. Note that ArrayUserSet does not included uses of elements
  // within the Array. Consequently, checkSafeArrayElementUse must prove that
  // no uses of the Array value, or projections of it can lead to mutation
  // (element uses may only be retained/released).
  for (auto II = std::next(SILBasicBlock::iterator(RetainInst)),
         IE = RetainInst->getParent()->end(); II != IE; ++II) {
    if (isRelease(II, ArrayVal))
      return true;

    if (ArrayUserSet.count(II)) // May be an array mutation.
      break;
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
    if (ApplyInst *AI = dyn_cast<ApplyInst>(UseInst)) {
      if (isArraySemanticCall(AI))
        continue;

      // Check of this escape can reach the current loop.
      if (!Loop->contains(UseInst->getParent()) &&
          !getReachingBlocks().count(UseInst->getParent())) {
        continue;
      }
      DEBUG(llvm::dbgs() << "    Skipping Array: may escape through call!\n    "
            << *UseInst);
    } else if (StoreInst *StInst = dyn_cast<StoreInst>(UseInst)) {
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

    } else if (isa<DeallocStackInst>(UseInst)) {
      // Handle destruction of a local array.
      continue;

    } else {
      DEBUG(llvm::dbgs() << "    Skipping Array: unknown Array use!\n    "
            << *UseInst);
    }
    // Found an unsafe or unknown user. The Array may escape here.
    return false;
  }
  return true;
}

/// Check that the use of an Array value, the value of an aggregate containing
/// an array, or the value of an element within the array, is safe w.r.t
/// make_mutable hoisting. Retains are safe as long as they are not inside the
/// Loop.
bool COWArrayOpt::checkSafeArrayValueUses(UserList &ArrayValueUsers) {
  for (auto *UseInst : ArrayValueUsers) {
    if (ApplyInst *AI = dyn_cast<ApplyInst>(UseInst)) {
      if (isArraySemanticCall(AI))
        continue;

    } else if (auto *SEI = dyn_cast<StructExtractInst>(UseInst)) {
      for (auto UI : SEI->getUses()) {
        if (!checkSafeArrayElementUse(UI->getUser(), SEI->getOperand()))
          return false;
      }
      continue;

    } else if (auto *RVI = dyn_cast<RetainValueInst>(UseInst)) {
      if (isRetainReleasedBeforeMutate(UseInst, RVI->getOperand()))
        continue;

    } else if (isa<ReleaseValueInst>(UseInst)) {
      // Releases are always safe. This case handles the release of an array
      // buffer that is loaded from a local array struct.
      continue;

    }
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
  if (isa<RetainValueInst>(UseInst) &&
      isRetainReleasedBeforeMutate(UseInst, ArrayVal))
    return true;

  if (isa<ReleaseValueInst>(UseInst))
    // Releases are always safe. This case handles the release of an array
    // buffer that is loaded from a local array struct.
    return true;

  if (StructExtractInst *SEI = dyn_cast<StructExtractInst>(UseInst)) {
    for (auto UI : SEI->getUses()) {
      // Recurse.
      if (!checkSafeArrayElementUse(UI->getUser(), ArrayVal))
        return false;
    }
    return true;
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

/// TODO: Isn't there a helper for this somewhere?
static SILBasicBlock *getValBB(SILValue Val) {
  if (auto Inst = dyn_cast<SILInstruction>(Val.getDef()))
    return Inst->getParent();
  if (auto Arg = dyn_cast<SILArgument>(Val.getDef()))
    return Arg->getParent();
  return nullptr;
}

/// Check if this call to "make_mutable" is hoistable, and move it, or delete it
/// if it's already hoisted.
bool COWArrayOpt::hoistMakeMutable(ApplyInst *MakeMutable, SILValue ArrayAddr) {
  DEBUG(llvm::dbgs() << "    Checking mutable array: " << ArrayAddr);

  SILBasicBlock *ArrayBB = getValBB(ArrayAddr);
  if (ArrayBB && !DomTree->dominates(ArrayBB, Preheader)) {
    DEBUG(llvm::dbgs() << "    Skipping Array: does not dominate loop!\n");
    return false;
  }
  SmallVector<unsigned, 4> AccessPath;
  SILValue ArrayContainer = getAccessPath(ArrayAddr, AccessPath);

  // Check that the array is a member of an inout argument or return value.
  if (!checkUniqueArrayContainer(ArrayContainer))
    return false;

  // Check that the Array is not retained with this loop and it's address does
  // not escape within this function.
  StructUseCollector StructUses;
  StructUses.collectUses(ArrayContainer.getDef(), AccessPath);
  for (auto *Oper : StructUses.Visited)
    ArrayUserSet.insert(Oper->getUser());

  if (!checkSafeArrayAddressUses(StructUses.AggregateAddressUsers) ||
      !checkSafeArrayAddressUses(StructUses.StructAddressUsers) ||
      !checkSafeArrayValueUses(StructUses.StructValueUsers) ||
      !checkSafeElementValueUses(StructUses.ElementValueUsers) ||
      !StructUses.ElementAddressUsers.empty())
    return false;

  // Hoist this call to make_mutable.
  DEBUG(llvm::dbgs() << "    Hoisting make_mutable: " << *MakeMutable);
  MakeMutable->moveBefore(Preheader->getTerminator());
  placeFuncRef(MakeMutable, DomTree);
  return true;
}

bool COWArrayOpt::run() {
  DEBUG(llvm::dbgs() << "  Array Opts in Loop " << *Loop);

  Preheader = Loop->getLoopPreheader();
  if (!Preheader) {
    DEBUG(llvm::dbgs() << "    Skipping Loop: No Preheader!\n");
    return false;
  }
  for (auto *BB : Loop->getBlocks()) {
    if (ColdBlocks.isCold(BB))
      continue;
    for (auto II = BB->begin(), IE = BB->end(); II != IE;) {
      // Inst may be moved by hoistMakeMutable.
      SILInstruction *Inst = II++;
      if (ApplyInst *AI = dyn_cast<ApplyInst>(Inst)) {
        if (FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(AI->getCallee())) {
          if (FRI->getReferencedFunction()
              ->hasSemanticsString("array.make_mutable")) {
            SILValue ArrayAddr = AI->getSelfArgument();
            auto HoistedCallEntry = ArrayMakeMutableMap.find(ArrayAddr);
            if (HoistedCallEntry == ArrayMakeMutableMap.end()) {
              if (hoistMakeMutable(AI, ArrayAddr)) {
                ArrayMakeMutableMap[ArrayAddr] = AI;
                HasChanged = true;
              } else
                ArrayMakeMutableMap[ArrayAddr] = nullptr;
            }
            else if (HoistedCallEntry->second) {
              DEBUG(llvm::dbgs() << "    Removing make_mutable call: " << *AI);
              AI->eraseFromParent();
              HasChanged = true;
            }
          }
        }
      }
    }
  }
  return HasChanged;
}

namespace {
class COWArrayOptPass : public SILFunctionTransform
{
  void run() override {
    DEBUG(llvm::dbgs() << "COW Array Opts in Func " << getFunction()->getName()
          << "\n");
    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    SILLoopAnalysis *LA = PM->getAnalysis<SILLoopAnalysis>();
    SILLoopInfo *LI = LA->getLoopInfo(getFunction());
    if (LI->empty()) {
      DEBUG(llvm::dbgs() << "  Skipping Function: No loops.\n");
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
      HasChanged |= COWArrayOpt(L, DA).run();

    if (HasChanged)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "SIL Array Optimization"; }
};
} // anonymous

SILTransform *swift::createCOWArrayOpts() {
  return new COWArrayOptPass();
}
