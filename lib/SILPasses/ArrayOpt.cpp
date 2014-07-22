//===------------- ArrayOpt.cpp - Optimize Swift Array Checks -------------===//
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

#define DEBUG_TYPE "array-opts"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILAnalysis/ColdBlockInfo.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/SILLoopInfo.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
using namespace swift;

namespace {
typedef SmallPtrSetImpl<SILInstruction*> VisitedSet;
typedef SmallVectorImpl<SILInstruction*> UserList;
}

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

/// Collect all uses of the address of an element at the given underlying object
/// and access path. Uses of the address of an aggregate that contains the
/// element are also recorded in AggregateAddressUsers. If the use is a Load,
/// record it separately so that uses of the value can be traversed.
///
/// bb0(%arg : $*S)
/// apply %f(%arg)        // <--- Aggregate Address User
/// %elt_addr = struct_element_addr %arg : $*S, #S.element
/// apply %g(%elt_addr)   // <--- Element Address User
/// %val = load %elt_addr // <--- Load
/// apply %h(%val)        // <--- Value User
///
/// This assumes that an instruction will not use both the address of an element
/// and the address of an aggregate. If it does, then only one is recorded,
/// which is fine for SILArrayOpt's purpose.
static void collectElementUses(ValueBase *V,
                               ArrayRef<unsigned> AccessPath,
                               UserList &Loads,
                               UserList &ElementAddressUsers,
                               UserList &AggregateAddressUsers,
                               VisitedSet &Visited) {

  for (auto UI : V->getUses()) {
    SILInstruction *UseInst = UI->getUser();
    if (!Visited.insert(UseInst))
      continue;

    if (AccessPath.empty()) {
      // Found a use of the element at the given access path.
      if (LoadInst *LoadI = dyn_cast<LoadInst>(UseInst))
        Loads.push_back(LoadI);
      else
        ElementAddressUsers.push_back(UseInst);
      continue;
    }
    AccessPathComponent APC(UseInst);
    if (!APC.isValid()) {
      // Found a use of an aggregate containing the given element.
      AggregateAddressUsers.push_back(UseInst);
      continue;
    }
    if (APC.Index != AccessPath[0]) {
      // Ignore uses of disjoint elements.
      continue;
    }
    assert(APC.Aggregate == V && "Expected unary element addr inst.");
    // Recursively check for users after stripping this component from the
    // access path.
    collectElementUses(UseInst, AccessPath.slice(1), Loads, ElementAddressUsers,
                       AggregateAddressUsers, Visited);
  }
}

/// Collect all uses of a set of Defs.
static void collectValueUses(const UserList &Defs, UserList &ValueUsers,
                            VisitedSet &Visited) {
  for (auto *DefInst : Defs) {
    for (auto DefUI : DefInst->getUses()) {
      SILInstruction *UseInst = DefUI->getUser();
      if (!Visited.insert(UseInst))
        continue;
      ValueUsers.push_back(UseInst);
    }
  }
}

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
/// Optimize Array checks based on high-level semantics.
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
/// TODO: Handle array's that are loaded or returned from a function call. (This
/// is not very common because it implies a copy).
class SILArrayOpt {
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
  // Track all known array users. During analysis of retains/releases within the
  // loop body, the users in this set are assumed to cover all possible mutating
  // operations on the array. If the array escaped through an unknown use, the
  // analysis must abort earlier.
  SmallPtrSet<SILInstruction*, 8> ArrayUserSet;

public:
  SILArrayOpt(SILLoop *L, DominanceAnalysis *DA)
    : Function(L->getHeader()->getParent()), Loop(L),
      Preheader(L->getLoopPreheader()), DomTree(DA->getDomInfo(Function)),
      ColdBlocks(DA)
    {}

  bool run();

protected:
  SmallPtrSetImpl<SILBasicBlock*> &getReachingBlocks();
  bool checkSafeArrayAddressUses(UserList &AddressUsers);
  bool checkSafeArrayElementUses(SILValue ArrayVal,
                                 StructExtractInst *SEI);
  bool checkSafeArrayValueUses(UserList &ArrayValueUsers);
  bool hoistMakeMutable(ApplyInst *MakeMutable, SILValue Array);
};
} // namespace

/// Lazilly compute blocks that may reach the loop.
SmallPtrSetImpl<SILBasicBlock*> &SILArrayOpt::getReachingBlocks() {
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

/// \return true if all given users of an array address are safe to hoist
/// make_mutable across.
///
/// General calls are unsafe because they may copy the array struct which in
/// turn bumps the reference count of the array storage.
///
/// The same logic currently applies to both uses of the array itself and uses
/// of an aggregate containing the array.
///
/// If an address user is also a value user, we will not see the value users. So
/// the address user must be safe for both kinds of uses.
bool SILArrayOpt::checkSafeArrayAddressUses(UserList &AddressUsers) {

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
    } else {
      DEBUG(llvm::dbgs() << "    Skipping Array: unknown Array use!\n    "
            << *UseInst);
    }
    // Found an unsafe or unknown user. The Array may escape here.
    return false;
  }
  return true;
}

/// Recursively check that uses of elements within the array are safe.
///
/// After the lower aggregates pass, SIL contains chains of struct_extract and
/// retain_value instructions. e.g.
///   %8 = load %0 : $*Array<Int>
///   %9 = struct_extract %8 : $Array<Int>, #Array._buffer
///   %10 = struct_extract %9 : $_ArrayBuffer<Int>, #_ArrayBuffer.storage
///  retain_value %10 : $Optional<Builtin.NativeObject>
///
/// Since this does not recurse through multi-operand instructions, no visited
/// set is necessary.
bool SILArrayOpt::checkSafeArrayElementUses(SILValue ArrayVal,
                                            StructExtractInst *SEI) {
  for (auto UI : SEI->getUses()) {
    SILInstruction *UseInst = UI->getUser();
    if (isa<RetainValueInst>(UseInst)) {
      // If a retain is found outside the loop ignore it. Otherwise, it must
      // have a matching @owned call.
      if (!Loop->contains(UseInst))
        continue;

      // Walk forward looking for a release of ArrayLoad or element of
      // ArrayUserSet. Note that ArrayUserSet does not included uses of elements
      // within the Array. Consequently, checkSafeArrayValueUses must prove that
      // no uses of the Array value, or projections of it can lead to mutation
      // (they are only for retains/releases).
      bool FoundRelease = false;
      for (SILBasicBlock::iterator II = UseInst,
             IE = UseInst->getParent()->end(); II != IE; ++II) {
        if (isRelease(II, ArrayVal)) {
          FoundRelease = true;
          break;
        }
        if (ArrayUserSet.count(II)) // May be an array mutation.
          break;
      }
      if (FoundRelease)
        continue;
      DEBUG(llvm::dbgs() << "    Skipping Array: retained in loop!\n    "
            << *UseInst);

    } else if (isa<ReleaseValueInst>(UseInst)) {
      // Releases are always safe.
      continue;
    }
    else if (StructExtractInst *NestedSEI =
             dyn_cast<StructExtractInst>(UseInst)) {
      // Recurse.
      if (checkSafeArrayElementUses(ArrayVal, NestedSEI))
        continue;
    }
    // Found an unsafe or unknown user. The Array may escape here.
    return false;
  }
  return true;
}

/// Check that the use of an Array value, or the value of an aggregate
/// containing an array is safe w.r.t make_mutable hoisting. Retains are safe as
/// long as they are not inside the Loop.
bool SILArrayOpt::checkSafeArrayValueUses(UserList &ArrayValueUsers) {
  for (auto *UseInst : ArrayValueUsers) {
    if (ApplyInst *AI = dyn_cast<ApplyInst>(UseInst)) {
      if (isArraySemanticCall(AI))
        continue;
      DEBUG(llvm::dbgs() << "    Skipping Array: Array value escapes!\n    "
            << *UseInst);

    } else if (StructExtractInst *SEI = dyn_cast<StructExtractInst>(UseInst)) {
      if (checkSafeArrayElementUses(SEI->getOperand(), SEI))
        continue;

    } else {
      DEBUG(llvm::dbgs() << "    Skipping Array: unknown Array use!\n"
            << *UseInst);
    }
    // Found an unsafe or unknown user. The Array may escape here.
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
bool SILArrayOpt::hoistMakeMutable(ApplyInst *MakeMutable, SILValue Array) {
  DEBUG(llvm::dbgs() << "    Checking mutable array: " << Array);

  SILBasicBlock *ArrayBB = getValBB(Array);
  if (ArrayBB && !DomTree->dominates(ArrayBB, Preheader)) {
    DEBUG(llvm::dbgs() << "    Skipping Array: does not dominate loop!\n");
    return false;
  }
  SmallVector<unsigned, 4> AccessPath;
  SILValue ArrayContainer = getAccessPath(Array, AccessPath);

  // Check that the array is a member of an inout argument.
  SILArgument *Arg = dyn_cast<SILArgument>(ArrayContainer);
  if (!Arg) {
    DEBUG(llvm::dbgs() << "    Skipping Array: Not a function argument!\n");
    return false;
  }
  // Check that the argument is passed as an inout type. This means there are no
  // aliases accessible within this function scope.
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
  // Check that the Array is not retained with this loop and it's address does
  // not escape within this function.
  ArrayUserSet.clear();
  SmallVector<SILInstruction*, 8> ArrayLoads;
  SmallVector<SILInstruction*, 8> ArrayAddressUsers;
  SmallVector<SILInstruction*, 8> AggregateAddressUsers;
  collectElementUses(Arg, AccessPath, ArrayLoads, ArrayAddressUsers,
                     AggregateAddressUsers, ArrayUserSet);

  // Using the same ArrayUserSet as the visited set here means that
  // checkSafeArrayValueUses will not see any value user that is also an
  // address user.
  SmallVector<SILInstruction*, 8> ArrayValueUsers;
  collectValueUses(ArrayLoads, ArrayValueUsers, ArrayUserSet);

  if (!checkSafeArrayAddressUses(ArrayAddressUsers) ||
      !checkSafeArrayAddressUses(AggregateAddressUsers) ||
      !checkSafeArrayValueUses(ArrayValueUsers))
    return false;

  // Hoist this call to make_mutable.
  DEBUG(llvm::dbgs() << "    Hoisting make_mutable: " << *MakeMutable);
  MakeMutable->moveBefore(Preheader->getTerminator());
  placeFuncRef(MakeMutable, DomTree);
  return true;
}

bool SILArrayOpt::run() {
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
            SILValue Array = AI->getSelfArgument();
            auto HoistedCallEntry = ArrayMakeMutableMap.find(Array);
            if (HoistedCallEntry == ArrayMakeMutableMap.end()) {
              if (hoistMakeMutable(AI, Array)) {
                ArrayMakeMutableMap[Array] = AI;
                HasChanged = true;
              } else
                ArrayMakeMutableMap[Array] = nullptr;
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

// Temporary flag for easy performance testing since this is a new feature.
llvm::cl::opt<bool>
EnableArrayOpt("sil-array-opts", llvm::cl::init(true));

namespace {
class SILArrayOptPass : public SILFunctionTransform
{
  void run() override {
    if (!EnableArrayOpt) {
      DEBUG(llvm::dbgs() << "Array Opts Pass is Disabled!\n");
      DEBUG(getFunction()->dump());
      return;
    }
    DEBUG(llvm::dbgs() << "Array Opts in Func " << getFunction()->getName()
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
      HasChanged |= SILArrayOpt(L, DA).run();

    if (HasChanged)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "SIL Array Optimization"; }
};
} // anonymous

SILTransform *swift::createArrayOpts() {
  return new SILArrayOptPass();
}
