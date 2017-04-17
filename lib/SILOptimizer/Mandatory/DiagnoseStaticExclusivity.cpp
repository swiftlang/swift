//===--- DiagnoseStaticExclusivity.cpp - Find violations of exclusivity ---===//
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
//
// This file implements a diagnostic pass that finds violations of the
// "Law of Exclusivity" at compile time. The Law of Exclusivity requires
// that the access duration of any access to an address not overlap
// with an access to the same address unless both accesses are reads.
//
// This pass relies on 'begin_access' and 'end_access' SIL instruction
// markers inserted during SILGen to determine when an access to an address
// begins and ends. It models the in-progress accesses with a map from
// storage locations to the counts of read and write-like accesses in progress
// for that location.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "static-exclusivity"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Support/Debug.h"
using namespace swift;

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &Context, SourceLoc loc,
                                   Diag<T...> diag, U &&... args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

namespace {

enum class AccessedStorageKind : unsigned {
  /// The access is to a location represented by a SIL value
  /// (for example, an 'alloc_box' instruction for a local variable).
  /// Two accesses accessing the exact same SILValue are considered to be
  /// accessing the same storage location.
  Value,

  /// The access is to a global variable.
  GlobalVar
};

/// Represents the identity of a storage location being accessed.
/// This is used to determine when two 'begin_access' instructions
/// definitely access the same underlying location.
class AccessedStorage {
  AccessedStorageKind Kind;

  union {
    SILValue Value;
    SILGlobalVariable *Global;
  };

public:
  AccessedStorage(SILValue V)
      : Kind(AccessedStorageKind::Value), Value(V) { }

  AccessedStorage(SILGlobalVariable *Global)
      : Kind(AccessedStorageKind::GlobalVar), Global(Global) {}

  AccessedStorageKind getKind() const { return Kind; }

  SILValue getValue() const {
    assert(Kind == AccessedStorageKind::Value);
    return Value;
  }

  SILGlobalVariable *getGlobal() const {
    assert(Kind == AccessedStorageKind::GlobalVar);
    return Global;
  }
};

/// Models the in-progress accesses for a single storage location.
class AccessInfo {

  /// The number of in-progress 'read' accesses (that is 'begin_access [read]'
  /// instructions that have not yet had the corresponding 'end_access').
  unsigned Reads = 0;

  /// The number of in-progress write-like accesses.
  unsigned NonReads = 0;

  /// The instruction that began the first in-progress access to the storage
  /// location. Used for diagnostic purposes.
  const BeginAccessInst *FirstAccess = nullptr;

public:
  // Returns true when beginning an access of the given Kind will
  // result in a conflict with a previous access.
  bool conflictsWithAccess(SILAccessKind Kind) {
    if (Kind == SILAccessKind::Read) {
      // A read conflicts with any non-read accesses.
      return NonReads > 0;
    }

    // A non-read access conflicts with any other access.
    return NonReads > 0 || Reads > 0;
  }

  /// Returns true when there must have already been a conflict diagnosed
  /// for an in-progress access. Used to suppress multiple diagnostics for
  /// the same underlying access violation.
  bool alreadyHadConflict() {
    return (NonReads > 0 && Reads > 0) || (NonReads > 1);
  }

  /// Returns true when there are any accesses to this location in progress.
  bool hasAccessesInProgress() { return Reads > 0 || NonReads > 0; }

  /// Increment the count for given access.
  void beginAccess(const BeginAccessInst *BAI) {
    if (!FirstAccess) {
      assert(Reads == 0 && NonReads == 0);
      FirstAccess = BAI;
    }

    if (BAI->getAccessKind() == SILAccessKind::Read)
      Reads++;
    else
      NonReads++;
  }

  /// Decrement the count for given access.
  void endAccess(const EndAccessInst *EAI) {
    if (EAI->getBeginAccess()->getAccessKind() == SILAccessKind::Read)
      Reads--;
    else
      NonReads--;

    // If all open accesses are now ended, forget the location of the
    // first access.
    if (Reads == 0 && NonReads == 0)
      FirstAccess = nullptr;
  }

  /// Returns the instruction that began the first in-progress access.
  const BeginAccessInst *getFirstAccess() { return FirstAccess; }
};

/// Indicates whether a 'begin_access' requires exclusive access
/// or allows shared acceess. This needs to be kept in sync with
/// diag::exclusivity_access_required and diag::exclusivity_conflicting_access.
enum class ExclusiveOrShared_t : unsigned {
  ExclusiveAccess = 0,
  SharedAccess = 1
};


/// Tracks the in-progress accesses on per-storage-location basis.
using StorageMap = llvm::SmallDenseMap<AccessedStorage, AccessInfo, 4>;

/// A pair of 'begin_access' instructions that conflict.
struct ConflictingAccess {
  const BeginAccessInst *FirstAccess;
  const BeginAccessInst *SecondAccess;
};

} // end anonymous namespace

namespace llvm {
/// Enable using AccessedStorage as a key in DenseMap.
template <> struct DenseMapInfo<AccessedStorage> {
  static AccessedStorage getEmptyKey() {
    return AccessedStorage(swift::SILValue::getFromOpaqueValue(
        llvm::DenseMapInfo<void *>::getEmptyKey()));
  }

  static AccessedStorage getTombstoneKey() {
    return AccessedStorage(swift::SILValue::getFromOpaqueValue(
        llvm::DenseMapInfo<void *>::getTombstoneKey()));
  }

  static unsigned getHashValue(AccessedStorage Storage) {
    switch (Storage.getKind()) {
    case AccessedStorageKind::Value:
      return DenseMapInfo<swift::SILValue>::getHashValue(Storage.getValue());
    case AccessedStorageKind::GlobalVar:
      return DenseMapInfo<void *>::getHashValue(Storage.getGlobal());
    }
    llvm_unreachable("Unhandled AccessedStorageKind");
  }

  static bool isEqual(AccessedStorage LHS, AccessedStorage RHS) {
    if (LHS.getKind() != RHS.getKind())
      return false;

    switch (LHS.getKind()) {
    case AccessedStorageKind::Value:
      return LHS.getValue() == RHS.getValue();
    case AccessedStorageKind::GlobalVar:
      return LHS.getGlobal() == RHS.getGlobal();
    }
    llvm_unreachable("Unhandled AccessedStorageKind");
  }
};

} // end namespace llvm


/// Returns whether a 'begin_access' requires exclusive or shared access
/// to its storage.
static ExclusiveOrShared_t getRequiredAccess(const BeginAccessInst *BAI) {
  if (BAI->getAccessKind() == SILAccessKind::Read)
    return ExclusiveOrShared_t::SharedAccess;

  return ExclusiveOrShared_t::ExclusiveAccess;
}

/// Perform a syntactic suppression that returns true when the accesses are for
/// inout arguments to a call that corresponds to to one of the passed-in
/// 'apply' instructions.
static bool
isConflictOnInoutArgumentsToSuppressed(const BeginAccessInst *Access1,
                                       const BeginAccessInst *Access2,
                                       ArrayRef<ApplyInst *> CallsToSuppress) {
  if (CallsToSuppress.empty())
    return false;

  // Inout arguments must be modifications.
  if (Access1->getAccessKind() != SILAccessKind::Modify ||
      Access2->getAccessKind() != SILAccessKind::Modify) {
    return false;
  }

  SILLocation Loc1 = Access1->getLoc();
  SILLocation Loc2 = Access2->getLoc();
  if (Loc1.isNull() || Loc2.isNull())
    return false;

  auto *InOut1 = Loc1.getAsASTNode<InOutExpr>();
  auto *InOut2 = Loc2.getAsASTNode<InOutExpr>();
  if (!InOut1 || !InOut2)
    return false;

  for (ApplyInst *AI : CallsToSuppress) {
    SILLocation CallLoc = AI->getLoc();
    if (CallLoc.isNull())
      continue;

    auto *CE = CallLoc.getAsASTNode<CallExpr>();
    if (!CE)
      continue;

    bool FoundTarget = false;
    CE->forEachChildExpr([=,&FoundTarget](Expr *Child) {
      if (Child == InOut1) {
        FoundTarget = true;
        // Stops the traversal.
        return (Expr *)nullptr;
      }
      return Child;
     }
    );
    if (FoundTarget)
      return true;
  }

  return false;
}

/// Emits a diagnostic if beginning an access with the given in-progress
/// accesses violates the law of exclusivity. Returns true when a
/// diagnostic was emitted.
static void diagnoseExclusivityViolation(const BeginAccessInst *PriorAccess,
                                         const BeginAccessInst *NewAccess,
                                         ASTContext &Ctx) {

  // Can't have a conflict if both accesses are reads.
  assert(!(PriorAccess->getAccessKind() == SILAccessKind::Read &&
           NewAccess->getAccessKind() == SILAccessKind::Read));

  ExclusiveOrShared_t NewRequires = getRequiredAccess(NewAccess);
  ExclusiveOrShared_t PriorRequires = getRequiredAccess(PriorAccess);

  diagnose(Ctx, NewAccess->getLoc().getSourceLoc(),
           diag::exclusivity_access_required,
           static_cast<unsigned>(NewAccess->getAccessKind()),
           static_cast<unsigned>(NewRequires));
  diagnose(Ctx, PriorAccess->getLoc().getSourceLoc(),
           diag::exclusivity_conflicting_access,
           static_cast<unsigned>(PriorAccess->getAccessKind()),
           static_cast<unsigned>(PriorRequires));
}

/// Look through a value to find the underlying storage accessed.
static AccessedStorage findAccessedStorage(SILValue Source) {
  SILValue Iter = Source;
  while (true) {
    if (auto *PBI = dyn_cast<ProjectBoxInst>(Iter)) {
      Iter = PBI->getOperand();
      continue;
    }

    if (auto *CVI = dyn_cast<CopyValueInst>(Iter)) {
      Iter = CVI->getOperand();
      continue;
    }

    if (auto *GAI = dyn_cast<GlobalAddrInst>(Iter)) {
      return AccessedStorage(GAI->getReferencedGlobal());
    }

    assert(Iter->getType().isAddress() || Iter->getType().is<SILBoxType>());
    return AccessedStorage(Iter);
  }
}

/// Returns true when the apply calls the Standard Library swap().
/// Used for diagnostic suppression.
bool isCallToStandardLibrarySwap(ApplyInst *AI, ASTContext &Ctx) {
  SILFunction *SF = AI->getReferencedFunction();
  if (!SF)
    return false;

  if (!SF->hasLocation())
    return false;

  auto *FD = SF->getLocation().getAsASTNode<FuncDecl>();
  if (!FD)
    return false;

  return FD == Ctx.getSwap(nullptr);
}

static void checkStaticExclusivity(SILFunction &Fn, PostOrderFunctionInfo *PO) {
  // The implementation relies on the following SIL invariants:
  //    - All incoming edges to a block must have the same in-progress
  //      accesses. This enables the analysis to not perform a data flow merge
  //      on incoming edges.
  //    - Further, for a given address each of the in-progress
  //      accesses must have begun in the same order on all edges. This ensures
  //      consistent diagnostics across changes to the exploration of the CFG.
  //    - On return from a function there are no in-progress accesses. This
  //      enables a sanity check for lean analysis state at function exit.
  //    - Each end_access instruction corresponds to exactly one begin access
  //      instruction. (This is encoded in the EndAccessInst itself)
  //    - begin_access arguments cannot be basic block arguments.
  //      This enables the analysis to look back to find the *single* storage
  //      storage location accessed.

  if (Fn.empty())
    return;

  // Collects calls to functions for which diagnostics about conflicting inout
  // arguments should be suppressed.
  llvm::SmallVector<ApplyInst *, 8> CallsToSuppress;

  // Stores the accesses that have been found to conflict. Used to defer
  // emitting diagnostics until we can determine whether they should
  // be suppressed.
  llvm::SmallVector<ConflictingAccess, 4> ConflictingAccesses;

  // For each basic block, track the stack of current accesses on
  // exit from that block.
  llvm::SmallDenseMap<SILBasicBlock *, Optional<StorageMap>, 32>
      BlockOutAccesses;

  BlockOutAccesses[Fn.getEntryBlock()] = StorageMap();

  for (auto *BB : PO->getReversePostOrder()) {
    Optional<StorageMap> &BBState = BlockOutAccesses[BB];

    // Because we use a reverse post-order traversal, unless this is the entry
    // at least one of its predecessors must have been reached. Use the out
    // state for that predecessor as our in state. The SIL verifier guarantees
    // that all incoming edges must have the same current accesses.
    for (auto *Pred : BB->getPredecessorBlocks()) {
      auto it = BlockOutAccesses.find(Pred);
      if (it == BlockOutAccesses.end())
        continue;

      const Optional<StorageMap> &PredAccesses = it->getSecond();
      if (PredAccesses) {
        BBState = PredAccesses;
        break;
      }
    }

    // The in-progress accesses for the current program point, represented
    // as map from storage locations to the accesses in progress for the
    // location.
    StorageMap &Accesses = *BBState;

    for (auto &I : *BB) {
      // Apply transfer functions. Beginning an access
      // increments the read or write count for the storage location;
      // Ending onr decrements the count.
      if (auto *BAI = dyn_cast<BeginAccessInst>(&I)) {
        SILAccessKind Kind = BAI->getAccessKind();
        AccessInfo &Info = Accesses[findAccessedStorage(BAI->getSource())];
        if (Info.conflictsWithAccess(Kind) && !Info.alreadyHadConflict()) {
          const BeginAccessInst *Conflict = Info.getFirstAccess();
          assert(Conflict && "Must already have had access to conflict!");
          ConflictingAccesses.push_back({ Conflict, BAI });
        }

        Info.beginAccess(BAI);
        continue;
      }

      if (auto *EAI = dyn_cast<EndAccessInst>(&I)) {
        auto It = Accesses.find(findAccessedStorage(EAI->getSource()));
        AccessInfo &Info = It->getSecond();
        Info.endAccess(EAI);

        // If the storage location has no more in-progress accesses, remove
        // it to keep the StorageMap lean.
        if (!Info.hasAccessesInProgress())
          Accesses.erase(It);
        continue;
      }

      if (auto *AI = dyn_cast<ApplyInst>(&I)) {
        // Suppress for the arguments to the Standard Library's swap()
        // function until we can recommend a safe alternative.
        if (isCallToStandardLibrarySwap(AI, Fn.getASTContext()))
          CallsToSuppress.push_back(AI);
      }

      // Sanity check to make sure entries are properly removed.
      assert((!isa<ReturnInst>(&I) || Accesses.size() == 0) &&
             "Entries were not properly removed?!");
    }
  }

  // Now that we've collected violations and suppressed calls, emit
  // diagnostics.
  for (auto &Violation : ConflictingAccesses) {
    const BeginAccessInst *PriorAccess = Violation.FirstAccess;
    const BeginAccessInst *NewAccess = Violation.SecondAccess;
    if (isConflictOnInoutArgumentsToSuppressed(PriorAccess, NewAccess,
                                               CallsToSuppress))
      continue;

    diagnoseExclusivityViolation(PriorAccess, NewAccess, Fn.getASTContext());
  }
}

namespace {

class DiagnoseStaticExclusivity : public SILFunctionTransform {
public:
  DiagnoseStaticExclusivity() {}

private:
  void run() override {
    SILFunction *Fn = getFunction();
    // This is a staging flag. Eventually the ability to turn off static
    // enforcement will be removed.
    if (!Fn->getModule().getOptions().EnforceExclusivityStatic)
      return;

    PostOrderFunctionInfo *PO = getAnalysis<PostOrderAnalysis>()->get(Fn);
    checkStaticExclusivity(*Fn, PO);
  }
};

} // end anonymous namespace

SILTransform *swift::createDiagnoseStaticExclusivity() {
  return new DiagnoseStaticExclusivity();
}
