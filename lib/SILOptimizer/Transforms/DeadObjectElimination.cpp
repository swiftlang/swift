//===--- DeadObjectElimination.cpp - Remove unused objects  ---------------===//
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
// This pass eliminates store only alloc_ref objects that have destructors
// without side effects.
//
// The high level overview of the algorithm is that first it visits the
// destructor and attempts to prove that the destructor is well behaved, i.e. it
// does not have any side effects outside of the destructor itself. If the
// destructor can be proven to be well behaved, it then goes through the use
// list of the alloc_ref and attempts to prove that the alloc_ref does not
// escape or is used in a way that could cause side effects. If both of those
// conditions apply, the alloc_ref and its entire use graph is eliminated.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "dead-object-elim"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(DeadAllocRefEliminated,
          "number of AllocRef instructions removed");

STATISTIC(DeadAllocStackEliminated,
          "number of AllocStack instructions removed");

STATISTIC(DeadAllocApplyEliminated,
          "number of allocating Apply instructions removed");

using UserList = llvm::SmallSetVector<SILInstruction *, 16>;

// Analyzing the body of this class destructor is valid because the object is
// dead. This means that the object is never passed to objc_setAssociatedObject,
// so its destructor cannot be extended at runtime.
static SILFunction *getDestructor(AllocRefInst *ARI) {
  // We only support classes.
  ClassDecl *ClsDecl = ARI->getType().getClassOrBoundGenericClass();
  if (!ClsDecl)
    return nullptr;

  // Look up the destructor of ClsDecl.
  DestructorDecl *Destructor = ClsDecl->getDestructor();
  assert(Destructor && "getDestructor() should never return a nullptr.");

  // Find the destructor name via SILDeclRef.
  // FIXME: When destructors get moved into vtables, update this to use the
  // vtable for the class.
  SILDeclRef Ref(Destructor);
  SILFunction *Fn = ARI->getModule().lookUpFunction(Ref);
  if (!Fn || Fn->empty()) {
    DEBUG(llvm::dbgs() << "    Could not find destructor.\n");
    return nullptr;
  }

  DEBUG(llvm::dbgs() << "    Found destructor!\n");

  // If the destructor has an objc_method calling convention, we cannot
  // analyze it since it could be swapped out from under us at runtime.
  if (Fn->getRepresentation() == SILFunctionTypeRepresentation::ObjCMethod) {
    DEBUG(llvm::dbgs() << "        Found Objective-C destructor. Can't "
          "analyze!\n");
    return nullptr;
  }

  return Fn;
}

/// Analyze the destructor for the class of ARI to see if any instructions in it
/// could have side effects on the program outside the destructor. If it does
/// not, then we can eliminate the destructor.
static bool doesDestructorHaveSideEffects(AllocRefInst *ARI) {
  SILFunction *Fn = getDestructor(ARI);
  // If we can't find a constructor then assume it has side effects.
  if (!Fn)
    return true;

  // A destructor only has one argument, self.
  assert(Fn->begin()->getNumArguments() == 1 &&
         "Destructor should have only one argument, self.");
  SILArgument *Self = Fn->begin()->getArgument(0);

  DEBUG(llvm::dbgs() << "    Analyzing destructor.\n");

  // For each BB in the destructor...
  for (auto &BB : *Fn)
    // For each instruction I in BB...
    for (auto &I : BB) {
      DEBUG(llvm::dbgs() << "        Visiting: " << I);

      // If I has no side effects, we can ignore it.
      if (!I.mayHaveSideEffects()) {
        DEBUG(llvm::dbgs() << "            SAFE! Instruction has no side "
              "effects.\n");
        continue;
      }

      // RefCounting operations on Self are ok since we are already in the
      // destructor. RefCountingOperations on other instructions could have side
      // effects though.
      if (auto *RefInst = dyn_cast<RefCountingInst>(&I)) {
        if (stripCasts(RefInst->getOperand(0)) == Self) {
          // For now all ref counting insts have 1 operand. Put in an assert
          // just in case.
          assert(RefInst->getNumOperands() == 1 &&
                 "Make sure RefInst only has one argument.");
          DEBUG(llvm::dbgs() << "            SAFE! Ref count operation on "
                "Self.\n");
          continue;
        } else {
          DEBUG(llvm::dbgs() << "            UNSAFE! Ref count operation not on"
                " self.\n");
          return true;
        }
      }

      // dealloc_stack can be ignored.
      if (isa<DeallocStackInst>(I)) {
        DEBUG(llvm::dbgs() << "            SAFE! dealloc_stack can be "
              "ignored.\n");
        continue;
      }

      // dealloc_ref on self can be ignored, but dealloc_ref on anything else
      // cannot be eliminated.
      if (auto *DeallocRef = dyn_cast<DeallocRefInst>(&I)) {
        if (stripCasts(DeallocRef->getOperand()) == Self) {
          DEBUG(llvm::dbgs() << "            SAFE! dealloc_ref on self.\n");
          continue;
        } else {
          DEBUG(llvm::dbgs() << "            UNSAFE! dealloc_ref on value "
                "besides self.\n");
          return true;
        }
      }

      // Storing into the object can be ignored.
      if (auto *SI = dyn_cast<StoreInst>(&I))
        if (stripAddressProjections(SI->getDest()) == Self) {
          DEBUG(llvm::dbgs() << "            SAFE! Instruction is a store into "
                "self.\n");
          continue;
        }

      DEBUG(llvm::dbgs() << "            UNSAFE! Unknown instruction.\n");
      // Otherwise, we can't remove the deallocation completely.
      return true;
    }

  // We didn't find any side effects.
  return false;
}

void static
removeInstructions(ArrayRef<SILInstruction*> UsersToRemove) {
  for (auto *I : UsersToRemove) {
    if (!I->use_empty())
      I->replaceAllUsesWith(SILUndef::get(I->getType(), I->getModule()));
    // Now we know that I should not have any uses... erase it from its parent.
    I->eraseFromParent();
  }
}

//===----------------------------------------------------------------------===//
//                             Use Graph Analysis
//===----------------------------------------------------------------------===//

/// Returns false if Inst is an instruction that would require us to keep the
/// alloc_ref alive.
static bool canZapInstruction(SILInstruction *Inst) {
  // It is ok to eliminate various retains/releases. We are either removing
  // everything or nothing.
  if (isa<RefCountingInst>(Inst))
    return true;

  // If we see a store here, we have already checked that we are storing into
  // the pointer before we added it to the worklist, so we can skip it.
  if (isa<StoreInst>(Inst))
    return true;

  // If Inst does not read or write to memory, have side effects, and is not a
  // terminator, we can zap it.
  if (!Inst->mayHaveSideEffects() && !Inst->mayReadFromMemory() &&
      !isa<TermInst>(Inst))
    return true;

  // We know that the destructor has no side effects so we can remove the
  // deallocation instruction too.
  if (isa<DeallocationInst>(Inst))
    return true;

  // Much like deallocation, destroy addr is safe.
  if (isa<DestroyAddrInst>(Inst))
    return true;

  // Otherwise we do not know how to handle this instruction. Be conservative
  // and don't zap it.
  return false;
}

/// Analyze the use graph of AllocRef for any uses that would prevent us from
/// zapping it completely.
static bool
hasUnremovableUsers(SILInstruction *AllocRef, UserList &Users) {
  SmallVector<SILInstruction *, 16> Worklist;
  Worklist.push_back(AllocRef);

  DEBUG(llvm::dbgs() << "    Analyzing Use Graph.");

  while (!Worklist.empty()) {
    SILInstruction *I = Worklist.pop_back_val();

    DEBUG(llvm::dbgs() << "        Visiting: " << *I);

    // Insert the instruction into our InvolvedInstructions set.  If we have
    // already seen it, then don't reprocess all of the uses.
    if (!Users.insert(I)) {
      DEBUG(llvm::dbgs() << "        Already seen skipping...\n");
      continue;
    }

    // If we can't zap this instruction... bail...
    if (!canZapInstruction(I)) {
      DEBUG(llvm::dbgs() << "        Found instruction we can't zap...\n");
      return true;
    }

    // At this point, we can remove the instruction as long as all of its users
    // can be removed as well. Scan its users and add them to the worklist for
    // recursive processing.
    for (auto *Op : I->getUses()) {
      auto *User = Op->getUser();

      // Make sure that we are only storing into our users, not storing our
      // users which would be an escape.
      if (auto *SI = dyn_cast<StoreInst>(User))
        if (Op->get() == SI->getSrc()) {
          DEBUG(llvm::dbgs() << "        Found store of pointer. Failure: " <<
                *SI);
          return true;
        }

      // Otherwise, add normal instructions to the worklist for processing.
      Worklist.push_back(User);
    }
  }

  return false;
}

//===----------------------------------------------------------------------===//
//                     NonTrivial DeadObject Elimination
//===----------------------------------------------------------------------===//

namespace {
// Trie node representing a sequence of unsigned integer indices.
class IndexTrieNode {
  static const unsigned RootIdx = ~0U;
  unsigned Index;
  llvm::SmallVector<IndexTrieNode*, 8> Children;

public:
  IndexTrieNode(): Index(RootIdx) {}

  explicit IndexTrieNode(unsigned V): Index(V) {}

  IndexTrieNode(IndexTrieNode &) =delete;
  IndexTrieNode &operator=(const IndexTrieNode&) =delete;

  ~IndexTrieNode() {
    for (auto *N : Children)
      delete N;
  }

  bool isRoot() const { return Index == RootIdx; }

  bool isLeaf() const { return Children.empty(); }

  unsigned getIndex() const { return Index; }

  IndexTrieNode *getChild(unsigned Idx) {
    assert(Idx != RootIdx);

    auto I = std::lower_bound(Children.begin(), Children.end(), Idx,
                              [](IndexTrieNode *a, unsigned i) {
                                return a->Index < i;
                              });
    if (I != Children.end() && (*I)->Index == Idx)
      return *I;
    auto *N = new IndexTrieNode(Idx);
    Children.insert(I, N);
    return N;
  }

  ArrayRef<IndexTrieNode*> getChildren() const { return Children; }
};
} // end anonymous namespace

namespace {
/// Determine if an object is dead. Compute its original lifetime. Find the
/// lifetime endpoints reached by each store of a refcounted object into the
/// object.
///
/// TODO: Use this to remove nontrivial dead alloc_ref/alloc_stack, not just
/// dead arrays. We just need a slightly better destructor analysis to prove
/// that it only releases elements.
class DeadObjectAnalysis {
  // Map each address projection of this object to a list of stores.
  // Do not iterate over this map's entries.
  using AddressToStoreMap =
    llvm::DenseMap<IndexTrieNode*, llvm::SmallVector<StoreInst*, 4> >;

  // The value of the object's address at the point of allocation.
  SILValue NewAddrValue;

  // Track all users that extend the lifetime of the object.
  UserList AllUsers;

  // Trie of stored locations.
  IndexTrieNode *AddressProjectionTrie;

  // Track all stores of refcounted elements per address projection.
  AddressToStoreMap StoredLocations;

  // Are any uses behind a PointerToAddressInst?
  bool SeenPtrToAddr;

public:
  explicit DeadObjectAnalysis(SILValue V):
    NewAddrValue(V), AddressProjectionTrie(nullptr), SeenPtrToAddr(false) {}

  ~DeadObjectAnalysis() {
    delete AddressProjectionTrie;
  }

  bool analyze();

  ArrayRef<SILInstruction*> getAllUsers() const {
    return ArrayRef<SILInstruction*>(AllUsers.begin(), AllUsers.end());
  }

  template<typename Visitor>
  void visitStoreLocations(Visitor visitor) {
    visitStoreLocations(visitor, AddressProjectionTrie);
  }

private:
  void addStore(StoreInst *Store, IndexTrieNode *AddressNode);
  bool recursivelyCollectInteriorUses(ValueBase *DefInst,
                                      IndexTrieNode *AddressNode,
                                      bool IsInteriorAddress);

  template<typename Visitor>
  void visitStoreLocations(Visitor visitor, IndexTrieNode *AddressNode);
};
} // end anonymous namespace

// Record a store into this object.
void DeadObjectAnalysis::
addStore(StoreInst *Store, IndexTrieNode *AddressNode) {
  if (Store->getSrc()->getType().isTrivial(Store->getModule()))
    return;

  // SSAUpdater cannot handle multiple defs in the same blocks. Therefore, we
  // ensure that only one store per block is present in the StoredLocations.
  auto &StoredLocs = StoredLocations[AddressNode];
  for (auto &OtherSt : StoredLocs) {
    // In case the object's address is stored in itself.
    if (OtherSt == Store)
      return;

    if (OtherSt->getParent() == Store->getParent()) {
      for (auto II = std::next(Store->getIterator()),
                IE = Store->getParent()->end();
           II != IE; ++II) {
        if (&*II == OtherSt)
          return; // Keep the other store.
      }
      // Replace OtherSt with this store.
      OtherSt = Store;
      return;
    }
  }
  StoredLocations[AddressNode].push_back(Store);
}

// Collect instructions that either initialize or release any values at the
// object defined by defInst.
//
// Populates AllUsers, AddressProjectionTrie, and StoredLocations.
//
// If a use is visited that potentially causes defInst's address to
// escape, then return false without fully populating the data structures.
//
// `InteriorAddress` is true if the current address projection already includes
// a struct/ref/tuple element address. index_addr is only expected at the top
// level. The first non-index element address encountered pushes an "zero index"
// address node to represent the implicit index_addr #0. We do not support
// nested indexed data types in native SIL.
bool DeadObjectAnalysis::
recursivelyCollectInteriorUses(ValueBase *DefInst,
                               IndexTrieNode* AddressNode,
                               bool IsInteriorAddress) {
  for (auto Op : DefInst->getUses()) {
    auto User = Op->getUser();

    // Lifetime endpoints that don't allow the address to escape.
    if (isa<RefCountingInst>(User) || isa<DebugValueInst>(User)) {
      AllUsers.insert(User);
      continue;
    }
    // Initialization points.
    if (auto *Store = dyn_cast<StoreInst>(User)) {
      // Bail if this address is stored to another object.
      if (Store->getDest() != DefInst) {
        DEBUG(llvm::dbgs() << "        Found an escaping store: " << *User);
        return false;
      }
      IndexTrieNode *StoreAddrNode = AddressNode;
      // Push an extra zero index node for a store to noninterior address.
      if (!IsInteriorAddress)
        StoreAddrNode = AddressNode->getChild(0);

      addStore(Store, StoreAddrNode);

      AllUsers.insert(User);
      continue;
    }
    if (isa<PointerToAddressInst>(User)) {
      // Only one pointer-to-address is allowed for safety.
      if (SeenPtrToAddr)
        return false;

      SeenPtrToAddr = true;
      if (!recursivelyCollectInteriorUses(User, AddressNode, IsInteriorAddress))
        return false;

      continue;
    }
    // Recursively follow projections.
    ProjectionIndex PI(User);
    if (PI.isValid()) {
      IndexTrieNode *ProjAddrNode = AddressNode;
      bool ProjInteriorAddr = IsInteriorAddress;
      if (Projection::isAddressProjection(User)) {
        if (User->getKind() == ValueKind::IndexAddrInst) {
          // Don't support indexing within an interior address.
          if (IsInteriorAddress)
            return false;
        }
        else if (!IsInteriorAddress) {
          // Push an extra zero index node for the first interior address.
          ProjAddrNode = AddressNode->getChild(0);
          ProjInteriorAddr = true;
        }
      }
      else if (IsInteriorAddress) {
        // Don't expect to extract values once we've taken an address.
        return false;
      }
      if (!recursivelyCollectInteriorUses(User,
                                          ProjAddrNode->getChild(PI.Index),
                                          ProjInteriorAddr)) {
        return false;
      }
      continue;
    }
    // Otherwise bail.
    DEBUG(llvm::dbgs() << "        Found an escaping use: " << *User);
    return false;
  }
  return true;
}

// Track the lifetime, release points, and released values referenced by a
// newly allocated object.
bool DeadObjectAnalysis::analyze() {
  DEBUG(llvm::dbgs() << "    Analyzing nontrivial dead object: "
        << NewAddrValue);

  // Populate AllValues, AddressProjectionTrie, and StoredLocations.
  AddressProjectionTrie = new IndexTrieNode();
  if (!recursivelyCollectInteriorUses(NewAddrValue,
                                      AddressProjectionTrie, false)) {
    return false;
  }
  // If all stores are leaves in the AddressProjectionTrie, then we can analyze
  // the stores that reach the end of the object lifetime. Otherwise bail.
  // This iteration order is nondeterministic but has no impact.
  for (auto &AddressToStoresPair : StoredLocations) {
    IndexTrieNode *Location = AddressToStoresPair.first;
    if (!Location->isLeaf())
      return false;
  }
  return true;
}

template<typename Visitor>
void DeadObjectAnalysis::
visitStoreLocations(Visitor visitor, IndexTrieNode *AddressNode) {
  if (AddressNode->isLeaf()) {
    auto LocI = StoredLocations.find(AddressNode);
    if (LocI != StoredLocations.end())
      visitor(LocI->second);
    return;
  }
  for (auto *SubAddressNode : AddressNode->getChildren())
    visitStoreLocations(visitor, SubAddressNode);
}

// At each release point, release the reaching values that have been stored to
// this address.
//
// The caller has already determined that all Stores are to the same element
// within an otherwise dead object.
static void insertReleases(ArrayRef<StoreInst*> Stores,
                           ArrayRef<SILInstruction*> ReleasePoints,
                           SILSSAUpdater &SSAUp) {
  assert(!Stores.empty());
  SILValue StVal = Stores.front()->getSrc();

  SSAUp.Initialize(StVal->getType());

  for (auto *Store : Stores)
    SSAUp.AddAvailableValue(Store->getParent(), Store->getSrc());

  SILLocation Loc = Stores[0]->getLoc();
  for (auto *RelPoint : ReleasePoints) {
    SILBuilder B(RelPoint);
    // This does not use the SSAUpdater::RewriteUse API because it does not do
    // the right thing for local uses. We have already ensured a single store
    // per block, and all release points occur after all stores. Therefore we
    // can simply ask SSAUpdater for the reaching store.
    SILValue RelVal = SSAUp.GetValueAtEndOfBlock(RelPoint->getParent());
    if (StVal->getType().isReferenceCounted(RelPoint->getModule()))
      B.createStrongRelease(Loc, RelVal, Atomicity::Atomic);
    else
      B.createReleaseValue(Loc, RelVal, Atomicity::Atomic);
  }
}

// Attempt to remove the array allocated at NewAddrValue and release its
// refcounted elements.
//
// This is tightly coupled with the implementation of array.uninitialized.
// The call to allocate an uninitialized array returns two values:
// (Array<E> ArrayBase, UnsafeMutable<E> ArrayElementStorage)
//
// TODO: This relies on the lowest level array.uninitialized not being
// inlined. To do better we could either run this pass before semantic inlining,
// or we could also handle calls to array.init.
static bool removeAndReleaseArray(SILValue NewArrayValue, bool &CFGChanged) {
  TupleExtractInst *ArrayDef = nullptr;
  TupleExtractInst *StorageAddress = nullptr;
  for (auto *Op : NewArrayValue->getUses()) {
    auto *TupleElt = dyn_cast<TupleExtractInst>(Op->getUser());
    if (!TupleElt)
      return false;
    if (TupleElt->getFieldNo() == 0 && !ArrayDef) {
      ArrayDef = TupleElt;
    } else if (TupleElt->getFieldNo() == 1 && !StorageAddress) {
      StorageAddress = TupleElt;
    } else {
      return false;
    }
  }
  if (!ArrayDef)
    return false; // No Array object to delete.

  assert(!ArrayDef->getType().isTrivial(ArrayDef->getModule()) &&
         "Array initialization should produce the proper tuple type.");

  // Analyze the array object uses.
  DeadObjectAnalysis DeadArray(ArrayDef);
  if (!DeadArray.analyze())
    return false;

  // Require all stores to be into the array storage not the array object,
  // otherwise bail.
  bool HasStores = false;
  DeadArray.visitStoreLocations([&](ArrayRef<StoreInst*>){ HasStores = true; });
  if (HasStores)
    return false;

  // Remove references to empty arrays.
  if (!StorageAddress) {
    removeInstructions(DeadArray.getAllUsers());
    return true;
  }
  assert(StorageAddress->getType().isTrivial(ArrayDef->getModule()) &&
         "Array initialization should produce the proper tuple type.");

  // Analyze the array storage uses.
  DeadObjectAnalysis DeadStorage(StorageAddress);
  if (!DeadStorage.analyze())
    return false;

  // Find array object lifetime.
  ValueLifetimeAnalysis VLA(NewArrayValue, DeadArray.getAllUsers());

  // Check that all storage users are in the Array's live blocks.
  for (auto *User : DeadStorage.getAllUsers()) {
    if (!VLA.isWithinLifetime(User))
      return false;
  }
  // For each store location, insert releases.
  // This makes a strong assumption that the allocated object is released on all
  // paths in which some object initialization occurs.
  SILSSAUpdater SSAUp;
  ValueLifetimeAnalysis::Frontier ArrayFrontier;
  CFGChanged |= !VLA.computeFrontier(ArrayFrontier,
                                     ValueLifetimeAnalysis::IgnoreExitEdges);

  DeadStorage.visitStoreLocations([&] (ArrayRef<StoreInst*> Stores) {
      insertReleases(Stores, ArrayFrontier, SSAUp);
    });

  // Delete all uses of the dead array and its storage address.
  removeInstructions(DeadArray.getAllUsers());
  removeInstructions(DeadStorage.getAllUsers());

  return true;
}

//===----------------------------------------------------------------------===//
//                            Function Processing
//===----------------------------------------------------------------------===//

/// Does this instruction perform object allocation with no other observable
/// side effect?
static bool isAllocatingApply(SILInstruction *Inst) {
  ArraySemanticsCall ArrayAlloc(Inst);
  return ArrayAlloc.getKind() == ArrayCallKind::kArrayUninitialized;
}

namespace {
class DeadObjectElimination : public SILFunctionTransform {
  llvm::DenseMap<SILType, bool> DestructorAnalysisCache;
  llvm::SmallVector<SILInstruction*, 16> Allocations;
  bool CFGChanged = false;

  void collectAllocations(SILFunction &Fn) {
    for (auto &BB : Fn)
      for (auto &II : BB) {
        if (isa<AllocationInst>(&II))
          Allocations.push_back(&II);
        else if (isAllocatingApply(&II))
          Allocations.push_back(&II);
      }
  }

  bool processAllocRef(AllocRefInst *ARI);
  bool processAllocStack(AllocStackInst *ASI);
  bool processAllocBox(AllocBoxInst *ABI){ return false;}
  bool processAllocApply(ApplyInst *AI);

  bool processFunction(SILFunction &Fn) {
    Allocations.clear();
    DestructorAnalysisCache.clear();
    bool Changed = false;
    collectAllocations(Fn);
    for (auto *II : Allocations) {
      if (auto *A = dyn_cast<AllocRefInst>(II))
        Changed |= processAllocRef(A);
      else if (auto *A = dyn_cast<AllocStackInst>(II))
        Changed |= processAllocStack(A);
      else if (auto *A = dyn_cast<AllocBoxInst>(II))
        Changed |= processAllocBox(A);
      else if (auto *A = dyn_cast<ApplyInst>(II))
        Changed |= processAllocApply(A);
    }
    return Changed;
  }

  void run() override {
    CFGChanged = false;
    if (processFunction(*getFunction())) {
      invalidateAnalysis(CFGChanged ?
                         SILAnalysis::InvalidationKind::FunctionBody :
                         SILAnalysis::InvalidationKind::CallsAndInstructions);
    }
  }

  StringRef getName() override { return "Dead Object Elimination"; }
};
} // end anonymous namespace

bool DeadObjectElimination::processAllocRef(AllocRefInst *ARI) {
  // Ok, we have an alloc_ref. Check the cache to see if we have already
  // computed the destructor behavior for its SILType.
  bool HasSideEffects;
  SILType Type = ARI->getType();
  auto CacheSearchResult = DestructorAnalysisCache.find(Type);
  if (CacheSearchResult != DestructorAnalysisCache.end()) {
    // Ok we found a value in the cache.
    HasSideEffects = CacheSearchResult->second;
  } else {
    // We did not find a value in the cache for our destructor. Analyze the
    // destructor to make sure it has no side effects. For now this only
    // supports alloc_ref of classes so any alloc_ref with a reference type
    // that is not a class this will return false for. Once we have analyzed
    // it, set Behavior to that value and insert the value into the Cache.
    //
    // TODO: We should be able to handle destructors that do nothing but release
    // members of the object.
    HasSideEffects = doesDestructorHaveSideEffects(ARI);
    DestructorAnalysisCache[Type] = HasSideEffects;
  }

  if (HasSideEffects) {
    DEBUG(llvm::dbgs() << " Destructor had side effects. \n");
    return false;
  }

  // Our destructor has no side effects, so if we can prove that no loads
  // escape, then we can completely remove the use graph of this alloc_ref.
  UserList UsersToRemove;
  if (hasUnremovableUsers(ARI, UsersToRemove)) {
    DEBUG(llvm::dbgs() << "    Found a use that cannot be zapped...\n");
    return false;
  }

  // Remove the AllocRef and all of its users.
  removeInstructions(
    ArrayRef<SILInstruction*>(UsersToRemove.begin(), UsersToRemove.end()));
  DEBUG(llvm::dbgs() << "    Success! Eliminating alloc_ref.\n");

  ++DeadAllocRefEliminated;
  return true;
}

bool DeadObjectElimination::processAllocStack(AllocStackInst *ASI) {
  // Trivial types don't have destructors. Let's try to zap this AllocStackInst.
  if (!ASI->getElementType().isTrivial(ASI->getModule()))
    return false;

  UserList UsersToRemove;
  if (hasUnremovableUsers(ASI, UsersToRemove)) {
    DEBUG(llvm::dbgs() << "    Found a use that cannot be zapped...\n");
    return false;
  }

  // Remove the AllocRef and all of its users.
  removeInstructions(
    ArrayRef<SILInstruction*>(UsersToRemove.begin(), UsersToRemove.end()));
  DEBUG(llvm::dbgs() << "    Success! Eliminating alloc_stack.\n");

  ++DeadAllocStackEliminated;
  return true;
}

bool DeadObjectElimination::processAllocApply(ApplyInst *AI) {
  // Currently only handle array.uninitialized
  if (ArraySemanticsCall(AI).getKind() != ArrayCallKind::kArrayUninitialized)
    return false;

  ApplyInst *AllocBufferAI = nullptr;
  SILValue Arg0 = AI->getArgument(0);
  if (Arg0->getType().isExistentialType()) {
    // This is a version of the initializer which receives a pre-allocated
    // buffer as first argument. If we want to delete the initializer we also
    // have to delete the allocation.
    AllocBufferAI = dyn_cast<ApplyInst>(Arg0);
    if (!AllocBufferAI)
      return false;
  }

  if (!removeAndReleaseArray(AI, CFGChanged))
    return false;

  DEBUG(llvm::dbgs() << "    Success! Eliminating apply allocate(...).\n");

  eraseUsesOfInstruction(AI);
  assert(AI->use_empty() && "All users should have been removed.");
  recursivelyDeleteTriviallyDeadInstructions(AI, true);
  if (AllocBufferAI) {
    recursivelyDeleteTriviallyDeadInstructions(AllocBufferAI, true);
  }
  ++DeadAllocApplyEliminated;
  return true;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

SILTransform *swift::createDeadObjectElimination() {
  return new DeadObjectElimination();
}
