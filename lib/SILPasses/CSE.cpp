//===- CSE.cpp - Simple and fast CSE pass ---------------------------------===//
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
//
// This pass performs a simple dominator tree walk that eliminates trivially
// redundant instructions.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-cse"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/RecyclingAllocator.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/Subsystems.h"

STATISTIC(NumSimplify, "Number of instructions simplified or DCE'd");
STATISTIC(NumCSE,      "Number of instructions CSE'd");
STATISTIC(NumCSELoad,  "Number of load instructions CSE'd");
STATISTIC(NumDSE,      "Number of trivial dead stores removed");

using namespace swift;

//===----------------------------------------------------------------------===//
//                                Simple Value
//===----------------------------------------------------------------------===//

namespace {
  /// SimpleValue - Instances of this struct represent available values in the
  /// scoped hash table.
  struct SimpleValue {
    SILInstruction *Inst;

    SimpleValue(SILInstruction *I) : Inst(I) {
      assert((isSentinel() || canHandle(I)) && "Inst can't be handled!");
    }

    bool isSentinel() const {
      return Inst == llvm::DenseMapInfo<SILInstruction*>::getEmptyKey() ||
             Inst == llvm::DenseMapInfo<SILInstruction*>::getTombstoneKey();
    }

    static bool canHandle(SILInstruction *Inst) {
      switch(Inst->getKind()) {     
      case ValueKind::FunctionRefInst:
      case ValueKind::BuiltinFunctionRefInst:
      case ValueKind::GlobalAddrInst:
      case ValueKind::IntegerLiteralInst:
        return true;
      default:
        return false;
      }
    }
  };
} // end anonymous namespace

namespace llvm {
template<> struct DenseMapInfo<SimpleValue> {
  static inline SimpleValue getEmptyKey() {
    return DenseMapInfo<SILInstruction*>::getEmptyKey();
  }
  static inline SimpleValue getTombstoneKey() {
    return DenseMapInfo<SILInstruction*>::getTombstoneKey();
  }
  static unsigned getHashValue(SimpleValue Val);
  static bool isEqual(SimpleValue LHS, SimpleValue RHS);
};
} // end namespace llvm

unsigned llvm::DenseMapInfo<SimpleValue>::getHashValue(SimpleValue Val) {
  SILInstruction *Inst = Val.Inst;
  
  switch (Inst->getKind()) {
  case ValueKind::FunctionRefInst: {
    auto *X = cast<FunctionRefInst>(Inst);
    return llvm::hash_combine(unsigned(ValueKind::FunctionRefInst),
                              X->getReferencedFunction());
  }
  case ValueKind::BuiltinFunctionRefInst: {
    auto *X = cast<BuiltinFunctionRefInst>(Inst);
    return llvm::hash_combine(unsigned(ValueKind::BuiltinFunctionRefInst),
                              X->getName().get());
  }
  case ValueKind::GlobalAddrInst: {
    auto *X = cast<GlobalAddrInst>(Inst);
    return llvm::hash_combine(unsigned(ValueKind::GlobalAddrInst),
                              X->getGlobal());
  }
  case ValueKind::IntegerLiteralInst: {
    auto *X = cast<IntegerLiteralInst>(Inst);
    return llvm::hash_combine(unsigned(ValueKind::IntegerLiteralInst),
                              X->getType(),
                              X->getValue());
  }
  default:
    llvm_unreachable("Unhandled ValueKind.");
  }
}

bool llvm::DenseMapInfo<SimpleValue>::isEqual(SimpleValue LHS,
                                              SimpleValue RHS) {
  SILInstruction *LHSI = LHS.Inst, *RHSI = RHS.Inst;

  if (LHS.isSentinel() || RHS.isSentinel())
    return LHSI == RHSI;

  if (LHSI->getKind() != RHSI->getKind()) return false;
  if (LHSI->isIdenticalTo(RHSI)) return true;

  return false;
}

//===----------------------------------------------------------------------===//
//                               CSE Interface
//===----------------------------------------------------------------------===//

namespace {

/// CSE - This pass does a simple depth-first walk over the dominator tree,
/// eliminating trivially redundant instructions and using simplifyInstruction
/// to canonicalize things as it goes. It is intended to be fast and catch
/// obvious cases so that SILCombine and other passes are more effective.
class CSE {
public:
  SILModule &Module;
  DominanceInfo *DT;

  typedef llvm::ScopedHashTableVal<SimpleValue, ValueBase*> SimpleValueHTType;
  typedef llvm::RecyclingAllocator<llvm::BumpPtrAllocator,
                                   SimpleValueHTType> AllocatorTy;
  typedef llvm::ScopedHashTable<SimpleValue, ValueBase*,
                                llvm::DenseMapInfo<SimpleValue>,
                                AllocatorTy> ScopedHTType;

  /// AvailableValues - This scoped hash table contains the current values of
  /// all of our simple scalar expressions.  As we walk down the domtree, we
  /// look to see if instructions are in this: if so, we replace them with what
  /// we find, otherwise we insert them so that dominated values can succeed in
  /// their lookup.
  ScopedHTType *AvailableValues;

  /// AvailableLoads - This scoped hash table contains the current values
  /// of loads.  This allows us to get efficient access to dominating loads when
  /// we have a fully redundant load.  In addition to the most recent load, we
  /// keep track of a generation count of the read, which is compared against
  /// the current generation count.  The current generation count is
  /// incremented after every possibly writing memory operation, which ensures
  /// that we only CSE loads with other loads that have no intervening store.
  typedef llvm::ScopedHashTableVal<ValueBase*,
                                   std::pair<ValueBase*, unsigned>> ValueHTType;
  typedef llvm::RecyclingAllocator<llvm::BumpPtrAllocator,
                                   ValueHTType> LoadMapAllocator;
  typedef llvm::ScopedHashTable<ValueBase*, std::pair<ValueBase*, unsigned>,
                                llvm::DenseMapInfo<ValueBase*>,
                                LoadMapAllocator> LoadHTType;
  LoadHTType *AvailableLoads;

  /// CurrentGeneration - This is the current generation of the memory value.
  unsigned CurrentGeneration;

  explicit CSE(SILModule &M) : Module(M) { }

  bool runOnFunction(SILFunction &F);

private:

  // NodeScope - almost a POD, but needs to call the constructors for the
  // scoped hash tables so that a new scope gets pushed on. These are RAII so
  // that the scope gets popped when the NodeScope is destroyed.
  class NodeScope {
   public:
    NodeScope(ScopedHTType *availableValues,
              LoadHTType *availableLoads) :
        Scope(*availableValues),
        LoadScope(*availableLoads) { }

   private:
    NodeScope(const NodeScope&) = delete;
    void operator=(const NodeScope&) = delete;

    ScopedHTType::ScopeTy Scope;
    LoadHTType::ScopeTy LoadScope;
  };

  // StackNode - contains all the needed information to create a stack for doing
  // a depth first traversal of the tree. This includes scopes for values and
  // loads as well as the generation. There is a child iterator so that the
  // children do not need to be store spearately.
  class StackNode {
   public:
    StackNode(ScopedHTType *availableValues,
              LoadHTType *availableLoads,
              unsigned cg, DominanceInfoNode *n,
              DominanceInfoNode::iterator child,
              DominanceInfoNode::iterator end) :
        CurrentGeneration(cg), ChildGeneration(cg), Node(n),
        ChildIter(child), EndIter(end),
        Scopes(availableValues, availableLoads),
        Processed(false) {}

    // Accessors.
    unsigned currentGeneration() { return CurrentGeneration; }
    unsigned childGeneration() { return ChildGeneration; }
    void childGeneration(unsigned generation) { ChildGeneration = generation; }
    DominanceInfoNode *node() { return Node; }
    DominanceInfoNode::iterator childIter() { return ChildIter; }
    DominanceInfoNode *nextChild() {
      DominanceInfoNode *child = *ChildIter;
      ++ChildIter;
      return child;
    }
    DominanceInfoNode::iterator end() { return EndIter; }
    bool isProcessed() { return Processed; }
    void process() { Processed = true; }

   private:
    StackNode(const StackNode&) = delete;
    void operator=(const StackNode&) = delete;

    // Members.
    unsigned CurrentGeneration;
    unsigned ChildGeneration;
    DominanceInfoNode *Node;
    DominanceInfoNode::iterator ChildIter;
    DominanceInfoNode::iterator EndIter;
    NodeScope Scopes;
    bool Processed;
  };

  bool processNode(DominanceInfoNode *Node);
};
}

//===----------------------------------------------------------------------===//
//                             CSE Implementation
//===----------------------------------------------------------------------===//

bool CSE::runOnFunction(SILFunction &F) {
  std::deque<StackNode *> nodesToProcess;

  DominanceInfo DT(&F);

  // Tables that the pass uses when walking the domtree.
  ScopedHTType AVTable;
  AvailableValues = &AVTable;
  LoadHTType LoadTable;
  AvailableLoads = &LoadTable;

  CurrentGeneration = 0;
  bool Changed = false;

  // Process the root node.
  nodesToProcess.push_front(
    new StackNode(AvailableValues, AvailableLoads,
                  CurrentGeneration, DT.getRootNode(),
                  DT.getRootNode()->begin(),
                  DT.getRootNode()->end()));

  // Save the current generation.
  unsigned LiveOutGeneration = CurrentGeneration;

  // Process the stack.
  while (!nodesToProcess.empty()) {
    // Grab the first item off the stack. Set the current generation, remove
    // the node from the stack, and process it.
    StackNode *NodeToProcess = nodesToProcess.front();

    // Initialize class members.
    CurrentGeneration = NodeToProcess->currentGeneration();

    // Check if the node needs to be processed.
    if (!NodeToProcess->isProcessed()) {
      // Process the node.
      Changed |= processNode(NodeToProcess->node());
      NodeToProcess->childGeneration(CurrentGeneration);
      NodeToProcess->process();

    } else if (NodeToProcess->childIter() != NodeToProcess->end()) {
      // Push the next child onto the stack.
      DominanceInfoNode *child = NodeToProcess->nextChild();
      nodesToProcess.push_front(
          new StackNode(AvailableValues,
                        AvailableLoads,
                        NodeToProcess->childGeneration(), child,
                        child->begin(), child->end()));
    } else {
      // It has been processed, and there are no more children to process,
      // so delete it and pop it off the stack.
      delete NodeToProcess;
      nodesToProcess.pop_front();
    }
  } // while (!nodes...)

  // Reset the current generation.
  CurrentGeneration = LiveOutGeneration;

  return Changed;
}

bool CSE::processNode(DominanceInfoNode *Node) {
  SILBasicBlock *BB = Node->getBlock();

  // If this block has a single predecessor, then the predecessor is the parent
  // of the domtree node and all of the live out memory values are still current
  // in this block.  If this block has multiple predecessors, then they could
  // have invalidated the live-out memory values of our parent value.  For now,
  // just be conservative and invalidate memory if this block has multiple
  // predecessors.
  if (!BB->getSinglePredecessor())
    ++CurrentGeneration;

  /// LastStore - Keep track of the last store that we saw... for as long as
  /// there in no instruction that reads memory.  If we see a store to the same
  /// location, we delete the dead store.  This zaps trivial dead stores which
  /// can occur in bitfield code among other things.
  StoreInst *LastStore = 0;

  bool Changed = false;

  // See if any instructions in the block can be eliminated.  If so, do it.  If
  // not, add them to AvailableValues.
  for (SILBasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; ) {
    SILInstruction *Inst = I++;

    DEBUG(llvm::dbgs() << "SILCSE VISITING: " << *Inst << "\n");

    // Dead instructions should just be removed.
    if (isInstructionTriviallyDead(Inst)) {
      DEBUG(llvm::dbgs() << "SILCSE DCE: " << *Inst << '\n');
      Inst->eraseFromParent();
      Changed = true;
      ++NumSimplify;
      continue;
    }

    // If the instruction can be simplified (e.g. X+0 = X) then replace it with
    // its simpler value.
    if (SILValue V = simplifyInstruction(Inst)) {
      DEBUG(llvm::dbgs() << "SILCSE SIMPLIFY: " << *Inst << "  to: " << *V
            << '\n');
      // All operands of SimplifyInstruction currently have only one result.
      Inst->replaceAllUsesWith(V.getDef());
      Inst->eraseFromParent();
      Changed = true;
      ++NumSimplify;
      continue;
    }

    // If this is a simple instruction that we can value number, process it.
    if (SimpleValue::canHandle(Inst)) {
      // See if the instruction has an available value.  If so, use it.
      if (ValueBase *V = AvailableValues->lookup(Inst)) {
        DEBUG(llvm::dbgs() << "SILCSE CSE: " << *Inst << "  to: " << *V
              << '\n');
        Inst->replaceAllUsesWith(V);
        Inst->eraseFromParent();
        Changed = true;
        ++NumCSE;
        continue;
      }

      // Otherwise, just remember that this value is available.
      AvailableValues->insert(Inst, Inst);
      DEBUG(llvm::dbgs() << "SILCSE Adding to value table: " << *Inst
            << " -> " << *Inst << "\n");
      continue;
    }

    // If this is a load, process it.
    if (isa<LoadInst>(Inst)) {
      // If we have an available version of this load, and if it is the right
      // generation, replace this instruction.
      std::pair<ValueBase*, unsigned> InVal =
        AvailableLoads->lookup(&*Inst->getOperand(0));
      if (InVal.first != 0 && InVal.second == CurrentGeneration) {
        DEBUG(llvm::dbgs() << "SILCSE CSE LOAD: " << *Inst << "  to: "
              << *InVal.first << '\n');
        if (!Inst->use_empty()) {
          Inst->replaceAllUsesWith(InVal.first);
        }
        Inst->eraseFromParent();
        Changed = true;
        ++NumCSELoad;
        continue;
      }

      // Otherwise, remember that we have this instruction.
      AvailableLoads->insert(&*Inst->getOperand(0),
                             std::make_pair(Inst, CurrentGeneration));
      DEBUG(llvm::dbgs() << "SILCSE Adding to load table: " <<
            *Inst->getOperand(0) << " -> " << *Inst << "\n");
      LastStore = 0;
      continue;
    }

    // If this instruction may read from memory, forget LastStore.
    if (Inst->mayReadFromMemory())
      LastStore = 0;

    // Okay, this isn't something we can CSE at all.  Check to see if it is
    // something that could modify memory.  If so, our available memory values
    // cannot be used so bump the generation count.
    if (Inst->mayWriteToMemory()) {
      ++CurrentGeneration;

      if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
        // We do a trivial form of DSE if there are two stores to the same
        // location with no intervening loads.  Delete the earlier store.
        if (LastStore &&
            LastStore->getDest().getDef() == SI->getDest().getDef()) {
          DEBUG(llvm::dbgs() << "SILCSE DEAD STORE: " << *LastStore <<
                "  due to: " << *Inst << '\n');
          LastStore->eraseFromParent();
          Changed = true;
          ++NumDSE;
          LastStore = 0;
          continue;
        }

        // Okay, we just invalidated anything we knew about loaded values.  Try
        // to salvage *something* by remembering that the stored value is a live
        // version of the pointer.
        AvailableLoads->insert(&*SI->getDest(),
                               std::make_pair(&*SI->getSrc(),
                                              CurrentGeneration));

        DEBUG(llvm::dbgs() << "SILCSE: Adding stored value to table: "
              << *SI->getDest() << " -> " << *SI->getSrc() << "\n");

        LastStore = SI;
      }
    }
  }

  return Changed;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILCSE(SILModule *M) {
  CSE C(*M);
  for (SILFunction &F : *M) {
    // If F is just a declaration and not a definition, skip it since it has no
    // BB's to process.
    if (F.empty())
      continue;

    C.runOnFunction(F);
  }
}
