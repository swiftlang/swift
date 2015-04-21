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
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/SimplifyInstruction.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/RecyclingAllocator.h"

STATISTIC(NumSimplify, "Number of instructions simplified or DCE'd");
STATISTIC(NumCSE,      "Number of instructions CSE'd");

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
    return Inst == llvm::DenseMapInfo<SILInstruction *>::getEmptyKey() ||
           Inst == llvm::DenseMapInfo<SILInstruction *>::getTombstoneKey();
  }

  static bool canHandle(SILInstruction *Inst) {
    if (auto *AI = dyn_cast<ApplyInst>(Inst)) {
      return !AI->mayReadOrWriteMemory();
    }
    if (auto *BI = dyn_cast<BuiltinInst>(Inst)) {
      return !BI->mayReadOrWriteMemory();
    }
    switch (Inst->getKind()) {
    case ValueKind::FunctionRefInst:
    case ValueKind::GlobalAddrInst:
    case ValueKind::IntegerLiteralInst:
    case ValueKind::FloatLiteralInst:
    case ValueKind::StringLiteralInst:
    case ValueKind::StructInst:
    case ValueKind::StructExtractInst:
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleInst:
    case ValueKind::TupleExtractInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::MetatypeInst:
    case ValueKind::NullClassInst:
    case ValueKind::ValueMetatypeInst:
    case ValueKind::ExistentialMetatypeInst:
    case ValueKind::ObjCProtocolInst:
    case ValueKind::RefElementAddrInst:
    case ValueKind::IndexRawPointerInst:
    case ValueKind::IndexAddrInst:
    case ValueKind::PointerToAddressInst:
    case ValueKind::AddressToPointerInst:
    case ValueKind::CondFailInst:
    case ValueKind::EnumInst:
    case ValueKind::UncheckedEnumDataInst:
    case ValueKind::IsNonnullInst:
    case ValueKind::UncheckedRefBitCastInst:
    case ValueKind::UncheckedTrivialBitCastInst:
    case ValueKind::RefToRawPointerInst:
    case ValueKind::RawPointerToRefInst:
    case ValueKind::RefToUnownedInst:
    case ValueKind::UnownedToRefInst:
    case ValueKind::RefToUnmanagedInst:
    case ValueKind::UnmanagedToRefInst:
    case ValueKind::UpcastInst:
    case ValueKind::ThickToObjCMetatypeInst:
    case ValueKind::ObjCToThickMetatypeInst:
    case ValueKind::UncheckedRefCastInst:
    case ValueKind::UncheckedAddrCastInst:
    case ValueKind::ObjCMetatypeToObjectInst:
    case ValueKind::ObjCExistentialMetatypeToObjectInst:
    case ValueKind::SelectEnumInst:
    case ValueKind::SelectValueInst:
    case ValueKind::RefToBridgeObjectInst:
    case ValueKind::BridgeObjectToRefInst:
    case ValueKind::BridgeObjectToWordInst:
    case ValueKind::ThinFunctionToPointerInst:
    case ValueKind::PointerToThinFunctionInst:
        return true;
    default:
        return false;
    }
  }
};
} // end anonymous namespace

namespace llvm {
template <> struct DenseMapInfo<SimpleValue> {
  static inline SimpleValue getEmptyKey() {
    return DenseMapInfo<SILInstruction *>::getEmptyKey();
  }
  static inline SimpleValue getTombstoneKey() {
    return DenseMapInfo<SILInstruction *>::getTombstoneKey();
  }
  static unsigned getHashValue(SimpleValue Val);
  static bool isEqual(SimpleValue LHS, SimpleValue RHS);
};
} // end namespace llvm

namespace {
class HashVisitor : public SILInstructionVisitor<HashVisitor, llvm::hash_code> {
  using hash_code = llvm::hash_code;

public:
  hash_code visitValueBase(ValueBase *) {
    llvm_unreachable("No hash implemented for the given type");
  }

  hash_code visitBridgeObjectToRefInst(BridgeObjectToRefInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getOperand());
  }

  hash_code visitBridgeObjectToWordInst(BridgeObjectToWordInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getOperand());
  }
  
  hash_code visitRefToBridgeObjectInst(RefToBridgeObjectInst *X) {
    OperandValueArrayRef Operands(X->getAllOperands());
    return llvm::hash_combine(
        X->getKind(), X->getType(),
        llvm::hash_combine_range(Operands.begin(), Operands.end()));
  }

  hash_code visitUncheckedRefBitCastInst(UncheckedRefBitCastInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getOperand());
  }

  hash_code visitUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getOperand());
  }

  hash_code visitUncheckedAddrCastInst(UncheckedAddrCastInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getOperand());
  }

  hash_code visitFunctionRefInst(FunctionRefInst *X) {
    return llvm::hash_combine(X->getKind(), X->getReferencedFunction());
  }

  hash_code visitGlobalAddrInst(GlobalAddrInst *X) {
    return llvm::hash_combine(X->getKind(), X->getReferencedGlobal());
  }

  hash_code visitIntegerLiteralInst(IntegerLiteralInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getValue());
  }

  hash_code visitFloatLiteralInst(FloatLiteralInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getBits());
  }

  hash_code visitRefElementAddrInst(RefElementAddrInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand(), X->getField());
  }

  hash_code visitRefToRawPointerInst(RefToRawPointerInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand());
  }

  hash_code visitRawPointerToRefInst(RawPointerToRefInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand());
  }

  hash_code visitUnownedToRefInst(UnownedToRefInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand());
  }

  hash_code visitRefToUnownedInst(RefToUnownedInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand());
  }

  hash_code visitUnmanagedToRefInst(UnmanagedToRefInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand());
  }

  hash_code visitRefToUnmanagedInst(RefToUnmanagedInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand());
  }

  hash_code visitUpcastInst(UpcastInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getOperand());
  }

  hash_code visitStringLiteralInst(StringLiteralInst *X) {
    return llvm::hash_combine(X->getKind(), X->getEncoding(), X->getValue());
  }

  hash_code visitStructInst(StructInst *X) {
    // This is safe since we are hashing the operands using the actual pointer
    // values of the values being used by the operand.
    OperandValueArrayRef Operands(X->getAllOperands());
    return llvm::hash_combine(X->getKind(), X->getStructDecl(),
      llvm::hash_combine_range(Operands.begin(), Operands.end()));
  }

  hash_code visitStructExtractInst(StructExtractInst *X) {
    return llvm::hash_combine(X->getKind(), X->getStructDecl(), X->getField(),
                              X->getOperand());
  }

  hash_code visitStructElementAddrInst(StructElementAddrInst *X) {
    return llvm::hash_combine(X->getKind(), X->getStructDecl(), X->getField(),
                              X->getOperand());
  }

  hash_code visitCondFailInst(CondFailInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand());
  }

  hash_code visitTupleInst(TupleInst *X) {
    OperandValueArrayRef Operands(X->getAllOperands());
    return llvm::hash_combine(X->getKind(), X->getTupleType(),
      llvm::hash_combine_range(Operands.begin(), Operands.end()));
  }

  hash_code visitTupleExtractInst(TupleExtractInst *X) {
    return llvm::hash_combine(X->getKind(), X->getTupleType(), X->getFieldNo(),
                              X->getOperand());
  }

  hash_code visitTupleElementAddrInst(TupleElementAddrInst *X) {
    return llvm::hash_combine(X->getKind(), X->getTupleType(), X->getFieldNo(),
                              X->getOperand());
  }

  hash_code visitMetatypeInst(MetatypeInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType());
  }

  hash_code visitValueMetatypeInst(ValueMetatypeInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getOperand());
  }

  hash_code visitExistentialMetatypeInst(ExistentialMetatypeInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType());
  }

  hash_code visitObjCProtocolInst(ObjCProtocolInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getProtocol());
  }

  hash_code visitIndexRawPointerInst(IndexRawPointerInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getBase(),
                              X->getIndex());
  }

  hash_code visitPointerToAddressInst(PointerToAddressInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getOperand());
  }

  hash_code visitAddressToPointerInst(AddressToPointerInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getOperand());
  }

  hash_code visitApplyInst(ApplyInst *X) {
    OperandValueArrayRef Operands(X->getAllOperands());
    return llvm::hash_combine(X->getKind(), X->getCallee(),
                              llvm::hash_combine_range(Operands.begin(),
                                                       Operands.end()),
                              X->hasSubstitutions());
  }

  hash_code visitBuiltinInst(BuiltinInst *X) {
    OperandValueArrayRef Operands(X->getAllOperands());
    return llvm::hash_combine(X->getKind(), X->getName().get(),
                              llvm::hash_combine_range(Operands.begin(),
                                                       Operands.end()),
                              X->hasSubstitutions());
  }
  
  hash_code visitEnumInst(EnumInst *X) {
    // We hash the enum by hashing its kind, element, and operand if it has one.
    if (!X->hasOperand())
      return llvm::hash_combine(X->getKind(), X->getElement());
    return llvm::hash_combine(X->getKind(), X->getElement(), X->getOperand());
  }

  hash_code visitUncheckedEnumDataInst(UncheckedEnumDataInst *X) {
    // We hash the enum by hashing its kind, element, and operand.
    return llvm::hash_combine(X->getKind(), X->getElement(), X->getOperand());
  }

  hash_code visitIndexAddrInst(IndexAddrInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType(), X->getBase(),
                              X->getIndex());
  }

  hash_code visitThickToObjCMetatypeInst(ThickToObjCMetatypeInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand(), X->getType());
  }

  hash_code visitObjCToThickMetatypeInst(ObjCToThickMetatypeInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand(), X->getType());
  }

  hash_code visitObjCMetatypeToObjectInst(ObjCMetatypeToObjectInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand(), X->getType());
  }

  hash_code visitObjCExistentialMetatypeToObjectInst(
      ObjCExistentialMetatypeToObjectInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand(), X->getType());
  }

  hash_code visitUncheckedRefCastInst(UncheckedRefCastInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand(), X->getType());
  }

  hash_code visitSelectEnumInstBase(SelectEnumInstBase *X) {
    auto hash = llvm::hash_combine(X->getKind(),
                                   X->getEnumOperand(),
                                   X->getType(),
                                   X->hasDefault());
    
    for (unsigned i = 0, e = X->getNumCases(); i < e; ++i) {
      hash = llvm::hash_combine(hash, X->getCase(i).first,
                                X->getCase(i).second);
    }
    
    if (X->hasDefault())
      hash = llvm::hash_combine(hash, X->getDefaultResult());
    
    return hash;
  }
  
  hash_code visitSelectEnumInst(SelectEnumInst *X) {
    return visitSelectEnumInstBase(X);
  }

  hash_code visitSelectEnumAddrInst(SelectEnumAddrInst *X) {
    return visitSelectEnumInstBase(X);
  }

  hash_code visitSelectValueInst(SelectValueInst *X) {
    auto hash = llvm::hash_combine(X->getKind(),
                                   X->getOperand(),
                                   X->getType(),
                                   X->hasDefault());

    for (unsigned i = 0, e = X->getNumCases(); i < e; ++i) {
      hash = llvm::hash_combine(hash, X->getCase(i).first,
                                X->getCase(i).second);
    }

    if (X->hasDefault())
      hash = llvm::hash_combine(hash, X->getDefaultResult());

    return hash;
  }

  hash_code visitIsNonnullInst(IsNonnullInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand(), X->getType());
  }
  hash_code visitNullClassInst(NullClassInst *X) {
    return llvm::hash_combine(X->getKind(), X->getType());
  }

  hash_code visitThinFunctionToPointerInst(ThinFunctionToPointerInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand(), X->getType());
  }

  hash_code visitPointerToThinFunctionInst(PointerToThinFunctionInst *X) {
    return llvm::hash_combine(X->getKind(), X->getOperand(), X->getType());
  }
};
} // end anonymous namespace

unsigned llvm::DenseMapInfo<SimpleValue>::getHashValue(SimpleValue Val) {
  return HashVisitor().visit(Val.Inst);
}

bool llvm::DenseMapInfo<SimpleValue>::isEqual(SimpleValue LHS,
                                              SimpleValue RHS) {
  SILInstruction *LHSI = LHS.Inst, *RHSI = RHS.Inst;
  if (LHS.isSentinel() || RHS.isSentinel())
    return LHSI == RHSI;

  return LHSI->getKind() == RHSI->getKind() && LHSI->isIdenticalTo(RHSI);
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
  typedef llvm::ScopedHashTableVal<SimpleValue, ValueBase *> SimpleValueHTType;
  typedef llvm::RecyclingAllocator<llvm::BumpPtrAllocator, SimpleValueHTType>
  AllocatorTy;
  typedef llvm::ScopedHashTable<SimpleValue, ValueBase *,
                                llvm::DenseMapInfo<SimpleValue>,
                                AllocatorTy> ScopedHTType;

  /// AvailableValues - This scoped hash table contains the current values of
  /// all of our simple scalar expressions.  As we walk down the domtree, we
  /// look to see if instructions are in this: if so, we replace them with what
  /// we find, otherwise we insert them so that dominated values can succeed in
  /// their lookup.
  ScopedHTType *AvailableValues;

  CSE() {}

  bool processFunction(SILFunction &F, DominanceInfo *DT);

private:
  // NodeScope - almost a POD, but needs to call the constructors for the
  // scoped hash tables so that a new scope gets pushed on. These are RAII so
  // that the scope gets popped when the NodeScope is destroyed.
  class NodeScope {
   public:
    NodeScope(ScopedHTType *availableValues) : Scope(*availableValues) {}

   private:
    NodeScope(const NodeScope &) = delete;
    void operator=(const NodeScope &) = delete;

    ScopedHTType::ScopeTy Scope;
  };

  // StackNode - contains all the needed information to create a stack for doing
  // a depth first traversal of the tree. This includes scopes for values and
  // loads as well as the generation. There is a child iterator so that the
  // children do not need to be store spearately.
  class StackNode {
   public:
    StackNode(ScopedHTType *availableValues, DominanceInfoNode *n,
              DominanceInfoNode::iterator child,
              DominanceInfoNode::iterator end)
        : Node(n), ChildIter(child), EndIter(end), Scopes(availableValues),
      Processed(false) {}

    // Accessors.
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
    StackNode(const StackNode &) = delete;
    void operator=(const StackNode &) = delete;

    // Members.
    DominanceInfoNode *Node;
    DominanceInfoNode::iterator ChildIter;
    DominanceInfoNode::iterator EndIter;
    NodeScope Scopes;
    bool Processed;
  };

  bool processNode(DominanceInfoNode *Node);
};
}  // end anonymous namespace

//===----------------------------------------------------------------------===//
//                             CSE Implementation
//===----------------------------------------------------------------------===//

bool CSE::processFunction(SILFunction &Fm, DominanceInfo *DT) {
  std::vector<StackNode *> nodesToProcess;

  // Tables that the pass uses when walking the domtree.
  ScopedHTType AVTable;
  AvailableValues = &AVTable;

  bool Changed = false;

  // Process the root node.
  nodesToProcess.push_back(new StackNode(AvailableValues, DT->getRootNode(),
                  DT->getRootNode()->begin(),
                  DT->getRootNode()->end()));

  // Process the stack.
  while (!nodesToProcess.empty()) {
    // Grab the first item off the stack. Set the current generation, remove
    // the node from the stack, and process it.
    StackNode *NodeToProcess = nodesToProcess.back();

    // Check if the node needs to be processed.
    if (!NodeToProcess->isProcessed()) {
      // Process the node.
      Changed |= processNode(NodeToProcess->node());
      NodeToProcess->process();

    } else if (NodeToProcess->childIter() != NodeToProcess->end()) {
      // Push the next child onto the stack.
      DominanceInfoNode *child = NodeToProcess->nextChild();
      nodesToProcess.push_back(
          new StackNode(AvailableValues, child, child->begin(), child->end()));
    } else {
      // It has been processed, and there are no more children to process,
      // so delete it and pop it off the stack.
      delete NodeToProcess;
      nodesToProcess.pop_back();
    }
  } // while (!nodes...)

  return Changed;
}

bool CSE::processNode(DominanceInfoNode *Node) {
  SILBasicBlock *BB = Node->getBlock();
  bool Changed = false;

  // See if any instructions in the block can be eliminated.  If so, do it.  If
  // not, add them to AvailableValues.
  for (SILBasicBlock::iterator I = BB->begin(), E = BB->end(); I != E;) {
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
      SILValue(Inst, 0).replaceAllUsesWith(V);
      Inst->eraseFromParent();
      Changed = true;
      ++NumSimplify;
      continue;
    }

    // If this is not a simple instruction that we can value number, skip it.
    if (!SimpleValue::canHandle(Inst))
      continue;

    // If an instruction can be handled here, then it must also be handled
    // in isIdenticalTo, otherwise looking up a key in the map with fail to
    // match itself.
    assert(Inst->isIdenticalTo(Inst) &&
           "Inst must match itself for map to work");

    // Now that we know we have an instruction we understand see if the
    // instruction has an available value.  If so, use it.
    if (ValueBase *V = AvailableValues->lookup(Inst)) {
      DEBUG(llvm::dbgs() << "SILCSE CSE: " << *Inst << "  to: " << *V << '\n');
      Inst->replaceAllUsesWith(V);
      Inst->eraseFromParent();
      Changed = true;
      ++NumCSE;
      continue;
    }

    // Otherwise, just remember that this value is available.
    AvailableValues->insert(Inst, Inst);
    DEBUG(llvm::dbgs() << "SILCSE Adding to value table: " << *Inst << " -> "
                       << *Inst << "\n");
  }

  return Changed;
}

namespace {
class SILCSE : public SILFunctionTransform {
  void run() override {
    DEBUG(llvm::dbgs() << "***** CSE on function: " << getFunction()->getName()
          << " *****\n");

    DominanceAnalysis* DA = getAnalysis<DominanceAnalysis>();

    CSE C;
    if (C.processFunction(*getFunction(), DA->get(getFunction())))
      invalidateAnalysis(SILAnalysis::PreserveKind::ProgramFlow);
  }

  StringRef getName() override { return "CSE"; }
};
} // end anonymous namespace

SILTransform *swift::createCSE() {
  return new SILCSE();
}
