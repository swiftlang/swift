//===------------------- SCCVisitor.h - SIL SCC Visitor -------*- C++ -*---===//
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

#ifndef SWIFT_SIL_SCCVISITOR_H
#define SWIFT_SIL_SCCVISITOR_H

#include "swift/SILAnalysis/Analysis.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include <algorithm>
#include <tuple>

namespace swift {

/// A visitor class for visiting the instructions and basic block
/// arguments of a SIL function one strongly connected component at a
/// time in reverse post-order.
///
/// Inherit from this in the usual CRTP fashion and define a visit()
/// function. This function will get called with a vector of
/// pointer-to-ValueBase which are the elements of the SCC.
template <typename ImplClass>
class SCCVisitor {
public:
  SCCVisitor(SILFunction &F) : F(F), CurrentNum(0) {}
  ~SCCVisitor() {
    cleanup();
  }
  ImplClass &asImpl() {  return static_cast<ImplClass &>(*this); }

  void visit(llvm::SmallVectorImpl<ValueBase *> &SCC) { }

  void run() {
    llvm::ReversePostOrderTraversal<SILFunction *> RPOT(&F);

    for (auto Iter = RPOT.begin(), E = RPOT.end(); Iter != E; ++Iter) {
      auto *BB = *Iter;
      for (auto &I : *BB)
        if (!Visited.count(&I))
          DFS(&I);
    }

    cleanup();
  }

private:
  struct DFSInfo {
    ValueBase *Value;
    int DFSNum;
    int LowNum;

    DFSInfo(ValueBase *Value, int Num) : Value(Value), DFSNum(Num),
                                         LowNum(Num) {}
  };

  SILFunction &F;
  int CurrentNum;

  llvm::DenseSet<ValueBase *> Visited;
  llvm::SetVector<ValueBase *> DFSStack;
  typedef llvm::DenseMap<ValueBase *, DFSInfo *> ValueInfoMapType;
  ValueInfoMapType ValueInfoMap;

  void cleanup() {
    Visited.clear();
    DFSStack.clear();
    for (auto &Entry : ValueInfoMap)
      delete Entry.second;

    ValueInfoMap.clear();
    CurrentNum = 0;
  }

  DFSInfo &addDFSInfo(ValueBase *Value) {
    typename ValueInfoMapType::iterator Iter;
    bool Inserted;

    auto MapEntry = std::make_pair(Value, new DFSInfo(Value, CurrentNum++));
    std::tie(Iter, Inserted) = ValueInfoMap.insert(MapEntry);
    assert(Inserted && "Cannot add DFS info more than once for a value!");
    return *Iter->second;
  }

  DFSInfo &getDFSInfo(ValueBase *Value) {
    assert(ValueInfoMap.find(Value) != ValueInfoMap.end() &&
           "Expected to find value in DFS info map!");

    return *ValueInfoMap.find(Value)->second;
  }

  ValueBase *getArgForTerminator(TermInst *Term, SILBasicBlock *SuccBB,
                                 int Index) {
    switch (Term->getKind()) {
    case ValueKind::BranchInst:
      return cast<BranchInst>(Term)->getArg(Index).getDef();

    case ValueKind::CondBranchInst: {
      auto *CBI = cast<CondBranchInst>(Term);
      if (SuccBB == CBI->getTrueBB())
        return CBI->getTrueArgs()[Index].getDef();
      assert(SuccBB == CBI->getFalseBB() &&
             "Block is not a successor of terminator!");
      return CBI->getFalseArgs()[Index].getDef();
    }

    case ValueKind::SwitchEnumInst:
    case ValueKind::SwitchEnumAddrInst:
    case ValueKind::CheckedCastBranchInst:
    case ValueKind::CheckedCastAddrBranchInst:
    case ValueKind::DynamicMethodBranchInst:
      assert(Index == 0 && "Expected argument index to always be zero!");
      return Term->getOperand(0).getDef();

    case ValueKind::UnreachableInst:
    case ValueKind::ReturnInst:
    case ValueKind::AutoreleaseReturnInst:
    case ValueKind::SwitchValueInst:
      llvm_unreachable("Did not expect terminator that does not have args!");

    default:
      llvm_unreachable("Unhandled terminator kind!");
    }
  }

  void collectOperandsForUser(ValueBase *User,
                              llvm::SmallVectorImpl<ValueBase *> &Operands) {
    if (auto *I = dyn_cast<SILInstruction>(User)) {
      for (auto &O : I->getAllOperands())
        Operands.push_back(O.get().getDef());
      return;
    }

    if (auto *A = dyn_cast<SILArgument>(User)) {
      auto *BB = A->getParent();
      auto Index = A->getIndex();

      for (auto *Pred : BB->getPreds()) {
        auto *V = getArgForTerminator(Pred->getTerminator(), BB, Index);
        Operands.push_back(V);
      }
      return;
    }
  }

  // Is Value currently on our DFS stack?
  bool onStack(ValueBase *Value) {
    return DFSStack.count(Value);
  }

  /// Do depth-first through the value graph, finding the strongly
  /// component that User is a part of, and call visit() with that SCC.
  void DFS(ValueBase *User) {
    assert(!Visited.count(User) &&
           "Attempting to visit a value twice in DFS search!");

    DFSStack.insert(User);
    Visited.insert(User);

    auto &UserInfo = addDFSInfo(User);

    llvm::SmallVector<ValueBase *, 4> Operands;
    collectOperandsForUser(User, Operands);

    // Visit each unvisited operand, updating the lowest DFS number we've seen
    // reachable in User's SCC.
    for (auto *Opnd : Operands) {
      if (!Visited.count(Opnd)) {
        DFS(Opnd);
        UserInfo.LowNum = std::min(UserInfo.LowNum, getDFSInfo(Opnd).LowNum);
      } else if (onStack(Opnd)) {
        UserInfo.LowNum = std::min(UserInfo.LowNum, getDFSInfo(Opnd).DFSNum);
      }
    }

    // If User is the head of its own SCC, pop that SCC off the DFS stack.
    if (UserInfo.DFSNum == UserInfo.LowNum) {
      llvm::SmallVector<ValueBase *, 4> SCC;
      ValueBase *PoppedValue;
      do {
        PoppedValue = DFSStack.pop_back_val();
        SCC.push_back(PoppedValue);
      } while (PoppedValue != User);

      asImpl().visit(SCC);
    }
  }
};

}

#endif
