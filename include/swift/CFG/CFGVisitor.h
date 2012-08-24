//===--- CFGVisitor.h - Defines the CFGVisitor class -------------*- C++ -*-==//
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
// This file defines the CFGVisitor class, used for walking CFGs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CFG_CFGVISITOR_H
#define SWIFT_CFG_CFGVISITOR_H

#include "swift/CFG/CFG.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {

/// CFGVisitor - This is a simple visitor class for Swift CFG nodes, allowing
/// clients to walk over entire CFGs, blocks, or instructions.
template<typename ImplClass, typename InstRetTy = void>
class CFGVisitor {
public:

  InstRetTy visitInstruction(Instruction *I) {
    // Base class, used if some class of instructions isn't handled.
  }

#define INST(CLASS, PARENT)              \
  case InstKind::CLASS:                  \
    return static_cast<ImplClass*>(this) \
    ->visit##CLASS##Inst(static_cast<CLASS##Inst*>(I));

  InstRetTy visit(Instruction *I) {
    switch (I->getKind()) {
#include "swift/CFG/CFGNodes.def"
    }
    llvm_unreachable("Not reachable, all cases handled");
  }

  // Define default dispatcher implementations chain to parent nodes.
#define INST(CLASS, PARENT)              \
InstRetTy visit##CLASS##Inst(CLASS##Inst *I) {             \
  return static_cast<ImplClass*>(this)->visit##PARENT(I);  \
}

#define ABSTRACT_INST(CLASS, PARENT)                       \
InstRetTy visit##CLASS##Inst(CLASS##Inst *I) {             \
  return static_cast<ImplClass*>(this)->visit##PARENT(I);  \
}
#include "swift/CFG/CFGNodes.def"

  void visit(BasicBlock *BB) {
    for (auto &I : *BB)
      visit(I);
  }
  void visit(BasicBlock &BB) {
    visit(&BB);
  }

  void visit(CFG *C) {
    for (auto &BB : *C)
      visit(BB);
  }
  void visit(CFG &C) {
    visit(&C);
  }
};

} // end namespace swift

#endif
