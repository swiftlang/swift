//===--- SILVisitor.h - Defines the SILVisitor class -------------*- C++ -*-==//
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
// This file defines the SILVisitor class, used for walking SIL code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILVISITOR_H
#define SWIFT_SIL_SILVISITOR_H

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILBBArgument.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {

/// SILVisitor - This is a simple visitor class for Swift SIL nodes, allowing
/// clients to walk over entire SIL functions, blocks, or instructions.
template<typename ImplClass, typename ValueRetTy = void>
class SILVisitor {
public:
  ValueRetTy visitValue(ValueBase *V) {
    // Base class, used if some class of value isn't handled.
  }

#define VALUE(CLASS, PARENT)              \
  case ValueKind::CLASS:                  \
    return static_cast<ImplClass*>(this)  \
    ->visit##CLASS(static_cast<CLASS*>(V));

  ValueRetTy visit(ValueBase *V) {
    switch (V->getKind()) {
#include "swift/SIL/SILNodes.def"
    }
    llvm_unreachable("Not reachable, all cases handled");
  }
  ValueRetTy visit(Value V) {
    return visit(V.getDef());
  }

  // Define default dispatcher implementations chain to parent nodes.
#define VALUE(CLASS, PARENT)                   \
ValueRetTy visit##CLASS(CLASS *I) {            \
  return static_cast<ImplClass*>(this)->visit##PARENT(I);  \
}

#define ABSTRACT_VALUE(CLASS, PARENT)                       \
ValueRetTy visit##CLASS(CLASS *I) {                         \
  return static_cast<ImplClass*>(this)->visit##PARENT(I);   \
}
#include "swift/SIL/SILNodes.def"

  void visitBasicBlock(BasicBlock *BB) {
    for (auto argI = BB->bbarg_begin(), argEnd = BB->bbarg_end();
         argI != argEnd;
         ++argI)
      visit(*argI);
      
    for (auto &I : *BB)
      visit(&I);
  }
  void visitBasicBlock(BasicBlock &BB) {
    this->ImplClass::visitBasicBlock(&BB);
  }

  void visitFunction(SILFunction *F) {
    for (auto &BB : *F)
      this->ImplClass::visitBasicBlock(&BB);
  }
  void visitFunction(SILFunction &F) {
    this->ImplClass::visitFunction(&F);
  }
};

} // end namespace swift

#endif
