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

#include "swift/SIL/SIL.h"
#include "swift/SIL/BBArgument.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {

/// SILVisitor - This is a simple visitor class for Swift SIL nodes, allowing
/// clients to walk over entire SIL functions, blocks, or instructions.
template<typename ImplClass, typename ValueRetTy = void>
class SILVisitor {
public:

  ValueRetTy visitValue(Value *V) {
    // Base class, used if some class of value isn't handled.
  }

#define VALUE(CLASS, PARENT)              \
  case ValueKind::CLASS:                  \
    return static_cast<ImplClass*>(this)  \
    ->visit##CLASS(static_cast<CLASS*>(V));

  ValueRetTy visit(Value *V) {
    switch (V->getKind()) {
#include "swift/SIL/SILNodes.def"
    }
    llvm_unreachable("Not reachable, all cases handled");
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

  void visitBB(BasicBlock *BB) {
    for (auto &I : *BB)
      visit(&I);
  }
  void visitBB(BasicBlock &BB) {
    visitBB(&BB);
  }

  void visitFunction(Function *F) {
    for (auto &BB : *F)
      visitBB(BB);
  }
  void visitFunction(Function &F) {
    visitBB(&F);
  }
};

} // end namespace swift

#endif
