//===--- SILVisitor.h - Defines the SILVisitor class ------------*- C++ -*-===//
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
// This file defines the SILVisitor class, used for walking SIL code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILVISITOR_H
#define SWIFT_SIL_SILVISITOR_H

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {

/// SILVisitor - This is a simple visitor class for Swift SIL nodes, allowing
/// clients to walk over entire SIL functions, blocks, or instructions.
template<typename ImplClass, typename ValueRetTy = void>
class SILVisitor {
public:
  ImplClass &asImpl() { return static_cast<ImplClass &>(*this); }

  // Perform any required pre-processing before visiting.
  // Sub-classes can override it to provide their custom
  // pre-processing steps.
  void beforeVisit(ValueBase *V) {
  }

  ValueRetTy visit(ValueBase *V) {
    asImpl().beforeVisit(V);

    switch (V->getKind()) {
#define VALUE(CLASS, PARENT)                                \
  case ValueKind::CLASS:                                    \
    return asImpl().visit##CLASS(static_cast<CLASS*>(V));
#include "swift/SIL/SILNodes.def"
    }
    llvm_unreachable("Not reachable, all cases handled");
  }

  // Define default dispatcher implementations chain to parent nodes.
#define VALUE(CLASS, PARENT)                                    \
  ValueRetTy visit##CLASS(CLASS *I) {                           \
    return asImpl().visit##PARENT(I);                           \
  }
#define ABSTRACT_VALUE(CLASS, PARENT) VALUE(CLASS, PARENT)
#include "swift/SIL/SILNodes.def"

  void visitSILBasicBlock(SILBasicBlock *BB) {
    asImpl().visitBasicBlockArguments(BB);
      
    for (auto &I : *BB)
      asImpl().visit(&I);
  }
  void visitSILBasicBlock(SILBasicBlock &BB) {
    asImpl().visitSILBasicBlock(&BB);
  }

  void visitBasicBlockArguments(SILBasicBlock *BB) {
    for (auto argI = BB->args_begin(), argEnd = BB->args_end(); argI != argEnd;
         ++argI)
      asImpl().visit(*argI);
  }

  void visitSILFunction(SILFunction *F) {
    for (auto &BB : *F)
      asImpl().visitSILBasicBlock(&BB);
  }
  void visitSILFunction(SILFunction &F) {
    asImpl().visitSILFunction(&F);
  }
};

/// A simple convenience class for a visitor that should only visit
/// SIL instructions.
template<typename ImplClass, typename ValueRetTy = void>
class SILInstructionVisitor : public SILVisitor<ImplClass, ValueRetTy> {
public:
  void visitBasicBlockArguments(SILBasicBlock *BB) {}

  ValueRetTy visitSILArgument(SILArgument *A) {
    llvm_unreachable("should only be visiting instructions");
  }
  ValueRetTy visitSILUndef(SILUndef *U) {
    llvm_unreachable("should only be visiting instructions");
  }

  ValueRetTy visit(SILInstruction *I) {
    return SILVisitor<ImplClass, ValueRetTy>::visit(I);
  }
};

} // end namespace swift

#endif
