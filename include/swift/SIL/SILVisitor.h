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

/// A helper class for all the SIL visitors.
/// You probably shouldn't use this directly.
template <typename ImplClass, typename RetTy = void, typename... ArgTys>
class SILVisitorBase {
public:
  ImplClass &asImpl() { return static_cast<ImplClass &>(*this); }

  void visitSILBasicBlock(SILBasicBlock *BB, ArgTys... args) {
    asImpl().visitBasicBlockArguments(BB, args...);

    for (auto &I : *BB)
      asImpl().visit(&I, args...);
  }
  void visitSILBasicBlock(SILBasicBlock &BB, ArgTys... args) {
    asImpl().visitSILBasicBlock(&BB, args...);
  }

  void visitSILFunction(SILFunction *F, ArgTys... args) {
    for (auto &BB : *F)
      asImpl().visitSILBasicBlock(&BB, args...);
  }
  void visitSILFunction(SILFunction &F, ArgTys... args) {
    asImpl().visitSILFunction(&F, args...);
  }
};

/// SILValueVisitor - This is a simple visitor class for Swift SIL nodes,
/// allowing clients to walk over entire SIL functions, blocks, or instructions.
template <typename ImplClass, typename RetTy = void, typename... ArgTys>
class SILValueVisitor
       : public SILVisitorBase<ImplClass, RetTy, ArgTys...> {
  using super = SILVisitorBase<ImplClass, RetTy, ArgTys...>;
public:
  using super::asImpl;

  RetTy visit(ValueBase *V, ArgTys... args) {
    switch (V->getKind()) {
#define VALUE(CLASS, PARENT)                                \
  case ValueKind::CLASS:                                    \
    return asImpl().visit##CLASS(static_cast<CLASS*>(V),    \
                                 std::forward<ArgTys>(args)...);
#include "swift/SIL/SILNodes.def"
    }
    llvm_unreachable("Not reachable, all cases handled");
  }

  // Define default dispatcher implementations chain to parent nodes.
#define VALUE(CLASS, PARENT)                                         \
  RetTy visit##CLASS(CLASS *I, ArgTys... args) {                     \
    return asImpl().visit##PARENT(I, std::forward<ArgTys>(args)...); \
  }
#define ABSTRACT_VALUE(CLASS, PARENT) VALUE(CLASS, PARENT)
#include "swift/SIL/SILNodes.def"
};

/// A visitor that should only visit SIL instructions.
template <typename ImplClass, typename RetTy = void, typename... ArgTys>
class SILInstructionVisitor
       : public SILVisitorBase<ImplClass, RetTy, ArgTys...> {
  using super = SILVisitorBase<ImplClass, RetTy, ArgTys...>;
public:
  using super::asImpl;

  // Perform any required pre-processing before visiting.
  // Sub-classes can override it to provide their custom
  // pre-processing steps.
  void beforeVisit(SILInstruction *inst) {}

  RetTy visit(SILInstruction *inst, ArgTys... args) {
    asImpl().beforeVisit(inst, args...);

    switch (inst->getKind()) {
#define INST(CLASS, PARENT)                                             \
    case SILInstructionKind::CLASS:                                     \
      return asImpl().visit##CLASS(static_cast<CLASS*>(inst),           \
                                   std::forward<ArgTys>(args)...);
#include "swift/SIL/SILNodes.def"
    }
    llvm_unreachable("Not reachable, all cases handled");
  }

  // Define default dispatcher implementations chain to parent nodes.
#define INST(CLASS, PARENT)                                             \
  RetTy visit##CLASS(CLASS *inst, ArgTys... args) {                     \
    return asImpl().visit##PARENT(inst, std::forward<ArgTys>(args)...); \
  }
#define ABSTRACT_INST(CLASS, PARENT) INST(CLASS, PARENT)
#include "swift/SIL/SILNodes.def"

  void visitBasicBlockArguments(SILBasicBlock *BB, ArgTys... args) {}
};

/// A visitor that should visit all SIL nodes.
template <typename ImplClass, typename RetTy = void, typename... ArgTys>
class SILNodeVisitor
       : public SILVisitorBase<ImplClass, RetTy, ArgTys...> {
  using super = SILVisitorBase<ImplClass, RetTy, ArgTys...>;
public:
  using super::asImpl;

  // Perform any required pre-processing before visiting.
  // Sub-classes can override it to provide their custom
  // pre-processing steps.
  void beforeVisit(SILNode *I, ArgTys... args) {}

  RetTy visit(SILNode *node, ArgTys... args) {
    asImpl().beforeVisit(node, args...);

    switch (node->getKind()) {
#define NODE(CLASS, PARENT)                                             \
    case SILNodeKind::CLASS:                                            \
      return asImpl().visit##CLASS(cast<CLASS>(node),                   \
                                   std::forward<ArgTys>(args)...);
#include "swift/SIL/SILNodes.def"
    }
    llvm_unreachable("Not reachable, all cases handled");
  }

  // Define default dispatcher implementations chain to parent nodes.
#define NODE(CLASS, PARENT)                                             \
  RetTy visit##CLASS(CLASS *node, ArgTys... args) {                     \
    return asImpl().visit##PARENT(node, std::forward<ArgTys>(args)...); \
  }
#define ABSTRACT_NODE(CLASS, PARENT) NODE(CLASS, PARENT)
#include "swift/SIL/SILNodes.def"

  void visitBasicBlockArguments(SILBasicBlock *BB, ArgTys... args) {
    for (auto argI = BB->args_begin(), argEnd = BB->args_end(); argI != argEnd;
         ++argI)
      asImpl().visit(*argI, args...);
  }
};

} // end namespace swift

#endif
