//===--- AnyFunctionRef.h - A Universal Function Reference ------*- C++ -*-===//
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

#ifndef SWIFT_AST_ANY_FUNCTION_REF_H
#define SWIFT_AST_ANY_FUNCTION_REF_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "llvm/ADT/PointerUnion.h"

namespace swift {
class CaptureInfo;

/// \brief A universal function reference -- can wrap all AST nodes that
/// represent functions and exposes a common interface to them.
class AnyFunctionRef {
  PointerUnion<AbstractFunctionDecl *, CapturingExpr *> TheFunction;

public:
  AnyFunctionRef(AbstractFunctionDecl *AFD) : TheFunction(AFD) {}
  AnyFunctionRef(CapturingExpr *CE) : TheFunction(CE) {}

  CaptureInfo &getCaptureInfo() {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getCaptureInfo();
    return TheFunction.get<CapturingExpr *>()->getCaptureInfo();
  }

  Type getType() {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getType();
    return TheFunction.get<CapturingExpr *>()->getType();
  }

  BraceStmt *getBody() {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getBody();
    auto *CE = TheFunction.get<CapturingExpr *>();
    if (auto *PCE = dyn_cast<PipeClosureExpr>(CE))
      return PCE->getBody();
    return cast<ImplicitClosureExpr>(CE)->getBody();
  }

  DeclContext *getAsDeclContext() {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD;
    auto *CE = TheFunction.get<CapturingExpr *>();
    if (auto *PCE = dyn_cast<PipeClosureExpr>(CE))
      return PCE;
    return cast<ImplicitClosureExpr>(CE);
  }
};

} // namespace swift

#endif // LLVM_SWIFT_AST_ANY_FUNCTION_REF_H

