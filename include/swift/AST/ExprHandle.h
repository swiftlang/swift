//===--- ExprHandle.h - Swift Expression Handle -----------------*- C++ -*-===//
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
// This file defines the ExprHandle class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_EXPRHANDLE_H
#define SWIFT_EXPRHANDLE_H

#include "llvm/Support/DataTypes.h"

namespace swift {
  class ASTContext;
  class Expr;

/// ExprHandle - Provides an indirection for expressions, so both a type and a
/// pattern can point at the same expression during type-checking.
class ExprHandle {
  Expr *E;
private:
  ExprHandle(Expr *E) : E(E) {}

  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8);
public:
  Expr *getExpr() {
    return E;
  }

  void setExpr(Expr *newE) {
    E = newE;
  }

  static ExprHandle *get(ASTContext &Context, Expr *E);
};

} // end namespace swift

#endif
