//===--- ExprHandle.h - Swift Expression Handle -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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

#include "swift/AST/TypeAlignments.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/ADT/PointerIntPair.h"

namespace swift {
  class ASTContext;
  class Expr;

/// ExprHandle - Provides an indirection for expressions, so both a type and a
/// pattern can point at the same expression during type-checking.
class alignas(8) ExprHandle {
  /// \brief The expression along with a bit saying whether this expression
  /// was already type-checked (or not).
  llvm::PointerIntPair<Expr *, 1, bool> EAndChecked;
private:
  ExprHandle(Expr *E) : EAndChecked(E, false) {}

  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8);
public:
  Expr *getExpr() {
    return EAndChecked.getPointer();
  }

  /// \brief Determine whether the referenced expression has already been
  /// type-checked.
  bool alreadyChecked() const { return EAndChecked.getInt(); }

  /// \brief Set the expression after it has been type-checked.
  void setExpr(Expr *newE, bool checked) {
    assert(newE && "Null expression in handle");
    EAndChecked.setPointer(newE);
    EAndChecked.setInt(checked);
  }

  static ExprHandle *get(ASTContext &Context, Expr *E);
};

} // end namespace swift

#endif
