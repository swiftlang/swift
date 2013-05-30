//===--- TypeCheckerOld.h - Old Type Checker Helpers ------------*- C++ -*-===//
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
//  This file defines types used only by the old type checker.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_TYPE_CHECKER_OLD_H
#define SWIFT_SEMA_TYPE_CHECKER_OLD_H
#include "TypeChecker.h"
using namespace swift;

/// \brief Wraps an expression that refers to an overloaded set of declarations,
/// which may have various syntactic forms (normal reference, member reference,
/// operator name) and may also refer to a single, generic declaration.
///
/// This class is used when decomposing an expression into the pieces used
/// by overload resolution.
class OverloadedExpr {
  /// \brief The actual expression, along with a bit that indicates whether
  /// this is an overloaded member operation
  llvm::PointerIntPair<Expr *, 1, bool> E;

  union {
    struct {
      ValueDecl * const *Start;
      unsigned Size;
    } Overloaded;

    struct {
      ValueDecl *Single;
    } Generic;
  };

  /// \brief The type of the base of a member access, if in fact this is a
  /// member access.
  Type BaseTy;

public:
  /// \brief Creates an empty overloaded expression.
  OverloadedExpr() : E(nullptr, false) { }

  /// \brief Creates an overloaded expression that refers to a single (generic)
  /// entity.
  OverloadedExpr(Expr *E, Type BaseTy, ValueDecl *Single)
    : E(E, false), BaseTy(BaseTy)
  {
    Generic.Single = Single;
  }

  /// \brief Creates an overloaded expression that refers to a set of
  /// declarations.
  OverloadedExpr(Expr *E, Type BaseTy, ArrayRef<ValueDecl *> Candidates)
    : E(E, true), BaseTy(BaseTy)
  {
    Overloaded.Start = Candidates.data();
    Overloaded.Size = Candidates.size();
  }

  /// \brief Creates an overloaded expression from an overload set expression.
  /// FIXME: This constructor is meant to be temporary; client should go
  /// through TypeChecker::getOverloadExpr() instead.
  OverloadedExpr(OverloadSetRefExpr *OSE)
    : E(OSE, true), BaseTy(OSE->getBaseType())
  {
    Overloaded.Start = OSE->getDecls().data();
    Overloaded.Size = OSE->getDecls().size();
  }

  /// \brief Determine if this overloaded expression stores a valid expression.
  explicit operator bool() const { return E.getPointer(); }

  /// \brief Retrieve the overloaded expression.
  Expr *getExpr() const { return E.getPointer(); }

  /// \brief Retrieve the type of the base object, or a null type if there is
  /// no base.
  Type getBaseType() const { return BaseTy; }

  /// \brief Retrieve the set of overload candidates, which may contain a
  /// single declaration (if it is generic).
  ArrayRef<ValueDecl *> getCandidates() const {
    if (E.getInt()) {
      return ArrayRef<ValueDecl *>(Overloaded.Start, Overloaded.Size);
    }

    return ArrayRef<ValueDecl *>(&Generic.Single, 1);
  }
};

Expr *buildFilteredOverloadSet(TypeChecker &TC,
                               OverloadedExpr Ovl,
                               ArrayRef<ValueDecl *> Remaining);
Expr *buildFilteredOverloadSet(TypeChecker &TC, OverloadedExpr Ovl,
                               const OverloadCandidate &Candidate);

#endif
