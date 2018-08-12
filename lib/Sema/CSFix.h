//===--- CSFix.h - Constraint Fixes ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides necessary abstractions for constraint fixes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_CSFIX_H
#define SWIFT_SEMA_CSFIX_H

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
class raw_ostream;
}

namespace swift {
namespace constraints {

class ConstraintLocator;
class Solution;

class ConstraintFix {
  ConstraintLocator *Locator;

public:
  ConstraintFix(ConstraintLocator *locator) : Locator(locator) {}

  virtual ~ConstraintFix();

  /// Diagnose a failure associated with this fix given
  /// root expression and information from well-formed solution.
  virtual bool diagnose(Expr *root, const Solution &solution) const = 0;
  virtual void print(llvm::raw_ostream &Out) const = 0;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");

  ConstraintLocator *getLocator() const { return Locator; }
};

/// Append 'as! T' to force a downcast to the specified type.
class ForceDowncast final : public ConstraintFix {
  Type DowncastTo;

public:
  ForceDowncast(Type toType, ConstraintLocator *locator)
      : ConstraintFix(locator), DowncastTo(toType) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override;
};

/// Introduce a '!' to force an optional unwrap.
class ForceOptional final : public ConstraintFix {
public:
  ForceOptional(ConstraintLocator *locator) : ConstraintFix(locator) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: force optional]";
  }
};

/// Unwrap an optional base when we have a member access.
class UnwrapOptionalBase final : public ConstraintFix {
  DeclName MemberName;

public:
  UnwrapOptionalBase(ConstraintLocator *locator, DeclName member)
      : ConstraintFix(locator), MemberName(member) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: unwrap optional base of member lookup]";
  }
};

/// Introduce a '&' to take the address of an lvalue.
class AddAddressOf final : public ConstraintFix {
public:
  AddAddressOf(ConstraintLocator *locator) : ConstraintFix(locator) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: add address-of]";
  }
};

/// Replace a coercion ('as') with a forced checked cast ('as!').
class CoerceToCheckedCast final : public ConstraintFix {
public:
  CoerceToCheckedCast(ConstraintLocator *locator) : ConstraintFix(locator) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: as to as!]";
  }
};

/// Mark function type as explicitly '@escaping'.
class MarkExplicitlyEscaping final : public ConstraintFix {
  /// Sometimes function type has to be marked as '@escaping'
  /// to be converted to some other generic type.
  Type ConvertTo;

public:
  MarkExplicitlyEscaping(ConstraintLocator *locator, Type convertingTo = Type())
      : ConstraintFix(locator), ConvertTo(convertingTo) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: add @escaping]";
  }
};

/// Arguments have labeling failures - missing/extraneous or incorrect
/// labels attached to the, fix it by suggesting proper labels.
class RelabelArguments final : public ConstraintFix {
  llvm::SmallVector<Identifier, 4> CorrectLabels;

public:
  RelabelArguments(llvm::ArrayRef<Identifier> correctLabels,
                   ConstraintLocator *locator)
      : ConstraintFix(locator),
        CorrectLabels(correctLabels.begin(), correctLabels.end()) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: re-label argument(s)]";
  }
};

/// Add a new conformance to the type to satisfy a requirement.
class MissingConformance final : public ConstraintFix {
  Type NonConformingType;
  ProtocolDecl *Protocol;

public:
  MissingConformance(Type type, ProtocolDecl *protocol,
                     ConstraintLocator *locator)
      : ConstraintFix(locator), NonConformingType(type), Protocol(protocol) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: add missing protocol conformance]";
  }
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSFIX_H
