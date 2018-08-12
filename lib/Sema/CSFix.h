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

/// Describes the kind of fix to apply to the given constraint before
/// visiting it.
enum class FixKind : uint8_t {
  /// Introduce a '!' to force an optional unwrap.
  ForceOptional,

  /// Unwrap an optional base when we have a member access.
  UnwrapOptionalBase,

  /// Append 'as! T' to force a downcast to the specified type.
  ForceDowncast,

  /// Introduce a '&' to take the address of an lvalue.
  AddressOf,

  /// Replace a coercion ('as') with a forced checked cast ('as!').
  CoerceToCheckedCast,

  /// Mark function type as explicitly '@escaping'.
  ExplicitlyEscaping,
  /// Mark function type as explicitly '@escaping' to be convertable to 'Any'.
  ExplicitlyEscapingToAny,

  /// Arguments have labeling failures - missing/extraneous or incorrect
  /// labels attached to the, fix it by suggesting proper labels.
  RelabelArguments,

  /// Add a new conformance to the type to satisfy a requirement.
  AddConformance,
};

class ConstraintFix {
  FixKind Kind;
  ConstraintLocator *Locator;

public:
  ConstraintFix(FixKind kind, ConstraintLocator *locator)
      : Kind(kind), Locator(locator) {}

  virtual ~ConstraintFix();

  FixKind getKind() const { return Kind; }

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
      : ConstraintFix(FixKind::ForceDowncast, locator), DowncastTo(toType) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override;
};

/// Introduce a '!' to force an optional unwrap.
class ForceOptional final : public ConstraintFix {
public:
  ForceOptional(ConstraintLocator *locator)
      : ConstraintFix(FixKind::ForceOptional, locator) {}

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
      : ConstraintFix(FixKind::UnwrapOptionalBase, locator),
        MemberName(member) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: unwrap optional base of member lookup]";
  }
};

/// Introduce a '&' to take the address of an lvalue.
class AddAddressOf final : public ConstraintFix {
public:
  AddAddressOf(ConstraintLocator *locator)
      : ConstraintFix(FixKind::AddressOf, locator) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: add address-of]";
  }
};

/// Replace a coercion ('as') with a forced checked cast ('as!').
class CoerceToCheckedCast final : public ConstraintFix {
public:
  CoerceToCheckedCast(ConstraintLocator *locator)
      : ConstraintFix(FixKind::CoerceToCheckedCast, locator) {}

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
      : ConstraintFix(FixKind::ExplicitlyEscaping, locator),
        ConvertTo(convertingTo) {}

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
      : ConstraintFix(FixKind::RelabelArguments, locator),
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
      : ConstraintFix(FixKind::AddConformance, locator),
        NonConformingType(type), Protocol(protocol) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: add missing protocol conformance]";
  }
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSFIX_H
