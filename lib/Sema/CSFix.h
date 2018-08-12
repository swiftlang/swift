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

class ConstraintSystem;
class ConstraintLocator;
class Solution;

/// Describes the kind of fix to apply to the given constraint before
/// visiting it.
enum class FixKind : uint8_t {
  /// Introduce a '!' to force an optional unwrap.
  ForceOptional,

  /// Unwrap an optional base when we have a member access.
  UnwrapOptionalBase,
  UnwrapOptionalBaseWithOptionalResult,

  /// Append 'as! T' to force a downcast to the specified type.
  ForceDowncast,

  /// Introduce a '&' to take the address of an lvalue.
  AddressOf,

  /// Replace a coercion ('as') with a forced checked cast ('as!').
  CoerceToCheckedCast,

  /// Mark function type as explicitly '@escaping'.
  ExplicitlyEscaping,

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

  /// Retrieve anchor expression associated with this fix.
  /// NOTE: such anchor comes directly from locator without
  /// any simplication attempts.
  Expr *getAnchor() const;
  ConstraintLocator *getLocator() const { return Locator; }
};

/// Append 'as! T' to force a downcast to the specified type.
class ForceDowncast final : public ConstraintFix {
  Type DowncastTo;

  ForceDowncast(Type toType, ConstraintLocator *locator)
      : ConstraintFix(FixKind::ForceDowncast, locator), DowncastTo(toType) {}

public:
  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override;

  static ForceDowncast *create(ConstraintSystem &cs, Type toType,
                               ConstraintLocator *locator);
};

/// Introduce a '!' to force an optional unwrap.
class ForceOptional final : public ConstraintFix {
  ForceOptional(ConstraintLocator *locator)
      : ConstraintFix(FixKind::ForceOptional, locator) {}

public:
  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: force optional]";
  }

  static ForceOptional *create(ConstraintSystem &cs,
                               ConstraintLocator *locator);
};

/// Unwrap an optional base when we have a member access.
class UnwrapOptionalBase final : public ConstraintFix {
  DeclName MemberName;

  UnwrapOptionalBase(FixKind kind, DeclName member, ConstraintLocator *locator)
      : ConstraintFix(kind, locator), MemberName(member) {
    assert(kind == FixKind::UnwrapOptionalBase ||
           kind == FixKind::UnwrapOptionalBaseWithOptionalResult);
  }

public:
  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: unwrap optional base of member lookup]";
  }

  static UnwrapOptionalBase *create(ConstraintSystem &cs, DeclName member,
                                    ConstraintLocator *locator);

  static UnwrapOptionalBase *
  createWithOptionalResult(ConstraintSystem &cs, DeclName member,
                           ConstraintLocator *locator);
};

/// Introduce a '&' to take the address of an lvalue.
class AddAddressOf final : public ConstraintFix {
  AddAddressOf(ConstraintLocator *locator)
      : ConstraintFix(FixKind::AddressOf, locator) {}

public:
  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: add address-of]";
  }

  static AddAddressOf *create(ConstraintSystem &cs, ConstraintLocator *locator);
};

/// Replace a coercion ('as') with a forced checked cast ('as!').
class CoerceToCheckedCast final : public ConstraintFix {
  CoerceToCheckedCast(ConstraintLocator *locator)
      : ConstraintFix(FixKind::CoerceToCheckedCast, locator) {}

public:
  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: as to as!]";
  }

  static CoerceToCheckedCast *create(ConstraintSystem &cs,
                                     ConstraintLocator *locator);
};

/// Mark function type as explicitly '@escaping'.
class MarkExplicitlyEscaping final : public ConstraintFix {
  /// Sometimes function type has to be marked as '@escaping'
  /// to be converted to some other generic type.
  Type ConvertTo;

  MarkExplicitlyEscaping(ConstraintLocator *locator, Type convertingTo = Type())
      : ConstraintFix(FixKind::ExplicitlyEscaping, locator),
        ConvertTo(convertingTo) {}

public:
  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: add @escaping]";
  }

  static MarkExplicitlyEscaping *create(ConstraintSystem &cs,
                                        ConstraintLocator *locator,
                                        Type convertingTo = Type());
};

/// Arguments have labeling failures - missing/extraneous or incorrect
/// labels attached to the, fix it by suggesting proper labels.
class RelabelArguments final : public ConstraintFix {
  llvm::SmallVector<Identifier, 4> CorrectLabels;

  RelabelArguments(llvm::ArrayRef<Identifier> correctLabels,
                   ConstraintLocator *locator)
      : ConstraintFix(FixKind::RelabelArguments, locator),
        CorrectLabels(correctLabels.begin(), correctLabels.end()) {}

public:
  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: re-label argument(s)]";
  }

  static RelabelArguments *create(ConstraintSystem &cs,
                                  llvm::ArrayRef<Identifier> correctLabels,
                                  ConstraintLocator *locator);
};

/// Add a new conformance to the type to satisfy a requirement.
class MissingConformance final : public ConstraintFix {
  Type NonConformingType;
  ProtocolDecl *Protocol;

  MissingConformance(Type type, ProtocolDecl *protocol,
                     ConstraintLocator *locator)
      : ConstraintFix(FixKind::AddConformance, locator),
        NonConformingType(type), Protocol(protocol) {}

public:
  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: add missing protocol conformance]";
  }

  static MissingConformance *create(ConstraintSystem &cs, Type type,
                                    ProtocolDecl *protocol,
                                    ConstraintLocator *locator);
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSFIX_H
