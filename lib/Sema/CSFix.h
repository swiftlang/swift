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
#include "llvm/Support/TrailingObjects.h"
#include <string>

namespace llvm {
class raw_ostream;
}

namespace swift {

class SourceManager;

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

  /// Treat rvalue as lvalue
  TreatRValueAsLValue,

  /// Add a new conformance to the type to satisfy a requirement.
  AddConformance,

  /// Skip same-type generic requirement constraint,
  /// and assume that types are equal.
  SkipSameTypeRequirement,
};

class ConstraintFix {
  ConstraintSystem &CS;
  FixKind Kind;
  ConstraintLocator *Locator;

public:
  ConstraintFix(ConstraintSystem &cs, FixKind kind, ConstraintLocator *locator)
      : CS(cs), Kind(kind), Locator(locator) {}

  virtual ~ConstraintFix();

  FixKind getKind() const { return Kind; }

  virtual std::string getName() const = 0;

  /// Diagnose a failure associated with this fix given
  /// root expression and information from constraint system.
  virtual bool diagnose(Expr *root, bool asNote = false) const = 0;

  void print(llvm::raw_ostream &Out) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const
                              LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");

  /// Retrieve anchor expression associated with this fix.
  /// NOTE: such anchor comes directly from locator without
  /// any simplication attempts.
  Expr *getAnchor() const;
  ConstraintLocator *getLocator() const { return Locator; }

protected:
  ConstraintSystem &getConstraintSystem() const { return CS; }
};

/// Append 'as! T' to force a downcast to the specified type.
class ForceDowncast final : public ConstraintFix {
  Type DowncastTo;

  ForceDowncast(ConstraintSystem &cs, Type toType, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::ForceDowncast, locator), DowncastTo(toType) {
  }

public:
  std::string getName() const override;
  bool diagnose(Expr *root, bool asNote = false) const override;

  static ForceDowncast *create(ConstraintSystem &cs, Type toType,
                               ConstraintLocator *locator);
};

/// Introduce a '!' to force an optional unwrap.
class ForceOptional final : public ConstraintFix {
  ForceOptional(ConstraintSystem &cs, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::ForceOptional, locator) {}

public:
  std::string getName() const override { return "force optional"; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static ForceOptional *create(ConstraintSystem &cs,
                               ConstraintLocator *locator);
};

/// Unwrap an optional base when we have a member access.
class UnwrapOptionalBase final : public ConstraintFix {
  DeclName MemberName;

  UnwrapOptionalBase(ConstraintSystem &cs, FixKind kind, DeclName member,
                     ConstraintLocator *locator)
      : ConstraintFix(cs, kind, locator), MemberName(member) {
    assert(kind == FixKind::UnwrapOptionalBase ||
           kind == FixKind::UnwrapOptionalBaseWithOptionalResult);
  }

public:
  std::string getName() const override {
    return "unwrap optional base of member lookup";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static UnwrapOptionalBase *create(ConstraintSystem &cs, DeclName member,
                                    ConstraintLocator *locator);

  static UnwrapOptionalBase *
  createWithOptionalResult(ConstraintSystem &cs, DeclName member,
                           ConstraintLocator *locator);
};

/// Introduce a '&' to take the address of an lvalue.
class AddAddressOf final : public ConstraintFix {
  AddAddressOf(ConstraintSystem &cs, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AddressOf, locator) {}

public:
  std::string getName() const override { return "add address-of"; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static AddAddressOf *create(ConstraintSystem &cs, ConstraintLocator *locator);
};

// Treat rvalue as if it was an lvalue
class TreatRValueAsLValue final : public ConstraintFix {
  TreatRValueAsLValue(ConstraintSystem &cs, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::TreatRValueAsLValue, locator) {}

public:
  std::string getName() const override { return "treat rvalue as lvalue"; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static TreatRValueAsLValue *create(ConstraintSystem &cs,
                                     ConstraintLocator *locator);
};


/// Replace a coercion ('as') with a forced checked cast ('as!').
class CoerceToCheckedCast final : public ConstraintFix {
  CoerceToCheckedCast(ConstraintSystem &cs, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::CoerceToCheckedCast, locator) {}

public:
  std::string getName() const override { return "as to as!"; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static CoerceToCheckedCast *create(ConstraintSystem &cs,
                                     ConstraintLocator *locator);
};

/// Mark function type as explicitly '@escaping'.
class MarkExplicitlyEscaping final : public ConstraintFix {
  /// Sometimes function type has to be marked as '@escaping'
  /// to be converted to some other generic type.
  Type ConvertTo;

  MarkExplicitlyEscaping(ConstraintSystem &cs, ConstraintLocator *locator,
                         Type convertingTo = Type())
      : ConstraintFix(cs, FixKind::ExplicitlyEscaping, locator),
        ConvertTo(convertingTo) {}

public:
  std::string getName() const override { return "add @escaping"; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static MarkExplicitlyEscaping *create(ConstraintSystem &cs,
                                        ConstraintLocator *locator,
                                        Type convertingTo = Type());
};

/// Arguments have labeling failures - missing/extraneous or incorrect
/// labels attached to the, fix it by suggesting proper labels.
class RelabelArguments final
    : public ConstraintFix,
      private llvm::TrailingObjects<RelabelArguments, Identifier> {
  friend TrailingObjects;

  unsigned NumLabels;

  RelabelArguments(ConstraintSystem &cs,
                   llvm::ArrayRef<Identifier> correctLabels,
                   ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::RelabelArguments, locator),
        NumLabels(correctLabels.size()) {
    std::uninitialized_copy(correctLabels.begin(), correctLabels.end(),
                            getLabelsBuffer().begin());
  }

public:
  std::string getName() const override { return "re-label argument(s)"; }

  ArrayRef<Identifier> getLabels() const {
    return {getTrailingObjects<Identifier>(), NumLabels};
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static RelabelArguments *create(ConstraintSystem &cs,
                                  llvm::ArrayRef<Identifier> correctLabels,
                                  ConstraintLocator *locator);

private:
  MutableArrayRef<Identifier> getLabelsBuffer() {
    return {getTrailingObjects<Identifier>(), NumLabels};
  }
};

/// Add a new conformance to the type to satisfy a requirement.
class MissingConformance final : public ConstraintFix {
  Type NonConformingType;
  ProtocolDecl *Protocol;

  MissingConformance(ConstraintSystem &cs, Type type, ProtocolDecl *protocol,
                     ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AddConformance, locator),
        NonConformingType(type), Protocol(protocol) {}

public:
  std::string getName() const override {
    return "add missing protocol conformance";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static MissingConformance *create(ConstraintSystem &cs, Type type,
                                    ProtocolDecl *protocol,
                                    ConstraintLocator *locator);
};

/// Skip same-type generic requirement constraint,
/// and assume that types are equal.
class SkipSameTypeRequirement final : public ConstraintFix {
  Type LHS, RHS;

  SkipSameTypeRequirement(ConstraintSystem &cs, Type lhs, Type rhs,
                          ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::SkipSameTypeRequirement, locator), LHS(lhs),
        RHS(rhs) {}

public:
  std::string getName() const override {
    return "skip same-type generic requirement";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static SkipSameTypeRequirement *create(ConstraintSystem &cs, Type lhs,
                                         Type rhs, ConstraintLocator *locator);
};

/// Skip 'superclass' generic requirement constraint,
/// and assume that types are equal.
class SkipSuperclassRequirement final : public ConstraintFix {
  Type LHS, RHS;

  SkipSuperclassRequirement(ConstraintSystem &cs, Type lhs, Type rhs,
                            ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::SkipSameTypeRequirement, locator), LHS(lhs),
        RHS(rhs) {}

public:
  std::string getName() const override {
    return "skip superclass generic requirement";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static SkipSuperclassRequirement *
  create(ConstraintSystem &cs, Type lhs, Type rhs, ConstraintLocator *locator);
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSFIX_H
