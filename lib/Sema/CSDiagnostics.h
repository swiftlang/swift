//===--- CSDiagnostics.h - Constraint Diagnostics -------------------------===//
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
// This file provides necessary abstractions for constraint system diagnostics.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CSDIAGNOSTICS_H
#define SWIFT_SEMA_CSDIAGNOSTICS_H

#include "Constraint.h"
#include "ConstraintSystem.h"
#include "OverloadChoice.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/ArrayRef.h"

namespace swift {
namespace constraints {

/// Base class for all of the possible diagnostics,
/// provides most basic information such as location of
/// the problem, parent expression and some utility methods.
class FailureDiagnostic {
  Expr *E;
  const Solution &solution;
  ConstraintLocator *Locator;

public:
  FailureDiagnostic(Expr *expr, const Solution &solution,
                    ConstraintLocator *locator)
      : E(expr), solution(solution), Locator(locator) {}

  virtual ~FailureDiagnostic();

  /// Try to diagnose a problem given affected expression,
  /// failure location, types and declarations deduced by
  /// constraint system, and other auxiliary information.
  ///
  /// \returns true If the problem has been successfully diagnosed
  /// and diagnostic message emitted, false otherwise.
  virtual bool diagnose() = 0;

  ConstraintSystem &getConstraintSystem() const {
    return solution.getConstraintSystem();
  }

  Expr *getParentExpr() const { return E; }

  Expr *getAnchor() const { return Locator->getAnchor(); }

  ConstraintLocator *getLocator() const { return Locator; }

  Type getType(Expr *expr) const;

  template <typename... ArgTypes>
  InFlightDiagnostic emitDiagnostic(ArgTypes &&... Args) const;

protected:
  Optional<SelectedOverload>
  getOverloadChoiceIfAvailable(ConstraintLocator *locator) const {
    return solution.getOverloadChoiceIfAvailable(locator);
  }
};

/// Base class for all of the diagnostics related to generic requirement
/// failures, provides common information like failed requirement,
/// declaration where such requirement comes from, etc.
class RequirementFailure : public FailureDiagnostic {
  using PathEltKind = ConstraintLocator::PathElementKind;

protected:
  const ValueDecl *AffectedDecl;

public:
  RequirementFailure(Expr *expr, const Solution &solution,
                     ConstraintLocator *locator)
      : FailureDiagnostic(expr, solution, locator), AffectedDecl(getDeclRef()) {
  }

  unsigned getRequirementIndex() const {
    auto path = getLocator()->getPath();
    assert(!path.empty());

    auto &requirementLoc = path.back();
    assert(requirementLoc.getKind() == PathEltKind::TypeParameterRequirement);
    return requirementLoc.getValue();
  }

  /// The generic base type where failing requirement comes from.
  Type getOwnerType() const;

  /// Generic requirement associated with the failure.
  const Requirement &getRequirement();

private:
  /// Retrieve declaration associated with failing generic requirement.
  ValueDecl *getDeclRef() const;
};

/// Diagnostics for failed conformance checks originating from
/// generic requirements e.g.
/// ```swift
///   struct S {}
///   func foo<T: Hashable>(_ t: T) {}
///   foo(S())
/// ```
class MissingConformanceFailure final : public RequirementFailure {
  Type NonConformingType;
  ProtocolDecl *Protocol;

public:
  MissingConformanceFailure(Expr *expr, const Solution &solution,
                            ConstraintLocator *locator,
                            std::pair<TypeBase *, ProtocolDecl *> conformance)
      : RequirementFailure(expr, solution, locator),
        NonConformingType(conformance.first), Protocol(conformance.second) {}

  bool diagnose() override;

private:
  /// The type which was expected, by one of the generic requirements,
  /// to conform to associated protocol.
  Type getNonConformingType() const { return NonConformingType; }

  /// The protocol generic requirement expected associated type to conform to.
  Type getProtocolType() const { return Protocol->getDeclaredType(); }
};

/// Diagnose errors associated with missing, extraneous
/// or incorrect labels supplied by arguments, e.g.
/// ```swift
///   func foo(q: String, _ a: Int) {}
///   foo("ultimate quesiton", a: 42)
/// ```
/// Call to `foo` is going to be diagnosed as missing `q:`
/// and having extraneous `a:` labels, with appropriate fix-its added.
class LabelingFailure final : public FailureDiagnostic {
  ArrayRef<Identifier> CorrectLabels;

public:
  LabelingFailure(const Solution &solution, ConstraintLocator *locator,
                  ArrayRef<Identifier> labels)
      : FailureDiagnostic(nullptr, solution, locator), CorrectLabels(labels) {}

  bool diagnose() override;
};

/// Diagnose errors related to converting function type which
/// isn't explicitly '@escaping' to some other type.
class NoEscapeFuncToTypeConversionFailure final : public FailureDiagnostic {
  Type ConvertTo;

public:
  NoEscapeFuncToTypeConversionFailure(Expr *expr, const Solution &solution,
                                      ConstraintLocator *locator,
                                      Type toType = Type())
      : FailureDiagnostic(expr, solution, locator), ConvertTo(toType) {}

  bool diagnose() override;
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSDIAGNOSTICS_H
