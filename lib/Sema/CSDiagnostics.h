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

  Expr *Anchor;
  /// Indicates whether locator could be simplified
  /// down to anchor expression.
  bool HasComplexLocator;

public:
  FailureDiagnostic(Expr *expr, const Solution &solution,
                    ConstraintLocator *locator)
      : E(expr), solution(solution), Locator(locator) {
    std::tie(Anchor, HasComplexLocator) = computeAnchor();
  }

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

  Expr *getAnchor() const { return Anchor; }

  ConstraintLocator *getLocator() const { return Locator; }

  Type getType(Expr *expr) const;

  /// Resolve type variables present in the raw type, if any.
  Type resolveType(Type rawType) const {
    return solution.simplifyType(rawType);
  }

  template <typename... ArgTypes>
  InFlightDiagnostic emitDiagnostic(ArgTypes &&... Args) const;

protected:
  TypeChecker &getTypeChecker() const { return getConstraintSystem().TC; }

  DeclContext *getDC() const { return getConstraintSystem().DC; }

  Optional<SelectedOverload>
  getOverloadChoiceIfAvailable(ConstraintLocator *locator) const {
    return solution.getOverloadChoiceIfAvailable(locator);
  }

  /// Retrieve overload choice resolved for given locator
  /// by the constraint solver.
  ResolvedOverloadSetListItem *getResolvedOverload(ConstraintLocator *locator) {
    auto resolvedOverload = getConstraintSystem().getResolvedOverloadSets();
    while (resolvedOverload) {
      if (resolvedOverload->Locator == locator)
        return resolvedOverload;
      resolvedOverload = resolvedOverload->Previous;
    }
    return nullptr;
  }

  /// \returns true is locator hasn't been simplified down to expression.
  bool hasComplexLocator() const { return HasComplexLocator; }

private:
  /// Compute anchor expression associated with current diagnostic.
  std::pair<Expr *, bool> computeAnchor() const;
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
  const Requirement &getRequirement() const;

protected:
  /// Retrieve declaration contextual where current
  /// requirement has been introduced.
  const DeclContext *getRequirementDC() const;

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
                            std::pair<Type, ProtocolDecl *> conformance)
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

class MissingForcedDowncastFailure final : public FailureDiagnostic {
public:
  MissingForcedDowncastFailure(Expr *expr, const Solution &solution,
                               ConstraintLocator *locator)
      : FailureDiagnostic(expr, solution, locator) {}

  bool diagnose() override;
};

/// Diagnose failures related to passing value of some type
/// to `inout` parameter, without explicitly specifying `&`.
class MissingAddressOfFailure final : public FailureDiagnostic {
public:
  MissingAddressOfFailure(Expr *expr, const Solution &solution,
                          ConstraintLocator *locator)
      : FailureDiagnostic(expr, solution, locator) {}

  bool diagnose() override;
};

/// Diagnose failures related attempt to implicitly convert types which
/// do not support such implicit converstion.
/// "as" or "as!" has to be specified explicitly in cases like that.
class MissingExplicitConversionFailure final : public FailureDiagnostic {
  Type ConvertingTo;

public:
  MissingExplicitConversionFailure(Expr *expr, const Solution &solution,
                                   ConstraintLocator *locator, Type toType)
      : FailureDiagnostic(expr, solution, locator), ConvertingTo(toType) {}

  bool diagnose() override;

private:
  bool exprNeedsParensBeforeAddingAs(Expr *expr) {
    auto *DC = getDC();
    auto &TC = getTypeChecker();

    auto asPG = TC.lookupPrecedenceGroup(
        DC, DC->getASTContext().Id_CastingPrecedence, SourceLoc());
    if (!asPG)
      return true;
    return exprNeedsParensInsideFollowingOperator(TC, DC, expr, asPG);
  }

  bool exprNeedsParensAfterAddingAs(Expr *expr, Expr *rootExpr) {
    auto *DC = getDC();
    auto &TC = getTypeChecker();

    auto asPG = TC.lookupPrecedenceGroup(
        DC, DC->getASTContext().Id_CastingPrecedence, SourceLoc());
    if (!asPG)
      return true;

    return exprNeedsParensOutsideFollowingOperator(TC, DC, expr, rootExpr,
                                                   asPG);
  }
};

/// Diagnose failures related to attempting member access on optional base
/// type without optional chaining or force-unwrapping it first.
class MemberAccessOnOptionalBaseFailure final : public FailureDiagnostic {
  DeclName Member;
  bool ResultTypeIsOptional;

public:
  MemberAccessOnOptionalBaseFailure(Expr *expr, const Solution &solution,
                                    ConstraintLocator *locator,
                                    DeclName memberName, bool resultOptional)
      : FailureDiagnostic(expr, solution, locator), Member(memberName),
        ResultTypeIsOptional(resultOptional) {}

  bool diagnose() override;
};

/// Diagnose failures related to use of the unwrapped optional types,
/// which require some type of force-unwrap e.g. "!" or "try!".
class MissingOptionalUnwrapFailure final : public FailureDiagnostic {
public:
  MissingOptionalUnwrapFailure(Expr *expr, const Solution &solution,
                               ConstraintLocator *locator)
      : FailureDiagnostic(expr, solution, locator) {}

  bool diagnose() override;
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSDIAGNOSTICS_H
