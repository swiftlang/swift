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
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include <tuple>

namespace swift {
namespace constraints {

/// Base class for all of the possible diagnostics,
/// provides most basic information such as location of
/// the problem, parent expression and some utility methods.
class FailureDiagnostic {
  Expr *E;
  ConstraintSystem &CS;
  ConstraintLocator *Locator;

  Expr *Anchor;
  /// Indicates whether locator could be simplified
  /// down to anchor expression.
  bool HasComplexLocator;

public:
  FailureDiagnostic(Expr *expr, ConstraintSystem &cs,
                    ConstraintLocator *locator)
      : E(expr), CS(cs), Locator(locator) {
    std::tie(Anchor, HasComplexLocator) = computeAnchor();
  }

  virtual ~FailureDiagnostic();

  /// Try to diagnose a problem given affected expression,
  /// failure location, types and declarations deduced by
  /// constraint system, and other auxiliary information.
  ///
  /// \param asNote In ambiguity cases it's beneficial to
  /// produce diagnostic as a note instead of an error if possible.
  ///
  /// \returns true If the problem has been successfully diagnosed
  /// and diagnostic message emitted, false otherwise.
  bool diagnose(bool asNote = false);

  /// Try to produce an error diagnostic for the problem at hand.
  virtual bool diagnoseAsError() = 0;

  /// Instead of producing an error diagnostic, attempt to
  /// produce a "note" to complement some other diagnostic
  /// e.g. ambiguity error.
  virtual bool diagnoseAsNote();

  ConstraintSystem &getConstraintSystem() const {
    return CS;
  }

  Expr *getParentExpr() const { return E; }

  Expr *getAnchor() const { return Anchor; }

  ConstraintLocator *getLocator() const { return Locator; }

  Type getType(Expr *expr) const;

  /// Resolve type variables present in the raw type, if any.
  Type resolveType(Type rawType) const {
    return CS.simplifyType(rawType);
  }

  template <typename... ArgTypes>
  InFlightDiagnostic emitDiagnostic(ArgTypes &&... Args) const;

protected:
  TypeChecker &getTypeChecker() const { return CS.TC; }

  DeclContext *getDC() const { return CS.DC; }

  ASTContext &getASTContext() const { return CS.getASTContext(); }

  Optional<std::pair<Type, ConversionRestrictionKind>>
  getRestrictionForType(Type type) const {
    for (auto &restriction : CS.ConstraintRestrictions) {
      if (std::get<0>(restriction)->isEqual(type))
        return std::pair<Type, ConversionRestrictionKind>(
            std::get<1>(restriction), std::get<2>(restriction));
    }
    return None;
  }

  ValueDecl *getResolvedMemberRef(UnresolvedDotExpr *member) {
    auto locator = CS.getConstraintLocator(member, ConstraintLocator::Member);
    return CS.findResolvedMemberRef(locator);
  }

  Optional<SelectedOverload>
  getOverloadChoiceIfAvailable(ConstraintLocator *locator) const {
    if (auto *overload = getResolvedOverload(locator))
      return Optional<SelectedOverload>(
           {overload->Choice, overload->OpenedFullType, overload->ImpliedType});
    return None;
  }

  /// Retrieve overload choice resolved for given locator
  /// by the constraint solver.
  ResolvedOverloadSetListItem *
  getResolvedOverload(ConstraintLocator *locator) const {
    auto resolvedOverload = CS.getResolvedOverloadSets();
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
protected:
  using PathEltKind = ConstraintLocator::PathElementKind;
  using DiagOnDecl = Diag<DescriptiveDeclKind, DeclName, Type, Type>;
  using DiagInReference = Diag<DescriptiveDeclKind, DeclName, Type, Type, Type>;
  using DiagAsNote = Diag<Type, Type, Type, Type, StringRef>;

  const ValueDecl *AffectedDecl;
  /// If possible, find application expression associated
  /// with current generic requirement failure, that helps
  /// to diagnose failures related to arguments.
  const ApplyExpr *Apply = nullptr;

public:
  RequirementFailure(ConstraintSystem &cs, Expr *expr, RequirementKind kind,
                     ConstraintLocator *locator)
      : FailureDiagnostic(expr, cs, locator), AffectedDecl(getDeclRef()) {
    assert(locator);
    assert(AffectedDecl);

    auto path = locator->getPath();
    assert(!path.empty());

    auto &last = path.back();
    assert(last.getKind() == ConstraintLocator::TypeParameterRequirement);
    assert(static_cast<RequirementKind>(last.getValue2()) == kind);

    // It's possible sometimes not to have no base expression.
    if (!expr)
      return;

    auto *anchor = getAnchor();
    expr->forEachChildExpr([&](Expr *subExpr) -> Expr * {
      auto *AE = dyn_cast<ApplyExpr>(subExpr);
      if (!AE || AE->getFn() != anchor)
        return subExpr;

      Apply = AE;
      return nullptr;
    });
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

  /// Generic context associated with the failure.
  const GenericContext *getGenericContext() const;

  /// Generic requirement associated with the failure.
  const Requirement &getRequirement() const;

  virtual Type getLHS() const = 0;
  virtual Type getRHS() const = 0;

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;

protected:
  /// Retrieve declaration contextual where current
  /// requirement has been introduced.
  const DeclContext *getRequirementDC() const;

  virtual DiagOnDecl getDiagnosticOnDecl() const = 0;
  virtual DiagInReference getDiagnosticInRereference() const = 0;
  virtual DiagAsNote getDiagnosticAsNote() const = 0;

  /// Determine whether it would be possible to diagnose
  /// current requirement failure.
  bool canDiagnoseFailure() const {
    // For static/initializer calls there is going to be
    // a separate fix, attached to the argument, which is
    // much easier to diagnose.
    // For operator calls we can't currently produce a good
    // diagnostic, so instead let's refer to expression diagnostics.
    return !(Apply && (isOperator(Apply) || isa<TypeExpr>(getAnchor())));
  }

  static bool isOperator(const ApplyExpr *apply) {
    return isa<PrefixUnaryExpr>(apply) || isa<PostfixUnaryExpr>(apply) ||
           isa<BinaryExpr>(apply);
  }

private:
  /// Retrieve declaration associated with failing generic requirement.
  ValueDecl *getDeclRef() const;

  void emitRequirementNote(const Decl *anchor) const;
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
  MissingConformanceFailure(Expr *expr, ConstraintSystem &cs,
                            ConstraintLocator *locator,
                            std::pair<Type, ProtocolDecl *> conformance)
      : RequirementFailure(cs, expr, RequirementKind::Conformance, locator),
        NonConformingType(conformance.first), Protocol(conformance.second) {}

  bool diagnoseAsError() override;

private:
  /// The type which was expected, by one of the generic requirements,
  /// to conform to associated protocol.
  Type getLHS() const override { return NonConformingType; }

  /// The protocol generic requirement expected associated type to conform to.
  Type getRHS() const override { return Protocol->getDeclaredType(); }

protected:
  DiagOnDecl getDiagnosticOnDecl() const override {
    return diag::type_does_not_conform_decl_owner;
  }

  DiagInReference getDiagnosticInRereference() const override {
    return diag::type_does_not_conform_in_decl_ref;
  }

  DiagAsNote getDiagnosticAsNote() const override {
    return diag::candidate_types_conformance_requirement;
  }
};

/// Diagnose failures related to same-type generic requirements, e.g.
/// ```swift
/// protocol P {
///   associatedtype T
/// }
///
/// struct S : P {
///   typealias T = String
/// }
///
/// func foo<U: P>(_ t: [U]) where U.T == Int {}
/// foo([S()])
/// ```
///
/// `S.T` is not the same type as `Int`, which is required by `foo`.
class SameTypeRequirementFailure final : public RequirementFailure {
  Type LHS, RHS;

public:
  SameTypeRequirementFailure(Expr *expr, ConstraintSystem &cs, Type lhs,
                             Type rhs, ConstraintLocator *locator)
      : RequirementFailure(cs, expr, RequirementKind::SameType, locator),
        LHS(lhs), RHS(rhs) {}

  Type getLHS() const override { return LHS; }
  Type getRHS() const override { return RHS; }

protected:
  DiagOnDecl getDiagnosticOnDecl() const override {
    return diag::types_not_equal_decl;
  }

  DiagInReference getDiagnosticInRereference() const override {
    return diag::types_not_equal_in_decl_ref;
  }

  DiagAsNote getDiagnosticAsNote() const override {
    return diag::candidate_types_equal_requirement;
  }
};

/// Diagnose failures related to superclass generic requirements, e.g.
/// ```swift
/// class A {
/// }
///
/// class B {
/// }
///
/// func foo<T>(_ t: [T]) where T: A {}
/// foo([B()])
/// ```
///
/// `A` is not the superclass of `B`, which is required by `foo<T>`.
class SuperclassRequirementFailure final : public RequirementFailure {
  Type LHS, RHS;

public:
  SuperclassRequirementFailure(Expr *expr, ConstraintSystem &cs, Type lhs,
                               Type rhs, ConstraintLocator *locator)
      : RequirementFailure(cs, expr, RequirementKind::Superclass, locator),
        LHS(lhs), RHS(rhs) {}

  Type getLHS() const override { return LHS; }
  Type getRHS() const override { return RHS; }

protected:
  DiagOnDecl getDiagnosticOnDecl() const override {
    return diag::types_not_inherited_decl;
  }

  DiagInReference getDiagnosticInRereference() const override {
    return diag::types_not_inherited_in_decl_ref;
  }

  DiagAsNote getDiagnosticAsNote() const override {
    return diag::candidate_types_inheritance_requirement;
  }
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
  LabelingFailure(ConstraintSystem &cs, ConstraintLocator *locator,
                  ArrayRef<Identifier> labels)
      : FailureDiagnostic(nullptr, cs, locator), CorrectLabels(labels) {}

  bool diagnoseAsError() override;
};

/// Diagnose errors related to converting function type which
/// isn't explicitly '@escaping' to some other type.
class NoEscapeFuncToTypeConversionFailure final : public FailureDiagnostic {
  Type ConvertTo;

public:
  NoEscapeFuncToTypeConversionFailure(Expr *expr, ConstraintSystem &cs,
                                      ConstraintLocator *locator,
                                      Type toType = Type())
      : FailureDiagnostic(expr, cs, locator), ConvertTo(toType) {}

  bool diagnoseAsError() override;
};

class MissingForcedDowncastFailure final : public FailureDiagnostic {
public:
  MissingForcedDowncastFailure(Expr *expr, ConstraintSystem &cs,
                               ConstraintLocator *locator)
      : FailureDiagnostic(expr, cs, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose failures related to passing value of some type
/// to `inout` parameter, without explicitly specifying `&`.
class MissingAddressOfFailure final : public FailureDiagnostic {
public:
  MissingAddressOfFailure(Expr *expr, ConstraintSystem &cs,
                          ConstraintLocator *locator)
      : FailureDiagnostic(expr, cs, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose failures related attempt to implicitly convert types which
/// do not support such implicit converstion.
/// "as" or "as!" has to be specified explicitly in cases like that.
class MissingExplicitConversionFailure final : public FailureDiagnostic {
  Type ConvertingTo;

public:
  MissingExplicitConversionFailure(Expr *expr, ConstraintSystem &cs,
                                   ConstraintLocator *locator, Type toType)
      : FailureDiagnostic(expr, cs, locator), ConvertingTo(toType) {}

  bool diagnoseAsError() override;

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
  MemberAccessOnOptionalBaseFailure(Expr *expr, ConstraintSystem &cs,
                                    ConstraintLocator *locator,
                                    DeclName memberName, bool resultOptional)
      : FailureDiagnostic(expr, cs, locator), Member(memberName),
        ResultTypeIsOptional(resultOptional) {}

  bool diagnoseAsError() override;
};

/// Diagnose failures related to use of the unwrapped optional types,
/// which require some type of force-unwrap e.g. "!" or "try!".
class MissingOptionalUnwrapFailure final : public FailureDiagnostic {
public:
  MissingOptionalUnwrapFailure(Expr *expr, ConstraintSystem &cs,
                               ConstraintLocator *locator)
      : FailureDiagnostic(expr, cs, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose errors associated with rvalues in positions
/// where an lvalue is required, such as inout arguments.
class RValueTreatedAsLValueFailure final : public FailureDiagnostic {

public:
  RValueTreatedAsLValueFailure(ConstraintSystem &cs, ConstraintLocator *locator)
      : FailureDiagnostic(nullptr, cs, locator) {}

  bool diagnoseAsError() override;
};

class TrailingClosureAmbiguityFailure final : public FailureDiagnostic {
  ArrayRef<OverloadChoice> Choices;

public:
  TrailingClosureAmbiguityFailure(Expr *root, ConstraintSystem &cs,
                                  Expr *anchor,
                                  ArrayRef<OverloadChoice> choices)
      : FailureDiagnostic(root, cs, cs.getConstraintLocator(anchor)),
        Choices(choices) {}

  bool diagnoseAsError() override { return false; }

  bool diagnoseAsNote() override;
};

/// Diagnose errors related to assignment expressions e.g.
/// trying to assign something to immutable value, or trying
/// to access mutating member on immutable base.
class AssignmentFailure final : public FailureDiagnostic {
  SourceLoc Loc;
  Diag<StringRef> DeclDiagnostic;
  Diag<Type> TypeDiagnostic;

public:
  AssignmentFailure(Expr *destExpr, ConstraintSystem &cs,
                    SourceLoc diagnosticLoc);

  AssignmentFailure(Expr *destExpr, ConstraintSystem &cs,
                    SourceLoc diagnosticLoc, Diag<StringRef> declDiag,
                    Diag<Type> typeDiag)
      : FailureDiagnostic(destExpr, cs, cs.getConstraintLocator(destExpr)),
        Loc(diagnosticLoc), DeclDiagnostic(declDiag), TypeDiagnostic(typeDiag) {
  }

  bool diagnoseAsError() override;

private:
  void fixItChangeInoutArgType(const Expr *arg, Type actualType,
                               Type neededType) const;

  /// Given an expression that has a non-lvalue type, dig into it until
  /// we find the part of the expression that prevents the entire subexpression
  /// from being mutable.  For example, in a sequence like "x.v.v = 42" we want
  /// to complain about "x" being a let property if "v.v" are both mutable.
  ///
  /// \returns The base subexpression that looks immutable (or that can't be
  /// analyzed any further) along with a decl extracted from it if we could.
  std::pair<Expr *, ValueDecl *> resolveImmutableBase(Expr *expr) const;

  static Diag<StringRef> findDeclDiagonstic(ASTContext &ctx, Expr *destExpr);

  static bool isLoadedLValue(Expr *expr) {
    expr = expr->getSemanticsProvidingExpr();
    if (isa<LoadExpr>(expr))
      return true;
    if (auto ifExpr = dyn_cast<IfExpr>(expr))
      return isLoadedLValue(ifExpr->getThenExpr()) &&
             isLoadedLValue(ifExpr->getElseExpr());
    return false;
  }
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSDIAGNOSTICS_H
