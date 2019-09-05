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
#include "swift/AST/Identifier.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include <tuple>

namespace swift {
namespace constraints {

class FunctionArgApplyInfo;

/// Base class for all of the possible diagnostics,
/// provides most basic information such as location of
/// the problem, parent expression and some utility methods.
class FailureDiagnostic {
  Expr *E;
  ConstraintSystem &CS;
  ConstraintLocator *Locator;

  /// The original anchor before any simplification.
  Expr *RawAnchor;
  /// Simplified anchor associated with the given locator.
  Expr *Anchor;
  /// Indicates whether locator could be simplified
  /// down to anchor expression.
  bool HasComplexLocator;

public:
  FailureDiagnostic(Expr *expr, ConstraintSystem &cs,
                    ConstraintLocator *locator)
      : E(expr), CS(cs), Locator(locator), RawAnchor(locator->getAnchor()) {
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
  ///
  /// \returns true If anything was diagnosed, false otherwise.
  virtual bool diagnoseAsError() = 0;

  /// Instead of producing an error diagnostic, attempt to
  /// produce a "note" to complement some other diagnostic
  /// e.g. ambiguity error.
  virtual bool diagnoseAsNote();

  ConstraintSystem &getConstraintSystem() const {
    return CS;
  }

  Expr *getParentExpr() const { return E; }

  Expr *getRawAnchor() const { return RawAnchor; }

  Expr *getAnchor() const { return Anchor; }

  ConstraintLocator *getLocator() const { return Locator; }

  Type getType(Expr *expr) const;
  Type getType(const TypeLoc &loc) const;

  /// Resolve type variables present in the raw type, if any.
  Type resolveType(Type rawType, bool reconstituteSugar = false) const {
    auto resolvedType = CS.simplifyType(rawType);
    return reconstituteSugar
               ? resolvedType->reconstituteSugar(/*recursive*/ true)
               : resolvedType;
  }

  /// Resolve type variables present in the raw type, using generic parameter
  /// types where possible.
  Type resolveInterfaceType(Type type, bool reconstituteSugar = false) const;

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

  ValueDecl *getResolvedMemberRef(UnresolvedDotExpr *member) const {
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

  /// Retrive the constraint locator for the given anchor and
  /// path, uniqued and automatically calculate the summary flags
  ConstraintLocator *
  getConstraintLocator(Expr *anchor,
                       ArrayRef<ConstraintLocator::PathElement> path) {
    return CS.getConstraintLocator(anchor, path);
  }

  /// \returns true is locator hasn't been simplified down to expression.
  bool hasComplexLocator() const { return HasComplexLocator; }

  /// \returns A parent expression if sub-expression is contained anywhere
  /// in the root expression or `nullptr` otherwise.
  Expr *findParentExpr(Expr *subExpr) const;

  /// If given expression is some kind of a member reference e.g.
  /// `x.foo` or `x[0]` extract and return its base expression.
  Expr *getBaseExprFor(Expr *anchor) const;

  /// \returns An argument expression if given anchor is a call, member
  /// reference or subscript, nullptr otherwise.
  Expr *getArgumentExprFor(Expr *anchor) const;

  Optional<SelectedOverload> getChoiceFor(Expr *) const;

  /// For a given locator describing a function argument conversion, or a
  /// constraint within an argument conversion, returns information about the
  /// application of the argument to its parameter. If the locator is not
  /// for an argument conversion, returns \c None.
  Optional<FunctionArgApplyInfo>
  getFunctionArgApplyInfo(ConstraintLocator *locator) const;

  /// \returns A new type with all of the type variables associated with
  /// generic parameters substituted back into being generic parameter type.
  Type restoreGenericParameters(
      Type type,
      llvm::function_ref<void(GenericTypeParamType *, Type)> substitution =
          [](GenericTypeParamType *, Type) {});

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

  /// If this failure associated with one of the conditional requirements,
  /// this field would represent conformance where requirement comes from.
  const ProtocolConformance *Conformance = nullptr;

  /// The source of the requirement, if available. One exception
  /// is failure associated with conditional requirement where
  /// underlying conformance is specialized.
  const GenericSignature *Signature;

  const ValueDecl *AffectedDecl;
  /// If possible, find application expression associated
  /// with current generic requirement failure, that helps
  /// to diagnose failures related to arguments.
  const ApplyExpr *Apply = nullptr;

  /// Types associated with requirement constraint this
  /// failure originates from.
  Type LHS, RHS;

public:
  RequirementFailure(ConstraintSystem &cs, Expr *expr, RequirementKind kind,
                     Type lhs, Type rhs, ConstraintLocator *locator)
      : FailureDiagnostic(expr, cs, locator),
        Conformance(getConformanceForConditionalReq(locator)),
        Signature(getSignature(locator)), AffectedDecl(getDeclRef()),
        LHS(resolveType(lhs)), RHS(resolveType(rhs)) {
    assert(locator);
    assert(isConditional() || Signature);
    assert(AffectedDecl);
    assert(getRequirementDC() &&
           "Couldn't find where the requirement came from?");
    assert(getGenericContext() &&
           "Affected decl not within a generic context?");

    auto reqElt = locator->castLastElementTo<LocatorPathElt::AnyRequirement>();
    assert(reqElt.getRequirementKind() == kind);

    // It's possible sometimes not to have no base expression.
    if (!expr)
      return;

    if (auto *parentExpr = findParentExpr(getRawAnchor()))
      Apply = dyn_cast<ApplyExpr>(parentExpr);
  }

  unsigned getRequirementIndex() const {
    auto reqElt =
        getLocator()->castLastElementTo<LocatorPathElt::AnyRequirement>();
    return reqElt.getIndex();
  }

  /// The generic base type where failing requirement comes from.
  Type getOwnerType() const;

  /// Generic context associated with the failure.
  const GenericContext *getGenericContext() const;

  /// Generic requirement associated with the failure.
  const Requirement &getRequirement() const;

  Type getLHS() const { return LHS; }
  Type getRHS() const { return RHS; }

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;

protected:
  /// Determine whether this is a conditional requirement failure.
  bool isConditional() const { return bool(Conformance); }

  /// Check whether this requirement comes from the contextual type
  /// that root expression is coerced/converted into.
  bool isFromContextualType() const;

  /// Retrieve declaration contextual where current
  /// requirement has been introduced.
  const DeclContext *getRequirementDC() const;

  virtual DiagOnDecl getDiagnosticOnDecl() const = 0;
  virtual DiagInReference getDiagnosticInRereference() const = 0;
  virtual DiagAsNote getDiagnosticAsNote() const = 0;

  /// Determine whether it would be possible to diagnose
  /// current requirement failure.
  bool canDiagnoseFailure() const {
    // If this is a conditional requirement failure,
    // we have a lot more information compared to
    // type requirement case, because we know that
    // underlying conformance requirement matched.
    if (isConditional())
      return true;

    // For static/initializer calls there is going to be
    // a separate fix, attached to the argument, which is
    // much easier to diagnose.
    // For operator calls we can't currently produce a good
    // diagnostic, so instead let's refer to expression diagnostics.
    return !(Apply && isOperator(Apply));
  }

  static bool isOperator(const ApplyExpr *apply) {
    return isa<PrefixUnaryExpr>(apply) || isa<PostfixUnaryExpr>(apply) ||
           isa<BinaryExpr>(apply);
  }

private:
  /// Retrieve declaration associated with failing generic requirement.
  ValueDecl *getDeclRef() const;

  /// Retrieve generic signature where this parameter originates from.
  GenericSignature *getSignature(ConstraintLocator *locator);

  void emitRequirementNote(const Decl *anchor, Type lhs, Type rhs) const;

  /// Determine whether given declaration represents a static
  /// or instance property/method, excluding operators.
  static bool isStaticOrInstanceMember(const ValueDecl *decl);

  /// If this is a failure in conditional requirement, retrieve
  /// conformance information.
  ProtocolConformance *
  getConformanceForConditionalReq(ConstraintLocator *locator);
};

/// Diagnostics for failed conformance checks originating from
/// generic requirements e.g.
/// ```swift
///   struct S {}
///   func foo<T: Hashable>(_ t: T) {}
///   foo(S())
/// ```
class MissingConformanceFailure final : public RequirementFailure {
public:
  MissingConformanceFailure(Expr *expr, ConstraintSystem &cs,
                            ConstraintLocator *locator,
                            std::pair<Type, Type> conformance)
      : RequirementFailure(cs, expr, RequirementKind::Conformance,
                           conformance.first, conformance.second, locator) {}

  bool diagnoseAsError() override;

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
public:
  SameTypeRequirementFailure(Expr *expr, ConstraintSystem &cs, Type lhs,
                             Type rhs, ConstraintLocator *locator)
      : RequirementFailure(cs, expr, RequirementKind::SameType, lhs, rhs,
                           locator) {}

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
public:
  SuperclassRequirementFailure(Expr *expr, ConstraintSystem &cs, Type lhs,
                               Type rhs, ConstraintLocator *locator)
      : RequirementFailure(cs, expr, RequirementKind::Superclass, lhs, rhs,
                           locator) {}

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
  LabelingFailure(Expr *root, ConstraintSystem &cs, ConstraintLocator *locator,
                  ArrayRef<Identifier> labels)
      : FailureDiagnostic(root, cs, locator), CorrectLabels(labels) {}

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;
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

private:
  /// Emit tailored diagnostics for no-escape parameter conversions e.g.
  /// passing such parameter as an @escaping argument, or trying to
  /// assign it to a variable which expects @escaping function.
  bool diagnoseParameterUse() const;
};

class MissingForcedDowncastFailure final : public FailureDiagnostic {
public:
  MissingForcedDowncastFailure(Expr *expr, ConstraintSystem &cs,
                               ConstraintLocator *locator)
      : FailureDiagnostic(expr, cs, locator) {}

  bool diagnoseAsError() override;
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
  Type BaseType;
  Type UnwrappedType;

public:
  MissingOptionalUnwrapFailure(Expr *expr, ConstraintSystem &cs, Type baseType,
                               Type unwrappedType, ConstraintLocator *locator)
      : FailureDiagnostic(expr, cs, locator), BaseType(baseType),
        UnwrappedType(unwrappedType) {}

  bool diagnoseAsError() override;

private:
  Type getBaseType() const {
    return resolveType(BaseType, /*reconstituteSugar=*/true);
  }

  Type getUnwrappedType() const {
    return resolveType(UnwrappedType, /*reconstituteSugar=*/true);
  }

  /// Suggest a default value via `?? <default value>`
  void offerDefaultValueUnwrapFixIt(DeclContext *DC, Expr *expr) const;
  /// Suggest a force optional unwrap via `!`
  void offerForceUnwrapFixIt(Expr *expr) const;
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
  /// Given an expression that has a non-lvalue type, dig into it until
  /// we find the part of the expression that prevents the entire subexpression
  /// from being mutable.  For example, in a sequence like "x.v.v = 42" we want
  /// to complain about "x" being a let property if "v.v" are both mutable.
  ///
  /// \returns The base subexpression that looks immutable (or that can't be
  /// analyzed any further) along with an OverloadChoice extracted from it if we
  /// could.
  std::pair<Expr *, Optional<OverloadChoice>>
  resolveImmutableBase(Expr *expr) const;

  static Diag<StringRef> findDeclDiagonstic(ASTContext &ctx, Expr *destExpr);

  /// Retrive an member reference associated with given member
  /// looking through dynamic member lookup on the way.
  Optional<OverloadChoice> getMemberRef(ConstraintLocator *locator) const;
};

/// Intended to diagnose any possible contextual failure
/// e.g. argument/parameter, closure result, conversions etc.
class ContextualFailure : public FailureDiagnostic {
  ContextualTypePurpose CTP;
  Type FromType, ToType;

public:
  ContextualFailure(Expr *root, ConstraintSystem &cs, Type lhs, Type rhs,
                    ConstraintLocator *locator)
      : ContextualFailure(root, cs, cs.getContextualTypePurpose(), lhs, rhs,
                          locator) {}

  ContextualFailure(Expr *root, ConstraintSystem &cs,
                    ContextualTypePurpose purpose, Type lhs, Type rhs,
                    ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), CTP(purpose),
        FromType(resolve(lhs)), ToType(resolve(rhs)) {}

  Type getFromType() const { return FromType; }

  Type getToType() const { return ToType; }

  bool diagnoseAsError() override;

  /// If we're trying to convert something to `nil`.
  bool diagnoseConversionToNil() const;

  // If we're trying to convert something of type "() -> T" to T,
  // then we probably meant to call the value.
  bool diagnoseMissingFunctionCall() const;

  /// Produce a specialized diagnostic if this is an invalid conversion to Bool.
  bool diagnoseConversionToBool() const;

  /// Produce a specialized diagnostic if this is an attempt to initialize
  /// or convert an array literal to a dictionary e.g. `let _: [String: Int] = ["A", 0]`
  bool diagnoseConversionToDictionary() const;

  /// Produce a specialized diagnostic if this is an attempt to throw
  /// something with doesn't conform to `Error`.
  bool diagnoseThrowsTypeMismatch() const;

  /// Produce a specialized diagnostic if this is an attempt to `yield`
  /// something of incorrect type.
  bool diagnoseYieldByReferenceMismatch() const;

  /// Attempt to attach any relevant fix-its to already produced diagnostic.
  void tryFixIts(InFlightDiagnostic &diagnostic) const;

  /// Attempts to add fix-its for these two mistakes:
  ///
  /// - Passing an integer where a type conforming to RawRepresentable is
  ///   expected, by wrapping the expression in a call to the contextual
  ///   type's initializer
  ///
  /// - Passing a type conforming to RawRepresentable where an integer is
  ///   expected, by wrapping the expression in a call to the rawValue
  ///   accessor
  ///
  /// - Return true on the fixit is added, false otherwise.
  ///
  /// This helps migration with SDK changes.
  bool
  tryRawRepresentableFixIts(InFlightDiagnostic &diagnostic,
                            KnownProtocolKind rawRepresentablePrococol) const;

  /// Attempts to add fix-its for these two mistakes:
  ///
  /// - Passing an integer with the right type but which is getting wrapped with
  ///   a different integer type unnecessarily. The fixit removes the cast.
  ///
  /// - Passing an integer but expecting different integer type. The fixit adds
  ///   a wrapping cast.
  ///
  /// - Return true on the fixit is added, false otherwise.
  ///
  /// This helps migration with SDK changes.
  bool tryIntegerCastFixIts(InFlightDiagnostic &diagnostic) const;

protected:
  /// Try to add a fix-it when converting between a collection and its slice
  /// type, such as String <-> Substring or (eventually) Array <-> ArraySlice
  bool trySequenceSubsequenceFixIts(InFlightDiagnostic &diagnostic) const;

  /// Try to add a fix-it that suggests to explicitly use `as` or `as!`
  /// to coerce one type to another if type-checker can prove that such
  /// conversion is possible.
  bool tryTypeCoercionFixIt(InFlightDiagnostic &diagnostic) const;

  /// Try to add a fix-it to conform the decl context (if it's a type) to the
  /// protocol
  bool tryProtocolConformanceFixIt(InFlightDiagnostic &diagnostic) const;

  /// Check whether this contextual failure represents an invalid
  /// conversion from array literal to dictionary.
  static bool isInvalidDictionaryConversion(ConstraintSystem &cs, Expr *anchor,
                                            Type contextualType);

private:
  Type resolve(Type rawType) {
    auto type = resolveType(rawType)->getWithoutSpecifierType();
    if (auto *BGT = type->getAs<BoundGenericType>()) {
      if (BGT->hasUnresolvedType())
        return BGT->getDecl()->getDeclaredInterfaceType();
    }
    return type;
  }

  /// Try to add a fix-it to convert a stored property into a computed
  /// property
  void tryComputedPropertyFixIts(Expr *expr) const;

  bool isIntegerType(Type type) const {
    return conformsToKnownProtocol(
        getConstraintSystem(), type,
        KnownProtocolKind::ExpressibleByIntegerLiteral);
  }

  /// Return true if the conversion from fromType to toType is
  /// an invalid string index operation.
  bool isIntegerToStringIndexConversion() const;

protected:
  ContextualTypePurpose getContextualTypePurpose() const { return CTP; }

  static Optional<Diag<Type, Type>>
  getDiagnosticFor(ContextualTypePurpose context, bool forProtocol);
};

/// Diagnostics for mismatched generic arguments e.g
/// ```swift
/// struct F<G> {}
/// let _:F<Int> = F<Bool>()
/// ```
class GenericArgumentsMismatchFailure final : public ContextualFailure {
  ArrayRef<unsigned> Mismatches;

public:
  GenericArgumentsMismatchFailure(Expr *expr, ConstraintSystem &cs,
                                  Type actualType, Type requiredType,
                                  ArrayRef<unsigned> mismatches,
                                  ConstraintLocator *locator)
      : ContextualFailure(expr, cs, actualType, requiredType, locator),
        Mismatches(mismatches) {
    assert(actualType->is<BoundGenericType>());
    assert(requiredType->is<BoundGenericType>());
  }

  bool diagnoseAsError() override;

private:
  void emitNotesForMismatches() {
    for (unsigned position : Mismatches) {
      emitNoteForMismatch(position);
    }
  }

  void emitNoteForMismatch(int mismatchPosition);

  Optional<Diag<Type, Type>> getDiagnosticFor(ContextualTypePurpose context);

  /// The actual type being used.
  BoundGenericType *getActual() const {
    return getFromType()->castTo<BoundGenericType>();
  }

  /// The type needed by the generic requirement.
  BoundGenericType *getRequired() const {
    return getToType()->castTo<BoundGenericType>();
  }
};

/// Diagnose failures related to conversion between throwing function type
/// and non-throwing one e.g.
///
/// ```swift
/// func foo<T>(_ t: T) throws -> Void {}
/// let _: (Int) -> Void = foo // `foo` can't be implictly converted to
///                            // non-throwing type `(Int) -> Void`
/// ```
class ThrowingFunctionConversionFailure final : public ContextualFailure {
public:
  ThrowingFunctionConversionFailure(Expr *root, ConstraintSystem &cs,
                                    Type fromType, Type toType,
                                    ConstraintLocator *locator)
      : ContextualFailure(root, cs, fromType, toType, locator) {
    auto fnType1 = fromType->castTo<FunctionType>();
    auto fnType2 = toType->castTo<FunctionType>();
    assert(fnType1->throws() != fnType2->throws());
  }

  bool diagnoseAsError() override;
};

/// Diagnose failures related attempt to implicitly convert types which
/// do not support such implicit converstion.
/// "as" or "as!" has to be specified explicitly in cases like that.
class MissingExplicitConversionFailure final : public ContextualFailure {
public:
  MissingExplicitConversionFailure(Expr *expr, ConstraintSystem &cs,
                                   Type fromType, Type toType,
                                   ConstraintLocator *locator)
      : ContextualFailure(expr, cs, fromType, toType, locator) {}

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

/// Diagnose failures related to passing value of some type
/// to `inout` or pointer parameter, without explicitly specifying `&`.
class MissingAddressOfFailure final : public ContextualFailure {
public:
  MissingAddressOfFailure(Expr *expr, ConstraintSystem &cs, Type argTy,
                          Type paramTy, ConstraintLocator *locator)
      : ContextualFailure(expr, cs, argTy, paramTy, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose extraneous use of address of (`&`) which could only be
/// associated with arguments to inout parameters e.g.
///
/// ```swift
/// struct S {}
///
/// var a: S = ...
/// var b: S = ...
///
/// a = &b
/// ```
class InvalidUseOfAddressOf final : public ContextualFailure {
public:
  InvalidUseOfAddressOf(Expr *root, ConstraintSystem &cs, Type lhs, Type rhs,
                        ConstraintLocator *locator)
      : ContextualFailure(root, cs, lhs, rhs, locator) {}

  bool diagnoseAsError() override;

protected:
  /// Compute location of the failure for diagnostic.
  SourceLoc getLoc() const;
};

/// Diagnose mismatches relating to tuple destructuring.
class TupleContextualFailure final : public ContextualFailure {
public:
  TupleContextualFailure(Expr *root, ConstraintSystem &cs, Type lhs, Type rhs,
                         ConstraintLocator *locator)
      : ContextualFailure(root, cs, lhs, rhs, locator) {}

  bool diagnoseAsError() override;

  bool isNumElementsMismatch() const {
    auto lhsTy = getFromType()->castTo<TupleType>();
    auto rhsTy = getToType()->castTo<TupleType>();
    assert(lhsTy && rhsTy);
    return lhsTy->getNumElements() != rhsTy->getNumElements();
  }
};

/// Diagnose situations when @autoclosure argument is passed to @autoclosure
/// parameter directly without calling it first.
class AutoClosureForwardingFailure final : public FailureDiagnostic {
public:
  AutoClosureForwardingFailure(ConstraintSystem &cs, ConstraintLocator *locator)
      : FailureDiagnostic(nullptr, cs, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose invalid pointer conversions for an autoclosure result type.
///
/// \code
/// func foo(_ x: @autoclosure () -> UnsafePointer<Int>) {}
///
/// var i = 0
/// foo(&i) // Invalid conversion to UnsafePointer
/// \endcode
class AutoClosurePointerConversionFailure final : public ContextualFailure {
public:
  AutoClosurePointerConversionFailure(Expr *root, ConstraintSystem &cs,
                                      Type pointeeType, Type pointerType,
                                      ConstraintLocator *locator)
      : ContextualFailure(root, cs, pointeeType, pointerType, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose situations when there was an attempt to unwrap entity
/// of non-optional type e.g.
///
/// ```swift
/// let i: Int = 0
/// _ = i!
///
/// struct A { func foo() {} }
/// func foo(_ a: A) {
///   a?.foo()
/// }
/// ```
class NonOptionalUnwrapFailure final : public FailureDiagnostic {
  Type BaseType;

public:
  NonOptionalUnwrapFailure(Expr *root, ConstraintSystem &cs, Type baseType,
                           ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), BaseType(baseType) {}

  bool diagnoseAsError() override;
};

class MissingCallFailure final : public FailureDiagnostic {
public:
  MissingCallFailure(Expr *root, ConstraintSystem &cs,
                     ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator) {}

  bool diagnoseAsError() override;
};

class PropertyWrapperReferenceFailure : public ContextualFailure {
  VarDecl *Property;
  bool UsingStorageWrapper;

public:
  PropertyWrapperReferenceFailure(Expr *root, ConstraintSystem &cs,
                                  VarDecl *property, bool usingStorageWrapper,
                                  Type base, Type wrapper,
                                  ConstraintLocator *locator)
      : ContextualFailure(root, cs, base, wrapper, locator), Property(property),
        UsingStorageWrapper(usingStorageWrapper) {}

  VarDecl *getProperty() const { return Property; }

  Identifier getPropertyName() const { return Property->getName(); }

  bool usingStorageWrapper() const { return UsingStorageWrapper; }

  ValueDecl *getReferencedMember() const {
    auto *locator = getLocator();
    if (auto overload = getOverloadChoiceIfAvailable(locator))
      return overload->choice.getDeclOrNull();
    return nullptr;
  }
};

class ExtraneousPropertyWrapperUnwrapFailure final
    : public PropertyWrapperReferenceFailure {
public:
  ExtraneousPropertyWrapperUnwrapFailure(Expr *root, ConstraintSystem &cs,
                                         VarDecl *property,
                                         bool usingStorageWrapper, Type base,
                                         Type wrapper,
                                         ConstraintLocator *locator)
      : PropertyWrapperReferenceFailure(root, cs, property, usingStorageWrapper,
                                        base, wrapper, locator) {}

  bool diagnoseAsError() override;
};

class MissingPropertyWrapperUnwrapFailure final
    : public PropertyWrapperReferenceFailure {
public:
  MissingPropertyWrapperUnwrapFailure(Expr *root, ConstraintSystem &cs,
                                      VarDecl *property,
                                      bool usingStorageWrapper, Type base,
                                      Type wrapper, ConstraintLocator *locator)
      : PropertyWrapperReferenceFailure(root, cs, property, usingStorageWrapper,
                                        base, wrapper, locator) {}

  bool diagnoseAsError() override;
};

class SubscriptMisuseFailure final : public FailureDiagnostic {
public:
  SubscriptMisuseFailure(Expr *root, ConstraintSystem &cs,
                         ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator) {}

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;
};

class InvalidMemberRefFailure : public FailureDiagnostic {
  Type BaseType;
  DeclName Name;

public:
  InvalidMemberRefFailure(Expr *root, ConstraintSystem &cs, Type baseType,
                          DeclName memberName, ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), BaseType(baseType->getRValueType()),
        Name(memberName) {}

protected:
  Type getBaseType() const { return BaseType; }
  DeclName getName() const { return Name; }
};

/// Diagnose situations when member referenced by name is missing
/// from the associated base type, e.g.
///
/// ```swift
/// struct S {}
/// func foo(_ s: S) {
///   let _: Int = s.foo(1, 2) // expected type is `(Int, Int) -> Int`
/// }
/// ```
class MissingMemberFailure final : public InvalidMemberRefFailure {
public:
  MissingMemberFailure(Expr *root, ConstraintSystem &cs, Type baseType,
                       DeclName memberName, ConstraintLocator *locator)
      : InvalidMemberRefFailure(root, cs, baseType, memberName, locator) {}

  bool diagnoseAsError() override;

private:
  static DeclName findCorrectEnumCaseName(Type Ty,
                                          TypoCorrectionResults &corrections,
                                          DeclName memberName);
};

/// Diagnose cases where a member only accessible on generic constraints
/// requiring conformance to a protocol is used on a value of the
/// existential protocol type e.g.
///
/// ```swift
/// protocol P {
///   var foo: Self { get }
/// }
///
/// func bar<X : P>(p: X) {
///   p.foo
/// }
/// ```
class InvalidMemberRefOnExistential final : public InvalidMemberRefFailure {
public:
  InvalidMemberRefOnExistential(Expr *root, ConstraintSystem &cs, Type baseType,
                                DeclName memberName, ConstraintLocator *locator)
      : InvalidMemberRefFailure(root, cs, baseType, memberName, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose situations when we use an instance member on a type
/// or a type member on an instance
///
/// ```swift
/// class Bar {}
///
/// enum Foo {
///
///   static func f() {
///     g(Bar())
///   }
///
///   func g(_: Bar) {}
///
/// }
/// ```
class AllowTypeOrInstanceMemberFailure final : public FailureDiagnostic {
  Type BaseType;
  ValueDecl *Member;
  DeclName Name;

public:
  AllowTypeOrInstanceMemberFailure(Expr *root, ConstraintSystem &cs,
                                   Type baseType, ValueDecl *member,
                                   DeclName name, ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator),
        BaseType(baseType->getRValueType()), Member(member), Name(name) {
    assert(member);
  }

  bool diagnoseAsError() override;
};

class PartialApplicationFailure final : public FailureDiagnostic {
  enum RefKind : unsigned {
    MutatingMethod,
    SuperInit,
    SelfInit,
  };

  bool CompatibilityWarning;

public:
  PartialApplicationFailure(Expr *root, bool warning, ConstraintSystem &cs,
                            ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), CompatibilityWarning(warning) {}

  bool diagnoseAsError() override;
};

class InvalidInitRefFailure : public FailureDiagnostic {
protected:
  Type BaseType;
  const ConstructorDecl *Init;
  SourceRange BaseRange;

  InvalidInitRefFailure(Expr *root, ConstraintSystem &cs, Type baseTy,
                        const ConstructorDecl *init, SourceRange baseRange,
                        ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), BaseType(baseTy), Init(init),
        BaseRange(baseRange) {}

public:
  bool diagnoseAsError() override = 0;
};

/// Diagnose an attempt to construct an object of class type with a metatype
/// value without using 'required' initializer:
///
/// ```swift
///  class C {
///    init(value: Int) {}
///  }
///
///  func make<T: C>(type: T.Type) -> T {
///    return T.init(value: 42)
///  }
/// ```
class InvalidDynamicInitOnMetatypeFailure final : public InvalidInitRefFailure {
public:
  InvalidDynamicInitOnMetatypeFailure(Expr *root, ConstraintSystem &cs,
                                      Type baseTy, const ConstructorDecl *init,
                                      SourceRange baseRange,
                                      ConstraintLocator *locator)
      : InvalidInitRefFailure(root, cs, baseTy, init, baseRange, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose an attempt to call initializer on protocol metatype:
///
/// ```swift
///  protocol P {
///    init(value: Int)
///  }
///
///  func make(type: P.Type) -> P {
///    return type.init(value: 42)
///  }
/// ```
class InitOnProtocolMetatypeFailure final : public InvalidInitRefFailure {
  bool IsStaticallyDerived;

public:
  InitOnProtocolMetatypeFailure(Expr *root, ConstraintSystem &cs, Type baseTy,
                                const ConstructorDecl *init,
                                bool isStaticallyDerived, SourceRange baseRange,
                                ConstraintLocator *locator)
      : InvalidInitRefFailure(root, cs, baseTy, init, baseRange, locator),
        IsStaticallyDerived(isStaticallyDerived) {}

  bool diagnoseAsError() override;
};

/// Diagnose an attempt to construct an instance using non-constant
/// metatype base without explictly specifying `init`:
///
/// ```swift
/// let foo = Int.self
/// foo(0) // should be `foo.init(0)`
/// ```
class ImplicitInitOnNonConstMetatypeFailure final
    : public InvalidInitRefFailure {
public:
  ImplicitInitOnNonConstMetatypeFailure(Expr *root, ConstraintSystem &cs,
                                        Type baseTy,
                                        const ConstructorDecl *init,
                                        ConstraintLocator *locator)
      : InvalidInitRefFailure(root, cs, baseTy, init, SourceRange(), locator) {}

  bool diagnoseAsError() override;
};

class MissingArgumentsFailure final : public FailureDiagnostic {
  using Param = AnyFunctionType::Param;

  FunctionType *Fn;
  unsigned NumSynthesized;

public:
  MissingArgumentsFailure(Expr *root, ConstraintSystem &cs,
                          FunctionType *funcType,
                          unsigned numSynthesized,
                          ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), Fn(funcType),
        NumSynthesized(numSynthesized) {}

  bool diagnoseAsError() override;

private:
  /// If missing arguments come from trailing closure,
  /// let's produce tailored diagnostics.
  bool diagnoseTrailingClosure(ClosureExpr *closure);
};

class OutOfOrderArgumentFailure final : public FailureDiagnostic {
  using ParamBinding = SmallVector<unsigned, 1>;

  unsigned ArgIdx;
  unsigned PrevArgIdx;

  SmallVector<ParamBinding, 4> Bindings;

public:
  OutOfOrderArgumentFailure(Expr *root, ConstraintSystem &cs,
                            unsigned argIdx,
                            unsigned prevArgIdx,
                            ArrayRef<ParamBinding> bindings,
                            ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), ArgIdx(argIdx),
        PrevArgIdx(prevArgIdx), Bindings(bindings.begin(), bindings.end()) {}

  bool diagnoseAsError() override;
};

/// Diagnose an attempt to destructure a single tuple closure parameter
/// into a multiple (possibly anonymous) arguments e.g.
///
/// ```swift
/// let _: ((Int, Int)) -> Void = { $0 + $1 }
/// ```
class ClosureParamDestructuringFailure final : public FailureDiagnostic {
  FunctionType *ContextualType;

public:
  ClosureParamDestructuringFailure(Expr *root, ConstraintSystem &cs,
                                   FunctionType *contextualType,
                                   ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), ContextualType(contextualType) {}

  bool diagnoseAsError() override;

private:
  Type getParameterType() const {
    const auto &param = ContextualType->getParams().front();
    return resolveType(param.getPlainType());
  }
};

/// Diagnose an attempt to reference inaccessible member e.g.
///
/// ```swift
/// struct S {
///   var foo: String
///
///   private init(_ v: String) {
///     self.foo = v
///   }
/// }
/// _ = S("ultimate question")
/// ```
class InaccessibleMemberFailure final : public FailureDiagnostic {
  ValueDecl *Member;

public:
  InaccessibleMemberFailure(Expr *root, ConstraintSystem &cs, ValueDecl *member,
                            ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), Member(member) {}

  bool diagnoseAsError() override;
};

/// Diagnose an attempt to reference member marked as `mutating`
/// on immutable base e.g. `let` variable:
///
/// ```swift
/// struct S {
///   mutating func foo(_ i: Int) {}
///   func foo(_ f: Float) {}
/// }
///
/// func bar(_ s: S, _ answer: Int) {
///  s.foo(answer)
/// }
/// ```
class MutatingMemberRefOnImmutableBase final : public FailureDiagnostic {
  ValueDecl *Member;

public:
  MutatingMemberRefOnImmutableBase(Expr *root, ConstraintSystem &cs,
                                   ValueDecl *member,
                                   ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), Member(member) {}

  bool diagnoseAsError() override;
};

// Diagnose an attempt to use AnyObject as the root type of a KeyPath
//
// ```swift
// let keyPath = \AnyObject.bar
// ```
class AnyObjectKeyPathRootFailure final : public FailureDiagnostic {

public:
  AnyObjectKeyPathRootFailure(Expr *root, ConstraintSystem &cs,
                              ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator) {}
  
  bool diagnoseAsError() override;
};

/// Diagnose an attempt to reference subscript as a keypath component
/// where at least one of the index arguments doesn't conform to Hashable e.g.
///
/// ```swift
/// protocol P {}
///
/// struct S {
///   subscript<T: P>(x: Int, _ y: T) -> Bool { return true }
/// }
///
/// func foo<T: P>(_ x: Int, _ y: T) {
///   _ = \S.[x, y]
/// }
/// ```
class KeyPathSubscriptIndexHashableFailure final : public FailureDiagnostic {
  Type NonConformingType;

public:
  KeyPathSubscriptIndexHashableFailure(Expr *root, ConstraintSystem &cs,
                                       Type type, ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), NonConformingType(type) {
    assert(locator->isResultOfKeyPathDynamicMemberLookup() ||
           locator->isKeyPathSubscriptComponent());
  }

  bool diagnoseAsError() override;
};

class InvalidMemberRefInKeyPath : public FailureDiagnostic {
  ValueDecl *Member;

public:
  InvalidMemberRefInKeyPath(Expr *root, ConstraintSystem &cs, ValueDecl *member,
                            ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), Member(member) {
    assert(member->hasName());
    assert(locator->isForKeyPathComponent() ||
           locator->isForKeyPathDynamicMemberLookup());
  }

  DescriptiveDeclKind getKind() const { return Member->getDescriptiveKind(); }

  DeclName getName() const { return Member->getFullName(); }

  bool diagnoseAsError() override = 0;

protected:
  /// Compute location of the failure for diagnostic.
  SourceLoc getLoc() const;

  bool isForKeyPathDynamicMemberLookup() const {
    return getLocator()->isForKeyPathDynamicMemberLookup();
  }
};

/// Diagnose an attempt to reference a static member as a key path component
/// e.g.
///
/// ```swift
/// struct S {
///   static var foo: Int = 42
/// }
///
/// _ = \S.Type.foo
/// ```
class InvalidStaticMemberRefInKeyPath final : public InvalidMemberRefInKeyPath {
public:
  InvalidStaticMemberRefInKeyPath(Expr *root, ConstraintSystem &cs,
                                  ValueDecl *member, ConstraintLocator *locator)
      : InvalidMemberRefInKeyPath(root, cs, member, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose an attempt to reference a member which has a mutating getter as a
/// key path component e.g.
///
/// ```swift
/// struct S {
///   var foo: Int {
///     mutating get { return 42 }
///   }
///
///   subscript(_: Int) -> Bool {
///     mutating get { return false }
///   }
/// }
///
/// _ = \S.foo
/// _ = \S.[42]
/// ```
class InvalidMemberWithMutatingGetterInKeyPath final
    : public InvalidMemberRefInKeyPath {
public:
  InvalidMemberWithMutatingGetterInKeyPath(Expr *root, ConstraintSystem &cs,
                                           ValueDecl *member,
                                           ConstraintLocator *locator)
      : InvalidMemberRefInKeyPath(root, cs, member, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose an attempt to reference a method as a key path component
/// e.g.
///
/// ```swift
/// struct S {
///   func foo() -> Int { return 42 }
///   static func bar() -> Int { return 0 }
/// }
///
/// _ = \S.foo
/// _ = \S.Type.bar
/// ```
class InvalidMethodRefInKeyPath final : public InvalidMemberRefInKeyPath {
public:
  InvalidMethodRefInKeyPath(Expr *root, ConstraintSystem &cs, ValueDecl *method,
                            ConstraintLocator *locator)
      : InvalidMemberRefInKeyPath(root, cs, method, locator) {
    assert(isa<FuncDecl>(method));
  }

  bool diagnoseAsError() override;
};

/// Diagnose an attempt return something from a function which
/// doesn't have a return type specified e.g.
///
/// ```swift
/// func foo() { return 42 }
/// ```
class ExtraneousReturnFailure final : public FailureDiagnostic {
public:
  ExtraneousReturnFailure(Expr *root, ConstraintSystem &cs,
                          ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose a contextual mismatch between expected collection element type
/// and the one provided (e.g. source of the assignment or argument to a call)
/// e.g.:
///
/// ```swift
/// let _: [Int] = ["hello"]
/// ```
class CollectionElementContextualFailure final : public ContextualFailure {
public:
  CollectionElementContextualFailure(Expr *root, ConstraintSystem &cs,
                                     Type eltType, Type contextualType,
                                     ConstraintLocator *locator)
      : ContextualFailure(root, cs, eltType, contextualType, locator) {}

  bool diagnoseAsError() override;
};

class MissingContextualConformanceFailure final : public ContextualFailure {
  ContextualTypePurpose Context;

public:
  MissingContextualConformanceFailure(Expr *root, ConstraintSystem &cs,
                                      ContextualTypePurpose context, Type type,
                                      Type protocolType,
                                      ConstraintLocator *locator)
      : ContextualFailure(root, cs, type, protocolType, locator),
        Context(context) {
    assert(protocolType->is<ProtocolType>() ||
           protocolType->is<ProtocolCompositionType>());
  }

  bool diagnoseAsError() override;
};

/// Diagnose a conversion mismatch between object types of `inout`
/// argument/parameter e.g. `'inout S' argument conv 'inout P'`.
///
/// Even if `S` conforms to `P` there is no subtyping rule for
/// argument type of `inout` parameter, they have to be equal.
class InOutConversionFailure final : public ContextualFailure {
public:
  InOutConversionFailure(Expr *root, ConstraintSystem &cs, Type argType,
                         Type paramType, ConstraintLocator *locator)
      : ContextualFailure(root, cs, argType, paramType, locator) {}

  bool diagnoseAsError() override;

protected:
  /// Suggest to change a type of the argument if possible.
  void fixItChangeArgumentType() const;
};

/// Diagnose generic argument omission e.g.
///
/// ```swift
/// struct S<T> {}
///
/// _ = S()
/// ```
class MissingGenericArgumentsFailure final : public FailureDiagnostic {
  using Anchor = llvm::PointerUnion<TypeRepr *, Expr *>;

  SmallVector<GenericTypeParamType *, 4> Parameters;

public:
  MissingGenericArgumentsFailure(Expr *root, ConstraintSystem &cs,
                                 ArrayRef<GenericTypeParamType *> missingParams,
                                 ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator) {
    assert(!missingParams.empty());
    Parameters.append(missingParams.begin(), missingParams.end());
  }

  bool hasLoc(GenericTypeParamType *GP) const;

  DeclContext *getDeclContext() const {
    auto *GP = Parameters.front();
    auto *decl = GP->getDecl();

    return decl ? decl->getDeclContext() : nullptr;
  }

  bool diagnoseAsError() override;

  bool diagnoseForAnchor(Anchor anchor,
                         ArrayRef<GenericTypeParamType *> params) const;

  bool diagnoseParameter(Anchor anchor, GenericTypeParamType *GP) const;

private:
  void emitGenericSignatureNote(Anchor anchor) const;

  /// Retrieve representative locations for associated generic prameters.
  ///
  /// \returns true if all of the parameters have been covered.
  bool findArgumentLocations(
      llvm::function_ref<void(TypeRepr *, GenericTypeParamType *)> callback);
};

class SkipUnhandledConstructInFunctionBuilderFailure final
    : public FailureDiagnostic {
public:
  using UnhandledNode = llvm::PointerUnion<Stmt *, Decl *>;

  UnhandledNode unhandled;
  NominalTypeDecl *builder;

  void diagnosePrimary(bool asNote);

public:
  SkipUnhandledConstructInFunctionBuilderFailure(Expr *root,
                                                 ConstraintSystem &cs,
                                                 UnhandledNode unhandled,
                                                 NominalTypeDecl *builder,
                                                 ConstraintLocator *locator)
    : FailureDiagnostic(root, cs, locator),
      unhandled(unhandled),
      builder(builder) { }

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;
};

/// Diagnose situation when a single "tuple" parameter is given N arguments e.g.
///
/// ```swift
/// func foo<T>(_ x: (T, Bool)) {}
/// foo(1, false) // foo exptects a single argument of tuple type `(1, false)`
/// ```
class InvalidTupleSplatWithSingleParameterFailure final
    : public FailureDiagnostic {
  Type ParamType;

public:
  InvalidTupleSplatWithSingleParameterFailure(Expr *root, ConstraintSystem &cs,
                                              Type paramTy,
                                              ConstraintLocator *locator)
      : FailureDiagnostic(root, cs, locator), ParamType(paramTy) {}

  bool diagnoseAsError() override;
};

/// Provides information about the application of a function argument to a
/// parameter.
class FunctionArgApplyInfo {
  Expr *ArgExpr;
  unsigned ArgIdx;
  Type ArgType;

  unsigned ParamIdx;

  Type FnInterfaceType;
  FunctionType *FnType;
  const ValueDecl *Callee;

public:
  FunctionArgApplyInfo(Expr *argExpr, unsigned argIdx, Type argType,
                       unsigned paramIdx, Type fnInterfaceType,
                       FunctionType *fnType, const ValueDecl *callee)
      : ArgExpr(argExpr), ArgIdx(argIdx), ArgType(argType), ParamIdx(paramIdx),
        FnInterfaceType(fnInterfaceType), FnType(fnType), Callee(callee) {}

  /// \returns The argument being applied.
  Expr *getArgExpr() const { return ArgExpr; }

  /// \returns The position of the argument, starting at 1.
  unsigned getArgPosition() const { return ArgIdx + 1; }

  /// \returns The position of the parameter, starting at 1.
  unsigned getParamPosition() const { return ParamIdx + 1; }

  /// \returns The type of the argument being applied, including any generic
  /// substitutions.
  ///
  /// \param withSpecifier Whether to keep the inout or @lvalue specifier of
  /// the argument, if any.
  Type getArgType(bool withSpecifier = false) const {
    return withSpecifier ? ArgType : ArgType->getWithoutSpecifierType();
  }

  /// \returns The interface type for the function being applied. Note that this
  /// may not a function type, for example it could be a generic parameter.
  Type getFnInterfaceType() const { return FnInterfaceType; }

  /// \returns The function type being applied, including any generic
  /// substitutions.
  FunctionType *getFnType() const { return FnType; }

  /// \returns The callee for the application.
  const ValueDecl *getCallee() const { return Callee; }

private:
  Type getParamTypeImpl(AnyFunctionType *fnTy,
                        bool lookThroughAutoclosure) const {
    auto param = fnTy->getParams()[ParamIdx];
    auto paramTy = param.getPlainType();
    if (lookThroughAutoclosure && param.isAutoClosure())
      paramTy = paramTy->castTo<FunctionType>()->getResult();
    return paramTy;
  }

public:
  /// \returns The type of the parameter which the argument is being applied to,
  /// including any generic substitutions.
  ///
  /// \param lookThroughAutoclosure Whether an @autoclosure () -> T parameter
  /// should be treated as being of type T.
  Type getParamType(bool lookThroughAutoclosure = true) const {
    return getParamTypeImpl(FnType, lookThroughAutoclosure);
  }

  /// \returns The interface type of the parameter which the argument is being
  /// applied to.
  ///
  /// \param lookThroughAutoclosure Whether an @autoclosure () -> T parameter
  /// should be treated as being of type T.
  Type getParamInterfaceType(bool lookThroughAutoclosure = true) const {
    auto interfaceFnTy = FnInterfaceType->getAs<AnyFunctionType>();
    if (!interfaceFnTy) {
      // If the interface type isn't a function, then just return the resolved
      // parameter type.
      return getParamType(lookThroughAutoclosure)->mapTypeOutOfContext();
    }
    return getParamTypeImpl(interfaceFnTy, lookThroughAutoclosure);
  }

  /// \returns The flags of the parameter which the argument is being applied
  /// to.
  ParameterTypeFlags getParameterFlags() const {
    return FnType->getParams()[ParamIdx].getParameterFlags();
  }
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSDIAGNOSTICS_H
