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
  const Solution &S;
  ConstraintLocator *Locator;

public:
  FailureDiagnostic(const Solution &solution, ConstraintLocator *locator)
      : S(solution), Locator(locator) {}

  FailureDiagnostic(const Solution &solution, const Expr *anchor)
      : FailureDiagnostic(solution, solution.getConstraintLocator(anchor)) {}

  virtual ~FailureDiagnostic();

  virtual SourceLoc getLoc() const { return getLoc(getAnchor()); }

  virtual SourceRange getSourceRange() const {
    return getSourceRange(getAnchor());
  }

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

  TypedNode getRawAnchor() const { return Locator->getAnchor(); }

  virtual TypedNode getAnchor() const;

  ConstraintLocator *getLocator() const { return Locator; }

  Type getType(TypedNode node, bool wantRValue = true) const;

  /// Resolve type variables present in the raw type, if any.
  Type resolveType(Type rawType, bool reconstituteSugar = false,
                   bool wantRValue = true) const {
    if (!rawType->hasTypeVariable()) {
      if (reconstituteSugar)
        rawType = rawType->reconstituteSugar(/*recursive*/ true);
      return wantRValue ? rawType->getRValueType() : rawType;
    }

    auto &cs = getConstraintSystem();
    return cs.simplifyTypeImpl(rawType, [&](TypeVariableType *typeVar) -> Type {
      auto fixed = S.simplifyType(typeVar);
      auto *genericParam = typeVar->getImpl().getGenericParameter();
      if (fixed->isHole() && genericParam)
        return genericParam;
      return resolveType(fixed, reconstituteSugar, wantRValue);
    });
  }

  template <typename... ArgTypes>
  InFlightDiagnostic emitDiagnostic(ArgTypes &&... Args) const;

  template <typename... ArgTypes>
  InFlightDiagnostic emitDiagnosticAt(ArgTypes &&... Args) const;

protected:
  const Solution &getSolution() const { return S; }

  ConstraintSystem &getConstraintSystem() const {
    return S.getConstraintSystem();
  }

  Type getContextualType(TypedNode anchor) const {
    auto &cs = getConstraintSystem();
    return cs.getContextualType(anchor.get<const Expr *>());
  }

  TypeLoc getContextualTypeLoc(TypedNode anchor) const {
    auto &cs = getConstraintSystem();
    return cs.getContextualTypeLoc(anchor.get<const Expr *>());
  }

  ContextualTypePurpose getContextualTypePurpose(TypedNode anchor) const {
    auto &cs = getConstraintSystem();
    return cs.getContextualTypePurpose(anchor.get<const Expr *>());
  }

  DeclContext *getDC() const {
    auto &cs = getConstraintSystem();
    return cs.DC;
  }

  ASTContext &getASTContext() const {
    auto &cs = getConstraintSystem();
    return cs.getASTContext();
  }

  /// Retrieve overload choice resolved for a given locator
  /// by the constraint solver.
  Optional<SelectedOverload>
  getOverloadChoiceIfAvailable(ConstraintLocator *locator) const {
    return S.getOverloadChoiceIfAvailable(locator);
  }

  /// Retrieve overload choice resolved for a callee for the anchor
  /// of a given locator.
  Optional<SelectedOverload>
  getCalleeOverloadChoiceIfAvailable(ConstraintLocator *locator) const {
    return getOverloadChoiceIfAvailable(S.getCalleeLocator(locator));
  }

  ConstraintLocator *
  getConstraintLocator(TypedNode anchor,
                       ConstraintLocator::PathElement element) const {
    return S.getConstraintLocator(anchor.get<const Expr *>(), {element});
  }

  /// Retrive the constraint locator for the given anchor and
  /// path, uniqued and automatically calculate the summary flags
  ConstraintLocator *getConstraintLocator(
      TypedNode anchor,
      ArrayRef<ConstraintLocator::PathElement> path = {}) const {
    return S.getConstraintLocator(anchor.get<const Expr *>(), path);
  }

  ConstraintLocator *
  getConstraintLocator(ConstraintLocator *baseLocator,
                       ConstraintLocator::PathElement element) const {
    return S.getConstraintLocator(baseLocator, element);
  }

  Optional<FunctionArgApplyInfo>
  getFunctionArgApplyInfo(ConstraintLocator *locator) const {
    return S.getFunctionArgApplyInfo(locator);
  }

  /// \returns A parent expression if sub-expression is contained anywhere
  /// in the root expression or `nullptr` otherwise.
  Expr *findParentExpr(const Expr *subExpr) const;

  /// If given expression is some kind of a member reference e.g.
  /// `x.foo` or `x[0]` extract and return its base expression.
  Expr *getBaseExprFor(const Expr *anchor) const;

  /// For a given locator describing an argument application, or a constraint
  /// within an argument application, returns the argument list for that
  /// application. If the locator is not for an argument application, or
  /// the argument list cannot be found, returns \c nullptr.
  Expr *getArgumentListExprFor(ConstraintLocator *locator) const;

  /// \returns A new type with all of the type variables associated with
  /// generic parameters substituted back into being generic parameter type.
  Type restoreGenericParameters(
      Type type,
      llvm::function_ref<void(GenericTypeParamType *, Type)> substitution =
          [](GenericTypeParamType *, Type) {});

  static SourceLoc getLoc(TypedNode node);
  static SourceRange getSourceRange(TypedNode node);

  template <typename T> static const T *castToExpr(TypedNode node) {
    return cast<T>(node.get<const Expr *>());
  }

  template <typename T> static T *getAsExpr(TypedNode node) {
    if (const auto *E = node.dyn_cast<const Expr *>())
      return dyn_cast<T>(const_cast<Expr *>(E));
    return nullptr;
  }

  template <typename T> static bool isExpr(TypedNode node) {
    auto *E = node.get<const Expr *>();
    return isa<T>(E);
  }
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
  GenericSignature Signature;

  const ValueDecl *AffectedDecl;
  /// If possible, find application expression associated
  /// with current generic requirement failure, that helps
  /// to diagnose failures related to arguments.
  const ApplyExpr *Apply = nullptr;

  /// Types associated with requirement constraint this
  /// failure originates from.
  Type LHS, RHS;

public:
  RequirementFailure(const Solution &solution, Type lhs, Type rhs,
                     ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator),
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

    if (auto *parentExpr = findParentExpr(getRawAnchor().get<const Expr *>()))
      Apply = dyn_cast<ApplyExpr>(parentExpr);
  }

  SourceLoc getLoc() const override;

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

  static bool isOperator(const ApplyExpr *apply) {
    return isa<PrefixUnaryExpr>(apply) || isa<PostfixUnaryExpr>(apply) ||
           isa<BinaryExpr>(apply);
  }

  /// Determine whether given declaration represents a static
  /// or instance property/method, excluding operators.
  static bool isStaticOrInstanceMember(const ValueDecl *decl);

private:
  /// Retrieve declaration associated with failing generic requirement.
  ValueDecl *getDeclRef() const;

  /// Retrieve generic signature where this parameter originates from.
  GenericSignature getSignature(ConstraintLocator *locator);

  void emitRequirementNote(const Decl *anchor, Type lhs, Type rhs) const;

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
  MissingConformanceFailure(const Solution &solution,
                            ConstraintLocator *locator,
                            std::pair<Type, Type> conformance)
      : RequirementFailure(solution, conformance.first, conformance.second,
                           locator) {
    auto reqElt = locator->castLastElementTo<LocatorPathElt::AnyRequirement>();
    assert(reqElt.getRequirementKind() == RequirementKind::Conformance ||
           reqElt.getRequirementKind() == RequirementKind::Layout);
  }

  bool diagnoseAsError() override;

protected:
  /// Check whether this requirement is associated with one of the
  /// operator overloads, in cases like that sometimes it makes more
  /// sense to produce a generic diagnostic about operator reference
  /// instead of conformance, because it could be something like
  /// `true + true`, and it doesn't make much sense to suggest to
  /// add a conformance from one library type to another.
  bool diagnoseAsAmbiguousOperatorRef();

  DiagOnDecl getDiagnosticOnDecl() const override {
    return (getRequirement().getKind() == RequirementKind::Layout ?
            diag::type_does_not_conform_anyobject_decl_owner :
            diag::type_does_not_conform_decl_owner);
  }

  DiagInReference getDiagnosticInRereference() const override {
    return (getRequirement().getKind() == RequirementKind::Layout ?
            diag::type_does_not_conform_anyobject_in_decl_ref :
            diag::type_does_not_conform_in_decl_ref);
  }

  DiagAsNote getDiagnosticAsNote() const override {
    return diag::candidate_types_conformance_requirement;
  }

private:
  bool diagnoseTypeCannotConform(Type nonConformingType,
                                 Type protocolType) const;
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
  SameTypeRequirementFailure(const Solution &solution, Type lhs, Type rhs,
                             ConstraintLocator *locator)
      : RequirementFailure(solution, lhs, rhs, locator) {
    auto reqElt = locator->castLastElementTo<LocatorPathElt::AnyRequirement>();
    assert(reqElt.getRequirementKind() == RequirementKind::SameType);
  }

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
  SuperclassRequirementFailure(const Solution &solution, Type lhs, Type rhs,
                               ConstraintLocator *locator)
      : RequirementFailure(solution, lhs, rhs, locator) {
    auto reqElt = locator->castLastElementTo<LocatorPathElt::AnyRequirement>();
    assert(reqElt.getRequirementKind() == RequirementKind::Superclass);
  }

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
  LabelingFailure(const Solution &solution, ConstraintLocator *locator,
                  ArrayRef<Identifier> labels)
      : FailureDiagnostic(solution, locator), CorrectLabels(labels) {}

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;
};

/// Diagnose failures related to attempting member access on optional base
/// type without optional chaining or force-unwrapping it first.
class MemberAccessOnOptionalBaseFailure final : public FailureDiagnostic {
  DeclNameRef Member;
  bool ResultTypeIsOptional;

public:
  MemberAccessOnOptionalBaseFailure(const Solution &solution,
                                    ConstraintLocator *locator,
                                    DeclNameRef memberName, bool resultOptional)
      : FailureDiagnostic(solution, locator), Member(memberName),
        ResultTypeIsOptional(resultOptional) {}

  bool diagnoseAsError() override;
};

/// Diagnose errors associated with rvalues in positions
/// where an lvalue is required, such as inout arguments.
class RValueTreatedAsLValueFailure final : public FailureDiagnostic {

public:
  RValueTreatedAsLValueFailure(const Solution &solution,
                               ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {}

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;
};

class TrailingClosureAmbiguityFailure final : public FailureDiagnostic {
  ArrayRef<OverloadChoice> Choices;

public:
  TrailingClosureAmbiguityFailure(ArrayRef<Solution> solutions, Expr *anchor,
                                  ArrayRef<OverloadChoice> choices)
      : FailureDiagnostic(solutions.front(), anchor), Choices(choices) {}

  bool diagnoseAsError() override { return false; }

  bool diagnoseAsNote() override;
};

/// Diagnose errors related to assignment expressions e.g.
/// trying to assign something to immutable value, or trying
/// to access mutating member on immutable base.
class AssignmentFailure final : public FailureDiagnostic {
  const Expr *DestExpr;
  SourceLoc Loc;
  Diag<StringRef> DeclDiagnostic;
  Diag<Type> TypeDiagnostic;

public:
  AssignmentFailure(const Expr *destExpr, const Solution &solution,
                    SourceLoc diagnosticLoc);

  AssignmentFailure(const Expr *destExpr, const Solution &solution,
                    SourceLoc diagnosticLoc, Diag<StringRef> declDiag,
                    Diag<Type> typeDiag)
      : FailureDiagnostic(solution, destExpr), DestExpr(destExpr),
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

  std::pair<Expr *, Optional<OverloadChoice>>
  resolveImmutableBase(const Expr *expr) const {
    return resolveImmutableBase(const_cast<Expr *>(expr));
  }

  static Diag<StringRef> findDeclDiagonstic(ASTContext &ctx,
                                            const Expr *destExpr);

  /// Retrive an member reference associated with given member
  /// looking through dynamic member lookup on the way.
  Optional<OverloadChoice> getMemberRef(ConstraintLocator *locator) const;
};

/// Intended to diagnose any possible contextual failure
/// e.g. argument/parameter, closure result, conversions etc.
class ContextualFailure : public FailureDiagnostic {
  ContextualTypePurpose CTP;
  Type RawFromType, RawToType;

public:
  ContextualFailure(const Solution &solution, Type lhs, Type rhs,
                    ConstraintLocator *locator)
      : ContextualFailure(
            solution,
            solution.getConstraintSystem().getContextualTypePurpose(
                locator->getAnchor()),
            lhs, rhs, locator) {}

  ContextualFailure(const Solution &solution, ContextualTypePurpose purpose,
                    Type lhs, Type rhs, ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), CTP(purpose), RawFromType(lhs),
        RawToType(rhs) {}

  Type getFromType() const { return resolve(RawFromType); }

  Type getToType() const { return resolve(RawToType); }

  Type getRawFromType() const { return RawFromType; }

  Type getRawToType() const { return RawToType; }

  bool diagnoseAsError() override;

  bool diagnoseAsNote() override;

  /// If we're trying to convert something to `nil`.
  bool diagnoseConversionToNil() const;
  
  /// Diagnose failed conversion in a `CoerceExpr`.
  bool diagnoseCoercionToUnrelatedType() const;

  /// If we're trying to convert something of type "() -> T" to T,
  /// then we probably meant to call the value.
  bool diagnoseMissingFunctionCall() const;

  /// Produce a specialized diagnostic if this is an invalid conversion to Bool.
  bool diagnoseConversionToBool() const;

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

private:
  Type resolve(Type rawType) const {
    return resolveType(rawType)->getWithoutSpecifierType();
  }

  /// Try to add a fix-it to convert a stored property into a computed
  /// property
  void tryComputedPropertyFixIts() const;

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
  getDiagnosticFor(ContextualTypePurpose context, Type contextualType);
};

/// Diagnose errors related to converting function type which
/// isn't explicitly '@escaping' to some other type.
class NoEscapeFuncToTypeConversionFailure final : public ContextualFailure {
public:
  NoEscapeFuncToTypeConversionFailure(const Solution &solution, Type fromType,
                                      Type toType, ConstraintLocator *locator)
      : ContextualFailure(solution, fromType, toType, locator) {}

  bool diagnoseAsError() override;

private:
  /// Emit tailored diagnostics for no-escape parameter conversions e.g.
  /// passing such parameter as an @escaping argument, or trying to
  /// assign it to a variable which expects @escaping function.
  bool diagnoseParameterUse() const;
};

/// Diagnose failures related to use of the unwrapped optional types,
/// which require some type of force-unwrap e.g. "!" or "try!".
class MissingOptionalUnwrapFailure final : public ContextualFailure {
public:
  MissingOptionalUnwrapFailure(const Solution &solution, Type fromType,
                               Type toType, ConstraintLocator *locator)
      : ContextualFailure(solution, fromType, toType, locator) {}

  bool diagnoseAsError() override;

private:
  Type getBaseType() const {
    return resolveType(getFromType(), /*reconstituteSugar=*/true);
  }

  Type getUnwrappedType() const {
    return resolveType(getBaseType()->getOptionalObjectType(),
                       /*reconstituteSugar=*/true);
  }

  /// Suggest a default value via `?? <default value>`
  void offerDefaultValueUnwrapFixIt(DeclContext *DC, const Expr *expr) const;
  /// Suggest a force optional unwrap via `!`
  void offerForceUnwrapFixIt(const Expr *expr) const;
};

/// Diagnostics for mismatched generic arguments e.g
/// ```swift
/// struct F<G> {}
/// let _:F<Int> = F<Bool>()
/// ```
class GenericArgumentsMismatchFailure final : public ContextualFailure {
  ArrayRef<unsigned> Mismatches;

public:
  GenericArgumentsMismatchFailure(const Solution &solution, Type actualType,
                                  Type requiredType,
                                  ArrayRef<unsigned> mismatches,
                                  ConstraintLocator *locator)
      : ContextualFailure(solution, actualType, requiredType, locator),
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
  ThrowingFunctionConversionFailure(const Solution &solution, Type fromType,
                                    Type toType, ConstraintLocator *locator)
      : ContextualFailure(solution, fromType, toType, locator) {
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
  MissingExplicitConversionFailure(const Solution &solution, Type fromType,
                                   Type toType, ConstraintLocator *locator)
      : ContextualFailure(solution, fromType, toType, locator) {}

  TypedNode getAnchor() const override;

  bool diagnoseAsError() override;

private:
  bool exprNeedsParensBeforeAddingAs(const Expr *expr) {
    auto *DC = getDC();
    auto asPG = TypeChecker::lookupPrecedenceGroup(
        DC, DC->getASTContext().Id_CastingPrecedence, SourceLoc());
    if (!asPG)
      return true;
    return exprNeedsParensInsideFollowingOperator(DC, const_cast<Expr *>(expr),
                                                  asPG);
  }

  bool exprNeedsParensAfterAddingAs(const Expr *expr, const Expr *rootExpr) {
    auto *DC = getDC();
    auto asPG = TypeChecker::lookupPrecedenceGroup(
        DC, DC->getASTContext().Id_CastingPrecedence, SourceLoc());
    if (!asPG)
      return true;

    return exprNeedsParensOutsideFollowingOperator(
        DC, const_cast<Expr *>(expr), const_cast<Expr *>(rootExpr), asPG);
  }
};

/// Diagnose failures related to passing value of some type
/// to `inout` or pointer parameter, without explicitly specifying `&`.
class MissingAddressOfFailure final : public ContextualFailure {
public:
  MissingAddressOfFailure(const Solution &solution, Type argTy, Type paramTy,
                          ConstraintLocator *locator)
      : ContextualFailure(solution, argTy, paramTy, locator) {}

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
  InvalidUseOfAddressOf(const Solution &solution, Type lhs, Type rhs,
                        ConstraintLocator *locator)
      : ContextualFailure(solution, lhs, rhs, locator) {}

  bool diagnoseAsError() override;

protected:
  /// Compute location of the failure for diagnostic.
  SourceLoc getLoc() const override;
};

/// Diagnose mismatches relating to tuple destructuring.
class TupleContextualFailure final : public ContextualFailure {
  /// Indices of the tuple elements whose types do not match.
  llvm::SmallVector<unsigned, 4> Indices;

public:
  TupleContextualFailure(const Solution &solution,
                         ContextualTypePurpose purpose, Type lhs, Type rhs,
                         llvm::ArrayRef<unsigned> indices,
                         ConstraintLocator *locator)
      : ContextualFailure(solution, purpose, lhs, rhs, locator),
        Indices(indices.begin(), indices.end()) {
    std::sort(Indices.begin(), Indices.end());
    assert(getFromType()->is<TupleType>() && getToType()->is<TupleType>());
  }

  bool diagnoseAsError() override;

  bool isNumElementsMismatch() const {
    auto lhsTy = getFromType()->castTo<TupleType>();
    auto rhsTy = getToType()->castTo<TupleType>();
    return lhsTy->getNumElements() != rhsTy->getNumElements();
  }
};

class FunctionTypeMismatch final : public ContextualFailure {
  /// Indices of the parameters whose types do not match.
  llvm::SmallVector<unsigned, 4> Indices;

public:
  FunctionTypeMismatch(const Solution &solution, ContextualTypePurpose purpose,
                       Type lhs, Type rhs, llvm::ArrayRef<unsigned> indices,
                       ConstraintLocator *locator)
      : ContextualFailure(solution, purpose, lhs, rhs, locator),
        Indices(indices.begin(), indices.end()) {
    std::sort(Indices.begin(), Indices.end());
    assert(getFromType()->is<AnyFunctionType>() && getToType()->is<AnyFunctionType>());
  }

  bool diagnoseAsError() override;
};

/// Diagnose situations when @autoclosure argument is passed to @autoclosure
/// parameter directly without calling it first.
class AutoClosureForwardingFailure final : public FailureDiagnostic {
public:
  AutoClosureForwardingFailure(const Solution &solution,
                               ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {}

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
  AutoClosurePointerConversionFailure(const Solution &solution,
                                      Type pointeeType, Type pointerType,
                                      ConstraintLocator *locator)
      : ContextualFailure(solution, pointeeType, pointerType, locator) {}

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
  NonOptionalUnwrapFailure(const Solution &solution, Type baseType,
                           ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), BaseType(baseType) {}

  bool diagnoseAsError() override;
};

class MissingCallFailure final : public FailureDiagnostic {
public:
  MissingCallFailure(const Solution &solution, ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {}

  TypedNode getAnchor() const override;

  bool diagnoseAsError() override;
};

class PropertyWrapperReferenceFailure : public ContextualFailure {
  VarDecl *Property;
  bool UsingStorageWrapper;

public:
  PropertyWrapperReferenceFailure(const Solution &solution, VarDecl *property,
                                  bool usingStorageWrapper, Type base,
                                  Type wrapper, ConstraintLocator *locator)
      : ContextualFailure(solution, base, wrapper, locator), Property(property),
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
  ExtraneousPropertyWrapperUnwrapFailure(const Solution &solution,
                                         VarDecl *property,
                                         bool usingStorageWrapper, Type base,
                                         Type wrapper,
                                         ConstraintLocator *locator)
      : PropertyWrapperReferenceFailure(solution, property, usingStorageWrapper,
                                        base, wrapper, locator) {}

  bool diagnoseAsError() override;
};

class MissingPropertyWrapperUnwrapFailure final
    : public PropertyWrapperReferenceFailure {
public:
  MissingPropertyWrapperUnwrapFailure(const Solution &solution,
                                      VarDecl *property,
                                      bool usingStorageWrapper, Type base,
                                      Type wrapper, ConstraintLocator *locator)
      : PropertyWrapperReferenceFailure(solution, property, usingStorageWrapper,
                                        base, wrapper, locator) {}

  bool diagnoseAsError() override;
};

class SubscriptMisuseFailure final : public FailureDiagnostic {
public:
  SubscriptMisuseFailure(const Solution &solution, ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {}

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;
};

class InvalidMemberRefFailure : public FailureDiagnostic {
  Type BaseType;
  DeclNameRef Name;

public:
  InvalidMemberRefFailure(const Solution &solution, Type baseType,
                          DeclNameRef memberName, ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator),
        BaseType(baseType->getRValueType()), Name(memberName) {}

protected:
  Type getBaseType() const { return BaseType; }
  DeclNameRef getName() const { return Name; }
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
  MissingMemberFailure(const Solution &solution, Type baseType,
                       DeclNameRef memberName, ConstraintLocator *locator)
      : InvalidMemberRefFailure(solution, baseType, memberName, locator) {}

  SourceLoc getLoc() const override {
    // Diagnostic should point to the member instead of its base expression.
    return FailureDiagnostic::getLoc(getRawAnchor());
  }

  bool diagnoseAsError() override;

private:
  /// Tailored diagnostics for missing special `@dynamicCallable` methods
  /// e.g. if caller expects `dynamicallyCall(withKeywordArguments:)`
  /// overload to be present, but a class marked as `@dynamicCallable`
  /// defines only `dynamicallyCall(withArguments:)` variant.
  bool diagnoseForDynamicCallable() const;
  
  /// Tailored diagnostics for collection literal with unresolved member expression
  /// that defaults the element type. e.g. _ = [.e]
  bool diagnoseInLiteralCollectionContext() const;

  static DeclName findCorrectEnumCaseName(Type Ty,
                                          TypoCorrectionResults &corrections,
                                          DeclNameRef memberName);
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
  InvalidMemberRefOnExistential(const Solution &solution, Type baseType,
                                DeclNameRef memberName,
                                ConstraintLocator *locator)
      : InvalidMemberRefFailure(solution, baseType, memberName, locator) {}

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
  DeclNameRef Name;

public:
  AllowTypeOrInstanceMemberFailure(const Solution &solution, Type baseType,
                                   ValueDecl *member, DeclNameRef name,
                                   ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator),
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
    SuperMethod,
  };

  bool CompatibilityWarning;

public:
  PartialApplicationFailure(bool warning, const Solution &solution,
                            ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), CompatibilityWarning(warning) {}

  bool diagnoseAsError() override;
};

class InvalidInitRefFailure : public FailureDiagnostic {
protected:
  Type BaseType;
  const ConstructorDecl *Init;
  SourceRange BaseRange;

  TypedNode getAnchor() const override { return getRawAnchor(); }

  InvalidInitRefFailure(const Solution &solution, Type baseTy,
                        const ConstructorDecl *init, SourceRange baseRange,
                        ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), BaseType(baseTy), Init(init),
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
  InvalidDynamicInitOnMetatypeFailure(const Solution &solution, Type baseTy,
                                      const ConstructorDecl *init,
                                      SourceRange baseRange,
                                      ConstraintLocator *locator)
      : InvalidInitRefFailure(solution, baseTy, init, baseRange, locator) {}

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
  InitOnProtocolMetatypeFailure(const Solution &solution, Type baseTy,
                                const ConstructorDecl *init,
                                bool isStaticallyDerived, SourceRange baseRange,
                                ConstraintLocator *locator)
      : InvalidInitRefFailure(solution, baseTy, init, baseRange, locator),
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
  ImplicitInitOnNonConstMetatypeFailure(const Solution &solution, Type baseTy,
                                        const ConstructorDecl *init,
                                        ConstraintLocator *locator)
      : InvalidInitRefFailure(solution, baseTy, init, SourceRange(), locator) {}

  SourceLoc getLoc() const override;

  bool diagnoseAsError() override;
};

class MissingArgumentsFailure final : public FailureDiagnostic {
  using SynthesizedParam = std::pair<unsigned, AnyFunctionType::Param>;

  SmallVector<SynthesizedParam, 4> SynthesizedArgs;

public:
  MissingArgumentsFailure(const Solution &solution,
                          ArrayRef<SynthesizedParam> synthesizedArgs,
                          ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator),
        SynthesizedArgs(synthesizedArgs.begin(), synthesizedArgs.end()) {
    assert(!SynthesizedArgs.empty() && "No missing arguments?!");
  }

  TypedNode getAnchor() const override;

  bool diagnoseAsError() override;

  bool diagnoseAsNote() override;

  bool diagnoseSingleMissingArgument() const;

private:
  /// If missing arguments come from a closure,
  /// let's produce tailored diagnostics.
  bool diagnoseClosure(const ClosureExpr *closure);

  /// Diagnose cases when instead of multiple distinct arguments
  /// call got a single tuple argument with expected arity/types.
  bool diagnoseInvalidTupleDestructuring() const;

  /// Determine whether missing arguments are associated with
  /// an implicit call to a property wrapper initializer e.g.
  /// `@Foo(answer: 42) var question = "ultimate question"`
  bool isPropertyWrapperInitialization() const;

  /// Gather information associated with expression that represents
  /// a call - function, arguments, # of arguments and the position of
  /// the first trailing closure.
  std::tuple<Expr *, Expr *, unsigned, Optional<unsigned>>
      getCallInfo(TypedNode anchor) const;

  /// Transform given argument into format suitable for a fix-it
  /// text e.g. `[<label>:]? <#<type#>`
  void forFixIt(llvm::raw_svector_ostream &out,
                const AnyFunctionType::Param &argument) const;

public:
  /// Due to the fact that `matchCallArgument` can't and
  /// doesn't take types into consideration while matching
  /// arguments to parameters, for cases where both arguments
  /// are un-labeled, it's impossible to say which one is missing:
  ///
  /// func foo(_: Int, _: String) {}
  /// foo("")
  ///
  /// In this case first argument is missing, but we end up with
  /// two fixes - argument mismatch (for #1) and missing argument
  /// (for #2), which is incorrect so it has to be handled specially.
  static bool isMisplacedMissingArgument(const Solution &solution,
                                         ConstraintLocator *locator);
};

class ExtraneousArgumentsFailure final : public FailureDiagnostic {
  FunctionType *ContextualType;
  SmallVector<std::pair<unsigned, AnyFunctionType::Param>, 4> ExtraArgs;

public:
  ExtraneousArgumentsFailure(
      const Solution &solution, FunctionType *contextualType,
      ArrayRef<std::pair<unsigned, AnyFunctionType::Param>> extraArgs,
      ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator),
        ContextualType(resolveType(contextualType)->castTo<FunctionType>()),
        ExtraArgs(extraArgs.begin(), extraArgs.end()) {}

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;

private:
  bool diagnoseSingleExtraArgument() const;

  unsigned getTotalNumArguments() const {
    return ContextualType->getNumParams() + ExtraArgs.size();
  }

  bool isContextualMismatch() const {
    auto *locator = getLocator();
    return locator->isLastElement<LocatorPathElt::ContextualType>() ||
           locator->isLastElement<LocatorPathElt::ApplyArgToParam>();
  }
};

class OutOfOrderArgumentFailure final : public FailureDiagnostic {
  using ParamBinding = SmallVector<unsigned, 1>;

  unsigned ArgIdx;
  unsigned PrevArgIdx;

  SmallVector<ParamBinding, 4> Bindings;

public:
  OutOfOrderArgumentFailure(const Solution &solution, unsigned argIdx,
                            unsigned prevArgIdx,
                            ArrayRef<ParamBinding> bindings,
                            ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), ArgIdx(argIdx),
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
  ClosureParamDestructuringFailure(const Solution &solution,
                                   FunctionType *contextualType,
                                   ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), ContextualType(contextualType) {}

  SourceLoc getLoc() const override;
  SourceRange getSourceRange() const override;

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
  InaccessibleMemberFailure(const Solution &solution, ValueDecl *member,
                            ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), Member(member) {}

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
  MutatingMemberRefOnImmutableBase(const Solution &solution, ValueDecl *member,
                                   ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), Member(member) {}

  bool diagnoseAsError() override;
};

/// Diagnose an attempt to use AnyObject as the root type of a KeyPath
///
/// ```swift
/// let keyPath = \AnyObject.bar
/// ```
class AnyObjectKeyPathRootFailure final : public FailureDiagnostic {

public:
  AnyObjectKeyPathRootFailure(const Solution &solution,
                              ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {}

  SourceLoc getLoc() const override;
  SourceRange getSourceRange() const override;

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
  KeyPathSubscriptIndexHashableFailure(const Solution &solution, Type type,
                                       ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), NonConformingType(type) {
    assert(locator->isResultOfKeyPathDynamicMemberLookup() ||
           locator->isKeyPathSubscriptComponent());
  }

  SourceLoc getLoc() const override;

  bool diagnoseAsError() override;
};

class InvalidMemberRefInKeyPath : public FailureDiagnostic {
  ValueDecl *Member;

public:
  InvalidMemberRefInKeyPath(const Solution &solution, ValueDecl *member,
                            ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), Member(member) {
    assert(member->hasName());
    assert(locator->isForKeyPathComponent() ||
           locator->isForKeyPathDynamicMemberLookup());
  }

  DescriptiveDeclKind getKind() const { return Member->getDescriptiveKind(); }

  DeclName getName() const { return Member->getName(); }

  bool diagnoseAsError() override = 0;

protected:
  /// Compute location of the failure for diagnostic.
  SourceLoc getLoc() const override;

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
  InvalidStaticMemberRefInKeyPath(const Solution &solution, ValueDecl *member,
                                  ConstraintLocator *locator)
      : InvalidMemberRefInKeyPath(solution, member, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose an attempt to reference an enum case as a key path component
/// e.g.
///
/// ```swift
/// enum E {
///   case foo
/// }
///
/// _ = \E.Type.foo
/// ```
class InvalidEnumCaseRefInKeyPath final : public InvalidMemberRefInKeyPath {
public:
  InvalidEnumCaseRefInKeyPath(const Solution &solution, ValueDecl *member,
                              ConstraintLocator *locator)
      : InvalidMemberRefInKeyPath(solution, member, locator) {}

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
  InvalidMemberWithMutatingGetterInKeyPath(const Solution &solution,
                                           ValueDecl *member,
                                           ConstraintLocator *locator)
      : InvalidMemberRefInKeyPath(solution, member, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose an attempt to reference a method or initializer as a key path component
/// e.g.
///
/// ```swift
/// struct S {
///   init() { }
///   func foo() -> Int { return 42 }
///   static func bar() -> Int { return 0 }
/// }
///
/// _ = \S.foo
/// _ = \S.Type.bar
/// _ = \S.init
/// ```
class InvalidMethodRefInKeyPath final : public InvalidMemberRefInKeyPath {
public:
  InvalidMethodRefInKeyPath(const Solution &solution, ValueDecl *method,
                            ConstraintLocator *locator)
      : InvalidMemberRefInKeyPath(solution, method, locator) {
    assert(isa<FuncDecl>(method) || isa<ConstructorDecl>(method));
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
  ExtraneousReturnFailure(const Solution &solution, ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {}

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
  CollectionElementContextualFailure(const Solution &solution, Type eltType,
                                     Type contextualType,
                                     ConstraintLocator *locator)
      : ContextualFailure(solution, eltType, contextualType, locator) {}

  bool diagnoseAsError() override;
};

class MissingContextualConformanceFailure final : public ContextualFailure {
  ContextualTypePurpose Context;

public:
  MissingContextualConformanceFailure(const Solution &solution,
                                      ContextualTypePurpose context, Type type,
                                      Type protocolType,
                                      ConstraintLocator *locator)
      : ContextualFailure(solution, type, protocolType, locator),
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
  InOutConversionFailure(const Solution &solution, Type argType, Type paramType,
                         ConstraintLocator *locator)
      : ContextualFailure(solution, argType, paramType, locator) {}

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
  using Anchor = llvm::PointerUnion<TypeRepr *, const Expr *>;

  SmallVector<GenericTypeParamType *, 4> Parameters;

public:
  MissingGenericArgumentsFailure(const Solution &solution,
                                 ArrayRef<GenericTypeParamType *> missingParams,
                                 ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {
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
  SkipUnhandledConstructInFunctionBuilderFailure(const Solution &solution,
                                                 UnhandledNode unhandled,
                                                 NominalTypeDecl *builder,
                                                 ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), unhandled(unhandled),
        builder(builder) {}

  SourceLoc getLoc() const override;

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
  InvalidTupleSplatWithSingleParameterFailure(const Solution &solution,
                                              Type paramTy,
                                              ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), ParamType(paramTy) {}
  bool diagnoseAsError() override;
};

/// Diagnose situation when an array is passed instead of varargs.
///
/// ```swift
/// func foo(_ x: Int...) {}
/// foo([1,2,3]]) // foo expects varags like foo(1,2,3) instead.
/// ```
class ExpandArrayIntoVarargsFailure final : public ContextualFailure {
public:
  ExpandArrayIntoVarargsFailure(const Solution &solution, Type lhs, Type rhs,
                                ConstraintLocator *locator)
      : ContextualFailure(solution, lhs, rhs, locator) {}

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;

  void tryDropArrayBracketsFixIt(const Expr *anchor) const;
};

/// Diagnose a situation there is a mismatch between argument and parameter
/// types e.g.:
///
/// ```swift
/// func foo(_: String) {}
/// func bar(_ v: Int) { foo(v) } // `Int` is not convertible to `String`
/// ```
class ArgumentMismatchFailure : public ContextualFailure {
  FunctionArgApplyInfo Info;

public:
  ArgumentMismatchFailure(const Solution &solution, Type argType,
                          Type paramType, ConstraintLocator *locator)
      : ContextualFailure(solution, argType, paramType, locator),
        Info(*getFunctionArgApplyInfo(getLocator())) {}

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;

  /// If both argument and parameter are represented by `ArchetypeType`
  /// produce a special diagnostic in case their names match.
  bool diagnoseArchetypeMismatch() const;

  /// Tailored diagnostic for pattern matching with `~=` operator.
  bool diagnosePatternMatchingMismatch() const;

  /// Tailored diagnostics for argument mismatches associated with
  /// reference equality operators `===` and `!==`.
  bool diagnoseUseOfReferenceEqualityOperator() const;

  /// Tailored diagnostics for type mismatches associated with
  /// property wrapper initialization via implicit `init(wrappedValue:)`
  /// or now deprecated `init(initialValue:)`.
  bool diagnosePropertyWrapperMismatch() const;

  /// Tailored diagnostics for argument mismatches associated with trailing
  /// closures being passed to non-closure parameters.
  bool diagnoseTrailingClosureMismatch() const;

protected:
  /// \returns The position of the argument being diagnosed, starting at 1.
  unsigned getArgPosition() const { return Info.getArgPosition(); }

  /// \returns The position of the parameter being diagnosed, starting at 1.
  unsigned getParamPosition() const { return Info.getParamPosition(); }

  /// Returns the argument expression being diagnosed.
  ///
  /// Note this may differ from \c getAnchor(), which will return a smaller
  /// sub-expression if the failed constraint is for a sub-expression within
  /// an argument. For example, in an argument conversion from (T, U) to (U, U),
  /// the conversion from T to U may fail. In this case, \c getArgExpr() will
  /// return the (T, U) expression, whereas \c getAnchor() will return the T
  /// expression.
  Expr *getArgExpr() const { return Info.getArgExpr(); }

  /// Returns the argument type for the conversion being diagnosed.
  ///
  /// \param withSpecifier Whether to keep the inout or @lvalue specifier of
  /// the argument, if any.
  ///
  /// Note this may differ from \c getFromType(), which will give the source
  /// type of a failed constraint for the argument conversion. For example in
  /// an argument conversion from T? to U?, the conversion from T to U may fail.
  /// In this case, \c getArgType() will return T?, whereas \c getFromType()
  /// will return T.
  Type getArgType(bool withSpecifier = false) const {
    return Info.getArgType(withSpecifier);
  }

  /// \returns A textual description of the argument suitable for diagnostics.
  /// For an argument with an unambiguous label, this will the label. Otherwise
  /// it will be its position in the argument list.
  StringRef getArgDescription(SmallVectorImpl<char> &scratch) const {
    return Info.getArgDescription(scratch);
  }

  /// \returns The interface type for the function being applied.
  Type getFnInterfaceType() const { return Info.getFnInterfaceType(); }

  /// \returns The function type being applied, including any generic
  /// substitutions.
  FunctionType *getFnType() const { return Info.getFnType(); }

  /// \returns The callee for the argument conversion, if any.
  const ValueDecl *getCallee() const { return Info.getCallee(); }

  /// \returns The full name of the callee, or a null decl name if there is no
  /// callee.
  DeclName getCalleeFullName() const {
    return getCallee() ? getCallee()->getName() : DeclName();
  }

  /// Returns the type of the parameter involved in the mismatch, including any
  /// generic substitutions.
  ///
  /// \param lookThroughAutoclosure Whether an @autoclosure () -> T parameter
  /// should be treated as being of type T.
  ///
  /// Note this may differ from \c getToType(), see the note on \c getArgType().
  Type getParamType(bool lookThroughAutoclosure = true) const {
    return Info.getParamType(lookThroughAutoclosure);
  }

  /// Returns the type of the parameter involved in the mismatch.
  ///
  /// \param lookThroughAutoclosure Whether an @autoclosure () -> T parameter
  /// should be treated as being of type T.
  ///
  /// Note this may differ from \c getToType(), see the note on \c getArgType().
  Type getParamInterfaceType(bool lookThroughAutoclosure = true) const {
    return Info.getParamInterfaceType(lookThroughAutoclosure);
  }

  /// \returns The flags of the parameter involved in the mismatch.
  ParameterTypeFlags getParameterFlags() const {
    return Info.getParameterFlags();
  }

  /// \returns The flags of a parameter at a given index.
  ParameterTypeFlags getParameterFlagsAtIndex(unsigned idx) const {
    return Info.getParameterFlagsAtIndex(idx);
  }

  /// Situations like this:
  ///
  /// func foo(_: Int, _: String) {}
  /// foo("")
  ///
  /// Are currently impossible to fix correctly,
  /// so we have to attend to that in diagnostics.
  bool diagnoseMisplacedMissingArgument() const;
};

/// Replace a coercion ('as') with a forced checked cast ('as!').
class MissingForcedDowncastFailure final : public ContextualFailure {
public:
  MissingForcedDowncastFailure(const Solution &solution, Type fromType,
                               Type toType, ConstraintLocator *locator)
      : ContextualFailure(solution, fromType, toType, locator) {}

  TypedNode getAnchor() const override;

  bool diagnoseAsError() override;
};

class ExtraneousCallFailure final : public FailureDiagnostic {
public:
  ExtraneousCallFailure(const Solution &solution, ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {}

  bool diagnoseAsError() override;
};

class InvalidUseOfTrailingClosure final : public ArgumentMismatchFailure {
public:
  InvalidUseOfTrailingClosure(const Solution &solution, Type argType,
                              Type paramType, ConstraintLocator *locator)
      : ArgumentMismatchFailure(solution, argType, paramType, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose the invalid conversion of a temporary pointer argument generated
/// from an X-to-pointer conversion to an @_nonEphemeral parameter.
///
/// ```swift
/// func foo(@_nonEphemeral _ ptr: UnsafePointer<Int>) {}
///
/// foo([1, 2, 3])
/// ```
class NonEphemeralConversionFailure final : public ArgumentMismatchFailure {
  ConversionRestrictionKind ConversionKind;
  bool DowngradeToWarning;

public:
  NonEphemeralConversionFailure(const Solution &solution,
                                ConstraintLocator *locator, Type fromType,
                                Type toType,
                                ConversionRestrictionKind conversionKind,
                                bool downgradeToWarning)
      : ArgumentMismatchFailure(solution, fromType, toType, locator),
        ConversionKind(conversionKind), DowngradeToWarning(downgradeToWarning) {
  }

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;

private:
  /// Attempts to emit a specialized diagnostic for
  /// Unsafe[Mutable][Raw]Pointer.init([mutating]:) &
  /// Unsafe[Mutable][Raw]BufferPointer.init(start:count:).
  bool diagnosePointerInit() const;

  /// Emits a note explaining to the user that an ephemeral conversion is only
  /// valid for the duration of the call, and suggests an alternative to use.
  void emitSuggestionNotes() const;
};

class AssignmentTypeMismatchFailure final : public ContextualFailure {
public:
  AssignmentTypeMismatchFailure(const Solution &solution,
                                ContextualTypePurpose context, Type srcType,
                                Type dstType, ConstraintLocator *locator)
      : ContextualFailure(solution, context, srcType, dstType, locator) {}

  bool diagnoseAsError() override;
  bool diagnoseAsNote() override;

private:
  bool diagnoseMissingConformance() const;
};

class MissingContextualBaseInMemberRefFailure final : public FailureDiagnostic {
  DeclNameRef MemberName;

public:
  MissingContextualBaseInMemberRefFailure(const Solution &solution,
                                          DeclNameRef member,
                                          ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator), MemberName(member) {}

  bool diagnoseAsError();
};

class UnableToInferClosureParameterType final : public FailureDiagnostic {
public:
  UnableToInferClosureParameterType(const Solution &solution,
                                    ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {}

  bool diagnoseAsError();
};

class UnableToInferClosureReturnType final : public FailureDiagnostic {
public:
  UnableToInferClosureReturnType(const Solution &solution,
                                 ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {}

  bool diagnoseAsError();
};

class UnableToInferProtocolLiteralType final : public FailureDiagnostic {
public:
  UnableToInferProtocolLiteralType(const Solution &solution,
                                   ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {}

  SourceLoc getLoc() const override;

  bool diagnoseAsError() override;
};

/// Diagnose an attempt to reference a top-level name shadowed by a local
/// member e.g.
///
/// ```swift
/// extension Sequence {
///   func test() -> Int {
///     return max(1, 2)
///   }
/// }
/// ```
///
/// Here `max` refers to a global function `max<T>(_: T, _: T)` in `Swift`
/// module and can only be accessed by adding `Swift.` to it, because `Sequence`
/// has a member named `max` which accepts a single argument.
class MissingQuialifierInMemberRefFailure final : public FailureDiagnostic {
public:
  MissingQuialifierInMemberRefFailure(const Solution &solution,
                                      ConstraintLocator *locator)
      : FailureDiagnostic(solution, locator) {}

  bool diagnoseAsError();
};

/// Emits a warning about an attempt to use the 'as' operator as the 'as!'
/// operator.
class CoercionAsForceCastFailure final : public ContextualFailure {
public:
  CoercionAsForceCastFailure(const Solution &solution, Type fromType,
                             Type toType, ConstraintLocator *locator)
      : ContextualFailure(solution, fromType, toType, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose a key path root type that cannot be applied to an instance
/// base that has another type.
///
/// \code
/// func f(_ bar: Bar , keyPath: KeyPath<Foo, Int> ) {
///   bar[keyPath: keyPath]
/// }
/// \endcode
class KeyPathRootTypeMismatchFailure final : public ContextualFailure {
public:
  KeyPathRootTypeMismatchFailure(const Solution &solution, Type lhs, Type rhs,
                                 ConstraintLocator *locator)
      : ContextualFailure(solution, lhs, rhs, locator) {}

  bool diagnoseAsError() override;
};

/// Diagnose an attempt to use a KeyPath where a multi-argument function is expected
///
/// ```swift
/// [Item].sorted(\Item.name)
/// ```
class MultiArgFuncKeyPathFailure final : public FailureDiagnostic {
  Type functionType;
public:
  MultiArgFuncKeyPathFailure(const Solution &solution, Type functionType,
                             ConstraintLocator *locator)
  : FailureDiagnostic(solution, locator),
  functionType(functionType) {}

  bool diagnoseAsError() override;
};

/// Diagnose a failure to infer a KeyPath type by context.
///
/// ```swift
/// _ = \.x
/// let _ : AnyKeyPath = \.x
/// ```
class UnableToInferKeyPathRootFailure final : public FailureDiagnostic {
public:
  UnableToInferKeyPathRootFailure(const Solution &solution,
                                  ConstraintLocator *locator)
  : FailureDiagnostic(solution, locator) {}

  bool diagnoseAsError() override;
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSDIAGNOSTICS_H
