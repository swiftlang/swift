//===--- TypeCheckProtocol.cpp - Protocol Checking ------------------------===//
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
// This file implements semantic analysis for protocols, in particular, checking
// whether a given type conforms to a given protocol.
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "DerivedConformances.h"
#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Defer.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

namespace {
  struct RequirementMatch;

  /// The result of attempting to resolve a witness.
  enum class ResolveWitnessResult {
    /// The resolution succeeded.
    Success,
    /// There was an explicit witness available, but it failed some
    /// criteria.
    ExplicitFailed,
    /// There was no witness available.
    Missing
  };

  /// Describes the result of checking a type witness.
  ///
  /// This class evaluates true if an error occurred.
  class CheckTypeWitnessResult {
    ProtocolDecl *Proto = nullptr;

  public:
    CheckTypeWitnessResult() { }

    CheckTypeWitnessResult(ProtocolDecl *proto) : Proto(proto) { }

    ProtocolDecl *getProtocol() const { return Proto; }

    explicit operator bool() const { return Proto != nullptr; }
  };

  /// The set of associated types that have been inferred by matching
  /// the given value witness to its corresponding requirement.
  struct InferredAssociatedTypesByWitness {
    /// The witness we matched.
    ValueDecl *Witness = nullptr;

    /// The associated types inferred from matching this witness.
    SmallVector<std::pair<AssociatedTypeDecl *, Type>, 4> Inferred;

    /// Inferred associated types that don't meet the associated type
    /// requirements.
    SmallVector<std::tuple<AssociatedTypeDecl *, Type, CheckTypeWitnessResult>,
                2> NonViable;
  };

  /// The set of witnesses that were considered when attempting to
  /// infer associated types.
  typedef SmallVector<InferredAssociatedTypesByWitness, 2>
    InferredAssociatedTypesByWitnesses;

  /// A mapping from requirements to the set of matches with witnesses.
  typedef SmallVector<std::pair<ValueDecl *,
                                InferredAssociatedTypesByWitnesses>, 4>
    InferredAssociatedTypes;

  /// The protocol conformance checker.
  ///
  /// This helper class handles most of the details of checking whether a
  /// given type (\c Adoptee) conforms to a protocol (\c Proto).
  class ConformanceChecker {
    TypeChecker &TC;
    NormalProtocolConformance *Conformance;
    ProtocolDecl *Proto;
    Type Adoptee;
    DeclContext *DC;
    SourceLoc Loc;
    
    /// Witnesses that are currently being resolved.
    llvm::SmallPtrSet<ValueDecl *, 4> ResolvingWitnesses;

    /// Caches the set of associated types that are referenced in each
    /// requirement.
    llvm::DenseMap<ValueDecl *, llvm::SmallVector<AssociatedTypeDecl *, 2>>
      ReferencedAssociatedTypes;

    /// True if we shouldn't complain about problems with this conformance
    /// right now, i.e. if methods are being called outside
    /// checkConformance().
    bool SuppressDiagnostics = true;

    /// Whether we've already complained about problems with this conformance.
    bool AlreadyComplained = false;

    /// Retrieve the associated types that are referenced by the given
    /// requirement with a base of 'Self'.
    ArrayRef<AssociatedTypeDecl *> getReferencedAssociatedTypes(ValueDecl *req);

    /// Record a (non-type) witness for the given requirement.
    void recordWitness(ValueDecl *requirement, const RequirementMatch &match);

    /// Record that the given optional requirement has no witness.
    void recordOptionalWitness(ValueDecl *requirement);

    /// Record a type witness.
    ///
    /// \param assocType The associated type whose witness is being recorded.
    ///
    /// \param type The witness type.
    ///
    /// \param typeDecl The decl the witness type came from; can be null.
    ///
    /// \param fromDC The DeclContext from which this associated type was
    /// computed, which may be different from the context associated with the
    /// protocol conformance.
    ///
    /// \param wasDeducedOrDefaulted Whether this witness was deduced or
    /// defaulted (rather than being explicitly provided).
    void recordTypeWitness(AssociatedTypeDecl *assocType, Type type,
                           TypeDecl *typeDecl, DeclContext *fromDC,
                           bool wasDeducedOrDefaulted,
                           bool performRedeclarationCheck = true);

    /// Resolve a (non-type) witness via name lookup.
    ResolveWitnessResult resolveWitnessViaLookup(ValueDecl *requirement);

    /// Resolve a (non-type) witness via derivation.
    ResolveWitnessResult resolveWitnessViaDerivation(ValueDecl *requirement);

    /// Resolve a (non-type) witness via default definition or optional.
    ResolveWitnessResult resolveWitnessViaDefault(ValueDecl *requirement);

    /// Gather the value witnesses for the given requirement.
    ///
    /// \param ignoringNames If non-null and there are no value
    /// witnesses with the correct full name, the results will reflect
    /// lookup for just the base name and the pointee will be set to
    /// \c true.
    SmallVector<ValueDecl *, 4> lookupValueWitnesses(ValueDecl *req,
                                                     bool *ignoringNames);

    /// Attempt to resolve a type witness via member name lookup.
    ResolveWitnessResult resolveTypeWitnessViaLookup(
                           AssociatedTypeDecl *assocType);

    /// Infer associated type witnesses for the given tentative
    /// requirement/witness match.
    InferredAssociatedTypesByWitness inferTypeWitnessesViaValueWitness(
                                       ValueDecl *req,
                                       ValueDecl *witness);

    /// Infer associated type witnesses for the given value requirement.
    InferredAssociatedTypesByWitnesses inferTypeWitnessesViaValueWitnesses(
                                         ValueDecl *req);

    /// Infer associated type witnesses for all relevant value requirements.
    ///
    /// \param assocTypes The set of associated types we're interested in.
    InferredAssociatedTypes
    inferTypeWitnessesViaValueWitnesses(
      const llvm::SetVector<AssociatedTypeDecl *> &assocTypes);

    /// Diagnose or defer a diagnostic, as appropriate.
    ///
    /// \param requirement The requirement with which this diagnostic is
    /// associated, if any.
    ///
    /// \param isError Whether this diagnostic is an error.
    ///
    /// \param fn A function to call to emit the actual diagnostic. If
    /// diagnostics are being deferred,
    void diagnoseOrDefer(
           ValueDecl *requirement, bool isError,
           std::function<void(TypeChecker &, NormalProtocolConformance *)> fn);

    /// Emit any diagnostics that have been delayed.
    void emitDelayedDiags();

  public:
    ConformanceChecker(TypeChecker &tc, NormalProtocolConformance *conformance)
      : TC(tc), Conformance(conformance),
        Proto(conformance->getProtocol()),
        Adoptee(conformance->getType()), 
        DC(conformance->getDeclContext()),
        Loc(conformance->getLoc()) { }

    /// Resolve all of the type witnesses.
    void resolveTypeWitnesses();

    /// Resolve the witness for the given non-type requirement as
    /// directly as possible, only resolving other witnesses if
    /// needed, e.g., to determine type witnesses used within the
    /// requirement.
    ///
    /// This entry point is designed to be used when the witness for a
    /// particular requirement and adoptee is required, before the
    /// conformance has been completed checked.
    void resolveSingleWitness(ValueDecl *requirement);

    /// Resolve the type witness for the given associated type as
    /// directly as possible.
    void resolveSingleTypeWitness(AssociatedTypeDecl *assocType);

    /// Check the entire protocol conformance, ensuring that all
    /// witnesses are resolved and emitting any diagnostics.
    void checkConformance();
  };
}

# pragma mark Witness resolution
/// \brief Retrieve the kind of requirement described by the given declaration,
/// for use in some diagnostics.
static diag::RequirementKind getRequirementKind(ValueDecl *VD) {
  if (isa<ConstructorDecl>(VD))
    return diag::RequirementKind::Constructor;

  if (isa<FuncDecl>(VD))
    return diag::RequirementKind::Func;

  if (isa<VarDecl>(VD))
    return diag::RequirementKind::Var;

  assert(isa<SubscriptDecl>(VD) && "Unhandled requirement kind");
  return diag::RequirementKind::Subscript;
}

namespace {
  /// \brief The result of matching a particular declaration to a given
  /// requirement.
  enum class MatchKind : unsigned char {
    /// \brief The witness matched the requirement exactly.
    ExactMatch,

    /// \brief There is a difference in optionality.
    OptionalityConflict,

    /// \brief The witness matched the requirement with some renaming.
    RenamedMatch,

    /// \brief The witness is invalid or has an invalid type.
    WitnessInvalid,

    /// \brief The kind of the witness and requirement differ, e.g., one
    /// is a function and the other is a variable.
    KindConflict,

    /// \brief The types conflict.
    TypeConflict,

    /// The witness throws, but the requirement does not.
    ThrowsConflict,

    /// \brief The witness did not match due to static/non-static differences.
    StaticNonStaticConflict,
    
    /// \brief The witness is not settable, but the requirement is.
    SettableConflict,

    /// \brief The witness did not match due to prefix/non-prefix differences.
    PrefixNonPrefixConflict,

    /// \brief The witness did not match due to postfix/non-postfix differences.
    PostfixNonPostfixConflict,
    
    /// \brief The witness did not match because of mutating conflicts.
    MutatingConflict,

    /// The witness is not @noreturn, but the requirement is.
    NoReturnConflict,

    /// The witness is not rethrows, but the requirement is.
    RethrowsConflict,

    /// The witness has a different Objective-C selector than the
    /// requirement.
    ObjCSelectorConflict,

    /// The witness is not @objc but the requirement is.
    NotObjC,
  };

  /// Describes the kind of optional adjustment performed when
  /// comparing two types.
  enum class OptionalAdjustmentKind {
    // No adjustment required.
    None,

    /// The witness can produce a 'nil' that won't be handled by
    /// callers of the requirement. This is a type-safety problem.
    ProducesUnhandledNil,

    /// Callers of the requirement can provide 'nil', but the witness
    /// does not handle it. This is a type-safety problem.
    ConsumesUnhandledNil,

    /// The witness handles 'nil', but won't ever be given a 'nil'.
    /// This is not a type-safety problem.
    WillNeverConsumeNil,
      
    /// Callers of the requirement can expect to receive 'nil', but
    /// the witness will never produce one. This is not a type-safety
    /// problem.
    WillNeverProduceNil,

    /// The witness has an IUO that can be removed, because the
    /// protocol doesn't need it. This is not a type-safety problem.
    RemoveIUO,

    /// The witness has an IUO that should be translated into a true
    /// optional. This is not a type-safety problem.
    IUOToOptional,
  };

  /// Describes an optional adjustment made to a witness.
  class OptionalAdjustment {
    /// The kind of adjustment.
    unsigned Kind : 16;

    /// Whether this is a parameter adjustment (with an index) vs. a
    /// result or value type adjustment (no index needed).
    unsigned IsParameterAdjustment : 1;

    /// The adjustment index, for parameter adjustments.
    unsigned ParameterAdjustmentIndex : 15;

  public:
    /// Create a non-parameter optional adjustment.
    explicit OptionalAdjustment(OptionalAdjustmentKind kind) 
      : Kind(static_cast<unsigned>(kind)), IsParameterAdjustment(false),
        ParameterAdjustmentIndex(0) { }

    /// Create an optional adjustment to a parameter.
    OptionalAdjustment(OptionalAdjustmentKind kind,
                       unsigned parameterIndex)
      : Kind(static_cast<unsigned>(kind)), IsParameterAdjustment(true),
        ParameterAdjustmentIndex(parameterIndex) { }

    /// Determine the kind of optional adjustment.
    OptionalAdjustmentKind getKind() const { 
      return static_cast<OptionalAdjustmentKind>(Kind);
    }

    /// Determine whether this is a parameter adjustment.
    bool isParameterAdjustment() const {
      return IsParameterAdjustment;
    }

    /// Return the index of a parameter adjustment.
    unsigned getParameterIndex() const {
      assert(isParameterAdjustment() && "Not a parameter adjustment");
      return ParameterAdjustmentIndex;
    }

    /// Determines whether the optional adjustment is an error.
    bool isError() const {
      switch (getKind()) {
      case OptionalAdjustmentKind::None:
        return false;

      case OptionalAdjustmentKind::ProducesUnhandledNil:
      case OptionalAdjustmentKind::ConsumesUnhandledNil:
        return true;

      case OptionalAdjustmentKind::WillNeverConsumeNil:
      case OptionalAdjustmentKind::WillNeverProduceNil:
      case OptionalAdjustmentKind::RemoveIUO:
      case OptionalAdjustmentKind::IUOToOptional:
        // Warnings at most.
        return false;
      }
    }

    /// Retrieve the source location at which the optional is
    /// specified or would be inserted.
    SourceLoc getOptionalityLoc(ValueDecl *witness) const;

    /// Retrieve the optionality location for the given type
    /// representation.
    SourceLoc getOptionalityLoc(TypeRepr *tyR) const;
  };

  /// Whether any of the given optional adjustments is an error (vs. a
  /// warning).
  bool hasAnyError(ArrayRef<OptionalAdjustment> adjustments) {
    for (const auto &adjustment : adjustments)
      if (adjustment.isError())
        return true;

    return false;
  }

  /// \brief Describes a match between a requirement and a witness.
  struct RequirementMatch {
    RequirementMatch(ValueDecl *witness, MatchKind kind)
      : Witness(witness), Kind(kind), WitnessType() {
      assert(!hasWitnessType() && "Should have witness type");
    }

    RequirementMatch(ValueDecl *witness, MatchKind kind,
                     Type witnessType,
                     ArrayRef<OptionalAdjustment> optionalAdjustments = {})
      : Witness(witness), Kind(kind), WitnessType(witnessType),
        OptionalAdjustments(optionalAdjustments.begin(),
                            optionalAdjustments.end())
    {
      assert(hasWitnessType() == !witnessType.isNull() &&
             "Should (or should not) have witness type");
    }

    /// \brief The witness that matches the (implied) requirement.
    ValueDecl *Witness;
    
    /// \brief The kind of match.
    MatchKind Kind;

    /// \brief The type of the witness when it is referenced.
    Type WitnessType;

    /// The set of optional adjustments performed on the witness.
    SmallVector<OptionalAdjustment, 2> OptionalAdjustments;

    /// \brief Determine whether this match is viable.
    bool isViable() const {
      switch(Kind) {
      case MatchKind::ExactMatch:
      case MatchKind::OptionalityConflict:
      case MatchKind::RenamedMatch:
        return true;

      case MatchKind::WitnessInvalid:
      case MatchKind::KindConflict:
      case MatchKind::TypeConflict:
      case MatchKind::StaticNonStaticConflict:
      case MatchKind::SettableConflict:
      case MatchKind::PrefixNonPrefixConflict:
      case MatchKind::PostfixNonPostfixConflict:
      case MatchKind::MutatingConflict:
      case MatchKind::NoReturnConflict:
      case MatchKind::RethrowsConflict:
      case MatchKind::ThrowsConflict:
      case MatchKind::ObjCSelectorConflict:
      case MatchKind::NotObjC:
        return false;
      }
    }

    /// \brief Determine whether this requirement match has a witness type.
    bool hasWitnessType() const {
      switch(Kind) {
      case MatchKind::ExactMatch:
      case MatchKind::RenamedMatch:
      case MatchKind::TypeConflict:
      case MatchKind::OptionalityConflict:
        return true;

      case MatchKind::WitnessInvalid:
      case MatchKind::KindConflict:
      case MatchKind::StaticNonStaticConflict:
      case MatchKind::SettableConflict:
      case MatchKind::PrefixNonPrefixConflict:
      case MatchKind::PostfixNonPostfixConflict:
      case MatchKind::MutatingConflict:
      case MatchKind::NoReturnConflict:
      case MatchKind::RethrowsConflict:
      case MatchKind::ThrowsConflict:
      case MatchKind::ObjCSelectorConflict:
      case MatchKind::NotObjC:
        return false;
      }
    }

    /// \brief Associated type substitutions needed to match the witness.
    SmallVector<Substitution, 2> WitnessSubstitutions;

    /// Classify the provided optionality issues for use in diagnostics.
    /// FIXME: Enumify this
    unsigned classifyOptionalityIssues(ValueDecl *requirement) const {
      unsigned numParameterAdjustments = 0;
      bool hasNonParameterAdjustment = false;
      for (const auto &adjustment : OptionalAdjustments) {
        if (adjustment.isParameterAdjustment())
          ++numParameterAdjustments;
        else
          hasNonParameterAdjustment = true;
      }

      if (hasNonParameterAdjustment) {
        // Both return and parameter adjustments.
        if (numParameterAdjustments > 0)
          return 4;

        // The type of a variable.
        if (isa<VarDecl>(requirement))
          return 0;

        // The result type of something.
        return 1;
      }

      // Only parameter adjustments.
      assert(numParameterAdjustments > 0 && "No adjustments?");
      return numParameterAdjustments == 1 ? 2 : 3;
    }

    /// Add Fix-Its that correct the optionality in the witness.
    void addOptionalityFixIts(const ASTContext &ctx,
                              ValueDecl *witness, 
                              InFlightDiagnostic &diag) const;
  };
}

SourceLoc OptionalAdjustment::getOptionalityLoc(ValueDecl *witness) const {
  // For non-parameter adjustments, use the result type or whole type,
  // as appropriate.
  if (!isParameterAdjustment()) {
    // For a function, use the result type.
    if (auto func = dyn_cast<FuncDecl>(witness)) {
      return getOptionalityLoc(
               func->getBodyResultTypeLoc().getTypeRepr());
    }

    // For a subscript, use the element type.
    if (auto subscript = dyn_cast<SubscriptDecl>(witness)) {
      return getOptionalityLoc(
               subscript->getElementTypeLoc().getTypeRepr());
    }

    // Otherwise, we have a variable.
    // FIXME: Dig into the pattern.
    return SourceLoc();
  }

  // For parameter adjustments, dig out the pattern.
  Pattern *pattern = nullptr;
  if (auto func = dyn_cast<AbstractFunctionDecl>(witness)) {
    auto bodyPatterns = func->getBodyParamPatterns();
    if (func->getDeclContext()->isTypeContext())
      bodyPatterns = bodyPatterns.slice(1);
    pattern = bodyPatterns[0];
  } else if (auto subscript = dyn_cast<SubscriptDecl>(witness)) {
    pattern = subscript->getIndices();
  } else {
    return SourceLoc();
  }

  // Handle parentheses.
  if (auto paren = dyn_cast<ParenPattern>(pattern)) {
    assert(getParameterIndex() == 0 && "just the one parameter");
    if (auto typed = dyn_cast<TypedPattern>(paren->getSubPattern())) {
      return getOptionalityLoc(typed->getTypeLoc().getTypeRepr());
    }
    return SourceLoc();
  }

  // Handle tuples.
  auto tuple = dyn_cast<TuplePattern>(pattern);
  if (!tuple)
    return SourceLoc();

  const auto &tupleElt = tuple->getElement(getParameterIndex());
  if (auto typed = dyn_cast<TypedPattern>(tupleElt.getPattern())) {
    return getOptionalityLoc(typed->getTypeLoc().getTypeRepr());
  }
  return SourceLoc();
}

SourceLoc OptionalAdjustment::getOptionalityLoc(TypeRepr *tyR) const {
  if (!tyR)
    return SourceLoc();

  switch (getKind()) {
  case OptionalAdjustmentKind::None:
    llvm_unreachable("not an adjustment");

  case OptionalAdjustmentKind::ConsumesUnhandledNil:
  case OptionalAdjustmentKind::WillNeverProduceNil:
    // The location of the '?' to be inserted is after the type.
    return tyR->getEndLoc();

  case OptionalAdjustmentKind::ProducesUnhandledNil:
  case OptionalAdjustmentKind::WillNeverConsumeNil:
  case OptionalAdjustmentKind::RemoveIUO:
  case OptionalAdjustmentKind::IUOToOptional:
    // Find the location of optionality, below.
    break;
  }

  if (auto optRepr = dyn_cast<OptionalTypeRepr>(tyR))
    return optRepr->getQuestionLoc();

  if (auto iuoRepr = dyn_cast<ImplicitlyUnwrappedOptionalTypeRepr>(tyR))
    return iuoRepr->getExclamationLoc();

  return SourceLoc();
}

void RequirementMatch::addOptionalityFixIts(
      const ASTContext &ctx,
       ValueDecl *witness, 
       InFlightDiagnostic &diag) const {
  for (const auto &adjustment : OptionalAdjustments) {
    SourceLoc adjustmentLoc = adjustment.getOptionalityLoc(witness);
    if (adjustmentLoc.isInvalid())
      continue;

    switch (adjustment.getKind()) {
    case OptionalAdjustmentKind::None:
      llvm_unreachable("not an optional adjustment");

    case OptionalAdjustmentKind::ProducesUnhandledNil:
    case OptionalAdjustmentKind::WillNeverConsumeNil:
    case OptionalAdjustmentKind::RemoveIUO:
      diag.fixItRemove(adjustmentLoc);
      break;

    case OptionalAdjustmentKind::WillNeverProduceNil:
    case OptionalAdjustmentKind::ConsumesUnhandledNil:
      diag.fixItInsertAfter(adjustmentLoc, "?");
      break;

    case OptionalAdjustmentKind::IUOToOptional:
      diag.fixItReplace(adjustmentLoc, "?");
      break;
    }
  }

}

///\ brief Decompose the given type into a set of tuple elements.
static SmallVector<TupleTypeElt, 4> decomposeIntoTupleElements(Type type) {
  SmallVector<TupleTypeElt, 4> result;

  if (auto tupleTy = dyn_cast<TupleType>(type.getPointer())) {
    result.append(tupleTy->getElements().begin(), tupleTy->getElements().end());
    return result;
  }

  result.push_back(type);
  return result;
}

namespace {
  /// Dependent type opener that maps the type of a requirement, replacing
  /// already-known associated types to their type witnesses and inner generic
  /// parameters to their archetypes.
  class RequirementTypeOpener : public constraints::DependentTypeOpener {
    /// The type variable that represents the 'Self' type.
    constraints::ConstraintSystem &CS;
    NormalProtocolConformance *Conformance;
    DeclContext *DC;
    ProtocolDecl *Proto;

  public:
    RequirementTypeOpener(constraints::ConstraintSystem &cs,
                          NormalProtocolConformance *conformance,
                          DeclContext *dc)
      : CS(cs), Conformance(conformance), DC(dc),
        Proto(conformance->getProtocol())
    {
    }

    virtual void openedGenericParameter(GenericTypeParamType *param,
                                        TypeVariableType *typeVar,
                                        Type &replacementType) {
      // If this is the 'Self' type, record it.
      if (param->getDepth() == 0 && param->getIndex() == 0)
        CS.SelfTypeVar = typeVar;
      else
        replacementType = ArchetypeBuilder::mapTypeIntoContext(DC, param);
    }

    virtual bool shouldBindAssociatedType(Type baseType,
                                          TypeVariableType *baseTypeVar,
                                          AssociatedTypeDecl *assocType,
                                          TypeVariableType *memberTypeVar,
                                          Type &replacementType) {
      // If the base is our 'Self' type, we have a witness for this
      // associated type already.
      if (baseTypeVar == CS.SelfTypeVar &&
          cast<ProtocolDecl>(assocType->getDeclContext()) == Proto) {
        replacementType = Conformance->getTypeWitness(assocType, nullptr)
                            .getReplacement();

        // Let the member type variable float; we don't want to
        // resolve it as a member.
        return false;
      }

      // If the base is somehow derived from our 'Self' type, we can go ahead
      // and bind it. There's nothing more to do.
      auto rootBaseType = baseType;
      while (auto dependentMember = rootBaseType->getAs<DependentMemberType>())
        rootBaseType = dependentMember->getBase();
      if (auto rootGP = rootBaseType->getAs<GenericTypeParamType>()) {
        if (rootGP->getDepth() == 0 && rootGP->getIndex() == 0)
          return true;
      } else {
        return true;
      }

      // We have a dependent member type based on a generic parameter; map it
      // to an archetype.
      auto memberType = DependentMemberType::get(baseType, assocType,
                                                 DC->getASTContext());
      replacementType = ArchetypeBuilder::mapTypeIntoContext(DC, memberType);
      return true;
    }
  };

  /// The kind of variance (none, covariance, contravariance) to apply
  /// when comparing types from a witness to types in the requirement
  /// we're matching it against.
  enum class VarianceKind {
    None,
    Covariant,
    Contravariant
   };
} // end anonymous namespace

static std::tuple<Type,Type, OptionalAdjustmentKind> 
getTypesToCompare(ValueDecl *reqt, 
                  Type reqtType,
                  Type witnessType,
                  VarianceKind variance) {  
  // For @objc protocols, deal with differences in the optionality.
  // FIXME: It probably makes sense to extend this to non-@objc
  // protocols as well, but this requires more testing.
  OptionalAdjustmentKind optAdjustment = OptionalAdjustmentKind::None;
  if (reqt->isObjC()) {
    OptionalTypeKind reqtOptKind;
    if (Type reqtValueType
          = reqtType->getAnyOptionalObjectType(reqtOptKind))
      reqtType = reqtValueType;
    OptionalTypeKind witnessOptKind;
    if (Type witnessValueType 
          = witnessType->getAnyOptionalObjectType(witnessOptKind))
      witnessType = witnessValueType;

    switch (reqtOptKind) {
    case OTK_None:
      switch (witnessOptKind) {
      case OTK_None:
        // Exact match is always okay.
        break;

      case OTK_Optional:
        switch (variance) {
        case VarianceKind::None:
        case VarianceKind::Covariant:
          optAdjustment = OptionalAdjustmentKind::ProducesUnhandledNil;
          break;

        case VarianceKind::Contravariant:
          optAdjustment = OptionalAdjustmentKind::WillNeverConsumeNil;
          break;
        }
        break;

      case OTK_ImplicitlyUnwrappedOptional:
        optAdjustment = OptionalAdjustmentKind::RemoveIUO;
        break;
      }
      break;

    case OTK_Optional:
      switch (witnessOptKind) {
      case OTK_None:
        switch (variance) {
        case VarianceKind::None:
        case VarianceKind::Contravariant:
          optAdjustment = OptionalAdjustmentKind::ConsumesUnhandledNil;
          break;

        case VarianceKind::Covariant:
          optAdjustment = OptionalAdjustmentKind::WillNeverProduceNil;
          break;
        }
        break;

      case OTK_Optional:
        // Exact match is always okay.
        break;

      case OTK_ImplicitlyUnwrappedOptional:
        optAdjustment = OptionalAdjustmentKind::IUOToOptional;
        break;
      }
      break;

    case OTK_ImplicitlyUnwrappedOptional:
      // When the requirement is an IUO, all is permitted, because we
      // assume that the user knows more about the signature than we
      // have information in the protocol.
      break;
    }
  }

  return std::make_tuple(reqtType, witnessType, optAdjustment);
}

// Given that we're looking at a stored property, should we use the
// mutating rules for the setter or the getter when trying to match
// the given requirement?
static bool shouldUseSetterRequirements(AccessorKind reqtKind) {
  // We have cases for addressors here because we might reasonably
  // allow them as protocol requirements someday.

  switch (reqtKind) {
  case AccessorKind::IsGetter:
  case AccessorKind::IsAddressor:
    return false;

  case AccessorKind::IsSetter:
  case AccessorKind::IsMutableAddressor:
  case AccessorKind::IsMaterializeForSet:
    return true;

  case AccessorKind::NotAccessor:
  case AccessorKind::IsWillSet:
  case AccessorKind::IsDidSet:
    llvm_unreachable("willSet/didSet protocol requirement?");
  }
  llvm_unreachable("bad accessor kind");
}

static FuncDecl *getAddressorForRequirement(AbstractStorageDecl *witness,
                                            AccessorKind reqtKind) {
  assert(witness->hasAddressors());
  if (shouldUseSetterRequirements(reqtKind))
    return witness->getMutableAddressor();
  return witness->getAddressor();
}

// Verify that the mutating bit is correct between a protocol requirement and a
// witness.  This returns true on invalid.
static bool checkMutating(FuncDecl *requirement, FuncDecl *witness,
                          ValueDecl *witnessDecl) {
  // Witnesses in classes never have mutating conflicts.
  if (auto contextType =
        witnessDecl->getDeclContext()->getDeclaredTypeInContext())
    if (contextType->hasReferenceSemantics())
      return false;
  
  // Determine whether the witness will be mutating or not.  If the witness is
  // stored property accessor, it may not be synthesized yet.
  bool witnessMutating;
  if (witness)
    witnessMutating = witness->isMutating();
  else {
    assert(requirement->isAccessor());
    auto storage = cast<AbstractStorageDecl>(witnessDecl);
    switch (storage->getStorageKind()) {

    // A stored property on a value type will have a mutating setter
    // and a non-mutating getter.
    case AbstractStorageDecl::Stored:
      witnessMutating = requirement->isInstanceMember() &&
        shouldUseSetterRequirements(requirement->getAccessorKind());
      break;

    // For an addressed property, consider the appropriate addressor.
    case AbstractStorageDecl::Addressed: {
      FuncDecl *addressor =
        getAddressorForRequirement(storage, requirement->getAccessorKind());
      witnessMutating = addressor->isMutating();
      break;
    }

    case AbstractStorageDecl::StoredWithObservers:
    case AbstractStorageDecl::StoredWithTrivialAccessors:
    case AbstractStorageDecl::InheritedWithObservers:
    case AbstractStorageDecl::AddressedWithTrivialAccessors:
    case AbstractStorageDecl::AddressedWithObservers:
    case AbstractStorageDecl::ComputedWithMutableAddress:
    case AbstractStorageDecl::Computed:
      llvm_unreachable("missing witness reference for kind with accessors");
    }
  }
  
  // If the requirement is for a nonmutating member, then the witness may not
  // mutate self.
  return !requirement->isMutating() && witnessMutating;
}

/// Check that the Objective-C method(s) provided by the witness have
/// the same selectors as those required by the requirement.
static bool checkObjCWitnessSelector(TypeChecker &tc, ValueDecl *req,
                                     ValueDecl *witness,
                                     bool complain) {
  // Simple case: for methods, check that the selectors match.
  if (auto reqFunc = dyn_cast<AbstractFunctionDecl>(req)) {
    auto witnessFunc = cast<AbstractFunctionDecl>(witness);
    if (reqFunc->getObjCSelector() == witnessFunc->getObjCSelector())
      return false;

    if (complain) {
      auto diagInfo = getObjCMethodDiagInfo(witnessFunc);
      tc.diagnose(witness, diag::objc_witness_selector_mismatch,
                  diagInfo.first, diagInfo.second,
                  witnessFunc->getObjCSelector(), reqFunc->getObjCSelector());
    }

    return true;
  }

  // Otherwise, we have an abstract storage declaration.
  auto reqStorage = cast<AbstractStorageDecl>(req);
  auto witnessStorage = cast<AbstractStorageDecl>(witness);

  // Check the getter.
  if (auto reqGetter = reqStorage->getGetter()) {
    if (checkObjCWitnessSelector(tc, reqGetter, witnessStorage->getGetter(),
                                 complain))
      return true;
  }

  // Check the setter.
  if (auto reqSetter = reqStorage->getSetter()) {
    if (checkObjCWitnessSelector(tc, reqSetter, witnessStorage->getSetter(),
                                 complain))
      return true;
  }

  return false;
}

/// \brief Match the given witness to the given requirement.
///
/// \returns the result of performing the match.
static RequirementMatch
matchWitness(TypeChecker &tc, NormalProtocolConformance *conformance,
             DeclContext *dc, ValueDecl *req, ValueDecl *witness,
             const std::function<
                     std::tuple<Optional<RequirementMatch>, Type, Type>(void)> 
               &setup,
             const std::function<Optional<RequirementMatch>(Type, Type)> 
               &matchTypes,
             const std::function<
                     RequirementMatch(bool, ArrayRef<OptionalAdjustment>)
                   > &finalize) {
  assert(!req->isInvalid() && "Cannot have an invalid requirement here");

  /// Make sure the witness is of the same kind as the requirement.
  if (req->getKind() != witness->getKind())
    return RequirementMatch(witness, MatchKind::KindConflict);

  // If the witness is invalid, record that and stop now.
  if (witness->isInvalid())
    return RequirementMatch(witness, MatchKind::WitnessInvalid);

  // Get the requirement and witness attributes.
  const auto &reqAttrs = req->getAttrs();
  const auto &witnessAttrs = witness->getAttrs();

  // Perform basic matching of the requirement and witness.
  bool decomposeFunctionType = false;
  bool ignoreReturnType = false;
  if (auto funcReq = dyn_cast<FuncDecl>(req)) {
    auto funcWitness = cast<FuncDecl>(witness);

    // Either both must be 'static' or neither.
    if (funcReq->isStatic() != funcWitness->isStatic())
      return RequirementMatch(witness, MatchKind::StaticNonStaticConflict);

    // If we require a prefix operator and the witness is not a prefix operator,
    // these don't match.
    if (reqAttrs.hasAttribute<PrefixAttr>() &&
        !witnessAttrs.hasAttribute<PrefixAttr>())
      return RequirementMatch(witness, MatchKind::PrefixNonPrefixConflict);

    // If we require a postfix operator and the witness is not a postfix
    // operator, these don't match.
    if (reqAttrs.hasAttribute<PostfixAttr>() &&
        !witnessAttrs.hasAttribute<PostfixAttr>())
      return RequirementMatch(witness, MatchKind::PostfixNonPostfixConflict);

    // Check that the mutating bit is ok.
    if (checkMutating(funcReq, funcWitness, funcWitness))
      return RequirementMatch(witness, MatchKind::MutatingConflict);

    /// If the requirement is @noreturn, the witness must also be @noreturn.
    if (reqAttrs.hasAttribute<NoReturnAttr>() &&
        !witnessAttrs.hasAttribute<NoReturnAttr>())
      return RequirementMatch(witness, MatchKind::NoReturnConflict);

    // If the requirement is rethrows, the witness must either be
    // rethrows or be non-throwing.
    if (reqAttrs.hasAttribute<RethrowsAttr>() &&
        !witnessAttrs.hasAttribute<RethrowsAttr>() &&
        cast<AbstractFunctionDecl>(witness)->isBodyThrowing())
      return RequirementMatch(witness, MatchKind::RethrowsConflict);

    // We want to decompose the parameters to handle them separately.
    decomposeFunctionType = true;
  } else if (auto *witnessASD = dyn_cast<AbstractStorageDecl>(witness)) {
    auto *reqASD = cast<AbstractStorageDecl>(req);
    
    // If this is a property requirement, check that the static-ness matches.
    if (auto *vdWitness = dyn_cast<VarDecl>(witness)) {
      if (cast<VarDecl>(req)->isStatic() != vdWitness->isStatic())
        return RequirementMatch(witness, MatchKind::StaticNonStaticConflict);
    }
    
    // If the requirement is settable and the witness is not, reject it.
    if (req->isSettable(req->getDeclContext()) &&
        !witness->isSettable(witness->getDeclContext()))
      return RequirementMatch(witness, MatchKind::SettableConflict);

    // Find a standin declaration to place the diagnostic at for the
    // given accessor kind.
    auto getStandinForAccessor = [&](AccessorKind kind) -> ValueDecl* {
      // If the witness actually explicitly provided that accessor,
      // then great.
      if (auto accessor = witnessASD->getAccessorFunction(kind))
        if (!accessor->isImplicit())
          return accessor;

      // If it didn't, check to see if it provides something else.
      if (witnessASD->hasAddressors()) {
        return getAddressorForRequirement(witnessASD, kind);
      }

      // Otherwise, just diagnose starting at the storage declaration
      // itself.
      return witnessASD;
    };
    
    // Validate that the 'mutating' bit lines up for getters and setters.
    if (checkMutating(reqASD->getGetter(), witnessASD->getGetter(),
                      witnessASD))
      return RequirementMatch(getStandinForAccessor(AccessorKind::IsGetter),
                              MatchKind::MutatingConflict);
    
    if (req->isSettable(req->getDeclContext()) &&
        checkMutating(reqASD->getSetter(), witnessASD->getSetter(), witnessASD))
      return RequirementMatch(getStandinForAccessor(AccessorKind::IsSetter),
                              MatchKind::MutatingConflict);

    // Decompose the parameters for subscript declarations.
    decomposeFunctionType = isa<SubscriptDecl>(req);
  } else if (isa<ConstructorDecl>(witness)) {
    decomposeFunctionType = true;
    ignoreReturnType = true;
  }

  // Objective-C checking for @objc requirements.
  if (req->isObjC()) {
    // The witness must also be @objc.
    if (!witness->isObjC())
      return RequirementMatch(witness, MatchKind::NotObjC);

    
    // The selectors must coincide.
    if (checkObjCWitnessSelector(tc, req, witness, /*complain=*/false))
      return RequirementMatch(witness, MatchKind::ObjCSelectorConflict);
  }

  // Set up the match, determining the requirement and witness types
  // in the process.
  Type reqType, witnessType;
  {
    Optional<RequirementMatch> result;
    std::tie(result, reqType, witnessType) = setup();
    if (result) {
      return *result;
    }
  }

  SmallVector<OptionalAdjustment, 2> optionalAdjustments;
  bool anyRenaming = false;
  if (decomposeFunctionType) {
    // Decompose function types into parameters and result type.
    auto reqFnType = reqType->castTo<AnyFunctionType>();
    auto reqInputType = reqFnType->getInput();
    auto reqResultType = reqFnType->getResult()->getRValueType();
    auto witnessFnType = witnessType->castTo<AnyFunctionType>();
    auto witnessInputType = witnessFnType->getInput();
    auto witnessResultType = witnessFnType->getResult()->getRValueType();

    // Result types must match.
    // FIXME: Could allow (trivial?) subtyping here.
    if (!ignoreReturnType) {
      auto types = getTypesToCompare(req, reqResultType, 
                                     witnessResultType,
                                     VarianceKind::Covariant);

      // Record optional adjustment, if any.
      if (std::get<2>(types) != OptionalAdjustmentKind::None) {
        optionalAdjustments.push_back(
          OptionalAdjustment(std::get<2>(types)));
      }

      if (auto result = matchTypes(std::get<0>(types), 
                                   std::get<1>(types))) {
        return *result;
      }
    }

    // Parameter types and kinds must match. Start by decomposing the input
    // types into sets of tuple elements.
    // Decompose the input types into parameters.
    auto reqParams = decomposeIntoTupleElements(reqInputType);
    auto witnessParams = decomposeIntoTupleElements(witnessInputType);

    // If the number of parameters doesn't match, we're done.
    if (reqParams.size() != witnessParams.size())
      return RequirementMatch(witness, MatchKind::TypeConflict, 
                              witnessType);

    // Match each of the parameters.
    for (unsigned i = 0, n = reqParams.size(); i != n; ++i) {
      // Variadic bits must match.
      // FIXME: Specialize the match failure kind
      if (reqParams[i].isVararg() != witnessParams[i].isVararg())
        return RequirementMatch(witness, MatchKind::TypeConflict, witnessType);

      // Check the parameter names.
      if (reqParams[i].getName() != witnessParams[i].getName()) {
        // A parameter has been renamed.
        anyRenaming = true;
      }

      // Gross hack: strip a level of unchecked-optionality off both
      // sides when matching against a protocol imported from Objective-C.
      auto types = getTypesToCompare(req, reqParams[i].getType(),
                                     witnessParams[i].getType(),
                                     VarianceKind::Contravariant);

      // Record any optional adjustment that occurred.
      if (std::get<2>(types) != OptionalAdjustmentKind::None) {
        optionalAdjustments.push_back(
          OptionalAdjustment(std::get<2>(types), i));
      }

      // Check whether the parameter types match.
      if (auto result = matchTypes(std::get<0>(types), 
                                   std::get<1>(types))) {
        return *result;
      }

      // FIXME: Consider default arguments here?
    }

    // If the witness is 'throws', the requirement must be.
    if (witnessFnType->getExtInfo().throws() &&
        !reqFnType->getExtInfo().throws()) {
      return RequirementMatch(witness, MatchKind::ThrowsConflict);
    }

  } else {
    // Simple case: add the constraint.
    auto types = getTypesToCompare(req, reqType, witnessType,
                                   VarianceKind::None);

    // Record optional adjustment, if any.
    if (std::get<2>(types) != OptionalAdjustmentKind::None) {
      optionalAdjustments.push_back(
        OptionalAdjustment(std::get<2>(types)));
    }

    if (auto result = matchTypes(std::get<0>(types), std::get<1>(types))) {
      return *result;
    }
  }

  // Now finalize the match.
  return finalize(anyRenaming, optionalAdjustments);
}

static RequirementMatch
matchWitness(ConformanceChecker &cc, TypeChecker &tc,
             NormalProtocolConformance *conformance,
             DeclContext *dc, ValueDecl *req, ValueDecl *witness) {
  using namespace constraints;

  // Initialized by the setup operation.
  Optional<ConstraintSystem> cs;
  ConstraintLocator *locator = nullptr;
  ConstraintLocator *witnessLocator = nullptr;
  Type witnessType, openWitnessType;
  Type openedFullWitnessType;
  Type reqType, openedFullReqType;
  
  // Set up the constraint system for matching.
  auto setup = [&]() -> std::tuple<Optional<RequirementMatch>, Type, Type> {
    // Construct a constraint system to use to solve the equality between
    // the required type and the witness type.
    cs.emplace(tc, dc, ConstraintSystemOptions());
    
    auto model = conformance->getType();

    // Open up the witness type.
    witnessType = witness->getInterfaceType();

    // FIXME: witness as a base locator?
    locator = cs->getConstraintLocator(nullptr);
    witnessLocator = cs->getConstraintLocator(
                       static_cast<Expr *>(nullptr),
                       LocatorPathElt(ConstraintLocator::Witness, witness));
    if (witness->getDeclContext()->isTypeContext()) {
      std::tie(openedFullWitnessType, openWitnessType) 
        = cs->getTypeOfMemberReference(model, witness,
                                      /*isTypeReference=*/false,
                                      /*isDynamicResult=*/false,
                                      witnessLocator,
                                      /*opener=*/nullptr);
    } else {
      std::tie(openedFullWitnessType, openWitnessType) 
        = cs->getTypeOfReference(witness,
                                /*isTypeReference=*/false,
                                /*isDynamicResult=*/false,
                                witnessLocator,
                                /*opener=*/nullptr);
    }
    openWitnessType = openWitnessType->getRValueType();
    
    // Open up the type of the requirement. We only truly open 'Self' and
    // its associated types (recursively); inner generic type parameters get
    // mapped to their archetypes directly.
    DeclContext *reqDC = req->getPotentialGenericDeclContext();
    RequirementTypeOpener reqTypeOpener(*cs, conformance, reqDC);
    std::tie(openedFullReqType, reqType)
      = cs->getTypeOfMemberReference(model, req,
                                     /*isTypeReference=*/false,
                                     /*isDynamicResult=*/false,
                                     locator,
                                     &reqTypeOpener);
    reqType = reqType->getRValueType();

    return std::make_tuple(None, reqType, openWitnessType);
  };

  // Match a type in the requirement to a type in the witness.
  auto matchTypes = [&](Type reqType, Type witnessType) 
                      -> Optional<RequirementMatch> {
    cs->addConstraint(ConstraintKind::Equal, reqType, witnessType, locator);
    // FIXME: Check whether this has already failed.
    return None;
  };

  // Finalize the match.
  auto finalize = [&](bool anyRenaming, 
                      ArrayRef<OptionalAdjustment> optionalAdjustments) 
                        -> RequirementMatch {
    // Try to solve the system.
    SmallVector<Solution, 1> solutions;
    if (cs->solve(solutions, FreeTypeVariableBinding::Allow)) {
      return RequirementMatch(witness, MatchKind::TypeConflict,
                              witnessType);
    }
    auto &solution = solutions.front();

    // Success. Form the match result.
    RequirementMatch result(witness,
                            hasAnyError(optionalAdjustments)
                              ? MatchKind::OptionalityConflict
                              : anyRenaming ? MatchKind::RenamedMatch
                                            : MatchKind::ExactMatch,
                            witnessType,
                            optionalAdjustments);

    if (openedFullWitnessType->hasTypeVariable()) {
      // Figure out the context we're substituting into.
      auto witnessDC = witness->getPotentialGenericDeclContext();
      
      // Compute the set of substitutions we'll need for the witness.
      solution.computeSubstitutions(witness->getInterfaceType(),
                                    witnessDC,
                                    openedFullWitnessType,
                                    witnessLocator,
                                    result.WitnessSubstitutions);
    }
    
    return result;
  };

  return matchWitness(tc, conformance, dc, req, witness, setup, matchTypes,
                      finalize);
}

/// \brief Determine whether one requirement match is better than the other.
static bool isBetterMatch(TypeChecker &tc, DeclContext *dc,
                          const RequirementMatch &match1,
                          const RequirementMatch &match2) {
  // Check whether one declaration is better than the other.
  switch (tc.compareDeclarations(dc, match1.Witness, match2.Witness)) {
  case Comparison::Better:
    return true;

  case Comparison::Worse:
    return false;

  case Comparison::Unordered:
    break;
  }

  // Earlier match kinds are better. This prefers exact matches over matches
  // that require renaming, for example.
  if (match1.Kind != match2.Kind)
    return static_cast<unsigned>(match1.Kind)
             < static_cast<unsigned>(match2.Kind);

  return false;
}

/// \brief Add the next associated type deduction to the string representation
/// of the deductions, used in diagnostics.
static void addAssocTypeDeductionString(llvm::SmallString<128> &str,
                                        AssociatedTypeDecl *assocType,
                                        Type deduced) {
  if (str.empty())
    str = " [with ";
  else
    str += ", ";

  str += assocType->getName().str();
  str += " = ";
  str += deduced.getString();
}

/// Clean up the given declaration type for display purposes.
static Type getTypeForDisplay(TypeChecker &tc, Module *module,
                              ValueDecl *decl) {
  // If we're not in a type context, just grab the interface type.
  Type type = decl->getInterfaceType();
  if (!decl->getDeclContext()->isTypeContext() ||
      !isa<AbstractFunctionDecl>(decl))
    return type;

  // For a constructor, we only care about the parameter types.
  if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    return ctor->getArgumentType();
  }

  // We have something function-like, so we want to strip off the 'self'.
  if (auto genericFn = type->getAs<GenericFunctionType>()) {
    if (auto resultFn = genericFn->getResult()->getAs<FunctionType>()) {
      // For generic functions, build a new generic function... but strip off
      // the requirements. They don't add value.
      auto sigWithoutReqts
        = GenericSignature::get(genericFn->getGenericParams(), {});
      return GenericFunctionType::get(sigWithoutReqts,
                                      resultFn->getInput(),
                                      resultFn->getResult(),
                                      resultFn->getExtInfo());
    }
  }

  return type->castTo<AnyFunctionType>()->getResult();
}

/// Clean up the given requirement type for display purposes.
static Type getRequirementTypeForDisplay(TypeChecker &tc, Module *module,
                                         NormalProtocolConformance *conformance,
                                         ValueDecl *req) {
  auto type = getTypeForDisplay(tc, module, req);

  // Replace generic type parameters and associated types with their
  // witnesses, when we have them.
  auto selfTy = GenericTypeParamType::get(0, 0, tc.Context);
  type = type.transform([&](Type type) -> Type {
    // If a dependent member refers to an associated type, replace it.
    if (auto member = type->getAs<DependentMemberType>()) {
      if (member->getBase()->isEqual(selfTy)) {
        // FIXME: Could handle inherited conformances here.
        if (conformance->hasTypeWitness(member->getAssocType()))
          return conformance->getTypeWitness(member->getAssocType(), nullptr)
                   .getReplacement();
      }
    }

    // Replace 'Self' with the conforming type type.
    if (type->isEqual(selfTy))
      return conformance->getType();

    return type;
  });

  //
  return type;
}

/// \brief Diagnose a requirement match, describing what went wrong (or not).
static void
diagnoseMatch(TypeChecker &tc, Module *module,
              NormalProtocolConformance *conformance, ValueDecl *req,
              const RequirementMatch &match){
  // Form a string describing the associated type deductions.
  // FIXME: Determine which associated types matter, and only print those.
  llvm::SmallString<128> withAssocTypes;
  for (auto member : conformance->getProtocol()->getMembers()) {
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
      if (conformance->usesDefaultDefinition(assocType)) {
        auto &witness = conformance->getTypeWitness(assocType, nullptr);
        addAssocTypeDeductionString(withAssocTypes, assocType,
                                    witness.getReplacement());
      }
    }
  }
  if (!withAssocTypes.empty())
    withAssocTypes += "]";

  switch (match.Kind) {
  case MatchKind::ExactMatch:
    tc.diagnose(match.Witness, diag::protocol_witness_exact_match,
                withAssocTypes);
    break;

  case MatchKind::RenamedMatch:
    tc.diagnose(match.Witness, diag::protocol_witness_renamed, withAssocTypes);
    break;

  case MatchKind::KindConflict:
    tc.diagnose(match.Witness, diag::protocol_witness_kind_conflict,
                getRequirementKind(req));
    break;

  case MatchKind::WitnessInvalid:
    // Don't bother to diagnose invalid witnesses; we've already complained
    // about them.
    break;

  case MatchKind::TypeConflict:
    tc.diagnose(match.Witness, diag::protocol_witness_type_conflict,
                getTypeForDisplay(tc, module, match.Witness),
                withAssocTypes);
    break;

  case MatchKind::ThrowsConflict:
    tc.diagnose(match.Witness, diag::protocol_witness_throws_conflict);
    break;

  case MatchKind::OptionalityConflict: {
    auto diag = tc.diagnose(match.Witness, 
                            diag::protocol_witness_optionality_conflict,
                            match.classifyOptionalityIssues(req),
                            withAssocTypes);
    match.addOptionalityFixIts(tc.Context, match.Witness, diag);
    break;
  }

  case MatchKind::StaticNonStaticConflict:
    // FIXME: Could emit a Fix-It here.
    tc.diagnose(match.Witness, diag::protocol_witness_static_conflict,
                !req->isInstanceMember());
    break;
      
  case MatchKind::SettableConflict:
    tc.diagnose(match.Witness, diag::protocol_witness_settable_conflict);
    break;

  case MatchKind::PrefixNonPrefixConflict:
    // FIXME: Could emit a Fix-It here.
    tc.diagnose(match.Witness, diag::protocol_witness_prefix_postfix_conflict,
                false,
                match.Witness->getAttrs().hasAttribute<PostfixAttr>() ? 2 : 0);
    break;

  case MatchKind::PostfixNonPostfixConflict:
    // FIXME: Could emit a Fix-It here.
    tc.diagnose(match.Witness, diag::protocol_witness_prefix_postfix_conflict,
                true,
                match.Witness->getAttrs().hasAttribute<PrefixAttr>() ? 1 : 0);
    break;
  case MatchKind::MutatingConflict:
    // FIXME: Could emit a Fix-It here.
    tc.diagnose(match.Witness, diag::protocol_witness_mutating_conflict);
    break;
  case MatchKind::NoReturnConflict:
    // FIXME: Could emit a Fix-It here.
    tc.diagnose(match.Witness, diag::protocol_witness_noreturn_conflict);
    break;
  case MatchKind::RethrowsConflict:
    // FIXME: Could emit a Fix-It here.
    tc.diagnose(match.Witness, diag::protocol_witness_rethrows_conflict);
    break;
  case MatchKind::ObjCSelectorConflict:
    (void)checkObjCWitnessSelector(tc, req, match.Witness, /*complain=*/true);
    break;
  case MatchKind::NotObjC: {
    SourceLoc witnessStartLoc = match.Witness->getStartLoc();
    SourceLoc attrStartLoc = match.Witness->getAttrs().getStartLoc();
    if (attrStartLoc.isValid())
      witnessStartLoc = attrStartLoc;

    if (auto varWitness = dyn_cast<VarDecl>(match.Witness)) {
      if (auto patternBinding = varWitness->getParentPatternBinding())
        witnessStartLoc = patternBinding->getStartLoc();
    }
    tc.diagnose(match.Witness, diag::protocol_witness_not_objc)
      .fixItInsert(witnessStartLoc, "@objc ");
    break;
  }
  }
}

/// Compute the substitution for the given archetype and its replacement
/// type.
static Substitution getArchetypeSubstitution(TypeChecker &tc,
                                             DeclContext *dc,
                                             ArchetypeType *archetype,
                                             Type replacement) {
  ArchetypeType *resultArchetype = archetype;
  Type resultReplacement = replacement;
  assert(!resultReplacement->isDependentType() && "Can't be dependent");
  SmallVector<ProtocolConformance *, 4> conformances;

  bool isError = replacement->is<ErrorType>();
  for (auto proto : archetype->getConformsTo()) {
    ProtocolConformance *conformance = nullptr;
    bool conforms = tc.conformsToProtocol(replacement, proto, dc, false,
                                          &conformance);
    assert((conforms || isError) &&
           "Conformance should already have been verified");
    (void)conforms;
    conformances.push_back(conformance);
  }

  return Substitution{
    resultArchetype,
    resultReplacement,
    tc.Context.AllocateCopy(conformances),
  };
}

/// If the given type is a direct reference to an associated type of
/// the given protocol, return the referenced associated type.
static AssociatedTypeDecl *
getReferencedAssocTypeOfProtocol(Type type, ProtocolDecl *proto) {
  if (auto dependentMember = type->getAs<DependentMemberType>()) {
    if (auto genericParam 
          = dependentMember->getBase()->getAs<GenericTypeParamType>()) {
      if (genericParam->getDepth() == 0 && genericParam->getIndex() == 0) {
        if (auto assocType = dependentMember->getAssocType()) {
          if (assocType->getDeclContext() == proto)
            return assocType;
        }
      }
    }
  }

  return nullptr;
}

ArrayRef<AssociatedTypeDecl *> 
ConformanceChecker::getReferencedAssociatedTypes(ValueDecl *req) {
  // Check whether we've already cached this information.
  auto known = ReferencedAssociatedTypes.find(req);
  if (known != ReferencedAssociatedTypes.end())
    return known->second;

  // Collect the set of associated types rooted on Self in the
  // signature.
  auto &assocTypes = ReferencedAssociatedTypes[req];
  llvm::SmallPtrSet<AssociatedTypeDecl *, 4> knownAssocTypes;
  req->getInterfaceType().visit([&](Type type) {
      if (auto assocType = getReferencedAssocTypeOfProtocol(type, Proto)) {
        if (knownAssocTypes.insert(assocType).second) {
          assocTypes.push_back(assocType);
        }
      }
    });

  return assocTypes;
}

void ConformanceChecker::recordWitness(ValueDecl *requirement,
                                       const RequirementMatch &match) {
  // If we already recorded this witness, don't do so again.
  if (Conformance->hasWitness(requirement)) {
    assert(Conformance->getWitness(requirement, nullptr).getDecl() ==
             match.Witness && "Deduced different witnesses?");
    return;
  }

  if (!TC.getLangOpts().DisableAvailabilityChecking &&
      !TC.isAvailabilitySafeForOverride(match.Witness, requirement)) {
    auto witness = match.Witness;
    diagnoseOrDefer(requirement, false,
      [witness, requirement](TypeChecker &tc,
                             NormalProtocolConformance *conformance) {
        tc.diagnose(witness,
                    diag::availability_declaration_less_available_than_protocol,
                    witness->getFullName());
        tc.diagnose(requirement, diag::availability_protocol_requirement_here);
      });
  }

  // Record this witness in the conformance.
  ConcreteDeclRef witness;
  if (match.WitnessSubstitutions.empty())
    witness = match.Witness;
  else
    witness = ConcreteDeclRef(TC.Context, match.Witness,
                              match.WitnessSubstitutions);
  Conformance->setWitness(requirement, witness);

  // Synthesize accessors for the protocol witness table to use.
  if (auto storage = dyn_cast<AbstractStorageDecl>(witness.getDecl()))
    TC.synthesizeWitnessAccessorsForStorage(
                                        cast<AbstractStorageDecl>(requirement),
                                        storage);
}

void ConformanceChecker::recordOptionalWitness(ValueDecl *requirement) {
  // If we already recorded this witness, don't do so again.
  if (Conformance->hasWitness(requirement)) {
    assert(!Conformance->getWitness(requirement, nullptr).getDecl() &&
           "Already have a non-optional witness?");
    return;
  }

  // Record that there is no witness.
  Conformance->setWitness(requirement, ConcreteDeclRef());

  // If the requirement is @objc, note that we have an unsatisfied
  // optional @objc requirement.
  if (requirement->isObjC()) {
    if (auto funcReq = dyn_cast<AbstractFunctionDecl>(requirement))
      TC.Context.recordObjCUnsatisfiedOptReq(DC, funcReq);
    else {
      auto storageReq = cast<AbstractStorageDecl>(requirement);
      if (auto getter = storageReq->getGetter())
        TC.Context.recordObjCUnsatisfiedOptReq(DC, getter);
      if (auto setter = storageReq->getSetter())
        TC.Context.recordObjCUnsatisfiedOptReq(DC, setter);
    }
  }
}

void ConformanceChecker::recordTypeWitness(AssociatedTypeDecl *assocType,
                                           Type type,
                                           TypeDecl *typeDecl,
                                           DeclContext *fromDC,
                                           bool wasDeducedOrDefaulted,
                                           bool performRedeclarationCheck) {
  // If the declaration context from which the type witness was determined
  // differs from that of the conformance, adjust the type so that it is
  // based on the declaration context of the conformance.
  if (fromDC != DC && DC->getGenericSignatureOfContext() &&
      fromDC->getGenericSignatureOfContext() && !isa<ProtocolDecl>(fromDC)) {
    // Map the type to an interface type.
    type = TC.getInterfaceTypeFromInternalType(fromDC, type);

    // Map the type into the conformance's context.
    type = Adoptee->getTypeOfMember(DC->getParentModule(), type, fromDC);
  }

  // If we already recoded this type witness, there's nothing to do.
  if (Conformance->hasTypeWitness(assocType)) {
    assert(Conformance->getTypeWitness(assocType, nullptr).getReplacement()
             ->isEqual(type) && "Conflicting type witness deductions");
    return;
  }

  // If there was no type declaration, synthesize one.
  if (!typeDecl) {
    // If we're just setting an error, double-check that nobody has
    // introduced a type declaration since we deduced one. This can
    // happen when type-checking a different conformance deduces a
    // different type witness with the same name. For non-error cases,
    // the caller handles this.
    if (performRedeclarationCheck && type->is<ErrorType>()) {
      switch (resolveTypeWitnessViaLookup(assocType)) {
      case ResolveWitnessResult::Success:
      case ResolveWitnessResult::ExplicitFailed:
        // A type witness has shown up, and will have been
        // recorded. There is nothing more to do.
        return;

      case ResolveWitnessResult::Missing:
        // The type witness is still missing: create a new one.
        break;
      }
    }

    auto aliasDecl = new (TC.Context) TypeAliasDecl(SourceLoc(),
                                                    assocType->getName(),
                                                    SourceLoc(),
                                                    TypeLoc::withoutLoc(type),
                                                    DC);
    Type metaType = MetatypeType::get(type);
    aliasDecl->setImplicit();
    if (type->is<ErrorType>())
      aliasDecl->setInvalid();
    if (metaType->hasArchetype()) {
      aliasDecl->setInterfaceType(
        TC.getInterfaceTypeFromInternalType(DC, metaType));
    }
    auto nominal = DC->getDeclaredTypeOfContext()->getAnyNominal();
    TC.computeAccessibility(nominal);
    aliasDecl->setAccessibility(nominal->getFormalAccess());
    if (nominal == DC) {
      nominal->addMember(aliasDecl);
    } else {
      auto ext = cast<ExtensionDecl>(DC);
      ext->addMember(aliasDecl);
    }

    typeDecl = aliasDecl;
  }

  // Record the type witness.
  Conformance->setTypeWitness(
    assocType,
    getArchetypeSubstitution(TC, DC, assocType->getArchetype(), type),
    typeDecl);

  // Note whether this witness was deduced or defaulted.
  if (wasDeducedOrDefaulted)
    Conformance->addDefaultDefinition(assocType);
}

namespace {
  /// Describes whether a requirement refers to 'Self', for use in the
  /// is-inheritable check.
  enum class SelfReferenceKind {
    /// The type does not refer to 'Self' at all.
    No,
    /// The type refers to 'Self', but only as the result type of a method.
    Result,
    /// The type refers to 'Self' in some position that is not the result type
    /// of a method.
    Yes
  };
}

/// Determine whether the given type is the 'Self' generic parameter
/// of a protocol.
static bool isSelf(Type type) {
  if (auto genericParam = type->getAs<GenericTypeParamType>()) {
    return genericParam->getDepth() == 0 && genericParam->getIndex() == 0;
  }

  return false;
}

/// Determine whether the given type is the 'Self' generic parameter of a
/// protocol or a (possibly implicitly unwrapped) optional thereof.
static bool isSelfOrOptionalSelf(Type type) {
  if (auto optType = type->getAnyOptionalObjectType())
    type = optType;
  return isSelf(type);
}

/// Determine whether the given type contains a reference to the
/// 'Self' generic parameter of a protocol that is not the base of a
/// dependent member expression.
static bool containsSelf(Type type) {
  struct SelfWalker : public TypeWalker {
    bool FoundSelf = false;

    virtual Action walkToTypePre(Type ty) { 
      // If we found a reference to 'Self', note it and stop.
      if (isSelf(ty)) {
        FoundSelf = true;
        return Action::Stop;
      }

      // Don't recurse into the base of a dependent member type: it
      // doesn't contain a bare 'Self'.
      if (ty->is<DependentMemberType>())
        return Action::SkipChildren;

      return Action::Continue; 
    }
  } selfWalker;

  type.walk(selfWalker);
  return selfWalker.FoundSelf;
}

/// Determine whether the given parameter type involves Self in a manner that
/// is not contravariant.
static bool isNonContravariantSelfParamType(Type type) {
  // 'Self' or an optional thereof will be contravariant in overrides.
  if (isSelfOrOptionalSelf(type))
    return false;

  // Decompose tuples.
  if (auto tuple = type->getAs<TupleType>()) {
    for (auto &elt: tuple->getElements()) {
      if (isNonContravariantSelfParamType(elt.getType()))
        return true;
    }

    return false;
  } 

  // Look into the input type of parameters.
  if (auto funcTy = type->getAs<AnyFunctionType>()) {
    if (isNonContravariantSelfParamType(funcTy->getInput()))
      return true;

    return containsSelf(funcTy->getResult());
  }

  // If the parameter contains Self, it is not contravariant.
  return containsSelf(type);
}

namespace {
  /// Describes how we should check for Self in the result type of a function.
  enum class SelfInResultType {
    /// Check for the Self type normally.
    Check,
    /// Ignore Self in the result type.
    Ignore,
    /// The result type is known to be a dynamic Self.
    DynamicSelf,
  };

}
/// Find references to Self within the given function type.
static SelfReferenceKind findSelfReferences(const AnyFunctionType *fnType,
                                            SelfInResultType inResultType) {
  // Check whether the input type contains Self in any position where it would
  // make an override not have contravariant parameter types.
  if (isNonContravariantSelfParamType(fnType->getInput()))
    return SelfReferenceKind::Yes;

  // Consider the result type.
  auto type = fnType->getResult();
  switch (inResultType) {
  case SelfInResultType::DynamicSelf:
    return SelfReferenceKind::Result;

  case SelfInResultType::Check:
    return isSelfOrOptionalSelf(type)
             ? SelfReferenceKind::Result
             : containsSelf(type) ? SelfReferenceKind::Yes
                                  : SelfReferenceKind::No;

  case SelfInResultType::Ignore:
    return SelfReferenceKind::No;
  }
}

/// Find the bare Self references within the given requirement.
static SelfReferenceKind findSelfReferences(ValueDecl *value) {
  // Types never refer to 'Self'.
  if (isa<TypeDecl>(value))
    return SelfReferenceKind::No;

  // If the function requirement returns Self and has no other
  // reference to Self, note that.
  if (auto afd = dyn_cast<AbstractFunctionDecl>(value)) {
    auto type = afd->getInterfaceType();

    // Skip the 'self' type.
    type = type->castTo<AnyFunctionType>()->getResult();

    // Check first input types. Any further input types are treated as part of
    // the result type.
    auto fnType = type->castTo<AnyFunctionType>();
    return findSelfReferences(fnType,
                              isa<ConstructorDecl>(afd)
                                ? SelfInResultType::Ignore
                                : (isa<FuncDecl>(afd) &&
                                   cast<FuncDecl>(afd)->hasDynamicSelf())
                                  ? SelfInResultType::DynamicSelf
                                  : SelfInResultType::Check);
  }

  auto type = value->getInterfaceType();

  if (isa<SubscriptDecl>(value)) {
    auto fnType = type->castTo<AnyFunctionType>();
    return findSelfReferences(fnType, SelfInResultType::Check);
  }

  if (isa<VarDecl>(value)) {
    type = type->getRValueType();
    return isSelfOrOptionalSelf(type)
             ? SelfReferenceKind::Result
             : containsSelf(type) ? SelfReferenceKind::Yes
                                  : SelfReferenceKind::No;

  }

  return containsSelf(type) ? SelfReferenceKind::Yes
                            : SelfReferenceKind::No;
}

SmallVector<ValueDecl *, 4> 
ConformanceChecker::lookupValueWitnesses(ValueDecl *req, bool *ignoringNames) {
  assert(!isa<AssociatedTypeDecl>(req) && "Not for lookup for type witnesses*");
  
  SmallVector<ValueDecl *, 4> witnesses;
  if (req->getName().isOperator()) {
    // Operator lookup is always global.
    UnqualifiedLookup lookup(req->getName(),
                             DC->getModuleScopeContext(),
                             &TC,
                             !DC->isCascadingContextForLookup(false));

    if (lookup.isSuccess()) {
      for (auto candidate : lookup.Results) {
        assert(candidate.hasValueDecl());
        witnesses.push_back(candidate.getValueDecl());
      }
    }
  } else {
    // Variable/function/subscript requirements.
    auto metaType = MetatypeType::get(Adoptee);
    auto candidates = TC.lookupMember(metaType, req->getFullName(), DC);

    // If we didn't find anything with the appropriate name, look
    // again using only the base name.
    if (candidates.empty() && ignoringNames) {
      candidates = TC.lookupMember(metaType, req->getName(), DC);
      *ignoringNames = true;
    }

    for (auto candidate : candidates) {
      witnesses.push_back(candidate);
    }
  }

  return witnesses;
}

ResolveWitnessResult
ConformanceChecker::resolveWitnessViaLookup(ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Use resolveTypeWitnessVia*");

  // Resolve all associated types before trying to resolve this witness.
  resolveTypeWitnesses();

  // If any of the type witnesses was erroneous, don't bother to check
  // this value witness: it will fail.
  for (auto assocType : getReferencedAssociatedTypes(requirement)) {
    if (Conformance->getTypeWitness(assocType, nullptr).getReplacement()
          ->is<ErrorType>()) {
      return ResolveWitnessResult::ExplicitFailed;
    }
  }

  // Determine whether we can derive this conformance.
  // FIXME: Hoist this computation out of here.
  bool canDerive = false;
  if (auto *nominal = Adoptee->getAnyNominal()) {
    if (nominal->derivesProtocolConformance(Proto))
      canDerive = true;
  }

  // Gather the witnesses.
  bool ignoringNames = false;
  auto witnesses = lookupValueWitnesses(requirement, 
                                        canDerive ? nullptr : &ignoringNames);

  // Match each of the witnesses to the requirement.
  SmallVector<RequirementMatch, 4> matches;
  unsigned numViable = 0;
  unsigned bestIdx = 0;
  bool invalidWitness = false;
  bool didDerive = false;
  for (auto witness : witnesses) {
    // Don't match anything in a protocol.
    // FIXME: When default implementations come along, we can try to match
    // these when they're default implementations coming from another
    // (unrelated) protocol.
    if (isa<ProtocolDecl>(witness->getDeclContext())) {
      continue;
    }

    if (!witness->hasType())
      TC.validateDecl(witness, true);

    auto match = matchWitness(*this, TC, Conformance, DC, requirement, witness);
    if (match.isViable()) {
      ++numViable;
      bestIdx = matches.size();
    } else if (match.Kind == MatchKind::WitnessInvalid) {
      invalidWitness = true;
    }

    matches.push_back(std::move(match));
  }

  // If there are any viable matches, try to find the best.
  if (numViable >= 1) {
    // If there numerous viable matches, throw out the non-viable matches
    // and try to find a "best" match.
    bool isReallyBest = true;
    if (numViable > 1) {
      matches.erase(std::remove_if(matches.begin(), matches.end(),
                                   [](const RequirementMatch &match) {
                                     return !match.isViable();
                                   }),
                    matches.end());

      // Find the best match.
      bestIdx = 0;
      for (unsigned i = 1, n = matches.size(); i != n; ++i) {
        if (isBetterMatch(TC, DC, matches[i], matches[bestIdx]))
          bestIdx = i;
      }

      // Make sure it is, in fact, the best.
      for (unsigned i = 0, n = matches.size(); i != n; ++i) {
        if (i == bestIdx)
          continue;

        if (!isBetterMatch(TC, DC, matches[bestIdx], matches[i])) {
          isReallyBest = false;
          break;
        }
      }
    }

    // If we really do have a best match, record it.
    if (isReallyBest) {
      auto &best = matches[bestIdx];
      auto witness = best.Witness;

      // If the name didn't actually line up, complain.
      if (ignoringNames && 
          requirement->getFullName() != best.Witness->getFullName()) {
        diagnoseOrDefer(requirement, false,
          [witness, requirement](TypeChecker &tc,
                                 NormalProtocolConformance *conformance) {
            auto proto = conformance->getProtocol();
            {
              auto diag = tc.diagnose(witness,
                                      diag::witness_argument_name_mismatch,
                                      isa<ConstructorDecl>(witness),
                                      witness->getFullName(),
                                      proto->getDeclaredType(),
                                      requirement->getFullName());
              tc.fixAbstractFunctionNames(diag,
                                          cast<AbstractFunctionDecl>(witness),
                                          requirement->getFullName());
            }

            tc.diagnose(requirement, diag::protocol_requirement_here,
                        requirement->getFullName());

          });
      }

      // If the optionality didn't line up, complain.
      if (!best.OptionalAdjustments.empty()) {
        diagnoseOrDefer(requirement, false,
          [witness, best, requirement](TypeChecker &tc,
                                       NormalProtocolConformance *conformance) {
            auto proto = conformance->getProtocol();
            {
              auto diag = tc.diagnose(
                            witness,
                            hasAnyError(best.OptionalAdjustments)
                              ? diag::err_protocol_witness_optionality
                              : diag::warn_protocol_witness_optionality,
                            best.classifyOptionalityIssues(requirement),
                            witness->getFullName(),
                            proto->getFullName());
              best.addOptionalityFixIts(tc.Context, witness, diag);
            }

            tc.diagnose(requirement, diag::protocol_requirement_here,
                        requirement->getFullName());
        });
      }

      // If the match isn't accessible enough, complain.
      // FIXME: Handle "private(set)" requirements.
      Accessibility requiredAccess =
        std::min(Proto->getFormalAccess(),
                 Adoptee->getAnyNominal()->getFormalAccess());
      bool shouldDiagnose = false;
      bool shouldDiagnoseSetter = false;
      if (requiredAccess > Accessibility::Private) {
        shouldDiagnose = (best.Witness->getFormalAccess() < requiredAccess);

        if (!shouldDiagnose && requirement->isSettable(DC)) {
          auto ASD = cast<AbstractStorageDecl>(best.Witness);
          const DeclContext *accessDC = nullptr;
          if (requiredAccess == Accessibility::Internal)
            accessDC = DC->getParentModule();
          shouldDiagnoseSetter = !ASD->isSetterAccessibleFrom(accessDC);
        }
      }
      if (shouldDiagnose || shouldDiagnoseSetter) {
        diagnoseOrDefer(requirement, false,
          [witness, requiredAccess, requirement, shouldDiagnoseSetter](
            TypeChecker &tc, NormalProtocolConformance *conformance) {
          auto proto = conformance->getProtocol();
          bool protoForcesAccess = (requiredAccess == proto->getFormalAccess());
          auto diagKind = protoForcesAccess
                            ? diag::witness_not_accessible_proto
                            : diag::witness_not_accessible_type;
            auto diag = tc.diagnose(witness, diagKind,
                                    getRequirementKind(requirement),
                                    witness->getFullName(),
                                    shouldDiagnoseSetter,
                                    requiredAccess,
                                    proto->getName());
            fixItAccessibility(diag, witness, requiredAccess,
                               shouldDiagnoseSetter);
          });
      }

      if (auto ctor = dyn_cast<ConstructorDecl>(requirement)) {
        // If we have an initializer requirement and the conforming type
        // is a non-final class, the witness must be 'required'.
        // We exempt Objective-C initializers from this requirement
        // because there is no equivalent to 'required' in Objective-C.
        ClassDecl *classDecl = nullptr;
        auto witnessCtor = cast<ConstructorDecl>(best.Witness);
        if (((classDecl = Adoptee->getClassOrBoundGenericClass()) &&
             !classDecl->isFinal()) &&
            !witnessCtor->isRequired() &&
            !witnessCtor->hasClangNode()) {
          // FIXME: We're not recovering (in the AST), so the Fix-It
          // should move.
          diagnoseOrDefer(requirement, false,
            [witness, requirement](
               TypeChecker &tc, NormalProtocolConformance *conformance) {
              bool inExtension = isa<ExtensionDecl>(witness->getDeclContext());
              auto diag = tc.diagnose(witness->getLoc(),
                                      diag::witness_initializer_not_required,
                                      requirement->getFullName(), inExtension,
                                      conformance->getType());
              if (!witness->isImplicit() && !inExtension)
                diag.fixItInsert(witness->getStartLoc(), "required ");
            });
        }

        // An non-failable initializer requirement cannot be satisfied
        // by a failable initializer.
        if (ctor->getFailability() == OTK_None) {
          switch (witnessCtor->getFailability()) {
          case OTK_None:
            // Okay
            break;

          case OTK_ImplicitlyUnwrappedOptional:
            // Only allowed for non-@objc protocols.
            if (!Proto->isObjC())
              break;

            SWIFT_FALLTHROUGH;

          case OTK_Optional:
            diagnoseOrDefer(requirement, false,
              [witness, requirement](TypeChecker &tc,
                                     NormalProtocolConformance *conformance) {
                auto ctor = cast<ConstructorDecl>(requirement);
                auto witnessCtor = cast<ConstructorDecl>(witness);
                tc.diagnose(witness->getLoc(),
                            diag::witness_initializer_failability,
                            ctor->getFullName(),
                            witnessCtor->getFailability()
                              == OTK_ImplicitlyUnwrappedOptional)
                  .highlight(witnessCtor->getFailabilityLoc());
              });
            break;
          }
        }
      }

      // Check whether this requirement uses Self in a way that might
      // prevent conformance from succeeding.
      switch (findSelfReferences(requirement)) {
      case SelfReferenceKind::No:
        // No references to Self: nothing more to do.
        break;

      case SelfReferenceKind::Yes: {
        // References to Self in a position where subclasses cannot do
        // the right thing. Complain if the adoptee is a non-final
        // class.
        ClassDecl *classDecl = nullptr;
        if ((classDecl = Adoptee->getClassOrBoundGenericClass()) &&
            !classDecl->isFinal()) {
          diagnoseOrDefer(requirement, false,
            [witness, requirement](TypeChecker &tc,
                                   NormalProtocolConformance *conformance) {
              auto proto = conformance->getProtocol();
              tc.diagnose(witness->getLoc(), diag::witness_self_non_subtype,
                          proto->getDeclaredType(), requirement->getFullName(),
                          conformance->getType());
            });
        }
        break;
      }

      case SelfReferenceKind::Result: {
        // The reference to Self occurs in the result type. A non-final class 
        // can satisfy this requirement with a method that returns Self.
        ClassDecl *classDecl = nullptr;
        if ((classDecl = Adoptee->getClassOrBoundGenericClass()) &&
            !classDecl->isFinal()) {
          if (auto func = dyn_cast<FuncDecl>(best.Witness)) {
            // If the function has a dynamic Self, it's okay.
            if (func->hasDynamicSelf())
              break;

            diagnoseOrDefer(requirement, false,
              [witness, requirement](TypeChecker &tc,
                                     NormalProtocolConformance *conformance) {
                auto proto = conformance->getProtocol();
                tc.diagnose(witness->getLoc(),
                            diag::witness_requires_dynamic_self,
                            requirement->getFullName(), conformance->getType(),
                            proto->getDeclaredType());
              });
            break;
          }

          diagnoseOrDefer(requirement, false,
            [witness, requirement](TypeChecker &tc,
                                   NormalProtocolConformance *conformance) {
              auto proto = conformance->getProtocol();

              tc.diagnose(witness->getLoc(), diag::witness_self_non_subtype,
                          proto->getDeclaredType(), requirement->getFullName(),
                          conformance->getType());
            });
          break;
        }
        
        break;
      }
      }

      // Record the match.
      recordWitness(requirement, best);
      return ResolveWitnessResult::Success;
    }

    // We have an ambiguity; diagnose it below.
  }

  // We have either no matches or an ambiguous match.

  // If we can derive a definition for this requirement, just call it missing.
  if (canDerive) {
    return ResolveWitnessResult::Missing;
  }

  // If the requirement is optional, it's okay. We'll satisfy this via
  // our handling of default definitions.
  // FIXME: also check for a default definition here.
  //
  // Treat 'unavailable' implicitly as if it were 'optional'.
  // The compiler will reject actual uses.
  auto Attrs = requirement->getAttrs();
  if (Attrs.hasAttribute<OptionalAttr>() || Attrs.isUnavailable(TC.Context)) {
    return ResolveWitnessResult::Missing;
  }

  // Diagnose the error.    

  // If there was an invalid witness that might have worked, just
  // suppress the diagnostic entirely. This stops the diagnostic cascade.
  // FIXME: We could do something crazy, like try to fix up the witness.
  if (invalidWitness) {
    return ResolveWitnessResult::ExplicitFailed;
  }

  diagnoseOrDefer(requirement, true,
    [requirement, matches, ignoringNames, numViable, didDerive](
      TypeChecker &tc, NormalProtocolConformance *conformance) {
      auto dc = conformance->getDeclContext();

      // Determine the type that the requirement is expected to have.
      Type reqType = getRequirementTypeForDisplay(tc, dc->getParentModule(),
                                                  conformance, requirement);

      // Point out the requirement that wasn't met.
      tc.diagnose(requirement,
                  numViable > 0 ? (ignoringNames
                                   ? diag::ambiguous_witnesses_wrong_name
                                   : diag::ambiguous_witnesses)
                                : diag::no_witnesses,
                  getRequirementKind(requirement),
                  requirement->getFullName(),
                  reqType);

      if (!didDerive) {
        // Diagnose each of the matches.
        for (const auto &match : matches)
          diagnoseMatch(tc, dc->getParentModule(), conformance, requirement,
                        match);
      }
    });


  // FIXME: Suggest a new declaration that does match?

  return ResolveWitnessResult::ExplicitFailed;
}

/// Attempt to resolve a witness via derivation.
ResolveWitnessResult ConformanceChecker::resolveWitnessViaDerivation(
                       ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Use resolveTypeWitnessVia*");

  // Find the declaration that derives the protocol conformance.
  NominalTypeDecl *derivingTypeDecl = nullptr;
  if (auto *nominal = Adoptee->getAnyNominal()) {
    if (nominal->derivesProtocolConformance(Proto))
      derivingTypeDecl = nominal;
  }

  if (!derivingTypeDecl) {
    return ResolveWitnessResult::Missing;
  }

  // Attempt to derive the witness.
  auto derived = TC.deriveProtocolRequirement(derivingTypeDecl, requirement);
  if (!derived)
    return ResolveWitnessResult::ExplicitFailed;

  // Try to match the derived requirement.
  auto match = matchWitness(*this, TC, Conformance, DC, requirement, derived);
  if (match.isViable()) {
    recordWitness(requirement, match);
    return ResolveWitnessResult::Success;
  }

  // Derivation failed.
  diagnoseOrDefer(requirement, true,
    [](TypeChecker &tc, NormalProtocolConformance *conformance) {
      auto proto = conformance->getProtocol();
      tc.diagnose(conformance->getLoc(), diag::protocol_derivation_is_broken,
                  proto->getDeclaredType(), conformance->getType());
    });

  return ResolveWitnessResult::ExplicitFailed;
}

ResolveWitnessResult ConformanceChecker::resolveWitnessViaDefault(
                       ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Use resolveTypeWitnessVia*");

  // An optional requirement is trivially satisfied with an empty requirement.
  // An 'unavailable' requirement is treated like optional requirements.
  auto Attrs = requirement->getAttrs();
  if (Attrs.hasAttribute<OptionalAttr>() || Attrs.isUnavailable(TC.Context)) {
    recordOptionalWitness(requirement);
    return ResolveWitnessResult::Success;
  }

  // FIXME: Default definition.

  diagnoseOrDefer(requirement, true,
    [requirement](TypeChecker &tc, NormalProtocolConformance *conformance) {
    auto dc = conformance->getDeclContext();
    // Determine the type that the requirement is expected to have.
    Type reqType = getRequirementTypeForDisplay(tc, dc->getParentModule(),
                                                conformance, requirement);

    // Point out the requirement that wasn't met.
    tc.diagnose(requirement, diag::no_witnesses,
                getRequirementKind(requirement),
                requirement->getName(),
                reqType);
    });

  return ResolveWitnessResult::ExplicitFailed;
}

# pragma mark Type witness resolution

/// Check whether the given type witness can be used for the given
/// associated type.
///
/// \returns an empty result on success, or a description of the error.
static CheckTypeWitnessResult checkTypeWitness(TypeChecker &tc, DeclContext *dc,
                                               AssociatedTypeDecl *assocType, 
                                               Type type) {
  // FIXME: Check class requirement.

  // Check protocol conformances.
  for (auto reqProto : assocType->getConformingProtocols(&tc)) {
    if (!tc.conformsToProtocol(type, reqProto, dc, false))
      return reqProto;
  }

  // Success!
  return CheckTypeWitnessResult();
}

/// Attempt to resolve a type witness via member name lookup.
ResolveWitnessResult ConformanceChecker::resolveTypeWitnessViaLookup(
                       AssociatedTypeDecl *assocType) {
  auto metaType = MetatypeType::get(Adoptee);

  // Look for a member type with the same name as the associated type.
  auto candidates = TC.lookupMemberType(metaType, assocType->getName(), DC,
                                        false, false);

  // If there aren't any candidates, we're done.
  if (!candidates) {
    return ResolveWitnessResult::Missing;
  }

  // Determine which of the candidates is viable.
  SmallVector<std::pair<TypeDecl *, Type>, 2> viable;
  SmallVector<std::pair<TypeDecl *, ProtocolDecl *>, 2> nonViable;
  for (auto candidate : candidates) {
    // Check this type against the protocol requirements.
    if (auto checkResult = checkTypeWitness(TC, DC, assocType, 
                                            candidate.second)) {
      auto reqProto = checkResult.getProtocol();
      nonViable.push_back({candidate.first, reqProto});
    } else {
      viable.push_back(candidate);
    }
  }

  // If there is a single viable candidate, form a substitution for it.
  if (viable.size() == 1) {
    recordTypeWitness(assocType, viable.front().second, viable.front().first,
                      viable.front().first->getDeclContext(), false);
    return ResolveWitnessResult::Success;
  }

  // Record an error.
  recordTypeWitness(assocType, ErrorType::get(TC.Context), nullptr, DC, true,
                    false);

  // If we had multiple viable types, diagnose the ambiguity.
  if (!viable.empty()) {
    diagnoseOrDefer(assocType, true,
      [assocType, viable](TypeChecker &tc,
                          NormalProtocolConformance *conformance) {
        tc.diagnose(assocType, diag::ambiguous_witnesses_type,
                    assocType->getName());

        for (auto candidate : viable)
          tc.diagnose(candidate.first, diag::protocol_witness_type);

      });

    return ResolveWitnessResult::ExplicitFailed;
  }

  // None of the candidates were viable.
  diagnoseOrDefer(assocType, true,
    [assocType, nonViable](TypeChecker &tc,
                           NormalProtocolConformance *conformance) {
      tc.diagnose(assocType, diag::no_witnesses_type, assocType->getName());

      for (auto candidate : nonViable) {
        if (candidate.first->getDeclaredType()->is<ErrorType>())
          continue;

        tc.diagnose(candidate.first,
                    diag::protocol_witness_nonconform_type,
                    candidate.first->getDeclaredType(),
                    candidate.second->getDeclaredType());
      }
    });

  return ResolveWitnessResult::ExplicitFailed;
}

InferredAssociatedTypesByWitnesses
ConformanceChecker::inferTypeWitnessesViaValueWitnesses(ValueDecl *req) {
  InferredAssociatedTypesByWitnesses result;

  for (auto witness : lookupValueWitnesses(req, /*ignoringNames=*/nullptr)) {
    // Try to resolve the type witness via this value witness.
    auto witnessResult = inferTypeWitnessesViaValueWitness(req, witness);

    // Filter out duplicated inferred types as well as inferred types
    // that don't meet the requirements placed on the associated type.
    llvm::DenseSet<std::pair<AssociatedTypeDecl *, CanType>> known;
    witnessResult.Inferred.erase(
      std::remove_if(witnessResult.Inferred.begin(),
                     witnessResult.Inferred.end(),
                     [&](const std::pair<AssociatedTypeDecl *, Type> &result) {
                       // Filter out duplicates.
                       if (!known.insert({result.first,
                                         result.second->getCanonicalType()})
                             .second)
                         return true;

                       // Check that the type witness meets the
                       // requirements on the associated type.
                       if (auto failed = checkTypeWitness(TC, DC, result.first,
                                                          result.second)) {
                         witnessResult.NonViable.push_back(
                           std::make_tuple(result.first,result.second,failed));
                         return true;
                       }

                       return false;
                     }),
      witnessResult.Inferred.end());

    // If no inferred types remain, skip this witness.
    if (witnessResult.Inferred.empty() && witnessResult.NonViable.empty())
      continue;

    // If there were any non-viable inferred associated types, don't
    // infer anything from this witness.
    if (!witnessResult.NonViable.empty())
      witnessResult.Inferred.clear();

    result.push_back(std::move(witnessResult));
  }

  return result;
}

InferredAssociatedTypes
ConformanceChecker::inferTypeWitnessesViaValueWitnesses(
  const llvm::SetVector<AssociatedTypeDecl *> &assocTypes)
{
  InferredAssociatedTypes result;
  for (auto member : Proto->getMembers()) {
    auto req = dyn_cast<ValueDecl>(member);
    if (!req)
      continue;

    // We only look at value witnesses.
    if (isa<AssociatedTypeDecl>(req))
      continue;

    // Skip operator requirements, because they match globally and 
    // therefore tend to cause deduction mismatches.
    // FIXME: If we had some basic sanity checking of Self, we might be able to
    // use these.
    if (auto func = dyn_cast<FuncDecl>(req)) {
      if (func->isOperator())
        continue;
    }

    // Validate the requirement.
    TC.validateDecl(req);
    if (req->isInvalid())
      continue;
      
    // Check whether any of the associated types we care about are
    // referenced in this value requirement.
    bool anyAssocTypeMatches = false;
    for (auto assocType : getReferencedAssociatedTypes(req)) {
      if (assocTypes.count(assocType) > 0) {
        anyAssocTypeMatches = true;
        break;
      }
    }

    // We cannot deduce anything from the witnesses of this
    // requirement; skip it.
    if (!anyAssocTypeMatches)
      continue;

    // Infer associated types from the potential value witnesses for
    // this requirement.
    auto reqInferred = inferTypeWitnessesViaValueWitnesses(req);
    if (reqInferred.empty())
      continue;

    result.push_back({req, std::move(reqInferred)});
  }

  return result;
}

/// Produce the type when matching a witness.
static Type getWitnessTypeForMatching(TypeChecker &tc, Type model, 
                                      ValueDecl *value) {
  if (!value->getDeclContext()->isTypeContext())
    return value->getInterfaceType();

  // If the witness is in a superclass, map the base type up until we
  // reach that superclass.
  Type baseTy = model;
  if (ClassDecl *targetClassDecl 
      = value->getDeclContext()->isClassOrClassExtensionContext()) {
    while (baseTy->getClassOrBoundGenericClass() != targetClassDecl)
      baseTy = tc.getSuperClassOf(baseTy);
  }
  
  return tc.substMemberTypeWithBase(value->getModuleContext(), value, baseTy,
                                    false);
}

/// Remove the 'self' type from the given type, if it's a method type.
static Type removeSelfParam(ValueDecl *value, Type type) {
  if (auto func = dyn_cast<AbstractFunctionDecl>(value)) {
    if (func->getDeclContext()->isTypeContext())
      return type->castTo<AnyFunctionType>()->getResult();
  }

  return type;
}

/// Attempt to resolve a type witness via a specific value witness.
InferredAssociatedTypesByWitness
ConformanceChecker::inferTypeWitnessesViaValueWitness(ValueDecl *req,
                                                      ValueDecl *witness) {
  InferredAssociatedTypesByWitness inferred;
  inferred.Witness = witness;

  // Compute the requirement and witness types we'll use for matching.
  Type fullWitnessType = getWitnessTypeForMatching(TC, Adoptee, witness);
  if (!fullWitnessType) {
    return inferred;
  }

  auto setup = [&]() -> std::tuple<Optional<RequirementMatch>, Type, Type> {
    fullWitnessType = removeSelfParam(witness, fullWitnessType);
    return std::make_tuple(
        None,
        removeSelfParam(req, req->getInterfaceType()),
        fullWitnessType);
  };

  /// Visits a requirement type to match it to a potential witness for
  /// the purpose of deducing associated types.
  ///
  /// The visitor argument is the witness type. If there are any
  /// obvious conflicts between the structure of the two types,
  /// returns true. The conflict checking is fairly conservative, only
  /// considering rough structure.
  class MatchVisitor : public CanTypeVisitor<MatchVisitor, bool, Type> {
    NormalProtocolConformance *Conformance;
    InferredAssociatedTypesByWitness &Inferred;

  public:
    MatchVisitor(NormalProtocolConformance *conformance,
                 InferredAssociatedTypesByWitness &inferred)
      : Conformance(conformance), Inferred(inferred) { }

#define TRIVIAL_CASE(TheType)                                     \
    bool visit##TheType(Can##TheType reqType, Type witnessType) { \
      return false;                                               \
    }

    TRIVIAL_CASE(ErrorType)
    TRIVIAL_CASE(BuiltinType)

    bool visitTupleType(CanTupleType reqTuple, Type witnessType) {
      if (auto witnessTuple = witnessType->getAs<TupleType>()) {
        if (reqTuple->getNumElements() != witnessTuple->getNumElements())
          return false;

        for (unsigned i = 0, n = reqTuple->getNumElements(); i != n; ++i) {
          const auto &reqElt = reqTuple->getElements()[i];
          const auto &witnessElt = witnessTuple->getElements()[i];

          if (reqElt.getName() != witnessElt.getName() ||
              reqElt.isVararg() != witnessElt.isVararg())
            return false;

          // Recurse on the tuple elements.
          if (visit(reqTuple.getElementType(i), witnessElt.getType()))
            return true;
        }

        return false;
      }

      return true;
    }

    bool visitReferenceStorageType(CanReferenceStorageType reqStorage,
                                   Type witnessType) {
      if (auto witnessStorage = witnessType->getAs<ReferenceStorageType>()) {
        if (reqStorage->getKind() != witnessStorage->getKind())
          return true;

        return visit(reqStorage.getReferentType(), 
                     witnessStorage->getReferentType());
      }

      return true;
    }

    bool visitNominalType(CanNominalType reqNominal, Type witnessType) {
      if (auto witnessNominal = witnessType->getAs<NominalType>()) {
        if (reqNominal->getDecl() != witnessNominal->getDecl())
          return false;

        if (reqNominal.getParent())
          return visit(reqNominal.getParent(), witnessNominal->getParent());

        return false;
      }

      return false;
    }

    bool visitAnyMetatypeType(CanAnyMetatypeType reqMeta, Type witnessType) {
      if (auto witnessMeta = witnessType->getAs<AnyMetatypeType>()) {
        if (reqMeta->getKind() != witnessMeta->getKind())
          return false;

        return visit(reqMeta.getInstanceType(), 
                     witnessMeta->getInstanceType());
      }

      return true;
    }

    TRIVIAL_CASE(ModuleType)
    TRIVIAL_CASE(DynamicSelfType)
    TRIVIAL_CASE(ArchetypeType)
    TRIVIAL_CASE(AbstractTypeParamType)

    bool visitDependentMemberType(CanDependentMemberType reqType,
                                  Type witnessType) {
      auto proto = Conformance->getProtocol();
      if (auto assocType = getReferencedAssocTypeOfProtocol(reqType, proto)) {
        // If the witness type is non-dependent, add it as a
        // deduction.
        if (!witnessType->isDependentType()) {
          Inferred.Inferred.push_back({assocType, witnessType});
        }
      }

      return false;
    }

    bool visitAnyFunctionType(CanAnyFunctionType reqFunc, Type witnessType) {
      if (auto witnessFunc = witnessType->getAs<AnyFunctionType>()) {
        return visit(reqFunc.getInput(), witnessFunc->getInput()) ||
               visit(reqFunc.getResult(), witnessFunc->getResult());
      }

      return true;
    }

    TRIVIAL_CASE(SILFunctionType)
    TRIVIAL_CASE(SILBlockStorageType)
    TRIVIAL_CASE(ProtocolCompositionType)

    bool visitLValueType(CanLValueType reqLValue, Type witnessType) {
      if (auto witnessLValue = witnessType->getAs<LValueType>()) {
        return visit(reqLValue.getObjectType(), 
                     witnessLValue->getObjectType());
      }

      return true;
    }

    bool visitInOutType(CanInOutType reqInOut, Type witnessType) {
      if (auto witnessInOut = witnessType->getAs<InOutType>()) {
        return visit(reqInOut.getObjectType(), witnessInOut->getObjectType());
      }

      return true;
    }

    TRIVIAL_CASE(UnboundGenericType)

    bool visitBoundGenericType(CanBoundGenericType reqBGT, Type witnessType) {
      if (auto witnessBGT = witnessType->getAs<BoundGenericType>()) {
        if (reqBGT->getDecl() != witnessBGT->getDecl() ||
            reqBGT->getGenericArgs().size() 
              != witnessBGT->getGenericArgs().size())
          return false;
        
        if (reqBGT->getParent() && 
            visit(reqBGT.getParent(), witnessBGT->getParent()))
          return true;

        for (unsigned i = 0, n = reqBGT->getGenericArgs().size(); i != n; ++i) {
          if (visit(reqBGT.getGenericArgs()[i], 
                    witnessBGT->getGenericArgs()[i]))
            return true;
        }

        return false;
      }

      return false;
    }

    TRIVIAL_CASE(TypeVariableType)

#undef TRIVIAL_CASE
  };

  // Match a requirement and witness type.
  MatchVisitor matchVisitor(Conformance, inferred);
  auto matchTypes = [&](Type reqType, Type witnessType)
                      -> Optional<RequirementMatch> {
    if (matchVisitor.visit(reqType->getCanonicalType(), witnessType)) {
      return RequirementMatch(witness, MatchKind::TypeConflict,
                              fullWitnessType);
    }

    return None;
  };

  // Finalization of the checking is pretty trivial; just bundle up a
  // result we can look at.
  auto finalize = [&](bool anyRenaming, ArrayRef<OptionalAdjustment>)
                    -> RequirementMatch {
    return RequirementMatch(witness, 
                            anyRenaming ? MatchKind::RenamedMatch
                                        : MatchKind::ExactMatch,
                            fullWitnessType);

  };

  // Match the witness. If we don't 
  // FIXME: A renamed match might be useful to retain for the failure case.
  if (matchWitness(TC, Conformance, DC, req, witness, setup, matchTypes,
                   finalize).Kind != MatchKind::ExactMatch) {
    inferred.Inferred.clear();
  }

  return inferred;
}

namespace {
  /// A potential solution to the set of inferred type witnesses.
  struct InferredTypeWitnessesSolution {
    /// The set of type witnesses inferred by this solution, along
    /// with the index into the value witnesses where the type was
    /// inferred.
    llvm::SmallDenseMap<AssociatedTypeDecl *, std::pair<Type, unsigned>, 4>
      TypeWitnesses;

    /// The value witnesses selected by this step of the solution.
    SmallVector<std::pair<ValueDecl *, ValueDecl *>, 4> ValueWitnesses;

    /// The number of value witnesses that occur in protocol
    /// extensions.
    unsigned NumValueWitnessesInProtocolExtensions;
  };

  /// A failed type witness binding.
  struct FailedTypeWitness {
    /// The value requirement that triggered inference.
    ValueDecl *Requirement;

    /// The corresponding value witness from which the type witness
    /// was inferred.
    ValueDecl *Witness;

    /// The actual type witness that was inferred.
    Type TypeWitness;

    /// The failed type witness result.
    CheckTypeWitnessResult Result;
  };

  /// A conflict between two inferred type witnesses for the same
  /// associated type.
  struct TypeWitnessConflict {
    /// The associated type.
    AssociatedTypeDecl *AssocType;

    /// The first type.
    Type FirstType;

    /// The requirement to which the first witness was matched.
    ValueDecl *FirstRequirement;

    /// The witness from which the first type witness was inferred.
    ValueDecl *FirstWitness;

    /// The second type.
    Type SecondType;

    /// The requirement to which the second witness was matched.
    ValueDecl *SecondRequirement;

    /// The witness from which the second type witness was inferred.
    ValueDecl *SecondWitness;
  };
}

void ConformanceChecker::resolveTypeWitnesses() {
  llvm::SetVector<AssociatedTypeDecl *> unresolvedAssocTypes;

  for (auto member : Proto->getMembers()) {
    auto assocType = dyn_cast<AssociatedTypeDecl>(member);
    if (!assocType)
      continue;

    // If we already have a type witness, do nothing.
    if (Conformance->hasTypeWitness(assocType))
      continue;
    
    // Try to resolve this type witness via name lookup, which is the
    // most direct mechanism, overriding all others.
    switch (resolveTypeWitnessViaLookup(assocType)) {
    case ResolveWitnessResult::Success:
      // Success. Move on to the next.
      continue;

    case ResolveWitnessResult::ExplicitFailed:
      continue;

    case ResolveWitnessResult::Missing:
      // Note that we haven't resolved this associated type yet.
      unresolvedAssocTypes.insert(assocType);
      break;
    }
  }

  // If we resolved everything, we're done.
  if (unresolvedAssocTypes.empty())
    return;

  // Infer type witnesses from value witnesses.
  auto inferred = inferTypeWitnessesViaValueWitnesses(unresolvedAssocTypes);

  // Compute the set of solutions.
  SmallVector<std::pair<ValueDecl *, ValueDecl *>, 4> valueWitnesses;
  unsigned numValueWitnessesInProtocolExtensions = 0;
  llvm::ScopedHashTable<AssociatedTypeDecl *, std::pair<Type, unsigned>>
    typeWitnesses;
  typedef decltype(typeWitnesses)::ScopeTy TypeWitnessesScope;
  unsigned numTypeWitnesses = 0;
  SmallVector<InferredTypeWitnessesSolution, 4> solutions;

  // Information to use for diagnosing failures when we don't have
  // something more specific.

  // Which type witness was missing?
  AssociatedTypeDecl *missingTypeWitness = nullptr;

  // Was there a conflict in type witness deduction?
  Optional<TypeWitnessConflict> typeWitnessConflict;
  unsigned numTypeWitnessesBeforeConflict;

  // Did an associated type default fail?
  AssociatedTypeDecl *failedDefaultedAssocType = nullptr;
  Type failedDefaultedWitness;
  CheckTypeWitnessResult failedDefaultedResult;

  // Local function to compute the default type of an associated type.
  auto computeDefaultTypeWitness = [&](AssociatedTypeDecl *assocType) -> Type {
    // If we don't have a default definition, we're done.
    if (assocType->getDefaultDefinitionLoc().isNull())
      return Type();

    // Create a set of type substitutions for all known associated type.
    // FIXME: Base this on dependent types rather than archetypes?
    TypeSubstitutionMap substitutions;
    substitutions[Proto->getProtocolSelf()->getArchetype()] = Adoptee;
    for (auto member : Proto->getMembers()) {
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
        if (Conformance->hasTypeWitness(assocType)) {
          substitutions[assocType->getArchetype()]
            = Conformance->getTypeWitness(assocType, nullptr).getReplacement();
        } else {
          auto known = typeWitnesses.begin(assocType);
          if (known != typeWitnesses.end())
            substitutions[assocType->getArchetype()] = known->first;
          else
            substitutions[assocType->getArchetype()] = ErrorType::get(TC.Context);
        }
      }
    }

    Type defaultType = TC.substType(
                         DC->getParentModule(),
                         assocType->getDefaultDefinitionLoc().getType(),
                         substitutions,
                         /*IgnoreMissing=*/true);
    if (!defaultType)
      return Type();

    if (auto failed = checkTypeWitness(TC, DC, assocType, defaultType)) {
      // Record the failure, if we haven't seen one already.
      if (!failedDefaultedAssocType) {
        failedDefaultedAssocType = assocType;
        failedDefaultedWitness = defaultType;
        failedDefaultedResult = failed;
      }

      return Type();
    }

    return defaultType;
  };

  // Local function to compute the derived type of an associated type,
  // for protocols known to the compiler.
  auto computeDerivedTypeWitness = [&](AssociatedTypeDecl *assocType) -> Type {
    // Can we derive conformances for this protocol and adoptee?
    NominalTypeDecl *derivingTypeDecl = Adoptee->getAnyNominal();
    if (!derivingTypeDecl->derivesProtocolConformance(Proto))
      return Type();

    // Try to derive the type witness.
    Type derivedType = TC.deriveTypeWitness(derivingTypeDecl, assocType);
    if (!derivedType)
      return Type();

    // Make sure that the derived type is sane.
    if (checkTypeWitness(TC, DC, assocType, derivedType)) {
      diagnoseOrDefer(assocType, true,
        [derivedType](TypeChecker &tc, NormalProtocolConformance *conformance) {
          // FIXME: give more detail here?
          tc.diagnose(conformance->getLoc(),
                      diag::protocol_derivation_is_broken,
                      conformance->getProtocol()->getDeclaredType(),
                      derivedType);
      });

      return Type();
    }

    return derivedType;
  };

  // Local function to perform the depth-first search of the solution
  // space.
  std::function<void(unsigned)> findSolutions;
  findSolutions = [&](unsigned reqDepth) {
    // If we hit the last requirement, record and check this solution.
    if (reqDepth == inferred.size()) {
      // Introduce a hash table scope; we may add type witnesses here.
      TypeWitnessesScope typeWitnessesScope(typeWitnesses);

      // Check for completeness of the solution
      for (auto assocType : unresolvedAssocTypes) {
        auto typeWitness = typeWitnesses.begin(assocType);
        if (typeWitness == typeWitnesses.end()) {
          // We don't have a type witness for this associated type.
          
          // If we can form a default type, do so.
          if (Type defaultType = computeDefaultTypeWitness(assocType)) {
            typeWitnesses.insert(assocType, {defaultType, reqDepth});
            continue;
          }

          // If we can derive a type witness, do so.
          if (Type derivedType = computeDerivedTypeWitness(assocType)) {
            typeWitnesses.insert(assocType, {derivedType, reqDepth});
            continue;
          }

          // The solution is incomplete.
          if (!missingTypeWitness)
            missingTypeWitness = assocType;
          return;
        }
      }

      // Determine whether there is already a solution with the same
      // bindings.
      for (const auto &solution : solutions) {
        bool sameSolution = true;
        for (const auto &existingTypeWitness : solution.TypeWitnesses) {
          auto typeWitness = typeWitnesses.begin(existingTypeWitness.first);
          if (!typeWitness->first->isEqual(existingTypeWitness.second.first)) {
            sameSolution = false;
            break;
          }
        }

        // We found the same solution. There is no point in recording
        // a new one.
        if (sameSolution)
          return;
      }

      solutions.push_back(InferredTypeWitnessesSolution());
      auto &solution = solutions.back();

      // Copy the type witnesses.
      for (auto assocType : unresolvedAssocTypes) {
        auto typeWitness = typeWitnesses.begin(assocType);
        solution.TypeWitnesses.insert({assocType, *typeWitness});
      }

      // Copy the value witnesses.
      solution.ValueWitnesses = valueWitnesses;
      solution.NumValueWitnessesInProtocolExtensions
        = numValueWitnessesInProtocolExtensions;

      // We're done recording the solution.
      return;
    }

    // Iterate over the potential witnesses for this requirement,
    // looking for solutions involving each one.
    const auto &inferredReq = inferred[reqDepth];
    for (const auto &witnessReq : inferredReq.second) {
      // Enter a new scope for the type witnesses hash table.
      TypeWitnessesScope typeWitnessesScope(typeWitnesses);
      llvm::SaveAndRestore<unsigned> savedNumTypeWitnesses(numTypeWitnesses);

      // Introduce each of the type witnesses into the hash table.
      bool failed = false;
      for (const auto &typeWitness : witnessReq.Inferred) {
        // If we've seen a type witness for this associated type that
        // conflicts, there is no solution.
        auto known = typeWitnesses.begin(typeWitness.first);
        if (known != typeWitnesses.end()) {
          // If witnesses for two difference requirements inferred the same
          // type, we're okay.
          if (known->first->isEqual(typeWitness.second))
            continue;
          
          if (!typeWitnessConflict ||
              numTypeWitnesses > numTypeWitnessesBeforeConflict) {
            typeWitnessConflict = {typeWitness.first,
                                   typeWitness.second,
                                   inferredReq.first,
                                   witnessReq.Witness,
                                   known->first,
                                   valueWitnesses[known->second].first,
                                   valueWitnesses[known->second].second};
            numTypeWitnessesBeforeConflict = numTypeWitnesses;
          }

          failed = true;
          break;
        }

        // Record the type witness.
        ++numTypeWitnesses;
        typeWitnesses.insert(typeWitness.first, {typeWitness.second, reqDepth});
      }

      if (failed)
        continue;

      // Record this value witness and recurse.
      valueWitnesses.push_back({inferredReq.first, witnessReq.Witness});
      if (witnessReq.Witness->getDeclContext()->isProtocolExtensionContext())
        ++numValueWitnessesInProtocolExtensions;
      findSolutions(reqDepth + 1);
      if (witnessReq.Witness->getDeclContext()->isProtocolExtensionContext())
        --numValueWitnessesInProtocolExtensions;
      valueWitnesses.pop_back();
    }
  };
  findSolutions(0);


  // Go make sure that type declarations that would act as witnesses
  // did not get injected while we were performing checks above. This
  // can happen when two associated types in different protocols have
  // the same name, and validating a declaration (above) triggers the
  // type-witness generation for that second protocol, introducing a
  // new type declaration.
  for (auto assocType : unresolvedAssocTypes) {
      switch (resolveTypeWitnessViaLookup(assocType)) {
      case ResolveWitnessResult::Success:
      case ResolveWitnessResult::ExplicitFailed:
        // A declaration that can become a witness has shown up. Go
        // perform the resolution again now that we have more
        // information.
        return resolveTypeWitnesses();

      case ResolveWitnessResult::Missing:
        // The type witness is still missing. Keep going.
        break;
      }
  }

  // If we have more than one solution, do some simple ranking.
  if (solutions.size() > 1) {
    // Find the smallest number of value witnesses found in protocol extensions.
    unsigned bestNumValueWitnessesInProtocolExtensions
      = solutions.front().NumValueWitnessesInProtocolExtensions;
    for (unsigned i = 1, n = solutions.size(); i != n; ++i) {
      bestNumValueWitnessesInProtocolExtensions
        = std::min(bestNumValueWitnessesInProtocolExtensions,
                   solutions[i].NumValueWitnessesInProtocolExtensions);
    }

    // Erase any solutions with more value witnesses in protocol
    // extensions than the best.
    solutions.erase(
      std::remove_if(solutions.begin(), solutions.end(),
                     [&](const InferredTypeWitnessesSolution &solution) {
                       return solution.NumValueWitnessesInProtocolExtensions >
                                bestNumValueWitnessesInProtocolExtensions;
                     }),
      solutions.end());
  }

  // If we found a single solution, take it.
  if (solutions.size() == 1) {
    // Record each of the deduced witnesses.
    for (const auto &typeWitness : solutions.front().TypeWitnesses) {
      recordTypeWitness(typeWitness.first, typeWitness.second.first, nullptr,
                        DC, true);
    }

    return;
  }

  // We're going to produce an error below. Mark each unresolved
  // associated type witness as erroneous.
  for (auto assocType : unresolvedAssocTypes) {
    recordTypeWitness(assocType, ErrorType::get(TC.Context), nullptr, DC, true);
  }

  // No solutions. Diagnose the first associated type for which we
  // could not determine a witness.
  if (solutions.empty()) {
    // If a defaulted type witness failed, diagnose it.
    if (failedDefaultedAssocType) {
      diagnoseOrDefer(failedDefaultedAssocType, true,
        [failedDefaultedAssocType, failedDefaultedWitness,
         failedDefaultedResult](TypeChecker &tc,
                                NormalProtocolConformance *conformance) {
          auto proto = conformance->getProtocol();
          tc.diagnose(failedDefaultedAssocType,
                      diag::default_assocated_type_req_fail,
                      failedDefaultedWitness,
                      failedDefaultedAssocType->getFullName(),
                      proto->getDeclaredType(),
                      failedDefaultedResult.getProtocol()->getDeclaredType());
        });
      return;
    }

    // Form a mapping from associated type declarations to failed type
    // witnesses.
    llvm::DenseMap<AssociatedTypeDecl *, SmallVector<FailedTypeWitness, 2>>
      failedTypeWitnesses;
    for (const auto &inferredReq : inferred) {
      for (const auto &inferredWitness : inferredReq.second) {
        for (const auto &nonViable : inferredWitness.NonViable) {
          failedTypeWitnesses[std::get<0>(nonViable)]
            .push_back({inferredReq.first, inferredWitness.Witness,
                        std::get<1>(nonViable), std::get<2>(nonViable)});
        }
      }
    }

    // Local function to attempt to diagnose potential type witnesses
    // that failed requirements.
    auto tryDiagnoseTypeWitness = [&](AssociatedTypeDecl *assocType) -> bool {
      auto known = failedTypeWitnesses.find(assocType);
      if (known == failedTypeWitnesses.end())
        return false;

      auto failedSet = std::move(known->second);
      diagnoseOrDefer(assocType, true,
        [assocType, failedSet](TypeChecker &tc,
                               NormalProtocolConformance *conformance) {
          auto proto = conformance->getProtocol();
          tc.diagnose(assocType, diag::bad_associated_type_deduction,
                      assocType->getFullName(), proto->getFullName());
          for (const auto &failed : failedSet) {
            tc.diagnose(failed.Witness,
                        diag::associated_type_deduction_witness_failed,
                        failed.Requirement->getFullName(),
                        failed.TypeWitness,
                        failed.Result.getProtocol()->getFullName());
          }
        });

      return true;
    };

    // Try to diagnose the first missing type witness we encountered.
    if (missingTypeWitness && tryDiagnoseTypeWitness(missingTypeWitness))
      return;

    // Failing that, try to diagnose any type witness that failed a
    // requirement.
    for (auto assocType : unresolvedAssocTypes) {
      if (tryDiagnoseTypeWitness(assocType))
        return;
    }

    // If we saw a conflict, complain about it.
    if (typeWitnessConflict) {
      diagnoseOrDefer(typeWitnessConflict->AssocType, true,
        [typeWitnessConflict](
          TypeChecker &tc, NormalProtocolConformance *conformance) {
          tc.diagnose(typeWitnessConflict->AssocType,
                      diag::ambiguous_associated_type_deduction,
                      typeWitnessConflict->AssocType->getFullName(),
                      typeWitnessConflict->FirstType,
                      typeWitnessConflict->SecondType);
        
          tc.diagnose(typeWitnessConflict->FirstWitness,
                      diag::associated_type_deduction_witness,
                      typeWitnessConflict->FirstRequirement->getFullName(),
                      typeWitnessConflict->FirstType);
          tc.diagnose(typeWitnessConflict->SecondWitness,
                      diag::associated_type_deduction_witness,
                      typeWitnessConflict->SecondRequirement->getFullName(),
                      typeWitnessConflict->SecondType);
        });

      return;
    }

    // Complain that we couldn't find anything.
    if (!missingTypeWitness)
      missingTypeWitness = *unresolvedAssocTypes.begin();

    diagnoseOrDefer(missingTypeWitness, true,
      [missingTypeWitness](TypeChecker &tc,
                           NormalProtocolConformance *conformance) {
      tc.diagnose(missingTypeWitness, diag::no_witnesses_type,
                  missingTypeWitness->getName());
    });

    return;
  }

  // Multiple solutions. Diagnose the ambiguity.
  for (auto assocType : unresolvedAssocTypes) {
    // Find two types that conflict.
    Type firstType = solutions.front().TypeWitnesses[assocType].first;
    std::pair<ValueDecl *, ValueDecl *> firstMatch
      = solutions.front().ValueWitnesses[
          solutions.front().TypeWitnesses[assocType].second];
    Type secondType;
    std::pair<ValueDecl *, ValueDecl *> secondMatch;
    for (auto &solution : solutions) {
      Type typeWitness = solution.TypeWitnesses[assocType].first;
      if (!typeWitness->isEqual(firstType)) {
        secondType = typeWitness;
        secondMatch = solution.ValueWitnesses[
                        solution.TypeWitnesses[assocType].second];
        break;
      }
    }

    if (!secondType)
      continue;

    // We found an ambiguity. diagnose it.
    diagnoseOrDefer(assocType, true,
      [assocType, firstType, firstMatch, secondType, secondMatch](
        TypeChecker &tc, NormalProtocolConformance *conformance) {
        tc.diagnose(assocType, diag::ambiguous_associated_type_deduction,
                    assocType->getFullName(), firstType, secondType);
      
        tc.diagnose(firstMatch.second, diag::associated_type_deduction_witness,
                    firstMatch.first->getFullName(), firstType);
        tc.diagnose(secondMatch.second, diag::associated_type_deduction_witness,
                    secondMatch.second->getFullName(), secondType);
      });

    return;
  }
}

void ConformanceChecker::resolveSingleTypeWitness(
       AssociatedTypeDecl *assocType) {
  switch (resolveTypeWitnessViaLookup(assocType)) {
  case ResolveWitnessResult::Success:
  case ResolveWitnessResult::ExplicitFailed:
    // We resolved this type witness one way or another.
    return;

  case ResolveWitnessResult::Missing:
    // The type witness is still missing. Resolve all of the type witnesses.
    resolveTypeWitnesses();
    return;
  }
}

void ConformanceChecker::resolveSingleWitness(ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Not a value witness");
  assert(!Conformance->hasWitness(requirement) && "Already resolved");

  // Note that we're resolving this witness.
  assert(ResolvingWitnesses.count(requirement) == 0 && "Currently resolving");
  ResolvingWitnesses.insert(requirement);
  defer([&]{ ResolvingWitnesses.erase(requirement); });

  // Make sure we've validated the requirement.
  if (!requirement->hasType())
    TC.validateDecl(requirement, true);

  if (requirement->isInvalid()) {
    // FIXME: Note that there is no witness?
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;
  }

  // If this is a getter/setter for a funcdecl, ignore it.
  if (auto *FD = dyn_cast<FuncDecl>(requirement))
    if (FD->isAccessor())
      return;

  // Resolve all associated types before trying to resolve this witness.
  resolveTypeWitnesses();

  // If any of the type witnesses was erroneous, don't bother to check
  // this value witness: it will fail.
  for (auto assocType : getReferencedAssociatedTypes(requirement)) {
    if (Conformance->getTypeWitness(assocType, nullptr).getReplacement()
          ->is<ErrorType>()) {
      Conformance->setState(ProtocolConformanceState::Invalid);    
      return;
    }
  }

  // Try to resolve the witness via explicit definitions.
  switch (resolveWitnessViaLookup(requirement)) {
  case ResolveWitnessResult::Success:
    return;
  
  case ResolveWitnessResult::ExplicitFailed:
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;

  case ResolveWitnessResult::Missing:
    // Continue trying below.
    break;
  }

  // Try to resolve the witness via derivation.
  switch (resolveWitnessViaDerivation(requirement)) {
  case ResolveWitnessResult::Success:
    return;

  case ResolveWitnessResult::ExplicitFailed:
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;

  case ResolveWitnessResult::Missing:
    // Continue trying below.
    break;
  }

  // Try to resolve the witness via defaults.
  switch (resolveWitnessViaDefault(requirement)) {
  case ResolveWitnessResult::Success:
    return;

  case ResolveWitnessResult::ExplicitFailed:
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;

  case ResolveWitnessResult::Missing:
    llvm_unreachable("Should have failed");
    break;
  }
}

#pragma mark Protocol conformance checking
void ConformanceChecker::checkConformance() {
  assert(!Conformance->isComplete() && "Conformance is already complete");

  llvm::SaveAndRestore<bool> restoreSuppressDiagnostics(SuppressDiagnostics);
  SuppressDiagnostics = false;

  // FIXME: Caller checks that this type conforms to all of the
  // inherited protocols.

  emitDelayedDiags();

  // Resolve all of the type witnesses.
  resolveTypeWitnesses();

  // If we complain about any associated types, there is no point in continuing.
  // FIXME: Not really true. We could check witnesses that don't involve the
  // failed associated types.
  if (AlreadyComplained) {
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;
  }

  // Check non-type requirements.
  bool invalid = false;
  for (auto member : Proto->getMembers()) {
    auto requirement = dyn_cast<ValueDecl>(member);
    if (!requirement)
      continue;

    // Associated type requirements handled above.
    if (isa<AssociatedTypeDecl>(requirement))
      continue;

    // If we've already determined this witness, skip it.
    if (Conformance->hasWitness(requirement))
      continue;

    // Make sure we've validated the requirement.
    if (!requirement->hasType())
      TC.validateDecl(requirement, true);

    if (requirement->isInvalid()) {
      invalid = true;
      continue;
    }

    // If this is a getter/setter for a funcdecl, ignore it.
    if (auto *FD = dyn_cast<FuncDecl>(requirement))
      if (FD->isAccessor())
        continue;
    
    // Try to resolve the witness via explicit definitions.
    switch (resolveWitnessViaLookup(requirement)) {
    case ResolveWitnessResult::Success:
      continue;

    case ResolveWitnessResult::ExplicitFailed:
      invalid = true;
      continue;

    case ResolveWitnessResult::Missing:
      // Continue trying below.
      break;
    }

    // Try to resolve the witness via derivation.
    switch (resolveWitnessViaDerivation(requirement)) {
    case ResolveWitnessResult::Success:
      continue;

    case ResolveWitnessResult::ExplicitFailed:
      invalid = true;
      continue;

    case ResolveWitnessResult::Missing:
      // Continue trying below.
      break;
    }

    // Try to resolve the witness via defaults.
    switch (resolveWitnessViaDefault(requirement)) {
    case ResolveWitnessResult::Success:
      continue;

    case ResolveWitnessResult::ExplicitFailed:
      invalid = true;
      continue;

    case ResolveWitnessResult::Missing:
      // Continue trying below.
      break;
    }
  }

  emitDelayedDiags();

  if (AlreadyComplained || invalid) {
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;
  }
  if (AlreadyComplained) {
    Conformance->setState(ProtocolConformanceState::Invalid);
  } else {
    Conformance->setState(ProtocolConformanceState::Complete);
  }
}

void ConformanceChecker::diagnoseOrDefer(
       ValueDecl *requirement, bool isError,
       std::function<void(TypeChecker &, NormalProtocolConformance *)> fn) {
  if (isError)
    Conformance->setState(ProtocolConformanceState::Invalid);

  if (SuppressDiagnostics) {
    // Stash this in the ASTContext for later emission.
    auto *tc = &TC;
    auto conformance = Conformance;
    TC.Context.addDelayedConformanceDiag(conformance,
                                         { requirement,
                                           [tc, conformance, fn] {
                                              fn(*tc, conformance);
                                            },
                                           isError });
    return;
  }

  // Complain that the type does not conform, once.
  if (isError && !AlreadyComplained) {
    TC.diagnose(Loc, diag::type_does_not_conform, Adoptee,
                Proto->getDeclaredType());
    AlreadyComplained = true;
  }

  fn(TC, Conformance);
}

void ConformanceChecker::emitDelayedDiags() {
  auto diags = TC.Context.takeDelayedConformanceDiags(Conformance);

  assert(!SuppressDiagnostics && "Should not be suppressing diagnostics now");
  for (const auto &diag: diags) {
    diagnoseOrDefer(diag.Requirement, diag.IsError,
      [&](TypeChecker &tc, NormalProtocolConformance *conformance) {
        return diag.Callback();
    });
  }
}

/// \brief Determine whether the type \c T conforms to the protocol \c Proto,
/// recording the complete witness table if it does.
static ProtocolConformance *
checkConformsToProtocol(TypeChecker &TC,
                        NormalProtocolConformance *conformance) {
  // If we're already checking this conformance, just return it.
  if (conformance->getState() == ProtocolConformanceState::Checking ||
      conformance->getState() == ProtocolConformanceState::Complete ||
      conformance->getState() == ProtocolConformanceState::Invalid)
    return conformance;

  // Eit ou5
  Type T = conformance->getType();
  auto canT = T->getCanonicalType();
  DeclContext *DC = conformance->getDeclContext();
  auto Proto = conformance->getProtocol();
  SourceLoc ComplainLoc = conformance->getLoc();

  // Note that we are checking this conformance now.
  conformance->setState(ProtocolConformanceState::Checking);

  // If the protocol requires a class, non-classes are a non-starter.
  if (Proto->requiresClass() && !canT->getClassOrBoundGenericClass()) {
    TC.diagnose(ComplainLoc, diag::non_class_cannot_conform_to_class_protocol,
                T, Proto->getDeclaredType());
    conformance->setState(ProtocolConformanceState::Invalid);
    return conformance;
  }

  // Foreign classes cannot conform to objc protocols.
  if (Proto->isObjC() &&
      !Proto->isSpecificProtocol(KnownProtocolKind::AnyObject)) {
    if (auto clas = canT->getClassOrBoundGenericClass())
      if (clas->isForeign()) {
        TC.diagnose(ComplainLoc,
                    diag::foreign_class_cannot_conform_to_objc_protocol,
                    T, Proto->getDeclaredType());
        conformance->setState(ProtocolConformanceState::Invalid);
        return conformance;
      }
  }

  // If the protocol contains missing requirements, it can't be conformed to
  // at all.
  if (Proto->hasMissingRequirements()) {
    TC.diagnose(ComplainLoc, diag::protocol_has_missing_requirements,
                T, Proto->getDeclaredType());
    conformance->setState(ProtocolConformanceState::Invalid);
    return conformance;
  }

  // Check that T conforms to all inherited protocols.
  for (auto InheritedProto : Proto->getInheritedProtocols(&TC)) {
    ProtocolConformance *InheritedConformance = nullptr;
    if (TC.conformsToProtocol(T, InheritedProto, DC, false,
                              &InheritedConformance, ComplainLoc)) {
      if (!conformance->hasInheritedConformance(InheritedProto))
        conformance->setInheritedConformance(InheritedProto,
                                             InheritedConformance);
    } else {
      // Recursive call already diagnosed this problem, but tack on a note
      // to establish the relationship.
      if (ComplainLoc.isValid()) {
        TC.diagnose(Proto,
                    diag::inherited_protocol_does_not_conform, T,
                    InheritedProto->getDeclaredType());
      }
      
      conformance->setState(ProtocolConformanceState::Invalid);
      return conformance;
    }
  }

  if (conformance->isComplete())
    return conformance;

  // The conformance checker we're using.
  ConformanceChecker checker(TC, conformance);
  checker.checkConformance();
  return conformance;
}

bool TypeChecker::conformsToProtocol(Type T, ProtocolDecl *Proto,
                                     DeclContext *DC, bool InExpression,
                                     ProtocolConformance **Conformance,
                                     SourceLoc ComplainLoc) {
  const DeclContext *topLevelContext = DC->getModuleScopeContext();
  auto recordDependency = [=](ProtocolConformance *conformance = nullptr) {
    // Record that we depend on the type's conformance.
    auto *constSF = dyn_cast<SourceFile>(topLevelContext);
    if (!constSF)
      return;
    auto *SF = const_cast<SourceFile *>(constSF);

    auto *tracker = SF->getReferencedNameTracker();
    if (!tracker)
      return;

    // We only care about intra-module dependencies.
    if (conformance)
      if (SF->getParentModule() !=
          conformance->getDeclContext()->getParentModule())
        return;

    if (auto nominal = T->getAnyNominal()) {
      tracker->addUsedNominal(nominal,
                              DC->isCascadingContextForLookup(InExpression));
    }
  };

  // Look up conformance in the module.
  Module *M = topLevelContext->getParentModule();
  auto lookupResult = M->lookupConformance(T, Proto, this);
  switch (lookupResult.getInt()) {
  case ConformanceKind::Conforms:
    if (Conformance)
      *Conformance = lookupResult.getPointer();
    recordDependency(lookupResult.getPointer());
    return true;

  case ConformanceKind::DoesNotConform:
    if (ComplainLoc.isValid()) {
      if (!T->is<ErrorType>())
        diagnose(ComplainLoc, diag::type_does_not_conform,
                 T, Proto->getDeclaredType());
    } else {
      recordDependency();
    }
    return false;

  case ConformanceKind::UncheckedConforms:
    llvm_unreachable("Can't get here!");
  }
}

void TypeChecker::checkConformance(NormalProtocolConformance *conformance) {
  checkConformsToProtocol(*this, conformance);
}

void TypeChecker::resolveTypeWitness(
       const NormalProtocolConformance *conformance,
       AssociatedTypeDecl *assocType) {
  ConformanceChecker checker(
                       *this, 
                       const_cast<NormalProtocolConformance*>(conformance));
  checker.resolveSingleTypeWitness(assocType);
}

void TypeChecker::resolveWitness(const NormalProtocolConformance *conformance,
                                 ValueDecl *requirement) {
  ConformanceChecker checker(
                       *this, 
                       const_cast<NormalProtocolConformance*>(conformance));
  checker.resolveSingleWitness(requirement);
}

ValueDecl *TypeChecker::deriveProtocolRequirement(NominalTypeDecl *TypeDecl,
                                                  ValueDecl *Requirement) {
  auto *protocol = cast<ProtocolDecl>(Requirement->getDeclContext());

  auto knownKind = protocol->getKnownProtocolKind();
  
  if (!knownKind)
    return nullptr;

  switch (*knownKind) {
  case KnownProtocolKind::RawRepresentable:
    return DerivedConformance::deriveRawRepresentable(*this,
                                                      TypeDecl, Requirement);

  case KnownProtocolKind::Equatable:
    return DerivedConformance::deriveEquatable(*this, TypeDecl, Requirement);
  
  case KnownProtocolKind::Hashable:
    return DerivedConformance::deriveHashable(*this, TypeDecl, Requirement);
    
  case KnownProtocolKind::_ErrorType:
    return DerivedConformance::deriveErrorType(*this, TypeDecl, Requirement);
    
  default:
    return nullptr;
  }
}

Type TypeChecker::deriveTypeWitness(NominalTypeDecl *nominal,
                                    AssociatedTypeDecl *assocType) {
  auto *protocol = cast<ProtocolDecl>(assocType->getDeclContext());

  auto knownKind = protocol->getKnownProtocolKind();
  
  if (!knownKind)
    return nullptr;

  switch (*knownKind) {
  case KnownProtocolKind::RawRepresentable:
    return DerivedConformance::deriveRawRepresentable(*this, nominal,
                                                      assocType);
        
  default:
    return nullptr;
  }
}

bool TypeChecker::isProtocolExtensionUsable(DeclContext *dc, Type type,
                                            ExtensionDecl *protocolExtension) {
  using namespace constraints;

  assert(protocolExtension->isProtocolExtensionContext() &&
         "Only intended for protocol extensions");

  resolveExtension(protocolExtension);

  // Dig down to the type we care about.
  type = type->getInOutObjectType()->getRValueType();
  if (auto metaTy = type->getAs<AnyMetatypeType>())
    type = metaTy->getInstanceType();

  auto genericSig = protocolExtension->getGenericSignature();
  auto protCanonSig = protocolExtension->getExtendedType()
                        ->getAs<ProtocolType>()
                        ->getDecl()
                        ->getGenericSignature()
                        ->getCanonicalSignature();
  // Unconstrained protocol extensions are usable.
  if (protCanonSig == genericSig->getCanonicalSignature())
    return true;

  // Set up a constraint system where we open the generic parameters of the
  // protocol extension.
  ConstraintSystem cs(*this, dc, None);
  llvm::DenseMap<CanType, TypeVariableType *> replacements;

  cs.openGeneric(protocolExtension, genericSig->getGenericParams(),
                 genericSig->getRequirements(), false, nullptr,
                 ConstraintLocatorBuilder(nullptr), replacements);

  // Bind the 'Self' type variable to the provided type.
  CanType selfType = genericSig->getGenericParams()[0]->getCanonicalType();
  auto selfTypeVar = replacements[selfType];
  cs.addConstraint(ConstraintKind::Bind, selfTypeVar, type, nullptr);

  // If we can solve the solution, the protocol extension is usable.
  SmallVector<Solution, 1> solutions;
  return !cs.solve(solutions);
}
