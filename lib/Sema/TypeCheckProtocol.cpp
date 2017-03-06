//===--- TypeCheckProtocol.cpp - Protocol Checking ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/Basic/StringExtras.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/TypeMatcher.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Defer.h"
#include "swift/Sema/IDETypeChecking.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"

#define DEBUG_TYPE "protocol-conformance-checking"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {
  struct RequirementMatch;
  struct RequirementCheck;

  /// Describes the environment of a requirement that will be used when
  /// matching witnesses against the requirement and to form the resulting
  /// \c Witness value.
  ///
  /// The produced generic environment will have a fresh set of archetypes that
  /// describe the combined constraints of the requirement (because those
  /// are available to all potential witnesses) as well as the constraints from
  /// the context to which the protocol conformance is ascribed, which may
  /// include additional constraints beyond those of the extended type if the
  /// conformance is conditional. The type parameters for the generic
  /// environment are the type parameters of the conformance context
  /// (\c conformanceDC) with another (deeper) level of type parameters for
  /// generic requirements. See the \c Witness class for more information about
  /// this synthetic environment.
  class RequirementEnvironment {
    GenericSignature *syntheticSignature = nullptr;
    GenericEnvironment *syntheticEnvironment = nullptr;
    GenericSignature *reqSig = nullptr;
    SubstitutionMap reqToSyntheticEnvMap;

  public:
    /// Create a new environment for matching the given requirement within a
    /// particular conformance.
    ///
    /// \param conformanceDC The \c DeclContext to which the protocol
    /// conformance is ascribed, which provides additional constraints.
    ///
    /// \param req The requirement for which we are creating a generic
    /// environment.
    ///
    /// \param conformance The protocol conformance, or null if there is no
    /// conformance (because we're finding default implementations).
    RequirementEnvironment(TypeChecker &tc,
                           DeclContext *conformanceDC,
                           ValueDecl *req,
                           ProtocolConformance *conformance);

    /// Retrieve the synthetic generic environment.
    GenericEnvironment *getSyntheticEnvironment() const {
      return syntheticEnvironment;
    }

    /// Retrieve the generic signature of the requirement.
    const GenericSignature *getRequirementSignature() const {
      return reqSig;
    }

    /// Retrieve the substitution map that maps the interface types of the
    /// requirement to the interface types of the synthetic environment.
    const SubstitutionMap &getRequirementToSyntheticMap() const {
      return reqToSyntheticEnvMap;
    }
  };

  class WitnessChecker {
  protected:
    TypeChecker &TC;
    ProtocolDecl *Proto;
    Type Adoptee;
    // The conforming context, either a nominal type or extension.
    DeclContext *DC;

    WitnessChecker(TypeChecker &tc, ProtocolDecl *proto,
                   Type adoptee, DeclContext *dc)
      : TC(tc), Proto(proto), Adoptee(adoptee), DC(dc) { }

    /// Gather the value witnesses for the given requirement.
    ///
    /// \param ignoringNames If non-null and there are no value
    /// witnesses with the correct full name, the results will reflect
    /// lookup for just the base name and the pointee will be set to
    /// \c true.
    SmallVector<ValueDecl *, 4> lookupValueWitnesses(ValueDecl *req,
                                                     bool *ignoringNames);

    bool findBestWitness(ValueDecl *requirement,
                         bool *ignoringNames,
                         NormalProtocolConformance *conformance,
                         const RequirementEnvironment &reqEnvironment,
                         SmallVectorImpl<RequirementMatch> &matches,
                         unsigned &numViable,
                         unsigned &bestIdx,
                         bool &doNotDiagnoseMatches);

    bool checkWitnessAccessibility(AccessScope &requiredAccessScope,
                                   ValueDecl *requirement,
                                   ValueDecl *witness,
                                   bool *isSetter);

    bool checkWitnessAvailability(ValueDecl *requirement,
                                  ValueDecl *witness,
                                  AvailabilityContext *requirementInfo);

    RequirementCheck checkWitness(AccessScope requiredAccessScope,
                                  ValueDecl *requirement,
                                  RequirementMatch match);
  };

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

    /// The witness is not rethrows, but the requirement is.
    RethrowsConflict,

    /// The witness is explicitly @nonobjc but the requirement is @objc.
    NonObjC,
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

  /// Once a witness has been found, there are several reasons it may
  /// not be usable.
  enum class CheckKind : unsigned {
    /// The witness is OK.
    Success,

    /// The witness is less accessible than the requirement.
    Accessibility,

    /// The witness is storage whose setter is less accessible than the
    /// requirement.
    AccessibilityOfSetter,

    /// The witness is less available than the requirement.
    Availability,

    /// The requirement was marked explicitly unavailable.
    Unavailable,

    /// The witness requires optional adjustments.
    OptionalityConflict,

    /// The witness is a constructor which is more failable than the
    /// requirement.
    ConstructorFailability,
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

      llvm_unreachable("Unhandled OptionalAdjustmentKind in switch.");
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
      case MatchKind::RethrowsConflict:
      case MatchKind::ThrowsConflict:
      case MatchKind::NonObjC:
        return false;
      }

      llvm_unreachable("Unhandled MatchKind in switch.");
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
      case MatchKind::RethrowsConflict:
      case MatchKind::ThrowsConflict:
      case MatchKind::NonObjC:
        return false;
      }

      llvm_unreachable("Unhandled MatchKind in switch.");
    }

    SmallVector<Substitution, 2> WitnessSubstitutions;

    swift::Witness getWitness(ASTContext &ctx,
                              RequirementEnvironment &&reqEnvironment) const {
      SmallVector<Substitution, 2> syntheticSubs;
      auto syntheticEnv = reqEnvironment.getSyntheticEnvironment();
      reqEnvironment.getRequirementSignature()->getSubstitutions(
          reqEnvironment.getRequirementToSyntheticMap(),
          syntheticSubs);
      return swift::Witness(this->Witness, WitnessSubstitutions,
                            syntheticEnv, syntheticSubs);
    }

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

  /// \brief Describes the suitability of the chosen witness for
  /// the requirement.
  struct RequirementCheck {
    CheckKind Kind;

    /// The required access scope, if the check failed due to the
    /// witness being less accessible than the requirement.
    AccessScope RequiredAccessScope;

    /// The required availability, if the check failed due to the
    /// witness being less available than the requirement.
    AvailabilityContext RequiredAvailability;

    RequirementCheck(CheckKind kind)
      : Kind(kind), RequiredAccessScope(AccessScope::getPublic()),
        RequiredAvailability(AvailabilityContext::alwaysAvailable()) { }

    RequirementCheck(CheckKind kind, AccessScope requiredAccessScope)
      : Kind(kind), RequiredAccessScope(requiredAccessScope),
        RequiredAvailability(AvailabilityContext::alwaysAvailable()) { }

    RequirementCheck(CheckKind kind, AvailabilityContext requiredAvailability)
      : Kind(kind), RequiredAccessScope(AccessScope::getPublic()),
        RequiredAvailability(requiredAvailability) { }
  };
} // end anonymous namespace

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

/// If the given type is a direct reference to an associated type of
/// the given protocol, return the referenced associated type.
static AssociatedTypeDecl *
getReferencedAssocTypeOfProtocol(Type type, ProtocolDecl *proto) {
  if (auto dependentMember = type->getAs<DependentMemberType>()) {
    if (auto assocType = dependentMember->getAssocType()) {
      if (dependentMember->getBase()->isEqual(proto->getSelfInterfaceType())) {
        // Exact match: this is our associated type.
        if (assocType->getProtocol() == proto)
          return assocType;

        // Check whether there is an associated type of the same name in
        // this protocol.
        for (auto member : proto->lookupDirect(assocType->getFullName())) {
          if (auto protoAssoc = dyn_cast<AssociatedTypeDecl>(member))
            return protoAssoc;
        }
      }
    }
  }

  return nullptr;
}

namespace {
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
        witnessDecl->getDeclContext()->getDeclaredInterfaceType())
    if (contextType->hasReferenceSemantics())
      return false;
  
  // Determine whether the witness will be mutating or not.  If the witness is
  // stored property accessor, it may not be synthesized yet.
  bool witnessMutating;
  if (witness)
    witnessMutating = (requirement->isInstanceMember() &&
                       witness->isMutating());
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

  // Requirements in class-bound protocols never 'mutate' self.
  auto *proto = cast<ProtocolDecl>(requirement->getDeclContext());
  bool requirementMutating = (requirement->isMutating() &&
                              !proto->requiresClass());

  // The witness must not be more mutating than the requirement.
  return !requirementMutating && witnessMutating;
}

/// Check that the Objective-C method(s) provided by the witness have
/// the same selectors as those required by the requirement.
static bool checkObjCWitnessSelector(TypeChecker &tc, ValueDecl *req,
                                     ValueDecl *witness) {
  // Simple case: for methods and initializers, check that the selectors match.
  if (auto reqFunc = dyn_cast<AbstractFunctionDecl>(req)) {
    auto witnessFunc = cast<AbstractFunctionDecl>(witness);
    if (reqFunc->getObjCSelector() == witnessFunc->getObjCSelector())
      return false;

    auto diagInfo = getObjCMethodDiagInfo(witnessFunc);
    auto diag = tc.diagnose(witness, diag::objc_witness_selector_mismatch,
                            diagInfo.first, diagInfo.second,
                            witnessFunc->getObjCSelector(),
                            reqFunc->getObjCSelector());
    fixDeclarationObjCName(diag, witnessFunc, reqFunc->getObjCSelector());

    return true;
  }

  // Otherwise, we have an abstract storage declaration.
  auto reqStorage = cast<AbstractStorageDecl>(req);
  auto witnessStorage = cast<AbstractStorageDecl>(witness);

  // FIXME: Check property names!

  // Check the getter.
  if (auto reqGetter = reqStorage->getGetter()) {
    if (checkObjCWitnessSelector(tc, reqGetter, witnessStorage->getGetter()))
      return true;
  }

  // Check the setter.
  if (auto reqSetter = reqStorage->getSetter()) {
    if (checkObjCWitnessSelector(tc, reqSetter, witnessStorage->getSetter()))
      return true;
  }

  return false;
}

/// \brief Match the given witness to the given requirement.
///
/// \returns the result of performing the match.
static RequirementMatch
matchWitness(TypeChecker &tc,
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
  if (witness->isInvalid() || !witness->hasValidSignature())
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
    if (funcReq->isStatic() != funcWitness->isStatic() &&
        !(funcReq->isOperator() &&
          !funcWitness->getDeclContext()->isTypeContext()))
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

    // If the requirement is rethrows, the witness must either be
    // rethrows or be non-throwing.
    if (reqAttrs.hasAttribute<RethrowsAttr>() &&
        !witnessAttrs.hasAttribute<RethrowsAttr>() &&
        cast<AbstractFunctionDecl>(witness)->hasThrows())
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

  // If the requirement is @objc, the witness must not be marked with @nonobjc.
  // @objc-ness will be inferred (separately) and the selector will be checked
  // later.
  if (req->isObjC() && witness->getAttrs().hasAttribute<NonObjCAttr>())
    return RequirementMatch(witness, MatchKind::NonObjC);

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
  bool anyRenaming = req->getFullName() != witness->getFullName();
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

RequirementEnvironment::RequirementEnvironment(
                                           TypeChecker &tc,
                                           DeclContext *conformanceDC,
                                           ValueDecl *req,
                                           ProtocolConformance *conformance) {

  auto reqDC = req->getInnermostDeclContext();
  reqSig = reqDC->getGenericSignatureOfContext();

  ASTContext &ctx = tc.Context;
  auto proto = cast<ProtocolDecl>(req->getDeclContext());

  // Build a substitution map to replace the protocol's \c Self and the type
  // parameters of the requirement into a combined context that provides the
  // type parameters of the conformance context and the parameters of the
  // requirement.
  auto selfType = cast<GenericTypeParamType>(
                            proto->getSelfInterfaceType()->getCanonicalType());

  // 'Self' is always at depth 0, index 0. Anything else is invalid code.
  if (selfType->getDepth() != 0 || selfType->getIndex() != 0)
    return;

  // Construct a generic signature builder by collecting the constraints from the
  // requirement and the context of the conformance together, because both
  // define the capabilities of the requirement.
  GenericSignatureBuilder builder(
           ctx,
           LookUpConformanceInModule(conformanceDC->getParentModule()));
  SmallVector<GenericTypeParamType*, 4> allGenericParams;

  // Add the generic signature of the context of the conformance. This includes
  // the generic parameters from the conforming type as well as any additional
  // constraints that might occur on the extension that declares the
  // conformance (i.e., if the conformance is conditional).
  unsigned depth = 0;
  if (auto *conformanceSig = conformanceDC->getGenericSignatureOfContext()) {
    // Use the canonical signature here.
    conformanceSig = conformanceSig->getCanonicalSignature();
    allGenericParams.append(conformanceSig->getGenericParams().begin(),
                            conformanceSig->getGenericParams().end());
    builder.addGenericSignature(conformanceSig);
    depth = allGenericParams.back()->getDepth() + 1;
  }

  // Add the generic signature of the requirement, substituting our concrete
  // type for 'Self'. We don't need the 'Self' requirement or parameter.
  auto concreteType = conformanceDC->getSelfInterfaceType();

  reqToSyntheticEnvMap = reqSig->getSubstitutionMap(
    [selfType, concreteType, depth, &ctx](SubstitutableType *type) -> Type {
      if (type->isEqual(selfType))
        return concreteType;
      auto *genericParam = cast<GenericTypeParamType>(type);
      if (genericParam->getDepth() != 1)
        return Type();
      auto substGenericParam =
        GenericTypeParamType::get(depth, genericParam->getIndex(), ctx);
      return substGenericParam;
    },
    [selfType, concreteType, conformance, conformanceDC, &ctx, &tc](
        CanType type, Type replacement, ProtocolType *protoType)
          -> Optional<ProtocolConformanceRef> {
      auto proto = protoType->getDecl();
      if (type->isEqual(selfType)) {
        ProtocolConformance *specialized = conformance;
        if (conformance && conformance->getGenericSignature()) {
          auto concreteSubs =
            concreteType->gatherAllSubstitutions(conformanceDC->getParentModule(),
                                                 &tc, nullptr);
          specialized =
            ctx.getSpecializedConformance(concreteType, conformance, concreteSubs);
        }

        if (specialized)
          return ProtocolConformanceRef(specialized);
      }

      return ProtocolConformanceRef(proto);
    });

  // First, add the generic parameters from the requirement.
  for (auto genericParam : reqSig->getGenericParams().slice(1)) {
    // The only depth that makes sense is depth == 1, the generic parameters
    // of the requirement itself. Anything else is from invalid code.
    if (genericParam->getDepth() != 1) {
      return;
    }

    // Create an equivalent generic parameter at the next depth.
    auto substGenericParam =
      GenericTypeParamType::get(depth, genericParam->getIndex(), ctx);

    allGenericParams.push_back(substGenericParam);
    builder.addGenericParameter(substGenericParam);
  }

  // If there were no generic parameters, we're done.
  if (allGenericParams.empty()) return;

  // Next, add each of the requirements (mapped from the requirement's
  // interface types into the abstract type parameters).
  auto source =
    GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
  for (auto &reqReq : reqSig->getRequirements()) {
    switch (reqReq.getKind()) {
    case RequirementKind::Conformance: {
      // Substitute the constrained types.
      auto first = reqReq.getFirstType().subst(reqToSyntheticEnvMap,
                                               SubstFlags::UseErrorType);
      if (!first->isTypeParameter()) break;

      builder.addRequirement(Requirement(RequirementKind::Conformance,
                                         first, reqReq.getSecondType()),
                             source);
      break;
    }

    case RequirementKind::Layout: {
      // Substitute the constrained types.
      auto first = reqReq.getFirstType().subst(reqToSyntheticEnvMap);
      if (!first->isTypeParameter()) break;

      builder.addRequirement(Requirement(RequirementKind::Layout, first,
                                         reqReq.getLayoutConstraint()),
                             source);
      break;
    }

    case RequirementKind::Superclass: {
      // Substitute the constrained types.
      auto first = reqReq.getFirstType().subst(reqToSyntheticEnvMap,
                                               SubstFlags::UseErrorType);
      auto second = reqReq.getSecondType().subst(reqToSyntheticEnvMap,
                                                 SubstFlags::UseErrorType);

      if (!first->isTypeParameter()) break;

      builder.addRequirement(Requirement(RequirementKind::Superclass,
                                         first, second),
                             source);
      break;
    }

    case RequirementKind::SameType: {
      // Substitute the constrained types.
      auto first = reqReq.getFirstType().subst(reqToSyntheticEnvMap,
                                               SubstFlags::UseErrorType);
      auto second = reqReq.getSecondType().subst(reqToSyntheticEnvMap,
                                                 SubstFlags::UseErrorType);

      // FIXME: We really want to check hasTypeParameter here, but the
      // GenericSignatureBuilder isn't ready for that.
      if (!first->isTypeParameter()) {
        // When the second is not a type parameter either, drop the requirement.
        // If the types were different, this requirement will be unsatisfiable.
        if (!second->isTypeParameter()) continue;

        // Put the type parameter first.
        std::swap(first, second);
      }

      builder.addRequirement(Requirement(RequirementKind::SameType,
                                         first, second),
                             source);
      break;
    }
    }
  }

  // Finalize the generic signature builder.
  // FIXME: Pass in a source location for the conformance, perhaps? It seems
  // like this could fail.
  builder.finalize(SourceLoc(), allGenericParams);

  // Produce the generic signature and environment.
  syntheticSignature = builder.getGenericSignature();
  syntheticEnvironment = syntheticSignature->createGenericEnvironment(
                                             *conformanceDC->getParentModule());
}

static RequirementMatch
matchWitness(TypeChecker &tc,
             ProtocolDecl *proto,
             ProtocolConformance *conformance,
             DeclContext *dc, ValueDecl *req, ValueDecl *witness,
             const RequirementEnvironment &reqEnvironment) {
  using namespace constraints;

  // Initialized by the setup operation.
  Optional<ConstraintSystem> cs;
  ConstraintLocator *locator = nullptr;
  ConstraintLocator *reqLocator = nullptr;
  ConstraintLocator *witnessLocator = nullptr;
  Type witnessType, openWitnessType;
  Type openedFullWitnessType;
  Type reqType, openedFullReqType;

  // Set up the constraint system for matching.
  auto setup = [&]() -> std::tuple<Optional<RequirementMatch>, Type, Type> {
    // Construct a constraint system to use to solve the equality between
    // the required type and the witness type.
    cs.emplace(tc, dc, ConstraintSystemOptions());

    auto reqGenericEnv = reqEnvironment.getSyntheticEnvironment();
    auto &reqSubMap = reqEnvironment.getRequirementToSyntheticMap();

    Type selfTy = proto->getSelfInterfaceType().subst(reqSubMap);
    if (reqGenericEnv)
      selfTy = reqGenericEnv->mapTypeIntoContext(selfTy);

        // Open up the type of the requirement.
    reqLocator = cs->getConstraintLocator(
                     static_cast<Expr *>(nullptr),
                     LocatorPathElt(ConstraintLocator::Requirement, req));
    llvm::DenseMap<CanType, TypeVariableType *> reqReplacements;
    std::tie(openedFullReqType, reqType)
      = cs->getTypeOfMemberReference(selfTy, req, dc,
                                     /*isTypeReference=*/false,
                                     /*isDynamicResult=*/false,
                                     FunctionRefKind::DoubleApply,
                                     reqLocator,
                                     /*base=*/nullptr,
                                     &reqReplacements);
    reqType = reqType->getRValueType();

    // For any type parameters we replaced in the witness, map them
    // to the corresponding archetypes in the witness's context.
    for (const auto &replacement : reqReplacements) {
      auto replacedInReq = replacement.first.subst(reqSubMap);

      // If substitution failed, skip the requirement. This only occurs in
      // invalid code.
      if (!replacedInReq)
        continue;

      if (reqGenericEnv) {
        replacedInReq = reqGenericEnv->mapTypeIntoContext(replacedInReq);
      }

      cs->addConstraint(ConstraintKind::Bind, replacement.second, replacedInReq,
                        reqLocator);
    }

    // Open up the witness type.
    witnessType = witness->getInterfaceType();
    // FIXME: witness as a base locator?
    locator = cs->getConstraintLocator(nullptr);
    witnessLocator = cs->getConstraintLocator(
                       static_cast<Expr *>(nullptr),
                       LocatorPathElt(ConstraintLocator::Witness, witness));
    llvm::DenseMap<CanType, TypeVariableType *> witnessReplacements;
    if (witness->getDeclContext()->isTypeContext()) {
      std::tie(openedFullWitnessType, openWitnessType) 
        = cs->getTypeOfMemberReference(selfTy, witness, dc,
                                       /*isTypeReference=*/false,
                                       /*isDynamicResult=*/false,
                                       FunctionRefKind::DoubleApply,
                                       witnessLocator,
                                       /*base=*/nullptr,
                                       &witnessReplacements);
    } else {
      std::tie(openedFullWitnessType, openWitnessType) 
        = cs->getTypeOfReference(witness,
                                 /*isTypeReference=*/false,
                                 /*isSpecialized=*/false,
                                 FunctionRefKind::DoubleApply,
                                 witnessLocator,
                                 /*base=*/nullptr);
    }
    openWitnessType = openWitnessType->getRValueType();

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
    // Try to solve the system disallowing free type variables, because
    // that would resolve in incorrect substitution matching when witness
    // type has free type variables present as well.
    auto solution = cs->solveSingle(FreeTypeVariableBinding::Disallow);
    if (!solution)
      return RequirementMatch(witness, MatchKind::TypeConflict,
                              witnessType);

    // Success. Form the match result.
    RequirementMatch result(witness,
                            hasAnyError(optionalAdjustments)
                              ? MatchKind::OptionalityConflict
                              : anyRenaming ? MatchKind::RenamedMatch
                                            : MatchKind::ExactMatch,
                            witnessType,
                            optionalAdjustments);

    // Record the substitutions.
    if (openedFullWitnessType->hasTypeVariable()) {
      // Figure out the context we're substituting into.
      auto witnessDC = witness->getInnermostDeclContext();

      // Compute the set of substitutions we'll need for the witness.
      solution->computeSubstitutions(witnessDC->getGenericSignatureOfContext(),
                                     openedFullWitnessType,
                                     witnessLocator,
                                     result.WitnessSubstitutions);
    }
    
    return result;
  };

  return matchWitness(tc, dc, req, witness, setup, matchTypes, finalize);
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

SmallVector<ValueDecl *, 4> 
WitnessChecker::lookupValueWitnesses(ValueDecl *req, bool *ignoringNames) {
  assert(!isa<AssociatedTypeDecl>(req) && "Not for lookup for type witnesses*");
  
  SmallVector<ValueDecl *, 4> witnesses;
  if (req->getName().isOperator()) {
    // Operator lookup is always global.
    auto lookupOptions = defaultUnqualifiedLookupOptions;
    if (!DC->isCascadingContextForLookup(false))
      lookupOptions |= NameLookupFlags::KnownPrivate;
    auto lookup = TC.lookupUnqualified(DC->getModuleScopeContext(),
                                       req->getName(),
                                       SourceLoc(),
                                       lookupOptions);
    for (auto candidate : lookup) {
      witnesses.push_back(candidate.Decl);
    }
  } else {
    // Variable/function/subscript requirements.
    auto lookupOptions = defaultMemberTypeLookupOptions;
    lookupOptions -= NameLookupFlags::PerformConformanceCheck;

    auto candidates = TC.lookupMember(DC, Adoptee, req->getFullName(),
                                      lookupOptions);

    // If we didn't find anything with the appropriate name, look
    // again using only the base name.
    if (candidates.empty() && ignoringNames) {
      candidates = TC.lookupMember(DC, Adoptee, req->getName(),
                                   lookupOptions);
      *ignoringNames = true;
    }

    for (auto candidate : candidates) {
      witnesses.push_back(candidate);
    }
  }

  return witnesses;
}

bool WitnessChecker::findBestWitness(
                               ValueDecl *requirement,
                               bool *ignoringNames,
                               NormalProtocolConformance *conformance,
                               const RequirementEnvironment &reqEnvironment,
                               SmallVectorImpl<RequirementMatch> &matches,
                               unsigned &numViable,
                               unsigned &bestIdx,
                               bool &doNotDiagnoseMatches) {
  auto witnesses = lookupValueWitnesses(requirement, ignoringNames);

  // Match each of the witnesses to the requirement.
  bool anyFromUnconstrainedExtension = false;
  numViable = 0;
  bestIdx = 0;

  for (auto witness : witnesses) {
    // Don't match anything in a protocol.
    // FIXME: When default implementations come along, we can try to match
    // these when they're default implementations coming from another
    // (unrelated) protocol.
    if (isa<ProtocolDecl>(witness->getDeclContext())) {
      continue;
    }

    if (!witness->hasInterfaceType())
      TC.validateDecl(witness);

    auto match = matchWitness(TC, Proto, conformance, DC,
                              requirement, witness, reqEnvironment);
    if (match.isViable()) {
      ++numViable;
      bestIdx = matches.size();
    } else if (match.Kind == MatchKind::WitnessInvalid) {
      doNotDiagnoseMatches = true;
    }

    if (auto *ext = dyn_cast<ExtensionDecl>(match.Witness->getDeclContext()))
      if (!ext->isConstrainedExtension() && ext->getAsProtocolExtensionContext())
        anyFromUnconstrainedExtension = true;

    matches.push_back(std::move(match));
  }

  if (numViable == 0) {
    if (anyFromUnconstrainedExtension &&
        conformance != nullptr &&
        conformance->isInvalid()) {
      doNotDiagnoseMatches = true;
    }

    return false;
  }

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

  // If there are multiple equally-good candidates, we fail.
  return isReallyBest;
}

bool WitnessChecker::
checkWitnessAccessibility(AccessScope &requiredAccessScope,
                          ValueDecl *requirement,
                          ValueDecl *witness,
                          bool *isSetter) {
  *isSetter = false;

  auto scopeIntersection =
    requiredAccessScope.intersectWith(Proto->getFormalAccessScope(DC));
  assert(scopeIntersection.hasValue());

  requiredAccessScope = *scopeIntersection;

  AccessScope actualScopeToCheck = requiredAccessScope;
  if (!witness->isAccessibleFrom(actualScopeToCheck.getDeclContext())) {
    // Special case: if we have `@testable import` of the witness's module,
    // allow the witness to match if it would have matched for just this file.
    // That is, if '@testable' allows us to see the witness here, it should
    // allow us to see it anywhere, because any other client could also add
    // their own `@testable import`.
    if (auto parentFile = dyn_cast<SourceFile>(DC->getModuleScopeContext())) {
      const ModuleDecl *witnessModule = witness->getModuleContext();
      if (parentFile->getParentModule() != witnessModule &&
          parentFile->hasTestableImport(witnessModule) &&
          witness->isAccessibleFrom(parentFile)) {
        actualScopeToCheck = parentFile;
      }
    }

    if (actualScopeToCheck.hasEqualDeclContextWith(requiredAccessScope))
      return true;
  }

  if (requirement->isSettable(DC)) {
    *isSetter = true;

    auto ASD = cast<AbstractStorageDecl>(witness);
    if (!ASD->isSetterAccessibleFrom(actualScopeToCheck.getDeclContext()))
      return true;
  }

  return false;
}

bool WitnessChecker::
checkWitnessAvailability(ValueDecl *requirement,
                         ValueDecl *witness,
                         AvailabilityContext *requiredAvailability) {
  return (!TC.getLangOpts().DisableAvailabilityChecking &&
          !TC.isAvailabilitySafeForConformance(Proto, requirement, witness,
                                               DC, *requiredAvailability));
}

RequirementCheck WitnessChecker::
checkWitness(AccessScope requiredAccessScope,
             ValueDecl *requirement,
             RequirementMatch match) {
  if (!match.OptionalAdjustments.empty())
    return CheckKind::OptionalityConflict;

  bool isSetter = false;
  if (checkWitnessAccessibility(requiredAccessScope, requirement,
                                match.Witness, &isSetter)) {
    CheckKind kind = (isSetter
                      ? CheckKind::AccessibilityOfSetter
                      : CheckKind::Accessibility);
    return RequirementCheck(kind, requiredAccessScope);
  }

  auto requiredAvailability = AvailabilityContext::alwaysAvailable();
  if (checkWitnessAvailability(requirement, match.Witness,
                               &requiredAvailability)) {
    return RequirementCheck(CheckKind::Availability, requiredAvailability);
  }

  if (requirement->getAttrs().isUnavailable(TC.Context) &&
      match.Witness->getDeclContext() == DC) {
    return RequirementCheck(CheckKind::Unavailable);
  }

  // A non-failable initializer requirement cannot be satisfied
  // by a failable initializer.
  if (auto ctor = dyn_cast<ConstructorDecl>(requirement)) {
    if (ctor->getFailability() == OTK_None) {
      auto witnessCtor = cast<ConstructorDecl>(match.Witness);

      switch (witnessCtor->getFailability()) {
      case OTK_None:
        // Okay
        break;

      case OTK_ImplicitlyUnwrappedOptional:
        // Only allowed for non-@objc protocols.
        if (!Proto->isObjC())
          break;

        LLVM_FALLTHROUGH;

      case OTK_Optional:
        return CheckKind::ConstructorFailability;
      }
    }
  }

  return CheckKind::Success;
}

# pragma mark Witness resolution

namespace {
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
    NominalTypeDecl *Nominal = nullptr;

  public:
    CheckTypeWitnessResult() { }

    CheckTypeWitnessResult(NominalTypeDecl *nominal) : Nominal(nominal) {
      assert(isa<ProtocolDecl>(nominal) || isa<ClassDecl>(nominal));
    }

    NominalTypeDecl *getProtocolOrClass() const { return Nominal; }
    bool isProtocol() const { return isa<ProtocolDecl>(Nominal); }

    explicit operator bool() const { return Nominal != nullptr; }
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

    void dump(llvm::raw_ostream &out, unsigned indent) const {
      out << "\n";
      out.indent(indent) << "(";
      if (Witness) {
        Witness->dumpRef(out);
      }

      for (const auto &inferred : Inferred) {
        out << "\n";
        out.indent(indent + 2);
        out << inferred.first->getName() << " := "
            << inferred.second.getString();
      }

      for (const auto &inferred : NonViable) {
        out << "\n";
        out.indent(indent + 2);
        out << std::get<0>(inferred)->getName() << " := "
            << std::get<1>(inferred).getString();
        if (auto nominal = std::get<2>(inferred).getProtocolOrClass())
          out << " [failed constraint " << nominal->getName() << "]";
      }

      out << ")";
    }

    LLVM_ATTRIBUTE_DEPRECATED(void dump() const,
                              "only for use in the debugger");
  };

  void InferredAssociatedTypesByWitness::dump() const {
    dump(llvm::errs(), 0);
  }

  /// The set of witnesses that were considered when attempting to
  /// infer associated types.
  typedef SmallVector<InferredAssociatedTypesByWitness, 2>
    InferredAssociatedTypesByWitnesses;

  void dumpInferredAssociatedTypesByWitnesses(
        const InferredAssociatedTypesByWitnesses &inferred,
        llvm::raw_ostream &out,
        unsigned indent) {
    for (const auto &value : inferred) {
      value.dump(out, indent);
    }
  }

  void dumpInferredAssociatedTypesByWitnesses(
        const InferredAssociatedTypesByWitnesses &inferred) LLVM_ATTRIBUTE_USED;

  void dumpInferredAssociatedTypesByWitnesses(
                          const InferredAssociatedTypesByWitnesses &inferred) {
    dumpInferredAssociatedTypesByWitnesses(inferred, llvm::errs(), 0);
  }

  /// A mapping from requirements to the set of matches with witnesses.
  typedef SmallVector<std::pair<ValueDecl *,
                                InferredAssociatedTypesByWitnesses>, 4>
    InferredAssociatedTypes;

  void dumpInferredAssociatedTypes(const InferredAssociatedTypes &inferred,
                                   llvm::raw_ostream &out,
                                   unsigned indent) {
    for (const auto &value : inferred) {
      out << "\n";
      out.indent(indent) << "(";
      value.first->dumpRef(out);
      dumpInferredAssociatedTypesByWitnesses(value.second, out, indent + 2);
      out << ")";
    }
    out << "\n";
  }

  void dumpInferredAssociatedTypes(
         const InferredAssociatedTypes &inferred) LLVM_ATTRIBUTE_USED;

  void dumpInferredAssociatedTypes(const InferredAssociatedTypes &inferred) {
    dumpInferredAssociatedTypes(inferred, llvm::errs(), 0);
  }

  enum class MissingWitnessDiagnosisKind {
    FixItOnly,
    ErrorOnly,
    ErrorFixIt,
  };

  /// The protocol conformance checker.
  ///
  /// This helper class handles most of the details of checking whether a
  /// given type (\c Adoptee) conforms to a protocol (\c Proto).
  class ConformanceChecker : public WitnessChecker {
    friend class MultiConformanceChecker;
    NormalProtocolConformance *Conformance;
    SourceLoc Loc;
    
    /// Witnesses that are currently being resolved.
    llvm::SmallPtrSet<ValueDecl *, 4> ResolvingWitnesses;

    /// Caches the set of associated types that are referenced in each
    /// requirement.
    llvm::DenseMap<ValueDecl *, llvm::SmallVector<AssociatedTypeDecl *, 2>>
      ReferencedAssociatedTypes;

    /// Keep track of missing witnesses, either type or value, for later
    /// diagnosis emits. This may contain witnesses that are external to the
    /// protocol under checking.
    llvm::SetVector<ValueDecl*> &GlobalMissingWitnesses;

    /// Keep track of the slice in GlobalMissingWitnesses that is local to
    /// this protocol under checking.
    unsigned LocalMissingWitnessesStartIndex;

    /// True if we shouldn't complain about problems with this conformance
    /// right now, i.e. if methods are being called outside
    /// checkConformance().
    bool SuppressDiagnostics;

    /// Whether we've already complained about problems with this conformance.
    bool AlreadyComplained = false;

    /// Retrieve the associated types that are referenced by the given
    /// requirement with a base of 'Self'.
    ArrayRef<AssociatedTypeDecl *> getReferencedAssociatedTypes(ValueDecl *req);

    /// Record a (non-type) witness for the given requirement.
    void recordWitness(ValueDecl *requirement, const RequirementMatch &match,
                       RequirementEnvironment &&reqEnvironment);

    /// Record that the given optional requirement has no witness.
    void recordOptionalWitness(ValueDecl *requirement);

    /// Record that the given requirement has no valid witness.
    void recordInvalidWitness(ValueDecl *requirement);

    /// Record a type witness.
    ///
    /// \param assocType The associated type whose witness is being recorded.
    ///
    /// \param type The witness type.
    ///
    /// \param typeDecl The decl the witness type came from; can be null.
    void recordTypeWitness(AssociatedTypeDecl *assocType, Type type,
                           TypeDecl *typeDecl, bool performRedeclarationCheck);

    /// Resolve a (non-type) witness via name lookup.
    ResolveWitnessResult resolveWitnessViaLookup(ValueDecl *requirement);

    /// Resolve a (non-type) witness via derivation.
    ResolveWitnessResult resolveWitnessViaDerivation(ValueDecl *requirement);

    /// Resolve a (non-type) witness via default definition or optional.
    ResolveWitnessResult resolveWitnessViaDefault(ValueDecl *requirement);

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
                     const llvm::SetVector<AssociatedTypeDecl *> &allUnresolved,
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
           std::function<void(NormalProtocolConformance *)> fn);

    void
    addUsedConformances(ProtocolConformance *conformance,
                        llvm::SmallPtrSetImpl<ProtocolConformance *> &visited);
    void addUsedConformances(ProtocolConformance *conformance);

    ArrayRef<ValueDecl*> getLocalMissingWitness() {
      return GlobalMissingWitnesses.getArrayRef().
        slice(LocalMissingWitnessesStartIndex,
              GlobalMissingWitnesses.size() - LocalMissingWitnessesStartIndex);
    }

  public:
    /// Call this to diagnose currently known missing witnesses.
    void diagnoseMissingWitnesses(MissingWitnessDiagnosisKind Kind);
    /// Emit any diagnostics that have been delayed.
    void emitDelayedDiags();

    ConformanceChecker(TypeChecker &tc, NormalProtocolConformance *conformance,
                       llvm::SetVector<ValueDecl*> &GlobalMissingWitnesses,
                       bool suppressDiagnostics = true)
        : WitnessChecker(tc, conformance->getProtocol(), conformance->getType(),
                         conformance->getDeclContext()),
          Conformance(conformance), Loc(conformance->getLoc()),
          GlobalMissingWitnesses(GlobalMissingWitnesses),
          LocalMissingWitnessesStartIndex(GlobalMissingWitnesses.size()),
          SuppressDiagnostics(suppressDiagnostics) {
      // The protocol may have only been validatedDeclForNameLookup'd until
      // here, so fill in any information that's missing.
      tc.validateDecl(conformance->getProtocol());
    }

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

    /// Check all of the protocols requirements are actually satisfied by a
    /// the chosen type witnesses.
    void ensureRequirementsAreSatisfied();

    /// Check the entire protocol conformance, ensuring that all
    /// witnesses are resolved and emitting any diagnostics.
    void checkConformance(MissingWitnessDiagnosisKind Kind);
  };

  /// This is a wrapper of multiple instances of ConformanceChecker to allow us
  /// to diagnose and fix code from a more global perspective; for instance,
  /// having this wrapper can help issue a fixit that inserts protocol stubs from
  /// multiple protocols under checking.
  class MultiConformanceChecker {
    TypeChecker &TC;
    llvm::SmallVector<ValueDecl*, 16> UnsatisfiedReqs;
    llvm::SmallVector<ConformanceChecker, 4> AllUsedCheckers;
    llvm::SmallVector<NormalProtocolConformance*, 4> AllConformances;
    llvm::SetVector<ValueDecl*> MissingWitnesses;

    /// Check one conformance.
    ProtocolConformance * checkIndividualConformance(
      NormalProtocolConformance *conformance, bool issueFixit);

    /// Determine whether the given requirement was left unsatisfied.
    bool isUnsatisfiedReq(NormalProtocolConformance *conformance, ValueDecl *req);
  public:
    MultiConformanceChecker(TypeChecker &TC): TC(TC){}

    /// Add a conformance into the batched checker.
    void addConformance(NormalProtocolConformance *conformance) {
      AllConformances.push_back(conformance);
    }

    /// Peek the unsatisfied requirements collected during conformance checking.
    ArrayRef<ValueDecl*> getUnsatisfiedRequirements() {
      return llvm::makeArrayRef(UnsatisfiedReqs);
    }

    /// Check all conformances and emit diagnosis globally.
    void checkAllConformances();
  };

  bool MultiConformanceChecker::
  isUnsatisfiedReq(NormalProtocolConformance *conformance, ValueDecl *req) {
    if (conformance->isInvalid()) return false;
    if (isa<TypeDecl>(req)) return false;

    // An optional requirement might not have a witness...
    if (!conformance->hasWitness(req) ||
        !conformance->getWitness(req, nullptr).getDecl())
      return req->getAttrs().hasAttribute<OptionalAttr>();

    return false;
  }

  void MultiConformanceChecker::checkAllConformances() {
    for(unsigned I = 0, N = AllConformances.size(); I < N; I ++) {
      auto *conformance = AllConformances[I];
      // Check this conformance and emit fixits if this is the last one in the pool.
      checkIndividualConformance(conformance, I == N - 1);

      // Check whether there are any unsatisfied requirements.
      auto proto = conformance->getProtocol();
      for (auto member : proto->getMembers()) {
        auto req = dyn_cast<ValueDecl>(member);
        if (!req || !req->isProtocolRequirement()) continue;

        // If the requirement is unsatisfied, we might want to warn
        // about near misses; record it.
        if (isUnsatisfiedReq(conformance, req)) {
          UnsatisfiedReqs.push_back(req);
          continue;
        }
      }
    }
    // If all missing witnesses are issued with fixits, we are done.
    if (MissingWitnesses.empty())
      return;

    // Otherwise, backtrack to the last checker that has missing witnesses
    // and diagnose missing witnesses from there.
    for (auto It = AllUsedCheckers.rbegin(); It != AllUsedCheckers.rend();
         It ++) {
      if (!It->getLocalMissingWitness().empty()) {
        It->diagnoseMissingWitnesses(MissingWitnessDiagnosisKind::FixItOnly);
      }
    }
  }

  /// \brief Determine whether the type \c T conforms to the protocol \c Proto,
  /// recording the complete witness table if it does.
  ProtocolConformance *MultiConformanceChecker::
  checkIndividualConformance(NormalProtocolConformance *conformance,
                             bool issueFixit) {
    switch (conformance->getState()) {
      case ProtocolConformanceState::Incomplete:
        if (conformance->isInvalid()) {
          // Emit any delayed diagnostics and return.
          ConformanceChecker(TC, conformance, MissingWitnesses, false).
          emitDelayedDiags();
        }

        // Check the rest of the conformance below.
        break;

      case ProtocolConformanceState::CheckingTypeWitnesses:
      case ProtocolConformanceState::Checking:
        // Nothing to do.
        return conformance;

      case ProtocolConformanceState::Complete:
        if (conformance->isInvalid()) {
          // Emit any delayed diagnostics and return.
          // FIXME: Should we complete checking to emit more diagnostics?
          ConformanceChecker(TC, conformance, MissingWitnesses, false).
          emitDelayedDiags();
        }
        return conformance;
    }

    // Dig out some of the fields from the conformance.
    Type T = conformance->getType();
    auto canT = T->getCanonicalType();
    DeclContext *DC = conformance->getDeclContext();
    auto Proto = conformance->getProtocol();
    SourceLoc ComplainLoc = conformance->getLoc();

    // Note that we are checking this conformance now.
    conformance->setState(ProtocolConformanceState::Checking);
    SWIFT_DEFER { conformance->setState(ProtocolConformanceState::Complete); };

    // If the protocol itself is invalid, there's nothing we can do.
    if (Proto->isInvalid()) {
      conformance->setInvalid();
      return conformance;
    }

    // If the protocol requires a class, non-classes are a non-starter.
    if (Proto->requiresClass() && !canT->getClassOrBoundGenericClass()) {
      TC.diagnose(ComplainLoc, diag::non_class_cannot_conform_to_class_protocol,
                  T, Proto->getDeclaredType());
      conformance->setInvalid();
      return conformance;
    }

    // Foreign classes cannot conform to objc protocols.
    if (Proto->isObjC() &&
        !Proto->isSpecificProtocol(KnownProtocolKind::AnyObject)) {
      if (auto clas = canT->getClassOrBoundGenericClass()) {
        Optional<decltype(diag::cf_class_cannot_conform_to_objc_protocol)>
        diagKind;
        switch (clas->getForeignClassKind()) {
          case ClassDecl::ForeignKind::Normal:
            break;
          case ClassDecl::ForeignKind::CFType:
            diagKind = diag::cf_class_cannot_conform_to_objc_protocol;
            break;
          case ClassDecl::ForeignKind::RuntimeOnly:
            diagKind = diag::objc_runtime_visible_cannot_conform_to_objc_protocol;
            break;
        }
        if (diagKind) {
          TC.diagnose(ComplainLoc, diagKind.getValue(),
                      T, Proto->getDeclaredType());
          conformance->setInvalid();
          return conformance;
        }
      }
    }

    // If the protocol contains missing requirements, it can't be conformed to
    // at all.
    if (Proto->hasMissingRequirements()) {
      TC.diagnose(ComplainLoc, diag::protocol_has_missing_requirements,
                  T, Proto->getDeclaredType());
      conformance->setInvalid();
      return conformance;
    }

    // Check that T conforms to all inherited protocols.
    for (auto InheritedProto : Proto->getInheritedProtocols()) {
      auto InheritedConformance =
      TC.conformsToProtocol(T, InheritedProto, DC,
                            ConformanceCheckFlags::Used,
                            ComplainLoc);
      if (InheritedConformance && InheritedConformance->isConcrete()) {
        if (!conformance->hasInheritedConformance(InheritedProto))
          conformance->setInheritedConformance(InheritedProto,
                                           InheritedConformance->getConcrete());
      } else {
        // Recursive call already diagnosed this problem, but tack on a note
        // to establish the relationship.
        if (ComplainLoc.isValid()) {
          TC.diagnose(Proto,
                      diag::inherited_protocol_does_not_conform, T,
                      InheritedProto->getDeclaredType());
        }

        conformance->setInvalid();
        return conformance;
      }
    }

    if (conformance->isComplete())
      return conformance;

    // The conformance checker we're using.
    AllUsedCheckers.emplace_back(TC, conformance, MissingWitnesses);
    AllUsedCheckers.back().checkConformance(issueFixit ?
                                      MissingWitnessDiagnosisKind::ErrorFixIt :
                                      MissingWitnessDiagnosisKind::ErrorOnly);
    return conformance;
  }
} // end anonymous namespace

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
static Type getTypeForDisplay(ModuleDecl *module, ValueDecl *decl) {
  // If we're not in a type context, just grab the interface type.
  Type type = decl->getInterfaceType();
  if (!decl->getDeclContext()->isTypeContext() ||
      !isa<AbstractFunctionDecl>(decl))
    return type;

  // For a constructor, we only care about the parameter types.
  if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    return ctor->getArgumentInterfaceType();
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
static Type getRequirementTypeForDisplay(ModuleDecl *module,
                                         NormalProtocolConformance *conformance,
                                         ValueDecl *req) {
  auto type = getTypeForDisplay(module, req);

  // Replace generic type parameters and associated types with their
  // witnesses, when we have them.
  auto selfTy = conformance->getProtocol()->getSelfInterfaceType();
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

    // Replace 'Self' with the conforming type.
    if (type->isEqual(selfTy))
      return conformance->getType();

    return type;
  });

  //
  return type;
}

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
  ParameterList *params = nullptr;
  if (auto func = dyn_cast<AbstractFunctionDecl>(witness)) {
    auto bodyParamLists = func->getParameterLists();
    if (func->getDeclContext()->isTypeContext())
      bodyParamLists = bodyParamLists.slice(1);
    params = bodyParamLists[0];
  } else if (auto subscript = dyn_cast<SubscriptDecl>(witness)) {
    params = subscript->getIndices();
  } else {
    return SourceLoc();
  }

  return getOptionalityLoc(params->get(getParameterIndex())->getTypeLoc()
                           .getTypeRepr());
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

/// \brief Diagnose a requirement match, describing what went wrong (or not).
static void
diagnoseMatch(ModuleDecl *module, NormalProtocolConformance *conformance,
              ValueDecl *req, const RequirementMatch &match){
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

  auto &diags = module->getASTContext().Diags;
  switch (match.Kind) {
  case MatchKind::ExactMatch:
    diags.diagnose(match.Witness, diag::protocol_witness_exact_match,
                   withAssocTypes);
    break;

  case MatchKind::RenamedMatch: {
    auto diag = diags.diagnose(match.Witness, diag::protocol_witness_renamed,
                               req->getFullName(), withAssocTypes);

    // Fix the name.
    fixDeclarationName(diag, match.Witness, req->getFullName());

    // Also fix the Objective-C name, if needed.
    if (!match.Witness->canInferObjCFromRequirement(req))
      fixDeclarationObjCName(diag, match.Witness, req->getObjCRuntimeName());
    break;
  }

  case MatchKind::KindConflict:
    diags.diagnose(match.Witness, diag::protocol_witness_kind_conflict,
                   getRequirementKind(req));
    break;

  case MatchKind::WitnessInvalid:
    // Don't bother to diagnose invalid witnesses; we've already complained
    // about them.
    break;

  case MatchKind::TypeConflict: {
    auto diag = diags.diagnose(match.Witness, 
                               diag::protocol_witness_type_conflict,
                               getTypeForDisplay(module, match.Witness),
                               withAssocTypes);
    if (!isa<TypeDecl>(req))
      fixItOverrideDeclarationTypes(diag, match.Witness, req);
    break;
  }

  case MatchKind::ThrowsConflict:
    diags.diagnose(match.Witness, diag::protocol_witness_throws_conflict);
    break;

  case MatchKind::OptionalityConflict: {
    auto diag = diags.diagnose(match.Witness, 
                               diag::protocol_witness_optionality_conflict,
                               match.classifyOptionalityIssues(req),
                               withAssocTypes);
    match.addOptionalityFixIts(match.Witness->getASTContext(), match.Witness,
                               diag);
    break;
  }

  case MatchKind::StaticNonStaticConflict:
    // FIXME: Could emit a Fix-It here.
    diags.diagnose(match.Witness, diag::protocol_witness_static_conflict,
                   !req->isInstanceMember());
    break;
      
  case MatchKind::SettableConflict:
    diags.diagnose(match.Witness, diag::protocol_witness_settable_conflict);
    break;

  case MatchKind::PrefixNonPrefixConflict:
    // FIXME: Could emit a Fix-It here.
    diags.diagnose(match.Witness,
                   diag::protocol_witness_prefix_postfix_conflict, false,
                   match.Witness->getAttrs().hasAttribute<PostfixAttr>() ? 2
                                                                         : 0);
    break;

  case MatchKind::PostfixNonPostfixConflict:
    // FIXME: Could emit a Fix-It here.
    diags.diagnose(match.Witness,
                   diag::protocol_witness_prefix_postfix_conflict, true,
                   match.Witness->getAttrs().hasAttribute<PrefixAttr>() ? 1
                                                                        : 0);
    break;
  case MatchKind::MutatingConflict:
    // FIXME: Could emit a Fix-It here.
    diags.diagnose(match.Witness, diag::protocol_witness_mutating_conflict);
    break;
  case MatchKind::RethrowsConflict:
    // FIXME: Could emit a Fix-It here.
    diags.diagnose(match.Witness, diag::protocol_witness_rethrows_conflict);
    break;
  case MatchKind::NonObjC:
    diags.diagnose(match.Witness, diag::protocol_witness_not_objc);
    break;
  }
}

/// Compute the substitution for the given archetype and its replacement
/// type.
static Substitution getArchetypeSubstitution(TypeChecker &tc,
                                             DeclContext *dc,
                                             ArrayRef<ProtocolDecl *> protocols,
                                             Type replacement) {
  assert(!replacement->isTypeParameter() && "Can't be dependent");
  SmallVector<ProtocolConformanceRef, 4> conformances;

  bool isError = replacement->hasError();

  for (auto proto : protocols) {
    auto conformance = tc.conformsToProtocol(replacement, proto, dc, None);
    assert((conformance || isError) &&
           "Conformance should already have been verified");
    (void)isError;
    if (conformance)
      conformances.push_back(*conformance);
    else
      conformances.push_back(ProtocolConformanceRef(proto));
  }

  return Substitution{
    replacement,
    tc.Context.AllocateCopy(conformances),
  };
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
  req->getInterfaceType()->getCanonicalType().visit([&](Type type) {
      if (auto assocType = getReferencedAssocTypeOfProtocol(type, Proto)) {
        if (knownAssocTypes.insert(assocType).second) {
          assocTypes.push_back(assocType);
        }
      }
    });

  return assocTypes;
}

void ConformanceChecker::recordWitness(ValueDecl *requirement,
                                       const RequirementMatch &match,
                                       RequirementEnvironment &&reqEnvironment){
  // If we already recorded this witness, don't do so again.
  if (Conformance->hasWitness(requirement)) {
    assert(Conformance->getWitness(requirement, nullptr).getDecl() ==
             match.Witness && "Deduced different witnesses?");
    return;
  }

  // Record this witness in the conformance.
  auto witness = match.getWitness(TC.Context, std::move(reqEnvironment));
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
  Conformance->setWitness(requirement, Witness());
}

void ConformanceChecker::recordInvalidWitness(ValueDecl *requirement) {
  assert(Conformance->isInvalid());

  // If we already recorded this witness, don't do so again.
  if (Conformance->hasWitness(requirement)) {
    assert(!Conformance->getWitness(requirement, nullptr).getDecl() &&
           "Already have a non-optional witness?");
    return;
  }

  // Record that there is no witness.
  Conformance->setWitness(requirement, Witness());
}

void ConformanceChecker::recordTypeWitness(AssociatedTypeDecl *assocType,
                                           Type type,
                                           TypeDecl *typeDecl,
                                           bool performRedeclarationCheck) {

  // If we already recoded this type witness, there's nothing to do.
  if (Conformance->hasTypeWitness(assocType)) {
    assert(Conformance->getTypeWitness(assocType, nullptr).getReplacement()
             ->isEqual(type) && "Conflicting type witness deductions");
    return;
  }

  if (typeDecl) {
    // Check access.
    AccessScope requiredAccessScope =
        Adoptee->getAnyNominal()->getFormalAccessScope(DC);
    bool isSetter = false;
    if (checkWitnessAccessibility(requiredAccessScope, assocType, typeDecl,
                                  &isSetter)) {
      assert(!isSetter);

      // Avoid relying on the lifetime of 'this'.
      const DeclContext *DC = this->DC;
      diagnoseOrDefer(assocType, false,
        [DC, typeDecl, requiredAccessScope, assocType](
          NormalProtocolConformance *conformance) {
        Accessibility requiredAccess =
          requiredAccessScope.requiredAccessibilityForDiagnostics();
        auto proto = conformance->getProtocol();
        auto protoAccessScope = proto->getFormalAccessScope(DC);
        bool protoForcesAccess =
          requiredAccessScope.hasEqualDeclContextWith(protoAccessScope);
        auto diagKind = protoForcesAccess
                          ? diag::type_witness_not_accessible_proto
                          : diag::type_witness_not_accessible_type;
        auto &diags = DC->getASTContext().Diags;
        auto diag = diags.diagnose(typeDecl, diagKind,
                                   typeDecl->getDescriptiveKind(),
                                   typeDecl->getFullName(),
                                   requiredAccess,
                                   proto->getName());
        fixItAccessibility(diag, typeDecl, requiredAccess);
      });
    }
  } else {
    // If there was no type declaration, synthesize one.

    // If we're just setting an error, double-check that nobody has
    // introduced a type declaration since we deduced one. This can
    // happen when type-checking a different conformance deduces a
    // different type witness with the same name. For non-error cases,
    // the caller handles this.
    if (performRedeclarationCheck && type->hasError()) {
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
                                                    SourceLoc(),
                                                    assocType->getName(),
                                                    SourceLoc(),
                                                    /*genericparams*/nullptr, 
                                                    DC);
    aliasDecl->setGenericEnvironment(DC->getGenericEnvironmentOfContext());
    aliasDecl->setUnderlyingType(type);

    aliasDecl->setImplicit();
    if (type->hasError())
      aliasDecl->setInvalid();

    // Inject the typealias into the nominal decl that conforms to the protocol.
    if (auto nominal = DC->getAsNominalTypeOrNominalTypeExtensionContext()) {
      TC.computeAccessibility(nominal);
      // FIXME: Ideally this would use the protocol's access too---that is,
      // a typealias added for an internal protocol shouldn't need to be
      // public---but that can be problematic if the same type conforms to two
      // protocols with different access levels.
      Accessibility aliasAccess = nominal->getFormalAccess();
      aliasAccess = std::max(aliasAccess, Accessibility::Internal);
      aliasDecl->setAccessibility(aliasAccess);

      if (nominal == DC) {
        nominal->addMember(aliasDecl);
      } else {
        auto ext = cast<ExtensionDecl>(DC);
        ext->addMember(aliasDecl);
      }
    } else {
      // If the declcontext is a Module, then we're in a special error recovery
      // situation.  Mark the typealias as an error and don't inject it into any
      // DeclContext.
      assert(isa<ModuleDecl>(DC) && "Not an UnresolvedType conformance?");
      aliasDecl->setInvalid();
    }

    typeDecl = aliasDecl;
  }

  // Record the type witness.
  auto protocols = assocType->getConformingProtocols();
  Conformance->setTypeWitness(
      assocType,
      getArchetypeSubstitution(TC, DC, protocols, type),
      typeDecl);
}

bool swift::
printRequirementStub(ValueDecl *Requirement, DeclContext *Adopter,
                     Type AdopterTy, SourceLoc TypeLoc, raw_ostream &OS) {

  // FIXME: Infer body indentation from the source rather than hard-coding
  // 4 spaces.
  ASTContext &Ctx = Requirement->getASTContext();
  std::string ExtraIndent = "    ";
  StringRef CurrentIndent = Lexer::getIndentationForLine(Ctx.SourceMgr,
                                                         TypeLoc);
  std::string StubIndent = CurrentIndent.str() + ExtraIndent;

  ExtraIndentStreamPrinter Printer(OS, StubIndent);
  Printer.printNewline();

  Accessibility Access =
    std::min(
      /* Access of the context */
      Adopter->getAsNominalTypeOrNominalTypeExtensionContext()->getFormalAccess(),
      /* Access of the protocol */
      Requirement->getDeclContext()->getAsProtocolOrProtocolExtensionContext()->
        getFormalAccess());
  if (Access == Accessibility::Public)
    Printer << "public ";

  if (auto MissingTypeWitness = dyn_cast<AssociatedTypeDecl>(Requirement)) {
    Printer << "typealias " << MissingTypeWitness->getName() << " = <#type#>";
    Printer << "\n";
  } else {
    if (isa<ConstructorDecl>(Requirement)) {
      if (auto CD = Adopter->getAsClassOrClassExtensionContext()) {
        if (!CD->isFinal() && Adopter->isExtensionContext()) {
          // In this case, user should mark class as 'final' or define
          // 'required' initializer directly in the class definition.
          return false;
        } else if (!CD->isFinal()) {
          Printer << "required ";
        } else if (Adopter->isExtensionContext()) {
          Printer << "convenience ";
        }
      }
    }

    PrintOptions Options = PrintOptions::printForDiagnostics();
    Options.AccessibilityFilter = Accessibility::Private;
    Options.PrintAccessibility = false;
    Options.FunctionBody = [](const ValueDecl *VD) { return "<#code#>"; };
    Options.setBaseType(AdopterTy);
    Options.CurrentModule = Adopter->getParentModule();
    if (!Adopter->isExtensionContext()) {
      // Create a variable declaration instead of a computed property in
      // nominal types
      Options.PrintPropertyAccessors = false;
    }
    Requirement->print(Printer, Options);
    Printer << "\n";
  }
  return true;
}

/// Print the stubs for an array of witnesses, either type or value, to
/// FixitString. If for a witness we cannot have stub printed, insert it to
/// NoStubRequirements.
static void
printProtocolStubFixitString(SourceLoc TypeLoc, ProtocolConformance *Conf,
                             ArrayRef<ValueDecl*> MissingWitnesses,
                             std::string &FixitString,
                             llvm::SetVector<ValueDecl*> &NoStubRequirements) {
  llvm::raw_string_ostream FixitStream(FixitString);
  std::for_each(MissingWitnesses.begin(), MissingWitnesses.end(),
    [&](ValueDecl* VD) {
      if (!printRequirementStub(VD, Conf->getDeclContext(), Conf->getType(),
                                TypeLoc, FixitStream)) {
        NoStubRequirements.insert(VD);
      }
    });
}

void ConformanceChecker::
diagnoseMissingWitnesses(MissingWitnessDiagnosisKind Kind) {
  auto LocalMissing = getLocalMissingWitness();

  // If this conformance has nothing to complain, return.
  if (LocalMissing.empty())
    return;

  llvm::SetVector<ValueDecl*> MissingWitnesses = GlobalMissingWitnesses;
  auto InsertFixitCallback = [MissingWitnesses](NormalProtocolConformance *Conf) {
    DeclContext *DC = Conf->getDeclContext();
    // The location where to insert stubs.
    SourceLoc FixitLocation;

    // The location where the type starts.
    SourceLoc TypeLoc;
    if (auto Extension = dyn_cast<ExtensionDecl>(DC)) {
      FixitLocation = Extension->getBraces().Start;
      TypeLoc = Extension->getStartLoc();
    } else if (auto Nominal = dyn_cast<NominalTypeDecl>(DC)) {
      FixitLocation = Nominal->getBraces().Start;
      TypeLoc = Nominal->getStartLoc();
    } else {
      llvm_unreachable("Unknown adopter kind");
    }
    std::string FixIt;
    llvm::SetVector<ValueDecl*> NoStubRequirements;

    // Print stubs for all known missing witnesses.
    printProtocolStubFixitString(TypeLoc, Conf,
                                 MissingWitnesses.getArrayRef(),
                                 FixIt, NoStubRequirements);
    auto &Diags = DC->getASTContext().Diags;
    for (auto VD : MissingWitnesses) {
      // Whether this VD has a stub printed.
      bool AddFixit = !NoStubRequirements.count(VD);

      // Issue diagnostics for witness types.
      if (auto MissingTypeWitness = dyn_cast<AssociatedTypeDecl>(VD)) {
        Diags.diagnose(MissingTypeWitness, diag::no_witnesses_type,
                       MissingTypeWitness->getName()).
        fixItInsertAfter(FixitLocation, FixIt);
        continue;
      }
      // Issue diagnostics for witness values.
      Type RequirementType =
      getRequirementTypeForDisplay(DC->getParentModule(), Conf, VD);
      auto Diag = Diags.diagnose(VD, diag::no_witnesses,
                                 getRequirementKind(VD), VD->getFullName(),
                                 RequirementType, AddFixit);
      if (AddFixit)
        Diag.fixItInsertAfter(FixitLocation, FixIt);
    }
  };

  switch (Kind) {
  case MissingWitnessDiagnosisKind::ErrorFixIt: {
    diagnoseOrDefer(LocalMissing[0], true, InsertFixitCallback);
    GlobalMissingWitnesses.clear();
    return;
  }
  case MissingWitnessDiagnosisKind::ErrorOnly: {
    diagnoseOrDefer(LocalMissing[0], true,
                    [](NormalProtocolConformance *Conf) {});
    return;
  }
  case MissingWitnessDiagnosisKind::FixItOnly:
    InsertFixitCallback(Conformance);
    GlobalMissingWitnesses.clear();
    return;
  }
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
          ->hasError()) {
      return ResolveWitnessResult::ExplicitFailed;
    }
  }

  // Determine whether we can derive a witness for this requirement.
  bool canDerive = false;
  if (auto *nominal = Adoptee->getAnyNominal()) {
    // Can a witness for this requirement be derived for this nominal type?
    if (auto derivable = DerivedConformance::getDerivableRequirement(
                           nominal,
                           requirement)) {
      if (derivable == requirement) {
        // If it's the same requirement, we can derive it here.
        canDerive = true;
      } else {
        // Otherwise, go satisfy the derivable requirement, which can introduce
        // a member that could in turn satisfy *this* requirement.
        auto derivableProto = cast<ProtocolDecl>(derivable->getDeclContext());
        if (auto conformance =
              TC.conformsToProtocol(Adoptee, derivableProto, DC, None)) {
          if (conformance->isConcrete())
            conformance->getConcrete()->getWitness(derivable, &TC);
        }
      }
    }
  }

  // Find the best witness for the requirement.
  SmallVector<RequirementMatch, 4> matches;
  unsigned numViable = 0;
  unsigned bestIdx = 0;
  bool doNotDiagnoseMatches = false;
  bool ignoringNames = false;
  bool considerRenames =
    !canDerive && !requirement->getAttrs().hasAttribute<OptionalAttr>() &&
    !requirement->getAttrs().isUnavailable(TC.Context);
  RequirementEnvironment reqEnvironment(TC, DC, requirement, Conformance);
  if (findBestWitness(requirement,
                      considerRenames ? &ignoringNames : nullptr,
                      Conformance,
                      reqEnvironment,
                      /* out parameters: */
                      matches, numViable, bestIdx, doNotDiagnoseMatches)) {
    auto &best = matches[bestIdx];
    auto witness = best.Witness;

    // If the name didn't actually line up, complain.
    if (ignoringNames && 
        requirement->getFullName() != best.Witness->getFullName()) {

      diagnoseOrDefer(requirement, false,
        [witness, requirement](NormalProtocolConformance *conformance) {
          auto proto = conformance->getProtocol();
          auto &diags = proto->getASTContext().Diags;
          {
            auto diag = diags.diagnose(witness,
                                       diag::witness_argument_name_mismatch,
                                       isa<ConstructorDecl>(witness),
                                       witness->getFullName(),
                                       proto->getDeclaredType(),
                                       requirement->getFullName());
            fixDeclarationName(diag, witness, requirement->getFullName());
          }

          diags.diagnose(requirement, diag::protocol_requirement_here,
                         requirement->getFullName());

        });
    }

    AccessScope nominalAccessScope =
        Adoptee->getAnyNominal()->getFormalAccessScope(DC);
    auto check = checkWitness(nominalAccessScope, requirement, best);

    switch (check.Kind) {
    case CheckKind::Success:
      break;

    case CheckKind::Accessibility:
    case CheckKind::AccessibilityOfSetter: {
      // Avoid relying on the lifetime of 'this'.
      const DeclContext *DC = this->DC;
      diagnoseOrDefer(requirement, false,
        [DC, witness, check, requirement](
          NormalProtocolConformance *conformance) {
        auto requiredAccessScope = check.RequiredAccessScope;
        Accessibility requiredAccess =
          requiredAccessScope.requiredAccessibilityForDiagnostics();
        auto proto = conformance->getProtocol();
        auto protoAccessScope = proto->getFormalAccessScope(DC);
        bool protoForcesAccess =
          requiredAccessScope.hasEqualDeclContextWith(protoAccessScope);
        auto diagKind = protoForcesAccess
                          ? diag::witness_not_accessible_proto
                          : diag::witness_not_accessible_type;
        bool isSetter = (check.Kind == CheckKind::AccessibilityOfSetter);

        auto &diags = DC->getASTContext().Diags;
        auto diag = diags.diagnose(
                             witness, diagKind,
                             getRequirementKind(requirement),
                             witness->getFullName(),
                             isSetter,
                             requiredAccess,
                             protoAccessScope.accessibilityForDiagnostics(),
                             proto->getName());
        fixItAccessibility(diag, witness, requiredAccess, isSetter);
      });
      break;
    }

    case CheckKind::Availability: {
      diagnoseOrDefer(requirement, false,
        [witness, requirement, check](
            NormalProtocolConformance *conformance) {
          // FIXME: The problem may not be the OS version.
          ASTContext &ctx = witness->getASTContext();
          auto &diags = ctx.Diags;
          diags.diagnose(
                  witness,
                  diag::availability_protocol_requires_version,
                  conformance->getProtocol()->getFullName(),
                  witness->getFullName(),
                  prettyPlatformString(targetPlatform(ctx.LangOpts)),
                  check.RequiredAvailability.getOSVersion().getLowerEndpoint());
          diags.diagnose(requirement, 
                         diag::availability_protocol_requirement_here);
          diags.diagnose(conformance->getLoc(),
                         diag::availability_conformance_introduced_here);
        });
      break;
    }

    case CheckKind::Unavailable: {
      auto *attr = requirement->getAttrs().getUnavailable(TC.Context);
      TC.diagnoseUnavailableOverride(witness, requirement, attr);
      break;
    }

    case CheckKind::OptionalityConflict:
      diagnoseOrDefer(requirement, false,
        [witness, best, requirement](NormalProtocolConformance *conformance) {
          auto proto = conformance->getProtocol();
          auto &ctx = witness->getASTContext();
          auto &diags = ctx.Diags;
          {
            auto diag = diags.diagnose(
                          witness,
                          hasAnyError(best.OptionalAdjustments)
                            ? diag::err_protocol_witness_optionality
                            : diag::warn_protocol_witness_optionality,
                          best.classifyOptionalityIssues(requirement),
                          witness->getFullName(),
                          proto->getFullName());
            best.addOptionalityFixIts(ctx, witness, diag);
          }

          diags.diagnose(requirement, diag::protocol_requirement_here,
                         requirement->getFullName());
      });
      break;

    case CheckKind::ConstructorFailability:
      diagnoseOrDefer(requirement, false,
        [witness, requirement](NormalProtocolConformance *conformance) {
          auto ctor = cast<ConstructorDecl>(requirement);
          auto witnessCtor = cast<ConstructorDecl>(witness);
          auto &diags = witness->getASTContext().Diags;
          diags.diagnose(witness->getLoc(),
                         diag::witness_initializer_failability,
                         ctor->getFullName(),
                         witnessCtor->getFailability()
                           == OTK_ImplicitlyUnwrappedOptional)
            .highlight(witnessCtor->getFailabilityLoc());
        });

      break;
    }

    ClassDecl *classDecl = Adoptee->getClassOrBoundGenericClass();

    if (classDecl && !classDecl->isFinal()) {
      // If we have an initializer requirement and the conforming type
      // is a non-final class, the witness must be 'required'.
      // We exempt Objective-C initializers from this requirement
      // because there is no equivalent to 'required' in Objective-C.
      if (auto ctor = dyn_cast<ConstructorDecl>(best.Witness)) {
        if (!ctor->isRequired() &&
            !ctor->getDeclContext()->getAsProtocolOrProtocolExtensionContext() &&
            !ctor->hasClangNode()) {
          // FIXME: We're not recovering (in the AST), so the Fix-It
          // should move.
          diagnoseOrDefer(requirement, false,
            [ctor, requirement](NormalProtocolConformance *conformance) {
              bool inExtension = isa<ExtensionDecl>(ctor->getDeclContext());
              auto &diags = ctor->getASTContext().Diags;
              auto diag = diags.diagnose(ctor->getLoc(),
                                         diag::witness_initializer_not_required,
                                         requirement->getFullName(), 
                                         inExtension,
                                         conformance->getType());
              if (!ctor->isImplicit() && !inExtension)
                diag.fixItInsert(ctor->getStartLoc(), "required ");
            });
        }
      }

      // Check whether this requirement uses Self in a way that might
      // prevent conformance from succeeding.
      auto selfKind = Proto->findProtocolSelfReferences(requirement,
                                           /*allowCovariantParameters=*/false,
                                           /*skipAssocTypes=*/true);

      if (selfKind.other) {
        // References to Self in a position where subclasses cannot do
        // the right thing. Complain if the adoptee is a non-final
        // class.
        diagnoseOrDefer(requirement, false,
          [witness, requirement](NormalProtocolConformance *conformance) {
            auto proto = conformance->getProtocol();
            auto &diags = proto->getASTContext().Diags;
            diags.diagnose(witness->getLoc(), diag::witness_self_non_subtype,
                           proto->getDeclaredType(), requirement->getFullName(),
                           conformance->getType());
          });
      } else if (selfKind.result) {
        // The reference to Self occurs in the result type. A non-final class 
        // can satisfy this requirement with a method that returns Self.

        // If the function has a dynamic Self, it's okay.
        if (auto func = dyn_cast<FuncDecl>(best.Witness)) {
          if (!func->hasDynamicSelf()) {
            diagnoseOrDefer(requirement, false,
              [witness, requirement](NormalProtocolConformance *conformance) {
                auto proto = conformance->getProtocol();
                auto &diags = proto->getASTContext().Diags;
                diags.diagnose(witness->getLoc(),
                               diag::witness_requires_dynamic_self,
                               requirement->getFullName(),
                               conformance->getType(),
                               proto->getDeclaredType());
              });
          }

        // Constructors conceptually also have a dynamic Self
        // return type, so they're okay.
        } else if (!isa<ConstructorDecl>(best.Witness)) {
          diagnoseOrDefer(requirement, false,
            [witness, requirement](NormalProtocolConformance *conformance) {
              auto proto = conformance->getProtocol();
              auto &diags = proto->getASTContext().Diags;
              diags.diagnose(witness->getLoc(), diag::witness_self_non_subtype,
                             proto->getDeclaredType(),
                             requirement->getFullName(),
                             conformance->getType());
            });
        }
      }

      // A non-final class can model a protocol requirement with a
      // contravariant Self, because here the witness will always have
      // a more general type than the requirement.
    }

    // Record the match.
    recordWitness(requirement, best, std::move(reqEnvironment));
    return ResolveWitnessResult::Success;

    // We have an ambiguity; diagnose it below.
  }

  // We have either no matches or an ambiguous match.

  // If we can derive a definition for this requirement, just call it missing.
  if (canDerive) {
    return ResolveWitnessResult::Missing;
  }

  // If the requirement is optional, it's okay. We'll satisfy this via
  // our handling of default definitions.
  //
  // FIXME: revisit this once we get default definitions in protocol bodies.
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
  if (doNotDiagnoseMatches) {
    return ResolveWitnessResult::ExplicitFailed;
  }


  if (!numViable) {
    // Save the missing requirement for later diagnosis.
    GlobalMissingWitnesses.insert(requirement);
    diagnoseOrDefer(requirement, true,
      [requirement, matches](NormalProtocolConformance *conformance) {
        auto dc = conformance->getDeclContext();
        // Diagnose each of the matches.
        for (const auto &match : matches)
          diagnoseMatch(dc->getParentModule(), conformance, requirement, match);
      });
    return ResolveWitnessResult::ExplicitFailed;
  }

  diagnoseOrDefer(requirement, true,
    [requirement, matches, ignoringNames](
      NormalProtocolConformance *conformance) {
      auto dc = conformance->getDeclContext();
      // Determine the type that the requirement is expected to have.
      Type reqType = getRequirementTypeForDisplay(dc->getParentModule(),
                                                  conformance, requirement);
      auto &diags = dc->getASTContext().Diags;
      auto diagnosticMessage = diag::ambiguous_witnesses;
      if (ignoringNames) {
        diagnosticMessage = diag::ambiguous_witnesses_wrong_name;
      }
      diags.diagnose(requirement, diagnosticMessage,
                     getRequirementKind(requirement),
                     requirement->getFullName(),
                     reqType);

      // Diagnose each of the matches.
      for (const auto &match : matches)
        diagnoseMatch(dc->getParentModule(), conformance, requirement, match);
    });

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
  auto derived = TC.deriveProtocolRequirement(DC, derivingTypeDecl, requirement);
  if (!derived)
    return ResolveWitnessResult::ExplicitFailed;

  // Try to match the derived requirement.
  RequirementEnvironment reqEnvironment(TC, DC, requirement, Conformance);
  auto match = matchWitness(TC, Proto, Conformance, DC, requirement, derived,
                            reqEnvironment);
  if (match.isViable()) {
    recordWitness(requirement, match, std::move(reqEnvironment));
    return ResolveWitnessResult::Success;
  }

  // Derivation failed.
  diagnoseOrDefer(requirement, true,
    [](NormalProtocolConformance *conformance) {
      auto proto = conformance->getProtocol();
      auto &diags = proto->getASTContext().Diags;
      diags.diagnose(conformance->getLoc(), diag::protocol_derivation_is_broken,
                     proto->getDeclaredType(), conformance->getType());
    });

  return ResolveWitnessResult::ExplicitFailed;
}

// FIXME: revisit this once we get default implementations in protocol bodies.
ResolveWitnessResult ConformanceChecker::resolveWitnessViaDefault(
                       ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Use resolveTypeWitnessVia*");

  // An optional requirement is trivially satisfied with an empty requirement.
  // An 'unavailable' requirement is treated like an optional requirement.
  auto Attrs = requirement->getAttrs();
  if (Attrs.hasAttribute<OptionalAttr>() || Attrs.isUnavailable(TC.Context)) {
    recordOptionalWitness(requirement);
    return ResolveWitnessResult::Success;
  }
  // Save the missing requirement for later diagnosis.
  GlobalMissingWitnesses.insert(requirement);
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
  if (auto superclass = assocType->getSuperclass()) {
    if (!superclass->isExactSuperclassOf(type, &tc))
      return superclass->getAnyNominal();
  }

  // Check protocol conformances.
  for (auto reqProto : assocType->getConformingProtocols()) {
    if (!tc.conformsToProtocol(type, reqProto, dc, None))
      return reqProto;

    // FIXME: Why is conformsToProtocol() not enough? The stdlib doesn't
    // build unless we fail here while inferring an associated type
    // somewhere.
    if (type->isSpecialized()) {
      auto substitutions = type->gatherAllSubstitutions(
          dc->getParentModule(), &tc);
      for (auto sub : substitutions) {
        if (sub.getReplacement()->hasError())
          return reqProto;
      }
    }
  }

  // Success!
  return CheckTypeWitnessResult();
}

/// Attempt to resolve a type witness via member name lookup.
ResolveWitnessResult ConformanceChecker::resolveTypeWitnessViaLookup(
                       AssociatedTypeDecl *assocType) {
  // Look for a member type with the same name as the associated type.
  auto candidates = TC.lookupMemberType(DC, Adoptee, assocType->getName(),
                                        /*options=*/None);

  // If there aren't any candidates, we're done.
  if (!candidates) {
    return ResolveWitnessResult::Missing;
  }

  // Determine which of the candidates is viable.
  SmallVector<std::pair<TypeDecl *, Type>, 2> viable;
  SmallVector<std::pair<TypeDecl *, NominalTypeDecl *>, 2> nonViable;
  for (auto candidate : candidates) {
    // Skip nested generic types.
    if (auto *genericDecl = dyn_cast<GenericTypeDecl>(candidate.first))
      if (genericDecl->getGenericParams())
        continue;

    // Check this type against the protocol requirements.
    if (auto checkResult = checkTypeWitness(TC, DC, assocType, 
                                            candidate.second)) {
      auto reqProto = checkResult.getProtocolOrClass();
      nonViable.push_back({candidate.first, reqProto});
    } else {
      viable.push_back(candidate);
    }
  }

  // If there is a single viable candidate, form a substitution for it.
  if (viable.size() == 1) {
    recordTypeWitness(assocType, viable.front().second, viable.front().first, true);
    return ResolveWitnessResult::Success;
  }

  // Record an error.
  recordTypeWitness(assocType, ErrorType::get(TC.Context), nullptr, false);

  // If we had multiple viable types, diagnose the ambiguity.
  if (!viable.empty()) {
    diagnoseOrDefer(assocType, true,
      [assocType, viable](NormalProtocolConformance *conformance) {
        auto &diags = assocType->getASTContext().Diags;
        diags.diagnose(assocType, diag::ambiguous_witnesses_type,
                       assocType->getName());

        for (auto candidate : viable)
          diags.diagnose(candidate.first, diag::protocol_witness_type);
      });

    return ResolveWitnessResult::ExplicitFailed;
  }
  // Save the missing type witness for later diagnosis.
  GlobalMissingWitnesses.insert(assocType);

  // None of the candidates were viable.
  diagnoseOrDefer(assocType, true,
    [nonViable](NormalProtocolConformance *conformance) {
      auto &diags = conformance->getDeclContext()->getASTContext().Diags;
      for (auto candidate : nonViable) {
        if (candidate.first->getDeclaredInterfaceType()->hasError())
          continue;

        diags.diagnose(
           candidate.first,
           diag::protocol_witness_nonconform_type,
           candidate.first->getDeclaredInterfaceType(),
           candidate.second->getDeclaredInterfaceType(),
           candidate.second->getDeclaredInterfaceType()->is<ProtocolType>());
      }
    });

  return ResolveWitnessResult::ExplicitFailed;
}

static bool associatedTypesAreSameEquivalenceClass(AssociatedTypeDecl *a,
                                                   AssociatedTypeDecl *b) {
  if (a == b)
    return true;
  
  // TODO: Do a proper equivalence check here by looking for some relationship
  // between a and b's protocols. In practice today, it's unlikely that
  // two same-named associated types can currently be independent, since we
  // don't have anything like `@implements(P.foo)` to rename witnesses (and
  // we still fall back to name lookup for witnesses in more cases than we
  // should).
  if (a->getName() == b->getName())
    return true;

  return false;
}

InferredAssociatedTypesByWitnesses
ConformanceChecker::inferTypeWitnessesViaValueWitnesses(
                    const llvm::SetVector<AssociatedTypeDecl *> &allUnresolved,
                    ValueDecl *req) {
  InferredAssociatedTypesByWitnesses result;

  auto isExtensionUsableForInference = [&](ExtensionDecl *extension) -> bool {
    // Assume unconstrained concrete extensions we found witnesses in are
    // always viable.
    if (!extension->getExtendedType()->isAnyExistentialType()) {
      // TODO: When constrained extensions are a thing, we'll need an "is
      // as specialized as" kind of check here.
      return !extension->isConstrainedExtension();
    }
    
    // The extension may not have a generic signature set up yet, as a
    // recursion breaker, in which case we can't yet confidently reject its
    // witnesses.
    if (!extension->getGenericSignature())
      return true;
    
    // The condition here is a bit more fickle than
    // `isProtocolExtensionUsable`. That check would prematurely reject
    // extensions like `P where AssocType == T` if we're relying on a
    // default implementation inside the extension to infer `AssocType == T`
    // in the first place. Only check conformances on the `Self` type,
    // because those have to be explicitly declared on the type somewhere
    // so won't be affected by whatever answer inference comes up with.
    auto selfTy = GenericTypeParamType::get(0, 0, TC.Context);
    for (const Requirement &reqt
         : extension->getGenericSignature()->getRequirements()) {
      switch (reqt.getKind()) {
      case RequirementKind::Conformance:
      case RequirementKind::Superclass:
        if (selfTy->isEqual(reqt.getFirstType())
            && !TC.isSubtypeOf(Conformance->getType(),reqt.getSecondType(), DC))
          return false;
        break;
      
      case RequirementKind::Layout:
      case RequirementKind::SameType:
        break;
      }
    }
    
    return true;
  };

  for (auto witness : lookupValueWitnesses(req, /*ignoringNames=*/nullptr)) {
    DEBUG(llvm::dbgs() << "Inferring associated types from decl:\n";
          witness->dump(llvm::dbgs()));
  
    // If the potential witness came from an extension, and our `Self`
    // type can't use it regardless of what associated types we end up
    // inferring, skip the witness.
    if (auto extension = dyn_cast<ExtensionDecl>(witness->getDeclContext()))
      if (!isExtensionUsableForInference(extension))
        continue;
  
    // Try to resolve the type witness via this value witness.
    auto witnessResult = inferTypeWitnessesViaValueWitness(req, witness);

    // Filter out duplicated inferred types as well as inferred types
    // that don't meet the requirements placed on the associated type.
    llvm::DenseSet<std::pair<AssociatedTypeDecl *, CanType>> known;
    for (unsigned i = 0; i < witnessResult.Inferred.size(); /*nothing*/) {
#define REJECT {\
  witnessResult.Inferred.erase(witnessResult.Inferred.begin() + i); \
  continue; \
}
      auto &result = witnessResult.Inferred[i];

      DEBUG(llvm::dbgs() << "Considering whether " << result.first->getName()
                         << " can infer to:\n";
            result.second->dump(llvm::dbgs()));

      // Filter out errors.
      if (result.second->hasError()) {
        DEBUG(llvm::dbgs() << "-- has error type\n");
        REJECT;
      }

      // Filter out duplicates.
      if (!known.insert({result.first, result.second->getCanonicalType()})
                .second) {
        DEBUG(llvm::dbgs() << "-- duplicate\n");
        REJECT;
      }
     
      // Filter out circular possibilities, e.g. that
      // AssocType == S.AssocType or
      // AssocType == Foo<S.AssocType>.
      bool canInferFromOtherAssociatedType = false;
      bool containsTautologicalType =
        result.second.findIf([&](Type t) -> bool {
          auto dmt = t->getAs<DependentMemberType>();
          if (!dmt)
            return false;
          if (!associatedTypesAreSameEquivalenceClass(dmt->getAssocType(),
                                                      result.first))
            return false;
          if (!dmt->getBase()->isEqual(Conformance->getType()))
            return false;
          
          // If this associated type is same-typed to another associated type
          // on `Self`, then it may still be an interesting candidate if we find
          // an answer for that other type.
          auto witnessContext = witness->getDeclContext();
          if (witnessContext->getAsProtocolExtensionContext()
              && witnessContext->getGenericSignatureOfContext()) {
            auto selfTy = witnessContext->getSelfInterfaceType();
            auto selfAssocTy = DependentMemberType::get(selfTy,
                                                        dmt->getAssocType());
            for (auto &reqt : witnessContext->getGenericSignatureOfContext()
                                            ->getRequirements()) {
              switch (reqt.getKind()) {
              case RequirementKind::Conformance:
              case RequirementKind::Superclass:
              case RequirementKind::Layout:
                break;
              
              case RequirementKind::SameType:
                Type other;
                if (reqt.getFirstType()->isEqual(selfAssocTy)) {
                  other = reqt.getSecondType();
                } else if (reqt.getSecondType()->isEqual(selfAssocTy)) {
                  other = reqt.getFirstType();
                } else {
                  break;
                }
                
                if (auto otherAssoc = other->getAs<DependentMemberType>()) {
                  if (otherAssoc->getBase()->isEqual(selfTy)) {
                    auto otherDMT = DependentMemberType::get(dmt->getBase(),
                                                    otherAssoc->getAssocType());
                    
                    // We may be able to infer one associated type from the
                    // other.
                    result.second = result.second.transform([&](Type t) -> Type{
                      if (t->isEqual(dmt))
                        return otherDMT;
                      return t;
                    });
                    canInferFromOtherAssociatedType = true;
                    DEBUG(llvm::dbgs() << "++ we can same-type to:\n";
                          result.second->dump(llvm::dbgs()));
                    return false;
                  }
                }
                break;
              }
            }
          }
          
          return true;
        });
      
      if (containsTautologicalType) {
        DEBUG(llvm::dbgs() << "-- tautological\n");
        REJECT;
      }
      
      // Check that the type witness doesn't contradict an
      // explicitly-given type witness. If it does contradict, throw out the
      // witness completely.
      if (!allUnresolved.count(result.first)) {
        auto existingWitness =
        Conformance->getTypeWitness(result.first, nullptr)
        .getReplacement();
        // If the deduced type contains an irreducible
        // DependentMemberType, that indicates a dependency
        // on another associated type we haven't deduced,
        // so we can't tell whether there's a contradiction
        // yet.
        auto newWitness = result.second->getCanonicalType();
        if (!newWitness->hasTypeParameter()
            && !existingWitness->isEqual(newWitness)) {
          DEBUG(llvm::dbgs() << "** contradicts explicit type witness, "
                                "rejecting inference from this decl\n");
          goto next_witness;
        }
      }
      
      // If we same-typed to another unresolved associated type, we won't
      // be able to check conformances yet.
      if (!canInferFromOtherAssociatedType) {
        // Check that the type witness meets the
        // requirements on the associated type.
        if (auto failed = checkTypeWitness(TC, DC, result.first,
                                           result.second)) {
          witnessResult.NonViable.push_back(
                              std::make_tuple(result.first,result.second,failed));
          DEBUG(llvm::dbgs() << "-- doesn't fulfill requirements\n");
          REJECT;
        }
      }
      
      DEBUG(llvm::dbgs() << "++ seems legit\n");
      ++i;
    }
#undef REJECT

    // If no inferred types remain, skip this witness.
    if (witnessResult.Inferred.empty() && witnessResult.NonViable.empty())
      continue;

    // If there were any non-viable inferred associated types, don't
    // infer anything from this witness.
    if (!witnessResult.NonViable.empty())
      witnessResult.Inferred.clear();

    result.push_back(std::move(witnessResult));
next_witness:;
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
      if (func->isOperator() || func->isAccessor())
        continue;
    }

    // Validate the requirement.
    TC.validateDecl(req);
    if (req->isInvalid() || !req->hasValidSignature())
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
    auto reqInferred = inferTypeWitnessesViaValueWitnesses(assocTypes, req);
    if (reqInferred.empty())
      continue;

    result.push_back({req, std::move(reqInferred)});
  }

  return result;
}

/// Map error types back to their original types.
static Type mapErrorTypeToOriginal(Type type) {
  if (auto errorType = type->getAs<ErrorType>()) {
    if (auto originalType = errorType->getOriginalType())
      return originalType.transform(mapErrorTypeToOriginal);
  }

  return type;
}

/// Produce the type when matching a witness.
static Type getWitnessTypeForMatching(TypeChecker &tc,
                                      NormalProtocolConformance *conformance,
                                      ValueDecl *witness) {
  if (!witness->hasInterfaceType())
    tc.validateDecl(witness);

  if (witness->isInvalid() || !witness->hasValidSignature())
    return Type();

  if (!witness->getDeclContext()->isTypeContext()) {
    // FIXME: Could we infer from 'Self' to make these work?
    return witness->getInterfaceType();
  }

  // Retrieve the set of substitutions to be applied to the witness.
  Type model = conformance->getType();
  TypeSubstitutionMap substitutions = model->getMemberSubstitutions(witness);
  if (substitutions.empty())
    return witness->getInterfaceType();

  Type type = witness->getInterfaceType();
  
  // Strip off the requirements of a generic function type.
  // FIXME: This doesn't actually break recursion when substitution
  // looks for an inferred type witness, but it makes it far less
  // common, because most of the recursion involves the requirements
  // of the generic type.
  if (auto genericFn = type->getAs<GenericFunctionType>()) {
    type = FunctionType::get(genericFn->getInput(),
                             genericFn->getResult(),
                             genericFn->getExtInfo());
  }

  ModuleDecl *module = conformance->getDeclContext()->getParentModule();
  auto resultType = type.subst(QueryTypeSubstitutionMap{substitutions},
                               LookUpConformanceInModule(module),
                               SubstFlags::UseErrorType);
  if (!resultType->hasError()) return resultType;

  // Map error types with original types *back* to the original, dependent type.
  return resultType.transform(mapErrorTypeToOriginal);
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
  Type fullWitnessType = getWitnessTypeForMatching(TC, Conformance, witness);
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
  class MatchVisitor : public TypeMatcher<MatchVisitor> {
    NormalProtocolConformance *Conformance;
    InferredAssociatedTypesByWitness &Inferred;

  public:
    MatchVisitor(NormalProtocolConformance *conformance,
                 InferredAssociatedTypesByWitness &inferred)
      : Conformance(conformance), Inferred(inferred) { }

    /// Structural mismatches imply that the witness cannot match.
    bool mismatch(TypeBase *firstType, TypeBase *secondType,
                  Type sugaredFirstType) {
      // If either type hit an error, don't stop yet.
      if (firstType->hasError() || secondType->hasError())
        return true;

      // FIXME: Check whether one of the types is dependent?
      return false;
    }

    /// Deduce associated types from dependent member types in the witness.
    bool mismatch(DependentMemberType *firstDepMember,
                  TypeBase *secondType, Type sugaredFirstType) {
      // If the second type is an error, don't look at it further.
      if (secondType->hasError())
        return true;

      auto proto = Conformance->getProtocol();
      if (auto assocType = getReferencedAssocTypeOfProtocol(firstDepMember,
                                                            proto)) {
        Inferred.Inferred.push_back({assocType, secondType});
      }

      // Always allow mismatches here.
      return true;
    }

    /// FIXME: Recheck the type of Self against the second type?
    bool mismatch(GenericTypeParamType *selfParamType,
                  TypeBase *secondType, Type sugaredFirstType) {
      return true;
    }
  };

  // Match a requirement and witness type.
  MatchVisitor matchVisitor(Conformance, inferred);
  auto matchTypes = [&](Type reqType, Type witnessType)
                      -> Optional<RequirementMatch> {
    if (!matchVisitor.match(reqType, witnessType)) {
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

  // Match the witness. If we don't succeed, throw away the inference
  // information.
  // FIXME: A renamed match might be useful to retain for the failure case.
  if (matchWitness(TC, DC, req, witness, setup, matchTypes, finalize)
          .Kind != MatchKind::ExactMatch) {
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
    
#ifndef NDEBUG
    LLVM_ATTRIBUTE_USED
#endif
    void dump() {
      llvm::errs() << "Type Witnesses:\n";
      for (auto &typeWitness : TypeWitnesses) {
        llvm::errs() << "  " << typeWitness.first->getName() << " := ";
        typeWitness.second.first->print(llvm::errs());
        llvm::errs() << " value " << typeWitness.second.second << '\n';
      }
      llvm::errs() << "Value Witnesses:\n";
      for (unsigned i : indices(ValueWitnesses)) {
        auto &valueWitness = ValueWitnesses[i];
        llvm::errs() << i << ":  " << (Decl*)valueWitness.first
                     << ' ' << valueWitness.first->getName() << '\n';
        valueWitness.first->getDeclContext()->dumpContext();
        llvm::errs() << "    for " << (Decl*)valueWitness.second
                     << ' ' << valueWitness.second->getName() << '\n';
        valueWitness.second->getDeclContext()->dumpContext();
      }
    }
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
} // end anonymous namespace

static Comparison
compareDeclsForInference(TypeChecker &TC, DeclContext *DC,
                         ValueDecl *decl1, ValueDecl *decl2) {
  // TC.compareDeclarations assumes that it's comparing two decls that
  // apply equally well to a call site. We haven't yet inferred the
  // associated types for a type, so the ranking algorithm used by
  // compareDeclarations to score protocol extensions is inappropriate,
  // since we may have potential witnesses from extensions with mutually
  // exclusive associated type constraints, and compareDeclarations will
  // consider these unordered since neither extension's generic signature
  // is a superset of the other.
  
  // If the witnesses come from the same decl context, score normally.
  auto dc1 = decl1->getDeclContext();
  auto dc2 = decl2->getDeclContext();
  
  if (dc1 == dc2)
    return TC.compareDeclarations(DC, decl1, decl2);
  
  auto isProtocolExt1 =
    (bool)dc1->getAsProtocolExtensionContext();
  auto isProtocolExt2 =
    (bool)dc2->getAsProtocolExtensionContext();
  
  // If one witness comes from a protocol extension, favor the one
  // from a concrete context.
  if (isProtocolExt1 != isProtocolExt2) {
    return isProtocolExt1 ? Comparison::Worse : Comparison::Better;
  }
  
  // If both witnesses came from concrete contexts, score normally.
  // Associated type inference shouldn't impact the result.
  // FIXME: It could, if someone constrained to ConcreteType.AssocType...
  if (!isProtocolExt1)
    return TC.compareDeclarations(DC, decl1, decl2);
  
  // Compare protocol extensions by which protocols they require Self to
  // conform to. If one extension requires a superset of the other's
  // constraints, it wins.
  auto sig1 = dc1->getGenericSignatureOfContext();
  auto sig2 = dc2->getGenericSignatureOfContext();

  // FIXME: Extensions sometimes have null generic signatures while
  // checking the standard library...
  if (!sig1 || !sig2)
    return TC.compareDeclarations(DC, decl1, decl2);
  
  auto selfParam = GenericTypeParamType::get(0, 0, TC.Context);
  
  // Collect the protocols required by extension 1.
  Type class1;
  SmallPtrSet<ProtocolDecl*, 4> protos1;
  
  std::function<void (ProtocolDecl*)> insertProtocol;
  insertProtocol = [&](ProtocolDecl *p) {
    if (!protos1.insert(p).second)
      return;

    for (auto parent : p->getInheritedProtocols())
      insertProtocol(parent);
  };
  
  for (auto &reqt : sig1->getRequirements()) {
    if (!reqt.getFirstType()->isEqual(selfParam))
      continue;
    switch (reqt.getKind()) {
    case RequirementKind::Conformance: {
      SmallVector<ProtocolDecl*, 4> protos;
      reqt.getSecondType()->getAnyExistentialTypeProtocols(protos);
      
      for (auto proto : protos) {
        insertProtocol(proto);
      }
      break;
    }
    case RequirementKind::Superclass:
      class1 = reqt.getSecondType();
      break;
    
    case RequirementKind::SameType:
    case RequirementKind::Layout:
      break;
    }
  }

  // Compare with the protocols required by extension 2.
  Type class2;
  SmallPtrSet<ProtocolDecl*, 4> protos2;
  bool protos2AreSubsetOf1 = true;
  std::function<void (ProtocolDecl*)> removeProtocol;
  removeProtocol = [&](ProtocolDecl *p) {
    if (!protos2.insert(p).second)
      return;

    protos2AreSubsetOf1 &= protos1.erase(p);
    for (auto parent : p->getInheritedProtocols())
      removeProtocol(parent);
  };

  for (auto &reqt : sig2->getRequirements()) {
    if (!reqt.getFirstType()->isEqual(selfParam))
      continue;
    switch (reqt.getKind()) {
    case RequirementKind::Conformance: {
      SmallVector<ProtocolDecl*, 4> protos;
      reqt.getSecondType()->getAnyExistentialTypeProtocols(protos);
      
      for (auto proto : protos) {
        removeProtocol(proto);
      }
      break;
    }
    case RequirementKind::Superclass:
      class2 = reqt.getSecondType();
      break;
    
    case RequirementKind::SameType:
    case RequirementKind::Layout:
      break;
    }
  }
  
  auto isClassConstraintAsStrict = [&](Type t1, Type t2) -> bool {
    if (!t1)
      return !t2;
    
    if (!t2)
      return true;
    
    return TC.isSubtypeOf(t1, t2, DC);
  };
  
  bool protos1AreSubsetOf2 = protos1.empty();
  // If the second extension requires strictly more protocols than the
  // first, it's better.
  if (protos1AreSubsetOf2 > protos2AreSubsetOf1
      && isClassConstraintAsStrict(class2, class1)) {
    return Comparison::Worse;
  // If the first extension requires strictly more protocols than the
  // second, it's better.
  } else if (protos2AreSubsetOf1 > protos1AreSubsetOf2
             && isClassConstraintAsStrict(class1, class2)) {
    return Comparison::Better;
  }
  
  // If they require the same set of protocols, or non-overlapping
  // sets, judge them normally.
  return TC.compareDeclarations(DC, decl1, decl2);
}

void ConformanceChecker::resolveTypeWitnesses() {
  llvm::SetVector<AssociatedTypeDecl *> unresolvedAssocTypes;

  // Track when we are checking type witnesses.
  ProtocolConformanceState initialState = Conformance->getState();
  Conformance->setState(ProtocolConformanceState::CheckingTypeWitnesses);
  SWIFT_DEFER { Conformance->setState(initialState); };

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
  DEBUG(llvm::dbgs() << "Candidates for inference:\n";
        dumpInferredAssociatedTypes(inferred));

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

    auto selfType = Proto->getSelfInterfaceType();

    // Create a set of type substitutions for all known associated type.
    // FIXME: Base this on dependent types rather than archetypes?
    TypeSubstitutionMap substitutions;
    substitutions[Proto->mapTypeIntoContext(selfType)
        ->castTo<ArchetypeType>()] = Adoptee;
    for (auto member : Proto->getMembers()) {
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
        auto archetype = Proto->mapTypeIntoContext(
            assocType->getDeclaredInterfaceType())
                ->getAs<ArchetypeType>();
        if (!archetype)
          continue;
        if (Conformance->hasTypeWitness(assocType)) {
          substitutions[archetype]
            = Conformance->getTypeWitness(assocType, nullptr).getReplacement();
        } else {
          auto known = typeWitnesses.begin(assocType);
          if (known != typeWitnesses.end())
            substitutions[archetype] = known->first;
          else
            substitutions[archetype] = ErrorType::get(TC.Context);
        }
      }
    }

    TC.validateDecl(assocType);
    Type defaultType = assocType->getDefaultDefinitionLoc().getType().subst(
        QueryTypeSubstitutionMap{substitutions},
        LookUpConformanceInModule(DC->getParentModule()));

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
    if (Adoptee->hasError())
      return Type();

    // UnresolvedTypes propagated their unresolvedness to any witnesses.
    if (Adoptee->is<UnresolvedType>())
      return Adoptee;

    // Can we derive conformances for this protocol and adoptee?
    NominalTypeDecl *derivingTypeDecl = Adoptee->getAnyNominal();
    if (!derivingTypeDecl->derivesProtocolConformance(Proto))
      return Type();

    // Try to derive the type witness.
    Type derivedType = TC.deriveTypeWitness(DC, derivingTypeDecl, assocType);
    if (!derivedType)
      return Type();

    // Make sure that the derived type is sane.
    if (checkTypeWitness(TC, DC, assocType, derivedType)) {
      diagnoseOrDefer(assocType, true,
        [derivedType](NormalProtocolConformance *conformance) {
          // FIXME: give more detail here?
          auto &diags = derivedType->getASTContext().Diags;
          diags.diagnose(conformance->getLoc(),
                         diag::protocol_derivation_is_broken,
                         conformance->getProtocol()->getDeclaredType(),
                         derivedType);
      });

      return Type();
    }

    return derivedType;
  };

  // Local function that folds dependent member types with non-dependent
  // bases into actual member references.
  std::function<Type(Type)> foldDependentMemberTypes;
  llvm::DenseSet<AssociatedTypeDecl *> recursionCheck;
  foldDependentMemberTypes = [&](Type type) -> Type {
    if (auto depMemTy = type->getAs<DependentMemberType>()) {
      auto baseTy = depMemTy->getBase().transform(foldDependentMemberTypes);
      if (baseTy.isNull() || baseTy->hasTypeParameter())
        return nullptr;

      auto assocType = depMemTy->getAssocType();
      if (!assocType)
        return nullptr;

      if (!recursionCheck.insert(assocType).second)
        return nullptr;

      SWIFT_DEFER { recursionCheck.erase(assocType); };

      // Try to substitute into the base type.
      if (Type result = depMemTy->substBaseType(DC->getParentModule(), baseTy)){
        return result;
      }

      // If that failed, check whether it's because of the conformance we're
      // evaluating.
      auto localConformance
        = TC.conformsToProtocol(baseTy, assocType->getProtocol(), DC, None);
      if (!localConformance || localConformance->isAbstract() ||
          (localConformance->getConcrete()->getRootNormalConformance()
             != Conformance)) {
        return nullptr;
      }

      // Find the tentative type witness for this associated type.
      auto known = typeWitnesses.begin(assocType);
      if (known == typeWitnesses.end())
        return nullptr;

      return known->first.transform(foldDependentMemberTypes);
    }

    // The presence of a generic type parameter indicates that we
    // cannot use this type binding.
    if (type->is<GenericTypeParamType>()) {
      return nullptr;
    }

    return type;

  };

  // Local function that checks the current (complete) set of type witnesses
  // to determine whether they meet all of the requirements and to deal with
  // substitution of type witness bindings into other type witness bindings.
  auto checkCurrentTypeWitnesses = [&]() -> bool {
    // Fold the dependent member types within this type.
    for (auto member : Proto->getMembers()) {
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
        if (Conformance->hasTypeWitness(assocType))
          continue;

        // If the type binding does not have a type parameter, there's nothing
        // to do.
        auto known = typeWitnesses.begin(assocType);
        assert(known != typeWitnesses.end());
        if (!known->first->hasTypeParameter())
          continue;

        Type replaced = known->first.transform(foldDependentMemberTypes);
        if (replaced.isNull())
          return true;
        
        if (checkTypeWitness(TC, DC, assocType, replaced))
          return true;

        known->first = replaced;
      }
    }

    return false;
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
        // Local function to record a missing associated type.
        auto recordMissing = [&] {
          if (!missingTypeWitness)
            missingTypeWitness = assocType;
        };

        auto typeWitness = typeWitnesses.begin(assocType);
        if (typeWitness != typeWitnesses.end()) {
          // The solution contains an error.
          if (typeWitness->first->hasError()) {
            recordMissing();
            return;
          }

          continue;
        }

        // We don't have a type witness for this associated type.

        // If we can form a default type, do so.
        if (Type defaultType = computeDefaultTypeWitness(assocType)) {
          if (defaultType->hasError()) {
            recordMissing();
            return;
          }

          typeWitnesses.insert(assocType, {defaultType, reqDepth});
          continue;
        }

        // If we can derive a type witness, do so.
        if (Type derivedType = computeDerivedTypeWitness(assocType)) {
          if (derivedType->hasError()) {
            recordMissing();
            return;
          }

          typeWitnesses.insert(assocType, {derivedType, reqDepth});
          continue;
        }

        // The solution is incomplete.
        recordMissing();
        return;
      }

      /// Check the current set of type witnesses.
      if (checkCurrentTypeWitnesses()) {
        return;
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

      // Record this value witness, popping it when we exit the current scope.
      valueWitnesses.push_back({inferredReq.first, witnessReq.Witness});
      if (witnessReq.Witness->getDeclContext()->getAsProtocolExtensionContext())
        ++numValueWitnessesInProtocolExtensions;
      SWIFT_DEFER {
        if (witnessReq.Witness->getDeclContext()->getAsProtocolExtensionContext())
          --numValueWitnessesInProtocolExtensions;
        valueWitnesses.pop_back();
      };

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

          // If one has a type parameter remaining but the other does not,
          // drop the one with the type parameter.
          if (known->first->hasTypeParameter()
              != typeWitness.second->hasTypeParameter()) {
            if (typeWitness.second->hasTypeParameter())
              continue;

            known->first = typeWitness.second;
            continue;
          }

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

      // Recurse
      findSolutions(reqDepth + 1);
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

  // If we (still) have more than one solution, find the one with
  // more-specialized witnesses.
  if (solutions.size() > 1) {
    // Local function to compare two solutions.
    auto compareSolutions = [&](const InferredTypeWitnessesSolution &first,
                                const InferredTypeWitnessesSolution &second) {
      assert(first.ValueWitnesses.size() == second.ValueWitnesses.size());
      bool firstBetter = false;
      bool secondBetter = false;
      for (unsigned i = 0, n = first.ValueWitnesses.size(); i != n; ++i) {
        assert(first.ValueWitnesses[i].first == second.ValueWitnesses[i].first);
        auto firstWitness = first.ValueWitnesses[i].second;
        auto secondWitness = second.ValueWitnesses[i].second;
        if (firstWitness == secondWitness)
          continue;

        switch (compareDeclsForInference(TC, DC, firstWitness, secondWitness)) {
        case Comparison::Better:
          if (secondBetter)
            return false;

          firstBetter = true;
          break;

        case Comparison::Worse:
          if (firstBetter)
            return false;

          secondBetter = true;
          break;

        case Comparison::Unordered:
          break;
        }
      }

      return firstBetter;
    };

    // Find a solution that's at least as good as the solutions that follow it.
    unsigned bestIdx = 0;
    for (unsigned i = 1, n = solutions.size(); i != n; ++i) {
      if (compareSolutions(solutions[i], solutions[bestIdx]))
        bestIdx = i;
    }
    
    // Make sure that solution is better than any of the other solutions.
    bool ambiguous = false;
    for (unsigned i = 1, n = solutions.size(); i != n; ++i) {
      if (i != bestIdx && !compareSolutions(solutions[bestIdx], solutions[i])) {
        ambiguous = true;
        break;
      }
    }
    
    // If we had a best solution, keep just that solution.
    if (!ambiguous) {
      if (bestIdx != 0)
        solutions[0] = std::move(solutions[bestIdx]);
      solutions.erase(solutions.begin() + 1, solutions.end());
    }
  }

  // If we found a single solution, take it.
  if (solutions.size() == 1) {
    // Record each of the deduced witnesses.
    auto &typeWitnesses = solutions.front().TypeWitnesses;
    for (auto assocType : unresolvedAssocTypes) {
      assert(typeWitnesses.count(assocType) == 1 && "missing witness");
      recordTypeWitness(assocType, typeWitnesses[assocType].first, nullptr, true);
    }

    return;
  }

  // Error cases follow.
  Conformance->setInvalid();

  // We're going to produce an error below. Mark each unresolved
  // associated type witness as erroneous.
  for (auto assocType : unresolvedAssocTypes) {
    recordTypeWitness(assocType, ErrorType::get(TC.Context), nullptr, true);
  }

  // No solutions. Diagnose the first associated type for which we
  // could not determine a witness.
  if (solutions.empty()) {
    // If a defaulted type witness failed, diagnose it.
    if (failedDefaultedAssocType) {
      diagnoseOrDefer(failedDefaultedAssocType, true,
        [failedDefaultedAssocType, failedDefaultedWitness,
         failedDefaultedResult](NormalProtocolConformance *conformance) {
          auto proto = conformance->getProtocol();
          auto &diags = proto->getASTContext().Diags;
          diags.diagnose(failedDefaultedAssocType,
                         diag::default_associated_type_req_fail,
                         failedDefaultedWitness,
                         failedDefaultedAssocType->getFullName(),
                         proto->getDeclaredType(),
                         failedDefaultedResult.getProtocolOrClass()
                           ->getDeclaredType(),
                         failedDefaultedResult.isProtocol());
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
        [assocType, failedSet](NormalProtocolConformance *conformance) {
          auto proto = conformance->getProtocol();
          auto &diags = proto->getASTContext().Diags;
          diags.diagnose(assocType, diag::bad_associated_type_deduction,
                         assocType->getFullName(), proto->getFullName());
          for (const auto &failed : failedSet) {
            diags.diagnose(failed.Witness,
                           diag::associated_type_deduction_witness_failed,
                           failed.Requirement->getFullName(),
                           failed.TypeWitness,
                           failed.Result.getProtocolOrClass()->getFullName(),
                           failed.Result.isProtocol());
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
        [typeWitnessConflict](NormalProtocolConformance *conformance) {
          auto &diags = conformance->getDeclContext()->getASTContext().Diags;
          diags.diagnose(typeWitnessConflict->AssocType,
                         diag::ambiguous_associated_type_deduction,
                         typeWitnessConflict->AssocType->getFullName(),
                         typeWitnessConflict->FirstType,
                         typeWitnessConflict->SecondType);
        
          diags.diagnose(typeWitnessConflict->FirstWitness,
                         diag::associated_type_deduction_witness,
                         typeWitnessConflict->FirstRequirement->getFullName(),
                         typeWitnessConflict->FirstType);
          diags.diagnose(typeWitnessConflict->SecondWitness,
                         diag::associated_type_deduction_witness,
                         typeWitnessConflict->SecondRequirement->getFullName(),
                         typeWitnessConflict->SecondType);
        });

      return;
    }

    // Save the missing type witnesses for later diagnosis.
    GlobalMissingWitnesses.insert(unresolvedAssocTypes.begin(),
                            unresolvedAssocTypes.end());

    return;
  }

  // Multiple solutions. Diagnose the ambiguity.
  for (auto assocType : unresolvedAssocTypes) {
    // Find two types that conflict.
    auto &firstSolution = solutions.front();

    // Local function to retrieve the value witness for the current associated
    // type within the given solution.
    auto getValueWitness = [&](InferredTypeWitnessesSolution &solution) {
      unsigned witnessIdx = solution.TypeWitnesses[assocType].second;
      if (witnessIdx < solution.ValueWitnesses.size())
        return solution.ValueWitnesses[witnessIdx];

      return std::pair<ValueDecl *, ValueDecl *>(nullptr, nullptr);
    };

    Type firstType = firstSolution.TypeWitnesses[assocType].first;

    // Extract the value witness used to deduce this associated type, if any.
    auto firstMatch = getValueWitness(firstSolution);

    Type secondType;
    std::pair<ValueDecl *, ValueDecl *> secondMatch;
    for (auto &solution : solutions) {
      Type typeWitness = solution.TypeWitnesses[assocType].first;
      if (!typeWitness->isEqual(firstType)) {
        secondType = typeWitness;
        secondMatch = getValueWitness(solution);
        break;
      }
    }

    if (!secondType)
      continue;

    // We found an ambiguity. diagnose it.
    diagnoseOrDefer(assocType, true,
      [assocType, firstType, firstMatch, secondType, secondMatch](
        NormalProtocolConformance *conformance) {
        auto &diags = assocType->getASTContext().Diags;
        diags.diagnose(assocType, diag::ambiguous_associated_type_deduction,
                       assocType->getFullName(), firstType, secondType);

        auto diagnoseWitness = [&](std::pair<ValueDecl *, ValueDecl *> match,
                                   Type type){
          // If we have a requirement/witness pair, diagnose it.
          if (match.first && match.second) {
            diags.diagnose(match.second,
                           diag::associated_type_deduction_witness,
                           match.first->getFullName(), type);

            return;
          }

          // Otherwise, we have a default.
          diags.diagnose(assocType, diag::associated_type_deduction_default,
                         type)
            .highlight(assocType->getDefaultDefinitionLoc().getSourceRange());
        };

        diagnoseWitness(firstMatch, firstType);
        diagnoseWitness(secondMatch, secondType);
      });

    return;
  }
}

void ConformanceChecker::resolveSingleTypeWitness(
       AssociatedTypeDecl *assocType) {
  // Ensure we diagnose if the witness is missing.
  SWIFT_DEFER {
    diagnoseMissingWitnesses(MissingWitnessDiagnosisKind::ErrorFixIt);
  };
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
  SWIFT_DEFER { ResolvingWitnesses.erase(requirement); };

  // Make sure we've validated the requirement.
  if (!requirement->hasInterfaceType())
    TC.validateDecl(requirement);

  if (requirement->isInvalid() || !requirement->hasValidSignature()) {
    Conformance->setInvalid();
    return;
  }

  if (!requirement->isProtocolRequirement())
    return;

  // Resolve all associated types before trying to resolve this witness.
  resolveTypeWitnesses();

  // If any of the type witnesses was erroneous, don't bother to check
  // this value witness: it will fail.
  for (auto assocType : getReferencedAssociatedTypes(requirement)) {
    if (Conformance->getTypeWitness(assocType, nullptr).getReplacement()
          ->hasError()) {
      Conformance->setInvalid();
      return;
    }
  }

  // Try to resolve the witness via explicit definitions.
  switch (resolveWitnessViaLookup(requirement)) {
  case ResolveWitnessResult::Success:
    return;
  
  case ResolveWitnessResult::ExplicitFailed:
    Conformance->setInvalid();
    recordInvalidWitness(requirement);
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
    Conformance->setInvalid();
    recordInvalidWitness(requirement);
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
    Conformance->setInvalid();
    recordInvalidWitness(requirement);
    return;

  case ResolveWitnessResult::Missing:
    llvm_unreachable("Should have failed");
    break;
  }
}

static void recordConformanceDependency(DeclContext *DC,
                                        NominalTypeDecl *Adoptee,
                                        ProtocolConformance *Conformance,
                                        bool InExpression) {
  if (!Conformance)
    return;

  auto *topLevelContext = DC->getModuleScopeContext();
  auto *SF = dyn_cast<SourceFile>(topLevelContext);
  if (!SF)
    return;

  auto *tracker = SF->getReferencedNameTracker();
  if (!tracker)
    return;

  if (SF->getParentModule() !=
      Conformance->getDeclContext()->getParentModule())
    return;

  auto &Context = DC->getASTContext();

  // FIXME: 'deinit' is being used as a dummy identifier here. Really we
  // don't care about /any/ of the type's members, only that it conforms to
  // the protocol.
  tracker->addUsedMember({Adoptee, Context.Id_deinit},
                         DC->isCascadingContextForLookup(InExpression));
}

void ConformanceChecker::addUsedConformances(
    ProtocolConformance *conformance,
    llvm::SmallPtrSetImpl<ProtocolConformance *> &visited) {
  // This deduplication cannot be implemented by just checking UsedConformance,
  // because conformances can be added to UsedConformances outside this
  // function, meaning their type witness conformances may not be tracked.
  if (!visited.insert(conformance).second)
    return;

  auto normalConf = conformance->getRootNormalConformance();

  if (normalConf->isIncomplete())
    TC.UsedConformances.insert(normalConf);

  // Mark each type witness conformance as used.
  conformance->forEachTypeWitness(nullptr, [&](AssociatedTypeDecl *assocType,
                                               Substitution sub,
                                               TypeDecl *witness) -> bool {
    for (auto nestedConformance : sub.getConformances())
      if (nestedConformance.isConcrete())
        addUsedConformances(nestedConformance.getConcrete(), visited);

    return false;
  });
}

void ConformanceChecker::addUsedConformances(ProtocolConformance *conformance) {
  llvm::SmallPtrSet<ProtocolConformance *, 8> visited;
  addUsedConformances(conformance, visited);
}

void ConformanceChecker::ensureRequirementsAreSatisfied() {
  auto proto = Conformance->getProtocol();
  // Some other problem stopped the signature being computed.
  if (!proto->isRequirementSignatureComputed()) {
    Conformance->setInvalid();
    return;
  }

  auto reqSig = proto->getRequirementSignature();

  auto substitutions = SubstitutionMap::getProtocolSubstitutions(
      proto, Conformance->getType(), ProtocolConformanceRef(Conformance));

  class GatherConformancesListener : public GenericRequirementsCheckListener {
  public:
    SmallVector<ProtocolConformanceRef, 4> conformances;

    void satisfiedConformance(Type depTy, Type replacementTy,
                              ProtocolConformanceRef conformance) override {
      conformances.push_back(conformance);
    }
  } listener;

  auto result = TC.checkGenericArguments(
      Conformance->getDeclContext(), Loc, Loc,
      // FIXME: maybe this should be the conformance's type
      proto->getDeclaredInterfaceType(), reqSig,
      QuerySubstitutionMap{substitutions},
      LookUpConformanceInSubstitutionMap{substitutions}, nullptr,
      ConformanceCheckFlags::Used, &listener);

  // If there were no errors, record the conformances.
  if (result.second) {
    Conformance->setSignatureConformances(listener.conformances);
  } else {
    Conformance->setInvalid();
  }
}

#pragma mark Protocol conformance checking
void ConformanceChecker::checkConformance(MissingWitnessDiagnosisKind Kind) {
  assert(!Conformance->isComplete() && "Conformance is already complete");

  llvm::SaveAndRestore<bool> restoreSuppressDiagnostics(SuppressDiagnostics);
  SuppressDiagnostics = false;

  // FIXME: Caller checks that this type conforms to all of the
  // inherited protocols.

  emitDelayedDiags();

  // Resolve all of the type witnesses.
  resolveTypeWitnesses();

  // Diagnose missing type witnesses for now.
  diagnoseMissingWitnesses(Kind);
  // Diagnose missing value witnesses later.
  SWIFT_DEFER { diagnoseMissingWitnesses(Kind); };

  // Resolution attempts to have the witnesses be correct by construction, but
  // this isn't guaranteed, so let's double check.
  ensureRequirementsAreSatisfied();

  // Ensure the conforming type is used.
  //
  // FIXME: This feels like the wrong place for this, but if we don't put
  // it here, extensions don't end up depending on the extended type.
  recordConformanceDependency(DC, Adoptee->getAnyNominal(), Conformance, false);

  // If we complain about any associated types, there is no point in continuing.
  // FIXME: Not really true. We could check witnesses that don't involve the
  // failed associated types.
  if (AlreadyComplained) {
    Conformance->setInvalid();
    return;
  }

  // Ensure the associated type conformances are used.
  addUsedConformances(Conformance);

  // Check non-type requirements.
  for (auto member : Proto->getMembers()) {
    auto requirement = dyn_cast<ValueDecl>(member);
    if (!requirement)
      continue;

    // Associated type requirements handled above.
    if (isa<TypeDecl>(requirement))
      continue;

    // Type aliases don't have requirements themselves.
    if (!requirement->isProtocolRequirement())
      continue;

    /// Local function to finalize the witness.
    auto finalizeWitness = [&] {
      // Find the witness.
      auto witness = Conformance->getWitness(requirement, nullptr).getDecl();
      if (!witness) return;

      // Objective-C checking for @objc requirements.
      if (requirement->isObjC() &&
          requirement->getFullName() == witness->getFullName() &&
          !requirement->getAttrs().isUnavailable(TC.Context)) {
        // The witness must also be @objc.
        if (!witness->isObjC()) {
          bool isOptional =
            requirement->getAttrs().hasAttribute<OptionalAttr>();
          if (auto witnessFunc = dyn_cast<AbstractFunctionDecl>(witness)) {
            auto diagInfo = getObjCMethodDiagInfo(witnessFunc);
            auto diag = TC.diagnose(witness,
                                    isOptional ? diag::witness_non_objc_optional
                                               : diag::witness_non_objc,
                                    diagInfo.first, diagInfo.second,
                                    Proto->getFullName());
            if (!witness->canInferObjCFromRequirement(requirement)) {
              fixDeclarationObjCName(
                diag, witness,
                cast<AbstractFunctionDecl>(requirement)->getObjCSelector());
            }
          } else if (isa<VarDecl>(witness)) {
            auto diag = TC.diagnose(witness,
                                    isOptional
                                      ? diag::witness_non_objc_storage_optional
                                      : diag::witness_non_objc_storage,
                                    /*isSubscript=*/false,
                                    witness->getFullName(),
                                    Proto->getFullName());
            if (!witness->canInferObjCFromRequirement(requirement)) {
              fixDeclarationObjCName(
                 diag, witness,
                 ObjCSelector(requirement->getASTContext(), 0,
                              cast<VarDecl>(requirement)
                                ->getObjCPropertyName()));
            }
          } else if (isa<SubscriptDecl>(witness)) {
            TC.diagnose(witness,
                        isOptional
                          ? diag::witness_non_objc_storage_optional
                          : diag::witness_non_objc_storage,
                        /*isSubscript=*/true,
                        witness->getFullName(),
                        Proto->getFullName())
              .fixItInsert(witness->getAttributeInsertionLoc(false),
                           "@objc ");
          }

          // If the requirement is optional, @nonobjc suppresses the
          // diagnostic.
          if (isOptional) {
            TC.diagnose(witness, diag::optional_req_near_match_nonobjc, false)
              .fixItInsert(witness->getAttributeInsertionLoc(false),
                           "@nonobjc ");
          }

          TC.diagnose(requirement, diag::protocol_requirement_here,
                      requirement->getFullName());

          Conformance->setInvalid();
          return;
        }

        // The selectors must coincide.
        if (checkObjCWitnessSelector(TC, requirement, witness)) {
          Conformance->setInvalid();
          return;
        }
      }
    };

    // If we've already determined this witness, skip it.
    if (Conformance->hasWitness(requirement)) {
      finalizeWitness();
      continue;
    }

    // Make sure we've validated the requirement.
    if (!requirement->hasInterfaceType())
      TC.validateDecl(requirement);

    if (requirement->isInvalid() || !requirement->hasValidSignature()) {
      Conformance->setInvalid();
      continue;
    }

    // If this is a getter/setter for a funcdecl, ignore it.
    if (auto *FD = dyn_cast<FuncDecl>(requirement))
      if (FD->isAccessor())
        continue;
    
    // Try to resolve the witness via explicit definitions.
    switch (resolveWitnessViaLookup(requirement)) {
    case ResolveWitnessResult::Success:
      finalizeWitness();
      continue;

    case ResolveWitnessResult::ExplicitFailed:
      Conformance->setInvalid();
      continue;

    case ResolveWitnessResult::Missing:
      // Continue trying below.
      break;
    }

    // Try to resolve the witness via derivation.
    switch (resolveWitnessViaDerivation(requirement)) {
    case ResolveWitnessResult::Success:
      finalizeWitness();
      continue;

    case ResolveWitnessResult::ExplicitFailed:
      Conformance->setInvalid();
      continue;

    case ResolveWitnessResult::Missing:
      // Continue trying below.
      break;
    }

    // Try to resolve the witness via defaults.
    switch (resolveWitnessViaDefault(requirement)) {
    case ResolveWitnessResult::Success:
      finalizeWitness();
      continue;

    case ResolveWitnessResult::ExplicitFailed:
      Conformance->setInvalid();
      continue;

    case ResolveWitnessResult::Missing:
      // Continue trying below.
      break;
    }
  }

  emitDelayedDiags();

  // Except in specific whitelisted cases for Foundation/Swift
  // standard library compatibility, an _ObjectiveCBridgeable
  // conformance must appear in the same module as the definition of
  // the conforming type.
  //
  // Note that we check the module name to smooth over the difference
  // between an imported Objective-C module and its overlay.
  if (Proto->isSpecificProtocol(KnownProtocolKind::ObjectiveCBridgeable)) {
    if (auto nominal = Adoptee->getAnyNominal()) {
      if (!TC.Context.isTypeBridgedInExternalModule(nominal)) {
        auto nominalModule = nominal->getParentModule();
        auto conformanceModule = DC->getParentModule();
        if (nominalModule->getName() != conformanceModule->getName()) {
          TC.diagnose(Loc, diag::nonlocal_bridged_to_objc, nominal->getName(),
                      Proto->getName(), nominalModule->getName());
        }
      }
    }
  }
}

static void diagnoseConformanceFailure(TypeChecker &TC, Type T,
                                       ProtocolDecl *Proto,
                                       DeclContext *DC,
                                       SourceLoc ComplainLoc) {
  if (T->hasError())
    return;

  // If we're checking conformance of an existential type to a protocol,
  // do a little bit of extra work to produce a better diagnostic.
  if (T->isExistentialType() &&
      TC.containsProtocol(T, Proto, DC, None)) {

    if (!T->isObjCExistentialType()) {
      TC.diagnose(ComplainLoc, diag::protocol_does_not_conform_objc,
                  T, Proto->getDeclaredType());
      return;
    }

    TC.diagnose(ComplainLoc, diag::protocol_does_not_conform_static,
                T, Proto->getDeclaredType());
    return;
  }

  // Special case: diagnose conversion to ExpressibleByNilLiteral, since we
  // know this is something involving 'nil'.
  if (Proto->isSpecificProtocol(KnownProtocolKind::ExpressibleByNilLiteral)) {
    TC.diagnose(ComplainLoc, diag::cannot_use_nil_with_this_type, T);
    return;
  }

  // Special case: for enums with a raw type, explain that the failing
  // conformance to RawRepresentable was inferred.
  if (auto enumDecl = T->getEnumOrBoundGenericEnum()) {
    if (Proto->isSpecificProtocol(KnownProtocolKind::RawRepresentable) &&
        enumDecl->derivesProtocolConformance(Proto) && enumDecl->hasRawType()) {

      auto rawType = enumDecl->getRawType();

      TC.diagnose(enumDecl->getInherited()[0].getSourceRange().Start,
                  diag::enum_raw_type_nonconforming_and_nonsynthable,
                  T, rawType);

      // If the reason is that the raw type does not conform to
      // Equatable, say so.
      auto equatableProto = TC.getProtocol(enumDecl->getLoc(),
                                           KnownProtocolKind::Equatable);
      if (!equatableProto)
        return;

      if (!TC.conformsToProtocol(rawType, equatableProto, enumDecl, None)) {
        SourceLoc loc = enumDecl->getInherited()[0].getSourceRange().Start;
        TC.diagnose(loc, diag::enum_raw_type_not_equatable, rawType);
        return;
      }

      return;
    }
  }

  TC.diagnose(ComplainLoc, diag::type_does_not_conform,
              T, Proto->getDeclaredType());
}

void ConformanceChecker::diagnoseOrDefer(
       ValueDecl *requirement, bool isError,
       std::function<void(NormalProtocolConformance *)> fn) {
  if (isError)
    Conformance->setInvalid();

  if (SuppressDiagnostics) {
    // Stash this in the ASTContext for later emission.
    auto conformance = Conformance;
    TC.Context.addDelayedConformanceDiag(conformance,
                                         { requirement,
                                           [conformance, fn] {
                                              fn(conformance);
                                            },
                                           isError });
    return;
  }

  // Complain that the type does not conform, once.
  if (isError && !AlreadyComplained) {
    diagnoseConformanceFailure(TC, Adoptee, Proto, DC, Loc);
    AlreadyComplained = true;
  }

  fn(Conformance);
}

void ConformanceChecker::emitDelayedDiags() {
  auto diags = TC.Context.takeDelayedConformanceDiags(Conformance);

  assert(!SuppressDiagnostics && "Should not be suppressing diagnostics now");
  for (const auto &diag: diags) {
    diagnoseOrDefer(diag.Requirement, diag.IsError,
      [&](NormalProtocolConformance *conformance) {
        return diag.Callback();
    });
  }
}

Optional<ProtocolConformanceRef> TypeChecker::containsProtocol(
                                               Type T, ProtocolDecl *Proto,
                                               DeclContext *DC,
                                               ConformanceCheckOptions options) {
  // Existential types don't need to conform, i.e., they only need to
  // contain the protocol.
  if (T->isExistentialType()) {
    SmallVector<ProtocolDecl *, 4> protocols;
    T->getAnyExistentialTypeProtocols(protocols);

    for (auto P : protocols) {
      // Special case -- any class-bound protocol can be passed as an
      // AnyObject argument.
      if (P->requiresClass() &&
          Proto->isSpecificProtocol(KnownProtocolKind::AnyObject))
        return ProtocolConformanceRef(Proto);

      // If we found the protocol we're looking forward, return an abstract
      /// conformance to it.
      if (P == Proto || P->inheritsFrom(Proto))
        return ProtocolConformanceRef(Proto);
    }

    return None;
  }

  // Check whether this type conforms to the protocol.
  return conformsToProtocol(T, Proto, DC, options);
}

Optional<ProtocolConformanceRef> TypeChecker::conformsToProtocol(
                                   Type T, ProtocolDecl *Proto,
                                   DeclContext *DC,
                                   ConformanceCheckOptions options,
                                   SourceLoc ComplainLoc) {
  bool InExpression = options.contains(ConformanceCheckFlags::InExpression);

  auto recordDependency = [=](ProtocolConformance *conformance = nullptr) {
    if (!options.contains(ConformanceCheckFlags::SuppressDependencyTracking))
      if (auto nominal = T->getAnyNominal())
        recordConformanceDependency(DC, nominal, conformance, InExpression);
  };

  // Look up conformance in the module.
  ModuleDecl *M = DC->getParentModule();
  auto lookupResult = M->lookupConformance(T, Proto, this);
  if (!lookupResult) {
    if (ComplainLoc.isValid())
      diagnoseConformanceFailure(*this, T, Proto, DC, ComplainLoc);
    else
      recordDependency();

    return None;
  }

  // Store the conformance and record the dependency.
  if (lookupResult->isConcrete()) {
    recordDependency(lookupResult->getConcrete());
  } else {
    recordDependency();
  }

  // If we're using this conformance, note that.
  if (options.contains(ConformanceCheckFlags::Used) &&
      lookupResult->isConcrete()) {
    auto concrete = lookupResult->getConcrete();
    auto normalConf = concrete->getRootNormalConformance();

    // If the conformance is incomplete, queue it for completion.
    if (normalConf->isIncomplete())
      UsedConformances.insert(normalConf);

    // Record the usage of this conformance in the enclosing source
    // file.
    if (auto sf = DC->getParentSourceFile()) {
      sf->addUsedConformance(normalConf);
    }
  }
  return lookupResult;
}

std::pair<bool, Optional<ProtocolConformanceRef>>
TypeChecker::conformsToProtocol(Type T, ProtocolDecl *Proto,
                                DeclContext *DC,
                                ConformanceCheckOptions options,
                                SourceLoc ComplainLoc,
                                UnsatisfiedDependency *unsatisfiedDependency) {
  // If we have a callback to report dependencies, do so.
  // FIXME: Woefully inadequate.
  if (unsatisfiedDependency) {
    if (auto *classDecl = dyn_cast_or_null<ClassDecl>(T->getAnyNominal())) {
      if ((*unsatisfiedDependency)(requestTypeCheckSuperclass(classDecl)))
        return std::make_pair(true, None);
    }

    SmallVector<ProtocolDecl *, 2> protos;
    if (T->isExistentialType(protos)) {
      bool anyUnsatisfied = false;
      for (auto *proto : protos) {
        if ((*unsatisfiedDependency)(requestInheritedProtocols(proto)))
          anyUnsatisfied = true;
      }
      if (anyUnsatisfied)
        return std::make_pair(true, None);
    }
  }

  // Just punt to the older conformsToProtocol() and hope it doesn't
  // recurse.
  auto conformance = conformsToProtocol(T, Proto, DC, options, ComplainLoc);
  return std::make_pair(false, conformance);
}

/// Mark any _ObjectiveCBridgeable conformances in the given type as "used".
///
/// These conformances might not appear in any substitution lists produced
/// by Sema, since bridging is done at the SILGen level, so we have to
/// force them here to ensure SILGen can find them.
bool TypeChecker::useObjectiveCBridgeableConformances(DeclContext *dc,
                                                      Type type,
                                 UnsatisfiedDependency *unsatisfiedDependency) {
  class Walker : public TypeWalker {
    TypeChecker &TC;
    DeclContext *DC;
    ProtocolDecl *Proto;
    UnsatisfiedDependency *Callback;

  public:
    bool WasUnsatisfied;

    Walker(TypeChecker &tc, DeclContext *dc, ProtocolDecl *proto,
           UnsatisfiedDependency *unsatisfiedDependency)
      : TC(tc), DC(dc), Proto(proto),
        Callback(unsatisfiedDependency),
        WasUnsatisfied(false) { }

    Action walkToTypePre(Type ty) override {
      ConformanceCheckOptions options =
          (ConformanceCheckFlags::InExpression |
           ConformanceCheckFlags::Used |
           ConformanceCheckFlags::SuppressDependencyTracking);

      // If we have a nominal type, "use" its conformance to
      // _ObjectiveCBridgeable if it has one.
      if (auto *nominalDecl = ty->getAnyNominal()) {
        auto result = TC.conformsToProtocol(ty, Proto, DC, options,
                                            /*ComplainLoc=*/SourceLoc(),
                                            Callback);

        WasUnsatisfied |= result.first;
        if (WasUnsatisfied)
          return Action::Stop;

        // Set and Dictionary bridging also requires the conformance
        // of the key type to Hashable.
        if (nominalDecl == TC.Context.getSetDecl() ||
            nominalDecl == TC.Context.getDictionaryDecl()) {
          if (auto boundGeneric = ty->getAs<BoundGenericType>()) {
            auto args = boundGeneric->getGenericArgs();
            if (!args.empty()) {
              auto keyType = args[0];
              auto *hashableProto =
                TC.Context.getProtocol(KnownProtocolKind::Hashable);

              auto result = TC.conformsToProtocol(
                  keyType, hashableProto, DC, options,
                  /*ComplainLoc=*/SourceLoc(), Callback);

              WasUnsatisfied |= result.first;
              if (WasUnsatisfied)
                return Action::Stop;
            }
          }
        }
      }

      return Action::Continue;
    }
  };

  auto proto = getProtocol(SourceLoc(),
                           KnownProtocolKind::ObjectiveCBridgeable);
  if (!proto) return false;

  Walker walker(*this, dc, proto, unsatisfiedDependency);
  type.walk(walker);
  assert(!walker.WasUnsatisfied || unsatisfiedDependency);
  return walker.WasUnsatisfied;
}

bool TypeChecker::useObjectiveCBridgeableConformancesOfArgs(
       DeclContext *dc, BoundGenericType *bound,
       UnsatisfiedDependency *unsatisfiedDependency) {
  auto proto = getProtocol(SourceLoc(),
                           KnownProtocolKind::ObjectiveCBridgeable);
  if (!proto) return false;

  // Check whether the bound generic type itself is bridged to
  // Objective-C.
  ConformanceCheckOptions options =
    (ConformanceCheckFlags::InExpression |
     ConformanceCheckFlags::SuppressDependencyTracking);
  auto result = conformsToProtocol(
      bound->getDecl()->getDeclaredType(), proto, dc,
      options, /*ComplainLoc=*/SourceLoc(),
      unsatisfiedDependency);

  // Unsatisfied dependency case.
  if (result.first)
    return true;

  // Failure case.
  if (!result.second)
    return false;

  bool anyUnsatisfied = false;

  // Mark the conformances within the arguments.
  for (auto arg : bound->getGenericArgs()) {
    anyUnsatisfied |= useObjectiveCBridgeableConformances(
        dc, arg, unsatisfiedDependency);
  }

  return anyUnsatisfied;
}

void TypeChecker::useBridgedNSErrorConformances(DeclContext *dc, Type type) {
  auto bridgedStoredNSError = Context.getProtocol(
                                    KnownProtocolKind::BridgedStoredNSError);
  auto errorCodeProto = Context.getProtocol(
                              KnownProtocolKind::ErrorCodeProtocol);
  auto rawProto = Context.getProtocol(
                        KnownProtocolKind::RawRepresentable);

  if (!bridgedStoredNSError || !errorCodeProto || !rawProto)
    return;

  // _BridgedStoredNSError.
  auto conformance = conformsToProtocol(type, bridgedStoredNSError, dc,
                                        ConformanceCheckFlags::Used);
  if (conformance && conformance->isConcrete()) {
    // Hack: If we've used a conformance to the _BridgedStoredNSError
    // protocol, also use the RawRepresentable and _ErrorCodeProtocol
    // conformances on the Code associated type witness.
    if (auto codeType = ProtocolConformanceRef::getTypeWitnessByName(
                          type, *conformance, Context.Id_Code, this)) {
      (void)conformsToProtocol(codeType, errorCodeProto, dc,
                               ConformanceCheckFlags::Used);
      (void)conformsToProtocol(codeType, rawProto, dc,
                               ConformanceCheckFlags::Used);
    }
  }

  // _ErrorCodeProtocol.
  conformance =
  conformsToProtocol(type, errorCodeProto, dc,
                     (ConformanceCheckFlags::SuppressDependencyTracking|
                      ConformanceCheckFlags::Used));
  if (conformance && conformance->isConcrete()) {
    if (Type errorType = ProtocolConformanceRef::getTypeWitnessByName(
          type, *conformance, Context.Id_ErrorType, this)) {
      (void)conformsToProtocol(errorType, bridgedStoredNSError, dc,
                               ConformanceCheckFlags::Used);
    }
  }
}

void TypeChecker::checkConformance(NormalProtocolConformance *conformance) {
  MultiConformanceChecker checker(*this);
  checker.addConformance(conformance);
  checker.checkAllConformances();
}

/// Determine the score when trying to match two identifiers together.
static unsigned scoreIdentifiers(Identifier lhs, Identifier rhs,
                                 unsigned limit) {
  // Simple case: we have the same identifier.
  if (lhs == rhs) return 0;

  // One of the identifiers is empty. Use the length of the non-empty
  // identifier.
  if (lhs.empty() != rhs.empty())
    return lhs.empty() ? rhs.str().size() : lhs.str().size();

  // Compute the edit distance between the two names.
  return lhs.str().edit_distance(rhs.str(), true, limit);
}

/// Combine the given base name and first argument label into a single
/// name.
static StringRef
combineBaseNameAndFirstArgument(Identifier baseName,
                                Identifier firstArgName,
                                SmallVectorImpl<char> &scratch) {
  // Handle cases where one or the other name is empty.
  if (baseName.empty()) {
    if (firstArgName.empty()) return "";
    return firstArgName.str();
  }

  if (firstArgName.empty())
    return baseName.str();

  // Append the first argument name to the base name.
  scratch.clear();
  scratch.append(baseName.str().begin(), baseName.str().end());
  camel_case::appendSentenceCase(scratch, firstArgName.str());
  return StringRef(scratch.data(), scratch.size());
}

/// Compute the scope between two potentially-matching names, which is
/// effectively the sum of the edit distances between the corresponding
/// argument labels.
static Optional<unsigned> scorePotentiallyMatchingNames(DeclName lhs,
                                                        DeclName rhs,
                                                        bool isFunc,
                                                        unsigned limit) {
  // If there are a different number of argument labels, we're done.
  if (lhs.getArgumentNames().size() != rhs.getArgumentNames().size())
    return None;

  // Score the base name match. If there is a first argument for a
  // function, include its text along with the base name's text.
  unsigned score;
  if (lhs.getArgumentNames().empty() || !isFunc) {
    score = scoreIdentifiers(lhs.getBaseName(), rhs.getBaseName(), limit);
  } else {
    llvm::SmallString<16> lhsScratch;
    StringRef lhsFirstName =
      combineBaseNameAndFirstArgument(lhs.getBaseName(),
                                      lhs.getArgumentNames()[0],
                                      lhsScratch);

    llvm::SmallString<16> rhsScratch;
    StringRef rhsFirstName =
      combineBaseNameAndFirstArgument(rhs.getBaseName(),
                                      rhs.getArgumentNames()[0],
                                      rhsScratch);

    score = lhsFirstName.edit_distance(rhsFirstName.str(), true, limit);
  }
  if (score > limit) return None;

  // Compute the edit distance between matching argument names.
  for (unsigned i = isFunc ? 1 : 0; i < lhs.getArgumentNames().size(); ++i) {
    score += scoreIdentifiers(lhs.getArgumentNames()[i],
                              rhs.getArgumentNames()[i],
                              limit - score);
    if (score > limit) return None;
  }

  return score;
}

/// Apply omit-needless-words to the given declaration, if possible.
static Optional<DeclName> omitNeedlessWords(TypeChecker &tc, ValueDecl *value) {
  if (auto func = dyn_cast<AbstractFunctionDecl>(value))
    return tc.omitNeedlessWords(func);
  if (auto var = dyn_cast<VarDecl>(value)) {
    if (auto newName = tc.omitNeedlessWords(var))
      return DeclName(*newName);
    return None;
  }
  return None;
}

/// Determine the score between two potentially-matching declarations.
static Optional<unsigned> scorePotentiallyMatching(TypeChecker &tc,
                                                   ValueDecl *req,
                                                   ValueDecl *witness,
                                                   unsigned limit) {
  DeclName reqName = req->getFullName();
  DeclName witnessName = witness->getFullName();

  // Apply the omit-needless-words heuristics to both names.
  if (auto adjustedReqName = ::omitNeedlessWords(tc, req))
    reqName = *adjustedReqName;
  if (auto adjustedWitnessName = ::omitNeedlessWords(tc, witness))
    witnessName = *adjustedWitnessName;

  return scorePotentiallyMatchingNames(reqName, witnessName, isa<FuncDecl>(req),
                                       limit);
}

namespace {
  /// Describes actions one could take to suppress a warning about a
  /// nearly-matching witness for an optional requirement.
  enum class PotentialWitnessWarningSuppression {
    MoveToExtension,
    MoveToAnotherExtension
  };
} // end anonymous namespace

/// Determine we can suppress the warning about a potential witness nearly
/// matching an optional requirement by moving the declaration.
Optional<PotentialWitnessWarningSuppression>
canSuppressPotentialWitnessWarningWithMovement(ValueDecl *requirement,
                                               ValueDecl *witness) {
  // If the witness is within an extension, it can be moved to another
  // extension.
  if (isa<ExtensionDecl>(witness->getDeclContext()))
    return PotentialWitnessWarningSuppression::MoveToAnotherExtension;

  // A stored property cannot be moved to an extension.
  if (auto var = dyn_cast<VarDecl>(witness)) {
    if (var->hasStorage()) return None;
  }

  // If the witness is within a struct or enum, it can be freely moved to
  // another extension.
  if (isa<StructDecl>(witness->getDeclContext()) ||
      isa<EnumDecl>(witness->getDeclContext()))
    return PotentialWitnessWarningSuppression::MoveToExtension;

  // From here on, we only handle members of classes.
  auto classDecl = dyn_cast<ClassDecl>(witness->getDeclContext());
  if (!classDecl) return None;

  // If the witness is a designated or required initializer, we can't move it
  // to an extension.
  if (auto ctor = dyn_cast<ConstructorDecl>(witness)) {
    switch (ctor->getInitKind()) {
    case CtorInitializerKind::Designated:
      return None;

    case CtorInitializerKind::Convenience:
    case CtorInitializerKind::ConvenienceFactory:
    case CtorInitializerKind::Factory:
      break;
    }

    if (ctor->isRequired()) return None;
  }

  // We can move this entity to an extension.
  return PotentialWitnessWarningSuppression::MoveToExtension;
}

/// Determine we can suppress the warning about a potential witness nearly
/// matching an optional requirement by adding @nonobjc.
static bool
canSuppressPotentialWitnessWarningWithNonObjC(ValueDecl *requirement,
                                              ValueDecl *witness) {
  // The requirement must be @objc.
  if (!requirement->isObjC()) return false;

  // The witness must not have @nonobjc.
  if (witness->getAttrs().hasAttribute<NonObjCAttr>()) return false;

  // The witness must be @objc.
  if (!witness->isObjC()) return false;

  // ... but not explicitly.
  if (auto attr = witness->getAttrs().getAttribute<ObjCAttr>()) {
    if (!attr->isImplicit()) return false;
  }

  // And not because it has to be for overriding.
  if (auto overridden = witness->getOverriddenDecl())
    if (overridden->isObjC()) return false;

  // @nonobjc can be used to silence this warning.
  return true;
}

/// Get the length of the given full name, counting up the base name and all
/// argument labels.
static unsigned getNameLength(DeclName name) {
  unsigned length = 0;
  if (!name.getBaseName().empty()) length += name.getBaseName().str().size();
  for (auto arg : name.getArgumentNames()) {
    if (!arg.empty())
      length += arg.str().size();
  }
  return length;
}

/// Determine whether we should warn about the given witness being a close
/// match for the given optional requirement.
static bool shouldWarnAboutPotentialWitness(ValueDecl *req,
                                            ValueDecl *witness,
                                            Accessibility accessibility,
                                            unsigned score) {
  // If the warning couldn't be suppressed, don't warn.
  if (!canSuppressPotentialWitnessWarningWithMovement(req, witness) &&
      !canSuppressPotentialWitnessWarningWithNonObjC(req, witness))
    return false;

  // If the potential witness is already marked @nonobjc, don't warn.
  if (witness->getAttrs().hasAttribute<NonObjCAttr>())
    return false;

  // Don't warn if the potential witness has been explicitly given less
  // visibility than the conformance.
  if (witness->getFormalAccess() < accessibility) {
    if (auto attr = witness->getAttrs().getAttribute<AccessibilityAttr>())
      if (!attr->isImplicit()) return false;
  }

  // If the score is relatively high, don't warn: this is probably
  // unrelated.  Allow about one typo for every four properly-typed
  // characters, which prevents completely-wacky suggestions in many
  // cases.
  unsigned reqNameLen = getNameLength(req->getFullName());
  unsigned witnessNameLen = getNameLength(witness->getFullName());
  if (score > (std::min(reqNameLen, witnessNameLen)) / 4)
    return false;

  return true;
}

/// Diagnose a potential witness.
static void diagnosePotentialWitness(TypeChecker &tc,
                                     NormalProtocolConformance *conformance,
                                     ValueDecl *req,
                                     ValueDecl *witness,
                                     Accessibility accessibility) {
  auto proto = cast<ProtocolDecl>(req->getDeclContext());

  // Primary warning.
  tc.diagnose(witness, diag::optional_req_near_match,
              witness->getDescriptiveKind(),
              witness->getFullName(),
              req->getFullName(),
              proto->getFullName());

  // Describe why the witness didn't satisfy the requirement.
  auto dc = conformance->getDeclContext();
  RequirementEnvironment reqEnvironment(tc, dc, req, conformance);
  auto match = matchWitness(tc, conformance->getProtocol(), conformance,
                            dc, req, witness, reqEnvironment);
  if (match.Kind == MatchKind::ExactMatch &&
      req->isObjC() && !witness->isObjC()) {
    // Special case: note to add @objc.
    auto diag = tc.diagnose(witness,
                            diag::optional_req_nonobjc_near_match_add_objc);
    if (!witness->canInferObjCFromRequirement(req))
      fixDeclarationObjCName(diag, witness, req->getObjCRuntimeName());
  } else {
    diagnoseMatch(conformance->getDeclContext()->getParentModule(),
                  conformance, req, match);
  }

  // If moving the declaration can help, suggest that.
  if (auto move
        = canSuppressPotentialWitnessWarningWithMovement(req, witness)) {
    tc.diagnose(witness, diag::optional_req_near_match_move,
                witness->getFullName(), static_cast<unsigned>(*move));
  }

  // If adding 'private', 'fileprivate', or 'internal' can help, suggest that.
  if (accessibility > Accessibility::FilePrivate &&
      !witness->getAttrs().hasAttribute<AccessibilityAttr>()) {
    tc.diagnose(witness, diag::optional_req_near_match_accessibility,
                witness->getFullName(),
                accessibility)
      .fixItInsert(witness->getAttributeInsertionLoc(true), "private ");
  }

  // If adding @nonobjc can help, suggest that.
  if (canSuppressPotentialWitnessWarningWithNonObjC(req, witness)) {
    tc.diagnose(witness, diag::optional_req_near_match_nonobjc, false)
      .fixItInsert(witness->getAttributeInsertionLoc(false), "@nonobjc ");
  }

  tc.diagnose(req, diag::protocol_requirement_here, req->getFullName());
}

void TypeChecker::checkConformancesInContext(DeclContext *dc,
                                             IterableDeclContext *idc) {
  // Determine the accessibility of this conformance.
  Accessibility defaultAccessibility;
  if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
    // For anything with a Clang node, lazily check conformances.
    if (ext->hasClangNode()) return;

    Type extendedTy = ext->getExtendedType();
    if (!extendedTy)
      return;
    const NominalTypeDecl *nominal = extendedTy->getAnyNominal();
    if (!nominal)
      return;
    defaultAccessibility = nominal->getFormalAccess();
  } else {
    // For anything with a Clang node, lazily check conformances.
    auto nominal = cast<NominalTypeDecl>(dc);
    if (nominal->hasClangNode()) return;

    defaultAccessibility = nominal->getFormalAccess();
  }

  ReferencedNameTracker *tracker = nullptr;
  if (SourceFile *SF = dc->getParentSourceFile())
    tracker = SF->getReferencedNameTracker();


  // Check each of the conformances associated with this context.
  SmallVector<ConformanceDiagnostic, 4> diagnostics;
  auto conformances = dc->getLocalConformances(ConformanceLookupKind::All,
                                               &diagnostics,
                                               /*sorted=*/true);

  // The conformance checker bundle that checks all conformances in the context.
  MultiConformanceChecker groupChecker(*this);

  bool anyInvalid = false;
  for (auto conformance : conformances) {
    // Check and record normal conformances.
    if (auto normal = dyn_cast<NormalProtocolConformance>(conformance)) {
      groupChecker.addConformance(normal);
    }

    if (tracker)
      tracker->addUsedMember({conformance->getProtocol(), Identifier()},
                             defaultAccessibility > Accessibility::FilePrivate);
  }

  // Check all conformances.
  groupChecker.checkAllConformances();

  // Catalog all of members of this declaration context that satisfy
  // requirements of conformances in this context.
  SmallVector<ValueDecl *, 16>
    unsatisfiedReqs(groupChecker.getUnsatisfiedRequirements().begin(),
                    groupChecker.getUnsatisfiedRequirements().end());

  // Diagnose any conflicts attributed to this declaration context.
  for (const auto &diag : diagnostics) {
    // Figure out the declaration of the existing conformance.
    Decl *existingDecl = dyn_cast<NominalTypeDecl>(diag.ExistingDC);
    if (!existingDecl)
      existingDecl = cast<ExtensionDecl>(diag.ExistingDC);

    // Complain about redundant conformances.
    diagnose(diag.Loc, diag::redundant_conformance,
             dc->getDeclaredInterfaceType(),
             diag.Protocol->getName());

    // Special case: explain that 'RawRepresentable' conformance
    // is implied for enums which already declare a raw type.
    if (auto enumDecl = dyn_cast<EnumDecl>(existingDecl)) {
      if (diag.Protocol->isSpecificProtocol(KnownProtocolKind::RawRepresentable)
          && enumDecl->derivesProtocolConformance(diag.Protocol)
          && enumDecl->hasRawType()
          && enumDecl->getInherited()[0].getSourceRange().isValid()) {
        diagnose(enumDecl->getInherited()[0].getSourceRange().Start,
                 diag::enum_declares_rawrep_with_raw_type,
                 dc->getDeclaredInterfaceType(), enumDecl->getRawType());
        continue;
      }
    }

    diagnose(existingDecl, diag::declared_protocol_conformance_here,
             dc->getDeclaredInterfaceType(),
             static_cast<unsigned>(diag.ExistingKind),
             diag.Protocol->getName(),
             diag.ExistingExplicitProtocol->getName());
  }

  // If there were any unsatisfied requirements, check whether there
  // are any near-matches we should diagnose.
  if (!unsatisfiedReqs.empty() && !anyInvalid) {
    // Find all of the members that aren't used to satisfy
    // requirements, and check whether they are close to an
    // unsatisfied or defaulted requirement.
    for (auto member : idc->getMembers()) {
      // Filter out anything that couldn't satisfy one of the
      // requirements or was used to satisfy a different requirement.
      auto value = dyn_cast<ValueDecl>(member);
      if (!value) continue;
      if (isa<TypeDecl>(value)) continue;
      if (!value->getFullName()) continue;

      // If this declaration overrides another declaration, the signature is
      // fixed; don't complain about near misses.
      if (value->getOverriddenDecl()) continue;

      // If this member is a witness to any @objc requirement, ignore it.
      if (!findWitnessedObjCRequirements(value, /*anySingleRequirement=*/true)
            .empty())
        continue;

      // Find the unsatisfied requirements with the nearest-matching
      // names.
      SmallVector<ValueDecl *, 4> bestOptionalReqs;
      unsigned bestScore = UINT_MAX;
      for (auto req : unsatisfiedReqs) {
        // Skip unavailable requirements.
        if (req->getAttrs().isUnavailable(Context)) continue;

        // Score this particular optional requirement.
        auto score = scorePotentiallyMatching(*this, req, value, bestScore);
        if (!score) continue;

        // If the score is better than the best we've seen, update the best
        // and clear out the list.
        if (*score < bestScore) {
          bestOptionalReqs.clear();
          bestScore = *score;
        }

        // If this score matches the (possible new) best score, record it.
        if (*score == bestScore)
          bestOptionalReqs.push_back(req);
      }

      // If we found some requirements with nearly-matching names, diagnose
      // the first one.
      if (bestScore < UINT_MAX) {
        bestOptionalReqs.erase(
          std::remove_if(
            bestOptionalReqs.begin(),
            bestOptionalReqs.end(),
            [&](ValueDecl *req) {
              return !shouldWarnAboutPotentialWitness(req, value,
                                                      defaultAccessibility,
                                                      bestScore);
            }),
          bestOptionalReqs.end());
      }

      // If we have something to complain about, do so.
      if (!bestOptionalReqs.empty()) {
        auto req = bestOptionalReqs[0];
        bool diagnosed = false;
        for (auto conformance : conformances) {
          if (conformance->getProtocol() == req->getDeclContext()) {
            diagnosePotentialWitness(*this,
                                     conformance->getRootNormalConformance(),
                                     req, value, defaultAccessibility);
            diagnosed = true;
            break;
          }
        }
        assert(diagnosed && "Failed to find conformance to diagnose?");
        (void)diagnosed;

        // Remove this optional requirement from the list. We don't want to
        // complain about it twice.
        unsatisfiedReqs.erase(std::find(unsatisfiedReqs.begin(),
                                        unsatisfiedReqs.end(),
                                        req));
      }
    }

    // For any unsatisfied optional @objc requirements that remain
    // unsatisfied, note them in the AST for @objc selector collision
    // checking.
    for (auto req : unsatisfiedReqs) {
      // Skip non-@objc requirements.
      if (!req->isObjC()) continue;

      // Skip unavailable requirements.
      if (req->getAttrs().isUnavailable(Context)) continue;

      // Record this requirement.
      if (auto funcReq = dyn_cast<AbstractFunctionDecl>(req)) {
        Context.recordObjCUnsatisfiedOptReq(dc, funcReq);
      } else {
        auto storageReq = cast<AbstractStorageDecl>(req);
        if (auto getter = storageReq->getGetter())
          Context.recordObjCUnsatisfiedOptReq(dc, getter);
        if (auto setter = storageReq->getSetter())
          Context.recordObjCUnsatisfiedOptReq(dc, setter);
      }
    }
  }
}

llvm::TinyPtrVector<ValueDecl *>
TypeChecker::findWitnessedObjCRequirements(const ValueDecl *witness,
                                           bool anySingleRequirement) {
  llvm::TinyPtrVector<ValueDecl *> result;

  // Types don't infer @objc this way.
  if (isa<TypeDecl>(witness)) return result;

  auto dc = witness->getDeclContext();
  auto nominal = dc->getAsNominalTypeOrNominalTypeExtensionContext();
  if (!nominal) return result;

  DeclName name = witness->getFullName();
  auto accessorKind = AccessorKind::NotAccessor;
  if (auto *fn = dyn_cast<FuncDecl>(witness)) {
    accessorKind = fn->getAccessorKind();
    switch (accessorKind) {
    case AccessorKind::IsAddressor:
    case AccessorKind::IsMutableAddressor:
    case AccessorKind::IsMaterializeForSet:
      // These accessors are never exposed to Objective-C.
      return result;
    case AccessorKind::IsDidSet:
    case AccessorKind::IsWillSet:
      // These accessors are folded into the setter.
      return result;
    case AccessorKind::IsGetter:
    case AccessorKind::IsSetter:
      // These are found relative to the main decl.
      name = fn->getAccessorStorageDecl()->getFullName();
      break;
    case AccessorKind::NotAccessor:
      // Do nothing.
      break;
    }
  }

  for (auto proto : nominal->getAllProtocols()) {
    // We only care about Objective-C protocols.
    if (!proto->isObjC()) continue;

    Optional<ProtocolConformance *> conformance;
    for (auto req : proto->lookupDirect(name, true)) {
      // Skip anything in a protocol extension.
      if (req->getDeclContext() != proto) continue;

      // Skip types.
      if (isa<TypeDecl>(req)) continue;

      // Skip unavailable requirements.
      if (req->getAttrs().isUnavailable(Context)) continue;

      // Dig out the conformance.
      if (!conformance.hasValue()) {
        SmallVector<ProtocolConformance *, 2> conformances;
        nominal->lookupConformance(dc->getParentModule(), proto,
                                   conformances);
        if (conformances.size() == 1)
          conformance = conformances.front();
        else
          conformance = nullptr;
      }
      if (!*conformance) continue;

      const Decl *found = (*conformance)->getWitness(req, this).getDecl();

      if (!found) {
        // If we have an optional requirement in an inherited conformance,
        // check whether the potential witness matches the requirement.
        // FIXME: for now, don't even try this with generics involved. We
        // should be tracking how subclasses implement optional requirements,
        // in which case the getWitness() check above would suffice.
        if (!req->getAttrs().hasAttribute<OptionalAttr>() ||
            !isa<InheritedProtocolConformance>(*conformance)) {
          continue;
        }

        auto normal = (*conformance)->getRootNormalConformance();
        auto dc = (*conformance)->getDeclContext();
        if (dc->getGenericSignatureOfContext() ||
            normal->getDeclContext()->getGenericSignatureOfContext()) {
          continue;
        }

        const ValueDecl *witnessToMatch = witness;
        if (accessorKind != AccessorKind::NotAccessor)
          witnessToMatch = cast<FuncDecl>(witness)->getAccessorStorageDecl();

        RequirementEnvironment reqEnvironment(*this, dc, req, *conformance);
        if (matchWitness(*this, proto, *conformance,
                         witnessToMatch->getDeclContext(), req,
                         const_cast<ValueDecl *>(witnessToMatch),
                         reqEnvironment).Kind == MatchKind::ExactMatch) {
          if (accessorKind != AccessorKind::NotAccessor) {
            auto *storageReq = dyn_cast<AbstractStorageDecl>(req);
            if (!storageReq)
              continue;
            req = storageReq->getAccessorFunction(accessorKind);
          }
          result.push_back(req);
          if (anySingleRequirement) return result;
          continue;
        }

        continue;
      }

      // Dig out the appropriate accessor, if necessary.
      if (accessorKind != AccessorKind::NotAccessor) {
        auto *storageReq = dyn_cast<AbstractStorageDecl>(req);
        auto *storageFound = dyn_cast_or_null<AbstractStorageDecl>(found);
        if (!storageReq || !storageFound)
          continue;
        req = storageReq->getAccessorFunction(accessorKind);
        if (!req)
          continue;
        found = storageFound->getAccessorFunction(accessorKind);
      }

      // Determine whether the witness for this conformance is in fact
      // our witness.
      if (found == witness) {
        result.push_back(req);
        if (anySingleRequirement) return result;
        continue;
      }
    }
  }

  // Sort the results.
  if (result.size() > 2) {
    std::stable_sort(result.begin(), result.end(),
                     [&](ValueDecl *lhs, ValueDecl *rhs) {
                       ProtocolDecl *lhsProto
                         = cast<ProtocolDecl>(lhs->getDeclContext());
                       ProtocolDecl *rhsProto
                         = cast<ProtocolDecl>(rhs->getDeclContext());
                       return ProtocolType::compareProtocols(&lhsProto,
                                                             &rhsProto) < 0;
                     });
  }
  return result;
}

void TypeChecker::resolveTypeWitness(
       const NormalProtocolConformance *conformance,
       AssociatedTypeDecl *assocType) {
  llvm::SetVector<ValueDecl*> MissingWitnesses;
  ConformanceChecker checker(
                       *this, 
                       const_cast<NormalProtocolConformance*>(conformance),
                       MissingWitnesses);
  checker.resolveSingleTypeWitness(assocType);
}

void TypeChecker::resolveWitness(const NormalProtocolConformance *conformance,
                                 ValueDecl *requirement) {
  llvm::SetVector<ValueDecl*> MissingWitnesses;
  ConformanceChecker checker(
                       *this, 
                       const_cast<NormalProtocolConformance*>(conformance),
                       MissingWitnesses);
  checker.resolveSingleWitness(requirement);
}

ProtocolConformance *TypeChecker::resolveInheritedConformance(
       const NormalProtocolConformance *conformance,
       ProtocolDecl *inherited) {
  auto inheritedConformance =
    conformsToProtocol(conformance->getType(), inherited,
                       conformance->getDeclContext(),
                       ConformanceCheckFlags::InExpression);
  if (inheritedConformance && inheritedConformance->isConcrete())
    return inheritedConformance->getConcrete();

  return nullptr;
}

ValueDecl *TypeChecker::deriveProtocolRequirement(DeclContext *DC,
                                                  NominalTypeDecl *TypeDecl,
                                                  ValueDecl *Requirement) {
  // Note: whenever you update this function, also update
  // DerivedConformance::getDerivableRequirement.
  auto *protocol = cast<ProtocolDecl>(Requirement->getDeclContext());

  auto knownKind = protocol->getKnownProtocolKind();
  
  if (!knownKind)
    return nullptr;

  auto Decl = DC->getInnermostDeclarationDeclContext();

  switch (*knownKind) {
  case KnownProtocolKind::RawRepresentable:
    return DerivedConformance::deriveRawRepresentable(*this, Decl,
                                                      TypeDecl, Requirement);

  case KnownProtocolKind::Equatable:
    return DerivedConformance::deriveEquatable(*this, Decl, TypeDecl, Requirement);
  
  case KnownProtocolKind::Hashable:
    return DerivedConformance::deriveHashable(*this, Decl, TypeDecl, Requirement);
    
  case KnownProtocolKind::BridgedNSError:
    return DerivedConformance::deriveBridgedNSError(*this, Decl, TypeDecl,
                                                    Requirement);

  default:
    return nullptr;
  }
}

Type TypeChecker::deriveTypeWitness(DeclContext *DC,
                                    NominalTypeDecl *TypeDecl,
                                    AssociatedTypeDecl *AssocType) {
  auto *protocol = cast<ProtocolDecl>(AssocType->getDeclContext());

  auto knownKind = protocol->getKnownProtocolKind();
  
  if (!knownKind)
    return nullptr;

  auto Decl = DC->getInnermostDeclarationDeclContext();

  switch (*knownKind) {
  case KnownProtocolKind::RawRepresentable:
    return DerivedConformance::deriveRawRepresentable(*this, Decl,
                                                      TypeDecl, AssocType);
        
  default:
    return nullptr;
  }
}

namespace {
  class DefaultWitnessChecker : public WitnessChecker {
    
  public:
    DefaultWitnessChecker(TypeChecker &tc,
                          ProtocolDecl *proto)
      : WitnessChecker(tc, proto, proto->getDeclaredType(), proto) { }

    ResolveWitnessResult resolveWitnessViaLookup(ValueDecl *requirement);
    void recordWitness(ValueDecl *requirement, const RequirementMatch &match,
                       RequirementEnvironment &&reqEnvironment);
  };
} // end anonymous namespace

ResolveWitnessResult
DefaultWitnessChecker::resolveWitnessViaLookup(ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Must be a value witness");

  // Find the best default witness for the requirement.
  SmallVector<RequirementMatch, 4> matches;
  unsigned numViable = 0;
  unsigned bestIdx = 0;
  bool doNotDiagnoseMatches = false;

  RequirementEnvironment reqEnvironment(TC, DC, requirement,
                                        /*conformance=*/nullptr);
  if (findBestWitness(
                 requirement, nullptr, nullptr, reqEnvironment,
                 /* out parameters: */
                 matches, numViable, bestIdx, doNotDiagnoseMatches)) {

    auto &best = matches[bestIdx];

    // Perform the same checks as conformance witness matching, but silently
    // ignore the candidate instead of diagnosing anything.
    auto check = checkWitness(AccessScope::getPublic(), requirement, best);
    if (check.Kind != CheckKind::Success)
      return ResolveWitnessResult::ExplicitFailed;

    // Record the match.
    recordWitness(requirement, best, std::move(reqEnvironment));
    return ResolveWitnessResult::Success;
  }

  // We have either no matches or an ambiguous match.
  return ResolveWitnessResult::Missing;
}

void DefaultWitnessChecker::recordWitness(
                                  ValueDecl *requirement,
                                  const RequirementMatch &match,
                                  RequirementEnvironment &&reqEnvironment) {
  Proto->setDefaultWitness(requirement,
                           match.getWitness(TC.Context,
                                            std::move(reqEnvironment)));

  // Synthesize accessors for the protocol witness table to use.
  if (auto storage = dyn_cast<AbstractStorageDecl>(match.Witness))
    TC.synthesizeWitnessAccessorsForStorage(
                                        cast<AbstractStorageDecl>(requirement),
                                        storage);
}

void TypeChecker::inferDefaultWitnesses(ProtocolDecl *proto) {
  DefaultWitnessChecker checker(*this, proto);

  for (auto *requirement : proto->getMembers()) {
    if (requirement->isInvalid())
      continue;

    auto *valueDecl = dyn_cast<ValueDecl>(requirement);
    if (!valueDecl)
      continue;

    if (isa<TypeDecl>(valueDecl))
      continue;

    if (!valueDecl->isProtocolRequirement())
      continue;

    checker.resolveWitnessViaLookup(valueDecl);
  }
}

bool TypeChecker::isProtocolExtensionUsable(DeclContext *dc, Type type,
                                            ExtensionDecl *protocolExtension) {
  using namespace constraints;

  assert(protocolExtension->getAsProtocolExtensionContext() &&
         "Only intended for protocol extensions");

  resolveExtension(protocolExtension);

  // Dig down to the type we care about.
  type = type->getInOutObjectType()->getRValueType();
  if (auto metaTy = type->getAs<AnyMetatypeType>())
    type = metaTy->getInstanceType();

  // Unconstrained protocol extensions are always usable.
  if (!protocolExtension->isConstrainedExtension())
    return true;

  // If the type still has parameters, the constrained extension is considered
  // unusable.
  if (type->hasTypeParameter() || type->hasTypeVariable())
    return false;

  // Set up a constraint system where we open the generic parameters of the
  // protocol extension.
  ConstraintSystem cs(*this, dc, None);
  llvm::DenseMap<CanType, TypeVariableType *> replacements;
  auto genericSig = protocolExtension->getGenericSignature();
  
  cs.openGeneric(protocolExtension, protocolExtension,
                 genericSig->getGenericParams(),
                 genericSig->getRequirements(), false,
                 ConstraintLocatorBuilder(nullptr), replacements);

  // Bind the 'Self' type variable to the provided type.
  CanType selfType = genericSig->getGenericParams().back()->getCanonicalType();
  auto selfTypeVar = replacements[selfType];
  cs.addConstraint(ConstraintKind::Bind, selfTypeVar, type, nullptr);

  // If we can solve the solution, the protocol extension is usable.
  return cs.solveSingle().hasValue();
}

void TypeChecker::recordKnownWitness(NormalProtocolConformance *conformance,
                                     ValueDecl *req, ValueDecl *witness) {
  // Match the witness. This should never fail, but it does allow renaming
  // (because property behaviors rely on renaming).
  auto dc = conformance->getDeclContext();
  RequirementEnvironment reqEnvironment(*this, dc, req, conformance);
  auto match = matchWitness(*this, conformance->getProtocol(), conformance,
                            dc, req, witness, reqEnvironment);
  if (match.Kind != MatchKind::ExactMatch &&
      match.Kind != MatchKind::RenamedMatch) {
    diagnose(witness, diag::property_behavior_conformance_broken,
             witness->getFullName(), conformance->getType());
    return;
  }

  conformance->setWitness(req,
                          match.getWitness(Context, std::move(reqEnvironment)));
}

Type TypeChecker::getWitnessType(Type type, ProtocolDecl *protocol,
                                 ProtocolConformanceRef conformance,
                                 Identifier name,
                                 Diag<> brokenProtocolDiag) {
  Type ty = ProtocolConformanceRef::getTypeWitnessByName(type, conformance,
                                                         name, this);
  if (!ty &&
      !(conformance.isConcrete() && conformance.getConcrete()->isInvalid()))
    diagnose(protocol->getLoc(), brokenProtocolDiag);

  return ty;
}
