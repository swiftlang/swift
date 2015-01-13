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
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;

namespace {
  struct RequirementMatch;

  /// Helper class whose objects invoke a function when destroyed.
  ///
  /// Use this via the SCOPE_GUARD macro.
  template <typename F>
  class ScopeGuard {
    F &TheFunc;

  public:
    ScopeGuard(F &func) : TheFunc(func) { }
    ~ScopeGuard() { TheFunc(); }

    ScopeGuard(const ScopeGuard &) = delete;
    ScopeGuard &operator=(const ScopeGuard &) = delete;
  };

#define JOIN(X, Y) JOIN2(X, Y)
#define JOIN2(X, Y) X ## Y

/// Provides a block of code that will be executed when exiting the
/// current scope, e.g., to clean up resources not held by some other
/// RAII object.
#define SCOPE_GUARD(Code)                                 \
  auto JOIN(scope_guard_func_, __LINE__) = [&] {Code;};   \
  ScopeGuard<decltype(JOIN(scope_guard_func_, __LINE__))> \
    JOIN(scope_guard_, __LINE__)(JOIN(scope_guard_func_, __LINE__));
  
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
    
    /// Type witnesses that are currently being resolved.
    llvm::SmallPtrSet<AssociatedTypeDecl *, 4> ResolvingTypeWitnesses;

    /// Witnesses that are currently being resolved.
    llvm::SmallPtrSet<ValueDecl *, 4> ResolvingWitnesses;

    /// Caches the set of associated types that are referenced in each
    /// requirement.
    llvm::DenseMap<ValueDecl *, llvm::SmallVector<AssociatedTypeDecl *, 2>>
      ReferencedAssociatedTypes;

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
    /// \param wasDeducedOrDefaulted Whether this witness was deduced or
    /// defaulted (rather than being explicitly provided).
    void recordTypeWitness(AssociatedTypeDecl *assocType, Type type,
                           bool wasDeducedOrDefaulted);

    /// Resolve a (non-type) witness via name lookup.
    ResolveWitnessResult resolveWitnessViaLookup(ValueDecl *requirement);

    /// Resolve a (non-type) witness via derivation.
    ResolveWitnessResult resolveWitnessViaDerivation(ValueDecl *requirement);

    /// Resolve a (non-type) witness via default definition or optional.
    ResolveWitnessResult resolveWitnessViaDefault(ValueDecl *requirement);

    /// Attempt to resolve a type witness via member name lookup.
    ResolveWitnessResult resolveTypeWitnessViaLookup(
                           AssociatedTypeDecl *assocType);

    /// Attempt to resolve a type witness via a default definition.
    ResolveWitnessResult resolveTypeWitnessViaDefault(
                           AssociatedTypeDecl *assocType);

    /// Attempt to resolve a type witness via derivation.
    ResolveWitnessResult resolveTypeWitnessViaDerivation(
                           AssociatedTypeDecl *assocType);

  public:
    ConformanceChecker(TypeChecker &tc, NormalProtocolConformance *conformance)
      : TC(tc), Conformance(conformance),
        Proto(conformance->getProtocol()),
        Adoptee(conformance->getType()), 
        DC(conformance->getDeclContext()),
        Loc(conformance->getLoc()) { }

    /// Resolve the type witness for the given associated type as
    /// directly as possible, only resolving other witnesses if
    /// needed, e.g., to deduce this type witness.
    ///
    /// This entry point is designed to be used when the type witness
    /// for a particular associated type and adoptee is required,
    /// before the conformance has been completed checked.
    void resolveSingleTypeWitness(AssociatedTypeDecl *assocType);

    /// Resolve the witness for the given non-type requirement as
    /// directly as possible, only resolving other witnesses if
    /// needed, e.g., to determine type witnesses used within the
    /// requirement.
    ///
    /// This entry point is designed to be used when the witness for a
    /// particular requirement and adoptee is required, before the
    /// conformance has been completed checked.
    void resolveSingleWitness(ValueDecl *requirement);

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

    /// \brief The witness matched the requirement with some renaming.
    RenamedMatch,

    /// \brief The witness is invalid or has an invalid type.
    WitnessInvalid,

    /// \brief The kind of the witness and requirement differ, e.g., one
    /// is a function and the other is a variable.
    KindConflict,

    /// \brief The types conflict.
    TypeConflict,

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
  };

  /// \brief Describes a match between a requirement and a witness.
  struct RequirementMatch {
    RequirementMatch(ValueDecl *witness, MatchKind kind,
                     Type witnessType = Type())
      : Witness(witness), Kind(kind), WitnessType(witnessType)
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

    /// \brief Determine whether this match is viable.
    bool isViable() const {
      switch(Kind) {
      case MatchKind::ExactMatch:
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
        return false;
      }
    }

    /// \brief Determine whether this requirement match has a witness type.
    bool hasWitnessType() const {
      switch(Kind) {
      case MatchKind::ExactMatch:
      case MatchKind::RenamedMatch:
      case MatchKind::TypeConflict:
        return true;

      case MatchKind::WitnessInvalid:
      case MatchKind::KindConflict:
      case MatchKind::StaticNonStaticConflict:
      case MatchKind::SettableConflict:
      case MatchKind::PrefixNonPrefixConflict:
      case MatchKind::PostfixNonPostfixConflict:
      case MatchKind::MutatingConflict:
      case MatchKind::NoReturnConflict:
        return false;
      }
    }

    /// \brief Associated types determined by matching this requirement.
    SmallVector<std::pair<AssociatedTypeDecl *, Type>, 2>
      AssociatedTypeDeductions;
    
    /// \brief Associated type substitutions needed to match the witness.
    SmallVector<Substitution, 2> WitnessSubstitutions;
  };
}

///\ brief Decompose the given type into a set of tuple elements.
static SmallVector<TupleTypeElt, 4> decomposeIntoTupleElements(Type type) {
  SmallVector<TupleTypeElt, 4> result;

  if (auto tupleTy = dyn_cast<TupleType>(type.getPointer())) {
    result.append(tupleTy->getFields().begin(), tupleTy->getFields().end());
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
    TypeVariableType *SelfTypeVar = nullptr;

    NormalProtocolConformance *Conformance;
    DeclContext *DC;
    ProtocolDecl *Proto;
    llvm::DenseMap<TypeVariableType *, AssociatedTypeDecl *> &OpenedAssocTypes;

  public:
    RequirementTypeOpener(
        NormalProtocolConformance *conformance,
        DeclContext *dc,
        llvm::DenseMap<TypeVariableType *, AssociatedTypeDecl*>
          &openedAssocTypes)
      : Conformance(conformance), DC(dc), Proto(conformance->getProtocol()),
        OpenedAssocTypes(openedAssocTypes)
    {
    }

    virtual void openedGenericParameter(GenericTypeParamType *param,
                                        TypeVariableType *typeVar,
                                        Type &replacementType) {
      // If this is the 'Self' type, record it.
      if (param->getDepth() == 0 && param->getIndex() == 0)
        SelfTypeVar = typeVar;
      else
        replacementType = ArchetypeBuilder::mapTypeIntoContext(DC, param);
    }

    virtual bool shouldBindAssociatedType(Type baseType,
                                          TypeVariableType *baseTypeVar,
                                          AssociatedTypeDecl *assocType,
                                          TypeVariableType *memberTypeVar,
                                          Type &replacementType) {
      // If the base is our 'Self' type, we might have a witness for this
      // associated type already.
      if (baseTypeVar == SelfTypeVar &&
          cast<ProtocolDecl>(assocType->getDeclContext()) == Proto) {
        // If we know about this associated type already, we know its
        // replacement type. Otherwise, record it.
        if (Conformance->hasTypeWitness(assocType))
          replacementType = Conformance->getTypeWitness(assocType, nullptr)
                              .getReplacement();
        else
          OpenedAssocTypes[memberTypeVar] = assocType;

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

} // end anonymous namespace

static std::pair<Type,Type> getTypesToCompare(ValueDecl *reqt, Type reqtType,
                                              Type witnessType) {
  // Only do this hack when the protocol is @objc.
  if (reqt->isObjC()) {
    // If the requirement type is an ImplicitlyUnwrappedOptional type, pretend it isn't.
    if (auto reqtValueType = reqtType->getImplicitlyUnwrappedOptionalObjectType()) {
      reqtType = reqtValueType;

      // But be sure to allow the witness type to be explicitly optional.
      if (auto witnessValueType = witnessType->getAnyOptionalObjectType()) {
        witnessType = witnessValueType;
      }
    }
  }

  return {reqtType, witnessType};
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
      witnessMutating = 
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
             const std::function<RequirementMatch(bool)> &finalize) {
  assert(!req->isInvalid() && "Cannot have an invalid requirement here");

  auto protocol = conformance->getProtocol();

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

  bool anyRenaming = false;
  if (decomposeFunctionType) {
    // Decompose function types into parameters and result type.
    auto reqInputType = reqType->castTo<AnyFunctionType>()->getInput();
    auto reqResultType = reqType->castTo<AnyFunctionType>()->getResult()
                           ->getRValueType();
    auto witnessInputType = witnessType->castTo<AnyFunctionType>()
                              ->getInput();
    auto witnessResultType = witnessType->castTo<AnyFunctionType>()
                               ->getResult()->getRValueType();

    // Result types must match.
    // FIXME: Could allow (trivial?) subtyping here.
    if (!ignoreReturnType) {
      auto typePair = getTypesToCompare(req, reqResultType, witnessResultType);
      if (auto result = matchTypes(typePair.first, typePair.second)) {
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
      return RequirementMatch(witness, MatchKind::TypeConflict, witnessType);

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

        // For an Objective-C requirement, all but the first parameter name is
        // significant.
        // FIXME: Specialize the match failure kind.
        // FIXME: Constructors care about the first name.
        // FIXME: This should be dead code.
        if (protocol->isObjC() && i > 0)
          return RequirementMatch(witness, MatchKind::TypeConflict,
                                  witnessType);
      }

      // Gross hack: strip a level of unchecked-optionality off both
      // sides when matching against a protocol imported from Objective-C.
      auto typePair = getTypesToCompare(req, reqParams[i].getType(),
                                        witnessParams[i].getType());

      // Check whether the parameter types match.
      if (auto result = matchTypes(typePair.first, typePair.second)) {
        return *result;
      }

      // FIXME: Consider default arguments here?
    }
  } else {
    // Simple case: add the constraint.
    auto typePair = getTypesToCompare(req, reqType, witnessType);
    if (auto result = matchTypes(typePair.first, typePair.second)) {
      return *result;
    }
  }

  // Now finalize the match.
  return finalize(anyRenaming);
}

static RequirementMatch
matchWitness(TypeChecker &tc, NormalProtocolConformance *conformance,
             DeclContext *dc, ValueDecl *req, ValueDecl *witness) {
  // Initialized by the setup operation.
  Optional<constraints::ConstraintSystem> cs;
  constraints::ConstraintLocator *locator = nullptr;
  Type witnessType, openWitnessType;
  Type openedFullWitnessType;
  Type reqType, openedFullReqType;
  llvm::DenseMap<TypeVariableType *, AssociatedTypeDecl *> openedAssocTypes;
  
  // Set up the constraint system for matching.
  auto setup = [&]() -> std::tuple<Optional<RequirementMatch>, Type, Type> {
    // Construct a constraint system to use to solve the equality between
    // the required type and the witness type.
    cs.emplace(tc, dc, constraints::ConstraintSystemOptions());
    
    auto model = conformance->getType();

    // Open up the witness type.
    witnessType = witness->getInterfaceType();
    // FIXME: witness as a base locator?
    locator = cs->getConstraintLocator(nullptr);
    if (witness->getDeclContext()->isTypeContext()) {
      std::tie(openedFullWitnessType, openWitnessType) 
        = cs->getTypeOfMemberReference(model, witness,
                                      /*isTypeReference=*/false,
                                      /*isDynamicResult=*/false,
                                      locator,
                                      /*opener=*/nullptr);
    } else {
      std::tie(openedFullWitnessType, openWitnessType) 
      = cs->getTypeOfReference(witness,
                              /*isTypeReference=*/false,
                              /*isDynamicResult=*/false,
                              locator,
                              /*opener=*/nullptr);
    }
    openWitnessType = openWitnessType->getRValueType();
    
    // Open up the type of the requirement. We only truly open 'Self' and
    // its associated types (recursively); inner generic type parameters get
    // mapped to their archetypes directly.
    DeclContext *reqDC = req->getPotentialGenericDeclContext();
    RequirementTypeOpener reqTypeOpener(conformance, reqDC, openedAssocTypes);
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
    cs->addConstraint(constraints::ConstraintKind::Equal, reqType, witnessType,
                      locator);
    // FIXME: Check whether this has already failed.
    return None;
  };

  // Finalize the match.
  auto finalize = [&](bool anyRenaming) -> RequirementMatch {
    // Try to solve the system.
    SmallVector<constraints::Solution, 1> solutions;
    if (cs->solve(solutions, FreeTypeVariableBinding::Allow)) {
      return RequirementMatch(witness, MatchKind::TypeConflict,
                              witnessType);
    }
    auto &solution = solutions.front();
    
    // Success. Form the match result.
    RequirementMatch result(witness,
                            anyRenaming? MatchKind::RenamedMatch
                                       : MatchKind::ExactMatch,
                            witnessType);

    // For any associated types for which we deduced replacement types,
    // record them now.
    for (const auto &opened : openedAssocTypes) {
      auto replacement = solution.simplifyType(tc, opened.first);
      
      // If any type variables remain in the replacement, we couldn't
      // fully deduce it.
      if (replacement->hasTypeVariable())
        continue;
      
      result.AssociatedTypeDeductions.push_back({opened.second, replacement});
    }
    
    if (openedFullWitnessType->hasTypeVariable()) {
      // Figure out the context we're substituting into.
      auto witnessDC = witness->getPotentialGenericDeclContext();
      
      // Compute the set of substitutions we'll need for the witness.
      solution.computeSubstitutions(witness->getInterfaceType(),
                                    witnessDC,
                                    openedFullWitnessType,
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
  for (const auto &deduced : match.AssociatedTypeDeductions) {
    addAssocTypeDeductionString(withAssocTypes, deduced.first, deduced.second);
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

  for (auto proto : archetype->getConformsTo()) {
    ProtocolConformance *conformance = nullptr;
    bool conforms = tc.conformsToProtocol(replacement, proto, dc, false,
                                          &conformance);
    assert(conforms && "Conformance should already have been verified");
    (void)conforms;
    conformances.push_back(conformance);
  }

  return Substitution{
    resultArchetype,
    resultReplacement,
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
  req->getInterfaceType().visit([&](Type type) {
      if (auto dependentMember = type->getAs<DependentMemberType>()) {
        if (auto genericParam 
              = dependentMember->getBase()->getAs<GenericTypeParamType>()) {
          if (genericParam->getDepth() == 0 && 
              genericParam->getIndex() == 0) {
            if (auto assocType = dependentMember->getAssocType()) {
              if (assocType->getDeclContext() == Proto &&
                  knownAssocTypes.insert(assocType).second) {
                assocTypes.push_back(assocType);
              }
            }
          }
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
    TC.synthesizeWitnessAccessorsForStorage(storage);

  // Note that the witness conforms to the requirement.
  if (requirement != match.Witness)
    TC.Context.recordConformingDecl(match.Witness, requirement);

  // If we didn't deduce any associated types, we're done.
  if (match.AssociatedTypeDeductions.empty())
    return;

  // Record the associated type deductions.
  for (auto deduction : match.AssociatedTypeDeductions) {
    recordTypeWitness(deduction.first, deduction.second, true);
  }
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
}

void ConformanceChecker::recordTypeWitness(AssociatedTypeDecl *assocType,
                                           Type type,
                                           bool wasDeducedOrDefaulted) {
  // If we already recoded this type witness, there's nothing to do.
  if (Conformance->hasTypeWitness(assocType)) {
    assert(Conformance->getTypeWitness(assocType, nullptr).getReplacement()
             ->isEqual(type) && "Conflicting type witness deductions");
    return;
  }

  // Record the type witness.
  Conformance->setTypeWitness(
    assocType,
    getArchetypeSubstitution(TC, DC, assocType->getArchetype(), type));

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
    for (auto &elt: tuple->getFields()) {
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

ResolveWitnessResult
ConformanceChecker::resolveWitnessViaLookup(ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Use resolveTypeWitnessVia*");

  auto metaType = MetatypeType::get(Adoptee);

  // Determine whether we can derive this conformance.
  // FIXME: Hoist this computation out of here.
  bool canDerive = false;
  if (auto *nominal = Adoptee->getAnyNominal()) {
    if (nominal->derivesProtocolConformance(Proto))
      canDerive = true;
  }

  // Gather the witnesses.
  SmallVector<ValueDecl *, 4> witnesses;
  bool ignoringNames = false;
  if (requirement->getName().isOperator()) {
    // Operator lookup is always global.
    UnqualifiedLookup lookup(requirement->getName(),
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
    auto candidates = TC.lookupMember(metaType, requirement->getFullName(), DC);

    // If we didn't find anything with the appropriate name, look
    // again using only the base name.
    if (candidates.empty() && !canDerive) {
      candidates = TC.lookupMember(metaType, requirement->getName(), DC);
      ignoringNames = true;
    }

    for (auto candidate : candidates) {
      witnesses.push_back(candidate);
    }
  }

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

    auto match = matchWitness(TC, Conformance, DC, requirement, witness);
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

      // If the name didn't actually line up, complain.
      if (ignoringNames && 
          requirement->getFullName() != best.Witness->getFullName()) {
        {
          auto diag = TC.diagnose(best.Witness, 
                                  diag::witness_argument_name_mismatch,
                                  isa<ConstructorDecl>(best.Witness),
                                  best.Witness->getFullName(),
                                  Proto->getDeclaredType(),
                                  requirement->getFullName());
          TC.fixAbstractFunctionNames(diag,
                                      cast<AbstractFunctionDecl>(best.Witness),
                                      requirement->getFullName());
        }

        TC.diagnose(requirement, diag::protocol_requirement_here,
                    requirement->getFullName());
      }

      // If the match isn't accessible enough, complain.
      // FIXME: Handle "private(set)" requirements.
      Accessibility requiredAccess =
        std::min(Proto->getAccessibility(),
                 Adoptee->getAnyNominal()->getAccessibility());
      bool shouldDiagnose = false;
      bool shouldDiagnoseSetter = false;
      if (requiredAccess > Accessibility::Private) {
        shouldDiagnose = (best.Witness->getAccessibility() < requiredAccess);

        if (!shouldDiagnose && requirement->isSettable(DC)) {
          auto ASD = cast<AbstractStorageDecl>(best.Witness);
          const DeclContext *accessDC = nullptr;
          if (requiredAccess == Accessibility::Internal)
            accessDC = DC->getParentModule();
          shouldDiagnoseSetter = !ASD->isSetterAccessibleFrom(accessDC);
        }
      }
      if (shouldDiagnose || shouldDiagnoseSetter) {
        bool protoForcesAccess = (requiredAccess == Proto->getAccessibility());
        auto diagKind = protoForcesAccess ? diag::witness_not_accessible_proto
                                          : diag::witness_not_accessible_type;
        auto diag = TC.diagnose(best.Witness, diagKind,
                                getRequirementKind(requirement),
                                best.Witness->getFullName(),
                                shouldDiagnoseSetter,
                                requiredAccess,
                                Proto->getName());
        fixItAccessibility(diag, best.Witness, requiredAccess,
                           shouldDiagnoseSetter);
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
          bool inExtension = isa<ExtensionDecl>(best.Witness->getDeclContext());
          auto diag = TC.diagnose(best.Witness->getLoc(), 
                                  diag::witness_initializer_not_required,
                                  requirement->getFullName(), inExtension,
                                  Adoptee);
          if (!best.Witness->isImplicit() && !inExtension)
            diag.fixItInsert(best.Witness->getStartLoc(), "required ");
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
            TC.diagnose(best.Witness->getLoc(),
                        diag::witness_initializer_failability,
                        ctor->getFullName(),
                        witnessCtor->getFailability()
                          == OTK_ImplicitlyUnwrappedOptional)
              .highlight(witnessCtor->getFailabilityLoc());
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
          TC.diagnose(best.Witness->getLoc(), diag::witness_self_non_subtype,
                      Proto->getDeclaredType(), requirement->getFullName(), 
                      Adoptee);
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

            TC.diagnose(best.Witness->getLoc(),
                        diag::witness_requires_dynamic_self,
                        requirement->getFullName(), Adoptee,
                        Proto->getDeclaredType());
            break;
          }

          TC.diagnose(best.Witness->getLoc(), diag::witness_self_non_subtype,
                      Proto->getDeclaredType(), requirement->getFullName(), 
                      Adoptee);
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

  // Complain that this type does not conform to this protocol.
  if (!AlreadyComplained) {
    TC.diagnose(Loc, diag::type_does_not_conform,
                Adoptee, Proto->getDeclaredType());
    AlreadyComplained = true;
  }

  // If there was an invalid witness that might have worked, just
  // suppress the diagnostic entirely. This stops the diagnostic cascade.
  // FIXME: We could do something crazy, like try to fix up the witness.
  if (invalidWitness) {
    return ResolveWitnessResult::ExplicitFailed;
  }

  // Determine the type that the requirement is expected to have.
  Type reqType = getRequirementTypeForDisplay(TC, DC->getParentModule(),
                                              Conformance, requirement);

  // Point out the requirement that wasn't met.
  TC.diagnose(requirement,
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
      diagnoseMatch(TC, DC->getParentModule(), Conformance, requirement, match);
  }

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
  auto match = matchWitness(TC, Conformance, DC, requirement, derived);
  if (match.isViable()) {
    recordWitness(requirement, match);
    return ResolveWitnessResult::Success;
  }

  // Derivation failed.
  if (!AlreadyComplained) {
    TC.diagnose(Loc, diag::protocol_derivation_is_broken,
                Proto->getDeclaredType(), Adoptee);
    AlreadyComplained = true;
  }
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

  // Complain that this type does not conform to this protocol.
  if (!AlreadyComplained) {
    TC.diagnose(Loc, diag::type_does_not_conform,
                Adoptee, Proto->getDeclaredType());
    AlreadyComplained = true;
  }

  // Determine the type that the requirement is expected to have.
  Type reqType = getRequirementTypeForDisplay(TC, DC->getParentModule(),
                                              Conformance, requirement);

  // Point out the requirement that wasn't met.
  TC.diagnose(requirement, diag::no_witnesses,
              getRequirementKind(requirement),
              requirement->getName(),
              reqType);
  return ResolveWitnessResult::ExplicitFailed;
}

# pragma mark Type witness resolution

namespace {
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
}

/// Check whether the given type witness can be used for the given
/// associated type.
///
/// \returns an empty result on success, or a description of the error.
static CheckTypeWitnessResult checkTypeWitness(TypeChecker &tc, DeclContext *dc, 
                                               AssociatedTypeDecl *assocType, 
                                               Type type) {
  // FIXME: Check class requirement.

  // Check protocol conformances.
  for (auto reqProto : assocType->getProtocols()) {
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
  auto candidates = TC.lookupMemberType(metaType, assocType->getName(), DC);

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
    recordTypeWitness(assocType, viable.front().second, false);
    return ResolveWitnessResult::Success;
  }

  // If we had multiple viable types, diagnose the ambiguity.
  if (!viable.empty()) {
    if (!AlreadyComplained) {
      TC.diagnose(Loc, diag::type_does_not_conform, Adoptee, 
                  Proto->getDeclaredType());
      AlreadyComplained = true;
    }

    TC.diagnose(assocType, diag::ambiguous_witnesses_type,
                assocType->getName());

    for (auto candidate : viable)
      TC.diagnose(candidate.first, diag::protocol_witness_type);

    return ResolveWitnessResult::ExplicitFailed;
  }

  // None of the candidates were viable.
  if (!AlreadyComplained) {
    TC.diagnose(Loc, diag::type_does_not_conform, Adoptee, 
                Proto->getDeclaredType());
    AlreadyComplained = true;
  }

  TC.diagnose(assocType, diag::no_witnesses_type,
              assocType->getName());

  for (auto candidate : nonViable) {
    TC.diagnose(candidate.first,
                diag::protocol_witness_nonconform_type,
                candidate.first->getDeclaredType(),
                candidate.second->getDeclaredType());
  }

  return ResolveWitnessResult::ExplicitFailed;
}

/// Attempt to resolve a type witness via a default definition.
ResolveWitnessResult ConformanceChecker::resolveTypeWitnessViaDefault(
                       AssociatedTypeDecl *assocType) {
  // If we don't have a default definition, we're done.
  if (assocType->getDefaultDefinitionLoc().isNull())
    return ResolveWitnessResult::Missing;

  // Create a set of type substitutions for all known associated type.
  // FIXME: Base this on dependent types rather than archetypes?
  TypeSubstitutionMap substitutions;
  substitutions[Proto->getSelf()->getArchetype()] = Adoptee;
  for (auto member : Proto->getMembers()) {
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
      if (Conformance->hasTypeWitness(assocType))
        substitutions[assocType->getArchetype()]
          = Conformance->getTypeWitness(assocType, nullptr).getReplacement();
    }
  }
  auto defaultType = TC.substType(DC->getParentModule(),
                                  assocType->getDefaultDefinitionLoc().getType(),
                                  substitutions,
                                  /*IgnoreMissing=*/true);
  if (!defaultType)
    return ResolveWitnessResult::Missing;

  if (auto checkResult = checkTypeWitness(TC, DC, assocType, defaultType)) {
    if (!AlreadyComplained) {
      TC.diagnose(Loc, diag::type_does_not_conform, Adoptee, 
                  Proto->getDeclaredType());
      AlreadyComplained = true;
    }

    TC.diagnose(assocType, diag::default_assocated_type_req_fail,
                defaultType, checkResult.getProtocol()->getDeclaredType());

    return ResolveWitnessResult::ExplicitFailed;
  } 
  
  // Fill in the type witness and declare success.
  recordTypeWitness(assocType, defaultType, true);
  return ResolveWitnessResult::Success;
}

/// Attempt to resolve a type witness via derivation.
ResolveWitnessResult ConformanceChecker::resolveTypeWitnessViaDerivation(
                       AssociatedTypeDecl *assocType) {
  // See whether we can derive members of this conformance.
  NominalTypeDecl *derivingTypeDecl = nullptr;
  if (auto *nominal = Adoptee->getAnyNominal()) {
    if (nominal->derivesProtocolConformance(Proto))
      derivingTypeDecl = nominal;
  }

  // If we can't derive, then don't.
  if (!derivingTypeDecl)
    return ResolveWitnessResult::Missing;

  // Perform the derivation.
  auto derived = TC.deriveProtocolRequirement(derivingTypeDecl, assocType);
  if (!derived) {
    // The problem was already diagnosed.
    return ResolveWitnessResult::ExplicitFailed;
  }

  auto derivedType = cast<TypeDecl>(derived)->getDeclaredType();
  if (checkTypeWitness(TC, DC, assocType, derivedType)) {
    // FIXME: give more detail here?
    TC.diagnose(Loc, diag::protocol_derivation_is_broken,
                Proto->getDeclaredType(), derivedType);
    return ResolveWitnessResult::ExplicitFailed;
  } 
  
  // Fill in the type witness and declare success.
  recordTypeWitness(assocType, derivedType, true);
  return ResolveWitnessResult::Success;
}

void 
ConformanceChecker::resolveSingleTypeWitness(AssociatedTypeDecl *assocType) {
  assert(!Conformance->hasTypeWitness(assocType) && "Already resolved");

  // Note that we're resolving this witness.
  assert(ResolvingTypeWitnesses.count(assocType) == 0 && "Currently resolving");
  ResolvingTypeWitnesses.insert(assocType);
  SCOPE_GUARD(ResolvingTypeWitnesses.erase(assocType));

  // Try to resolve this type witness via name lookup, which is the
  // most direct mechanism.
  switch (resolveTypeWitnessViaLookup(assocType)) {
  case ResolveWitnessResult::Success:
    return;

  case ResolveWitnessResult::ExplicitFailed:
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;

  case ResolveWitnessResult::Missing:
    // Keep trying.
    break;
  }

  // Look for requirements that refer to this associated type;
  // determining their witnesses can deduce this associated type.
  for (auto member : Proto->getMembers()) {
    // Ignore associated types.
    if (isa<AssociatedTypeDecl>(member))
      continue;

    auto requirement = dyn_cast<ValueDecl>(member);
    if (!requirement)
      continue;

    if (!requirement->hasType())
      TC.validateDecl(requirement, true);

    // If we have or are trying to resolve this witness, don't try again.
    if (ResolvingWitnesses.count(requirement) > 0 ||
        Conformance->hasWitness(requirement))
      continue;

    // Determine whether this requirement refers to the associated
    // type we're resolving.
    bool hasAssocType = false;
    for (auto referencedAssocType : getReferencedAssociatedTypes(requirement)) {
      if (referencedAssocType == assocType) {
        hasAssocType = true;
        break;
      }
    }

    // If the requirement doesn't refer to the associated type we're
    // resolving, ignore it.
    if (!hasAssocType)
      continue;

    // Try to resolve that witness.
    resolveSingleWitness(requirement);

    // If resolving the witness deduced our associated type, we're done.
    if (Conformance->hasTypeWitness(assocType))
      return;
  }

  // Otherwise, try to resolve via a default.
  switch (resolveTypeWitnessViaDefault(assocType)) {
  case ResolveWitnessResult::Success:
    return;

  case ResolveWitnessResult::ExplicitFailed:
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;

  case ResolveWitnessResult::Missing:
    // Keep trying.
    break;
  }

  // Otherwise, try to derive an answer.
  switch (resolveTypeWitnessViaDerivation(assocType)) {
  case ResolveWitnessResult::Success:
    return;

  case ResolveWitnessResult::ExplicitFailed:
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;

  case ResolveWitnessResult::Missing:
    // Diagnose failure below.
    break;
  }

  // FIXME: Note this failure so we don't try again?

  TC.diagnose(Loc, diag::type_does_not_conform,
              Adoptee, Proto->getDeclaredType());

  TC.diagnose(assocType, diag::no_witnesses_type,
              assocType->getName());
  Conformance->setState(ProtocolConformanceState::Invalid);
}

void ConformanceChecker::resolveSingleWitness(ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Not a value witness");
  assert(!Conformance->hasWitness(requirement) && "Already resolved");

  // Note that we're resolving this witness.
  assert(ResolvingWitnesses.count(requirement) == 0 && "Currently resolving");
  ResolvingWitnesses.insert(requirement);
  SCOPE_GUARD(ResolvingWitnesses.erase(requirement));

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

  // Try to resolve each of the associated types referenced within the
  // requirement's type to type witnesses via name lookup. Such
  // bindings can inform the choice of witness.

  // Note that we copy the associated types vector because attempting to
  // resolve type witnesses could invalidate the ArrayRef we get.
  SmallVector<AssociatedTypeDecl *, 2> assocTypes;
  {
    auto refAssocTypes = getReferencedAssociatedTypes(requirement);
    assocTypes.append(refAssocTypes.begin(), refAssocTypes.end());
  }

  bool failed = false;
  for (auto assocType : assocTypes) {
    // If we don't already have a type witness and aren't in the
    // process of trying to resolve it already...
    if (!Conformance->hasTypeWitness(assocType) &&
        ResolvingTypeWitnesses.count(assocType) == 0) {
      switch (resolveTypeWitnessViaLookup(assocType)) {
      case ResolveWitnessResult::Success:
        break;

      case ResolveWitnessResult::ExplicitFailed:
        failed = true;
        break;

      case ResolveWitnessResult::Missing:
        // Okay: this associated type can be deduced.
        break;
      }
    }

    if (failed)
      break;

    // If the type witness is an error, just fail quietly.
    if (Conformance->hasTypeWitness(assocType) &&
        Conformance->getTypeWitness(assocType, nullptr).getReplacement()
          ->is<ErrorType>()) {
      failed = true;
      break;
    }
  }

  // If one of the associated types had an outright error, don't bother
  // trying to check this witness: we won't be able to meaningfully
  // compare the types anyway.
  if (failed) {
    Conformance->setState(ProtocolConformanceState::Invalid);    
    return;
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

  // FIXME: Caller checks that this the type conforms to all of the
  // inherited protocols.
  
  // Resolve any associated type members via lookup.
  for (auto member : Proto->getMembers()) {
    auto assocType = dyn_cast<AssociatedTypeDecl>(member);
    if (!assocType)
      continue;

    // If we've already determined this type witness, skip it.
    if (Conformance->hasTypeWitness(assocType))
      continue;

    // Try to resolve the type witness via name lookup.
    (void)resolveTypeWitnessViaLookup(assocType);
  }

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

  if (AlreadyComplained || invalid) {
    Conformance->setState(ProtocolConformanceState::Invalid);
    return;
  }

  // For any requirements that do not yet have witnesses, try default
  // definitions or compiler-supported derivation.
  for (auto member : Proto->getMembers()) {
    // For an associated type.
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
      // If we already have a type witness, there's nothing to do.
      if (Conformance->hasTypeWitness(assocType))
        continue;

      // Otherwise, try to resolve via a default.
      switch (resolveTypeWitnessViaDefault(assocType)) {
      case ResolveWitnessResult::Success:
      case ResolveWitnessResult::ExplicitFailed:
        continue;

      case ResolveWitnessResult::Missing:
        // Keep trying.
        break;
      }

      // Otherwise, try to derive an answer.
      switch (resolveTypeWitnessViaDerivation(assocType)) {
      case ResolveWitnessResult::Success:
      case ResolveWitnessResult::ExplicitFailed:
        continue;

      case ResolveWitnessResult::Missing:
        // Diagnose failure below.
        break;
      }

      if (!AlreadyComplained) {
        TC.diagnose(Loc, diag::type_does_not_conform,
                    Adoptee, Proto->getDeclaredType());
        AlreadyComplained = true;
      }

      TC.diagnose(assocType, diag::no_witnesses_type,
                  assocType->getName());
      break;
    }

    // For a non-type witness.
    if (auto value = dyn_cast<ValueDecl>(member)) {
      (void)value;
      // If this is an accessor for something, ignore it.
      if (auto *FD = dyn_cast<FuncDecl>(member))
        if (FD->isAccessor())
          continue;

      assert((AlreadyComplained || Conformance->hasWitness(value)) &&
             "Failed to handle witness");
    }
  }

  if (AlreadyComplained) {
    Conformance->setState(ProtocolConformanceState::Invalid);
  } else {
    Conformance->setState(ProtocolConformanceState::Complete);
  }
}

/// \brief Determine whether the type \c T conforms to the protocol \c Proto,
/// recording the complete witness table if it does.
static ProtocolConformance *
checkConformsToProtocol(TypeChecker &TC, Type T, ProtocolDecl *Proto,
                        DeclContext *DC,
                        Decl *ExplicitConformance,
                        SourceLoc ComplainLoc) {
  DeclContext *conformingDC;
  if (auto nominal = dyn_cast<NominalTypeDecl>(ExplicitConformance))
    conformingDC = nominal;
  else
    conformingDC = cast<ExtensionDecl>(ExplicitConformance);

  // Find or create the conformance for this type.
  NormalProtocolConformance *conformance;
  auto canT = T->getCanonicalType();
  auto knownConformance = TC.Context.getConformsTo(canT, Proto);
  if (knownConformance && knownConformance->getPointer()) {
    // Get the normal protocol conformance.
    conformance = cast<NormalProtocolConformance>(
                    knownConformance->getPointer());
  } else {
    // Create and record this conformance.
    conformance = TC.Context.getConformance(
                    T, Proto, ComplainLoc,
                    conformingDC,
                    ProtocolConformanceState::Incomplete);

    TC.Context.setConformsTo(canT, Proto, ConformanceEntry(conformance, true));
  }

  // If the protocol requires a class, non-classes are a non-starter.
  if (Proto->requiresClass() && !canT->getClassOrBoundGenericClass()) {
    TC.diagnose(ComplainLoc, diag::non_class_cannot_conform_to_class_protocol,
                T, Proto->getDeclaredType());
    conformance->setState(ProtocolConformanceState::Invalid);
    return conformance;
  }

  // Foreign classes cannot conform to objc protocols.
  if (Proto->isObjC())
    if (auto clas = canT->getClassOrBoundGenericClass())
      if (clas->isForeign()) {
        TC.diagnose(ComplainLoc, diag::foreign_class_cannot_conform_to_objc_protocol,
                    T, Proto->getDeclaredType());
        conformance->setState(ProtocolConformanceState::Invalid);
        return conformance;
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
  for (auto InheritedProto : Proto->getProtocols()) {
    ProtocolConformance *InheritedConformance = nullptr;
    if (TC.conformsToProtocol(T, InheritedProto, DC, false,
                              &InheritedConformance, ComplainLoc,
                              ExplicitConformance)) {
      if (!conformance->hasInheritedConformance(InheritedProto))
        conformance->setInheritedConformance(InheritedProto,
                                             InheritedConformance);
    } else {
      if (auto knownConformance =
              TC.Context.getConformsTo(canT, InheritedProto)) {
        // Check to see if the conformance is in an incomplete state.  If it is,
        // the inherited protocol has an indirectly recursive requirement.
        if (ComplainLoc.isValid() &&
            knownConformance->getInt() &&
            (knownConformance->getPointer()->getState() ==
                ProtocolConformanceState::Incomplete)) {
              if (!conformance->hasInheritedConformance(InheritedProto)) {
                conformance->setInheritedConformance(InheritedProto,
                                                     knownConformance->
                                                        getPointer());
              }
              continue;
        }
      }
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

  // The conformance checker we're using.
  ConformanceChecker checker(TC, conformance);
  checker.checkConformance();
  return conformance;
}

/// \brief Check whether an existential value of the given protocol conforms
/// to itself.
///
/// \param tc The type checker.
/// \param type The existential type we're checking, used for diagnostics.
/// \param proto The protocol to test.
/// \param If we're allowed to complain, the location to use.

/// \returns true if the existential type conforms to itself, false otherwise.
static bool
existentialConformsToItself(TypeChecker &tc,
                            Type type,
                            ProtocolDecl *proto,
                            SourceLoc complainLoc,
                            llvm::SmallPtrSet<ProtocolDecl *, 4> &checking) {
  // If we already know whether this protocol's existential conforms to itself
  // use the cached value... unless it's negative and we're supposed to
  // complain, in which case we fall through.
  if (auto known = proto->existentialConformsToSelf()) {
    if (*known || complainLoc.isInvalid())
      return *known;
  }

  // Assume for now that it does. This prevents circularity issues.
  proto->setExistentialConformsToSelf(true);

  // Check that all inherited protocols conform to themselves.
  for (auto inheritedProto : proto->getProtocols()) {
    // If we're already checking this protocol, assume it's fine.
    if (!checking.insert(inheritedProto).second)
      continue;

    // Check whether the inherited protocol conforms to itself.
    if (!existentialConformsToItself(tc, type, inheritedProto, complainLoc,
                                     checking)) {
      // Recursive call already diagnosed this problem, but tack on a note
      // to establish the relationship.
      // FIXME: Poor location information.
      if (complainLoc.isValid() && *proto->existentialConformsToSelf()) {
        tc.diagnose(proto,
                    diag::inherited_protocol_does_not_conform, type,
                    inheritedProto->getType());
      }

      proto->setExistentialConformsToSelf(false);
      return false;
    }
  }

  // Check whether this protocol conforms to itself.
  auto selfType = proto->getSelf()->getArchetype();
  for (auto member : proto->getMembers()) {
    
    // If the protocol in question is already being type-checked, we've entered
    // this conformance check recursively. Should that be the case, don't
    // attempt to re-validate any value declarations. The declaration will be
    // properly validated later on, and we want to avoid an infinite loop.
    if (!proto->isBeingTypeChecked()) {
      if (auto vd = dyn_cast<ValueDecl>(member))
        tc.validateDecl(vd, true);
    }
    
    if (member->isInvalid())
      continue;

    // Check for associated types.
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
      // A protocol cannot conform to itself if it has an associated type.
      if (complainLoc.isValid() && *proto->existentialConformsToSelf()) {
        tc.diagnose(complainLoc, diag::type_does_not_conform, type,
                    proto->getDeclaredType());
        tc.diagnose(assocType, diag::protocol_existential_assoc_type,
                    assocType->getName());
      }
      proto->setExistentialConformsToSelf(false);
      return false;
    }

    // For value members, look at their type signatures.
    auto valueMember = dyn_cast<ValueDecl>(member);
    if (!valueMember || !valueMember->hasType())
      continue;

    // Extract the type of the member, ignoring the 'self' parameter and return
    // type of functions.
    auto memberTy = valueMember->getType();
    if (memberTy->is<ErrorType>())
      continue;
    if (isa<AbstractFunctionDecl>(valueMember)) {
      // Drop the 'Self' parameter.
      memberTy = memberTy->castTo<AnyFunctionType>()->getResult();
      // Drop the return type. Methods are allowed to return Self.
      memberTy = memberTy->castTo<AnyFunctionType>()->getInput();
    }

    // "Transform" the type to walk the whole type. If we find 'Self', return
    // null. Otherwise, make this the identity transform and throw away the
    // result.
    if (memberTy.transform([&](Type type) -> Type {
          // If we found our archetype, return null.
          if (auto archetype = type->getAs<ArchetypeType>()) {
            return archetype == selfType? nullptr : type;
          }

          return type;
        })) {
      // We didn't find 'Self'. We're okay.
      continue;
    }

    // A protocol cannot conform to itself if any of its value members
    // refers to 'Self'.
    if (complainLoc.isValid() && *proto->existentialConformsToSelf()) {
      tc.diagnose(complainLoc, diag::type_does_not_conform, type,
                  proto->getDeclaredType());
      tc.diagnose(valueMember, diag::protocol_existential_refers_to_this,
                  valueMember->getName());
    }

    proto->setExistentialConformsToSelf(false);
    return false;
  }

  return true;
}

/// Check whether the given archetype conforms to the protocol.
static bool archetypeConformsToProtocol(TypeChecker &tc, Type type,
                                        ArchetypeType *archetype,
                                        ProtocolDecl *protocol,
                                        SourceLoc complainLoc) {
  // An archetype that must be a class trivially conforms to AnyObject.
  if (archetype->requiresClass() &&
      protocol == tc.Context.getProtocol(KnownProtocolKind::AnyObject))
    return true;

  for (auto ap : archetype->getConformsTo()) {
    if (ap == protocol || ap->inheritsFrom(protocol))
      return true;
  }

  // If we need to complain, do so.
  if (complainLoc.isValid()) {
    // FIXME: Fix-It to add a requirement on the corresponding type
    // parameter?
    tc.diagnose(complainLoc, diag::type_does_not_conform, type,
                protocol->getDeclaredType());
  }

  return false;
}

/// Check whether the given existential type conforms to the protocol.
static bool existentialConformsToProtocol(TypeChecker &tc, Type type,
                                          ProtocolDecl *protocol,
                                          SourceLoc complainLoc) {
  SmallVector<ProtocolDecl *, 4> protocols;
  bool isExistential = type->isExistentialType(protocols);
  assert(isExistential && "Not existential?");
  (void)isExistential;

  // An existential that must be a class trivially conforms to AnyObject.
  if (type->isClassExistentialType() &&
      protocol == tc.Context.getProtocol(KnownProtocolKind::AnyObject))
    return true;

  for (auto ap : protocols) {
    // If this isn't the protocol we're looking for, continue looking.
    if (ap != protocol && !ap->inheritsFrom(protocol))
      continue;

    // Check whether this protocol conforms to itself.
    llvm::SmallPtrSet<ProtocolDecl *, 4> checking;
    checking.insert(protocol);
    return existentialConformsToItself(tc, type, ap, complainLoc, checking);
  }

  // We didn't find the protocol we were looking for.
  // If we need to complain, do so.
  if (complainLoc.isValid()) {
    tc.diagnose(complainLoc, diag::type_does_not_conform, type,
             protocol->getDeclaredType());
  }
  return false;
}

bool TypeChecker::conformsToProtocol(Type T, ProtocolDecl *Proto,
                                     DeclContext *DC, bool InExpression,
                                     ProtocolConformance **Conformance,
                                     SourceLoc ComplainLoc, 
                                     Decl *ExplicitConformance) {
  if (Conformance)
    *Conformance = nullptr;
  if (!Proto->hasType())
    validateDecl(Proto);

  // If we have an archetype, check whether this archetype's requirements
  // include this protocol (or something that inherits from it).
  if (auto Archetype = T->getAs<ArchetypeType>())
    return archetypeConformsToProtocol(*this, T, Archetype, Proto, ComplainLoc);

  // If we have an existential type, check whether this type includes this
  // protocol we're looking for (or something that inherits from it).
  if (T->isExistentialType())
    return existentialConformsToProtocol(*this, T, Proto, ComplainLoc);

  // Only nominal types conform to protocols.
  auto canT = T->getCanonicalType();
  auto nominal = canT->getAnyNominal();
  if (!nominal) {
    // If we need to complain, do so.
    if (ComplainLoc.isValid()) {
      diagnose(ComplainLoc, diag::type_does_not_conform, T,
               Proto->getDeclaredType());
    }

    return false;
  }

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

    tracker->addUsedNominal(nominal,
                            DC->isCascadingContextForLookup(InExpression));
  };

  // Check whether we have already cached an answer to this query.
  if (auto Known = Context.getConformsTo(canT, Proto)) {
    // If we conform, set the conformance and return true.
    if (Known->getInt()) {
      if (Conformance)
        *Conformance = Known->getPointer();

      recordDependency(Known->getPointer());
      return !(Known->getPointer()->isInvalid());
    }

    // If we're just checking for conformance, we already know the answer.
    if (!ExplicitConformance) {
      // If we need to complain, do so.
      if (ComplainLoc.isValid()) {
        diagnose(ComplainLoc, diag::type_does_not_conform, T,
                 Proto->getDeclaredType());
      } else {
        recordDependency();
      }

      return false;
    }

    // For explicit conformance, force the check again.
  }

  // If we're checking for conformance (rather than stating it),
  // look for the explicit declaration of conformance in the list of protocols.
  if (!ExplicitConformance) {
    Module *M = topLevelContext->getParentModule();
    auto lookupResult = M->lookupConformance(T, Proto, this);
    switch (lookupResult.getInt()) {
    case ConformanceKind::Conforms:
      Context.setConformsTo(canT, Proto, 
                            ConformanceEntry(lookupResult.getPointer(), true));

      if (Conformance)
        *Conformance = lookupResult.getPointer();
      recordDependency(lookupResult.getPointer());
      return true;

    case ConformanceKind::DoesNotConform:
      // FIXME: Could try implicit conformance.
      if (ComplainLoc.isValid()) {
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

  // Check this protocol's conformance.
  auto result = checkConformsToProtocol(*this, T, Proto, DC,
                                        ExplicitConformance, ComplainLoc);
  assert(result && "Didn't build any protocol conformance");
  assert(!result->isIncomplete() && "Retrieved incomplete conformance");

  if (Conformance)
    *Conformance = result;
  return true;
}

ProtocolConformance *TypeChecker::resolveConformance(NominalTypeDecl *type,
                                                     ProtocolDecl *protocol,
                                                     ExtensionDecl *ext) {
  auto explicitConformance = ext ? (Decl *)ext : (Decl *)type;
  auto conformanceContext = ext ? (DeclContext *)ext : (DeclContext *)type;
  ProtocolConformance *conformance = nullptr;
  bool conforms = conformsToProtocol(type->getDeclaredTypeInContext(), protocol,
                                     conformanceContext, false, &conformance,
                                     explicitConformance->getLoc(),
                                     explicitConformance);
  return conforms ? conformance : nullptr;
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

void TypeChecker::resolveExistentialConformsToItself(ProtocolDecl *proto) {
  llvm::SmallPtrSet<ProtocolDecl *, 4> checking;
  existentialConformsToItself(*this, proto->getDeclaredType(), proto,
                              SourceLoc(), checking);
}

ValueDecl *TypeChecker::deriveProtocolRequirement(NominalTypeDecl *TypeDecl,
                                                  ValueDecl *Requirement) {
  auto *protocol = cast<ProtocolDecl>(Requirement->getDeclContext());

  if (protocol == Context.getProtocol(
        KnownProtocolKind::RawRepresentable)
     ) {
    return DerivedConformance::deriveRawRepresentable(*this,
                                                      TypeDecl, Requirement);
  }
  if (protocol == Context.getProtocol(KnownProtocolKind::Equatable)) {
    return DerivedConformance::deriveEquatable(*this, TypeDecl, Requirement);
  }
  if (protocol == Context.getProtocol(KnownProtocolKind::Hashable)) {
    return DerivedConformance::deriveHashable(*this, TypeDecl, Requirement);
  }
  
  return nullptr;
}
