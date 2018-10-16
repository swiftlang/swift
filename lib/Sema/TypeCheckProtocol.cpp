//===--- TypeCheckProtocol.cpp - Protocol Checking ------------------------===//
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
// This file implements semantic analysis for protocols, in particular, checking
// whether a given type conforms to a given protocol.
//===----------------------------------------------------------------------===//

#include "TypeCheckProtocol.h"
#include "ConstraintSystem.h"
#include "DerivedConformances.h"
#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "TypeCheckAvailability.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Basic/Statistic.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/TypeMatcher.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Defer.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SaveAndRestore.h"

#define DEBUG_TYPE "Protocol conformance checking"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {
  /// Whether any of the given optional adjustments is an error (vs. a
  /// warning).
  bool hasAnyError(ArrayRef<OptionalAdjustment> adjustments) {
    for (const auto &adjustment : adjustments)
      if (adjustment.isError())
        return true;

    return false;
  }
}

/// \brief Describes the suitability of the chosen witness for
/// the requirement.
struct swift::RequirementCheck {
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

swift::Witness RequirementMatch::getWitness(ASTContext &ctx) const {
  auto syntheticEnv = ReqEnv->getSyntheticEnvironment();
  return swift::Witness(this->Witness, WitnessSubstitutions,
                        syntheticEnv, ReqEnv->getRequirementToSyntheticMap());
}

AssociatedTypeDecl *
swift::getReferencedAssocTypeOfProtocol(Type type, ProtocolDecl *proto) {
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

static std::tuple<Type, Type, OptionalAdjustmentKind>
getTypesToCompare(ValueDecl *reqt, Type reqtType, bool reqtTypeIsIUO,
                  Type witnessType, bool witnessTypeIsIUO,
                  VarianceKind variance) {
  // If the witness type is noescape but the requirement type is not,
  // adjust the witness type to be escaping. This permits a limited form of
  // covariance.
  bool reqNoescapeToEscaping = false;
  (void)adjustInferredAssociatedType(reqtType, reqNoescapeToEscaping);
  bool witnessNoescapeToEscaping = false;
  Type adjustedWitnessType =
    adjustInferredAssociatedType(witnessType, witnessNoescapeToEscaping);
  if (witnessNoescapeToEscaping && !reqNoescapeToEscaping)
    witnessType = adjustedWitnessType;

  // For @objc protocols, deal with differences in the optionality.
  // FIXME: It probably makes sense to extend this to non-@objc
  // protocols as well, but this requires more testing.
  OptionalAdjustmentKind optAdjustment = OptionalAdjustmentKind::None;

  if (!reqt->isObjC())
    return std::make_tuple(reqtType, witnessType, optAdjustment);

  bool reqtIsOptional = false;
  if (Type reqtValueType = reqtType->getOptionalObjectType()) {
    reqtIsOptional = true;
    reqtType = reqtValueType;
  }
  bool witnessIsOptional = false;
  if (Type witnessValueType = witnessType->getOptionalObjectType()) {
    witnessIsOptional = true;
    witnessType = witnessValueType;
  }

  // When the requirement is an IUO, all is permitted, because we
  // assume that the user knows more about the signature than we
  // have information in the protocol.
  if (reqtTypeIsIUO)
    return std::make_tuple(reqtType, witnessType, optAdjustment);

  if (reqtIsOptional) {
    if (witnessIsOptional) {
      if (witnessTypeIsIUO)
        optAdjustment = OptionalAdjustmentKind::IUOToOptional;
    } else {
      switch (variance) {
      case VarianceKind::None:
      case VarianceKind::Contravariant:
        optAdjustment = OptionalAdjustmentKind::ConsumesUnhandledNil;
        break;

      case VarianceKind::Covariant:
        optAdjustment = OptionalAdjustmentKind::WillNeverProduceNil;
        break;
      }
    }
  } else if (witnessIsOptional) {
    if (witnessTypeIsIUO) {
      optAdjustment = OptionalAdjustmentKind::RemoveIUO;
    } else {
      switch (variance) {
      case VarianceKind::None:
      case VarianceKind::Covariant:
        optAdjustment = OptionalAdjustmentKind::ProducesUnhandledNil;
        break;

      case VarianceKind::Contravariant:
        optAdjustment = OptionalAdjustmentKind::WillNeverConsumeNil;
        break;
      }
    }
  }

  return std::make_tuple(reqtType, witnessType, optAdjustment);
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
    auto reqtAsAccessor = cast<AccessorDecl>(requirement);
    auto storage = cast<AbstractStorageDecl>(witnessDecl);

    auto isReadMutating = [&] {
      switch (storage->getReadImpl()) {
      case ReadImplKind::Stored:
        return false;
      case ReadImplKind::Address:
        return storage->getAddressor()->isMutating();
      case ReadImplKind::Read:
        return storage->getReadCoroutine()->isMutating();
      case ReadImplKind::Inherited:
      case ReadImplKind::Get:
        llvm_unreachable("should have a getter");
      }
      llvm_unreachable("unhandled kind");
    };

    auto isStoredSetterMutating = [&] {
      // A stored property on a value type will have a mutating setter
      // and a non-mutating getter.
      return reqtAsAccessor->isInstanceMember();
    };

    auto isWriteMutating = [&] {
      switch (storage->getWriteImpl()) {
      case WriteImplKind::Stored:
        return isStoredSetterMutating();
      case WriteImplKind::MutableAddress:
        return storage->getMutableAddressor()->isMutating();
      case WriteImplKind::Modify:
        return storage->getModifyCoroutine()->isMutating();
      case WriteImplKind::Immutable:
        llvm_unreachable("asking for setter for immutable storage");
      case WriteImplKind::Set:
      case WriteImplKind::StoredWithObservers:
      case WriteImplKind::InheritedWithObservers:
        llvm_unreachable("should have a setter");
      }
      llvm_unreachable("unhandled kind");
    };

    auto isReadWriteMutating = [&] {
      switch (storage->getReadWriteImpl()) {
      case ReadWriteImplKind::Stored:
        return isStoredSetterMutating();
      case ReadWriteImplKind::MutableAddress:
        return storage->getMutableAddressor()->isMutating();
      case ReadWriteImplKind::Modify:
        return storage->getModifyCoroutine()->isMutating();
      case ReadWriteImplKind::MaterializeToTemporary:
        return isReadMutating() || isWriteMutating();
      case ReadWriteImplKind::Immutable:
        llvm_unreachable("asking for setter for immutable storage");
      }
      llvm_unreachable("unhandled kind");
    };

    switch (reqtAsAccessor->getAccessorKind()) {
    case AccessorKind::Get:
    case AccessorKind::Read:
      witnessMutating = isReadMutating();
      break;

    case AccessorKind::Set:
      witnessMutating = isWriteMutating();
      break;

    case AccessorKind::Modify:
      witnessMutating = isReadWriteMutating();
      break;

#define OPAQUE_ACCESSOR(ID, KEYWORD)
#define ACCESSOR(ID) \
    case AccessorKind::ID:
#include "swift/AST/AccessorKinds.def"
      llvm_unreachable("unexpected accessor requirement");
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

static ParameterList *getParameterList(ValueDecl *value) {
  if (auto func = dyn_cast<AbstractFunctionDecl>(value))
    return func->getParameters();

  auto subscript = cast<SubscriptDecl>(value);
  return subscript->getIndices();
}

// Find a standin declaration to place the diagnostic at for the
// given accessor kind.
static ValueDecl *getStandinForAccessor(AbstractStorageDecl *witnessStorage,
                                        AccessorKind requirementKind) {
  auto getExplicitAccessor = [&](AccessorKind kind) -> AccessorDecl* {
    if (auto accessor = witnessStorage->getAccessor(kind)) {
      if (!accessor->isImplicit())
        return accessor;
    }
    return nullptr;
  };

  // If the storage actually explicitly provides that accessor, great.
  if (auto accessor = getExplicitAccessor(requirementKind))
    return accessor;

  // If it didn't, check to see if it provides something else that corresponds
  // to the requirement.
  switch (requirementKind) {
  case AccessorKind::Get:
    if (auto read = getExplicitAccessor(AccessorKind::Read))
      return read;
    if (auto addressor = getExplicitAccessor(AccessorKind::Address))
      return addressor;
    break;

  case AccessorKind::Read:
    if (auto getter = getExplicitAccessor(AccessorKind::Get))
      return getter;
    if (auto addressor = getExplicitAccessor(AccessorKind::Address))
      return addressor;
    break;

  case AccessorKind::Modify:
    if (auto setter = getExplicitAccessor(AccessorKind::Set))
      return setter;
    if (auto addressor = getExplicitAccessor(AccessorKind::MutableAddress))
      return addressor;
    break;

  case AccessorKind::Set:
    if (auto modify = getExplicitAccessor(AccessorKind::Modify))
      return modify;
    if (auto addressor = getExplicitAccessor(AccessorKind::MutableAddress))
      return addressor;
    break;

#define OPAQUE_ACCESSOR(ID, KEYWORD)
#define ACCESSOR(ID) \
  case AccessorKind::ID:
#include "swift/AST/AccessorKinds.def"
    llvm_unreachable("unexpected accessor requirement");
  }

  // Otherwise, just diagnose starting at the storage declaration itself.
  return witnessStorage;
}

RequirementMatch
swift::matchWitness(
             TypeChecker &tc,
             DeclContext *dc, ValueDecl *req, ValueDecl *witness,
             llvm::function_ref<
                     std::tuple<Optional<RequirementMatch>, Type, Type>(void)> 
               setup,
             llvm::function_ref<Optional<RequirementMatch>(Type, Type)>
               matchTypes,
             llvm::function_ref<
                     RequirementMatch(bool, ArrayRef<OptionalAdjustment>)
                   > finalize) {
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

    // Validate that the 'mutating' bit lines up for getters and setters.
    if (checkMutating(reqASD->getGetter(), witnessASD->getGetter(),
                      witnessASD))
      return RequirementMatch(getStandinForAccessor(witnessASD, AccessorKind::Get),
                              MatchKind::MutatingConflict);
    
    if (req->isSettable(req->getDeclContext()) &&
        checkMutating(reqASD->getSetter(), witnessASD->getSetter(), witnessASD))
      return RequirementMatch(getStandinForAccessor(witnessASD, AccessorKind::Set),
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
      return std::move(result.getValue());
    }
  }

  SmallVector<OptionalAdjustment, 2> optionalAdjustments;
  bool anyRenaming = req->getFullName() != witness->getFullName();
  if (decomposeFunctionType) {
    // Decompose function types into parameters and result type.
    auto reqFnType = reqType->castTo<AnyFunctionType>();
    auto reqResultType = reqFnType->getResult()->getRValueType();
    auto witnessFnType = witnessType->castTo<AnyFunctionType>();
    auto witnessResultType = witnessFnType->getResult()->getRValueType();

    // Result types must match.
    // FIXME: Could allow (trivial?) subtyping here.
    if (!ignoreReturnType) {
      if (reqResultType->hasDynamicSelfType()) {
        auto classDecl = witness->getDeclContext()->getSelfClassDecl();
        if (!classDecl || classDecl->isFinal() ||
            witnessResultType->hasDynamicSelfType())
          reqResultType = reqResultType->eraseDynamicSelfType();
        witnessResultType = witnessResultType->eraseDynamicSelfType();
      }

      auto reqTypeIsIUO =
          req->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
      auto witnessTypeIsIUO =
          witness->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
      auto types =
          getTypesToCompare(req, reqResultType, reqTypeIsIUO, witnessResultType,
                            witnessTypeIsIUO, VarianceKind::Covariant);

      // Record optional adjustment, if any.
      if (std::get<2>(types) != OptionalAdjustmentKind::None) {
        optionalAdjustments.push_back(
          OptionalAdjustment(std::get<2>(types)));
      }

      if (!req->isObjC() && reqTypeIsIUO != witnessTypeIsIUO)
        return RequirementMatch(witness, MatchKind::TypeConflict, witnessType);

      if (auto result = matchTypes(std::get<0>(types), std::get<1>(types))) {
        return std::move(result.getValue());
      }
    }

    // Parameter types and kinds must match. Start by decomposing the input
    // types into sets of tuple elements.
    // Decompose the input types into parameters.
    auto reqParams = reqFnType->getParams();
    auto witnessParams = witnessFnType->getParams();

    // If the number of parameters doesn't match, we're done.
    if (reqParams.size() != witnessParams.size())
      return RequirementMatch(witness, MatchKind::TypeConflict, 
                              witnessType);

    ParameterList *witnessParamList = getParameterList(witness);
    assert(witnessParamList->size() == witnessParams.size());

    ParameterList *reqParamList = getParameterList(req);
    assert(reqParamList->size() == reqParams.size());

    // Match each of the parameters.
    for (unsigned i = 0, n = reqParams.size(); i != n; ++i) {
      // Variadic bits must match.
      // FIXME: Specialize the match failure kind
      if (reqParams[i].isVariadic() != witnessParams[i].isVariadic())
        return RequirementMatch(witness, MatchKind::TypeConflict, witnessType);

      if (reqParams[i].isInOut() != witnessParams[i].isInOut())
        return RequirementMatch(witness, MatchKind::TypeConflict, witnessType);

      auto reqParamDecl = reqParamList->get(i);
      auto witnessParamDecl = witnessParamList->get(i);

      auto reqParamTypeIsIUO =
          reqParamDecl->getAttrs()
              .hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
      auto witnessParamTypeIsIUO =
          witnessParamDecl->getAttrs()
              .hasAttribute<ImplicitlyUnwrappedOptionalAttr>();

      // Gross hack: strip a level of unchecked-optionality off both
      // sides when matching against a protocol imported from Objective-C.
      auto types =
          getTypesToCompare(req, reqParams[i].getOldType(), reqParamTypeIsIUO,
                            witnessParams[i].getOldType(), witnessParamTypeIsIUO,
                            VarianceKind::Contravariant);

      // Record any optional adjustment that occurred.
      if (std::get<2>(types) != OptionalAdjustmentKind::None) {
        optionalAdjustments.push_back(
          OptionalAdjustment(std::get<2>(types), i));
      }

      if (!req->isObjC() && reqParamTypeIsIUO != witnessParamTypeIsIUO)
        return RequirementMatch(witness, MatchKind::TypeConflict, witnessType);

      if (auto result = matchTypes(std::get<0>(types), std::get<1>(types))) {
        return std::move(result.getValue());
      }
    }

    // If the witness is 'throws', the requirement must be.
    if (witnessFnType->getExtInfo().throws() &&
        !reqFnType->getExtInfo().throws()) {
      return RequirementMatch(witness, MatchKind::ThrowsConflict);
    }

  } else {
    auto reqTypeIsIUO =
        req->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
    auto witnessTypeIsIUO =
        witness->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
    auto types = getTypesToCompare(req, reqType, reqTypeIsIUO, witnessType,
                                   witnessTypeIsIUO, VarianceKind::None);

    // Record optional adjustment, if any.
    if (std::get<2>(types) != OptionalAdjustmentKind::None) {
      optionalAdjustments.push_back(
        OptionalAdjustment(std::get<2>(types)));
    }

    if (!req->isObjC() && reqTypeIsIUO != witnessTypeIsIUO)
      return RequirementMatch(witness, MatchKind::TypeConflict, witnessType);

    if (auto result = matchTypes(std::get<0>(types), std::get<1>(types))) {
      return std::move(result.getValue());
    }
  }

  // Now finalize the match.
  return finalize(anyRenaming, optionalAdjustments);
}

/// Checks \p reqEnvCache for a requirement environment appropriate for
/// \p reqSig and \p covariantSelf. If one isn't there, it gets created from
/// the rest of the parameters.
///
/// Note that this means RequirementEnvironmentCaches must not be shared across
/// multiple protocols or conformances.
static const RequirementEnvironment &getOrCreateRequirementEnvironment(
    WitnessChecker::RequirementEnvironmentCache &reqEnvCache,
    DeclContext *dc, GenericSignature *reqSig, ProtocolDecl *proto,
    ClassDecl *covariantSelf, ProtocolConformance *conformance) {
  WitnessChecker::RequirementEnvironmentCacheKey cacheKey(reqSig,
                                                          covariantSelf);
  auto cacheIter = reqEnvCache.find(cacheKey);
  if (cacheIter == reqEnvCache.end()) {
    RequirementEnvironment reqEnv(dc, reqSig, proto, covariantSelf,
                                  conformance);
    cacheIter = reqEnvCache.insert({cacheKey, std::move(reqEnv)}).first;
  }
  return cacheIter->getSecond();
}

RequirementMatch
swift::matchWitness(TypeChecker &tc,
                    WitnessChecker::RequirementEnvironmentCache &reqEnvCache,
                    ProtocolDecl *proto, ProtocolConformance *conformance,
                    DeclContext *dc, ValueDecl *req, ValueDecl *witness) {
  using namespace constraints;

  // Initialized by the setup operation.
  Optional<ConstraintSystem> cs;
  ConstraintLocator *locator = nullptr;
  ConstraintLocator *reqLocator = nullptr;
  ConstraintLocator *witnessLocator = nullptr;
  Type witnessType, openWitnessType;
  Type openedFullWitnessType;
  Type reqType, openedFullReqType;

  auto *reqSig = req->getInnermostDeclContext()->getGenericSignatureOfContext();

  ClassDecl *covariantSelf = nullptr;
  if (witness->getDeclContext()->getExtendedProtocolDecl()) {
    if (auto *classDecl = dc->getSelfClassDecl()) {
      if (!classDecl->isFinal()) {
        // If the requirement's type does not involve any associated types,
        // we use a class-constrained generic parameter as the 'Self' type
        // in the witness thunk.
        //
        // This allows the following code to type check:
        //
        // protocol P {
        //   func f() -> Self
        // }
        //
        // extension P {
        //   func f() { return self }
        // }
        //
        // class C : P {}
        //
        // When we call (C() as P).f(), we want to pass the 'Self' type
        // from the call site, not the static 'Self' type of the conformance.
        //
        // On the other hand, if the requirement's type contains associated
        // types, we use the static 'Self' type, to preserve backward
        // compatibility with code that uses this pattern:
        //
        // protocol P {
        //   associatedtype T = Self
        //   func f() -> T
        // }
        //
        // extension P where Self.T == Self {
        //   func f() -> Self { return self }
        // }
        //
        // class C : P {}
        //
        // It would have been much nicer to just ban this completely if
        // the class 'C' is not final, but there is a great deal of existing
        // code out there that relies on this behavior, most commonly by
        // defining a non-final class conforming to 'Collection' which uses
        // the default witness for 'Collection.Iterator', which is defined
        // as 'IndexingIterator<Self>'.
        auto selfKind = proto->findProtocolSelfReferences(req,
                                             /*allowCovariantParameters=*/false,
                                             /*skipAssocTypes=*/false);
        if (!selfKind.other) {
          covariantSelf = classDecl;
        }
      }
    }
  }

  const RequirementEnvironment &reqEnvironment =
      getOrCreateRequirementEnvironment(reqEnvCache, dc, reqSig, proto,
                                        covariantSelf, conformance);

  // Set up the constraint system for matching.
  auto setup = [&]() -> std::tuple<Optional<RequirementMatch>, Type, Type> {
    // Construct a constraint system to use to solve the equality between
    // the required type and the witness type.
    cs.emplace(tc, dc, ConstraintSystemOptions());

    auto reqGenericEnv = reqEnvironment.getSyntheticEnvironment();
    auto reqSubMap = reqEnvironment.getRequirementToSyntheticMap();

    Type selfTy = proto->getSelfInterfaceType().subst(reqSubMap);
    if (reqGenericEnv)
      selfTy = reqGenericEnv->mapTypeIntoContext(selfTy);

    // Open up the type of the requirement.
    reqLocator = cs->getConstraintLocator(
                     static_cast<Expr *>(nullptr),
                     LocatorPathElt(ConstraintLocator::Requirement, req));
    OpenedTypeMap reqReplacements;
    std::tie(openedFullReqType, reqType)
      = cs->getTypeOfMemberReference(selfTy, req, dc,
                                     /*isDynamicResult=*/false,
                                     FunctionRefKind::DoubleApply,
                                     reqLocator,
                                     /*base=*/nullptr,
                                     &reqReplacements);
    reqType = reqType->getRValueType();

    // For any type parameters we replaced in the witness, map them
    // to the corresponding archetypes in the witness's context.
    for (const auto &replacement : reqReplacements) {
      auto replacedInReq = Type(replacement.first).subst(reqSubMap);

      // If substitution failed, skip the requirement. This only occurs in
      // invalid code.
      if (!replacedInReq || replacedInReq->hasError())
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
    if (witness->getDeclContext()->isTypeContext()) {
      std::tie(openedFullWitnessType, openWitnessType) 
        = cs->getTypeOfMemberReference(selfTy, witness, dc,
                                       /*isDynamicResult=*/false,
                                       FunctionRefKind::DoubleApply,
                                       witnessLocator,
                                       /*base=*/nullptr);
    } else {
      std::tie(openedFullWitnessType, openWitnessType) 
        = cs->getTypeOfReference(witness,
                                 FunctionRefKind::DoubleApply,
                                 witnessLocator,
                                 /*base=*/nullptr);
    }
    openWitnessType = openWitnessType->getRValueType();

    return std::make_tuple(None, reqType, openWitnessType);
  };

  // Match a type in the requirement to a type in the witness.
  auto matchTypes = [&](Type reqType,
                        Type witnessType) -> Optional<RequirementMatch> {
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
                            reqEnvironment,
                            optionalAdjustments);

    // Compute the set of substitutions we'll need for the witness.
    auto witnessSig =
      witness->getInnermostDeclContext()->getGenericSignatureOfContext();
    result.WitnessSubstitutions =
      solution->computeSubstitutions(witnessSig, witnessLocator);
    
    return result;
  };

  return matchWitness(tc, dc, req, witness, setup, matchTypes, finalize);
}

static bool
witnessHasImplementsAttrForRequiredName(ValueDecl *witness,
                                        ValueDecl *requirement) {
  if (auto A = witness->getAttrs().getAttribute<ImplementsAttr>()) {
    return A->getMemberName() == requirement->getFullName();
  }
  return false;
}

static bool
witnessHasImplementsAttrForExactRequirement(ValueDecl *witness,
                                            ValueDecl *requirement) {
  assert(requirement->isProtocolRequirement());
  auto *PD = cast<ProtocolDecl>(requirement->getDeclContext());
  if (auto A = witness->getAttrs().getAttribute<ImplementsAttr>()) {
    Type T = A->getProtocolType().getType();
    if (T->castTo<ProtocolType>()->getDecl() == PD) {
      return A->getMemberName() == requirement->getFullName();
    }
  }
  return false;
}

/// \brief Determine whether one requirement match is better than the other.
static bool isBetterMatch(TypeChecker &tc, DeclContext *dc,
                          ValueDecl *requirement,
                          const RequirementMatch &match1,
                          const RequirementMatch &match2) {

  // Special case to prefer a witness with @_implements(Foo, bar) over one without
  // it, when the requirement was exactly for Foo.bar.
  bool match1ImplementsAttr =
    witnessHasImplementsAttrForExactRequirement(match1.Witness,
                                                requirement);
  bool match2ImplementsAttr =
    witnessHasImplementsAttrForExactRequirement(match2.Witness,
                                                requirement);
  if (match1ImplementsAttr && !match2ImplementsAttr) {
    return true;
  } else if (!match1ImplementsAttr && match2ImplementsAttr) {
    return false;
  }

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

WitnessChecker::WitnessChecker(TypeChecker &tc, ProtocolDecl *proto,
                               Type adoptee, DeclContext *dc)
    : TC(tc), Proto(proto), Adoptee(adoptee), DC(dc) {}

void
WitnessChecker::lookupValueWitnessesViaImplementsAttr(
    ValueDecl *req, SmallVector<ValueDecl *, 4> &witnesses) {
  auto lookupOptions = defaultMemberTypeLookupOptions;
  lookupOptions -= NameLookupFlags::PerformConformanceCheck;
  lookupOptions |= NameLookupFlags::IncludeAttributeImplements;
  auto candidates = TC.lookupMember(DC, Adoptee, req->getFullName(),
                                    lookupOptions);
  for (auto candidate : candidates) {
    if (witnessHasImplementsAttrForExactRequirement(candidate.getValueDecl(), req)) {
      witnesses.push_back(candidate.getValueDecl());
    }
  }
}

SmallVector<ValueDecl *, 4>
WitnessChecker::lookupValueWitnesses(ValueDecl *req, bool *ignoringNames) {
  assert(!isa<AssociatedTypeDecl>(req) && "Not for lookup for type witnesses*");
  assert(req->isProtocolRequirement());

  SmallVector<ValueDecl *, 4> witnesses;

  // Do an initial check to see if there are any @_implements remappings
  // for this requirement.
  lookupValueWitnessesViaImplementsAttr(req, witnesses);

  if (req->isOperator()) {
    // Operator lookup is always global.
    auto lookupOptions = defaultUnqualifiedLookupOptions;
    if (!DC->isCascadingContextForLookup(false))
      lookupOptions |= NameLookupFlags::KnownPrivate;
    auto lookup = TC.lookupUnqualified(DC->getModuleScopeContext(),
                                       req->getBaseName(),
                                       SourceLoc(),
                                       lookupOptions);
    for (auto candidate : lookup) {
      auto decl = candidate.getValueDecl();
      if (swift::isMemberOperator(cast<FuncDecl>(decl), Adoptee)) {
        witnesses.push_back(decl);
      }
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
      candidates = TC.lookupMember(DC, Adoptee, req->getBaseName(),
                                   lookupOptions);
      *ignoringNames = true;
    }

    for (auto candidate : candidates) {
      witnesses.push_back(candidate.getValueDecl());
    }
  }

  return witnesses;
}

bool WitnessChecker::findBestWitness(
                               ValueDecl *requirement,
                               bool *ignoringNames,
                               NormalProtocolConformance *conformance,
                               SmallVectorImpl<RequirementMatch> &matches,
                               unsigned &numViable,
                               unsigned &bestIdx,
                               bool &doNotDiagnoseMatches) {
  enum Attempt {
    Regular,
    OperatorsFromOverlay,
    Done
  };

  bool anyFromUnconstrainedExtension;
  numViable = 0;

  for (Attempt attempt = Regular; numViable == 0 && attempt != Done;
       attempt = static_cast<Attempt>(attempt + 1)) {
    SmallVector<ValueDecl *, 4> witnesses;
    switch (attempt) {
    case Regular:
      witnesses = lookupValueWitnesses(requirement, ignoringNames);
      break;
    case OperatorsFromOverlay: {
      // If we have a Clang declaration, the matching operator might be in the
      // overlay for that module.
      if (!requirement->isOperator())
        continue;

      auto *clangModule =
          dyn_cast<ClangModuleUnit>(DC->getModuleScopeContext());
      if (!clangModule)
        continue;

      DeclContext *overlay = clangModule->getAdapterModule();
      if (!overlay)
        continue;

      auto lookupOptions = defaultUnqualifiedLookupOptions;
      lookupOptions |= NameLookupFlags::KnownPrivate;
      auto lookup = TC.lookupUnqualified(overlay, requirement->getBaseName(),
                                         SourceLoc(), lookupOptions);
      for (auto candidate : lookup)
        witnesses.push_back(candidate.getValueDecl());
      break;
    }
    case Done:
      llvm_unreachable("should have exited loop");
    }

    // Match each of the witnesses to the requirement.
    anyFromUnconstrainedExtension = false;
    bestIdx = 0;

    for (auto witness : witnesses) {
      // Don't match anything in a protocol.
      // FIXME: When default implementations come along, we can try to match
      // these when they're default implementations coming from another
      // (unrelated) protocol.
      if (isa<ProtocolDecl>(witness->getDeclContext())) {
        continue;
      }

      if (!witness->hasValidSignature()) {
        TC.validateDecl(witness);

        if (!witness->hasValidSignature()) {
          doNotDiagnoseMatches = true;
          continue;
        }
      }

      auto match = matchWitness(TC, ReqEnvironmentCache, Proto, conformance, DC,
                                requirement, witness);
      if (match.isViable()) {
        ++numViable;
        bestIdx = matches.size();
      } else if (match.Kind == MatchKind::WitnessInvalid) {
        doNotDiagnoseMatches = true;
      }

      if (auto *ext = dyn_cast<ExtensionDecl>(match.Witness->getDeclContext())){
        if (!ext->isConstrainedExtension() && ext->getExtendedProtocolDecl())
          anyFromUnconstrainedExtension = true;
      }

      matches.push_back(std::move(match));
    }
  }

  if (numViable == 0) {
    // Assume any missing value witnesses for a conformance in a parseable
    // interface can be treated as opaque.
    // FIXME: ...but we should do something better about types.
    if (conformance && !conformance->isInvalid()) {
      if (auto *SF = DC->getParentSourceFile()) {
        if (SF->Kind == SourceFileKind::Interface) {
          auto match = matchWitness(TC, ReqEnvironmentCache, Proto,
                                    conformance, DC, requirement, requirement);
          assert(match.isViable());
          numViable = 1;
          bestIdx = matches.size();
          matches.push_back(std::move(match));
          return true;
        }
      }
    }

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
      if (isBetterMatch(TC, DC, requirement, matches[i], matches[bestIdx]))
        bestIdx = i;
    }

    // Make sure it is, in fact, the best.
    for (unsigned i = 0, n = matches.size(); i != n; ++i) {
      if (i == bestIdx)
        continue;

      if (!isBetterMatch(TC, DC, requirement, matches[bestIdx], matches[i])) {
        isReallyBest = false;
        break;
      }
    }
  }

  // If there are multiple equally-good candidates, we fail.
  return isReallyBest;
}

bool WitnessChecker::checkWitnessAccess(AccessScope &requiredAccessScope,
                                        ValueDecl *requirement,
                                        ValueDecl *witness,
                                        bool *isSetter) {
  *isSetter = false;
  if (!TC.getLangOpts().EnableAccessControl)
    return false;

  // Compute the intersection of the conforming type's access scope
  // and the protocol's access scope.
  auto scopeIntersection =
    requiredAccessScope.intersectWith(Proto->getFormalAccessScope(DC));
  assert(scopeIntersection.hasValue());

  requiredAccessScope = *scopeIntersection;

  AccessScope actualScopeToCheck = requiredAccessScope;

  // Setting the 'forConformance' flag means that we admit witnesses in
  // protocol extensions that we can see, but are not necessarily as
  // visible as the conforming type and protocol.
  if (!witness->isAccessibleFrom(actualScopeToCheck.getDeclContext(),
                                 /*forConformance=*/true)) {
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

    // See above about the forConformance flag.
    if (!ASD->isSetterAccessibleFrom(actualScopeToCheck.getDeclContext(),
                                     /*forConformance=*/true))
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
             const RequirementMatch &match) {
  if (!match.OptionalAdjustments.empty())
    return CheckKind::OptionalityConflict;

  bool isSetter = false;
  if (checkWitnessAccess(requiredAccessScope, requirement, match.Witness,
                         &isSetter)) {
    CheckKind kind = (isSetter
                      ? CheckKind::AccessOfSetter
                      : CheckKind::Access);
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

  if (match.Witness->getAttrs().isUnavailable(TC.Context) &&
      !requirement->getAttrs().isUnavailable(TC.Context)) {
    return CheckKind::WitnessUnavailable;
  }

  return CheckKind::Success;
}

# pragma mark Witness resolution

/// This is a wrapper of multiple instances of ConformanceChecker to allow us
/// to diagnose and fix code from a more global perspective; for instance,
/// having this wrapper can help issue a fixit that inserts protocol stubs from
/// multiple protocols under checking.
class swift::MultiConformanceChecker {
  TypeChecker &TC;
  llvm::SmallVector<ValueDecl*, 16> UnsatisfiedReqs;
  llvm::SmallVector<ConformanceChecker, 4> AllUsedCheckers;
  llvm::SmallVector<NormalProtocolConformance*, 4> AllConformances;
  llvm::SetVector<ValueDecl*> MissingWitnesses;
  llvm::SmallPtrSet<ValueDecl *, 8> CoveredMembers;

  /// Check one conformance.
  ProtocolConformance * checkIndividualConformance(
    NormalProtocolConformance *conformance, bool issueFixit);

  /// Determine whether the given requirement was left unsatisfied.
  bool isUnsatisfiedReq(NormalProtocolConformance *conformance, ValueDecl *req);
public:
  MultiConformanceChecker(TypeChecker &TC): TC(TC){}

  TypeChecker &getTypeChecker() const { return TC; }

  /// Add a conformance into the batched checker.
  void addConformance(NormalProtocolConformance *conformance) {
    AllConformances.push_back(conformance);
  }

  /// Peek the unsatisfied requirements collected during conformance checking.
  ArrayRef<ValueDecl*> getUnsatisfiedRequirements() {
    return llvm::makeArrayRef(UnsatisfiedReqs);
  }

  /// Whether this member is "covered" by one of the conformances.
  bool isCoveredMember(ValueDecl *member) const {
    return CoveredMembers.count(member) > 0;
  }

  /// Check all conformances and emit diagnosis globally.
  void checkAllConformances();
};

bool MultiConformanceChecker::
isUnsatisfiedReq(NormalProtocolConformance *conformance, ValueDecl *req) {
  if (conformance->isInvalid()) return false;
  if (isa<TypeDecl>(req)) return false;

  auto witness = conformance->hasWitness(req)
    ? conformance->getWitness(req, nullptr).getDecl()
    : nullptr;

  // An optional requirement might not have a witness...
  if (!witness)
    return req->getAttrs().hasAttribute<OptionalAttr>();

  // If the witness lands within the declaration context of the conformance,
  // record it as a "covered" member.
  if (witness->getDeclContext() == conformance->getDeclContext())
    CoveredMembers.insert(witness);

  // The witness might come from a protocol or protocol extension.
  if (witness->getDeclContext()->getSelfProtocolDecl())
    return true;

  return false;
}

void MultiConformanceChecker::checkAllConformances() {
  bool anyInvalid = false;
  for (unsigned I = 0, N = AllConformances.size(); I < N; I ++) {
    auto *conformance = AllConformances[I];
    // Check this conformance and emit fixits if this is the last one in the pool.
    checkIndividualConformance(conformance, I == N - 1);
    anyInvalid |= conformance->isInvalid();
    if (anyInvalid)
      continue;
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

static void diagnoseConformanceImpliedByConditionalConformance(
    DiagnosticEngine &Diags, NormalProtocolConformance *conformance,
    NormalProtocolConformance *implyingConf, bool issueFixit) {
  Type T = conformance->getType();
  auto proto = conformance->getProtocol();
  Type protoType = proto->getDeclaredType();
  auto implyingProto = implyingConf->getProtocol()->getDeclaredType();
  auto loc = implyingConf->getLoc();
  Diags.diagnose(loc, diag::conditional_conformances_cannot_imply_conformances,
                 T, implyingProto, protoType);

  if (!issueFixit)
    return;

  // Now we get down to business: constructing a few options for new
  // extensions. They all look like:
  //
  // extension T: ProtoType where ... {
  //     <# witnesses #>
  // }
  //
  // The options are:
  //
  // - if possible, the original bounds relaxed, when the requirements match the
  //   conforming protocol, e.g. 'X: Hashable where T: Hashable' often
  //   corresponds to 'X: Equatable where T: Equatable'. This fixit is included
  //   if all the requirements are conformance constraints to the protocol
  //   that implies the conformance.
  // - the same bounds: ... is copied from the implying extension
  // - new bounds: ... becomes a placeholder
  //
  // We could also suggest adding ", ProtoType" to the existing extension,
  // but we don't think having multiple conformances in a single extension
  // (especially conditional ones) is good Swift style, and so we don't
  // want to encourage it.

  auto ext = cast<ExtensionDecl>(implyingConf->getDeclContext());
  auto &ctxt = ext->getASTContext();

  auto &SM = ctxt.SourceMgr;
  StringRef extraIndent;
  StringRef indent = Lexer::getIndentationForLine(SM, loc, &extraIndent);

  // First, the bits that aren't the requirements are the same for all the
  // types.
  llvm::SmallString<128> prefix;
  llvm::SmallString<128> suffix;
  {
    llvm::raw_svector_ostream prefixStream(prefix);
    llvm::raw_svector_ostream suffixStream(suffix);

    ValueDecl *decl = T->getAnyNominal();
    if (!decl)
      decl = T->getAnyGeneric();

    prefixStream << "extension " << decl->getFullName() << ": " << protoType << " ";
    suffixStream << " {\n"
                 << indent << extraIndent << "<#witnesses#>\n"
                 << indent << "}\n\n"
                 << indent;
  }

  if (!ctxt.LangOpts.DiagnosticsEditorMode) {
    // The fixits below are too complicated for the command line: the suggested
    // code ends up not being displayed, and the text by itself doesn't help. So
    // instead we skip all that and just have some text.
    Diags.diagnose(loc,
                   diag::note_explicitly_state_conditional_conformance_noneditor,
                   prefix.str());
    return;
  }

  // First, we do the fixit for "matching" requirements (i.e. X: P where T: P).
  bool matchingIsValid = true;
  llvm::SmallString<128> matchingFixit = prefix;
  {
    llvm::raw_svector_ostream matchingStream(matchingFixit);
    matchingStream << "where ";
    bool first = true;
    for (const auto &req : implyingConf->getConditionalRequirements()) {
      auto firstType = req.getFirstType();
      // T: ImplyingProto => T: Proto
      if (req.getKind() == RequirementKind::Conformance &&
          req.getSecondType()->isEqual(implyingProto)) {
        auto comma = first ? "" : ", ";
        matchingStream << comma << firstType << ": " << protoType;
        first = false;
        continue;
      }
      // something didn't work out, so give up on this fixit.
      matchingIsValid = false;
      break;
    }
  }

  if (matchingIsValid) {
    matchingFixit += suffix;
    Diags
        .diagnose(loc,
                  diag::note_explicitly_state_conditional_conformance_relaxed)
        .fixItInsert(loc, matchingFixit);
  }

  // Next, do the fixit for using the same requirements, but be resilient to a
  // missing `where` clause: this is one of a few fixits that get emitted here,
  // and so is a very low priority diagnostic, and so shouldn't crash.
  if (auto TWC = ext->getTrailingWhereClause()) {
    llvm::SmallString<128> sameFixit = prefix;
    auto CSR =
        Lexer::getCharSourceRangeFromSourceRange(SM, TWC->getSourceRange());
    sameFixit += SM.extractText(CSR);
    sameFixit += suffix;
    Diags
        .diagnose(loc, diag::note_explicitly_state_conditional_conformance_same)
        .fixItInsert(loc, sameFixit);
  }

  // And finally, just the generic new-requirements one:
  llvm::SmallString<128> differentFixit = prefix;
  differentFixit += "where <#requirements#>";
  differentFixit += suffix;
  Diags
      .diagnose(loc,
                diag::note_explicitly_state_conditional_conformance_different)
      .fixItInsert(loc, differentFixit);
}

/// \brief Determine whether the type \c T conforms to the protocol \c Proto,
/// recording the complete witness table if it does.
ProtocolConformance *MultiConformanceChecker::
checkIndividualConformance(NormalProtocolConformance *conformance,
                           bool issueFixit) {
  std::vector<ValueDecl*> revivedMissingWitnesses;
  switch (conformance->getState()) {
    case ProtocolConformanceState::Incomplete:
      if (conformance->isInvalid()) {
        // Revive registered missing witnesses to handle it below.
        revivedMissingWitnesses = TC.Context.
          takeDelayedMissingWitnesses(conformance);

        // If we have no missing witnesses for this invalid conformance, the
        // conformance is invalid for other reasons, so emit diagnosis now.
        if (revivedMissingWitnesses.empty()) {
          // Emit any delayed diagnostics.
          ConformanceChecker(TC, conformance, MissingWitnesses, false).
            emitDelayedDiags();
        }
      }

      // Check the rest of the conformance below.
      break;

    case ProtocolConformanceState::CheckingTypeWitnesses:
    case ProtocolConformanceState::Checking:
    case ProtocolConformanceState::Complete:
      // Nothing to do.
      return conformance;
  }

  // Dig out some of the fields from the conformance.
  Type T = conformance->getType();
  auto canT = T->getCanonicalType();
  DeclContext *DC = conformance->getDeclContext();
  auto Proto = conformance->getProtocol();
  auto ProtoType = Proto->getDeclaredType();
  SourceLoc ComplainLoc = conformance->getLoc();

  // Note that we are checking this conformance now.
  conformance->setState(ProtocolConformanceState::Checking);
  SWIFT_DEFER { conformance->setState(ProtocolConformanceState::Complete); };

  TC.validateDecl(Proto);

  // If the protocol itself is invalid, there's nothing we can do.
  if (Proto->isInvalid()) {
    conformance->setInvalid();
    return conformance;
  }

  // If the protocol requires a class, non-classes are a non-starter.
  if (Proto->requiresClass() && !canT->getClassOrBoundGenericClass()) {
    TC.diagnose(ComplainLoc, diag::non_class_cannot_conform_to_class_protocol,
                T, ProtoType);
    conformance->setInvalid();
    return conformance;
  }

  if (Proto->isObjC()) {
    // Foreign classes cannot conform to objc protocols.
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
        TC.diagnose(ComplainLoc, diagKind.getValue(), T, ProtoType);
        conformance->setInvalid();
        return conformance;
      }
    }

    // @objc protocols can't be conditionally-conformed to. We could, in theory,
    // front-load the requirement checking to generic-instantiation time (rather
    // than conformance-lookup/construction time) and register the conformance
    // with the Obj-C runtime when they're satisfied, but we'd still have solve
    // the problem with extensions that we check for below.
    if (!conformance->getConditionalRequirements().empty()) {
      TC.diagnose(ComplainLoc,
                  diag::objc_protocol_cannot_have_conditional_conformance,
                  T, ProtoType);
      conformance->setInvalid();
      return conformance;
    }
    // And... even if it isn't conditional, we still don't currently support
    // @objc protocols in extensions of Swift generic classes, because there's
    // no stable Objective-C class object to install the protocol conformance
    // category onto.
    if (auto ext = dyn_cast<ExtensionDecl>(DC)) {
      if (auto classDecl = ext->getSelfClassDecl()) {
        if (classDecl->isGenericContext()) {
          if (!classDecl->usesObjCGenericsModel()) {
            TC.diagnose(ComplainLoc, diag::objc_protocol_in_generic_extension,
                        classDecl->isGeneric(), T, ProtoType);
            conformance->setInvalid();
            return conformance;
          }
        }
      }
    }
  }

  // Not every protocol/type is compatible with conditional conformances.
  auto conditionalReqs = conformance->getConditionalRequirementsIfAvailable();
  if (conditionalReqs && !conditionalReqs->empty()) {
    auto nestedType = canT;
    // Obj-C generics cannot be looked up at runtime, so we don't support
    // conditional conformances involving them. Check the full stack of nested
    // types for any obj-c ones.
    while (nestedType) {
      if (auto clas = nestedType->getClassOrBoundGenericClass()) {
        if (clas->usesObjCGenericsModel()) {
          TC.diagnose(ComplainLoc,
                      diag::objc_generics_cannot_conditionally_conform, T,
                      ProtoType);
          conformance->setInvalid();
          return conformance;
        }
      }

      nestedType = nestedType.getNominalParent();
    }
  }

  // If the protocol contains missing requirements, it can't be conformed to
  // at all.
  if (Proto->hasMissingRequirements()) {
    bool hasDiagnosed = false;
    auto *protoFile = Proto->getModuleScopeContext();
    if (auto *serialized = dyn_cast<SerializedASTFile>(protoFile)) {
      if (serialized->getLanguageVersionBuiltWith() !=
          TC.getLangOpts().EffectiveLanguageVersion) {
        TC.diagnose(ComplainLoc,
                    diag::protocol_has_missing_requirements_versioned, T,
                    ProtoType, serialized->getLanguageVersionBuiltWith(),
                    TC.getLangOpts().EffectiveLanguageVersion);
        hasDiagnosed = true;
      }
    }
    if (!hasDiagnosed) {
      TC.diagnose(ComplainLoc, diag::protocol_has_missing_requirements, T,
                  ProtoType);
    }
    conformance->setInvalid();
    return conformance;
  }

  bool impliedDisablesMissingWitnessFixits = false;
  if (conformance->getSourceKind() == ConformanceEntryKind::Implied) {
    // We've got something like:
    //
    //   protocol Foo : Proto {}
    //   extension SomeType : Foo {}
    //
    // We don't want to allow this when the SomeType : Foo conformance is
    // conditional
    auto implyingConf = conformance->getImplyingConformance();
    // There might be a long chain of implications, e.g. protocol Foo: Proto {}
    // protocol Bar: Foo {} extension SomeType: Bar {}, so keep looking all the
    // way up.
    while (implyingConf->getSourceKind() == ConformanceEntryKind::Implied) {
      implyingConf = implyingConf->getImplyingConformance();
    }

    auto implyingCondReqs =
      implyingConf->getConditionalRequirementsIfAvailable();
    if (implyingCondReqs && !implyingCondReqs->empty()) {
      // We shouldn't suggest including witnesses for the conformance, because
      // those suggestions will go in the current DeclContext, but really they
      // should go into the new extension we (might) suggest here.
      impliedDisablesMissingWitnessFixits = true;

      diagnoseConformanceImpliedByConditionalConformance(
          TC.Diags, conformance, implyingConf, issueFixit);

      conformance->setInvalid();
    }
  }

  // Check that T conforms to all inherited protocols.
  for (auto InheritedProto : Proto->getInheritedProtocols()) {
    auto InheritedConformance =
    TC.conformsToProtocol(
                        T, InheritedProto, DC,
                        (ConformanceCheckFlags::Used|
                         ConformanceCheckFlags::SkipConditionalRequirements),
                        ComplainLoc);
    if (!InheritedConformance || !InheritedConformance->isConcrete()) {
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
  MissingWitnesses.insert(revivedMissingWitnesses.begin(),
                          revivedMissingWitnesses.end());

  auto missingWitnessFixits = issueFixit && !impliedDisablesMissingWitnessFixits;
  AllUsedCheckers.back().checkConformance(
      missingWitnessFixits ? MissingWitnessDiagnosisKind::ErrorFixIt
                           : MissingWitnessDiagnosisKind::ErrorOnly);
  return conformance;
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
static Type getTypeForDisplay(ModuleDecl *module, ValueDecl *decl) {
  // If we're not in a type context, just grab the interface type.
  Type type = decl->getInterfaceType();
  if (!decl->getDeclContext()->isTypeContext() ||
      !isa<AbstractFunctionDecl>(decl))
    return type;

  // For a constructor, we only care about the parameter types.
  if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    return AnyFunctionType::composeInput(module->getASTContext(),
                                         ctor->getMethodInterfaceType()
                                             ->castTo<FunctionType>()
                                             ->getParams(),
                                         /*canonicalVararg=*/false);
  }

  // We have something function-like, so we want to strip off the 'self'.
  if (auto genericFn = type->getAs<GenericFunctionType>()) {
    if (auto resultFn = genericFn->getResult()->getAs<FunctionType>()) {
      // For generic functions, build a new generic function... but strip off
      // the requirements. They don't add value.
      auto sigWithoutReqts
        = GenericSignature::get(genericFn->getGenericParams(), {});
      return GenericFunctionType::get(sigWithoutReqts,
                                      resultFn->getParams(),
                                      resultFn->getResult(),
                                      resultFn->getExtInfo());
    }
  }

  // Redeclaration checking might mark a candidate as `invalid` and
  // reset it's type to ErrorType, let's dig out original type to
  // make the diagnostic better.
  if (auto errorType = type->getAs<ErrorType>()) {
    auto originalType = errorType->getOriginalType();
    if (!originalType || !originalType->is<AnyFunctionType>())
      return type;

    type = originalType;
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
  return type.subst([&](SubstitutableType *dependentType) {
                      if (dependentType->isEqual(selfTy))
                        return conformance->getType();

                      return Type(dependentType);
                    },
                    LookUpConformanceInModule(module),
                    SubstFlags::UseErrorType);
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
    params = func->getParameters();
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

/// Classify the provided optionality issues for use in diagnostics.
/// FIXME: Enumify this
static unsigned classifyOptionalityIssues(
    const SmallVectorImpl<OptionalAdjustment> &adjustments,
    ValueDecl *requirement) {
  unsigned numParameterAdjustments = 0;
  bool hasNonParameterAdjustment = false;
  for (const auto &adjustment : adjustments) {
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

static void addOptionalityFixIts(
    const SmallVectorImpl<OptionalAdjustment> &adjustments,
    const ASTContext &ctx,
    ValueDecl *witness, 
    InFlightDiagnostic &diag) {
  for (const auto &adjustment : adjustments) {
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
              ValueDecl *req, const RequirementMatch &match) {

  // If the name doesn't match and that's not the only problem,
  // it is likely this witness wasn't intended to be a match at all, so omit
  // diagnosis.
  if (match.Kind != MatchKind::RenamedMatch &&
      !match.Witness->getAttrs().hasAttribute<ImplementsAttr>() &&
      match.Witness->getFullName() &&
      req->getFullName() != match.Witness->getFullName())
    return;

  // Form a string describing the associated type deductions.
  // FIXME: Determine which associated types matter, and only print those.
  llvm::SmallString<128> withAssocTypes;
  for (auto assocType : conformance->getProtocol()->getAssociatedTypeMembers()) {
    if (conformance->usesDefaultDefinition(assocType)) {
      Type witness = conformance->getTypeWitness(assocType, nullptr);
      addAssocTypeDeductionString(withAssocTypes, assocType, witness);
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
    auto &adjustments = match.OptionalAdjustments;
    auto diag = diags.diagnose(match.Witness, 
                               diag::protocol_witness_optionality_conflict,
                               classifyOptionalityIssues(adjustments, req),
                               withAssocTypes);
    addOptionalityFixIts(adjustments,
                         match.Witness->getASTContext(),
                         match.Witness,
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
    diags.diagnose(match.Witness,
                   diag::protocol_witness_mutation_modifier_conflict,
                   unsigned(SelfAccessKind::Mutating));
    break;
  case MatchKind::NonMutatingConflict:
    // FIXME: Could emit a Fix-It here.
    diags.diagnose(match.Witness,
                   diag::protocol_witness_mutation_modifier_conflict,
                   unsigned(SelfAccessKind::NonMutating));
    break;
  case MatchKind::ConsumingConflict:
    // FIXME: Could emit a Fix-It here.
    diags.diagnose(match.Witness,
                   diag::protocol_witness_mutation_modifier_conflict,
                   unsigned(SelfAccessKind::__Consuming));
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

ConformanceChecker::ConformanceChecker(
                   TypeChecker &tc, NormalProtocolConformance *conformance,
                   llvm::SetVector<ValueDecl*> &GlobalMissingWitnesses,
                   bool suppressDiagnostics)
    : WitnessChecker(tc, conformance->getProtocol(),
                     conformance->getType(),
                     conformance->getDeclContext()),
      Conformance(conformance), Loc(conformance->getLoc()),
      GlobalMissingWitnesses(GlobalMissingWitnesses),
      LocalMissingWitnessesStartIndex(GlobalMissingWitnesses.size()),
      SuppressDiagnostics(suppressDiagnostics) {
  // The protocol may have only been validatedDeclForNameLookup'd until
  // here, so fill in any information that's missing.
  tc.validateDecl(conformance->getProtocol());
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
  req->getInterfaceType()->getCanonicalType().visit([&](CanType type) {
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

  // Record this witness in the conformance.
  auto witness = match.getWitness(TC.Context);
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

/// Returns the location we should use for a primary diagnostic (an error or
/// warning) that concerns \p witness but arose as part of checking
/// \p conformance.
///
/// Ideally we'd like to use the location of \p witness for this, but that
/// could be confusing if the conformance is declared somewhere else. Moreover,
/// if the witness and the conformance declaration are in different files, we
/// could be issuing diagnostics in one file that wouldn't be present if we
/// recompiled just that file. Therefore, we only use the witness's location if
/// it's in the same type or extension that declares the conformance.
static SourceLoc
getLocForDiagnosingWitness(const NormalProtocolConformance *conformance,
                           const ValueDecl *witness) {
  if (witness && witness->getDeclContext() == conformance->getDeclContext()) {
    SourceLoc witnessLoc = witness->getLoc();
    if (witnessLoc.isValid())
      return witnessLoc;
  }
  return conformance->getLoc();
}

/// Emits a "'foo' declared here" note unless \p mainDiagLoc is already the
/// location of \p value.
static void emitDeclaredHereIfNeeded(DiagnosticEngine &diags,
                                     SourceLoc mainDiagLoc,
                                     const ValueDecl *value) {
  if (!value)
    return;
  if (mainDiagLoc == value->getLoc())
    return;
  diags.diagnose(value, diag::decl_declared_here, value->getFullName());
}

bool ConformanceChecker::checkObjCTypeErasedGenerics(
                                                 AssociatedTypeDecl *assocType,
                                                 Type type,
                                                 TypeDecl *typeDecl) {
  // Objective-C's type-erased generics don't allow the type arguments
  // to be extracted from an instance (or a metatype), so we cannot refer to
  // the type parameters from an associated type. Check that here.
  auto &ctx = assocType->getASTContext();
  if (!ctx.LangOpts.EnableObjCInterop && type->hasError())
    return false;

  auto classDecl = Adoptee->getClassOrBoundGenericClass();
  if (!classDecl) return false;

  if (!classDecl->usesObjCGenericsModel()) return false;

  // Concrete types are okay.
  if (!type->hasTypeParameter()) return false;

  // Find one of the generic parameters named. It doesn't matter
  // which one.
  Type genericParam;
  (void)type.findIf([&](Type type) {
    if (auto gp = type->getAs<GenericTypeParamType>()) {
      genericParam = gp;
      return true;
    }

    return false;
  });

  // Diagnose the problem.
  SourceLoc diagLoc = getLocForDiagnosingWitness(Conformance, typeDecl);
  ctx.Diags.diagnose(diagLoc, diag::type_witness_objc_generic_parameter,
                     type, genericParam, !genericParam.isNull(),
                     assocType->getFullName(), Proto->getFullName());
  emitDeclaredHereIfNeeded(ctx.Diags, diagLoc, typeDecl);

  return true;
}

void ConformanceChecker::recordTypeWitness(AssociatedTypeDecl *assocType,
                                           Type type,
                                           TypeDecl *typeDecl) {

  // If we already recoded this type witness, there's nothing to do.
  if (Conformance->hasTypeWitness(assocType)) {
    assert(Conformance->getTypeWitness(assocType, nullptr)->isEqual(type) &&
           "Conflicting type witness deductions");
    return;
  }

  assert(!type->hasArchetype() && "Got a contextual type here?");

  checkObjCTypeErasedGenerics(assocType, type, typeDecl);

  if (typeDecl) {
    // Check access.
    AccessScope requiredAccessScope =
        Adoptee->getAnyNominal()->getFormalAccessScope(DC);
    bool isSetter = false;
    if (checkWitnessAccess(requiredAccessScope, assocType, typeDecl,
                           &isSetter)) {
      assert(!isSetter);

      // Avoid relying on the lifetime of 'this'.
      const DeclContext *DC = this->DC;
      diagnoseOrDefer(assocType, false,
        [DC, typeDecl, requiredAccessScope](
          NormalProtocolConformance *conformance) {
        AccessLevel requiredAccess =
          requiredAccessScope.requiredAccessForDiagnostics();
        auto proto = conformance->getProtocol();
        auto protoAccessScope = proto->getFormalAccessScope(DC);
        bool protoForcesAccess =
          requiredAccessScope.hasEqualDeclContextWith(protoAccessScope);
        auto diagKind = protoForcesAccess
                          ? diag::type_witness_not_accessible_proto
                          : diag::type_witness_not_accessible_type;
        auto &diags = DC->getASTContext().Diags;
        diags.diagnose(getLocForDiagnosingWitness(conformance, typeDecl),
                       diagKind,
                       typeDecl->getDescriptiveKind(),
                       typeDecl->getFullName(),
                       requiredAccess,
                       proto->getName());
        auto fixItDiag = diags.diagnose(typeDecl, diag::witness_fix_access,
                                        typeDecl->getDescriptiveKind(),
                                        requiredAccess);
        fixItAccess(fixItDiag, typeDecl, requiredAccess);
      });
    }
  } else {
    // If there was no type declaration, synthesize one.
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
    if (auto nominal = DC->getSelfNominalTypeDecl()) {
      // FIXME: Ideally this would use the protocol's access too---that is,
      // a typealias added for an internal protocol shouldn't need to be
      // public---but that can be problematic if the same type conforms to two
      // protocols with different access levels.
      aliasDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/true);

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
  Conformance->setTypeWitness(assocType, type, typeDecl);

  // Record type witnesses for any "overridden" associated types.
  llvm::SetVector<AssociatedTypeDecl *> overriddenAssocTypes;
  auto assocOverriddenDecls = assocType->getOverriddenDecls();
  overriddenAssocTypes.insert(assocOverriddenDecls.begin(),
                              assocOverriddenDecls.end());
  for (unsigned idx = 0; idx < overriddenAssocTypes.size(); ++idx) {
    auto overridden = overriddenAssocTypes[idx];

    // Note all of the newly-discovered overridden associated types.
    auto overriddenDecls = overridden->getOverriddenDecls();
    overriddenAssocTypes.insert(overriddenDecls.begin(), overriddenDecls.end());

    // Find the conformance for this overridden protocol.
    auto overriddenConformance =
      DC->getParentModule()->lookupConformance(Adoptee,
                                               overridden->getProtocol());
    if (!overriddenConformance ||
        !overriddenConformance->isConcrete())
      continue;

    auto overriddenRootConformance =
      overriddenConformance->getConcrete()->getRootNormalConformance();
    ConformanceChecker(TC, overriddenRootConformance, GlobalMissingWitnesses)
      .recordTypeWitness(overridden, type, typeDecl);
  }
}

bool swift::
printRequirementStub(ValueDecl *Requirement, DeclContext *Adopter,
                     Type AdopterTy, SourceLoc TypeLoc, raw_ostream &OS) {
  if (isa<ConstructorDecl>(Requirement)) {
    if (auto CD = Adopter->getSelfClassDecl()) {
      if (!CD->isFinal() && Adopter->isExtensionContext()) {
          // In this case, user should mark class as 'final' or define
          // 'required' initializer directly in the class definition.
          return false;
      }
    }
  }
  if (auto MissingTypeWitness = dyn_cast<AssociatedTypeDecl>(Requirement)) {
    if (!MissingTypeWitness->getDefaultDefinitionLoc().isNull()) {
      // For type witnesses with default definitions, we don't print the stub.
      return false;
    }
  }
  // FIXME: Infer body indentation from the source rather than hard-coding
  // 4 spaces.
  ASTContext &Ctx = Requirement->getASTContext();
  StringRef ExtraIndent;
  StringRef CurrentIndent =
      Lexer::getIndentationForLine(Ctx.SourceMgr, TypeLoc, &ExtraIndent);
  std::string StubIndent = (CurrentIndent + ExtraIndent).str();

  ExtraIndentStreamPrinter Printer(OS, StubIndent);
  Printer.printNewline();

  AccessLevel Access =
    std::min(
      /* Access of the context */
      Adopter->getSelfNominalTypeDecl()->getFormalAccess(),
      /* Access of the protocol */
      Requirement->getDeclContext()->getSelfProtocolDecl()->
        getFormalAccess());
  if (Access == AccessLevel::Public)
    Printer << "public ";

  if (auto MissingTypeWitness = dyn_cast<AssociatedTypeDecl>(Requirement)) {
    Printer << "typealias " << MissingTypeWitness->getName() << " = <#type#>";
    Printer << "\n";
  } else {
    if (isa<ConstructorDecl>(Requirement)) {
      if (auto CD = Adopter->getSelfClassDecl()) {
        if (!CD->isFinal()) {
          Printer << "required ";
        } else if (Adopter->isExtensionContext()) {
          Printer << "convenience ";
        }
      }
    }

    PrintOptions Options = PrintOptions::printForDiagnostics();
    Options.PrintDocumentationComments = false;
    Options.AccessFilter = AccessLevel::Private;
    Options.PrintAccess = false;
    Options.SkipAttributes = true;
    Options.FunctionDefinitions = true;
    Options.PrintAccessorBodiesInProtocols = true;
    Options.FunctionBody = [&](const ValueDecl *VD, ASTPrinter &Printer) {
      Printer << " {";
      Printer.printNewline();
      Printer << ExtraIndent << getCodePlaceholder();
      Printer.printNewline();
      Printer << "}";
    };
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
  SourceLoc ComplainLoc = Loc;
  bool EditorMode = TC.getLangOpts().DiagnosticsEditorMode;
  llvm::SetVector<ValueDecl*> MissingWitnesses(GlobalMissingWitnesses.begin(),
                                               GlobalMissingWitnesses.end());
  auto InsertFixitCallback = [ComplainLoc, EditorMode, MissingWitnesses]
      (NormalProtocolConformance *Conf) {
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

    // If we are in editor mode, squash all notes into a single fixit.
    if (EditorMode) {
      if (!FixIt.empty()) {
        Diags.diagnose(ComplainLoc, diag::missing_witnesses_general).
          fixItInsertAfter(FixitLocation, FixIt);
      }
      return;
    }
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
    if (SuppressDiagnostics) {
      // If the diagnostics are suppressed, we register these missing witnesses
      // for later revisiting.
      Conformance->setInvalid();
      TC.Context.addDelayedMissingWitnesses(Conformance,
                                            MissingWitnesses.getArrayRef());
    } else {
      diagnoseOrDefer(LocalMissing[0], true, InsertFixitCallback);
    }
    clearGlobalMissingWitnesses();
    return;
  }
  case MissingWitnessDiagnosisKind::ErrorOnly: {
    diagnoseOrDefer(LocalMissing[0], true,
                    [](NormalProtocolConformance *Conf) {});
    return;
  }
  case MissingWitnessDiagnosisKind::FixItOnly:
    InsertFixitCallback(Conformance);
    clearGlobalMissingWitnesses();
    return;
  }
}

/// Determine the given witness has a same-type constraint constraining the
/// given 'Self' type, and return the requirement that does.
///
/// \returns None if there is no such constraint; a non-empty optional that
/// may have the \c RequirementRepr for the actual constraint.
static Optional<RequirementRepr *>
getAdopteeSelfSameTypeConstraint(ClassDecl *selfClass, ValueDecl *witness) {
  auto genericSig =
    witness->getInnermostDeclContext()->getGenericSignatureOfContext();
  if (!genericSig) return None;

  for (const auto &req : genericSig->getRequirements()) {
    if (req.getKind() != RequirementKind::SameType)
      continue;

    if (req.getFirstType()->getAnyNominal() == selfClass ||
        req.getSecondType()->getAnyNominal() == selfClass) {
      // Try to find the requirement-as-written.
      GenericParamList *genericParams = nullptr;

      if (auto func = dyn_cast<AbstractFunctionDecl>(witness))
        genericParams = func->getGenericParams();
      else if (auto subscript = dyn_cast<SubscriptDecl>(witness))
        genericParams = subscript->getGenericParams();
      if (genericParams) {
        for (auto &req : genericParams->getRequirements()) {
          if (req.getKind() != RequirementReprKind::SameType)
            continue;

          if (req.getFirstType()->getAnyNominal() == selfClass ||
              req.getSecondType()->getAnyNominal() == selfClass)
            return &req;
        }
      }

      // Form an optional(nullptr) to indicate that we don't have the
      // requirement itself.
      return nullptr;
    }
  }

  return None;
}

void ConformanceChecker::checkNonFinalClassWitness(ValueDecl *requirement,
                                                   ValueDecl *witness) {
  auto *classDecl = Adoptee->getClassOrBoundGenericClass();

  // If we have an initializer requirement and the conforming type
  // is a non-final class, the witness must be 'required'.
  // We exempt Objective-C initializers from this requirement
  // because there is no equivalent to 'required' in Objective-C.
  if (auto ctor = dyn_cast<ConstructorDecl>(witness)) {
    if (!ctor->isRequired() &&
        !ctor->getDeclContext()->getSelfProtocolDecl() &&
        !ctor->hasClangNode()) {
      // FIXME: We're not recovering (in the AST), so the Fix-It
      // should move.
      diagnoseOrDefer(requirement, false,
        [ctor, requirement](NormalProtocolConformance *conformance) {
          bool inExtension = isa<ExtensionDecl>(ctor->getDeclContext());
          auto &diags = ctor->getASTContext().Diags;
          SourceLoc diagLoc = getLocForDiagnosingWitness(conformance, ctor);
          Optional<InFlightDiagnostic> fixItDiag =
              diags.diagnose(diagLoc, diag::witness_initializer_not_required,
                             requirement->getFullName(), inExtension,
                             conformance->getType());
          if (diagLoc != ctor->getLoc() && !ctor->isImplicit()) {
            // If the main diagnostic is emitted on the conformance, we want to
            // attach the fix-it to the note that shows where the initializer is
            // defined.
            fixItDiag.getValue().flush();
            fixItDiag.emplace(diags.diagnose(ctor, diag::decl_declared_here,
                                             ctor->getFullName()));
          }
          if (!inExtension) {
            fixItDiag->fixItInsert(ctor->getAttributeInsertionLoc(true),
                                   "required ");
          }
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
        SourceLoc diagLoc = getLocForDiagnosingWitness(conformance, witness);
        diags.diagnose(diagLoc, diag::witness_self_non_subtype,
                       proto->getDeclaredType(), requirement->getFullName(),
                       conformance->getType());
        emitDeclaredHereIfNeeded(diags, diagLoc, witness);
      });
  } else if (selfKind.result) {
    // The reference to Self occurs in the result type. A non-final class
    // can satisfy this requirement with a method that returns Self.

    // If the function has a dynamic Self, it's okay.
    if (auto func = dyn_cast<FuncDecl>(witness)) {
      if (func->getDeclContext()->getSelfClassDecl() &&
          !func->hasDynamicSelf()) {
        diagnoseOrDefer(requirement, false,
          [witness, requirement](NormalProtocolConformance *conformance) {
            auto proto = conformance->getProtocol();
            auto &diags = proto->getASTContext().Diags;
            SourceLoc diagLoc = getLocForDiagnosingWitness(conformance,witness);
            diags.diagnose(diagLoc,
                           diag::witness_requires_dynamic_self,
                           requirement->getFullName(),
                           conformance->getType(),
                           proto->getDeclaredType());
            emitDeclaredHereIfNeeded(diags, diagLoc, witness);
          });
      }

    // Constructors conceptually also have a dynamic Self
    // return type, so they're okay.
    } else if (!isa<ConstructorDecl>(witness)) {
      diagnoseOrDefer(requirement, false,
        [witness, requirement](NormalProtocolConformance *conformance) {
          auto proto = conformance->getProtocol();
          auto &diags = proto->getASTContext().Diags;
          SourceLoc diagLoc = getLocForDiagnosingWitness(conformance, witness);
          diags.diagnose(diagLoc, diag::witness_self_non_subtype,
                         proto->getDeclaredType(),
                         requirement->getFullName(),
                         conformance->getType());
          emitDeclaredHereIfNeeded(diags, diagLoc, witness);
        });
    }
  } else if (selfKind.requirement) {
    if (auto constraint = getAdopteeSelfSameTypeConstraint(classDecl,
                                                           witness)) {
      // A "Self ==" constraint works incorrectly with subclasses. Complain.
      auto proto = Conformance->getProtocol();
      auto &diags = proto->getASTContext().Diags;
      SourceLoc diagLoc = getLocForDiagnosingWitness(Conformance, witness);
      diags.diagnose(diagLoc, diag::witness_self_same_type,
                     witness->getDescriptiveKind(),
                     witness->getFullName(),
                     Conformance->getType(),
                     requirement->getDescriptiveKind(),
                     requirement->getFullName(),
                     proto->getDeclaredType());
      emitDeclaredHereIfNeeded(diags, diagLoc, witness);

      if (auto requirementRepr = *constraint) {
        diags.diagnose(requirementRepr->getSeparatorLoc(),
                       diag::witness_self_weaken_same_type,
                       requirementRepr->getFirstType(),
                       requirementRepr->getSecondType())
          .fixItReplace(requirementRepr->getSeparatorLoc(), ":");
      }
    }
  }

  // A non-final class can model a protocol requirement with a
  // contravariant Self, because here the witness will always have
  // a more general type than the requirement.

  // If the witness is in a protocol extension, there's an additional
  // constraint that either the requirement not produce 'Self' in a
  // covariant position, or the type of the requirement does not involve
  // associated types.
  if (auto func = dyn_cast<FuncDecl>(witness)) {
    if (func->getDeclContext()->getExtendedProtocolDecl()) {
      auto selfKindWithAssocTypes = Proto->findProtocolSelfReferences(
          requirement,
          /*allowCovariantParameters=*/false,
          /*skipAssocTypes=*/false);
      if (selfKindWithAssocTypes.other &&
          selfKindWithAssocTypes.result) {
        diagnoseOrDefer(requirement, false,
          [witness, requirement](NormalProtocolConformance *conformance) {
            auto proto = conformance->getProtocol();
            auto &diags = proto->getASTContext().Diags;
            diags.diagnose(conformance->getLoc(),
                           diag::witness_requires_class_implementation,
                           requirement->getFullName(),
                           conformance->getType());
            diags.diagnose(witness, diag::decl_declared_here,
                           witness->getFullName());
          });
      }
    }
  }
}

ResolveWitnessResult
ConformanceChecker::resolveWitnessViaLookup(ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Use resolveTypeWitnessVia*");

  auto *nominal = Adoptee->getAnyNominal();

  // Resolve all associated types before trying to resolve this witness.
  resolveTypeWitnesses();

  // If any of the type witnesses was erroneous, don't bother to check
  // this value witness: it will fail.
  for (auto assocType : getReferencedAssociatedTypes(requirement)) {
    if (Conformance->getTypeWitness(assocType, nullptr)->hasError()) {
      return ResolveWitnessResult::ExplicitFailed;
    }
  }

  // Determine whether we can derive a witness for this requirement.
  bool canDerive = false;

  // Can a witness for this requirement be derived for this nominal type?
  if (auto derivable = DerivedConformance::getDerivableRequirement(
                         TC,
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
          (void)conformance->getConcrete()->getWitnessDecl(derivable, &TC);
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
  if (findBestWitness(requirement,
                      considerRenames ? &ignoringNames : nullptr,
                      Conformance,
                      /* out parameters: */
                      matches, numViable, bestIdx, doNotDiagnoseMatches)) {
    const auto &best = matches[bestIdx];
    auto witness = best.Witness;

    // If the name didn't actually line up, complain.
    if (ignoringNames &&
        requirement->getFullName() != best.Witness->getFullName() &&
        !witnessHasImplementsAttrForRequiredName(best.Witness, requirement)) {

      diagnoseOrDefer(requirement, false,
        [witness, requirement](NormalProtocolConformance *conformance) {
          auto proto = conformance->getProtocol();
          auto &diags = proto->getASTContext().Diags;
          {
            SourceLoc diagLoc = getLocForDiagnosingWitness(conformance,witness);
            auto diag = diags.diagnose(diagLoc,
                                       diag::witness_argument_name_mismatch,
                                       isa<ConstructorDecl>(witness),
                                       witness->getFullName(),
                                       proto->getDeclaredType(),
                                       requirement->getFullName());
            if (diagLoc == witness->getLoc()) {
              fixDeclarationName(diag, witness, requirement->getFullName());
            } else {
              diag.flush();
              diags.diagnose(witness, diag::decl_declared_here,
                             witness->getFullName());
            }
          }

          diags.diagnose(requirement, diag::kind_declname_declared_here,
                         DescriptiveDeclKind::Requirement,
                         requirement->getFullName());
        });
    }

    auto nominalAccessScope = nominal->getFormalAccessScope(DC);
    auto check = checkWitness(nominalAccessScope, requirement, best);

    switch (check.Kind) {
    case CheckKind::Success:
      break;

    case CheckKind::Access:
    case CheckKind::AccessOfSetter: {
      // Swift 4.2 relaxed some rules for protocol witness matching.
      //
      // This meant that it was possible for an optional protocol requirement
      // to have a witness where previously in Swift 4.1 it did not.
      //
      // Since witnesses must be as visible as the protocol, this caused a
      // source compatibility break if the witness was not sufficiently
      // visible.
      //
      // Work around this by discarding the witness if its not sufficiently
      // visible.
      if (!TC.Context.isSwiftVersionAtLeast(5))
        if (requirement->getAttrs().hasAttribute<OptionalAttr>())
          return ResolveWitnessResult::Missing;

      // Avoid relying on the lifetime of 'this'.
      const DeclContext *DC = this->DC;
      diagnoseOrDefer(requirement, false,
        [DC, witness, check, requirement](
          NormalProtocolConformance *conformance) {
        auto requiredAccessScope = check.RequiredAccessScope;
        AccessLevel requiredAccess =
          requiredAccessScope.requiredAccessForDiagnostics();
        auto proto = conformance->getProtocol();
        auto protoAccessScope = proto->getFormalAccessScope(DC);
        bool protoForcesAccess =
          requiredAccessScope.hasEqualDeclContextWith(protoAccessScope);
        auto diagKind = protoForcesAccess
                          ? diag::witness_not_accessible_proto
                          : diag::witness_not_accessible_type;
        bool isSetter = (check.Kind == CheckKind::AccessOfSetter);

        auto &diags = DC->getASTContext().Diags;
        diags.diagnose(getLocForDiagnosingWitness(conformance, witness),
                       diagKind,
                       getRequirementKind(requirement),
                       witness->getFullName(),
                       isSetter,
                       requiredAccess,
                       protoAccessScope.accessLevelForDiagnostics(),
                       proto->getName());
        if (auto *decl = dyn_cast<AbstractFunctionDecl>(witness)) {
          auto isMemberwiseInitializer =
              decl->getBodyKind() ==
              AbstractFunctionDecl::BodyKind::MemberwiseInitializer;
          if (isMemberwiseInitializer) {
            return;
          }
        }
        auto fixItDiag = diags.diagnose(witness, diag::witness_fix_access,
                                        witness->getDescriptiveKind(),
                                        requiredAccess);
        fixItAccess(fixItDiag, witness, requiredAccess, isSetter);
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
          SourceLoc diagLoc = getLocForDiagnosingWitness(conformance, witness);
          diags.diagnose(
              diagLoc, diag::availability_protocol_requires_version,
              conformance->getProtocol()->getFullName(),
              witness->getFullName(),
              prettyPlatformString(targetPlatform(ctx.LangOpts)),
              check.RequiredAvailability.getOSVersion().getLowerEndpoint());
          emitDeclaredHereIfNeeded(diags, diagLoc, witness);
          diags.diagnose(requirement,
                         diag::availability_protocol_requirement_here);
        });
      break;
    }

    case CheckKind::Unavailable: {
      auto *attr = requirement->getAttrs().getUnavailable(TC.Context);
      diagnoseUnavailableOverride(witness, requirement, attr);
      break;
    }

    case CheckKind::OptionalityConflict: {
      auto adjustments = best.OptionalAdjustments;

      diagnoseOrDefer(requirement, false,
        [witness, adjustments, requirement](NormalProtocolConformance *conformance) {
          auto proto = conformance->getProtocol();
          auto &ctx = witness->getASTContext();
          auto &diags = ctx.Diags;
          {
            SourceLoc diagLoc = getLocForDiagnosingWitness(conformance,witness);
            auto diag = diags.diagnose(
                diagLoc,
                hasAnyError(adjustments)
                  ? diag::err_protocol_witness_optionality
                  : diag::warn_protocol_witness_optionality,
                classifyOptionalityIssues(adjustments, requirement),
                witness->getFullName(),
                proto->getFullName());
            if (diagLoc == witness->getLoc()) {
              addOptionalityFixIts(adjustments, ctx, witness, diag);
            } else {
              diag.flush();
              diags.diagnose(witness, diag::decl_declared_here,
                             witness->getFullName());
            }
          }

          diags.diagnose(requirement, diag::kind_declname_declared_here,
                         DescriptiveDeclKind::Requirement,
                         requirement->getFullName());
      });
      break;
    }

    case CheckKind::ConstructorFailability:
      diagnoseOrDefer(requirement, false,
        [witness, requirement](NormalProtocolConformance *conformance) {
          auto ctor = cast<ConstructorDecl>(requirement);
          auto witnessCtor = cast<ConstructorDecl>(witness);
          auto &diags = witness->getASTContext().Diags;
          SourceLoc diagLoc = getLocForDiagnosingWitness(conformance, witness);
          diags.diagnose(diagLoc,
                         diag::witness_initializer_failability,
                         ctor->getFullName(),
                         witnessCtor->getFailability()
                           == OTK_ImplicitlyUnwrappedOptional)
            .highlight(witnessCtor->getFailabilityLoc());
          emitDeclaredHereIfNeeded(diags, diagLoc, witness);
        });

      break;

    case CheckKind::WitnessUnavailable: {
      bool emitError = !witness->getASTContext().LangOpts.isSwiftVersion3();
      diagnoseOrDefer(requirement, /*isError=*/emitError,
        [witness, requirement, emitError](
                                    NormalProtocolConformance *conformance) {
          auto &diags = witness->getASTContext().Diags;
          SourceLoc diagLoc = getLocForDiagnosingWitness(conformance, witness);
          diags.diagnose(diagLoc,
                         emitError ? diag::witness_unavailable
                                   : diag::witness_unavailable_warn,
                         witness->getDescriptiveKind(),
                         witness->getFullName(),
                         conformance->getProtocol()->getFullName());
          emitDeclaredHereIfNeeded(diags, diagLoc, witness);
          diags.diagnose(requirement, diag::kind_declname_declared_here,
                         DescriptiveDeclKind::Requirement,
                         requirement->getFullName());
        });
      break;
    }
    }

    if (auto *classDecl = Adoptee->getClassOrBoundGenericClass()) {
      if (!classDecl->isFinal()) {
        checkNonFinalClassWitness(requirement, witness);
      }
    }

    // Record the match.
    recordWitness(requirement, best);
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
  auto *nominal = Adoptee->getAnyNominal();
  if (DerivedConformance::derivesProtocolConformance(DC, nominal, Proto))
    derivingTypeDecl = nominal;

  if (!derivingTypeDecl) {
    return ResolveWitnessResult::Missing;
  }

  // Attempt to derive the witness.
  auto derived = TC.deriveProtocolRequirement(DC, derivingTypeDecl, requirement);
  if (!derived)
    return ResolveWitnessResult::ExplicitFailed;

  // Try to match the derived requirement.
  auto match = matchWitness(TC, ReqEnvironmentCache, Proto, Conformance, DC,
                            requirement, derived);
  if (match.isViable()) {
    recordWitness(requirement, match);
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

CheckTypeWitnessResult swift::checkTypeWitness(TypeChecker &tc, DeclContext *dc,
                                               ProtocolDecl *proto,
                                               AssociatedTypeDecl *assocType, 
                                               Type type) {
  auto *genericSig = proto->getGenericSignature();
  auto *depTy = DependentMemberType::get(proto->getSelfInterfaceType(),
                                         assocType);

  if (type->hasError())
    return ErrorType::get(tc.Context);

  Type contextType = type->hasTypeParameter() ? dc->mapTypeIntoContext(type)
                                              : type;

  if (auto superclass = genericSig->getSuperclassBound(depTy)) {
    if (!superclass->isExactSuperclassOf(contextType))
      return superclass;
  }

  // Check protocol conformances.
  for (auto reqProto : genericSig->getConformsTo(depTy)) {
    if (!tc.conformsToProtocol(
                          contextType, reqProto, dc,
                          ConformanceCheckFlags::SkipConditionalRequirements))
      return CheckTypeWitnessResult(reqProto->getDeclaredType());

    // FIXME: Why is conformsToProtocol() not enough? The stdlib doesn't
    // build unless we fail here while inferring an associated type
    // somewhere.
    if (contextType->isSpecialized()) {
      auto *decl = contextType->getAnyNominal();
      auto subMap = contextType->getContextSubstitutionMap(
          dc->getParentModule(),
          decl,
          decl->getGenericEnvironmentOfContext());
      for (auto replacement : subMap.getReplacementTypes()) {
        if (replacement->hasError())
          return CheckTypeWitnessResult(reqProto->getDeclaredType());
      }
    }
  }

  if (genericSig->requiresClass(depTy) &&
      !contextType->satisfiesClassConstraint())
    return CheckTypeWitnessResult(tc.Context.getAnyObjectType());

  // Success!
  return CheckTypeWitnessResult();
}

/// Attempt to resolve a type witness via member name lookup.
ResolveWitnessResult ConformanceChecker::resolveTypeWitnessViaLookup(
                       AssociatedTypeDecl *assocType) {
  // Conformances constructed by the ClangImporter should have explicit type
  // witnesses already.
  if (isa<ClangModuleUnit>(Conformance->getDeclContext()->getModuleScopeContext())) {
    llvm::errs() << "Cannot look up associated type for imported conformance:\n";
    Conformance->getType().dump(llvm::errs());
    assocType->dump(llvm::errs());
    abort();
  }

  if (!Proto->isRequirementSignatureComputed()) {
    Conformance->setInvalid();
    return ResolveWitnessResult::Missing;
  }

  // Look for a member type with the same name as the associated type.
  auto candidates = TC.lookupMemberType(DC, Adoptee, assocType->getName(),
                                        NameLookupFlags::ProtocolMembers);

  // If there aren't any candidates, we're done.
  if (!candidates) {
    return ResolveWitnessResult::Missing;
  }

  // Determine which of the candidates is viable.
  SmallVector<LookupTypeResultEntry, 2> viable;
  SmallVector<std::pair<TypeDecl *, CheckTypeWitnessResult>, 2> nonViable;
  for (auto candidate : candidates) {
    // Skip nested generic types.
    if (auto *genericDecl = dyn_cast<GenericTypeDecl>(candidate.Member))
      if (genericDecl->isGeneric())
        continue;

    // Skip typealiases with an unbound generic type as their underlying type.
    if (auto *typeAliasDecl = dyn_cast<TypeAliasDecl>(candidate.Member))
      if (typeAliasDecl->getDeclaredInterfaceType()->is<UnboundGenericType>())
        continue;

    // Check this type against the protocol requirements.
    if (auto checkResult =
            checkTypeWitness(TC, DC, Proto, assocType, candidate.MemberType)) {
      nonViable.push_back({candidate.Member, checkResult});
    } else {
      viable.push_back(candidate);
    }
  }

  // If there are no viable witnesses, and all nonviable candidates came from
  // protocol extensions, treat this as "missing".
  if (viable.empty() &&
      std::find_if(nonViable.begin(), nonViable.end(),
                   [](const std::pair<TypeDecl *, CheckTypeWitnessResult> &x) {
                     return x.first->getDeclContext()
                        ->getSelfProtocolDecl() == nullptr;
                   }) == nonViable.end())
    return ResolveWitnessResult::Missing;

  // If there is a single viable candidate, form a substitution for it.
  if (viable.size() == 1) {
    auto interfaceType = viable.front().MemberType;
    if (interfaceType->hasArchetype())
      interfaceType = interfaceType->mapTypeOutOfContext();
    recordTypeWitness(assocType, interfaceType, viable.front().Member);
    return ResolveWitnessResult::Success;
  }

  // Record an error.
  recordTypeWitness(assocType, ErrorType::get(TC.Context), nullptr);

  // If we had multiple viable types, diagnose the ambiguity.
  if (!viable.empty()) {
    diagnoseOrDefer(assocType, true,
      [assocType, viable](NormalProtocolConformance *conformance) {
        auto &diags = assocType->getASTContext().Diags;
        diags.diagnose(assocType, diag::ambiguous_witnesses_type,
                       assocType->getName());

        for (auto candidate : viable)
          diags.diagnose(candidate.Member, diag::protocol_witness_type);
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
        if (candidate.first->getDeclaredInterfaceType()->hasError() ||
            candidate.second.isError())
          continue;

        diags.diagnose(
           candidate.first,
           diag::protocol_witness_nonconform_type,
           candidate.first->getDeclaredInterfaceType(),
           candidate.second.getRequirement(),
           candidate.second.isConformanceRequirement());
      }
    });

  return ResolveWitnessResult::ExplicitFailed;
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

  // FIXME: 'deinit' is being used as a dummy identifier here. Really we
  // don't care about /any/ of the type's members, only that it conforms to
  // the protocol.
  tracker->addUsedMember({Adoptee, DeclBaseName::createDestructor()},
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

  // Mark each conformance in the signature as used.
  for (auto sigConformance : normalConf->getSignatureConformances()) {
    if (sigConformance.isConcrete())
      addUsedConformances(sigConformance.getConcrete(), visited);
  }
}

void ConformanceChecker::addUsedConformances(ProtocolConformance *conformance) {
  llvm::SmallPtrSet<ProtocolConformance *, 8> visited;
  addUsedConformances(conformance, visited);
}

void ConformanceChecker::ensureRequirementsAreSatisfied(
                                                     bool failUnsubstituted) {
  auto proto = Conformance->getProtocol();
  // Some other problem stopped the signature being computed.
  if (!proto->isRequirementSignatureComputed()) {
    Conformance->setInvalid();
    return;
  }

  if (CheckedRequirementSignature)
    return;

  CheckedRequirementSignature = true;

  if (!Conformance->getSignatureConformances().empty())
    return;

  auto DC = Conformance->getDeclContext();
  auto substitutingType = DC->mapTypeIntoContext(Conformance->getType());
  auto substitutions = SubstitutionMap::getProtocolSubstitutions(
      proto, substitutingType, ProtocolConformanceRef(Conformance));

  // Create a writer to populate the signature conformances.
  std::function<void(ProtocolConformanceRef)> writer
    = Conformance->populateSignatureConformances();

  class GatherConformancesListener : public GenericRequirementsCheckListener {
    NormalProtocolConformance *conformance;
    std::function<void(ProtocolConformanceRef)> &writer;
  public:
    GatherConformancesListener(
        NormalProtocolConformance *conformance,
        std::function<void(ProtocolConformanceRef)> &writer)
      : conformance(conformance), writer(writer) { }

    void satisfiedConformance(Type depTy, Type replacementTy,
                              ProtocolConformanceRef conformance) override {
      // The conformance will use contextual types, but we want the
      // interface type equivalent.
      if (conformance.isConcrete() &&
          conformance.getConcrete()->getType()->hasArchetype()) {
        auto concreteConformance = conformance.getConcrete();

        // Map the conformance.
        concreteConformance = concreteConformance->subst(
            [](SubstitutableType *type) -> Type {
              if (auto *archetypeType = type->getAs<ArchetypeType>())
                return archetypeType->getInterfaceType();
              return type;
            },
            MakeAbstractConformanceForGenericType());

        conformance = ProtocolConformanceRef(concreteConformance);
      }

      writer(conformance);
    }

    bool diagnoseUnsatisfiedRequirement(
                      const Requirement &req, Type first, Type second,
                      ArrayRef<ParentConditionalConformance> parents) override {
      // Invalidate the conformance to suppress further diagnostics.
      if (conformance->getLoc().isValid()) {
        conformance->setInvalid();
      }

      return false;
    }
  } listener(Conformance, writer);

  auto result = TC.checkGenericArguments(
      DC, Loc, Loc,
      // FIXME: maybe this should be the conformance's type
      proto->getDeclaredInterfaceType(),
      { proto->getSelfInterfaceType() },
      proto->getRequirementSignature(),
      QuerySubstitutionMap{substitutions},
      TypeChecker::LookUpConformance(DC),
      ConformanceCheckFlags::Used, &listener);

  switch (result) {
  case RequirementCheckResult::Success:
    return;

  case RequirementCheckResult::Failure:
    Conformance->setInvalid();
    return;

  case RequirementCheckResult::SubstitutionFailure:
    // If we're not allowed to fail, record this as a partially-checked
    // conformance.
    if (!failUnsubstituted) {
      TC.PartiallyCheckedConformances.insert(Conformance);
      return;
    }

    // Diagnose the failure generically.
    // FIXME: Would be nice to give some more context here!
    if (!Conformance->isInvalid()) {
      TC.diagnose(Loc, diag::type_does_not_conform,
                  Adoptee, Proto->getDeclaredType());
      Conformance->setInvalid();
    }
    return;
  }
}

#pragma mark Protocol conformance checking
void ConformanceChecker::checkConformance(MissingWitnessDiagnosisKind Kind) {
  assert(!Conformance->isComplete() && "Conformance is already complete");

  FrontendStatsTracer statsTracer(TC.Context.Stats, "check-conformance",
                                  Conformance);

  llvm::SaveAndRestore<bool> restoreSuppressDiagnostics(SuppressDiagnostics);
  SuppressDiagnostics = false;

  // FIXME: Caller checks that this type conforms to all of the
  // inherited protocols.

  // Emit known diags for this conformance.
  emitDelayedDiags();

  // If delayed diags have already complained, return.
  if (AlreadyComplained) {
    Conformance->setInvalid();
    return;
  }

  // Resolve all of the type witnesses.
  resolveTypeWitnesses();

  // Diagnose missing type witnesses for now.
  diagnoseMissingWitnesses(Kind);

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

  // Diagnose missing value witnesses later.
  SWIFT_DEFER { diagnoseMissingWitnesses(Kind); };

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

      // Make sure that we finalize the witness, so we can emit this
      // witness table.
      TC.DeclsToFinalize.insert(witness);

      // Objective-C checking for @objc requirements.
      if (requirement->isObjC() &&
          requirement->getFullName() == witness->getFullName() &&
          !requirement->getAttrs().isUnavailable(TC.Context)) {
        // The witness must also be @objc.
        if (!witness->isObjC()) {
          bool isOptional =
            requirement->getAttrs().hasAttribute<OptionalAttr>();
          SourceLoc diagLoc = getLocForDiagnosingWitness(Conformance, witness);
          if (auto witnessFunc = dyn_cast<AbstractFunctionDecl>(witness)) {
            auto diagInfo = getObjCMethodDiagInfo(witnessFunc);
            Optional<InFlightDiagnostic> fixItDiag =
                TC.diagnose(diagLoc,
                            isOptional ? diag::witness_non_objc_optional
                                       : diag::witness_non_objc,
                            diagInfo.first, diagInfo.second,
                            Proto->getFullName());
            if (diagLoc != witness->getLoc()) {
              // If the main diagnostic is emitted on the conformance, we want
              // to attach the fix-it to the note that shows where the
              // witness is defined.
              fixItDiag.getValue().flush();
              fixItDiag.emplace(TC.diagnose(witness, diag::make_decl_objc,
                                            witness->getDescriptiveKind()));
            }
            if (!witness->canInferObjCFromRequirement(requirement)) {
              fixDeclarationObjCName(
                  fixItDiag.getValue(), witness,
                  cast<AbstractFunctionDecl>(requirement)->getObjCSelector());
            }
          } else if (isa<VarDecl>(witness)) {
            Optional<InFlightDiagnostic> fixItDiag =
                TC.diagnose(diagLoc,
                            isOptional ? diag::witness_non_objc_storage_optional
                                       : diag::witness_non_objc_storage,
                            /*isSubscript=*/false,
                            witness->getFullName(),
                            Proto->getFullName());
            if (diagLoc != witness->getLoc()) {
              // If the main diagnostic is emitted on the conformance, we want
              // to attach the fix-it to the note that shows where the
              // witness is defined.
              fixItDiag.getValue().flush();
              fixItDiag.emplace(TC.diagnose(witness, diag::make_decl_objc,
                                            witness->getDescriptiveKind()));
            }
            if (!witness->canInferObjCFromRequirement(requirement)) {
              fixDeclarationObjCName(
                 fixItDiag.getValue(), witness,
                 ObjCSelector(requirement->getASTContext(), 0,
                              cast<VarDecl>(requirement)
                                ->getObjCPropertyName()));
            }
          } else if (isa<SubscriptDecl>(witness)) {
            Optional<InFlightDiagnostic> fixItDiag =
                TC.diagnose(diagLoc,
                            isOptional
                              ? diag::witness_non_objc_storage_optional
                              : diag::witness_non_objc_storage,
                            /*isSubscript=*/true,
                            witness->getFullName(),
                            Proto->getFullName());
            if (diagLoc != witness->getLoc()) {
              // If the main diagnostic is emitted on the conformance, we want
              // to attach the fix-it to the note that shows where the
              // witness is defined.
              fixItDiag.getValue().flush();
              fixItDiag.emplace(TC.diagnose(witness, diag::make_decl_objc,
                                            witness->getDescriptiveKind()));
            }
            fixItDiag->fixItInsert(witness->getAttributeInsertionLoc(false),
                                   "@objc ");
          }

          // If the requirement is optional, @nonobjc suppresses the
          // diagnostic.
          if (isOptional) {
            TC.diagnose(witness, diag::req_near_match_nonobjc, false)
              .fixItInsert(witness->getAttributeInsertionLoc(false),
                           "@nonobjc ");
          }

          TC.diagnose(requirement, diag::kind_declname_declared_here,
                      DescriptiveDeclKind::Requirement,
                      requirement->getFullName());

          Conformance->setInvalid();
          return;
        }

        // The selectors must coincide.
        if (checkObjCWitnessSelector(TC, requirement, witness)) {
          Conformance->setInvalid();
          return;
        }

        // If the @objc on the witness was inferred using the deprecated
        // Swift 3 rules, warn if asked.
        if (auto attr = witness->getAttrs().getAttribute<ObjCAttr>()) {
          if (attr->isSwift3Inferred() &&
              TC.Context.LangOpts.WarnSwift3ObjCInference
                == Swift3ObjCInferenceWarnings::Minimal) {
            TC.diagnose(Conformance->getLoc(),
                        diag::witness_swift3_objc_inference,
                        witness->getDescriptiveKind(), witness->getFullName(),
                        Conformance->getProtocol()->getDeclaredInterfaceType());
            TC.diagnose(witness, diag::make_decl_objc,
                        witness->getDescriptiveKind())
              .fixItInsert(witness->getAttributeInsertionLoc(false),
                           "@objc ");
          }
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

    // If this is an accessor for a storage decl, ignore it.
    if (isa<AccessorDecl>(requirement))
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

  // Except in specific hardcoded cases for Foundation/Swift
  // standard library compatibility, an _ObjectiveCBridgeable
  // conformance must appear in the same module as the definition of
  // the conforming type.
  //
  // Note that we check the module name to smooth over the difference
  // between an imported Objective-C module and its overlay.
  if (Proto->isSpecificProtocol(KnownProtocolKind::ObjectiveCBridgeable)) {
    auto nominal = Adoptee->getAnyNominal();
    if (!TC.Context.isTypeBridgedInExternalModule(nominal)) {
      auto clangLoader = TC.Context.getClangModuleLoader();
      if (nominal->getParentModule() != DC->getParentModule() &&
          !(clangLoader &&
            clangLoader->isInOverlayModuleForImportedModule(DC, nominal))) {
        auto nominalModule = nominal->getParentModule();
        TC.diagnose(Loc, diag::nonlocal_bridged_to_objc, nominal->getName(),
                    Proto->getName(), nominalModule->getName());
      }
    }
  }
}

static void diagnoseConformanceFailure(Type T,
                                       ProtocolDecl *Proto,
                                       DeclContext *DC,
                                       SourceLoc ComplainLoc) {
  if (T->hasError())
    return;

  ASTContext &ctx = DC->getASTContext();
  auto &diags = ctx.Diags;

  // If we're checking conformance of an existential type to a protocol,
  // do a little bit of extra work to produce a better diagnostic.
  if (T->isExistentialType() &&
      TypeChecker::containsProtocol(T, Proto, DC, None)) {

    if (!T->isObjCExistentialType()) {
      diags.diagnose(ComplainLoc, diag::protocol_does_not_conform_objc,
                     T, Proto->getDeclaredType());
      return;
    }

    diags.diagnose(ComplainLoc, diag::protocol_does_not_conform_static,
                   T, Proto->getDeclaredType());
    return;
  }

  // Special case: diagnose conversion to ExpressibleByNilLiteral, since we
  // know this is something involving 'nil'.
  if (Proto->isSpecificProtocol(KnownProtocolKind::ExpressibleByNilLiteral)) {
    diags.diagnose(ComplainLoc, diag::cannot_use_nil_with_this_type, T);
    return;
  }

  // Special case: for enums with a raw type, explain that the failing
  // conformance to RawRepresentable was inferred.
  if (auto enumDecl = T->getEnumOrBoundGenericEnum()) {
    if (Proto->isSpecificProtocol(KnownProtocolKind::RawRepresentable) &&
        DerivedConformance::derivesProtocolConformance(DC, enumDecl,
                                                       Proto) &&
        enumDecl->hasRawType() &&
        !enumDecl->getRawType()->is<ErrorType>()) {

      auto rawType = enumDecl->getRawType();

      diags.diagnose(enumDecl->getInherited()[0].getSourceRange().Start,
                     diag::enum_raw_type_nonconforming_and_nonsynthable,
                     T, rawType);

      // If the reason is that the raw type does not conform to
      // Equatable, say so.
      auto equatableProto = ctx.getProtocol(KnownProtocolKind::Equatable);
      if (!equatableProto)
        return;

      if (!TypeChecker::conformsToProtocol(rawType, equatableProto, enumDecl,
                                           None)) {
        SourceLoc loc = enumDecl->getInherited()[0].getSourceRange().Start;
        diags.diagnose(loc, diag::enum_raw_type_not_equatable, rawType);
        return;
      }

      return;
    }
  }

  diags.diagnose(ComplainLoc, diag::type_does_not_conform,
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
    diagnoseConformanceFailure(Adoptee, Proto, DC, Loc);
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
    auto layout = T->getExistentialLayout();

    // First, if we have a superclass constraint, the class may conform
    // concretely.
    if (layout.explicitSuperclass) {
      if (auto result = conformsToProtocol(layout.explicitSuperclass, Proto,
                                           DC, options)) {
        return result;
      }
    }

    // Next, check if the existential contains the protocol in question.
    for (auto P : layout.getProtocols()) {
      auto *PD = P->getDecl();

      // If we found the protocol we're looking for, return an abstract
      // conformance to it.
      if (PD == Proto)
        return ProtocolConformanceRef(Proto);

      // If the protocol has a superclass constraint, we might conform
      // concretely.
      if (auto superclass = PD->getSuperclass()) {
        if (auto result = conformsToProtocol(superclass, Proto,
                                             DC, options)) {
          return result;
        }
      }

      // Now check refined protocols.
      if (PD->inheritsFrom(Proto))
        return ProtocolConformanceRef(Proto);
    }

    return None;
  }

  // For non-existential types, this is equivalent to checking conformance.
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
  auto lookupResult = M->lookupConformance(T, Proto);
  if (!lookupResult) {
    if (ComplainLoc.isValid())
      diagnoseConformanceFailure(T, Proto, DC, ComplainLoc);
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
  if (options.contains(ConformanceCheckFlags::Used)) {
    if (auto lazyResolver = DC->getASTContext().getLazyResolver())
      lazyResolver->markConformanceUsed(*lookupResult, DC);
  }

  auto condReqs = lookupResult->getConditionalRequirementsIfAvailable();
  // If we have a conditional requirements that
  // we need to check, do so now.
  if (!condReqs) {
    assert(
        options.contains(
            ConformanceCheckFlags::AllowUnavailableConditionalRequirements) &&
        "unhandled recursion: missing conditional requirements when they're "
        "required");
  } else if (!condReqs->empty() &&
             !options.contains(
                 ConformanceCheckFlags::SkipConditionalRequirements)) {
    // Figure out the location of the conditional conformance.
    auto conformanceDC = lookupResult->getConcrete()->getDeclContext();
    SourceLoc noteLoc;
    if (auto ext = dyn_cast<ExtensionDecl>(conformanceDC))
      noteLoc = ext->getLoc();
    else
      noteLoc = cast<NominalTypeDecl>(conformanceDC)->getLoc();

    auto conditionalCheckResult = checkGenericArguments(
        DC, ComplainLoc, noteLoc, T,
        {lookupResult->getRequirement()->getSelfInterfaceType()},
        *condReqs,
        [](SubstitutableType *dependentType) { return Type(dependentType); },
        LookUpConformance(DC), options);
    switch (conditionalCheckResult) {
    case RequirementCheckResult::Success:
      break;

    case RequirementCheckResult::Failure:
    case RequirementCheckResult::SubstitutionFailure:
      return None;
    }
  }

  // When requested, print the conformance access path used to find this
  // conformance.
  ASTContext &ctx = Proto->getASTContext();
  if (ctx.LangOpts.DebugGenericSignatures &&
      InExpression && T->is<ArchetypeType>() && lookupResult->isAbstract() &&
      !T->castTo<ArchetypeType>()->isOpenedExistential() &&
      !T->castTo<ArchetypeType>()->requiresClass() &&
      T->castTo<ArchetypeType>()->getGenericEnvironment()
        == DC->getGenericEnvironmentOfContext()) {
    auto interfaceType = T->mapTypeOutOfContext();
    if (interfaceType->isTypeParameter()) {
      auto genericSig = DC->getGenericSignatureOfContext();
      auto path = genericSig->getConformanceAccessPath(interfaceType, Proto);

      // Debugging aid: display the conformance access path for archetype
      // conformances.
      llvm::errs() << "Conformance access path for ";
      T.print(llvm::errs());
      llvm::errs() << ": " << Proto->getName() << " is ";
      path.print(llvm::errs());
      llvm::errs() << "\n";
    }
  }

  return lookupResult;
}

void TypeChecker::markConformanceUsed(ProtocolConformanceRef conformance,
                                      DeclContext *dc) {
  if (conformance.isAbstract()) return;

  auto normalConformance =
    conformance.getConcrete()->getRootNormalConformance();

  // Make sure that the type checker completes this conformance.
  if (normalConformance->isIncomplete())
    UsedConformances.insert(normalConformance);

  // Record the usage of this conformance in the enclosing source
  // file.
  if (auto sf = dc->getParentSourceFile()) {
    sf->addUsedConformance(normalConformance);
  }
}

Optional<ProtocolConformanceRef>
TypeChecker::LookUpConformance::operator()(
                                       CanType dependentType,
                                       Type conformingReplacementType,
                                       ProtocolDecl *conformedProtocol) const {
  if (conformingReplacementType->isTypeParameter())
    return ProtocolConformanceRef(conformedProtocol);

  return TypeChecker::conformsToProtocol(
                         conformingReplacementType,
                         conformedProtocol,
                         dc,
                         (ConformanceCheckFlags::Used|
                          ConformanceCheckFlags::InExpression|
                          ConformanceCheckFlags::SkipConditionalRequirements));
}

/// Mark any _ObjectiveCBridgeable conformances in the given type as "used".
///
/// These conformances might not appear in any substitution lists produced
/// by Sema, since bridging is done at the SILGen level, so we have to
/// force them here to ensure SILGen can find them.
void swift::useObjectiveCBridgeableConformances(DeclContext *dc, Type type) {
  class Walker : public TypeWalker {
    ASTContext &Ctx;
    DeclContext *DC;
    ProtocolDecl *Proto;

  public:
    Walker(DeclContext *dc, ProtocolDecl *proto)
      : Ctx(dc->getASTContext()), DC(dc), Proto(proto) { }

    Action walkToTypePre(Type ty) override {
      ConformanceCheckOptions options =
          (ConformanceCheckFlags::InExpression |
           ConformanceCheckFlags::Used |
           ConformanceCheckFlags::SuppressDependencyTracking);

      // If we have a nominal type, "use" its conformance to
      // _ObjectiveCBridgeable if it has one.
      if (auto *nominalDecl = ty->getAnyNominal()) {
        if (isa<ClassDecl>(nominalDecl) || isa<ProtocolDecl>(nominalDecl))
          return Action::Continue;

        auto lazyResolver = Ctx.getLazyResolver();
        assert(lazyResolver &&
               "Cannot do conforms-to-protocol check without a type checker");
        TypeChecker &tc = *static_cast<TypeChecker *>(lazyResolver);
        (void)tc.conformsToProtocol(ty, Proto, DC, options,
                                    /*ComplainLoc=*/SourceLoc());

        // Set and Dictionary bridging also requires the conformance
        // of the key type to Hashable.
        if (nominalDecl == Ctx.getSetDecl() ||
            nominalDecl == Ctx.getDictionaryDecl()) {
          if (auto boundGeneric = ty->getAs<BoundGenericType>()) {
            auto args = boundGeneric->getGenericArgs();
            if (!args.empty()) {
              auto keyType = args[0];
              auto *hashableProto =
                Ctx.getProtocol(KnownProtocolKind::Hashable);
              if (!hashableProto)
                return Action::Stop;

              (void)tc.conformsToProtocol(
                  keyType, hashableProto, DC, options,
                  /*ComplainLoc=*/SourceLoc());
            }
          }
        }
      }

      return Action::Continue;
    }
  };

  auto proto =
    dc->getASTContext().getProtocol(KnownProtocolKind::ObjectiveCBridgeable);
  if (!proto) return;

  Walker walker(dc, proto);
  type.walk(walker);
}

void swift::useObjectiveCBridgeableConformancesOfArgs(
       DeclContext *dc, BoundGenericType *bound) {
  ASTContext &ctx = dc->getASTContext();
  auto proto = ctx.getProtocol(KnownProtocolKind::ObjectiveCBridgeable);
  if (!proto) return;

  // Check whether the bound generic type itself is bridged to
  // Objective-C.
  ConformanceCheckOptions options =
    (ConformanceCheckFlags::InExpression |
     ConformanceCheckFlags::SuppressDependencyTracking);
  auto lazyResolver = ctx.getLazyResolver();
  assert(lazyResolver && "Need a type checker to check conforms-to-protocol");
  auto &tc = *static_cast<TypeChecker *>(lazyResolver);
  (void)tc.conformsToProtocol(
      bound->getDecl()->getDeclaredType(), proto, dc,
      options, /*ComplainLoc=*/SourceLoc());
}

void TypeChecker::useBridgedNSErrorConformances(DeclContext *dc, Type type) {
  auto errorProto = Context.getProtocol(KnownProtocolKind::Error);
  auto bridgedStoredNSError = Context.getProtocol(
                                    KnownProtocolKind::BridgedStoredNSError);
  auto errorCodeProto = Context.getProtocol(
                              KnownProtocolKind::ErrorCodeProtocol);
  auto rawProto = Context.getProtocol(
                        KnownProtocolKind::RawRepresentable);

  if (!errorProto || !bridgedStoredNSError || !errorCodeProto || !rawProto)
    return;

  // The NSError: Error conformance.
  if (auto nsError = Context.getNSErrorDecl()) {
    validateDecl(nsError);
    (void)conformsToProtocol(nsError->TypeDecl::getDeclaredInterfaceType(),
                             errorProto, dc, ConformanceCheckFlags::Used);
  }

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
  PrettyStackTraceType trace1(Context, "checking conformance of",
                              conformance->getType());
  PrettyStackTraceDecl trace2("...to", conformance->getProtocol());

  MultiConformanceChecker checker(*this);
  checker.addConformance(conformance);
  checker.checkAllConformances();
}

void TypeChecker::checkConformanceRequirements(
                                     NormalProtocolConformance *conformance) {
  // If the conformance is already invalid, there's nothing to do here.
  if (conformance->isInvalid())
    return;

  PrettyStackTraceType trace1(Context, "checking conformance requirements of",
                              conformance->getType());
  PrettyStackTraceDecl trace2("...to", conformance->getProtocol());

  conformance->setSignatureConformances({ });

  llvm::SetVector<ValueDecl *> globalMissingWitnesses;
  ConformanceChecker checker(*this, conformance, globalMissingWitnesses);
  checker.ensureRequirementsAreSatisfied(/*failUnsubstituted=*/true);
  checker.diagnoseMissingWitnesses(MissingWitnessDiagnosisKind::ErrorFixIt);
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
  if (!lhs.isSpecial() && !rhs.isSpecial()) {
    if (lhs.getArgumentNames().empty() || !isFunc) {
      score = scoreIdentifiers(lhs.getBaseIdentifier(), rhs.getBaseIdentifier(),
                               limit);
    } else {
      llvm::SmallString<16> lhsScratch;
      StringRef lhsFirstName =
        combineBaseNameAndFirstArgument(lhs.getBaseIdentifier(),
                                        lhs.getArgumentNames()[0],
                                        lhsScratch);

      llvm::SmallString<16> rhsScratch;
      StringRef rhsFirstName =
        combineBaseNameAndFirstArgument(rhs.getBaseIdentifier(),
                                        rhs.getArgumentNames()[0],
                                        rhsScratch);

      score = lhsFirstName.edit_distance(rhsFirstName.str(), true, limit);
    }
  } else {
    if (lhs.getBaseName().getKind() == rhs.getBaseName().getKind()) {
      score = 0;
    } else {
      return None;
    }
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

  // For @objc protocols, apply the omit-needless-words heuristics to
  // both names.
  if (cast<ProtocolDecl>(req->getDeclContext())->isObjC()) {
    if (auto adjustedReqName = ::omitNeedlessWords(tc, req))
      reqName = *adjustedReqName;
    if (auto adjustedWitnessName = ::omitNeedlessWords(tc, witness))
      witnessName = *adjustedWitnessName;
  }

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
  if (!name.getBaseName().empty() && !name.getBaseName().isSpecial())
    length += name.getBaseIdentifier().str().size();
  for (auto arg : name.getArgumentNames()) {
    if (!arg.empty())
      length += arg.str().size();
  }
  return length;
}

/// Determine whether a particular declaration is generic.
static bool isGeneric(ValueDecl *decl) {
  if (auto func = dyn_cast<AbstractFunctionDecl>(decl))
    return func->isGeneric();
  if (auto subscript = dyn_cast<SubscriptDecl>(decl))
    return subscript->isGeneric();
  return false;
}

/// Determine whether this is an unlabeled initializer or subscript.
static bool isUnlabeledInitializerOrSubscript(ValueDecl *value) {
  ParameterList *paramList = nullptr;
  if (auto constructor = dyn_cast<ConstructorDecl>(value))
    paramList = constructor->getParameters();
  else if (auto subscript = dyn_cast<SubscriptDecl>(value))
    paramList = subscript->getIndices();
  else
    return false;

  for (auto param : *paramList) {
    if (!param->getArgumentName().empty()) return false;
  }

  return true;
}

/// Determine whether this declaration is an initializer
/// Determine whether we should warn about the given witness being a close
/// match for the given optional requirement.
static bool shouldWarnAboutPotentialWitness(
                                      MultiConformanceChecker &groupChecker,
                                      ValueDecl *req,
                                      ValueDecl *witness,
                                      AccessLevel access,
                                      unsigned score) {
  // If the witness is covered, don't warn about it.
  if (groupChecker.isCoveredMember(witness))
    return false;

  // If the kinds of the requirement and witness are different, there's
  // nothing to warn about.
  if (req->getKind() != witness->getKind())
    return false;

  // If the warning couldn't be suppressed, don't warn.
  if (!canSuppressPotentialWitnessWarningWithMovement(req, witness) &&
      !canSuppressPotentialWitnessWarningWithNonObjC(req, witness))
    return false;

  // If the potential witness for an @objc requirement is already
  // marked @nonobjc, don't warn.
  if (req->isObjC() && witness->getAttrs().hasAttribute<NonObjCAttr>())
    return false;

  // If the witness is generic and requirement is not, or vice-versa,
  // don't warn.
  if (isGeneric(req) != isGeneric(witness))
    return false;

  // Don't warn if the potential witness has been explicitly given less
  // visibility than the conformance.
  if (witness->getFormalAccess() < access) {
    if (auto attr = witness->getAttrs().getAttribute<AccessControlAttr>())
      if (!attr->isImplicit()) return false;
  }

  // Don't warn if the requirement or witness is an initializer or subscript
  // with no argument labels.
  if (isUnlabeledInitializerOrSubscript(req) ||
      isUnlabeledInitializerOrSubscript(witness))
    return false;

  // For non-@objc requirements, only warn if the witness comes from an
  // extension.
  if (!req->isObjC() && !isa<ExtensionDecl>(witness->getDeclContext()))
    return false;

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
                                     AccessLevel access) {
  auto proto = cast<ProtocolDecl>(req->getDeclContext());

  // Primary warning.
  tc.diagnose(witness, diag::req_near_match,
              witness->getDescriptiveKind(),
              witness->getFullName(),
              req->getAttrs().hasAttribute<OptionalAttr>(),
              req->getFullName(),
              proto->getFullName());

  // Describe why the witness didn't satisfy the requirement.
  WitnessChecker::RequirementEnvironmentCache oneUseCache;
  auto dc = conformance->getDeclContext();
  auto match = matchWitness(tc, oneUseCache, conformance->getProtocol(),
                            conformance, dc, req, witness);
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
    tc.diagnose(witness, diag::req_near_match_move,
                witness->getFullName(), static_cast<unsigned>(*move));
  }

  // If adding 'private', 'fileprivate', or 'internal' can help, suggest that.
  if (access > AccessLevel::FilePrivate &&
      !witness->getAttrs().hasAttribute<AccessControlAttr>()) {
    tc.diagnose(witness, diag::req_near_match_access,
                witness->getFullName(), access)
      .fixItInsert(witness->getAttributeInsertionLoc(true), "private ");
  }

  // If adding @nonobjc can help, suggest that.
  if (canSuppressPotentialWitnessWarningWithNonObjC(req, witness)) {
    tc.diagnose(witness, diag::req_near_match_nonobjc, false)
      .fixItInsert(witness->getAttributeInsertionLoc(false), "@nonobjc ");
  }

  tc.diagnose(req, diag::kind_declname_declared_here,
              DescriptiveDeclKind::Requirement, req->getFullName());
}

/// Whether the given protocol is "NSCoding".
static bool isNSCoding(ProtocolDecl *protocol) {
  ASTContext &ctx = protocol->getASTContext();
  return protocol->getModuleContext()->getName() == ctx.Id_Foundation &&
    protocol->getName().str().equals("NSCoding");
}

/// Whether the given class has an explicit '@objc' name.
static bool hasExplicitObjCName(ClassDecl *classDecl) {
  if (classDecl->getAttrs().hasAttribute<ObjCRuntimeNameAttr>())
    return true;

  auto objcAttr = classDecl->getAttrs().getAttribute<ObjCAttr>();
  if (!objcAttr) return false;

  return objcAttr->hasName() && !objcAttr->isNameImplicit();
}

/// Determine whether a particular class has generic ancestry.
static bool hasGenericAncestry(ClassDecl *classDecl) {
  SmallPtrSet<ClassDecl *, 4> visited;
  while (classDecl && visited.insert(classDecl).second) {
    if (classDecl->isGenericContext())
      return true;

    classDecl = classDecl->getSuperclassDecl();
  }

  return false;
}

/// Infer the attribute tostatic-initialize the Objective-C metadata for the
/// given class, if needed.
static void inferStaticInitializeObjCMetadata(TypeChecker &tc,
                                              ClassDecl *classDecl) {
  // If we already have the attribute, there's nothing to do.
  if (classDecl->getAttrs().hasAttribute<StaticInitializeObjCMetadataAttr>())
    return;

  // If we know that the Objective-C metadata will be statically registered,
  // there's nothing to do.
  if (!hasGenericAncestry(classDecl)) {
    return;
  }

  // If this class isn't always available on the deployment target, don't
  // mark it as statically initialized.
  // FIXME: This is a workaround. The proper solution is for IRGen to
  // only statically initializae the Objective-C metadata when running on
  // a new-enough OS.
  if (auto sourceFile = classDecl->getParentSourceFile()) {
    AvailabilityContext availableInfo = AvailabilityContext::alwaysAvailable();
    for (Decl *enclosingDecl = classDecl; enclosingDecl;
         enclosingDecl = enclosingDecl->getDeclContext()
                           ->getInnermostDeclarationDeclContext()) {
      if (!tc.isDeclAvailable(enclosingDecl, SourceLoc(), sourceFile,
                              availableInfo))
        return;
    }
  }

  // Infer @_staticInitializeObjCMetadata.
  ASTContext &ctx = classDecl->getASTContext();
  classDecl->getAttrs().add(
            new (ctx) StaticInitializeObjCMetadataAttr(/*implicit=*/true));
}

void TypeChecker::checkConformancesInContext(DeclContext *dc,
                                             IterableDeclContext *idc) {
  // For anything imported from Clang, lazily check conformances.
  if (isa<ClangModuleUnit>(dc->getModuleScopeContext()))
    return;

  // Determine the access level of this conformance.
  Decl *currentDecl = nullptr;
  AccessLevel defaultAccess;
  if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
    const NominalTypeDecl *nominal = ext->getExtendedNominal();
    if (!nominal)
      return;
    defaultAccess = nominal->getFormalAccess();
    currentDecl = ext;
  } else {
    defaultAccess = cast<NominalTypeDecl>(dc)->getFormalAccess();
    currentDecl = cast<NominalTypeDecl>(dc);
  }

  SourceFile *SF = dc->getParentSourceFile();
  ReferencedNameTracker *tracker = nullptr;
  if (SF)
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
                             defaultAccess > AccessLevel::FilePrivate);

    // Diagnose @NSCoding on file/fileprivate/nested/generic classes, which
    // have unstable archival names.
    if (auto classDecl = dc->getSelfClassDecl()) {
      if (Context.LangOpts.EnableObjCInterop &&
          isNSCoding(conformance->getProtocol()) &&
          !classDecl->isGenericContext()) {
        // Note: these 'kind' values are synchronized with
        // diag::nscoding_unstable_mangled_name.
        enum class UnstableNameKind : unsigned {
          Private = 0,
          FilePrivate,
          Nested,
          Local,
        };
        Optional<UnstableNameKind> kind;
        if (!classDecl->getDeclContext()->isModuleScopeContext()) {
          if (classDecl->getDeclContext()->isTypeContext())
            kind = UnstableNameKind::Nested;
          else
            kind = UnstableNameKind::Local;
        } else {
          switch (classDecl->getFormalAccess()) {
          case AccessLevel::FilePrivate:
            kind = UnstableNameKind::FilePrivate;
            break;

          case AccessLevel::Private:
            kind = UnstableNameKind::Private;
            break;

          case AccessLevel::Internal:
          case AccessLevel::Open:
          case AccessLevel::Public:
            break;
          }
        }

        if (kind && getLangOpts().EnableNSKeyedArchiverDiagnostics &&
            isa<NormalProtocolConformance>(conformance) &&
            !hasExplicitObjCName(classDecl)) {
          diagnose(cast<NormalProtocolConformance>(conformance)->getLoc(),
                   diag::nscoding_unstable_mangled_name,
                   static_cast<unsigned>(kind.getValue()),
                   classDecl->getDeclaredInterfaceType());
          auto insertionLoc =
            classDecl->getAttributeInsertionLoc(/*forModifier=*/false);
          // Note: this is intentionally using the Swift 3 mangling,
          // to provide compatibility with archives created in the Swift 3
          // time frame.
          Mangle::ASTMangler mangler;
          std::string mangledName = mangler.mangleObjCRuntimeName(classDecl);
          assert(Lexer::isIdentifier(mangledName) &&
                 "mangled name is not an identifier; can't use @objc");
          diagnose(classDecl, diag::unstable_mangled_name_add_objc)
            .fixItInsert(insertionLoc,
                         "@objc(" + mangledName + ")");
          diagnose(classDecl, diag::unstable_mangled_name_add_objc_new)
            .fixItInsert(insertionLoc,
                         "@objc(<#prefixed Objective-C class name#>)");
        }

        // Infer @_staticInitializeObjCMetadata if needed.
        inferStaticInitializeObjCMetadata(*this, classDecl);
      }
    }
  }

  // Check all conformances.
  groupChecker.checkAllConformances();

  if (Context.LangOpts.DebugGenericSignatures) {
    // Now that they're filled out, print out information about the conformances
    // here, when requested.
    for (auto conformance : conformances) {
      dc->dumpContext();
      conformance->dump();
    }
  }

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

    // Complain about the redundant conformance.

    auto currentSig = dc->getGenericSignatureOfContext();
    auto existingSig = diag.ExistingDC->getGenericSignatureOfContext();
    auto differentlyConditional = currentSig && existingSig &&
                                  currentSig->getCanonicalSignature() !=
                                      existingSig->getCanonicalSignature();

    // If we've redundantly stated a conformance for which the original
    // conformance came from the module of the type or the module of the
    // protocol, just warn; we'll pick up the original conformance.
    auto existingModule = diag.ExistingDC->getParentModule();
    auto extendedNominal = diag.ExistingDC->getSelfNominalTypeDecl();
    if (existingModule != dc->getParentModule() &&
        (existingModule->getName() ==
           extendedNominal->getParentModule()->getName() ||
         existingModule == diag.Protocol->getParentModule())) {
      // Warn about the conformance.
      auto diagID = differentlyConditional
                        ? diag::redundant_conformance_adhoc_conditional
                        : diag::redundant_conformance_adhoc;
      diagnose(diag.Loc, diagID, dc->getDeclaredInterfaceType(),
               diag.Protocol->getName(),
               existingModule->getName() ==
                   extendedNominal->getParentModule()->getName(),
               existingModule->getName());

      // Complain about any declarations in this extension whose names match
      // a requirement in that protocol.
      SmallPtrSet<DeclName, 4> diagnosedNames;
      for (auto decl : idc->getMembers()) {
        if (decl->isImplicit())
          continue;

        auto value = dyn_cast<ValueDecl>(decl);
        if (!value) continue;

        if (!diagnosedNames.insert(value->getFullName()).second)
          continue;

        bool valueIsType = isa<TypeDecl>(value);
        auto flags = OptionSet<NominalTypeDecl::LookupDirectFlags>();
        flags |= NominalTypeDecl::LookupDirectFlags::IgnoreNewExtensions;
        for (auto requirement
                : diag.Protocol->lookupDirect(value->getFullName(), flags)) {
          auto requirementIsType = isa<TypeDecl>(requirement);
          if (valueIsType != requirementIsType)
            continue;

          diagnose(value, diag::redundant_conformance_witness_ignored,
                   value->getDescriptiveKind(), value->getFullName(),
                   diag.Protocol->getFullName());
          break;
        }
      }
    } else {
      auto diagID = differentlyConditional
                        ? diag::redundant_conformance_conditional
                        : diag::redundant_conformance;
      diagnose(diag.Loc, diagID, dc->getDeclaredInterfaceType(),
               diag.Protocol->getName());
    }

    // Special case: explain that 'RawRepresentable' conformance
    // is implied for enums which already declare a raw type.
    if (auto enumDecl = dyn_cast<EnumDecl>(existingDecl)) {
      if (diag.Protocol->isSpecificProtocol(
              KnownProtocolKind::RawRepresentable) &&
          DerivedConformance::derivesProtocolConformance(dc, enumDecl,
                                                         diag.Protocol) &&
          enumDecl->hasRawType() &&
          enumDecl->getInherited()[0].getSourceRange().isValid()) {
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
    if (SF && SF->Kind != SourceFileKind::Interface) {
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
                return !shouldWarnAboutPotentialWitness(groupChecker, req,
                                                        value, defaultAccess,
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
                                       req, value, defaultAccess);
              diagnosed = true;
              break;
            }
          }
          assert(diagnosed && "Failed to find conformance to diagnose?");
          (void)diagnosed;

          // Remove this requirement from the list. We don't want to
          // complain about it twice.
          unsatisfiedReqs.erase(std::find(unsatisfiedReqs.begin(),
                                          unsatisfiedReqs.end(),
                                          req));
        }
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
swift::findWitnessedObjCRequirements(const ValueDecl *witness,
                                     bool anySingleRequirement) {
  llvm::TinyPtrVector<ValueDecl *> result;

  // Types don't infer @objc this way.
  if (isa<TypeDecl>(witness)) return result;

  auto dc = witness->getDeclContext();
  auto nominal = dc->getSelfNominalTypeDecl();
  if (!nominal) return result;

  DeclName name = witness->getFullName();
  Optional<AccessorKind> accessorKind;
  if (auto *accessor = dyn_cast<AccessorDecl>(witness)) {
    accessorKind = accessor->getAccessorKind();
    switch (*accessorKind) {
    case AccessorKind::Address:
    case AccessorKind::MutableAddress:
    case AccessorKind::Read:
    case AccessorKind::Modify:
      // These accessors are never exposed to Objective-C.
      return result;
    case AccessorKind::DidSet:
    case AccessorKind::WillSet:
      // These accessors are folded into the setter.
      return result;
    case AccessorKind::Get:
    case AccessorKind::Set:
      // These are found relative to the main decl.
      name = accessor->getStorage()->getFullName();
      break;
    }
  }

  WitnessChecker::RequirementEnvironmentCache reqEnvCache;
  ASTContext &ctx = nominal->getASTContext();
  for (auto proto : nominal->getAllProtocols()) {
    // We only care about Objective-C protocols.
    if (!proto->isObjC()) continue;

    Optional<ProtocolConformance *> conformance;
    auto flags = OptionSet<NominalTypeDecl::LookupDirectFlags>();
    flags |= NominalTypeDecl::LookupDirectFlags::IgnoreNewExtensions;
    for (auto req : proto->lookupDirect(name, flags)) {
      // Skip anything in a protocol extension.
      if (req->getDeclContext() != proto) continue;

      // Skip types.
      if (isa<TypeDecl>(req)) continue;

      // Skip unavailable requirements.
      if (req->getAttrs().isUnavailable(ctx)) continue;

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

      const Decl *found = (*conformance)->getWitnessDecl(req, nullptr);

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
        if (accessorKind)
          witnessToMatch = cast<AccessorDecl>(witness)->getStorage();

        auto lazyResolver = ctx.getLazyResolver();
        assert(lazyResolver && "Need a type checker to match witnesses");
        auto &tc = *static_cast<TypeChecker *>(lazyResolver);
        if (matchWitness(tc, reqEnvCache, proto, *conformance,
                         witnessToMatch->getDeclContext(), req,
                         const_cast<ValueDecl *>(witnessToMatch))
              .Kind == MatchKind::ExactMatch) {
          if (accessorKind) {
            auto *storageReq = dyn_cast<AbstractStorageDecl>(req);
            if (!storageReq)
              continue;
            req = storageReq->getAccessor(*accessorKind);
            if (!req)
              continue;
          }
          result.push_back(req);
          if (anySingleRequirement) return result;
          continue;
        }

        continue;
      }

      // Dig out the appropriate accessor, if necessary.
      if (accessorKind) {
        auto *storageReq = dyn_cast<AbstractStorageDecl>(req);
        auto *storageFound = dyn_cast_or_null<AbstractStorageDecl>(found);
        if (!storageReq || !storageFound)
          continue;
        req = storageReq->getAccessor(*accessorKind);
        if (!req)
          continue;
        found = storageFound->getAccessor(*accessorKind);
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
                       return TypeDecl::compare(lhsProto, rhsProto) < 0;
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
  if (!assocType)
    checker.resolveTypeWitnesses();
  else
    checker.resolveSingleTypeWitness(assocType);
  checker.diagnoseMissingWitnesses(MissingWitnessDiagnosisKind::ErrorFixIt);
}

void TypeChecker::resolveWitness(const NormalProtocolConformance *conformance,
                                 ValueDecl *requirement) {
  llvm::SetVector<ValueDecl*> MissingWitnesses;
  ConformanceChecker checker(
                       *this, 
                       const_cast<NormalProtocolConformance*>(conformance),
                       MissingWitnesses);
  checker.resolveSingleWitness(requirement);
  checker.diagnoseMissingWitnesses(MissingWitnessDiagnosisKind::ErrorFixIt);
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
  if (Decl->isInvalid())
    return nullptr;

  DerivedConformance derived(*this, Decl, TypeDecl, protocol);

  switch (*knownKind) {
  case KnownProtocolKind::RawRepresentable:
    return derived.deriveRawRepresentable(Requirement);

  case KnownProtocolKind::CaseIterable:
    return derived.deriveCaseIterable(Requirement);

  case KnownProtocolKind::Equatable:
    return derived.deriveEquatable(Requirement);

  case KnownProtocolKind::Hashable:
    return derived.deriveHashable(Requirement);

  case KnownProtocolKind::BridgedNSError:
    return derived.deriveBridgedNSError(Requirement);

  case KnownProtocolKind::CodingKey:
    return derived.deriveCodingKey(Requirement);

  case KnownProtocolKind::Encodable:
    return derived.deriveEncodable(Requirement);

  case KnownProtocolKind::Decodable:
    return derived.deriveDecodable(Requirement);

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

  DerivedConformance derived(*this, Decl, TypeDecl, protocol);
  switch (*knownKind) {
  case KnownProtocolKind::RawRepresentable:
    return derived.deriveRawRepresentable(AssocType);
  case KnownProtocolKind::CaseIterable:
    return derived.deriveCaseIterable(AssocType);
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
    void recordWitness(ValueDecl *requirement, const RequirementMatch &match);
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

  if (findBestWitness(
                 requirement, nullptr, nullptr,
                 /* out parameters: */
                 matches, numViable, bestIdx, doNotDiagnoseMatches)) {

    auto &best = matches[bestIdx];

    // Perform the same checks as conformance witness matching, but silently
    // ignore the candidate instead of diagnosing anything.
    auto check = checkWitness(AccessScope::getPublic(), requirement, best);
    if (check.Kind != CheckKind::Success)
      return ResolveWitnessResult::ExplicitFailed;

    // Record the match.
    recordWitness(requirement, best);
    return ResolveWitnessResult::Success;
  }

  // We have either no matches or an ambiguous match.
  return ResolveWitnessResult::Missing;
}

void DefaultWitnessChecker::recordWitness(
                                  ValueDecl *requirement,
                                  const RequirementMatch &match) {
  Proto->setDefaultWitness(requirement,
                           match.getWitness(TC.Context));

  // Synthesize accessors for the protocol witness table to use.
  if (auto storage = dyn_cast<AbstractStorageDecl>(match.Witness))
    TC.synthesizeWitnessAccessorsForStorage(
                                        cast<AbstractStorageDecl>(requirement),
                                        storage);
}

void TypeChecker::inferDefaultWitnesses(ProtocolDecl *proto) {
  DefaultWitnessChecker checker(*this, proto);

  // Find the default for the given associated type.
  auto findAssociatedTypeDefault =
      [&](AssociatedTypeDecl *assocType,
      AssociatedTypeDecl **defaultedAssocTypeOut = nullptr) -> Type {
    auto defaultedAssocType =
      AssociatedTypeInference::findDefaultedAssociatedType(*this, assocType);
    if (!defaultedAssocType)
      return nullptr;;

    Type defaultType = defaultedAssocType->getDefaultDefinitionType();
    if (!defaultType)
      return nullptr;

    if (defaultedAssocTypeOut)
      *defaultedAssocTypeOut = defaultedAssocType;

    return defaultType;
  };

  for (auto *requirement : proto->getMembers()) {
    if (requirement->isInvalid())
      continue;

    auto *valueDecl = dyn_cast<ValueDecl>(requirement);
    if (!valueDecl)
      continue;

    if (auto assocType = dyn_cast<AssociatedTypeDecl>(valueDecl)) {
      if (assocType->getOverriddenDecls().empty()) {
        if (Type defaultType = findAssociatedTypeDefault(assocType))
          proto->setDefaultTypeWitness(assocType, defaultType);
      }

      continue;
    }

    if (isa<TypeDecl>(valueDecl))
      continue;

    if (!valueDecl->isProtocolRequirement())
      continue;

    checker.resolveWitnessViaLookup(valueDecl);
  }

  // Find defaults for any associated conformances rooted on defaulted
  // associated types.
  for (const auto &req : proto->getRequirementSignature()) {
    if (req.getKind() != RequirementKind::Conformance)
      continue;
    if (req.getFirstType()->isEqual(proto->getSelfInterfaceType()))
      continue;

    // Find the innermost dependent member type (e.g., Self.AssocType), so
    // we can look at the associated type.
    auto depMemTy = req.getFirstType()->getAs<DependentMemberType>();
    if (!depMemTy)
      continue;

    while (auto innerDepMemTy =
             depMemTy->getBase()->getAs<DependentMemberType>())
      depMemTy = innerDepMemTy;

    if (!depMemTy->getBase()->isEqual(proto->getSelfInterfaceType()))
      continue;

    auto assocType = depMemTy->getAssocType();
    if (!assocType)
      continue;

    // Find the associated type nearest our own protocol, which might have
    // a default not available in the associated type referenced by the
    // (canonicalized) requirement.
    if (assocType->getProtocol() != proto) {
      SmallVector<ValueDecl *, 2> found;
      proto->getModuleContext()->lookupQualified(
                           proto, assocType->getFullName(),
                           NL_QualifiedDefault|NL_ProtocolMembers|NL_OnlyTypes,
                           found);
      if (found.size() == 1 && isa<AssociatedTypeDecl>(found[0]))
        assocType = cast<AssociatedTypeDecl>(found[0]);
    }

    // Dig out the default associated type definition.
    AssociatedTypeDecl *defaultedAssocType = nullptr;
    Type defaultAssocType = findAssociatedTypeDefault(assocType,
                                                      &defaultedAssocType);
    if (!defaultAssocType)
      continue;

    Type defaultAssocTypeInContext =
      proto->mapTypeIntoContext(defaultAssocType);
    auto requirementProto =
      req.getSecondType()->castTo<ProtocolType>()->getDecl();
    auto conformance = conformsToProtocol(defaultAssocTypeInContext,
                                          requirementProto, proto,
                                          ConformanceCheckFlags::Used);
    if (!conformance) {
      // Diagnose the lack of a conformance. This is potentially an ABI
      // incompatibility.
      diagnose(proto, diag::assoc_type_default_conformance_failed,
               defaultAssocType, assocType->getFullName(), req.getFirstType(),
               req.getSecondType());
      diagnose(defaultedAssocType, diag::assoc_type_default_here,
               assocType->getFullName(), defaultAssocType)
        .highlight(
          defaultedAssocType->getDefaultDefinitionLoc().getSourceRange());

      continue;
    }

    // Record the default associated conformance.
    proto->setDefaultAssociatedConformanceWitness(
        req.getFirstType()->getCanonicalType(), requirementProto, *conformance);
  }
}

void TypeChecker::recordKnownWitness(NormalProtocolConformance *conformance,
                                     ValueDecl *req, ValueDecl *witness) {
  // Match the witness. This should never fail, but it does allow renaming
  // (because property behaviors rely on renaming).
  validateDecl(witness);
  auto dc = conformance->getDeclContext();
  WitnessChecker::RequirementEnvironmentCache oneUseCache;
  auto match = matchWitness(*this, oneUseCache, conformance->getProtocol(),
                            conformance, dc, req, witness);
  if (match.Kind != MatchKind::ExactMatch &&
      match.Kind != MatchKind::RenamedMatch) {
    diagnose(witness, diag::property_behavior_conformance_broken,
             witness->getFullName(), conformance->getType());
    return;
  }

  conformance->setWitness(req, match.getWitness(Context));
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

  return (!ty || ty->hasError()) ? Type() : ty;
}
