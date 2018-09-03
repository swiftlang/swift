//===--- AccessRequests.cpp - AccessLevel and AccessScope Requests --------===//
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

#include "swift/Subsystems.h"
#include "swift/AST/AccessScopeChecker.h"
#include "swift/AST/AccessRequests.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"

#include "llvm/Support/MathExtras.h"

using namespace swift;

namespace swift {
// Implement the access-control type zone.
#define SWIFT_TYPEID_ZONE SWIFT_ACCESSS_REQUESTS_TYPEID_ZONE
#define SWIFT_TYPEID_HEADER "swift/AST/AccessTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
}

//----------------------------------------------------------------------------//
// AccessLevel computation
//----------------------------------------------------------------------------//
llvm::Expected<AccessLevel>
AccessLevelRequest::evaluate(Evaluator &evaluator, ValueDecl *D) const {
  assert(!D->hasAccess());

  // Check if the decl has an explicit access control attribute.
  if (auto *AA = D->getAttrs().getAttribute<AccessControlAttr>())
    return AA->getAccess();

  // Special case for accessors, which inherit the access of their storage.
  // decl. A setter attribute can also override this.
  if (auto accessor = dyn_cast<AccessorDecl>(D)) {
    AbstractStorageDecl *storage = accessor->getStorage();
    switch (accessor->getAccessorKind()) {
    case AccessorKind::Get:
    case AccessorKind::Address:
    case AccessorKind::Read:
      return storage->getFormalAccess();
    case AccessorKind::Set:
    case AccessorKind::MutableAddress:
    case AccessorKind::Modify:
      return storage->getSetterFormalAccess();
    case AccessorKind::WillSet:
    case AccessorKind::DidSet:
      // These are only needed to synthesize the setter.
      return AccessLevel::Private;
    }
  }

  // Special case for associated types: inherit access from protocol.
  if (auto assocType = dyn_cast<AssociatedTypeDecl>(D)) {
    auto prot = assocType->getProtocol();
    return std::max(prot->getFormalAccess(), AccessLevel::Internal);
  }

  // Special case for dtors and enum elements: inherit from container
  if (D->getKind() == DeclKind::Destructor ||
      D->getKind() == DeclKind::EnumElement) {
    if (D->isInvalid()) {
      return AccessLevel::Private;
    } else {
      auto container = cast<NominalTypeDecl>(D->getDeclContext());
      return std::max(container->getFormalAccess(), AccessLevel::Internal);
    }
  }

  DeclContext *DC = D->getDeclContext();
  switch (DC->getContextKind()) {
  case DeclContextKind::TopLevelCodeDecl:
    // Variables declared in a top-level 'guard' statement can be accessed in
    // later top-level code.
    return AccessLevel::FilePrivate;
  case DeclContextKind::AbstractClosureExpr:
    if (isa<ParamDecl>(D)) {
      // Closure parameters may need to be accessible to the enclosing
      // context, for single-expression closures.
      return AccessLevel::FilePrivate;
    } else {
      return AccessLevel::Private;
    }
  case DeclContextKind::SerializedLocal:
  case DeclContextKind::Initializer:
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::SubscriptDecl:
    return AccessLevel::Private;
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
    return AccessLevel::Internal;
  case DeclContextKind::GenericTypeDecl: {
    auto generic = cast<GenericTypeDecl>(DC);
    AccessLevel access = AccessLevel::Internal;
    if (isa<ProtocolDecl>(generic))
      access = std::max(AccessLevel::FilePrivate, generic->getFormalAccess());
    return access;
  }
  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(DC)->getDefaultAccessLevel();
  }
}

void AccessLevelRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto valueDecl = std::get<0>(getStorage());
  diags.diagnose(valueDecl, diag::circular_reference);
}

void AccessLevelRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto valueDecl = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(valueDecl, diag::circular_reference_through);
}

Optional<AccessLevel> AccessLevelRequest::getCachedResult() const {
  auto valueDecl = std::get<0>(getStorage());
  if (valueDecl->hasAccess())
    return valueDecl->TypeAndAccess.getInt().getValue();

  return None;
}

void AccessLevelRequest::cacheResult(AccessLevel value) const {
  auto valueDecl = std::get<0>(getStorage());
  valueDecl->setAccess(value);
}

//----------------------------------------------------------------------------//
// SetterAccessLevel computation
//----------------------------------------------------------------------------//
//
// An AbstractStorageDecl has both its own formal access and also a special
// "setter" formal access like "private(set)" that might override (and lower)
// the normal one, when evaluating the accessibility of mutating accessors.
//
// As this value can be computed, stored, synthesized and set independently from
// the cycle of computation associated with formal accesses, we give it its own
// request.

llvm::Expected<AccessLevel>
SetterAccessLevelRequest::evaluate(Evaluator &evaluator,
                                   AbstractStorageDecl *ASD) const {
  assert(!ASD->Accessors.getInt().hasValue());
  if (auto *AA = ASD->getAttrs().getAttribute<SetterAccessAttr>())
    return AA->getAccess();
  return ASD->getFormalAccess();
}

void SetterAccessLevelRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto abstractStorageDecl = std::get<0>(getStorage());
  diags.diagnose(abstractStorageDecl, diag::circular_reference);
}

void SetterAccessLevelRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto abstractStorageDecl = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(abstractStorageDecl, diag::circular_reference_through);
}

Optional<AccessLevel> SetterAccessLevelRequest::getCachedResult() const {
  auto abstractStorageDecl = std::get<0>(getStorage());
  if (abstractStorageDecl->Accessors.getInt().hasValue())
    return abstractStorageDecl->Accessors.getInt().getValue();

  return None;
}

void SetterAccessLevelRequest::cacheResult(AccessLevel value) const {
  auto abstractStorageDecl = std::get<0>(getStorage());
  // NB: don't call setSetterAccess here because it drives values through to the
  // associated accessors' formalAccess, which we might also be in the middle of
  // doing a request for. Reserve setSetterAccess for deserialization &
  // clangImporter use.
  assert(!abstractStorageDecl->Accessors.getInt().hasValue());
  abstractStorageDecl->Accessors.setInt(value);
}


//----------------------------------------------------------------------------//
// DefaultAccessLevel computation
//----------------------------------------------------------------------------//

llvm::Expected<std::pair<AccessLevel, AccessLevel>>
DefaultAndMaxAccessLevelRequest::evaluate(Evaluator &evaluator,
                                          ExtensionDecl *ED) const {
  assert(!ED->hasDefaultAccessLevel());

  LazyResolver *Resolver = ED->getASTContext().getLazyResolver();
  Resolver->resolveExtension(ED);

  AccessLevel maxAccess = AccessLevel::Public;

  if (NominalTypeDecl *nominal = ED->getExtendedNominal()) {
    maxAccess = std::max(nominal->getFormalAccess(),
                         AccessLevel::FilePrivate);
  }

  if (GenericParamList *genericParams = ED->getGenericParams()) {
    auto getTypeAccess = [ED](Type type, TypeRepr *typeRepr) -> AccessLevel {
      if (!type)
        return AccessLevel::Public;
      auto accessScope =
          TypeReprAccessScopeChecker::getAccessScope(typeRepr,
                                                     ED->getDeclContext());
      // This is an error case and will be diagnosed elsewhere.
      if (!accessScope.hasValue())
        return AccessLevel::Public;

      if (accessScope->isPublic())
        return AccessLevel::Public;
      if (isa<ModuleDecl>(accessScope->getDeclContext()))
        return AccessLevel::Internal;
      // Because extensions are always at top-level, they should never
      // reference declarations not at the top level. (And any such references
      // should be diagnosed elsewhere.) This code should not crash if that
      // occurs, though.
      return AccessLevel::FilePrivate;
    };

    // Only check the trailing 'where' requirements. Other requirements come
    // from the extended type and have already been checked.
    RequirementRequest::visitRequirements(
        WhereClauseOwner(ED, genericParams),
        TypeResolutionStage::Interface,
        [&](Requirement req, RequirementRepr *reqRepr) {
          switch (req.getKind()) {
          case RequirementKind::Conformance:
          case RequirementKind::Superclass:
          case RequirementKind::SameType:
            maxAccess = std::min(getTypeAccess(
                                   req.getSecondType(),
                                   RequirementRepr::getSecondTypeRepr(reqRepr)),
                                 maxAccess);
            LLVM_FALLTHROUGH;

          case RequirementKind::Layout:
            maxAccess = std::min(getTypeAccess(
                                   req.getFirstType(),
                                   RequirementRepr::getFirstTypeRepr(reqRepr)),
                                 maxAccess);
            break;
          }

          return false;
        });
  }

  AccessLevel defaultAccess;
  if (auto *AA = ED->getAttrs().getAttribute<AccessControlAttr>())
    defaultAccess = std::max(AA->getAccess(), AccessLevel::FilePrivate);
  else
    defaultAccess = AccessLevel::Internal;

  // Don't set the max or default access level to 'open'.  This should
  // be diagnosed as invalid anyway.
  defaultAccess = std::min(defaultAccess, AccessLevel::Public);
  maxAccess = std::min(maxAccess, AccessLevel::Public);

  // Normally putting a public member in an internal extension is harmless,
  // because that member can never be used elsewhere. But if some of the types
  // in the signature are public, it could actually end up getting picked in
  // overload resolution. Therefore, we only enforce the maximum access if the
  // extension has a 'where' clause.
  if (ED->getTrailingWhereClause())
    defaultAccess = std::min(defaultAccess, maxAccess);
  else
    maxAccess = AccessLevel::Public;

  return std::make_pair(defaultAccess, maxAccess);
}

void DefaultAndMaxAccessLevelRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto extensionDecl = std::get<0>(getStorage());
  diags.diagnose(extensionDecl, diag::circular_reference);
}

void DefaultAndMaxAccessLevelRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto extensionDecl = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(extensionDecl, diag::circular_reference_through);
}

// Default and Max access levels are stored combined as a 3-bit bitset. The Bits
// are numbered using the 3 middle values of the AccessLevel enumeration, and
// the combined value is just the bitwise-OR of the bits for Default and Max.
//
// For example, if Max=Internal and Default=FilePrivate, we will see:
//
//      0 1 1
//      | | |
//      | | [FilePrivate]
//      | |
//      | [Internal]
//      |
//      [Public]
//
// This is unambiguous to decode because of the following facts:
//
//   - At least one of the bits is set (all-zero means "not yet set").
//   - At most two of the bits are set.
//   - Max >= Default by definition.
//
// So we decode Max as the last (high) bit that is set, and Default as the first
// (low). And add one to each, to map them back into AccessLevels.

Optional<std::pair<AccessLevel,AccessLevel>>
DefaultAndMaxAccessLevelRequest::getCachedResult() const {
  auto extensionDecl = std::get<0>(getStorage());
  if (extensionDecl->hasDefaultAccessLevel()) {
    uint8_t Bits = extensionDecl->getDefaultAndMaxAccessLevelBits();
    assert(Bits != 0x7 && "more than two bits set for Default and Max");
    AccessLevel Max = static_cast<AccessLevel>(llvm::findLastSet(Bits) + 1);
    AccessLevel Default = static_cast<AccessLevel>(llvm::findFirstSet(Bits) + 1);
    assert(Max >= Default);
    return std::make_pair(Default, Max);
  }
  return None;
}

void
DefaultAndMaxAccessLevelRequest::cacheResult(
  std::pair<AccessLevel, AccessLevel> value) const {
  auto extensionDecl = std::get<0>(getStorage());
  extensionDecl->setDefaultAndMaxAccessLevelBits(value.first, value.second);
  assert(getCachedResult().getValue().first == value.first);
  assert(getCachedResult().getValue().second == value.second);
}

// Define request evaluation functions for each of the access requests.
static AbstractRequestFunction *accessRequestFunctions[] = {
#define SWIFT_TYPEID(Name)                                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/AST/AccessTypeIDZone.def"
#undef SWIFT_TYPEID
};

void swift::registerAccessRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(SWIFT_ACCESS_REQUESTS_TYPEID_ZONE,
                                     accessRequestFunctions);
}
