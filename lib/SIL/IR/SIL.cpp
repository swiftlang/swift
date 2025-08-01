//===--- SIL.cpp - Implements random SIL functionality --------------------===//
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

#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILUndef.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Assertions.h"
#include "swift/ClangImporter/ClangModule.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"

using namespace swift;

FormalLinkage swift::getDeclLinkage(const ValueDecl *D) {
  const DeclContext *fileContext = D->getDeclContext()->getModuleScopeContext();

  // Clang declarations are public and can't be assured of having a
  // unique defining location.
  if (isa<ClangModuleUnit>(fileContext) &&
          !D->getObjCImplementationDecl())
    return FormalLinkage::PublicNonUnique;

  switch (D->getEffectiveAccess()) {
  case AccessLevel::Package:
    return FormalLinkage::PackageUnique;
  case AccessLevel::Public:
  case AccessLevel::Open:
    return FormalLinkage::PublicUnique;
  case AccessLevel::Internal:
    return FormalLinkage::HiddenUnique;
  case AccessLevel::FilePrivate:
  case AccessLevel::Private:
    return FormalLinkage::Private;
  }

  llvm_unreachable("Unhandled access level in switch.");
}

SILLinkage swift::getSILLinkage(FormalLinkage linkage,
                                ForDefinition_t forDefinition) {
  switch (linkage) {
  case FormalLinkage::PublicUnique:
    return (forDefinition ? SILLinkage::Public : SILLinkage::PublicExternal);

  case FormalLinkage::PublicNonUnique:
    // FIXME: any place we have to do this that actually requires
    // uniqueness is buggy.
    return (forDefinition ? SILLinkage::Shared : SILLinkage::PublicExternal);

  case FormalLinkage::PackageUnique:
    return (forDefinition ? SILLinkage::Package : SILLinkage::PackageExternal);

  case FormalLinkage::HiddenUnique:
    return (forDefinition ? SILLinkage::Hidden : SILLinkage::HiddenExternal);

  case FormalLinkage::Private:
    return SILLinkage::Private;
  }
  llvm_unreachable("bad formal linkage");
}

SILLinkage
swift::getLinkageForProtocolConformance(const ProtocolConformance *C,
                                        ForDefinition_t definition) {
  // If the conformance was synthesized, give it shared linkage.
  if (C->getRootConformance()->isSynthesized())
    return SILLinkage::Shared;

  auto typeDecl = C->getDeclContext()->getSelfNominalTypeDecl();
  AccessLevel access = std::min(C->getProtocol()->getEffectiveAccess(),
                                typeDecl->getEffectiveAccess());
  switch (access) {
    case AccessLevel::Private:
    case AccessLevel::FilePrivate:
      return SILLinkage::Private;
    case AccessLevel::Internal:
      return (definition ? SILLinkage::Hidden : SILLinkage::HiddenExternal);
    case AccessLevel::Package:
      return (definition ? SILLinkage::Package : SILLinkage::PackageExternal);
    case AccessLevel::Public:
    case AccessLevel::Open:
      return (definition ? SILLinkage::Public : SILLinkage::PublicExternal);
  }
}

bool SILModule::isTypeMetadataAccessible(CanType type) {
  // SILModules built for the debugger have special powers to access metadata
  // for types in other files/modules.
  if (getASTContext().LangOpts.DebuggerSupport)
    return true;

  assert(type->isLegalFormalType());

  return !type.findIf([&](CanType type) {
    // Note that this function returns true if the type is *illegal* to use.

    // Ignore non-nominal types -- except for opaque result types which can be
    // private and in a different translation unit in which case they can't be
    // accessed.
    ValueDecl *decl = type.getNominalOrBoundGenericNominal();
    if (!decl)
      decl = isa<OpaqueTypeArchetypeType>(type)
                 ? cast<OpaqueTypeArchetypeType>(type)->getDecl()
                 : nullptr;
    if (!decl)
      return false;

    // Check whether the declaration is inaccessible from the current context.
    switch (getDeclLinkage(decl)) {

    // Public declarations are accessible from everywhere.
    case FormalLinkage::PublicUnique:
    case FormalLinkage::PublicNonUnique:
    case FormalLinkage::PackageUnique: 
      return false;

    // Hidden declarations are inaccessible from different modules.
    case FormalLinkage::HiddenUnique:
      return (decl->getModuleContext() != getSwiftModule());

    // Private declarations are inaccessible from different files unless
    // this is WMO and we're in the same module.
    case FormalLinkage::Private: {
      // The associated DC should be either a SourceFile or, in WMO mode,
      // a ModuleDecl.  In the WMO modes, IRGen will ensure that private
      // declarations are usable throughout the module.  Therefore, in
      // either case we just need to make sure that the declaration comes
      // from within the associated DC.
      auto declDC = decl->getDeclContext();
      return !(declDC == AssociatedDeclContext ||
               declDC->isChildContextOf(AssociatedDeclContext));
    }
    }
    llvm_unreachable("bad linkage");
  });
}

/// Return the formal linkage of the component restrictions of this
/// generic signature.  This is the appropriate linkage for a lazily-
/// emitted entity derived from the generic signature.
///
/// This function never returns PublicUnique.
FormalLinkage swift::getGenericSignatureLinkage(CanGenericSignature sig) {
  // This can only be PublicNonUnique or HiddenUnique.  Signatures can
  // never be PublicUnique in the first place, and we short-circuit on
  // Private.  So we only ever update it when we see HiddenUnique linkage.
  FormalLinkage linkage = FormalLinkage::PublicNonUnique;

  for (auto &req : sig.getRequirements()) {
    // The first type can be ignored because it should always be
    // a dependent type.

    switch (req.getKind()) {
    case RequirementKind::SameShape:
    case RequirementKind::Layout:
      continue;

    case RequirementKind::Conformance:
    case RequirementKind::SameType:
    case RequirementKind::Superclass:
      switch (getTypeLinkage(CanType(req.getSecondType()))) {
      case FormalLinkage::PublicUnique:
      case FormalLinkage::PublicNonUnique:
      case FormalLinkage::PackageUnique:
        continue;
      case FormalLinkage::HiddenUnique:
        linkage = FormalLinkage::HiddenUnique;
        continue;
      case FormalLinkage::Private:
        // We can short-circuit with this.
        return linkage;
      }
    }
  }

  return linkage;
}

/// Return the formal linkage of the given formal type.
/// This in the appropriate linkage for a lazily-emitted entity
/// derived from the type.
///
/// This function never returns PublicUnique, which means that,
/// even if a type is simply a reference to a non-generic
/// uniquely-emitted nominal type, the formal linkage of that
/// type may differ from the formal linkage of the underlying
/// type declaration.
FormalLinkage swift::getTypeLinkage(CanType t) {
  assert(t->isLegalFormalType());
  
  class Walker : public TypeWalker {
  public:
    FormalLinkage Linkage;
    Walker() : Linkage(FormalLinkage::PublicNonUnique) {}

    Action walkToTypePre(Type ty) override {
      // Non-nominal types are always available.
      auto decl = ty->getNominalOrBoundGenericNominal();
      if (!decl)
        return Action::Continue;
      
      Linkage = std::max(Linkage, getDeclLinkage(decl));
      return Action::Continue;
    }
  };

  Walker w;
  t.walk(w);
  return w.Linkage;
}

/// Answer whether IRGen's emitTypeMetadataForLayout can fetch metadata for
/// a type, which is the necessary condition for being able to do value
/// operations on the type using dynamic metadata.
static bool isTypeMetadataForLayoutAccessible(SILModule &M, SILType type) {
  // Look through types that aren't necessarily legal formal types:

  //   - tuples
  if (auto tupleType = type.getAs<TupleType>()) {
    for (auto index : indices(tupleType.getElementTypes())) {
      if (!isTypeMetadataForLayoutAccessible(M, type.getTupleElementType(index)))
        return false;
    }
    return true;
  }

  //   - optionals
  if (auto objType = type.getOptionalObjectType()) {
    return isTypeMetadataForLayoutAccessible(M, objType);
  }

  //   - function types
  if (type.is<SILFunctionType>())
    return true;

  //   - metatypes
  if (type.is<AnyMetatypeType>())
    return true;

  //   - pack expansion types
  if (auto expansionType = type.getAs<PackExpansionType>()) {
    auto patternType = SILType::getPrimitiveType(expansionType.getPatternType(),
                                                 type.getCategory());
    return isTypeMetadataForLayoutAccessible(M, patternType);
  }

  //   - lowered pack types
  if (auto packType = type.getAs<SILPackType>()) {
    for (auto eltType : packType.getElementTypes()) {
      if (!isTypeMetadataForLayoutAccessible(
              M, SILType::getPrimitiveAddressType(eltType)))
        return false;
    }

    return true;
  }

  // Otherwise, check that we can fetch the type metadata.
  return M.isTypeMetadataAccessible(type.getASTType());
}

/// Can we perform value operations on the given type?  We have no way
/// of doing value operations on resilient-layout types from other modules
/// that are ABI-private to their defining module.  But if the type is not
/// ABI-private, we can always at least fetch its metadata and use the
/// value witness table stored there.
bool SILModule::isTypeABIAccessible(SILType type,
                                    TypeExpansionContext forExpansion) {
  // Fixed-ABI types can have value operations done without metadata.
  if (Types.getTypeProperties(type, forExpansion).isFixedABI())
    return true;

  assert(!type.is<ReferenceStorageType>() &&
         !type.is<SILFunctionType>() &&
         !type.is<AnyMetatypeType>() &&
         "unexpected SIL lowered-only type with non-fixed layout");

  // Otherwise, we need to be able to fetch layout-metadata for the type.
  return isTypeMetadataForLayoutAccessible(type);
}

bool SILModule::isTypeMetadataForLayoutAccessible(SILType type) {
  if (type.is<ReferenceStorageType>() || type.is<SILFunctionType>() ||
      type.is<AnyMetatypeType>() || type.is<SILPackType>())
    return false;

  return ::isTypeMetadataForLayoutAccessible(*this, type);
}

// Given the type `ty`, which should be in the generic environment of the signature
// `sig`, return a generic signature with all of the requirements of `sig`,
// combined with all of the requirements necessary for `ty` to be both
// `Copyable` and `Escapable`, if possible. Returns `nullopt` if the type
// can never be both Copyable and Escapable.
static std::optional<GenericSignature>
getKeyPathSupportingGenericSignature(Type ty, GenericSignature contextSig) {
  auto &C = ty->getASTContext();
  
  // If the type is already unconditionally Copyable and Escapable, we don't
  // need any further requirements.
  if (!ty->isNoncopyable() && ty->isEscapable()) {
    return contextSig;
  }
  
  ProtocolConformanceRef copyable, escapable;
  auto copyableProtocol = C.getProtocol(KnownProtocolKind::Copyable);
  auto escapableProtocol = C.getProtocol(KnownProtocolKind::Escapable);
  
  // If the type is an archetype, then it just needs Copyable and Escapable
  // constraints imposed.
  if (ty->is<ArchetypeType>()) {
    copyable = ProtocolConformanceRef::forAbstract(ty->mapTypeOutOfContext(),
                                                   copyableProtocol);
    escapable = ProtocolConformanceRef::forAbstract(ty->mapTypeOutOfContext(),
                                                    escapableProtocol);
  } else {
    // Look for any conditional conformances.
    copyable = lookupConformance(ty, copyableProtocol);
    escapable = lookupConformance(ty, escapableProtocol);
  }

  // If the type is never copyable or escapable, that's it.
  if (copyable.isInvalid() || escapable.isInvalid()) {
    return std::nullopt;
  }
  
  // Otherwise, let's see if we get a viable generic signature combining the
  // requirements for those conformances with the requirements of the
  // declaration context.
  SmallVector<Requirement, 2> ceRequirements;

  auto getRequirementsFromConformance = [&](ProtocolConformanceRef ref) {
    if (ref.isAbstract()) {
      // The only requirements are that the abstract type itself be copyable
      // and escapable.
      ceRequirements.push_back(Requirement(RequirementKind::Conformance,
               ty->mapTypeOutOfContext(), copyableProtocol->getDeclaredType()));
      ceRequirements.push_back(Requirement(RequirementKind::Conformance,
              ty->mapTypeOutOfContext(), escapableProtocol->getDeclaredType()));
      return;
    }
    
    if (!ref.isConcrete()) {
      return;
    }
    auto conformance = ref.getConcrete();
    
    for (auto reqt : conformance->getRootConformance()
                                ->getConditionalRequirements()) {
      ceRequirements.push_back(reqt);
    }
  };
  getRequirementsFromConformance(copyable);
  getRequirementsFromConformance(escapable);

  auto regularSignature = buildGenericSignatureWithError(C,
                                               contextSig,
                                               {},
                                               std::move(ceRequirements),
                                               /*allowInverses*/ false);
  
  // If the resulting signature has conflicting requirements, then it is
  // impossible for the type to be copyable and equatable.
  if (regularSignature.getInt()) {
    return std::nullopt;
  }
  
  // Otherwise, we have the signature we're looking for.
  return regularSignature.getPointer();
}

static std::optional<GenericSignature>
getKeyPathSupportingGenericSignatureForValueType(Type ty,
                                                 GenericSignature sig) {
  std::optional<GenericSignature> contextSig = sig;
  
  // Visit lowered positions.
  if (auto tupleTy = ty->getAs<TupleType>()) {
    for (auto eltTy : tupleTy->getElementTypes()) {
      if (eltTy->is<PackExpansionType>())
        return std::nullopt;

      contextSig = getKeyPathSupportingGenericSignatureForValueType(
                                                            eltTy, *contextSig);
      if (!contextSig)
        return std::nullopt;
    }

    return contextSig;
  }

  if (auto objTy = ty->getOptionalObjectType())
    ty = objTy;

  // FIXME: Remove this once isUnimplementableVariadicFunctionAbstraction()
  // goes away in SILGenPoly.cpp.
  if (auto funcTy = ty->getAs<FunctionType>()) {
    for (auto param : funcTy->getParams()) {
      auto paramTy = param.getPlainType();
      if (paramTy->is<PackExpansionType>())
        return std::nullopt;

      contextSig = getKeyPathSupportingGenericSignatureForValueType(paramTy,
                                                                    *contextSig);
      if (!contextSig) {
        return std::nullopt;
      }
    }

    contextSig = getKeyPathSupportingGenericSignatureForValueType(funcTy->getResult(),
                                                                  *contextSig);

    if (!contextSig) {
      return std::nullopt;
    }
  }

  // Noncopyable types aren't supported by key paths in their current form.
  // They would also need a new ABI that's yet to be implemented in order to
  // be properly supported, so let's suppress the descriptor for now if either
  // the container or storage type of the declaration is non-copyable.
  return getKeyPathSupportingGenericSignature(ty, *contextSig);
}

std::optional<GenericSignature>
AbstractStorageDecl::getPropertyDescriptorGenericSignature() const {
  // The storage needs a descriptor if it sits at a module's ABI boundary,
  // meaning it has public linkage, and it is eligible to be part of a key path.
  
  auto contextTy = getDeclContext()->getDeclaredTypeInContext();
  auto contextSig = getInnermostDeclContext()->getGenericSignatureOfContext();
  
  // If the root type is never `Copyable` or `Escapable`, then instance
  // members can't be used in key paths, at least as they are implemented
  // today.
  if (!isStatic() && contextTy) {
    auto ceContextSig = getKeyPathSupportingGenericSignature(contextTy,
                                                             contextSig);
    if (!ceContextSig) {
      return std::nullopt;
    }
    contextSig = *ceContextSig;
  }

  // TODO: Global properties ought to eventually be referenceable
  // as key paths from ().
  if (!getDeclContext()->isTypeContext())
    return std::nullopt;
  
  // Protocol requirements do not need property descriptors.
  if (isa<ProtocolDecl>(getDeclContext()))
    return std::nullopt;

  // Static properties declared directly in protocol do not need
  // descriptors as existential Any.Type will not resolve to a value.
  if (isStatic() && isa<ProtocolDecl>(getDeclContext()))
    return std::nullopt;

  // FIXME: We should support properties and subscripts with '_read' accessors;
  // 'get' is not part of the opaque accessor set there.
  auto *getter = getOpaqueAccessor(AccessorKind::Get);
  if (!getter)
    return std::nullopt;

  // If the getter is mutating, we cannot form a keypath to it at all.
  if (isGetterMutating())
    return std::nullopt;

  // If the storage is an ABI-compatible override of another declaration, we're
  // not going to be emitting a property descriptor either.
  if (!isValidKeyPathComponent())
    return std::nullopt;

  // TODO: If previous versions of an ABI-stable binary needed the descriptor,
  // then we still do.

  // Check the linkage of the declaration.
  auto getterLinkage = SILDeclRef(getter).getLinkage(ForDefinition);
  
  switch (getterLinkage) {
  case SILLinkage::Public:
  case SILLinkage::PublicNonABI:
  case SILLinkage::Package:
  case SILLinkage::PackageNonABI:
    // We may need a descriptor.
    break;
    
  case SILLinkage::Shared:
  case SILLinkage::Private:
  case SILLinkage::Hidden:
    // Don't need a public descriptor.
    return std::nullopt;
    
  case SILLinkage::HiddenExternal:
  case SILLinkage::PublicExternal:
  case SILLinkage::PackageExternal:
    llvm_unreachable("should be definition linkage?");
  }

  auto typeInContext = contextSig.getGenericEnvironment()->mapTypeIntoContext(
      getValueInterfaceType());
  auto valueTypeSig = getKeyPathSupportingGenericSignatureForValueType(typeInContext, contextSig);
  if (!valueTypeSig) {
    return std::nullopt;
  }
  contextSig = *valueTypeSig;

  // Subscripts with inout arguments (FIXME)and reabstracted arguments(/FIXME)
  // don't have descriptors either.
  if (auto sub = dyn_cast<SubscriptDecl>(this)) {
    for (auto *index : *sub->getIndices()) {
      // Keypaths can't capture inout indices.
      if (index->isInOut()) {
        return std::nullopt;
      }
      
      auto indexTy = index->getInterfaceType()
                        ->getReducedType(sub->getGenericSignatureOfContext());
      
      // TODO: Handle reabstraction and tuple explosion in thunk generation.
      // This wasn't previously a concern because anything that was Hashable
      // had only one abstraction level and no explosion.
      
      if (isa<TupleType>(indexTy))
        return std::nullopt;
      
      auto indexObjTy = indexTy;
      if (auto objTy = indexObjTy.getOptionalObjectType())
        indexObjTy = objTy;
      
      if (isa<AnyFunctionType>(indexObjTy)
          || isa<AnyMetatypeType>(indexObjTy))
        return std::nullopt;
    }
  }

  return contextSig;
}
