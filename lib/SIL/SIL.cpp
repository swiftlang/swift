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
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/ClangImporter/ClangModule.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"

using namespace swift;

SILUndef *SILUndef::get(SILType Ty, SILModule *M) {
  // Unique these.
  SILUndef *&Entry = M->UndefValues[Ty];
  if (Entry == nullptr)
    Entry = new (*M) SILUndef(Ty);
  return Entry;
}

FormalLinkage swift::getDeclLinkage(const ValueDecl *D) {
  const DeclContext *fileContext = D->getDeclContext()->getModuleScopeContext();

  // Clang declarations are public and can't be assured of having a
  // unique defining location.
  if (isa<ClangModuleUnit>(fileContext))
    return FormalLinkage::PublicNonUnique;

  switch (D->getEffectiveAccess()) {
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

  case FormalLinkage::HiddenUnique:
    return (forDefinition ? SILLinkage::Hidden : SILLinkage::HiddenExternal);

  case FormalLinkage::Private:
    return SILLinkage::Private;
  }
  llvm_unreachable("bad formal linkage");
}

SILLinkage
swift::getLinkageForProtocolConformance(const RootProtocolConformance *C,
                                        ForDefinition_t definition) {
  // Behavior conformances are always private.
  if (C->isBehaviorConformance())
    return (definition ? SILLinkage::Private : SILLinkage::PrivateExternal);

  // If the conformance was synthesized by the ClangImporter, give it
  // shared linkage.
  if (isa<ClangModuleUnit>(C->getDeclContext()->getModuleScopeContext()))
    return SILLinkage::Shared;

  auto typeDecl = C->getType()->getNominalOrBoundGenericNominal();
  AccessLevel access = std::min(C->getProtocol()->getEffectiveAccess(),
                                typeDecl->getEffectiveAccess());
  switch (access) {
    case AccessLevel::Private:
    case AccessLevel::FilePrivate:
      return (definition ? SILLinkage::Private : SILLinkage::PrivateExternal);

    case AccessLevel::Internal:
      return (definition ? SILLinkage::Hidden : SILLinkage::HiddenExternal);

    default:
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

    // Ignore non-nominal types.
    auto decl = type.getNominalOrBoundGenericNominal();
    if (!decl)
      return false;

    // Check whether the declaration is inaccessible from the current context.
    switch (getDeclLinkage(decl)) {

    // Public declarations are accessible from everywhere.
    case FormalLinkage::PublicUnique:
    case FormalLinkage::PublicNonUnique:
      return false;

    // Hidden declarations are inaccessible from different modules.
    case FormalLinkage::HiddenUnique:
      return (decl->getModuleContext() != getSwiftModule());

    // Private declarations are inaccessible from different files unless
    // this is WMO and we're in the same module.
    case FormalLinkage::Private: {
      // The only time we don't have an associated DC is in the
      // integrated REPL, where we also don't have a concept of other
      // source files within the current module.
      if (!AssociatedDeclContext)
        return (decl->getModuleContext() != getSwiftModule());

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

  // Otherwise, check that we can fetch the type metadata.
  return M.isTypeMetadataAccessible(type.getASTType());

}

/// Can we perform value operations on the given type?  We have no way
/// of doing value operations on resilient-layout types from other modules
/// that are ABI-private to their defining module.  But if the type is not
/// ABI-private, we can always at least fetch its metadata and use the
/// value witness table stored there.
bool SILModule::isTypeABIAccessible(SILType type) {
  // Fixed-ABI types can have value operations done without metadata.
  if (Types.getTypeLowering(type).isFixedABI())
    return true;

  assert(!type.is<ReferenceStorageType>() &&
         !type.is<SILFunctionType>() &&
         !type.is<AnyMetatypeType>() &&
         "unexpected SIL lowered-only type with non-fixed layout");

  // Otherwise, we need to be able to fetch layout-metadata for the type.
  return isTypeMetadataForLayoutAccessible(*this, type);
}

bool AbstractStorageDecl::exportsPropertyDescriptor() const {
  // The storage needs a descriptor if it sits at a module's ABI boundary,
  // meaning it has public linkage.
  
  // TODO: Global and static properties ought to eventually be referenceable
  // as key paths from () or T.Type too.
  if (!getDeclContext()->isTypeContext() || isStatic())
    return false;
  
  // Protocol requirements do not need property descriptors.
  if (isa<ProtocolDecl>(getDeclContext()))
    return false;
  
  // Any property that's potentially resilient should have accessors
  // synthesized.
  if (!getGetter())
    return false;

  // If the getter is mutating, we cannot form a keypath to it at all.
  if (isGetterMutating())
    return false;

  // If the storage is an ABI-compatible override of another declaration, we're
  // not going to be emitting a property descriptor either.
  if (!isValidKeyPathComponent())
    return false;

  // TODO: If previous versions of an ABI-stable binary needed the descriptor,
  // then we still do.

  // Check the linkage of the declaration.
  auto getter = SILDeclRef(getGetter());
  auto getterLinkage = getter.getLinkage(ForDefinition);
  
  switch (getterLinkage) {
  case SILLinkage::Public:
  case SILLinkage::PublicNonABI:
    // We may need a descriptor.
    break;
    
  case SILLinkage::Shared:
  case SILLinkage::Private:
  case SILLinkage::Hidden:
    // Don't need a public descriptor.
    return false;
    
  case SILLinkage::HiddenExternal:
  case SILLinkage::PrivateExternal:
  case SILLinkage::PublicExternal:
  case SILLinkage::SharedExternal:
    llvm_unreachable("should be definition linkage?");
  }

  // Subscripts with inout arguments (FIXME)and reabstracted arguments(/FIXME)
  // don't have descriptors either.
  if (auto sub = dyn_cast<SubscriptDecl>(this)) {
    for (auto *index : *sub->getIndices()) {
      // Keypaths can't capture inout indices.
      if (index->isInOut())
        return false;
      
      auto indexTy = index->getInterfaceType()
                        ->getCanonicalType(sub->getGenericSignatureOfContext());
      
      // TODO: Handle reabstraction and tuple explosion in thunk generation.
      // This wasn't previously a concern because anything that was Hashable
      // had only one abstraction level and no explosion.
      
      if (isa<TupleType>(indexTy))
        return false;
      
      auto indexObjTy = indexTy;
      if (auto objTy = indexObjTy.getOptionalObjectType())
        indexObjTy = objTy;
      
      if (isa<AnyFunctionType>(indexObjTy)
          || isa<AnyMetatypeType>(indexObjTy))
        return false;
    }
  }

  return true;
}
