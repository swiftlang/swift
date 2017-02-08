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
#include "swift/AST/Expr.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Basic/Fallthrough.h"
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

  if (!D->hasAccessibility()) {
    assert(D->getDeclContext()->isLocalContext());
    return FormalLinkage::Private;
  }

  switch (D->getEffectiveAccess()) {
  case Accessibility::Public:
  case Accessibility::Open:
    return FormalLinkage::PublicUnique;
  case Accessibility::Internal:
    // If we're serializing all function bodies, type metadata for internal
    // types needs to be public too.
    if (D->getDeclContext()->getParentModule()->getResilienceStrategy()
        == ResilienceStrategy::Fragile)
      return FormalLinkage::PublicUnique;
    return FormalLinkage::HiddenUnique;
  case Accessibility::FilePrivate:
  case Accessibility::Private:
    // Why "hidden" instead of "private"? Because the debugger may need to
    // access these symbols.
    return FormalLinkage::HiddenUnique;
  }

  llvm_unreachable("Unhandled Accessibility in switch.");
}

FormalLinkage swift::getTypeLinkage(CanType type) {
  FormalLinkage result = FormalLinkage::Top;

  // Merge all nominal types from the structural type.
  (void) type.findIf([&](Type _type) {
    CanType type = CanType(_type);

    // For any nominal type reference, look at the type declaration.
    if (auto nominal = type->getAnyNominal())
      result ^= getDeclLinkage(nominal);

    return false; // continue searching
  });

  return result;
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

  case FormalLinkage::HiddenNonUnique:
    return (forDefinition ? SILLinkage::Shared : SILLinkage::HiddenExternal);

  case FormalLinkage::Private:
    return SILLinkage::Private;
  }
  llvm_unreachable("bad formal linkage");
}

SILLinkage
swift::getLinkageForProtocolConformance(const NormalProtocolConformance *C,
                                        ForDefinition_t definition) {
  // Behavior conformances are always private.
  if (C->isBehaviorConformance())
    return (definition ? SILLinkage::Private : SILLinkage::PrivateExternal);

  ModuleDecl *conformanceModule = C->getDeclContext()->getParentModule();

  // If the conformance was synthesized by the ClangImporter, give it
  // shared linkage.
  auto typeDecl = C->getType()->getNominalOrBoundGenericNominal();
  auto typeUnit = typeDecl->getModuleScopeContext();
  if (isa<ClangModuleUnit>(typeUnit)
      && conformanceModule == typeUnit->getParentModule())
    return SILLinkage::Shared;

  Accessibility accessibility = std::min(C->getProtocol()->getEffectiveAccess(),
                                         typeDecl->getEffectiveAccess());
  switch (accessibility) {
    case Accessibility::Private:
    case Accessibility::FilePrivate:
      return (definition ? SILLinkage::Private : SILLinkage::PrivateExternal);

    case Accessibility::Internal:
      return (definition ? SILLinkage::Hidden : SILLinkage::HiddenExternal);

    default:
      return (definition ? SILLinkage::Public : SILLinkage::PublicExternal);
  }
}
