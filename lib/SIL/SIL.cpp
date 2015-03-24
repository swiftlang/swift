//===--- SIL.cpp - Implements random SIL functionality --------------------===//
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
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
using namespace swift;

void ValueBase::replaceAllUsesWith(ValueBase *RHS) {
  assert(this != RHS && "Cannot RAUW a value with itself");
  assert(getNumTypes() == RHS->getNumTypes() &&
         "An instruction and the value base that it is being replaced by "
         "must have the same number of types");

  while (!use_empty()) {
    Operand *Op = *use_begin();
    Op->set(SILValue(RHS, Op->get().getResultNumber()));
  }
}


SILUndef *SILUndef::get(SILType Ty, SILModule *M) {
  // Unique these.
  SILUndef *&Entry = M->UndefValues[Ty];
  if (Entry == nullptr)
    Entry = new (*M) SILUndef(Ty);
  return Entry;
}

static FormalLinkage
getGenericClauseLinkage(ArrayRef<GenericTypeParamDecl *> params) {
  FormalLinkage result = FormalLinkage::Top;
  for (auto &param : params) {
    for (auto proto : param->getConformingProtocols(nullptr))
      result ^= getTypeLinkage(CanType(proto->getDeclaredType()));
    if (auto superclass = param->getSuperclass())
      result ^= getTypeLinkage(superclass->getCanonicalType());
  }
  return result;
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
    return FormalLinkage::PublicUnique;
  case Accessibility::Internal:
    // FIXME: This ought to be "hidden" as well, but that causes problems when
    // inlining code from the standard library, which may reference internal
    // declarations.
    return FormalLinkage::PublicUnique;
  case Accessibility::Private:
    // Why "hidden" instead of "private"? Because the debugger may need to
    // access these symbols.
    return FormalLinkage::HiddenUnique;
  }
}

FormalLinkage swift::getTypeLinkage(CanType type) {
  FormalLinkage result = FormalLinkage::Top;

  // Merge all nominal types from the structural type.
  (void) type.findIf([&](Type _type) {
    CanType type = CanType(_type);

    // For any nominal type reference, look at the type declaration.
    if (auto nominal = type->getAnyNominal()) {
      result ^= getDeclLinkage(nominal);

    // For polymorphic function types, look at the generic parameters.
    // FIXME: findIf should do this, once polymorphic function types can be
    // canonicalized and re-formed properly.
    } else if (auto polyFn = dyn_cast<PolymorphicFunctionType>(type)) {
      result ^= getGenericClauseLinkage(polyFn->getGenericParameters());
    }

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
