//===--- SwiftNameTranslation.cpp - Swift to ObjC Name Translation APIs ---===//
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
//  This file contains utilities for translating Swift names to ObjC.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/SwiftNameTranslation.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/AST/Decl.h"
#include "swift/AST/LazyResolver.h"
#include "swift/Basic/StringExtras.h"

#include "clang/AST/DeclObjC.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;

StringRef swift::objc_translation::
getNameForObjC(const ValueDecl *VD, CustomNamesOnly_t customNamesOnly) {
  assert(isa<ClassDecl>(VD) || isa<ProtocolDecl>(VD) || isa<StructDecl>(VD) ||
         isa<EnumDecl>(VD) || isa<EnumElementDecl>(VD) ||
         isa<TypeAliasDecl>(VD));
  if (auto objc = VD->getAttrs().getAttribute<ObjCAttr>()) {
    if (auto name = objc->getName()) {
      assert(name->getNumSelectorPieces() == 1);
      return name->getSelectorPieces().front().str();
    }
  }

  if (customNamesOnly)
    return StringRef();

  if (auto clangDecl = dyn_cast_or_null<clang::NamedDecl>(VD->getClangDecl())) {
    if (const clang::IdentifierInfo *II = clangDecl->getIdentifier())
      return II->getName();
    if (auto *anonDecl = dyn_cast<clang::TagDecl>(clangDecl))
      if (auto *anonTypedef = anonDecl->getTypedefNameForAnonDecl())
        return anonTypedef->getIdentifier()->getName();
  }

  return VD->getBaseName().getIdentifier().str();
}

std::string swift::objc_translation::
getErrorDomainStringForObjC(const EnumDecl *ED) {
  // Should have already been diagnosed as diag::objc_enum_generic.
  assert(!ED->isGenericContext() && "Trying to bridge generic enum error to Obj-C");

  // Clang decls have custom domains, but we shouldn't see them here anyway.
  assert(!ED->getClangDecl() && "clang decls shouldn't be re-exported");

  SmallVector<const NominalTypeDecl *, 4> outerTypes;
  for (const NominalTypeDecl * D = ED;
       D != nullptr;
       D = D->getDeclContext()->getSelfNominalTypeDecl()) {
    // We don't currently PrintAsObjC any types whose parents are private or
    // fileprivate.
    assert(D->getFormalAccess() >= AccessLevel::Internal &&
            "We don't currently append private discriminators");
    outerTypes.push_back(D);
  }

  std::string buffer = ED->getParentModule()->getNameStr();
  for (auto D : llvm::reverse(outerTypes)) {
    buffer += ".";
    buffer += D->getNameStr();
  }

  return buffer;
}

bool swift::objc_translation::
printSwiftEnumElemNameInObjC(const EnumElementDecl *EL, llvm::raw_ostream &OS,
                             Identifier PreferredName) {
  StringRef ElemName = getNameForObjC(EL, CustomNamesOnly);
  if (!ElemName.empty()) {
    OS << ElemName;
    return true;
  }
  OS << getNameForObjC(EL->getDeclContext()->getSelfEnumDecl());
  if (PreferredName.empty())
    ElemName = EL->getName().str();
  else
    ElemName = PreferredName.str();

  SmallString<64> Scratch;
  OS << camel_case::toSentencecase(ElemName, Scratch);
  return false;
}

std::pair<Identifier, ObjCSelector> swift::objc_translation::
getObjCNameForSwiftDecl(const ValueDecl *VD, DeclName PreferredName){
  ASTContext &Ctx = VD->getASTContext();
  Identifier BaseName;
  if (PreferredName) {
    auto BaseNameStr = PreferredName.getBaseName().userFacingName();
    BaseName = Ctx.getIdentifier(BaseNameStr);
  }
  if (auto *FD = dyn_cast<AbstractFunctionDecl>(VD)) {
    return {Identifier(), FD->getObjCSelector(PreferredName)};
  } else if (auto *VAD = dyn_cast<VarDecl>(VD)) {
    if (PreferredName)
      return {BaseName, ObjCSelector()};
    return {VAD->getObjCPropertyName(), ObjCSelector()};
  } else if (auto *SD = dyn_cast<SubscriptDecl>(VD)) {
    return getObjCNameForSwiftDecl(SD->getParsedAccessor(AccessorKind::Get),
                                   PreferredName);
  } else if (auto *EL = dyn_cast<EnumElementDecl>(VD)) {
    SmallString<64> Buffer;
    {
      llvm::raw_svector_ostream OS(Buffer);
      printSwiftEnumElemNameInObjC(EL, OS, BaseName);
    }
    return {Ctx.getIdentifier(Buffer.str()), ObjCSelector()};
  } else {
    // @objc(ExplicitName) > PreferredName > Swift name.
    StringRef Name = getNameForObjC(VD, CustomNamesOnly);
    if (!Name.empty())
      return {Ctx.getIdentifier(Name), ObjCSelector()};
    if (PreferredName)
      return {BaseName, ObjCSelector()};
    return {Ctx.getIdentifier(getNameForObjC(VD)), ObjCSelector()};
  }
}

bool swift::objc_translation::
isVisibleToObjC(const ValueDecl *VD, AccessLevel minRequiredAccess,
                bool checkParent) {
  if (!(VD->isObjC() || VD->getAttrs().hasAttribute<CDeclAttr>()))
    return false;
  if (VD->getFormalAccess() >= minRequiredAccess) {
    return true;
  } else if (checkParent) {
    if (auto ctor = dyn_cast<ConstructorDecl>(VD)) {
      // Check if we're overriding an initializer that is visible to obj-c
      if (auto parent = ctor->getOverriddenDecl())
        return isVisibleToObjC(parent, minRequiredAccess, false);
    }
  }
  return false;
}
