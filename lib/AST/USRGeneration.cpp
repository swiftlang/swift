//===--- USRGeneration.cpp - Routines for USR generation ------------------===//
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

#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/AST/USRGeneration.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/SwiftNameTranslation.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Lex/PreprocessingRecord.h"
#include "clang/Lex/Preprocessor.h"

using namespace swift;
using namespace ide;

static inline StringRef getUSRSpacePrefix() {
  return "s:";
}

bool ide::printTypeUSR(Type Ty, raw_ostream &OS) {
  assert(!Ty->hasArchetype() && "cannot have contextless archetypes mangled.");
  OS << NewMangling::mangleTypeForDebugger(Ty->getRValueType(), nullptr);
  return false;
}

bool ide::printDeclTypeUSR(const ValueDecl *D, raw_ostream &OS) {
  using namespace Mangle;
  Mangler Mangler(true);
  Mangler.mangleDeclTypeForDebugger(D);
  Mangler.finalize(OS);
  return false;
}

static bool printObjCNameFragment(const ValueDecl *D, StringRef ObjCName,
                                  raw_ostream &OS) {
  if (!D)
    return true;

  if (isa<ClassDecl>(D)) {
    clang::index::generateUSRForObjCClass(ObjCName, OS);
  } else if (isa<ProtocolDecl>(D)) {
    clang::index::generateUSRForObjCProtocol(ObjCName, OS);
  } else if (isa<VarDecl>(D)) {
    clang::index::generateUSRForObjCProperty(ObjCName, D->isStatic(), OS);
  } else if (isa<AbstractFunctionDecl>(D)) {
    clang::index::generateUSRForObjCMethod(ObjCName, D->isInstanceMember(), OS);
  } else if (isa<EnumDecl>(D)) {
    OS << "@E@" << ObjCName; // FIXME: expose clang API to handle enum names
  } else if (isa<EnumElementDecl>(D)) {
    OS << "@" << ObjCName;
  } else {
    llvm_unreachable("Unexpected value decl");
    return true;
  }
  return false;
}

static bool printObjCUSR(const ValueDecl *D, raw_ostream &OS, Identifier Ident,
                         ObjCSelector Selector) {
  OS << clang::index::getUSRSpacePrefix();

  if (D->getDeclContext()->isTypeContext()) {
    auto *Parent = D->getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext();
    auto ObjCName = objc_translation::getObjCNameForSwiftDecl(Parent);
    if (printObjCNameFragment(Parent, ObjCName.first.str(), OS))
      return true;
  }

  if (!Ident.empty())
    return printObjCNameFragment(D, Ident.str(), OS);
  if (Selector) {
    llvm::SmallString<128> Buf;
    return printObjCNameFragment(D, Selector.getString(Buf), OS);
  }
  return false;
}

static bool ShouldUseObjCName(const Decl *D) {
  // Only the subscript getter/setter are visible to ObjC rather than the
  // subscript itself
  if (isa<SubscriptDecl>(D))
    return false;

  auto Parent = D->getDeclContext()->getInnermostDeclarationDeclContext();
  if (Parent && (!ShouldUseObjCName(Parent) || // parent should be visible too
                    !D->getDeclContext()->isTypeContext() || // no local decls
                    isa<TypeDecl>(D))) // nested types aren't supported
    return false;

  if (const ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
    if (isa<EnumElementDecl>(VD))
      return Parent;
    return objc_translation::isVisibleToObjC(VD, Accessibility::Internal);
  }

  if (const ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D)) {
    if (auto ExtendedType = ED->getExtendedType()) {
      auto baseClass = ExtendedType->getClassOrBoundGenericClass();
      return baseClass && ShouldUseObjCName(baseClass) && !baseClass->isForeign();
    }
  }
  return false;
}

bool ide::printDeclUSR(const ValueDecl *D, raw_ostream &OS) {
  using namespace Mangle;

  if (!D->hasName() && (!isa<FuncDecl>(D) || cast<FuncDecl>(D)->getAccessorKind() == AccessorKind::NotAccessor))
    return true; // Ignore.
  if (D->getModuleContext()->isBuiltinModule())
    return true; // Ignore.

  ValueDecl *VD = const_cast<ValueDecl *>(D);

  auto interpretAsClangNode = [](const ValueDecl *D)->ClangNode {
    ClangNode ClangN = D->getClangNode();
    if (auto ClangD = ClangN.getAsDecl()) {
      // NSErrorDomain causes the clang enum to be imported like this:
      //
      // struct MyError {
      //     enum Code : Int32 {
      //         case errFirst
      //         case errSecond
      //     }
      //     static var errFirst: MyError.Code { get }
      //     static var errSecond: MyError.Code { get }
      // }
      //
      // The clang enum and enum constants are associated with both the
      // struct/nested enum, and the static vars/enum cases.
      // But we want unique USRs for the above symbols, so use the clang USR
      // for the enum and enum cases, and the Swift USR for the struct and vars.
      //
      if (isa<clang::EnumDecl>(ClangD)) {
        if (ClangD->hasAttr<clang::NSErrorDomainAttr>() && isa<StructDecl>(D))
          return ClangNode();
      } else if (auto *ClangEnumConst = dyn_cast<clang::EnumConstantDecl>(ClangD)) {
        if (auto *ClangEnum = dyn_cast<clang::EnumDecl>(ClangEnumConst->getDeclContext())) {
          if (ClangEnum->hasAttr<clang::NSErrorDomainAttr>() && isa<VarDecl>(D))
            return ClangNode();
        }
      }
    }
    return ClangN;
  };

  if (ClangNode ClangN = interpretAsClangNode(D)) {
    llvm::SmallString<128> Buf;
    if (auto ClangD = ClangN.getAsDecl()) {
      bool Ignore = clang::index::generateUSRForDecl(ClangD, Buf);
      if (!Ignore)
        OS << Buf.str();
      return Ignore;
    }

    auto &Importer = *D->getASTContext().getClangModuleLoader();

    auto ClangMacroInfo = ClangN.getAsMacro();
    bool Ignore = clang::index::generateUSRForMacro(D->getNameStr(),
        ClangMacroInfo->getDefinitionLoc(),
        Importer.getClangASTContext().getSourceManager(), Buf);
    if (!Ignore)
      OS << Buf.str();
    return Ignore;
  }

  if (ShouldUseObjCName(VD)) {
    auto ObjCName = objc_translation::getObjCNameForSwiftDecl(D);
    return printObjCUSR(VD, OS, ObjCName.first, ObjCName.second);
  }

  if (!D->hasInterfaceType())
    return true;

  // FIXME: mangling 'self' in destructors crashes in mangler.
  if (isa<ParamDecl>(VD) && isa<DestructorDecl>(VD->getDeclContext()))
    return true;

  std::string Old = getUSRSpacePrefix().str();
  Mangler Mangler;

  Mangler.bindGenericParameters(VD->getDeclContext());

  if (auto Ctor = dyn_cast<ConstructorDecl>(VD)) {
    Mangler.mangleConstructorEntity(Ctor, /*isAllocating=*/false,
                                    /*uncurryingLevel=*/0);
  } else if (auto Dtor = dyn_cast<DestructorDecl>(VD)) {
    Mangler.mangleDestructorEntity(Dtor, /*isDeallocating=*/false);
  } else if (auto NTD = dyn_cast<NominalTypeDecl>(VD)) {
    Mangler.mangleNominalType(NTD);
  } else if (isa<TypeAliasDecl>(VD) || isa<AssociatedTypeDecl>(VD)) {
    Mangler.mangleContextOf(VD);
    Mangler.mangleDeclName(VD);
  } else {
    Mangler.mangleEntity(VD, /*uncurryingLevel=*/0);
  }

  Old += Mangler.finalize();

  NewMangling::ASTMangler NewMangler;
  std::string New = NewMangler.mangleDeclAsUSR(VD, getUSRSpacePrefix());

  OS << NewMangling::selectMangling(Old, New);

  return false;
}

bool ide::printAccessorUSR(const AbstractStorageDecl *D, AccessorKind AccKind,
                           llvm::raw_ostream &OS) {
  using namespace Mangle;

  // AccKind should always be either IsGetter or IsSetter here, based
  // on whether a reference is a mutating or non-mutating use.  USRs
  // aren't supposed to reflect implementation differences like stored
  // vs. addressed vs. observing.
  //
  // On the other side, the implementation indexer should be
  // registering the getter/setter USRs independently of how they're
  // actually implemented.  So a stored variable should still have
  // getter/setter USRs (pointing to the variable declaration), and an
  // addressed variable should have its "getter" point at the
  // addressor.

  AbstractStorageDecl *SD = const_cast<AbstractStorageDecl*>(D);
  if (ShouldUseObjCName(SD)) {
    switch(AccKind) {
    case AccessorKind::IsGetter:
      return printObjCUSR(SD, OS, Identifier(), SD->getObjCGetterSelector());
    case AccessorKind::IsSetter:
      return printObjCUSR(SD, OS, Identifier(), SD->getObjCSetterSelector());
    default:
      llvm_unreachable("unhandled accessor kind");
    }
  }

  std::string Old = getUSRSpacePrefix().str();
  Mangler Mangler;
  Mangler.mangleAccessorEntity(AccKind, AddressorKind::NotAddressor, SD);
  Old += Mangler.finalize();

  NewMangling::ASTMangler NewMangler;
  std::string New = NewMangler.mangleAccessorEntityAsUSR(AccKind,
                          AddressorKind::NotAddressor, SD, getUSRSpacePrefix());

  OS << NewMangling::selectMangling(Old, New);

  return false;
}

bool ide::printExtensionUSR(const ExtensionDecl *ED, raw_ostream &OS) {
  if (ED->getExtendedType().isNull())
    return true;

  // We make up a unique usr for each extension by combining a prefix
  // and the USR of the first value member of the extension.
  for (auto D : ED->getMembers()) {
    if (auto VD = dyn_cast<ValueDecl>(D)) {
      OS << getUSRSpacePrefix() << "e:";
      return printDeclUSR(VD, OS);
    }
  }
  if (ED->getExtendedType() && ED->getExtendedType()->getAnyNominal()) {
    OS << getUSRSpacePrefix() << "e:";
    printDeclUSR(ED->getExtendedType()->getAnyNominal(), OS);
  } else {
    return true;
  }
  for (auto Inherit : ED->getInherited()) {
    if (auto T = Inherit.getType()) {
      if (T->getAnyNominal())
        return printDeclUSR(T->getAnyNominal(), OS);
    }
  }
  return true;
}

