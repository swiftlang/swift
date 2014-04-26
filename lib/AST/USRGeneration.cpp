//===--- USRGeneration.h - Routines for USR generation --------------------===//
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

#include "swift/AST/ASTContext.h"
#include "swift/AST/USRGeneration.h"
#include "swift/AST/Mangle.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/AST/ASTContext.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Lex/PreprocessingRecord.h"
#include "clang/Lex/Preprocessor.h"

using namespace swift;
using namespace ide;

static inline StringRef getUSRSpacePrefix() {
  return "s:";
}

bool ide::printDeclUSR(const ValueDecl *D, raw_ostream &OS) {
  using namespace Mangle;

  ValueDecl *VD = const_cast<ValueDecl *>(D);

  if (ClangNode ClangN = VD->getClangNode()) {
    llvm::SmallString<128> Buf;
    if (auto ClangD = ClangN.getAsDecl()) {
      bool Ignore = clang::index::generateUSRForDecl(ClangD, Buf);
      if (!Ignore)
        OS << Buf.str();
      return Ignore;
    }

    auto &Importer = *D->getASTContext().getClangModuleLoader();

    auto ClangMacroInfo = ClangN.getAsMacro();
    auto PPRecord = Importer.getClangPreprocessor().getPreprocessingRecord();
    assert(PPRecord && "Clang importer should be created with "
                       "-detailed-preprocessing-record option");
    auto ClangMacroDef = PPRecord->findMacroDefinition(ClangMacroInfo);

    bool Ignore = clang::index::generateUSRForMacro(
        ClangMacroDef, Importer.getClangASTContext().getSourceManager(), Buf);
    if (!Ignore)
      OS << Buf.str();
    return Ignore;
  }

  // FIXME: mangling 'self' in destructors crashes in mangler.
  if (isa<ParamDecl>(VD) && isa<DestructorDecl>(VD->getDeclContext()))
      return true;

  OS << getUSRSpacePrefix();
  Mangler Mangler(OS);
  if (auto Ctor = dyn_cast<ConstructorDecl>(VD)) {
    Mangler.mangleConstructorEntity(Ctor, /*isAllocating=*/false,
        ResilienceExpansion::Minimal, /*uncurryingLevel=*/0);
  } else if (auto Dtor = dyn_cast<DestructorDecl>(VD)) {
    Mangler.mangleDestructorEntity(Dtor, /*isDeallocating=*/false);
  } else if (auto NTD = dyn_cast<NominalTypeDecl>(VD)) {
    Mangler.mangleNominalType(NTD, ResilienceExpansion::Minimal,
                              Mangler::BindGenerics::None);
  } else if (isa<TypeAliasDecl>(VD) || isa<AssociatedTypeDecl>(VD)) {
    Mangler.mangleContextOf(VD, Mangler::BindGenerics::None);
    Mangler.mangleDeclName(VD);
  } else {
    Mangler.mangleEntity(VD, ResilienceExpansion::Minimal,
                         /*uncurryingLevel=*/0);
  }
  return false;
}

bool ide::printAccessorUSR(const AbstractStorageDecl *D, AccessorKind AccKind,
                           llvm::raw_ostream &OS) {
  using namespace Mangle;

  AbstractStorageDecl *SD = const_cast<AbstractStorageDecl*>(D);
  OS << getUSRSpacePrefix();
  Mangler Mangler(OS);
  Mangler.mangleAccessorEntity(AccKind, SD, ResilienceExpansion::Minimal);
  return false;
}

