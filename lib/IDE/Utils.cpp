//===- Utils.cpp - Misc utilities -----------------------------------------===//
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

#include "swift/IDE/Utils.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/Module.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/Parse/Parser.h"
#include "swift/Parse/PersistentParserState.h"
#include "clang/Index/USRGeneration.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace swift;
using namespace ide;

bool ide::isSourceInputComplete(std::unique_ptr<llvm::MemoryBuffer> MemBuf) {
  LangOptions LangOpts;
  SearchPathOptions SearchPathOpts;
  SourceManager SM;
  auto BufferID = SM.addNewSourceBuffer(MemBuf.release());
  DiagnosticEngine Diags(SM);
  ASTContext Ctx(LangOpts, SearchPathOpts, SM, Diags);
  auto ModName = Ctx.getIdentifier("input");

  Module &Mod = *Module::create(ModName, Ctx);
  SourceFile &SF = *new (Ctx) SourceFile(Mod, SourceFileKind::Main, BufferID);

  PersistentParserState PersistentState;
  Parser P(BufferID, SF, /*SIL=*/nullptr, &PersistentState);

  bool Done;
  do {
    P.parseTopLevel();
    Done = P.Tok.is(tok::eof);
  } while (!Done);

  return !P.isInputIncomplete();
}

bool ide::isSourceInputComplete(StringRef Text) {
  std::unique_ptr<llvm::MemoryBuffer> InputBuf;
  InputBuf.reset(llvm::MemoryBuffer::getMemBufferCopy(Text));
  return ide::isSourceInputComplete(std::move(InputBuf));
}

static inline StringRef getUSRSpacePrefix() {
  return "s:";
}

bool ide::printDeclUSR(const ValueDecl *D, raw_ostream &OS) {
  using namespace Mangle;

  ValueDecl *VD = const_cast<ValueDecl*>(D);

  if (ClangNode ClangN = VD->getClangNode()) {
    if (auto ClangD = ClangN.getAsDecl()) {
      llvm::SmallString<128> Buf;
      bool Ignore = clang::index::generateUSRForDecl(ClangD, Buf);
      if (!Ignore)
        OS << Buf.str();
      return Ignore;
    }
    // MacroInfos not handled.
    return true;
  }

  if (isa<SubscriptDecl>(VD))
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
