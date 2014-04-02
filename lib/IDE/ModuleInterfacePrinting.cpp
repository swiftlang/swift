//===--- ModuleInterfacePrinting.h - Routines to print module interface ---===//
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

#include "swift/IDE/ModuleInterfacePrinting.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Serialization/ModuleFile.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/Module.h"
#include "clang/Lex/MacroInfo.h"
#include <utility>

using namespace swift;

static const clang::Module *findTopLevelClangModule(const Module *M) {
  const ClangModuleUnit *CMU = nullptr;
  for (auto *FU : M->getFiles()) {
    if ((CMU = dyn_cast<ClangModuleUnit>(FU)))
      break;
    if (auto *AST = dyn_cast<SerializedASTFile>(FU)) {
      if (auto *ShadowedModule = AST->getFile().getShadowedModule())
        if (auto *Result = findTopLevelClangModule(ShadowedModule))
          return Result;
    }
  }
  if (!CMU)
    return nullptr;
  return CMU->getClangModule();
}

void swift::ide::printModuleInterface(Module *M, ASTPrinter &Printer,
                                      const PrintOptions &Options) {
  printSubmoduleInterface(M, M->Name.str(), Printer, Options);
}

void swift::ide::printSubmoduleInterface(Module *M,
                                         ArrayRef<StringRef> FullModuleName,
                                         ASTPrinter &Printer,
                                         const PrintOptions &Options) {
  auto AdjustedOptions = Options;

  // Don't print empty curly braces while printing the module interface.
  AdjustedOptions.FunctionDefinitions = false;

  AdjustedOptions.PrintGetSetOnRWProperties = false;

  // Print var declarations separately, one variable per decl.
  AdjustedOptions.ExplodePatternBindingDecls = true;
  AdjustedOptions.VarInitializers = false;

  AdjustedOptions.PrintDefaultParameterPlaceholder = true;

  SmallVector<Decl *, 1> Decls;
  M->getDisplayDecls(Decls);

  auto &SwiftContext = M->Ctx;
  auto &Importer =
      static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());

  const clang::Module *InterestingClangModule = nullptr;

  SmallVector<ImportDecl *, 1> ImportDecls;
  SmallVector<Decl *, 1> SwiftDecls;
  SmallVector<std::pair<Decl *, clang::SourceLocation>, 1> ClangDecls;

  // Drop top-level module name.
  FullModuleName = FullModuleName.slice(1);

  InterestingClangModule = findTopLevelClangModule(M);
  if (InterestingClangModule) {
    for (StringRef Name : FullModuleName) {
      InterestingClangModule = InterestingClangModule->findSubmodule(Name);
      if (!InterestingClangModule)
        return;
    }
  } else {
    assert(FullModuleName.empty());
  }

  // Separate the declarations that we are going to print into different
  // buckets.
  for (Decl *D : Decls) {
    if (auto ID = dyn_cast<ImportDecl>(D)) {
      ImportDecls.push_back(ID);
      continue;
    }
    if (auto CN = D->getClangNode()) {
      clang::SourceLocation Loc;
      if (auto *CD = CN.getAsDecl()) {
        Loc = CD->getLocation();
      } else {
        Loc = CN.getAsMacro()->getDefinitionLoc();
      }
      if (Importer.getClangOwningModule(CN) == InterestingClangModule)
        ClangDecls.push_back({ D, Loc });
      continue;
    }
    SwiftDecls.push_back(D);
  }

  auto &ClangSourceManager = Importer.getClangASTContext().getSourceManager();

  // Sort imported declarations in source order.
  std::sort(ClangDecls.begin(), ClangDecls.end(),
            [&](std::pair<Decl *, clang::SourceLocation> LHS,
                std::pair<Decl *, clang::SourceLocation> RHS) -> bool {
    return ClangSourceManager.isBeforeInTranslationUnit(LHS.second,
                                                        RHS.second);
  });

  // Sort Swift declarations so that we print them in a consistent order.
  std::sort(ImportDecls.begin(), ImportDecls.end(),
            [](ImportDecl *LHS, ImportDecl *RHS) -> bool {
    auto LHSPath = LHS->getFullAccessPath();
    auto RHSPath = RHS->getFullAccessPath();
    for (unsigned i = 0, e = std::min(LHSPath.size(), RHSPath.size()); i != e;
         i++) {
      if (int Ret = LHSPath[i].first.str().compare(RHSPath[i].first.str()))
        return Ret < 0;
    }
    return false;
  });

  std::sort(SwiftDecls.begin(), SwiftDecls.end(),
            [](Decl *LHS, Decl *RHS) -> bool {
    auto *LHSValue = dyn_cast<ValueDecl>(LHS);
    auto *RHSValue = dyn_cast<ValueDecl>(RHS);
    if (LHSValue && RHSValue) {
      StringRef LHSName = LHSValue->getName().str();
      StringRef RHSName = RHSValue->getName().str();
      if (int Ret = LHSName.compare(RHSName))
        return Ret < 0;
      // FIXME: this is not sufficient to establish a total order for overloaded
      // decls.
      return LHS->getKind() < RHS->getKind();
    }

    return LHS->getKind() < RHS->getKind();
  });

  auto PrintDecl = [&](Decl *D) {
    if (isa<ExtensionDecl>(D))
      return;

    D->print(Printer, AdjustedOptions);
    Printer << "\n";
    if (auto NTD = dyn_cast<NominalTypeDecl>(D)) {
      for (auto Ext : NTD->getExtensions()) {
        Ext->print(Printer, AdjustedOptions);
        Printer << "\n";
      }
    }
  };

  for (auto *D : ImportDecls)
    PrintDecl(D);

  for (auto DeclAndLoc : ClangDecls)
    PrintDecl(DeclAndLoc.first);

  for (auto *D : SwiftDecls)
    PrintDecl(D);
}

