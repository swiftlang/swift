#include "swift/IDE/ModuleInterfacePrinting.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/Module.h"
#include "clang/Lex/MacroInfo.h"
#include <utility>

using namespace swift;

void swift::ide::printModuleInterface(Module *M, ASTPrinter &Printer,
                                      const PrintOptions &Options) {
  StringRef FullModuleName[] = { M->Name.str() };
  printSubmoduleInterface(M, FullModuleName, Printer, Options);
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

  SmallVector<Decl *, 1> Decls;
  M->getDisplayDecls(Decls);

  auto &SwiftContext = M->Ctx;
  auto &Importer =
      static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());

  SmallVector<ImportDecl *, 1> ImportDecls;
  SmallVector<Decl *, 1> SwiftDecls;
  llvm::DenseMap<const clang::Module *,
                 SmallVector<std::pair<Decl *, clang::SourceLocation>, 1> >
  ClangDecls;

  // Drop top-level module name.
  FullModuleName = FullModuleName.slice(1);

  bool WholeModule = FullModuleName.empty();
  if (!WholeModule) {
    // Find the top-level Clang module.
    ClangModuleUnit *CMU = nullptr;
    for (auto *FU : M->getFiles()) {
      if ((CMU = dyn_cast<ClangModuleUnit>(FU)))
        break;
    }
    const clang::Module *TopLevel = CMU->getClangModule();
    for (StringRef Name : FullModuleName) {
      TopLevel = TopLevel->findSubmodule(Name);
      if (!TopLevel)
        return;
    }

    // Find all submodules to print.
    SmallVector<const clang::Module *, 8> Worklist;
    Worklist.push_back(TopLevel);
    while (!Worklist.empty()) {
      const clang::Module *CM = Worklist.pop_back_val();
      if (!TopLevel->isModuleVisible(CM))
        return;
      ClangDecls.insert({ CM, {} });
      Worklist.append(CM->submodule_begin(), CM->submodule_end());
    }
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
      auto *OwningModule = Importer.getClangOwningModule(CN);
      if (WholeModule) {
        ClangDecls[OwningModule].push_back({ D, Loc });
      } else {
        auto I = ClangDecls.find(OwningModule);
        if (I != ClangDecls.end())
          I->second.push_back({ D, Loc });
      }
      continue;
    }
    SwiftDecls.push_back(D);
  }

  auto &ClangSourceManager = Importer.getClangASTContext().getSourceManager();

  // Sort imported declarations in source order *within a submodule*.
  for (auto &P : ClangDecls) {
    std::sort(P.second.begin(), P.second.end(),
              [&](std::pair<Decl *, clang::SourceLocation> LHS,
                  std::pair<Decl *, clang::SourceLocation> RHS)
                  -> bool {
      return ClangSourceManager.isBeforeInTranslationUnit(LHS.second,
                                                          RHS.second);
    });
  }

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

  {
    using ModuleAndName = std::pair<const clang::Module *, std::string>;
    SmallVector<ModuleAndName, 8> ClangModules;
    for (auto P : ClangDecls) {
      ClangModules.push_back({ P.first, P.first->getFullModuleName() });
    }

    // Sort modules by name.
    std::sort(ClangModules.begin(), ClangModules.end(),
              [](const ModuleAndName &LHS, const ModuleAndName &RHS)
                  -> bool {
      return LHS.second < RHS.second;
    });

    for (auto CM : ClangModules) {
      for (auto DeclAndLoc : ClangDecls[CM.first])
        PrintDecl(DeclAndLoc.first);
    }
  }

  for (auto *D : SwiftDecls)
    PrintDecl(D);
}

