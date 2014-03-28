#include "swift/IDE/ModuleInterfacePrinting.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Lex/MacroInfo.h"
#include <utility>

using namespace swift;

void swift::ide::printModuleInterface(Module *M, ASTPrinter &Printer,
                                      const PrintOptions &Options) {
  auto AdjustedOptions = Options;

  // Don't print empty curly braces while printing the module interface.
  AdjustedOptions.FunctionDefinitions = false;

  AdjustedOptions.PrintGetSetOnRWProperties = false;

  // Print var declarations separately, one variable per decl.
  AdjustedOptions.ExplodePatternBindingDecls = true;
  AdjustedOptions.VarInitializers = false;

  AdjustedOptions.PrintDocumentationComments = true;

  SmallVector<Decl *, 1> Decls;
  M->getDisplayDecls(Decls);

  SmallVector<ImportDecl *, 1> ImportDecls;
  SmallVector<Decl *, 1> SwiftDecls;
  SmallVector<std::pair<Decl *, clang::SourceLocation>, 1> ClangDecls;
  for (Decl *D : Decls) {
    if (auto ID = dyn_cast<ImportDecl>(D))
      ImportDecls.push_back(ID);
    if (auto *VD = dyn_cast<ValueDecl>(D)) {
      if (auto *CD = VD->getClangDecl()) {
        ClangDecls.push_back({ VD, CD->getLocation() });
      } else if (auto *CMI = VD->getClangMacro()) {
        ClangDecls.push_back({ VD, CMI->getDefinitionLoc() });
      } else {
        SwiftDecls.push_back(VD);
      }
    }
  }

  auto &SwiftContext = M->Ctx;
  auto &Importer =
      static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());
  auto &ClangSourceManager = Importer.getClangASTContext().getSourceManager();

  std::sort(ClangDecls.begin(), ClangDecls.end(),
            [&](std::pair<Decl *, clang::SourceLocation> LHS,
                std::pair<Decl *, clang::SourceLocation> RHS) -> bool {
    return ClangSourceManager.isBeforeInTranslationUnit(LHS.second, RHS.second);
  });

  // Sort the declarations so that we print them in a consistent order.
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

