//===--- PolyglotAST.cpp - Routines to print module interface -===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024-2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/JSON.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Frontend/CompilerInstance.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/PrintOptions.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "ImporterImpl.h"

using namespace swift;

class PolyglotAPIVisitor : public clang::ConstDeclVisitor<PolyglotAPIVisitor> {
  ClangImporter &clangImporter;
  PrintOptions printOptions;

  // JSON output stream.
  llvm::json::OStream &Json;

  // Container currently visited.
  const clang::ObjCContainerDecl *openContainerDecl = nullptr;

  void PrintCommonAttributes(const clang::NamedDecl* decl) {
    Json.attribute("declKind", decl->getDeclKindName());
    Json.attribute("diagName", decl->getQualifiedNameAsString());
  }

  std::string GetText(clang::SourceRange range) {
    auto &clangSM = clangImporter.getClangInstance().getSourceManager();
    auto &langOpts = clangImporter.getClangASTContext().getLangOpts();
    auto clangRW = clang::Rewriter(clangSM, langOpts);
    return clangRW.getRewrittenText(range);
  }

  std::optional<const clang::NamedDecl *>
      FindInterfaceDeclaration(const clang::ObjCMethodDecl *decl) {
    auto prop = decl->findPropertyDecl();
    if (prop) {
      // Look for the property declaring this accessor. Used for `description`.
      Json.attribute("debug", "note: findPropertyDecl path");
      return prop;
    } else if (decl->isOverriding()) {
      // Look for inherited decls being overriden.
      Json.attribute("debug", "note: isOverriding path");
      auto overridens = SmallVector<const clang::ObjCMethodDecl*>();
      decl->getOverriddenMethods(overridens);
      decl = *overridens.rbegin();
      return decl;
    } else {
      // Look for local decls introduced in this class.
      Json.attribute("debug", "note: getClassInterface path");
      auto interface = decl->getClassInterface();
      auto sel = decl->getSelector();
      bool isInterfaceMethod = decl->isInstanceMethod();
      assert(interface);
      auto sup = interface;
      while (sup) {
        auto localDecl = sup->getMethod(sel, isInterfaceMethod);
        if (localDecl) {
          return localDecl;
        }
        sup = sup->getSuperClass();
      }
    }

    return std::nullopt;
  }

public:
  PolyglotAPIVisitor(ClangImporter &clangImporter, llvm::json::OStream &Json):
      clangImporter(clangImporter), Json(Json)
  {
    printOptions = PrintOptions::printForDiagnostics(AccessLevel::FilePrivate,
                                                  /*printFullConvention*/true);
    printOptions.PrintOverrideKeyword = true;
    printOptions.PrintPropertyAccessors = false;
    printOptions.PrintAccess = true;
    printOptions.SkipAttributes = true;
  }

  void VisitDecl(const clang::Decl *decl) {
    Json.object([&] {
      Json.attribute("formatKind", "unhandled");
      Json.attribute("debug", "Unhandled kind");
      Json.attribute("fullText", GetText(decl->getSourceRange()));
    });
  }

  void VisitObjCContainerDecl(const clang::ObjCContainerDecl *decl) {
    Json.object([&] {
      PrintCommonAttributes(decl);
      Json.attribute("formatKind", "container");

      StringRef name = decl->getName();
      if (auto cd = dyn_cast<clang::ObjCCategoryDecl>(decl)) {
        auto interface = cd->getClassInterface();
        name = interface->getIdentifier()->getName();
      }
      Json.attribute("className", name);

      openContainerDecl = decl;

      Json.attributeArray("properties", [&] {
        for (auto prop : decl->properties()) {
          if (prop->isImplicit())
            continue;
          VisitObjCPropertyDecl(prop);
        }
      });

      Json.attributeArray("methods", [&] {
        for (auto meth : decl->methods()) {
          if (meth->isImplicit())
            continue;
          VisitObjCMethodDecl(meth);
        }
      });

      openContainerDecl = nullptr;
    });
  }

  void VisitObjCMethodDecl(const clang::ObjCMethodDecl *decl) {
    // Skip methods accessed directly, we look at them through the container.
    if (!openContainerDecl)
      return;

    Json.object([&] {
      PrintCommonAttributes(decl);
      Json.attribute("formatKind", "method");

      clang::Selector sel = decl->getSelector();
      Json.attribute("selector", sel.getAsString());

      auto start = decl->getDeclaratorEndLoc();
      auto end = decl->getBodyRBrace();
      if (end.isValid())
        Json.attribute("body", GetText(clang::SourceRange(start, end)));
      else
        Json.attribute("body", nullptr);

      // Find the declaration decl.
      auto declRef =
                FindInterfaceDeclaration(decl);
      if (!declRef) {
        Json.attribute("debug", "error: declaration in interfaces not found");
        return;
      }

      // Import to Swift.
      Decl *swiftDecl = clangImporter.importDeclDirectly(*declRef);
      if (!swiftDecl) {
        Json.attribute("debug", "error: import failed");
      } else if (auto VD = dyn_cast<ValueDecl>(swiftDecl)) {
        llvm::SmallString<64> storage;
        llvm::raw_svector_ostream OS(storage);

        SourceLoc loc = SourceLoc();
        DeclContext *adopter = VD->getDeclContext();
        Type adopterTy = adopter->getSelfTypeInContext();

        swift::printRequirementStub(VD, adopter, adopterTy,
                                    loc, OS,
                                    /*objCAttr=*/true);

        Json.attribute("swiftSignature", storage);
      } else {
        auto declKind = swiftDecl->getKindName(swiftDecl->getKind());
        Json.attribute("debug",
            ("error: unhandled kind after import: " + declKind).str());
      }
    });
  }

  void VisitObjCPropertyDecl(const clang::ObjCPropertyDecl *decl) {
    Json.object([&] {
      PrintCommonAttributes(decl);
      Json.attribute("formatKind", "property");

      Decl *swiftDecl = clangImporter.importDeclDirectly(decl);
      if (swiftDecl) {
        llvm::SmallString<64> storage;
        llvm::raw_svector_ostream OS(storage);
        swiftDecl->print(OS, printOptions);
        Json.attribute("swiftSignature", storage);
      } else {
        Json.attribute("debug", "error: import failed");
      }
    });
  }
};

void ClangImporter::Implementation::lookupImplementationFileDecls(
         llvm::function_ref<bool(ClangNode)> filter,
         llvm::function_ref<void(const clang::Decl *)> receiver,
         llvm::function_ref<void(const swift::ImportDecl *)> receiverImports
     ) const {
  for (auto &Import : BridgeHeaderTopLevelImports) {
    if (const auto *ImportD = Import.dyn_cast<swift::ImportDecl*>()) {
      receiverImports(ImportD);
    }
  }

  for (auto *ClangD : BridgeHeaderTopLevelDecls) {
    if (filter(ClangD)) {
      receiver(ClangD);
    }
  }

  // We can also cover Impl.getClangPreprocessor() here.
}

llvm::Error
ClangImporter::Implementation::lookupImplementationFileDeclsFromFile(
    StringRef Filename,
    llvm::function_ref<void(const clang::Decl*)> receiver,
    llvm::function_ref<void(const swift::ImportDecl *)> receiverImports)
  const {
  llvm::Expected<clang::FileEntryRef> ExpectedFile =
      getClangPreprocessor().getFileManager().getFileRef(Filename);
  if (!ExpectedFile) {
    return ExpectedFile.takeError();
  }
  clang::FileEntryRef File = *ExpectedFile;

  auto &ClangCtx = getClangASTContext();
  auto &ClangSM = ClangCtx.getSourceManager();

  // Look up the header in the includes of the bridging header.
  if (BridgeHeaderFiles.count(File)) {
    auto headerFilter = [&](ClangNode ClangN) -> bool {
      if (ClangN.isNull())
        return false;

      auto ClangLoc = ClangSM.getFileLoc(ClangN.getLocation());
      if (ClangLoc.isInvalid())
        return false;

      clang::OptionalFileEntryRef LocRef =
          ClangSM.getFileEntryRefForID(ClangSM.getFileID(ClangLoc));
      if (!LocRef || *LocRef != File)
        return false;

      return true;
    };

    lookupImplementationFileDecls(headerFilter, receiver, receiverImports);
    return llvm::Error::success();
  }

  return llvm::make_error<llvm::StringError>(
      "No information for target file",
      llvm::inconvertibleErrorCode());
}

void
ClangImporter::printPolyglotAST(StringRef Filename, raw_ostream &OS) {
  SmallVector<const clang::Decl *, 32> ClangDecls;
  auto headerReceiver = [&](const clang::Decl *D) {
      ClangDecls.push_back(D);
  };

  SmallVector<const swift::ImportDecl *, 32> Imports;
  auto importReceiver = [&](const swift::ImportDecl *D) {
      Imports.push_back(D);
  };

  auto error = Impl.lookupImplementationFileDeclsFromFile(Filename,
                                               headerReceiver, importReceiver);
  // Even if there's an error here, keep on going. No actual work will be
  // executed but we'll still generate the JSON output.

  // Sort imported declarations in source order.
  auto &ClangSM = getClangASTContext().getSourceManager();
  std::sort(ClangDecls.begin(), ClangDecls.end(),
            [&](const clang::Decl *LHS, const clang::Decl *RHS) -> bool {
              return ClangSM.isBeforeInTranslationUnit(
                                            LHS->getLocation(),
                                            RHS->getLocation());
            });

  auto Json = llvm::json::OStream(OS, /*indent*/2);
  Json.object([&] {
    Json.attribute("formatVersion", 1);
    Json.attribute("compilerVersion", version::getSwiftFullVersion());
    Json.attributeArray("topLevelDecls", [&] {
      for (auto *ID : Imports) {
        Json.object([&] {
          Json.attribute("formatKind", "import");
          if (ID->getModule()) {
            Json.attribute("target", ID->getModule()->getName().str());
          } else {
            auto clangImport = cast<clang::ImportDecl>(ID->getClangDecl());
            auto target = clangImport->getImportedModule();
            Json.attribute("target", target->getTopLevelModuleName());
          }
        });
      }
      for (auto *D : ClangDecls) {
        PolyglotAPIVisitor(*this, Json).Visit(D);
      }
    });

    if (error) {
      Json.attribute("error", toString(std::move(error)));
    }
  });
}
