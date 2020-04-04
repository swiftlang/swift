//===--- ModuleInterfacePrinting.cpp - Routines to print module interface -===//
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

#include "swift/IDE/ModuleInterfacePrinting.h"
#include "swift/IDE/Utils.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTDemangler.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/PrimitiveParsing.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Parse/Token.h"
#include "swift/Subsystems.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/Module.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/MacroInfo.h"
#include "clang/Lex/Preprocessor.h"
#include <algorithm>
#include <memory>
#include <queue>
#include <string>
#include <utility>
#include <vector>

using namespace swift;

namespace {
/// Prints regular comments from clang module headers.
class ClangCommentPrinter : public ASTPrinter {
public:
  ClangCommentPrinter(ASTPrinter &OtherPrinter, ClangModuleLoader &ClangLoader)
    : OtherPrinter(OtherPrinter),
      ClangLoader(ClangLoader) {}

private:
  void printDeclPre(const Decl *D, Optional<BracketOptions> Bracket) override;
  void printDeclPost(const Decl *D, Optional<BracketOptions> Bracket) override;
  void avoidPrintDeclPost(const Decl *D) override;
  // Forwarding implementations.

  void printText(StringRef Text) override {
    return OtherPrinter.printText(Text);
  }
  void printDeclLoc(const Decl *D) override {
    return OtherPrinter.printDeclLoc(D);
  }
  void printDeclNameEndLoc(const Decl *D) override {
    return OtherPrinter.printDeclNameEndLoc(D);
  }
  void printDeclNameOrSignatureEndLoc(const Decl *D) override {
    return OtherPrinter.printDeclNameOrSignatureEndLoc(D);
  }
  void printTypePre(const TypeLoc &TL) override {
    return OtherPrinter.printTypePre(TL);
  }
  void printTypePost(const TypeLoc &TL) override {
    return OtherPrinter.printTypePost(TL);
  }
  void printTypeRef(Type T, const TypeDecl *TD, Identifier Name,
                    PrintNameContext NameContext) override {
    return OtherPrinter.printTypeRef(T, TD, Name, NameContext);
  }
  void printModuleRef(ModuleEntity Mod, Identifier Name) override {
    return OtherPrinter.printModuleRef(Mod, Name);
  }
  void printSynthesizedExtensionPre(const ExtensionDecl *ED,
                                    TypeOrExtensionDecl Target,
                                    Optional<BracketOptions> Bracket) override {
    return OtherPrinter.printSynthesizedExtensionPre(ED, Target, Bracket);
  }

  void
  printSynthesizedExtensionPost(const ExtensionDecl *ED,
                                TypeOrExtensionDecl Target,
                                Optional<BracketOptions> Bracket) override {
    return OtherPrinter.printSynthesizedExtensionPost(ED, Target, Bracket);
  }

  void printStructurePre(PrintStructureKind Kind, const Decl *D) override {
    return OtherPrinter.printStructurePre(Kind, D);
  }
  void printStructurePost(PrintStructureKind Kind, const Decl *D) override {
    return OtherPrinter.printStructurePost(Kind, D);
  }

  void printNamePre(PrintNameContext Context) override {
    return OtherPrinter.printNamePre(Context);
  }
  void printNamePost(PrintNameContext Context) override {
    return OtherPrinter.printNamePost(Context);
  }

  // Prints regular comments of the header the clang node comes from, until
  // the location of the node. Keeps track of the comments that were printed
  // from the file and resumes printing for the next node from the same file.
  // This expects to get passed clang nodes in source-order (at least within the
  // same header).
  void printCommentsUntil(ClangNode Node);

  void printComment(StringRef Text, unsigned StartLocCol);

  bool isDocumentationComment(clang::SourceLocation CommentLoc,
                              ClangNode Node) const;

  unsigned getResumeOffset(clang::FileID FID) const {
    auto OffsI = ResumeOffsets.find(FID);
    if (OffsI != ResumeOffsets.end())
      return OffsI->second;
    return 0;
  }
  void setResumeOffset(clang::FileID FID, unsigned Offset) {
    ResumeOffsets[FID] = Offset;
  }

  bool shouldPrintNewLineBefore(ClangNode Node) const;
  void updateLastEntityLine(clang::SourceLocation Loc);
  void updateLastEntityLine(clang::FileID FID, unsigned LineNo);

  ASTPrinter &OtherPrinter;
  ClangModuleLoader &ClangLoader;
  llvm::DenseMap<clang::FileID, unsigned> ResumeOffsets;
  SmallVector<StringRef, 2> PendingComments;
  llvm::DenseMap<clang::FileID, unsigned> LastEntityLines;
};
} // unnamed namespace

static const clang::Module *
getUnderlyingClangModuleForImport(ImportDecl *Import) {
  if (auto *ClangMod = Import->getClangModule())
    return ClangMod;

  if (auto Mod = Import->getModule())
    if (auto *ClangMod = Mod->findUnderlyingClangModule())
      return ClangMod;

  return nullptr;
}

static void printTypeNameToString(Type Ty, std::string &Text) {
  SmallString<128> Buffer;
  llvm::raw_svector_ostream OS(Buffer);
  Ty->print(OS);
  Text = std::string(OS.str());
}

bool swift::ide::
printTypeInterface(ModuleDecl *M, Type Ty, ASTPrinter &Printer,
                   std::string &TypeName, std::string &Error) {
  if (!Ty) {
    if (Error.empty())
      Error = "type cannot be null.";
    return true;
  }
  Ty = Ty->getRValueType();
  if (auto ND = Ty->getNominalOrBoundGenericNominal()) {
    PrintOptions Options = PrintOptions::printTypeInterface(Ty.getPointer());
    ND->print(Printer, Options);
    printTypeNameToString(Ty, TypeName);
    return false;
  }
  Error = "cannot find declaration of type.";
  return true;
}

bool swift::ide::
printTypeInterface(ModuleDecl *M, StringRef TypeUSR, ASTPrinter &Printer,
                   std::string &TypeName, std::string &Error) {
  return printTypeInterface(M, Demangle::getTypeForMangling(M->getASTContext(),
                                                            TypeUSR),
                            Printer, TypeName, Error);
}

void swift::ide::printModuleInterface(ModuleDecl *M, Optional<StringRef> Group,
                                      ModuleTraversalOptions TraversalOptions,
                                      ASTPrinter &Printer,
                                      const PrintOptions &Options,
                                      const bool PrintSynthesizedExtensions) {
  printSubmoduleInterface(M, M->getName().str(),
                          Group.hasValue() ? Group.getValue() : ArrayRef<StringRef>(),
                          TraversalOptions, Printer, Options,
                          PrintSynthesizedExtensions);
}

static void adjustPrintOptions(PrintOptions &AdjustedOptions) {
  // Don't print empty curly braces while printing the module interface.
  AdjustedOptions.FunctionDefinitions = false;

  AdjustedOptions.PrintGetSetOnRWProperties = false;

  // Print var declarations separately, one variable per decl.
  AdjustedOptions.ExplodePatternBindingDecls = true;
  AdjustedOptions.VarInitializers = false;
}

ArrayRef<StringRef>
swift::ide::collectModuleGroups(ModuleDecl *M, std::vector<StringRef> &Scratch) {
  for (auto File : M->getFiles()) {
    File->collectAllGroups(Scratch);
  }
  std::sort(Scratch.begin(), Scratch.end(), [](StringRef L, StringRef R) {
    return L.compare_lower(R) < 0;
  });
  return llvm::makeArrayRef(Scratch);
}

/// Determine whether the given extension has a Clang node that
/// created it (vs. being a Swift extension).
static bool extensionHasClangNode(ExtensionDecl *ext) {
  return static_cast<bool>(swift::ide::extensionGetClangNode(ext));
}

Optional<StringRef>
swift::ide::findGroupNameForUSR(ModuleDecl *M, StringRef USR) {
  for (auto File : M->getFiles()) {
    if (auto Name = File->getGroupNameByUSR(USR)) {
      return Name;
    }
  }
  return None;
}

/// Prints a single decl using the \p Printer and \p Options provided.
///
/// \returns Whether the given decl was printed.
static bool printModuleInterfaceDecl(Decl *D,
                                     ASTPrinter &Printer,
                                     PrintOptions &Options,
                                     bool PrintSynthesizedExtensions) {
  if (!Options.shouldPrint(D)) {
    Printer.callAvoidPrintDeclPost(D);
    return false;
  }
  if (auto Ext = dyn_cast<ExtensionDecl>(D)) {
    // Clang extensions (categories) are always printed in source order.
    // Swift extensions are printed with their associated type unless it's
    // a cross-module extension.
    if (!extensionHasClangNode(Ext)) {
      auto ExtendedNominal = Ext->getExtendedNominal();
      if (Ext->getModuleContext() == ExtendedNominal->getModuleContext())
        return false;
    }
  }
  std::unique_ptr<SynthesizedExtensionAnalyzer> pAnalyzer;
  if (auto NTD = dyn_cast<NominalTypeDecl>(D)) {
    if (PrintSynthesizedExtensions) {
      pAnalyzer.reset(new SynthesizedExtensionAnalyzer(NTD, Options));
      Options.BracketOptions = {
        NTD, true, true,
        !pAnalyzer->hasMergeGroup(
          SynthesizedExtensionAnalyzer::MergeGroupKind::MergeableWithTypeDef
        )
      };
    }
  }
  if (D->print(Printer, Options)) {
    if (Options.BracketOptions.shouldCloseNominal(D))
      Printer << "\n";
    Options.BracketOptions = BracketOptions();
    if (auto NTD = dyn_cast<NominalTypeDecl>(D)) {
      std::queue<NominalTypeDecl *> SubDecls{{NTD}};

      while (!SubDecls.empty()) {
        auto NTD = SubDecls.front();
        SubDecls.pop();

        // Add sub-types of NTD.
        for (auto Sub : NTD->getMembers())
          if (auto N = dyn_cast<NominalTypeDecl>(Sub))
            SubDecls.push(N);

        // Print Ext and add sub-types of Ext.
        for (auto Ext : NTD->getExtensions()) {
          if (!PrintSynthesizedExtensions) {
            if (!Options.shouldPrint(Ext)) {
              Printer.callAvoidPrintDeclPost(Ext);
              continue;
            }
            if (extensionHasClangNode(Ext))
              continue; // will be printed in its source location, see above.
            Printer << "\n";
            Ext->print(Printer, Options);
            Printer << "\n";
          }
          for (auto Sub : Ext->getMembers())
            if (auto N = dyn_cast<NominalTypeDecl>(Sub))
              SubDecls.push(N);
        }
        if (!PrintSynthesizedExtensions)
          continue;

        bool IsTopLevelDecl = D == NTD;

        // If printed Decl is the top-level, merge the constraint-free extensions
        // into the main body.
        if (IsTopLevelDecl) {
          // Print the part that should be merged with the type decl.
          pAnalyzer->forEachExtensionMergeGroup(
            SynthesizedExtensionAnalyzer::MergeGroupKind::MergeableWithTypeDef,
            [&](ArrayRef<ExtensionInfo> Decls) {
              for (auto ET : Decls) {
                Options.BracketOptions = {
                  ET.Ext, false, Decls.back().Ext == ET.Ext, true
                };
                if (ET.IsSynthesized)
                  Options.initForSynthesizedExtension(NTD);
                ET.Ext->print(Printer, Options);
                if (ET.IsSynthesized)
                  Options.clearSynthesizedExtension();
                if (Options.BracketOptions.shouldCloseExtension(ET.Ext))
                  Printer << "\n";
              }
            });
        }

        // If the printed Decl is not the top-level one, reset analyzer.
        if (!IsTopLevelDecl)
          pAnalyzer.reset(new SynthesizedExtensionAnalyzer(NTD, Options));

        // Print the rest as synthesized extensions.
        pAnalyzer->forEachExtensionMergeGroup(
          // For top-level decls, only constraint extensions need to be
          // printed, since the rest are merged into the main body.
          IsTopLevelDecl
          ? SynthesizedExtensionAnalyzer::MergeGroupKind::UnmergeableWithTypeDef
          : SynthesizedExtensionAnalyzer::MergeGroupKind::All,
          [&](ArrayRef<ExtensionInfo> Decls) {
            // Whether we've started the extension merge group in printing.
            bool Opened = false;
            for (auto ET : Decls) {
              Options.BracketOptions = {
                ET.Ext, !Opened, Decls.back().Ext == ET.Ext, true
              };
              if (Options.BracketOptions.shouldOpenExtension(ET.Ext))
                Printer << "\n";
              if (ET.IsSynthesized) {
                if (ET.EnablingExt)
                  Options.initForSynthesizedExtension(ET.EnablingExt);
                else
                  Options.initForSynthesizedExtension(NTD);
              }
              // Set opened if we actually printed this extension.
              Opened |= ET.Ext->print(Printer, Options);
              if (ET.IsSynthesized)
                Options.clearSynthesizedExtension();
              if (Options.BracketOptions.shouldCloseExtension(ET.Ext))
                Printer << "\n";
            }
          });
        Options.BracketOptions = BracketOptions();
      }
    }
    return true;
  }
  return false;
}

/// Sorts import declarations for display.
static bool compareImports(ImportDecl *LHS, ImportDecl *RHS) {
  auto LHSPath = LHS->getFullAccessPath();
  auto RHSPath = RHS->getFullAccessPath();
  for (unsigned i: range(std::min(LHSPath.size(), RHSPath.size()))) {
    if (int Ret = LHSPath[i].Item.str().compare(RHSPath[i].Item.str()))
      return Ret < 0;
  }
  return false;
};

/// Sorts Swift declarations for display.
static bool compareSwiftDecls(Decl *LHS, Decl *RHS) {
  auto *LHSValue = dyn_cast<ValueDecl>(LHS);
  auto *RHSValue = dyn_cast<ValueDecl>(RHS);

  if (LHSValue && RHSValue) {
    auto LHSName = LHSValue->getBaseName();
    auto RHSName = RHSValue->getBaseName();
    if (int Ret = LHSName.compare(RHSName))
      return Ret < 0;
    // FIXME: not sufficient to establish a total order for overloaded decls.
  }
  return LHS->getKind() < RHS->getKind();
};

static std::pair<ArrayRef<Decl*>, ArrayRef<Decl*>>
getDeclsFromOverlay(ModuleDecl *Overlay, ModuleDecl *Underlying,
                    SmallVectorImpl<Decl *> &Decls, AccessLevel AccessFilter) {
  Overlay->getDisplayDecls(Decls);

  // Collector the imports of the underlying module so we can filter them out.
  SmallPtrSet<ModuleDecl *, 8> PrevImported;
  SmallVector<Decl*, 1> UnderlyingDecls;
  Underlying->getDisplayDecls(UnderlyingDecls);
  for (auto *D: UnderlyingDecls) {
    if (auto *ID = dyn_cast<ImportDecl>(D))
      PrevImported.insert(ID->getModule());
  }

  // Filter out inaccessible decls and any imports of, or shared with the
  // underlying module.
  auto NewEnd = std::partition(Decls.begin(), Decls.end(), [&](Decl *D) {
    if (auto *ID = dyn_cast<ImportDecl>(D)) {
      // Ignore imports of the underlying module, or any cross-import
      // that would map back to it.
      if (ID->getModule() == Underlying ||
          Underlying->isUnderlyingModuleOfCrossImportOverlay(ID->getModule()))
        return false;
      // Ignore an imports of modules also imported by the underlying module.
      if (PrevImported.find(ID->getModule()) != PrevImported.end())
        return false;
    }
    if (auto *VD = dyn_cast<ValueDecl>(D)) {
      if (AccessFilter > AccessLevel::Private &&
          VD->getFormalAccess() < AccessFilter)
        return false;
    }
    return true;
  });
  if (NewEnd != Decls.end())
    Decls.erase(NewEnd, Decls.end());

  // Separate out the import declarations and sort
  MutableArrayRef<Decl*> Imports, Remainder;
  auto ImportsEnd = std::partition(Decls.begin(), Decls.end(), [](Decl *D) {
    return isa<ImportDecl>(D);
  });
  if (ImportsEnd != Decls.begin()) {
    Imports = {Decls.begin(), ImportsEnd};
    Remainder = {ImportsEnd, Decls.end()};
    std::sort(Imports.begin(), Imports.end(), [](Decl *LHS, Decl *RHS) {
      return compareImports(cast<ImportDecl>(LHS), cast<ImportDecl>(RHS));
    });
  } else {
    Remainder = Decls;
  }
  std::sort(Remainder.begin(), Remainder.end(), compareSwiftDecls);
  return {Imports, Remainder};
}

static void printCrossImportOverlays(ModuleDecl *Underlying, ASTContext &Ctx,
                                     ASTPrinter &Printer,
                                     PrintOptions Options,
                                     bool PrintSynthesizedExtensions) {
  // If we end up printing decls from any cross-import overlay modules, make
  // sure we map any qualifying module references to the underlying module.
  Options.mapModuleToUnderlying = [&](const ModuleDecl *M) {
    if (Underlying->isUnderlyingModuleOfCrossImportOverlay(M))
      return const_cast<const ModuleDecl*>(Underlying);
    return M;
  };

  SmallVector<Decl *, 1> OverlayDecls;
  SmallVector<Identifier, 1> Bystanders;

  auto PrintDecl = [&](Decl *D) {
    return printModuleInterfaceDecl(D, Printer, Options,
                                    PrintSynthesizedExtensions);
  };

  SmallVector<ModuleDecl *, 1> OverlayModules;
  Underlying->findDeclaredCrossImportOverlaysTransitive(OverlayModules);
  std::sort(OverlayModules.begin(), OverlayModules.end(),
            [](ModuleDecl *LHS, ModuleDecl *RHS) {
    return LHS->getNameStr() < RHS->getNameStr();
  });

  for (auto *Overlay: OverlayModules) {
    OverlayDecls.clear();
    auto DeclLists = getDeclsFromOverlay(Overlay, Underlying, OverlayDecls,
                                         Options.AccessFilter);

    // Ignore overlays without any decls
    if (OverlayDecls.empty())
      continue;

    Bystanders.clear();
    Underlying->getAllBystandersForCrossImportOverlay(Overlay, Bystanders);
    assert(!Bystanders.empty() && "Overlay with no bystanders?");
    std::sort(Bystanders.begin(), Bystanders.end(),
              [](Identifier LHS, Identifier RHS) {
      return LHS.str() < RHS.str();
    });

    std::string BystanderList;
    for (size_t I: range(Bystanders.size())) {
      if (I == Bystanders.size() - 1) {
        if (I != 0)
          BystanderList += " and ";
      } else if (I != 0) {
        BystanderList += ", ";
      }
      BystanderList += Bystanders[I].str();
    }

    Printer << "\n// MARK: - " << BystanderList << " Additions\n\n";
    for (auto *Import : DeclLists.first)
      PrintDecl(Import);
    Printer << "\n";

    std::string PerDeclComment = "// Available when " + BystanderList;
    PerDeclComment += Bystanders.size() == 1 ? " is" : " are";
    PerDeclComment += " imported with " + Underlying->getNameStr().str();

    for (auto *D : DeclLists.second) {
      // FIXME: only print this comment if the decl is actually printed.
      Printer << PerDeclComment << "\n";
      if (PrintDecl(D))
        Printer << "\n";
    }
  }
}

void swift::ide::printSubmoduleInterface(
       ModuleDecl *M,
       ArrayRef<StringRef> FullModuleName,
       ArrayRef<StringRef> GroupNames,
       ModuleTraversalOptions TraversalOptions,
       ASTPrinter &Printer,
       const PrintOptions &Options,
       const bool PrintSynthesizedExtensions) {
  auto &SwiftContext = M->getASTContext();
  auto &Importer =
      static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());

  auto AdjustedOptions = Options;
  adjustPrintOptions(AdjustedOptions);

  SmallVector<Decl *, 1> Decls;
  M->getDisplayDecls(Decls);

  const clang::Module *InterestingClangModule = nullptr;
  SmallVector<ImportDecl *, 1> ImportDecls;
  llvm::DenseSet<const clang::Module *> ClangModulesForImports;
  SmallVector<Decl *, 1> SwiftDecls;
  llvm::DenseMap<const clang::Module *,
                 SmallVector<std::pair<Decl *, clang::SourceLocation>, 1>>
    ClangDecls;

  // Drop top-level module name.
  FullModuleName = FullModuleName.slice(1);

  InterestingClangModule = M->findUnderlyingClangModule();
  if (InterestingClangModule) {
    for (StringRef Name : FullModuleName) {
      InterestingClangModule = InterestingClangModule->findSubmodule(Name);
      if (!InterestingClangModule)
        return;
    }
  } else {
    assert(FullModuleName.empty());
  }

  // If we're printing recursively, find all of the submodules to print.
  if (InterestingClangModule) {
    if (TraversalOptions) {
      SmallVector<const clang::Module *, 8> Worklist;
      SmallPtrSet<const clang::Module *, 8> Visited;
      Worklist.push_back(InterestingClangModule);
      Visited.insert(InterestingClangModule);
      while (!Worklist.empty()) {
        const clang::Module *CM = Worklist.pop_back_val();
        if (!(TraversalOptions & ModuleTraversal::VisitHidden) &&
            CM->IsExplicit)
          continue;

        ClangDecls.insert({ CM, {} });

        // If we're supposed to visit submodules, add them now.
        if (TraversalOptions & ModuleTraversal::VisitSubmodules) {
          for (auto Sub = CM->submodule_begin(), SubEnd = CM->submodule_end();
               Sub != SubEnd; ++Sub) {
            if (Visited.insert(*Sub).second)
              Worklist.push_back(*Sub);
          }
        }
      }
    } else {
      ClangDecls.insert({ InterestingClangModule, {} });
    }
  }

  // Collect those submodules that are actually imported but have no import decls
  // in the module.
  llvm::SmallPtrSet<const clang::Module *, 16> NoImportSubModules;
  if (InterestingClangModule) {
    // Assume all submodules are missing.
    for (auto It =InterestingClangModule->submodule_begin();
         It != InterestingClangModule->submodule_end(); It++) {
      NoImportSubModules.insert(*It);
    }
  }
  llvm::StringMap<std::vector<Decl*>> FileRangedDecls;
  // Separate the declarations that we are going to print into different
  // buckets.
  for (Decl *D : Decls) {

    // Skip declarations that are not accessible.
    if (auto *VD = dyn_cast<ValueDecl>(D)) {
      if (Options.AccessFilter > AccessLevel::Private &&
          VD->getFormalAccess() < Options.AccessFilter)
        continue;
    }

    auto ShouldPrintImport = [&](ImportDecl *ImportD) -> bool {
      if (!InterestingClangModule)
        return true;
      auto ClangMod = ImportD->getClangModule();
      if (!ClangMod)
        return true;
      if (!ClangMod->isSubModule())
        return true;
      if (ClangMod == InterestingClangModule)
        return false;
      // FIXME: const-ness on the clang API.
      return ClangMod->isSubModuleOf(
                          const_cast<clang::Module*>(InterestingClangModule));
    };

    if (auto ID = dyn_cast<ImportDecl>(D)) {
      if (ShouldPrintImport(ID)) {
        if (ID->getClangModule())
          // Erase those submodules that are not missing.
          NoImportSubModules.erase(ID->getClangModule());
        if (ID->getImportKind() == ImportKind::Module) {
          // Make sure we don't print duplicate imports, due to getting imports
          // for both a clang module and its overlay.
          if (auto *ClangMod = getUnderlyingClangModuleForImport(ID)) {
            auto P = ClangModulesForImports.insert(ClangMod);
            bool IsNew = P.second;
            if (!IsNew)
              continue;
          }
        }
        ImportDecls.push_back(ID);
      }
      continue;
    }

    auto addToClangDecls = [&](Decl *D, ClangNode CN) {
      assert(CN && "No Clang node here");
      clang::SourceLocation Loc = CN.getLocation();

      auto *OwningModule = Importer.getClangOwningModule(CN);
      auto I = ClangDecls.find(OwningModule);
      if (I != ClangDecls.end()) {
        I->second.push_back({ D, Loc });
      }
    };

    if (auto clangNode = getEffectiveClangNode(D)) {
      addToClangDecls(D, clangNode);
      continue;
    }

    // If we have an extension containing globals imported as members,
    // use the first member as the Clang node.
    if (auto Ext = dyn_cast<ExtensionDecl>(D)) {
      if (extensionHasClangNode(Ext)) {
        addToClangDecls(Ext, extensionGetClangNode(Ext));
        continue;
      }
    }

    if (FullModuleName.empty()) {
      // If group name is given and the decl does not belong to the group, skip it.
      if (!GroupNames.empty()){
        if (auto Target = D->getGroupName()) {
          if (std::find(GroupNames.begin(), GroupNames.end(),
                        Target.getValue()) != GroupNames.end()) {
            FileRangedDecls.insert(std::make_pair(D->getSourceFileName().getValue(),
              std::vector<Decl*>())).first->getValue().push_back(D);
          }
        }
        continue;
      }
      // Add Swift decls if we are printing the top-level module.
      SwiftDecls.push_back(D);
    }
  }
  if (!GroupNames.empty()) {
    assert(SwiftDecls.empty());
    for (auto &Entry : FileRangedDecls) {
      auto &DeclsInFile = Entry.getValue();
      std::sort(DeclsInFile.begin(), DeclsInFile.end(),
                [](Decl* LHS, Decl *RHS) {
                  assert(LHS->getSourceOrder().hasValue());
                  assert(RHS->getSourceOrder().hasValue());
                  return LHS->getSourceOrder().getValue() <
                         RHS->getSourceOrder().getValue();
                });

      for (auto D : DeclsInFile) {
        SwiftDecls.push_back(D);
      }
    }
  }

  // Create the missing import decls and add to the collector.
  for (auto *SM : NoImportSubModules) {
    ImportDecls.push_back(createImportDecl(M->getASTContext(), M, SM, {}));
  }

  // Sort imported clang declarations in source order *within a submodule*.
  auto &ClangSourceManager = Importer.getClangASTContext().getSourceManager();
  for (auto &P : ClangDecls) {
    std::stable_sort(P.second.begin(), P.second.end(),
                     [&](std::pair<Decl *, clang::SourceLocation> LHS,
                         std::pair<Decl *, clang::SourceLocation> RHS) -> bool {
      return ClangSourceManager.isBeforeInTranslationUnit(LHS.second,
                                                          RHS.second);
    });
  }

  // Sort Swift declarations so that we print them in a consistent order.
  std::sort(ImportDecls.begin(), ImportDecls.end(), compareImports);

  // If the group name is specified, we sort them according to their source order,
  // which is the order preserved by getTopLevelDecls.
  if (GroupNames.empty())
    std::stable_sort(SwiftDecls.begin(), SwiftDecls.end(), compareSwiftDecls);

  ASTPrinter *PrinterToUse = &Printer;

  ClangCommentPrinter RegularCommentPrinter(Printer, Importer);
  if (Options.PrintRegularClangComments)
    PrinterToUse = &RegularCommentPrinter;

  auto PrintDecl = [&](Decl *D) {
    return printModuleInterfaceDecl(D, *PrinterToUse, AdjustedOptions,
                                    PrintSynthesizedExtensions);
  };

  // Imports from the stdlib are internal details that don't need to be exposed.
  if (!M->isStdlibModule()) {
    for (auto *D : ImportDecls)
      PrintDecl(D);
    Printer << "\n";
  }

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

  if (!(TraversalOptions & ModuleTraversal::SkipOverlay) ||
      !InterestingClangModule) {
    for (auto *D : SwiftDecls) {
      if (PrintDecl(D))
        Printer << "\n";
    }

    // If we're printing the entire target module (not specific sub-groups),
    // also print the decls from any underscored Swift cross-import overlays it
    // is the underlying module of, transitively.
    if (GroupNames.empty()) {
      printCrossImportOverlays(M, SwiftContext, *PrinterToUse, AdjustedOptions,
                               PrintSynthesizedExtensions);
    }
  }
}

static SourceLoc getDeclStartPosition(SourceFile &File) {
  SourceManager &SM = File.getASTContext().SourceMgr;
  SourceLoc Winner;

  auto tryUpdateStart = [&](SourceLoc Loc) -> bool {
    if (Loc.isInvalid())
      return false;
    if (Winner.isInvalid()) {
      Winner = Loc;
      return true;
    }
    if (SM.isBeforeInBuffer(Loc, Winner)) {
      Winner = Loc;
      return true;
    }
    return false;
  };

  for (auto D : File.getTopLevelDecls()) {
    if (tryUpdateStart(D->getStartLoc())) {
      tryUpdateStart(D->getAttrs().getStartLoc());
      auto RawComment = D->getRawComment();
      if (!RawComment.isEmpty())
        tryUpdateStart(RawComment.Comments.front().Range.getStart());
    }
  }

  return Winner;
}

static void printUntilFirstDeclStarts(SourceFile &File, ASTPrinter &Printer) {
  if (!File.getBufferID().hasValue())
    return;
  auto BufferID = *File.getBufferID();

  auto &SM = File.getASTContext().SourceMgr;
  CharSourceRange TextRange = SM.getRangeForBuffer(BufferID);

  auto DeclStartLoc = getDeclStartPosition(File);
  if (DeclStartLoc.isValid()) {
    TextRange = CharSourceRange(SM, TextRange.getStart(), DeclStartLoc);
  }

  Printer << SM.extractText(TextRange, BufferID);
}

void swift::ide::printSwiftSourceInterface(SourceFile &File,
                                           ASTPrinter &Printer,
                                           const PrintOptions &Options) {

  // We print all comments before the first line of Swift code.
  printUntilFirstDeclStarts(File, Printer);
  File.print(Printer, Options);
}

void swift::ide::printHeaderInterface(
       StringRef Filename,
       ASTContext &Ctx,
       ASTPrinter &Printer,
       const PrintOptions &Options) {
  auto AdjustedOptions = Options;
  adjustPrintOptions(AdjustedOptions);

  auto &Importer = static_cast<ClangImporter &>(*Ctx.getClangModuleLoader());
  auto &ClangSM = Importer.getClangASTContext().getSourceManager();

  auto headerFilter = [&](ClangNode ClangN) -> bool {
    return true; // no need for filtering.
  };

  SmallVector<Decl *, 32> ClangDecls;
  llvm::SmallPtrSet<Decl *, 32> SeenDecls;
  auto headerReceiver = [&](Decl *D) {
    if (SeenDecls.count(D) == 0) {
      SeenDecls.insert(D);
      ClangDecls.push_back(D);
    }
  };

  Importer.lookupDeclsFromHeader(Filename, headerFilter, headerReceiver);

  // Sort imported declarations in source order.
  std::sort(ClangDecls.begin(), ClangDecls.end(),
            [&](Decl *LHS, Decl *RHS) -> bool {
              return ClangSM.isBeforeInTranslationUnit(
                                            getEffectiveClangNode(LHS).getLocation(),
                                            getEffectiveClangNode(RHS).getLocation());
            });

  ASTPrinter *PrinterToUse = &Printer;

  ClangCommentPrinter RegularCommentPrinter(Printer, Importer);
  if (Options.PrintRegularClangComments)
    PrinterToUse = &RegularCommentPrinter;

  for (auto *D : ClangDecls) {
    ASTPrinter &Printer = *PrinterToUse;
    if (!AdjustedOptions.shouldPrint(D)) {
      Printer.callAvoidPrintDeclPost(D);
      continue;
    }
    if (D->print(Printer, AdjustedOptions))
      Printer << "\n";
  }
}

void ClangCommentPrinter::avoidPrintDeclPost(const Decl *D) {
  auto CD = D->getClangDecl();
  if (!CD)
    return;
  const auto &Ctx = ClangLoader.getClangASTContext();
  const auto &SM = Ctx.getSourceManager();
  auto EndLoc = CD->getSourceRange().getEnd();
  if (EndLoc.isInvalid())
    return;
  clang::FileID FID = SM.getFileID(EndLoc);
  if (FID.isInvalid())
    return;
  auto Loc = EndLoc;

  for (unsigned Line = SM.getSpellingLineNumber(EndLoc);
       Loc.isValid() && SM.getSpellingLineNumber(Loc) == Line;
       Loc = Loc.getLocWithOffset(1));
  if (Loc.isInvalid())
    return;
  if (SM.getFileOffset(Loc) > getResumeOffset(FID))
    setResumeOffset(FID, SM.getFileOffset(Loc));
}

void ClangCommentPrinter::printDeclPre(const Decl *D,
                                       Optional<BracketOptions> Bracket) {
  // Skip parameters, since we do not gracefully handle nested declarations on a
  // single line.
  // FIXME: we should fix that, since it also affects struct members, etc.
  if (!isa<ParamDecl>(D)) {
    if (auto ClangN = swift::ide::getEffectiveClangNode(D)) {
      printCommentsUntil(ClangN);
      if (shouldPrintNewLineBefore(ClangN)) {
        *this << "\n";
        printIndent();
      }
      updateLastEntityLine(ClangN.getSourceRange().getBegin());
    }
  }
  return OtherPrinter.printDeclPre(D, Bracket);
}

void ClangCommentPrinter::printDeclPost(const Decl *D,
                                        Optional<BracketOptions> Bracket) {
  OtherPrinter.printDeclPost(D, Bracket);

  // Skip parameters; see printDeclPre().
  if (isa<ParamDecl>(D))
    return;

  for (auto CommentText : PendingComments) {
    *this << " " << ASTPrinter::sanitizeUtf8(CommentText);
  }
  PendingComments.clear();
  if (auto ClangN = swift::ide::getEffectiveClangNode(D))
    updateLastEntityLine(ClangN.getSourceRange().getEnd());
}

void ClangCommentPrinter::printCommentsUntil(ClangNode Node) {
  const auto &Ctx = ClangLoader.getClangASTContext();
  const auto &SM = Ctx.getSourceManager();

  clang::SourceLocation NodeLoc =
      SM.getFileLoc(Node.getSourceRange().getBegin());
  if (NodeLoc.isInvalid())
    return;
  unsigned NodeLineNo = SM.getSpellingLineNumber(NodeLoc);
  clang::FileID FID = SM.getFileID(NodeLoc);
  if (FID.isInvalid())
    return;
  clang::SourceLocation FileLoc = SM.getLocForStartOfFile(FID);
  StringRef Text = SM.getBufferData(FID);
  if (Text.empty())
    return;

  const char *BufStart = Text.data();
  const char *BufPtr = BufStart + getResumeOffset(FID);
  const char *BufEnd = BufStart + Text.size();
  assert(BufPtr <= BufEnd);
  if (BufPtr == BufEnd)
    return; // nothing left.

  clang::Lexer Lex(FileLoc, Ctx.getLangOpts(), BufStart, BufPtr, BufEnd);
  Lex.SetCommentRetentionState(true);

  unsigned &LastPrintedLineNo = LastEntityLines[FID];
  clang::Token Tok;
  do {
    BufPtr = Lex.getBufferLocation();
    Lex.LexFromRawLexer(Tok);
    if (Tok.is(clang::tok::eof))
      break;
    if (Tok.isNot(clang::tok::comment))
      continue;

    // Reached a comment.

    clang::SourceLocation CommentLoc = Tok.getLocation();
    std::pair<clang::FileID, unsigned> LocInfo =
      SM.getDecomposedLoc(CommentLoc);
    assert(LocInfo.first == FID);

    unsigned LineNo = SM.getLineNumber(LocInfo.first, LocInfo.second);
    if (LineNo > NodeLineNo)
      break; // Comment is past the clang node.

    bool IsDocComment = isDocumentationComment(CommentLoc, Node);

    // Print out the comment.

    StringRef CommentText(BufStart + LocInfo.second, Tok.getLength());

    // Check if comment is on same line but after the declaration.
    if (SM.isBeforeInTranslationUnit(NodeLoc, Tok.getLocation())) {
      if (!IsDocComment)
        PendingComments.push_back(CommentText);
      continue;
    }

    if (LastPrintedLineNo && LineNo - LastPrintedLineNo > 1) {
      *this << "\n";
      printIndent();
    }
    if (!IsDocComment) {
      unsigned StartLocCol = SM.getSpellingColumnNumber(Tok.getLocation());
      printComment(CommentText, StartLocCol);
    }
    LastPrintedLineNo =
        SM.getLineNumber(LocInfo.first, LocInfo.second + Tok.getLength());

  } while (true);

  // Resume printing comments from this point.
  setResumeOffset(FID, BufPtr - BufStart);
}

void ClangCommentPrinter::printComment(StringRef RawText, unsigned StartCol) {
  unsigned WhitespaceToTrim = StartCol ? StartCol - 1 : 0;
  SmallVector<StringRef, 8> Lines;
  trimLeadingWhitespaceFromLines(RawText, WhitespaceToTrim, Lines);

  for (auto Line : Lines) {
    *this << ASTPrinter::sanitizeUtf8(Line) << "\n";
    printIndent();
  }
}

bool ClangCommentPrinter::isDocumentationComment(
      clang::SourceLocation CommentLoc, ClangNode Node) const {
  const clang::Decl *D = Node.getAsDecl();
  if (!D)
    return false;

  const auto &Ctx = ClangLoader.getClangASTContext();
  const auto &SM = Ctx.getSourceManager();
  const clang::RawComment *RC = Ctx.getRawCommentForAnyRedecl(D);
  if (!RC)
    return false;

  clang::SourceRange DocRange = RC->getSourceRange();
  if (SM.isBeforeInTranslationUnit(CommentLoc, DocRange.getBegin()) ||
      SM.isBeforeInTranslationUnit(DocRange.getEnd(), CommentLoc))
    return false;
  return true;
}

bool ClangCommentPrinter::shouldPrintNewLineBefore(ClangNode Node) const {
  assert(Node);
  const auto &Ctx = ClangLoader.getClangASTContext();
  const auto &SM = Ctx.getSourceManager();

  clang::SourceLocation NodeLoc =
      SM.getFileLoc(Node.getSourceRange().getBegin());
  if (NodeLoc.isInvalid())
    return false;
  unsigned NodeLineNo = SM.getSpellingLineNumber(NodeLoc);
  clang::FileID FID = SM.getFileID(NodeLoc);
  if (FID.isInvalid())
    return false;

  unsigned LastEntiyLine = 0;
  auto It = LastEntityLines.find(FID);
  if (It != LastEntityLines.end())
    LastEntiyLine = It->second;
  return (NodeLineNo > LastEntiyLine) && NodeLineNo - LastEntiyLine > 1;
}

void ClangCommentPrinter::updateLastEntityLine(clang::SourceLocation Loc) {
  if (Loc.isInvalid())
    return;

  const auto &Ctx = ClangLoader.getClangASTContext();
  const auto &SM = Ctx.getSourceManager();

  unsigned LineNo = SM.getSpellingLineNumber(Loc);
  clang::FileID FID = SM.getFileID(Loc);
  if (FID.isInvalid())
    return;

  updateLastEntityLine(FID, LineNo);
}

void ClangCommentPrinter::updateLastEntityLine(clang::FileID FID,
                                               unsigned LineNo) {
  assert(!FID.isInvalid());
  unsigned &LastEntiyLine = LastEntityLines[FID];
  if (LineNo > LastEntiyLine)
    LastEntiyLine = LineNo;
}
