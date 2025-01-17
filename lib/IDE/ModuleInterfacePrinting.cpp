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
#include "swift/Basic/Assertions.h"
#include "swift/Basic/PrimitiveParsing.h"
#include "swift/Basic/Unicode.h"
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
    PrintOptions Options = PrintOptions::printTypeInterface(
        Ty.getPointer(),
        Ty->getASTContext().TypeCheckerOpts.PrintFullConvention);
    Options.CurrentModule = M;
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

static void adjustPrintOptions(PrintOptions &AdjustedOptions) {
  // Don't print empty curly braces while printing the module interface.
  AdjustedOptions.FunctionDefinitions = false;

  AdjustedOptions.PrintGetSetOnRWProperties = false;

  // Print var declarations separately, one variable per decl.
  AdjustedOptions.ExplodePatternBindingDecls = true;
  AdjustedOptions.VarInitializers = false;
}

void swift::ide::collectModuleGroups(ModuleDecl *M,
                                     SmallVectorImpl<StringRef> &Into) {
  for (auto File : M->getFiles()) {
    File->collectAllGroups(Into);
  }
  std::sort(Into.begin(), Into.end(), [](StringRef L, StringRef R) {
    return L.compare_insensitive(R) < 0;
  });
}

/// Determine whether the given extension has a Clang node that
/// created it (vs. being a Swift extension).
static bool extensionHasClangNode(ExtensionDecl *ext) {
  return static_cast<bool>(swift::ide::extensionGetClangNode(ext));
}

std::optional<StringRef> swift::ide::findGroupNameForUSR(ModuleDecl *M,
                                                         StringRef USR) {
  for (auto File : M->getTopLevelModule()->getFiles()) {
    if (auto Name = File->getGroupNameByUSR(USR)) {
      return Name;
    }
  }
  return std::nullopt;
}

/// Prints a single decl using the \p Printer and \p Options provided. If
/// \p LeadingComment is non-empty, it will be printed verbatim before the decl
/// and any documentation comment associated with it.
///
/// \returns Whether the given decl was printed.
static bool printModuleInterfaceDecl(Decl *D,
                                     ASTPrinter &Printer,
                                     PrintOptions &Options,
                                     bool PrintSynthesizedExtensions,
                                     StringRef LeadingComment = StringRef()) {
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
      if (!ExtendedNominal ||
          Ext->getModuleContext() == ExtendedNominal->getModuleContext())
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
  if (!LeadingComment.empty() && Options.shouldPrint(D)) {
    Printer << LeadingComment;
    Printer.printNewline();
  }
  if (D->print(Printer, Options)) {
    if (Options.BracketOptions.shouldCloseNominal(D))
      Printer.printNewline();
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
            Printer.printNewline();
            if (!LeadingComment.empty()) {
              Printer << LeadingComment;
              Printer.printNewline();
            }
            Ext->print(Printer, Options);
            Printer.printNewline();
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
                  Printer.printNewline();
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
              if (Options.BracketOptions.shouldOpenExtension(ET.Ext)) {
                Printer.printNewline();
                if (Options.shouldPrint(ET.Ext) && !LeadingComment.empty()) {
                  Printer << LeadingComment;
                  Printer.printNewline();
                }
              }
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
              if (Options.BracketOptions.shouldCloseExtension(ET.Ext)) {
                Printer.printNewline();
              }
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
  return LHS->getImportPath() < RHS->getImportPath();
}

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
}

static std::pair<ArrayRef<Decl*>, ArrayRef<Decl*>>
getDeclsFromCrossImportOverlay(ModuleDecl *Overlay, ModuleDecl *Declaring,
                               SmallVectorImpl<Decl *> &Decls,
                               AccessLevel AccessFilter) {
  swift::getTopLevelDeclsForDisplay(Overlay, Decls);

  // Collect the imports of the underlying module so we can filter them out.
  SmallPtrSet<ModuleDecl *, 8> PrevImported;
  SmallVector<Decl*, 1> DeclaringDecls;
  swift::getTopLevelDeclsForDisplay(Declaring, DeclaringDecls);
  for (auto *D: DeclaringDecls) {
    if (auto *ID = dyn_cast<ImportDecl>(D))
      PrevImported.insert(ID->getModule());
  }

  // Filter out inaccessible decls and any imports of, or shared with the
  // underlying module.
  auto NewEnd = std::partition(Decls.begin(), Decls.end(), [&](Decl *D) {
    if (auto *ID = dyn_cast<ImportDecl>(D)) {
      ModuleDecl *Imported = ID->getModule();
      if (!Imported)
        return true;

      // Ignore imports of the underlying module, or any cross-import
      // that would map back to it.
      if (Imported == Declaring || Imported->isCrossImportOverlayOf(Declaring))
        return false;

      // Ignore an imports of modules also imported by the underlying module.
      if (PrevImported.contains(Imported))
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

static void printCrossImportOverlays(ModuleDecl *Declaring, ASTContext &Ctx,
                                     ASTPrinter &Printer,
                                     PrintOptions Options,
                                     bool PrintSynthesizedExtensions) {
  SmallVector<Decl *, 1> OverlayDecls;
  SmallVector<Identifier, 1> Bystanders;

  auto PrintDecl = [&](Decl *D, StringRef LeadingComment = StringRef()) {
    return printModuleInterfaceDecl(D, Printer, Options,
                                    PrintSynthesizedExtensions,
                                    LeadingComment);
  };

  SmallVector<ModuleDecl *, 1> OverlayModules;
  Declaring->findDeclaredCrossImportOverlaysTransitive(OverlayModules);
  std::sort(OverlayModules.begin(), OverlayModules.end(),
            [](ModuleDecl *LHS, ModuleDecl *RHS) {
    return LHS->getNameStr() < RHS->getNameStr();
  });

  for (auto *Overlay: OverlayModules) {
    OverlayDecls.clear();
    auto DeclLists = getDeclsFromCrossImportOverlay(Overlay, Declaring,
                                                    OverlayDecls,
                                                    Options.AccessFilter);

    // Ignore overlays without any decls
    if (OverlayDecls.empty())
      continue;

    Bystanders.clear();
    auto BystandersValid =
      Overlay->getRequiredBystandersIfCrossImportOverlay(Declaring, Bystanders);

    // Ignore badly formed overlays that don't import their declaring module.
    if (!BystandersValid)
      continue;

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

    Printer.printNewline();
    Printer << "// MARK: - " << BystanderList << " Additions";
    Printer.printNewline();
    Printer.printNewline();
    for (auto *Import : DeclLists.first)
      PrintDecl(Import);
    if (!DeclLists.first.empty())
      Printer.printNewline();

    std::string PerDeclComment = "// Available when " + BystanderList;
    PerDeclComment += Bystanders.size() == 1 ? " is" : " are";
    PerDeclComment += " imported with " + Declaring->getNameStr().str();

    for (auto *D : DeclLists.second) {
      if (PrintDecl(D, PerDeclComment) && Options.EmptyLineBetweenDecls)
        Printer.printNewline();
    }
  }
}

void swift::ide::printModuleInterface(
       ModuleDecl *TargetMod,
       ArrayRef<StringRef> GroupNames,
       ModuleTraversalOptions TraversalOptions,
       ASTPrinter &Printer,
       const PrintOptions &Options,
       const bool PrintSynthesizedExtensions) {

  // Clang submodules aren't handled well by `getDisplayDecls()` (no decls are
  // returned), so map them to their top-level module and filter out the extra
  // results below.
  const clang::Module *TargetClangMod = TargetMod->findUnderlyingClangModule();
  ModuleDecl *TopLevelMod = TargetMod->getTopLevelModule();
  bool IsSubmodule = TargetMod != TopLevelMod;

  auto &SwiftContext = TopLevelMod->getASTContext();
  auto &Importer =
      static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());

  auto AdjustedOptions = Options;
  adjustPrintOptions(AdjustedOptions);

  SmallVector<Decl *, 1> Decls;
  swift::getTopLevelDeclsForDisplay(TopLevelMod, Decls);

  SmallVector<ImportDecl *, 1> ImportDecls;
  llvm::DenseSet<const clang::Module *> ClangModulesForImports;
  SmallVector<Decl *, 1> SwiftDecls;
  llvm::DenseMap<const clang::Module *,
                 SmallVector<std::pair<Decl *, clang::SourceLocation>, 1>>
    ClangDecls;

  // If we're printing recursively, find all of the submodules to print.
  if (TargetClangMod) {
    if (TraversalOptions) {
      SmallVector<const clang::Module *, 8> Worklist;
      SmallPtrSet<const clang::Module *, 8> Visited;
      Worklist.push_back(TargetClangMod);
      Visited.insert(TargetClangMod);
      while (!Worklist.empty()) {
        const clang::Module *CM = Worklist.pop_back_val();
        if (!(TraversalOptions & ModuleTraversal::VisitHidden) &&
            CM->IsExplicit)
          continue;

        ClangDecls.insert({ CM, {} });

        // If we're supposed to visit submodules, add them now.
        if (TraversalOptions & ModuleTraversal::VisitSubmodules) {
          for (clang::Module * submodule: CM->submodules()) {
            if (Visited.insert(submodule).second) {
                Worklist.push_back(submodule);
            }
          }
        }
      }
    } else {
      ClangDecls.insert({ TargetClangMod, {} });
    }
  }

  // Collect those submodules that are actually imported but have no import
  // decls in the module.
  llvm::SmallPtrSet<const clang::Module *, 16> NoImportSubModules;
  if (TargetClangMod) {
    // Assume all submodules are missing.
    for (clang::Module *submodule: TargetClangMod->submodules()) {
      NoImportSubModules.insert(submodule);
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
      if (ImportD->getAttrs().hasAttribute<ImplementationOnlyAttr>())
        return false;

      if (!TargetClangMod)
        return true;
      if (ImportD->getModule() == TargetMod)
        return false;

      auto ImportedMod = ImportD->getClangModule();
      if (!ImportedMod)
        return true;
      if (!ImportedMod->isSubModule())
        return true;
      if (ImportedMod == TargetClangMod)
        return false;
      return ImportedMod->isSubModuleOf(TargetClangMod);
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
      if (auto namespaceDecl =
              dyn_cast_or_null<clang::NamespaceDecl>(clangNode.getAsDecl())) {
        // An imported namespace decl will contain members from all redecls, so
        // make sure we add all the redecls.
        for (auto redecl : namespaceDecl->redecls()) {
          if (redecl->decls_empty())
            continue;
          // Namespace redecls may exist across mutliple modules. We want to
          // add the decl "D" to every module that has a redecl. But we only
          // want to add "D" once to prevent duplicate printing.
          clang::SourceLocation loc = redecl->getLocation();
          assert(loc.isValid() &&
                 "expected a valid SourceLocation for a non-empty namespace");
          auto *owningModule = Importer.getClangOwningModule(redecl);
          auto found = ClangDecls.find(owningModule);
          if (found != ClangDecls.end() &&
              // Don't re-add this decl if it already exists for "OwningModule".
              llvm::find_if(found->second, [D](auto p) {
                return p.first == D;
              }) == found->second.end()) {
            found->second.push_back({D, loc});
          }
        }
      } else {
        addToClangDecls(D, clangNode);
      }
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

    if (!IsSubmodule) {
      // If group name is given and the decl does not belong to the group, skip it.
      if (!GroupNames.empty()){
        if (auto TargetGroup = D->getGroupName()) {
          if (std::find(GroupNames.begin(), GroupNames.end(),
                        TargetGroup.value()) != GroupNames.end()) {
            FileRangedDecls.insert({
              D->getSourceFileName().value(),
              std::vector<Decl*>()
            }).first->getValue().push_back(D);
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
                  assert(LHS->getSourceOrder().has_value());
                  assert(RHS->getSourceOrder().has_value());
                  return LHS->getSourceOrder().value() <
                         RHS->getSourceOrder().value();
                });

      for (auto D : DeclsInFile) {
        SwiftDecls.push_back(D);
      }
    }
  }

  // Create the missing import decls and add to the collector.
  for (auto *SubMod : NoImportSubModules) {
    ImportDecls.push_back(createImportDecl(TopLevelMod->getASTContext(),
                                           TopLevelMod, SubMod, {}));
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

  auto PrintDecl = [&](Decl *D) {
    return printModuleInterfaceDecl(D, Printer, AdjustedOptions,
                                    PrintSynthesizedExtensions);
  };

  // Imports from the stdlib are internal details that don't need to be exposed.
  if (!TargetMod->isStdlibModule()) {
    for (auto *D : ImportDecls)
      PrintDecl(D);
    Printer.printNewline();
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
        if (PrintDecl(DeclAndLoc.first) && Options.EmptyLineBetweenDecls)
          Printer.printNewline();
    }
  }

  if (!(TraversalOptions & ModuleTraversal::SkipOverlay) || !TargetClangMod) {
    for (auto *D : SwiftDecls) {
      if (PrintDecl(D) && Options.EmptyLineBetweenDecls)
        Printer.printNewline();
    }

    // If we're printing the entire target module (not specific sub-groups),
    // also print the decls from any underscored Swift cross-import overlays it
    // is the underlying module of, transitively.
    if (GroupNames.empty()) {
      printCrossImportOverlays(TargetMod, SwiftContext, Printer,
                               AdjustedOptions, PrintSynthesizedExtensions);
    }
  }
  // Flush pending newlines.
  Printer.forceNewlines();
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
  auto BufferID = File.getBufferID();

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

static Decl* getTopLevelDecl(Decl *D) {
  while (!D->getDeclContext()->isModuleScopeContext()) {
    auto *ParentD = D->getDeclContext()->getAsDecl();
    if (!ParentD)
      break;
    D = ParentD;
  }
  return D;
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
    if (SeenDecls.insert(getTopLevelDecl(D)).second)
      ClangDecls.push_back(D);
  };

  Importer.lookupDeclsFromHeader(Filename, headerFilter, headerReceiver);

  // Sort imported declarations in source order.
  std::sort(ClangDecls.begin(), ClangDecls.end(),
            [&](Decl *LHS, Decl *RHS) -> bool {
              return ClangSM.isBeforeInTranslationUnit(
                                            getEffectiveClangNode(LHS).getLocation(),
                                            getEffectiveClangNode(RHS).getLocation());
            });

  for (auto *D : ClangDecls) {
    // Even though the corresponding clang decl should be top-level, its
    // equivalent Swift decl may not be. E.g. a top-level function may be mapped
    // to a property accessor in Swift.
    D = getTopLevelDecl(D);
    if (!AdjustedOptions.shouldPrint(D)) {
      Printer.callAvoidPrintDeclPost(D);
      continue;
    }
    if (D->print(Printer, AdjustedOptions))
      Printer.printNewline();
  }
  Printer.forceNewlines();
}

void swift::ide::printSymbolicSwiftClangModuleInterface(
    ModuleDecl *M, ASTPrinter &Printer, const clang::Module *clangModule) {
  std::string headerComment;
  llvm::raw_string_ostream(headerComment)
      << "// Swift interface for " << (clangModule->IsSystem ? "system " : "")
      << "module '" << clangModule->Name << "'\n";
  Printer.printText(headerComment);

  ModuleTraversalOptions opts;
  opts |= ModuleTraversal::VisitSubmodules;
  auto popts =
      PrintOptions::printModuleInterface(/*printFullConvention=*/false);
  popts.PrintDocumentationComments = false;
  popts.SkipInlineCXXNamespace = true;

  auto &SwiftContext = M->getTopLevelModule()->getASTContext();
  auto &Importer =
      static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());
  Importer.withSymbolicFeatureEnabled([&]() {
    printModuleInterface(M, {}, opts, Printer, popts,
                         /*SynthesizeExtensions=*/false);
  });
}
