//===--- SymbolGraph.cpp - Symbol Graph Data Structure -------------------===//
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

#include "clang/AST/DeclObjC.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Serialization/SerializedModuleLoader.h"

#include "DeclarationFragmentPrinter.h"
#include "FormatVersion.h"
#include "Symbol.h"
#include "SymbolGraph.h"

using namespace swift;
using namespace symbolgraphgen;

SymbolGraph::SymbolGraph(ModuleDecl &M,
                         Optional<ModuleDecl *> ExtendedModule,
                         llvm::Triple Target,
                         markup::MarkupContext &Ctx,
                         Optional<llvm::VersionTuple> ModuleVersion)
: M(M),
  ExtendedModule(ExtendedModule),
  Target(Target),
  Ctx(Ctx),
  ModuleVersion(ModuleVersion) {}

// MARK: - Utilities

StringRef SymbolGraph::getUSR(const ValueDecl *VD) {
  auto Found = USRCache.find(VD);
  if (Found != USRCache.end()) {
    return Found->second;
  }
  llvm::SmallString<32> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  ide::printDeclUSR(VD, OS);
  auto USR = Ctx.allocateCopy(Scratch.str());
  USRCache.insert({VD, USR});
  return USR;
}

void
SymbolGraph::getPathComponents(const ValueDecl *VD,
                               SmallVectorImpl<SmallString<32>> &Components) {
  // Collect the spellings of the fully qualified identifier components.
  auto Decl = VD;
  while (Decl && !isa<ModuleDecl>(Decl)) {
    SmallString<32> Scratch;
    Decl->getFullName().getString(Scratch);
    Components.push_back(Scratch);
    if (const auto *DC = Decl->getDeclContext()) {
      if (const auto *Proto = DC->getExtendedProtocolDecl()) {
        Decl = Proto;
      } else if (const auto *Ext = dyn_cast_or_null<ExtensionDecl>(DC->getAsDecl())) {
        Decl = Ext->getExtendedNominal();
      } else {
        Decl = dyn_cast_or_null<ValueDecl>(DC->getAsDecl());
      }
    } else {
      Decl = nullptr;
    }
  }

  // The list is leaf-to-root, but our list is root-to-leaf, so reverse it.
  std::reverse(Components.begin(), Components.end());
}

PrintOptions SymbolGraph::getDeclarationFragmentsPrintOptions() const {
  PrintOptions Opts;
  Opts.FunctionDefinitions = false;
  Opts.ArgAndParamPrinting =
    PrintOptions::ArgAndParamPrintingMode::ArgumentOnly;
  Opts.PrintGetSetOnRWProperties = false;
  Opts.PrintPropertyAccessors = false;
  Opts.PrintSubscriptAccessors = false;
  Opts.SkipUnderscoredKeywords = true;
  Opts.SkipAttributes = true;
  Opts.PrintOverrideKeyword = true;
  Opts.PrintImplicitAttrs = false;
  Opts.PrintFunctionRepresentationAttrs =
    PrintOptions::FunctionRepresentationMode::None;
  Opts.PrintUserInaccessibleAttrs = false;
  Opts.SkipPrivateStdlibDecls = true;
  Opts.SkipUnderscoredStdlibProtocols = true;

  Opts.ExclusiveAttrList.clear();

#define DECL_ATTR(SPELLING, CLASS, OPTIONS, CODE) Opts.ExcludeAttrList.push_back(DAK_##CLASS);
#define TYPE_ATTR(X) Opts.ExcludeAttrList.push_back(TAK_##X);
#include "swift/AST/Attr.def"

  return Opts;
}

// MARK: - Symbols (Nodes)

void SymbolGraph::recordNode(const ValueDecl *VD) {
  Nodes.insert(VD);

  // Record all of the possible relationships (edges) originating
  // with this declaration.
  recordMemberRelationship(VD);
  recordConformanceRelationships(VD);
  recordInheritanceRelationships(VD);
  recordDefaultImplementationRelationships(VD);
  recordOverrideRelationship(VD);
  recordRequirementRelationships(VD);
  recordOptionalRequirementRelationships(VD);
}

// MARK: - Relationships (Edges)

void SymbolGraph::recordEdge(const ValueDecl *Source,
                             const ValueDecl *Target,
                             RelationshipKind Kind) {
  if (Target->isPrivateStdlibDecl(
      /*treatNonBuiltinProtocolsAsPublic = */false)) {
    return;
  }

  // There might be relationships on implicit declarations,
  // such as overriding implicit @objc init().
  if (Target->isImplicit()) {
    return;
  }

  Nodes.insert(Source);
  if (Target->getModuleContext() != &M) {
    // Don't claim a symbol just because we have a relationship to it.
    // For example, if we conform to `Sequence`, that symbol's node should be
    // under Swift, not this module.
    Nodes.insert(Target);
  }

  Edges.insert({this, Kind, Source, Target});
}

void SymbolGraph::recordMemberRelationship(const ValueDecl *VD) {
  auto *DC = VD->getDeclContext();
  switch (DC->getContextKind()) {
    case DeclContextKind::GenericTypeDecl:
    case DeclContextKind::ExtensionDecl:
    case swift::DeclContextKind::EnumElementDecl:
      return recordEdge(VD, VD->getDeclContext()->getSelfNominalTypeDecl(),
                        RelationshipKind::MemberOf());
    case swift::DeclContextKind::AbstractClosureExpr:
    case swift::DeclContextKind::Initializer:
    case swift::DeclContextKind::TopLevelCodeDecl:
    case swift::DeclContextKind::SubscriptDecl:
    case swift::DeclContextKind::AbstractFunctionDecl:
    case swift::DeclContextKind::SerializedLocal:
    case swift::DeclContextKind::Module:
    case swift::DeclContextKind::FileUnit:
      break;
  }
}

void
SymbolGraph::recordInheritanceRelationships(const ValueDecl *VD) {
  if (const auto *NTD = dyn_cast<NominalTypeDecl>(VD)) {
    for (const auto &InheritanceLoc : NTD->getInherited()) {
      auto Ty = InheritanceLoc.getType();
      if (!Ty) {
        continue;
      }
      auto *InheritedTypeDecl =
          dyn_cast_or_null<ClassDecl>(Ty->getAnyNominal());
      if (!InheritedTypeDecl) {
        continue;
      }

      recordEdge(VD, InheritedTypeDecl, RelationshipKind::InheritsFrom());
    }
  }
}

void SymbolGraph::recordDefaultImplementationRelationships(
    const ValueDecl *VD) {
  if (const auto *Extension = dyn_cast<ExtensionDecl>(VD->getDeclContext())) {
    if (const auto *Protocol = Extension->getExtendedProtocolDecl()) {
      for (const auto *Member : Protocol->getMembers()) {
        if (const auto *MemberVD = dyn_cast<ValueDecl>(Member)) {
          if (MemberVD->getFullName().compare(VD->getFullName()) == 0) {
            recordEdge(VD, MemberVD,
                       RelationshipKind::DefaultImplementationOf());
          }
        }
      }
    }
  }
}

void
SymbolGraph::recordRequirementRelationships(const ValueDecl *VD) {
  if (const auto *Protocol = dyn_cast<ProtocolDecl>(VD->getDeclContext())) {
    if (VD->isProtocolRequirement()) {
      recordEdge(VD, Protocol, RelationshipKind::RequirementOf());
    }
  }
}

void SymbolGraph::recordOptionalRequirementRelationships(
    const ValueDecl *VD) {
  if (const auto *Protocol = dyn_cast<ProtocolDecl>(VD->getDeclContext())) {
    if (VD->isProtocolRequirement()) {
      if (const auto *ClangDecl = VD->getClangDecl()) {
        if (const auto *Method = dyn_cast<clang::ObjCMethodDecl>(ClangDecl)) {
          if (Method->isOptional()) {
            recordEdge(VD, Protocol,
                       RelationshipKind::OptionalRequirementOf());
          }
        }
      }
    }
  }
}

void
SymbolGraph::recordConformanceRelationships(const ValueDecl *VD) {
  if (const auto *NTD = dyn_cast<NominalTypeDecl>(VD)) {
    for (const auto *Conformance : NTD->getAllConformances()) {
      recordEdge(VD, Conformance->getProtocol(),
                 RelationshipKind::ConformsTo());
    }
  }
}

void SymbolGraph::recordOverrideRelationship(const ValueDecl *VD) {
  if (const auto *Override = VD->getOverriddenDecl()) {
    recordEdge(VD, Override, RelationshipKind::Overrides());
  }
}

// MARK: - Serialization

void SymbolGraph::serialize(llvm::json::OStream &OS) {
  OS.object([&](){
    OS.attributeObject("metadata", [&](){
      {
        AttributeRAII FV("formatVersion", OS);
        llvm::VersionTuple FormatVersion(SWIFT_SYMBOLGRAPH_FORMAT_MAJOR,
                                         SWIFT_SYMBOLGRAPH_FORMAT_MINOR,
                                         SWIFT_SYMBOLGRAPH_FORMAT_PATCH);
        symbolgraphgen::serialize(FormatVersion, OS);
      } // end formatVersion:

      auto VersionString = version::getSwiftFullVersion();
      StringRef VersionStringRef(VersionString.c_str(), VersionString.size());
      OS.attribute("generator", VersionStringRef);
    }); // end metadata:

    OS.attributeObject("module", [&](){
      OS.attribute("name", M.getNameStr());
      AttributeRAII Platform("platform", OS);

      auto *MainFile = M.getFiles().front();
      switch (MainFile->getKind()) {
          case FileUnitKind::Builtin:
            llvm_unreachable("Unexpected module kind: Builtin");
          case FileUnitKind::DWARFModule:
            llvm_unreachable("Unexpected module kind: DWARFModule");
          case FileUnitKind::Source:
            llvm_unreachable("Unexpected module kind: Source");
            break;
          case FileUnitKind::SerializedAST: {
            auto SerializedAST = cast<SerializedASTFile>(MainFile);
            auto Target = llvm::Triple(SerializedAST->getTargetTriple());
            symbolgraphgen::serialize(Target, OS);
            break;
          }
          case FileUnitKind::ClangModule: {
            auto ClangModule = cast<ClangModuleUnit>(MainFile);
            if (const auto *Overlay = ClangModule->getOverlayModule()) {
              auto &OverlayMainFile =
                  Overlay->getMainFile(FileUnitKind::SerializedAST);
              auto SerializedAST = cast<SerializedASTFile>(OverlayMainFile);
              auto Target = llvm::Triple(SerializedAST.getTargetTriple());
              symbolgraphgen::serialize(Target, OS);
            } else {
              symbolgraphgen::serialize(Target, OS);
            }
            break;
        }
      }
    });

    if (ModuleVersion) {
      AttributeRAII MV("moduleVersion", OS);
      symbolgraphgen::serialize(*ModuleVersion, OS);
    }

    OS.attributeArray("symbols", [&](){
      for (const auto *VD: Nodes) {
        Symbol S { *this, VD };
        S.serialize(OS);
      }
    });

    OS.attributeArray("relationships", [&](){
      for (const auto Relationship : Edges) {
        Relationship.serialize(OS);
      }
    });

  });
}

void
SymbolGraph::serializeDeclarationFragments(StringRef Key,
                                                    const ValueDecl *VD,
                                                    llvm::json::OStream &OS) {
  DeclarationFragmentPrinter Printer(*this, OS, Key);
  VD->print(Printer, getDeclarationFragmentsPrintOptions());
}

void
SymbolGraph::serializeSubheadingDeclarationFragments(StringRef Key,
    const ValueDecl *VD,
    llvm::json::OStream &OS) {
  DeclarationFragmentPrinter Printer(*this, OS, Key);
  auto Options = getDeclarationFragmentsPrintOptions();
  Options.VarInitializers = false;
  Options.PrintDefaultArgumentValue = false;
  Options.PrintEmptyArgumentNames = false;
  Options.PrintOverrideKeyword = false;
  VD->print(Printer, Options);
}

void
SymbolGraph::serializeDeclarationFragments(StringRef Key, Type T,
                                                    llvm::json::OStream &OS) {
  DeclarationFragmentPrinter Printer(*this, OS, Key);
  T->print(Printer, getDeclarationFragmentsPrintOptions());
}
