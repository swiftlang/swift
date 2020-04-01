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
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Serialization/SerializedModuleLoader.h"

#include "DeclarationFragmentPrinter.h"
#include "FormatVersion.h"
#include "Symbol.h"
#include "SymbolGraph.h"
#include "SymbolGraphASTWalker.h"

using namespace swift;
using namespace symbolgraphgen;

SymbolGraph::SymbolGraph(SymbolGraphASTWalker &Walker,
                         ModuleDecl &M,
                         Optional<ModuleDecl *> ExtendedModule,
                         markup::MarkupContext &Ctx,
                         Optional<llvm::VersionTuple> ModuleVersion)
: Walker(Walker),
  M(M),
  ExtendedModule(ExtendedModule),
  Ctx(Ctx),
  ModuleVersion(ModuleVersion) {}

// MARK: - Utilities

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

void SymbolGraph::recordNode(Symbol S) {
  Nodes.insert(S);

  // Record all of the possible relationships (edges) originating
  // with this declaration.
  recordMemberRelationship(S);
  recordConformanceSynthesizedMemberRelationships(S);
  recordSuperclassSynthesizedMemberRelationships(S);
  recordConformanceRelationships(S);
  recordInheritanceRelationships(S);
  recordDefaultImplementationRelationships(S);
  recordOverrideRelationship(S);
  recordRequirementRelationships(S);
  recordOptionalRequirementRelationships(S);
}

// MARK: - Relationships (Edges)

void SymbolGraph::recordEdge(Symbol Source,
                             Symbol Target,
                             RelationshipKind Kind,
                             const ExtensionDecl *ConformanceExtension) {
  if (isImplicitlyPrivate(Target.getSymbolDecl())) {
    // Don't record relationships to privately named things because
    // we'll never be able to look up the target anyway.
    return;
  }
  Edges.insert({this, Kind, Source, Target, ConformanceExtension});
}

void SymbolGraph::recordMemberRelationship(Symbol S) {
  auto *DC = S.getSymbolDecl()->getDeclContext();
  switch (DC->getContextKind()) {
    case DeclContextKind::GenericTypeDecl:
    case DeclContextKind::ExtensionDecl:
    case swift::DeclContextKind::EnumElementDecl:
      return recordEdge(S,
          Symbol(this,
                 S.getSymbolDecl()->getDeclContext()->getSelfNominalTypeDecl(),
                 nullptr),
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

void SymbolGraph::recordSuperclassSynthesizedMemberRelationships(Symbol S) {
  if (!Walker.Options.EmitSynthesizedMembers) {
    return;
  }
  // Via class inheritance...
  if (const auto *C = dyn_cast<ClassDecl>(S.getSymbolDecl())) {
    // Collect all superclass members up the inheritance chain.
    SmallPtrSet<const ValueDecl *, 32> SuperClassMembers;
    const auto *Super = C->getSuperclassDecl();
    while (Super) {
      for (const auto *SuperMember : Super->getMembers()) {
        if (const auto *SuperMemberVD = dyn_cast<ValueDecl>(SuperMember)) {
          SuperClassMembers.insert(SuperMemberVD);
        }
      }
      Super = Super->getSuperclassDecl();
    }
    // Remove any that are overridden by this class.
    for (const auto *DerivedMember : C->getMembers()) {
      if (const auto *DerivedMemberVD = dyn_cast<ValueDecl>(DerivedMember)) {
        if (const auto *Overridden = DerivedMemberVD->getOverriddenDecl()) {
          SuperClassMembers.erase(Overridden);
        }
      }
    }
    // What remains in SuperClassMembers are inherited members that
    // haven't been overridden by the class.
    // Add a synthesized relationship.
    for (const auto *InheritedMember : SuperClassMembers) {
      if (canIncludeDeclAsNode(InheritedMember)) {
        Symbol Source(this, InheritedMember, C);
        Symbol Target(this, C, nullptr);
        Nodes.insert(Source);
        recordEdge(Source, Target, RelationshipKind::MemberOf());
      }
    }
  }
}

void SymbolGraph::recordConformanceSynthesizedMemberRelationships(Symbol S) {
  if (!Walker.Options.EmitSynthesizedMembers) {
    return;
  }
  const auto VD = S.getSymbolDecl();
  const NominalTypeDecl *OwningNominal = nullptr;
  if (const auto *ThisNominal = dyn_cast<NominalTypeDecl>(VD)) {
    OwningNominal = ThisNominal;
  } else if (const auto *Extension = dyn_cast<ExtensionDecl>(VD)) {
    if (const auto *ExtendedNominal = Extension->getExtendedNominal()) {
      if (!ExtendedNominal->getModuleContext()->getNameStr()
          .equals(M.getNameStr())) {
        OwningNominal = ExtendedNominal;
      } else {
        return;
      }
    } else {
      return;
    }
  } else {
    return;
  }

  SynthesizedExtensionAnalyzer
  ExtensionAnalyzer(const_cast<NominalTypeDecl*>(OwningNominal),
      PrintOptions::printModuleInterface());
  auto MergeGroupKind = SynthesizedExtensionAnalyzer::MergeGroupKind::All;
  ExtensionAnalyzer.forEachExtensionMergeGroup(MergeGroupKind,
      [&](ArrayRef<ExtensionInfo> ExtensionInfos){
    for (const auto &Info : ExtensionInfos) {
      if (!Info.IsSynthesized) {
        continue;
      }

      // We are only interested in synthesized members that come from an
      // extension that we defined in our module.
      if (Info.EnablingExt && Info.EnablingExt->getModuleContext() != &M) {
        continue;
      }

      for (const auto ExtensionMember : Info.Ext->getMembers()) {
        if (const auto SynthMember = dyn_cast<ValueDecl>(ExtensionMember)) {
          if (SynthMember->isObjC()) {
            continue;
          }

          // There can be synthesized members on effectively private protocols
          // or things that conform to them. We don't want to include those.
          if (SynthMember->hasUnderscoredNaming()) {
            continue;
          }

          auto ExtendedSG =
              Walker.getModuleSymbolGraph(OwningNominal->getModuleContext());

          Symbol Source(this, SynthMember, OwningNominal);
          Symbol Target(this, OwningNominal, nullptr);

          ExtendedSG->Nodes.insert(Source);

          ExtendedSG->recordEdge(Source, Target, RelationshipKind::MemberOf());
         }
      }
    }
  });
}

void
SymbolGraph::recordInheritanceRelationships(Symbol S) {
  const auto VD = S.getSymbolDecl();
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

      recordEdge(Symbol(this, VD, nullptr),
                 Symbol(this, InheritedTypeDecl, nullptr),
                 RelationshipKind::InheritsFrom());
    }
  }
}

void SymbolGraph::recordDefaultImplementationRelationships(Symbol S) {
  const auto VD = S.getSymbolDecl();
  if (const auto *Extension = dyn_cast<ExtensionDecl>(VD->getDeclContext())) {
    if (const auto *Protocol = Extension->getExtendedProtocolDecl()) {
      for (const auto *Member : Protocol->getMembers()) {
        if (const auto *MemberVD = dyn_cast<ValueDecl>(Member)) {
          if (MemberVD->getFullName().compare(VD->getFullName()) == 0) {
            recordEdge(Symbol(this, VD, nullptr),
                       Symbol(this, MemberVD, nullptr),
                       RelationshipKind::DefaultImplementationOf());
          }
        }
      }
    }
  }
}

void
SymbolGraph::recordRequirementRelationships(Symbol S) {
  const auto VD = S.getSymbolDecl();
  if (const auto *Protocol = dyn_cast<ProtocolDecl>(VD->getDeclContext())) {
    if (VD->isProtocolRequirement()) {
      recordEdge(Symbol(this, VD, nullptr),
                 Symbol(this, Protocol, nullptr),
                 RelationshipKind::RequirementOf());
    }
  }
}

void SymbolGraph::recordOptionalRequirementRelationships(Symbol S) {
  const auto VD = S.getSymbolDecl();
  if (const auto *Protocol = dyn_cast<ProtocolDecl>(VD->getDeclContext())) {
    if (VD->isProtocolRequirement()) {
      if (const auto *ClangDecl = VD->getClangDecl()) {
        if (const auto *Method = dyn_cast<clang::ObjCMethodDecl>(ClangDecl)) {
          if (Method->isOptional()) {
            recordEdge(Symbol(this, VD, nullptr),
                       Symbol(this, Protocol, nullptr),
                       RelationshipKind::OptionalRequirementOf());
          }
        }
      }
    }
  }
}

void
SymbolGraph::recordConformanceRelationships(Symbol S) {
  const auto VD = S.getSymbolDecl();
  if (const auto *NTD = dyn_cast<NominalTypeDecl>(VD)) {
    for (const auto *Conformance : NTD->getAllConformances()) {
      recordEdge(Symbol(this, VD, nullptr),
        Symbol(this, Conformance->getProtocol(), nullptr),
        RelationshipKind::ConformsTo(),
        dyn_cast_or_null<ExtensionDecl>(Conformance->getDeclContext()));
    }
  }
}

void SymbolGraph::recordOverrideRelationship(Symbol S) {
  const auto VD = S.getSymbolDecl();
  if (const auto *Override = VD->getOverriddenDecl()) {
    recordEdge(Symbol(this, VD, nullptr),
               Symbol(this, Override, nullptr),
               RelationshipKind::Overrides());
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
              symbolgraphgen::serialize(Walker.Options.Target, OS);
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
      for (const auto S: Nodes) {
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
                                           const Symbol &S,
                                           llvm::json::OStream &OS) {
  DeclarationFragmentPrinter Printer(OS, Key);
  auto Options = getDeclarationFragmentsPrintOptions();
  if (S.getSynthesizedBaseType()) {
    Options.setBaseType(S.getSynthesizedBaseType());
  }
  S.getSymbolDecl()->print(Printer, Options);
}

void
SymbolGraph::serializeSubheadingDeclarationFragments(StringRef Key,
                                                     const Symbol &S,
                                                     llvm::json::OStream &OS) {
  DeclarationFragmentPrinter Printer(OS, Key);
  auto Options = getDeclarationFragmentsPrintOptions();
  Options.VarInitializers = false;
  Options.PrintDefaultArgumentValue = false;
  Options.PrintEmptyArgumentNames = false;
  Options.PrintOverrideKeyword = false;
  if (S.getSynthesizedBaseType()) {
    Options.setBaseType(S.getSynthesizedBaseType());
  }
  S.getSymbolDecl()->print(Printer, Options);
}

void
SymbolGraph::serializeDeclarationFragments(StringRef Key, Type T,
                                            llvm::json::OStream &OS) {
  DeclarationFragmentPrinter Printer(OS, Key);
  T->print(Printer, getDeclarationFragmentsPrintOptions());
}

bool SymbolGraph::isImplicitlyPrivate(const ValueDecl *VD) const {
  // Don't record unconditionally private declarations
  if (VD->isPrivateStdlibDecl(/*treatNonBuiltinProtocolsAsPublic=*/false)) {
    return true;
  }

  // Don't record effectively internal declarations if specified
  if (Walker.Options.MinimumAccessLevel > AccessLevel::Internal &&
      VD->hasUnderscoredNaming()) {
    return true;
  }

  // Symbols must meet the minimum access level to be included in the graph.
  if (VD->getFormalAccess() < Walker.Options.MinimumAccessLevel) {
    return true;
  }

  // Special cases below.

  auto BaseName = VD->getBaseName().userFacingName();

  // ${MODULE}Version{Number,String} in ${Module}.h
  SmallString<32> VersionNameIdentPrefix { M.getName().str() };
  VersionNameIdentPrefix.append("Version");
  if (BaseName.startswith(VersionNameIdentPrefix.str())) {
    return true;
  }

  // Automatically mapped SIMD types
  auto IsGlobalSIMDType = llvm::StringSwitch<bool>(BaseName)
#define MAP_SIMD_TYPE(C_TYPE, _, __) \
.Case("swift_" #C_TYPE "2", true) \
.Case("swift_" #C_TYPE "3", true) \
.Case("swift_" #C_TYPE "4", true)
#include "swift/ClangImporter/SIMDMappedTypes.def"
  .Case("SWIFT_TYPEDEFS", true)
  .Case("char16_t", true)
  .Case("char32_t", true)
  .Default(false);

  if (IsGlobalSIMDType) {
    return true;
  }

  // Check up the parent chain. Anything inside a privately named
  // thing is also private. We could be looking at the `B` of `_A.B`.
  if (const auto *DC = VD->getDeclContext()) {
    if (const auto *Parent = DC->getAsDecl()) {
      if (const auto *ParentVD = dyn_cast<ValueDecl>(Parent)) {
        return isImplicitlyPrivate(ParentVD);
      } else if (const auto *Extension = dyn_cast<ExtensionDecl>(Parent)) {
        if (const auto *Nominal = Extension->getExtendedNominal()) {
          return isImplicitlyPrivate(Nominal);
        }
      }
    }
  }
  return false;
}

/// Returns `true` if the symbol should be included as a node in the graph.
bool SymbolGraph::canIncludeDeclAsNode(const Decl *D) const {
  // If this decl isn't in this module, don't record it,
  // as it will appear elsewhere in its module's symbol graph.
  if (D->getModuleContext()->getName() != M.getName()) {
    return false;
  }

  if (D->isImplicit()) {
    return false;
  }

  if (!isa<ValueDecl>(D)) {
    return false;
  }
  return !isImplicitlyPrivate(cast<ValueDecl>(D));
}
