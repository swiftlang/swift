//===--- SymbolGraphASTWalker.cpp - Symbol Graph AST Walker ---------------===//
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
#include "clang/Basic/Module.h"
#include "llvm/ADT/StringSwitch.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/SymbolGraphGen/SymbolGraphGen.h"

#include "SymbolGraphASTWalker.h"

using namespace swift;
using namespace symbolgraphgen;

namespace {

/// Compare the two \c ModuleDecl instances to see whether they are the same.
///
/// This does a by-name comparison to consider a module's underlying Clang module to be equivalent
/// to the wrapping module of the same name.
///
/// If the `isClangEqual` argument is set to `false`, the modules must also be from the same
/// compiler, i.e. a Swift module and its underlying Clang module would be considered not equal.
bool areModulesEqual(const ModuleDecl *lhs, const ModuleDecl *rhs, bool isClangEqual = true) {
  if (lhs->getNameStr() != rhs->getNameStr())
    return false;

  if (!isClangEqual && (lhs->isNonSwiftModule() != rhs->isNonSwiftModule()))
    return false;

  return true;
}

bool clangModuleExports(const clang::Module *ClangParent, const clang::Module *CM) {
  if (!ClangParent || !CM) return false;
  if (ClangParent == CM) return true;

  for (auto ClangExport : ClangParent->Exports) {
    auto *ExportedModule = ClangExport.getPointer();
    if (ClangExport.getInt()) {
      if (!ExportedModule && CM->isSubModuleOf(ClangParent)) {
        return true;
      } else if (ExportedModule && CM->isSubModuleOf(ExportedModule)) {
        return true;
      }
    }
    if (ExportedModule && clangModuleExports(ExportedModule, CM)) {
      return true;
    }
  }

  if (ClangParent->Exports.empty() && CM->isSubModuleOf(ClangParent)) {
    // HACK: In the absence of an explicit export statement, consider any submodule to be exported.
    return true;
  }

  return false;
}

bool underlyingClangModuleExports(const ModuleDecl *ParentModule, const ModuleDecl *M) {
  return clangModuleExports(ParentModule->findUnderlyingClangModule(), M->findUnderlyingClangModule());
}

} // anonymous namespace

SymbolGraphASTWalker::SymbolGraphASTWalker(ModuleDecl &M,
                                           const SymbolGraphOptions &Options)
    : Options(Options), M(M), MainGraph(*this, M, std::nullopt, Ctx) {}

SymbolGraphASTWalker::SymbolGraphASTWalker(
    ModuleDecl &M,
    const SmallPtrSet<const ModuleDecl *, 4> ExportedImportedModules,
    const llvm::SmallDenseMap<const ModuleDecl *, SmallPtrSet<Decl *, 4>, 4>
        QualifiedExportedImports,
    const SymbolGraphOptions &Options)
    : Options(Options), M(M), ExportedImportedModules(ExportedImportedModules),
      QualifiedExportedImports(QualifiedExportedImports),
      MainGraph(*this, M, std::nullopt, Ctx) {}

ModuleDecl *SymbolGraphASTWalker::getRealModuleOf(const Decl *D) const {
  ModuleDecl *Module = D->getModuleContext();
  if (auto *ClangDecl = D->getClangDecl())
    if (auto *ClangModule = ClangDecl->getOwningModule())
      if (auto *ClangModuleLoader = D->getASTContext().getClangModuleLoader())
        if (auto *M = ClangModuleLoader->getWrapperForModule(ClangModule))
          Module = M;

  return Module;
}

/// Get a "sub" symbol graph for the parent module of a type that
/// the main module `M` is extending.
SymbolGraph *SymbolGraphASTWalker::getModuleSymbolGraph(const Decl *D) {
  auto *M = getRealModuleOf(D);
  const auto *DC = D->getDeclContext();
  SmallVector<const NominalTypeDecl *, 2> ParentTypes = {};
  const Decl *ExtendedNominal = nullptr;
  while (DC) {
    if (const auto *NTD = dyn_cast_or_null<NominalTypeDecl>(DC->getAsDecl())) {
      DC = NTD->getDeclContext();
      M = getRealModuleOf(NTD);
      ParentTypes.push_back(NTD);
    } else if (const auto *Ext = dyn_cast_or_null<ExtensionDecl>(DC->getAsDecl())) {
      DC = Ext->getExtendedNominal()->getDeclContext();
      M = getRealModuleOf(Ext->getExtendedNominal());
      if (!ExtendedNominal)
        ExtendedNominal = Ext->getExtendedNominal();
    } else {
      DC = nullptr;
    }
  }

  auto moduleIsMainGraph = [&](const ModuleDecl *M) {
    if (areModulesEqual(&this->M, M)) {
      return true;
    } else if (MainGraph.DeclaringModule.has_value() &&
               areModulesEqual(MainGraph.DeclaringModule.value(), M)) {
      // Cross-import overlay modules already appear as "extensions" of their declaring module; we
      // should put actual extensions of that module into the main graph
      return true;
    }

    // Check the module and decl separately since the extension could be from a different module
    // than the decl itself.
    if (isExportedImportedModule(M)) {
      return true;
    }

    return false;
  };

  if (moduleIsMainGraph(M) || isQualifiedExportedImport(D))
    return &MainGraph;

  // If this type is the child of a type which was re-exported in a qualified export, use the main graph.
  if (llvm::any_of(ParentTypes, [&](const NominalTypeDecl *NTD){ return isQualifiedExportedImport(NTD); })) {
    return &MainGraph;
  }

  // As a shorthand when dealing with Clang submodules, use their top-level module's graph if the
  // submodule is ultimately exported from its top-level module.
  auto *TopLevelModule = M->getTopLevelModule();
  if (TopLevelModule != M && underlyingClangModuleExports(TopLevelModule, M))
    M = TopLevelModule;

  if (moduleIsMainGraph(M))
    return &MainGraph;

  auto Found = ExtendedModuleGraphs.find(M->getNameStr());
  if (Found != ExtendedModuleGraphs.end()) {
    return Found->getValue();
  }
  auto *Memory = Ctx.allocate(sizeof(SymbolGraph), alignof(SymbolGraph));
  auto *SG = new (Memory)
      SymbolGraph(*this, MainGraph.M, std::optional<ModuleDecl *>(M), Ctx);

  ExtendedModuleGraphs.insert({M->getNameStr(), SG});
  return SG;
}

static bool isUnavailableOrObsoletedOnPlatform(const Decl *D) {
  if (const auto Avail = D->getUnavailableAttr()) {
    if (Avail->getPlatform() != PlatformKind::none)
      return true;
  }
  return false;
}

bool SymbolGraphASTWalker::walkToDeclPre(Decl *D, CharSourceRange Range) {
  if (SynthesizedChildrenBaseDecl && D == SynthesizedChildrenBaseDecl)
    return true;

  if (isUnavailableOrObsoletedOnPlatform(D)) {
    return false;
  }

  switch (D->getKind()) {
  // We'll record nodes for the following kinds of declarations.
  case swift::DeclKind::Class:
  case swift::DeclKind::Struct:
  case swift::DeclKind::Enum:
  case swift::DeclKind::EnumElement:
  case swift::DeclKind::Protocol:
  case swift::DeclKind::Constructor:
  case swift::DeclKind::Func:
  case swift::DeclKind::Var:
  case swift::DeclKind::Subscript:
  case swift::DeclKind::TypeAlias:
  case swift::DeclKind::AssociatedType:
  case swift::DeclKind::Extension:
  case swift::DeclKind::Macro:
    break;

  // We'll descend into everything else.
  default:
    return true;
  }

  auto SG = getModuleSymbolGraph(D);

  // If this is an extension, let's check that it implies some new conformances,
  // potentially with generic requirements.
  if (const auto *Extension = dyn_cast<ExtensionDecl>(D)) {
    const auto *ExtendedNominal = Extension->getExtendedNominal();
    auto ExtendedSG = getModuleSymbolGraph(ExtendedNominal);
    // Ignore effectively private decls.
    if (ExtendedSG->isImplicitlyPrivate(Extension)) {
      return false;
    }

    if (SG->isUnconditionallyUnavailableOnAllPlatforms(Extension)) {
      return false;
    }

    if (isUnavailableOrObsoletedOnPlatform(ExtendedNominal)) {
      return false;
    }

    // We only treat extensions to external types as extensions. Extensions to
    // local types are directly associated with the extended nominal.
    auto const shouldBeRecordedAsExtension =
        this->shouldBeRecordedAsExtension(Extension);

    Symbol Source = shouldBeRecordedAsExtension
                        ? Symbol(ExtendedSG, Extension, nullptr)
                        : Symbol(ExtendedSG, ExtendedNominal, nullptr);
    // The extended nominal is recorded elsewhere for local types.
    if (shouldBeRecordedAsExtension) {
      ExtendedSG->recordNode(Source);

      // Next to the extension symbol itself, we also introduce a relationship
      // between the extension symbol and the extended nominal.
      ExtendedSG->recordEdge(Source,
                             Symbol(ExtendedSG, ExtendedNominal, nullptr),
                             RelationshipKind::ExtensionTo());
    }

    // If there are some protocol conformances on this extension, we'll
    // grab them for some new conformsTo relationships.
    if (!Extension->getInherited().empty()) {
      // We want to add conformsTo relationships for all protocols implicitly
      // implied by those explicitly stated on the extension.
      //
      // We start by collecting the conformances declared on the extension with
      // `getLocalConformances`. From there, we inspect each protocol for any
      // other protocols it inherits (whether stated explicitly or via a
      // composed protocol type alias) with `getInheritedProtocols`. Each new
      // protocol is added to `UnexpandedProtocols` until there are no new
      // protocols to add. At that point, all direct and indirect conformances
      // are stored in `Protocols`.

      SmallPtrSet<const ProtocolDecl *, 4> Protocols;
      SmallVector<const ProtocolDecl *, 4> UnexpandedProtocols;

      // Start the process with the conformances stated
      // explicitly on the extension.
      for (const auto *Conformance : Extension->getLocalConformances()) {
        UnexpandedProtocols.push_back(Conformance->getProtocol());
      }

      // "Recursively" expand the unexpanded list and populate
      // the expanded `Protocols` list (in an iterative manner).
      while (!UnexpandedProtocols.empty()) {
        const auto *Proto = UnexpandedProtocols.pop_back_val();
        if (!Protocols.contains(Proto)) {
          for (const auto *InheritedProtocol : Proto->getInheritedProtocols()) {
            UnexpandedProtocols.push_back(InheritedProtocol);
          }
          Protocols.insert(Proto);
        }
      }

      // Record the expanded list of protocols.
      for (const auto *Proto : Protocols) {
        Symbol Target(&MainGraph, Proto, nullptr);
        ExtendedSG->recordEdge(Source, Target, RelationshipKind::ConformsTo(),
                               Extension);
      }

      // We also might establish some synthesized members because we
      // extended an external type.
      if (ExtendedNominal->getModuleContext() != &M) {
        ExtendedSG->recordConformanceSynthesizedMemberRelationships(Source);
      }
    }

    // Continue looking into the extension.
    return true;
  }

  auto *VD = cast<ValueDecl>(D);

  if (!BaseDecl && !SG->canIncludeDeclAsNode(VD)) {
    return false;
  }

  // If this symbol extends a type from another module, record it in that
  // module's symbol graph, which will be emitted separately.
  if (const auto *Extension
      = dyn_cast_or_null<ExtensionDecl>(VD->getDeclContext())) {
    if (const auto *ExtendedNominal = Extension->getExtendedNominal()) {
      auto ExtendedModule = ExtendedNominal->getModuleContext();
      auto ExtendedSG = getModuleSymbolGraph(ExtendedNominal);
      if (!isOurModule(ExtendedModule)) {
        ExtendedSG->recordNode(Symbol(ExtendedSG, VD, nullptr));
        return true;
      }
    }
  }

  // Clang decls that are inherited from protocols get the USR of the protocol
  // symbol, regardless of which class it's actually appearing on. To prevent
  // multiple of these symbols colliding with each other, treat them as
  // synthesized symbols and use their parent type as the base type.
  if (VD->isImplicit() && VD->hasClangNode() &&
      VD->getClangNode().getAsDecl()) {
    if (const auto *Parent =
            dyn_cast_or_null<NominalTypeDecl>(VD->getDeclContext())) {
      SG->recordNode(Symbol(SG, VD, Parent));
      return true;
    }
  }

  // If this is a Clang typedef of an underlying type that is being hidden (e.g. `typedef struct
  // _MyStruct { ... } MyStruct`) then copy in the child symbols from the underlying type to the
  // type alias.
  if (const auto *TD = dyn_cast_or_null<TypeAliasDecl>(VD)) {
    const auto InnerType = TD->getUnderlyingType();
    if (NominalTypeDecl *NTD = InnerType->getAnyNominal()) {
      // Only fold typedefs together if the inner type is from our module and it
      // otherwise isn't being shown
      if (isOurModule(NTD->getModuleContext()) &&
          !SG->canIncludeDeclAsNode(NTD)) {
        // We specifically only want to look for underlying types that are "embedded" in the typedef
        // definition, so let's pull out the Clang decl and check for that
        if (NTD->hasClangNode()) {
          if (const auto *ClangDecl = NTD->getClangNode().getAsDecl()) {
            if (const auto *ClangTagDecl = dyn_cast<clang::TagDecl>(ClangDecl)) {
              if (ClangTagDecl->isEmbeddedInDeclarator()) {
                PublicPrivateTypeAliases.insert_or_assign(NTD, TD);
                synthesizeChildSymbols(NTD, TD);
              }
            }
          }
        }
      }
    }
  }

  // Otherwise, record this in the main module `M`'s symbol graph.
  SG->recordNode(Symbol(SG, VD, BaseDecl));

  return true;
}

bool SymbolGraphASTWalker::isConsideredExportedImported(const Decl *D) const {
  // Check to see if this decl is an extension of something else that was re-exported.
  // Do this first in case there's a chain of extensions that leads somewhere that's not a re-export.
  // FIXME: this considers synthesized members of extensions to be valid
  const auto *DC = D->getDeclContext();
  const Decl *ExtendedNominal = nullptr;
  while (DC && !ExtendedNominal) {
    if (const auto *ED = dyn_cast_or_null<ExtensionDecl>(DC->getAsDecl())) {
      ExtendedNominal = ED->getExtendedNominal();
    } else {
      DC = DC->getParent();
    }
  }

  if (ExtendedNominal && isConsideredExportedImported(ExtendedNominal)) {
    return true;
  }

  // Check to see if the decl is a child symbol of another decl that was re-exported.
  DC = D->getDeclContext();
  if (DC) {
    if (const auto *VD = dyn_cast_or_null<ValueDecl>(DC->getAsDecl())) {
      if (isConsideredExportedImported(VD))
        return true;
    }
  }

  // Check the decl itself to see if it was directly re-exported.
  if (isFromExportedImportedModule(D) || isQualifiedExportedImport(D))
    return true;

  // If none of the other checks passed, this wasn't from a re-export.
  return false;
}

bool SymbolGraphASTWalker::isFromExportedImportedModule(const Decl* D, bool countUnderlyingClangModule) const {
  auto *M = getRealModuleOf(D);
  return isQualifiedExportedImport(D) || isExportedImportedModule(M, countUnderlyingClangModule);
}

bool SymbolGraphASTWalker::isQualifiedExportedImport(const Decl *D) const {
  return llvm::any_of(QualifiedExportedImports, [&D](const auto &QI) {
    return QI.getSecond().contains(D);
  });
}

bool SymbolGraphASTWalker::isExportedImportedModule(const ModuleDecl *M, bool countUnderlyingClangModule) const {
  return llvm::any_of(ExportedImportedModules, [&M, countUnderlyingClangModule](const auto *MD) {
    return areModulesEqual(M, MD->getModuleContext(), /*isClangEqual*/countUnderlyingClangModule);
  });
}

bool SymbolGraphASTWalker::isOurModule(const ModuleDecl *M) const {
  return areModulesEqual(M, &this->M) || isExportedImportedModule(M);
}

bool SymbolGraphASTWalker::shouldBeRecordedAsExtension(
    const ExtensionDecl *ED) const {
  return Options.EmitExtensionBlockSymbols &&
         !areModulesEqual(ED->getModuleContext(),
                          ED->getExtendedNominal()->getModuleContext()) &&
         !isExportedImportedModule(
             ED->getExtendedNominal()->getModuleContext());
}

bool SymbolGraphASTWalker::synthesizeChildSymbols(Decl *D,
                                                  const ValueDecl *BD) {
  BaseDecl = BD;
  SynthesizedChildrenBaseDecl = D;
  SWIFT_DEFER {
    BaseDecl = nullptr;
    SynthesizedChildrenBaseDecl = nullptr;
  };

  return walk(D);
}
