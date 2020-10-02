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

#include "llvm/ADT/StringSwitch.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/SymbolGraphGen/SymbolGraphGen.h"

#include "SymbolGraphASTWalker.h"

using namespace swift;
using namespace symbolgraphgen;

SymbolGraphASTWalker::SymbolGraphASTWalker(ModuleDecl &M,
                                           const SymbolGraphOptions &Options)
  : Options(Options),
    M(M),
    MainGraph(*this, M, None, Ctx) {}

/// Get a "sub" symbol graph for the parent module of a type that
/// the main module `M` is extending.
SymbolGraph *SymbolGraphASTWalker::getModuleSymbolGraph(const Decl *D) {
  auto *M = D->getModuleContext();
  const auto *DC = D->getDeclContext();
  while (DC) {
    M = DC->getParentModule();
    if (const auto *NTD = dyn_cast_or_null<NominalTypeDecl>(DC->getAsDecl())) {
      DC = NTD->getDeclContext();
    } else if (const auto *Ext = dyn_cast_or_null<ExtensionDecl>(DC->getAsDecl())) {
      DC = Ext->getExtendedNominal()->getDeclContext();
    } else {
      DC = nullptr;
    }
  }

  if (this->M.getNameStr().equals(M->getNameStr())) {
    return &MainGraph;
  }
  auto Found = ExtendedModuleGraphs.find(M->getNameStr());
  if (Found != ExtendedModuleGraphs.end()) {
    return Found->getValue();
  }
  auto *Memory = Ctx.allocate(sizeof(SymbolGraph), alignof(SymbolGraph));  
  auto *SG = new (Memory) SymbolGraph(*this,
                                      MainGraph.M,
                                      Optional<ModuleDecl *>(M),
                                      Ctx);

  ExtendedModuleGraphs.insert({M->getNameStr(), SG});
  return SG;
}

namespace {
bool isUnavailableOrObsoleted(const Decl *D) {
  if (const auto *Avail =
        D->getAttrs().getUnavailable(D->getASTContext())) {
    switch (Avail->getVersionAvailability(D->getASTContext())) {
      case AvailableVersionComparison::Unavailable:
      case AvailableVersionComparison::Obsoleted:
        return true;
      default:
        break;
    }
  }
  return false;
}
} // end anonymous namespace

bool SymbolGraphASTWalker::walkToDeclPre(Decl *D, CharSourceRange Range) {
    if (isUnavailableOrObsoleted(D)) {
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

    if (isUnavailableOrObsoleted(ExtendedNominal)) {
      return false;
    }

    // If there are some protocol conformances on this extension, we'll
    // grab them for some new conformsTo relationships.
    if (!Extension->getInherited().empty()) {

      // The symbol graph to use to record these relationships.
      SmallVector<const ProtocolDecl *, 4> Protocols;
      SmallVector<const ProtocolCompositionType *, 4> UnexpandedCompositions;

      auto HandleProtocolOrComposition = [&](Type Ty) {
        if (const auto *Proto =
            dyn_cast_or_null<ProtocolDecl>(Ty->getAnyNominal())) {
          Protocols.push_back(Proto);
        } else if (const auto *Comp = Ty->getAs<ProtocolCompositionType>()) {
          UnexpandedCompositions.push_back(Comp);
        } else {
          abort();
        }
      };

      for (const auto &InheritedLoc : Extension->getInherited()) {
        auto InheritedTy = InheritedLoc.getType();
        if (!InheritedTy) {
          continue;
        }
        HandleProtocolOrComposition(InheritedTy);
      }

      while (!UnexpandedCompositions.empty()) {
        const auto *Comp = UnexpandedCompositions.pop_back_val();
        for (const auto &Member : Comp->getMembers()) {
          HandleProtocolOrComposition(Member);
        }
      }

      Symbol Source(ExtendedSG, ExtendedNominal, nullptr);

      for (const auto *Proto : Protocols) {
        Symbol Target(&MainGraph, Proto, nullptr);
        ExtendedSG->recordEdge(Source, Target, RelationshipKind::ConformsTo(),
                               Extension);
      }

      // While we won't record this node per se, or all of the other kinds of
      // relationships, we might establish some synthesized members because we
      // extended an external type.
      if (ExtendedNominal->getModuleContext() != &M) {
        ExtendedSG->recordConformanceSynthesizedMemberRelationships({
          ExtendedSG,
          ExtendedNominal,
          nullptr
        });
      }
    }

    // Continue looking into the extension.
    return true;
  }

  auto *VD = cast<ValueDecl>(D);

  if (!SG->canIncludeDeclAsNode(VD)) {
    return false;
  }

  // If this symbol extends a type from another module, record it in that
  // module's symbol graph, which will be emitted separately.
  if (const auto *Extension
      = dyn_cast_or_null<ExtensionDecl>(VD->getDeclContext())) {
    if (const auto *ExtendedNominal = Extension->getExtendedNominal()) {
      auto ExtendedModule = ExtendedNominal->getModuleContext();
      auto ExtendedSG = getModuleSymbolGraph(ExtendedNominal);
      if (ExtendedModule != &M) {
        ExtendedSG->recordNode(Symbol(ExtendedSG, VD, nullptr));
        return true;
      }
    }
  }

  // Otherwise, record this in the main module `M`'s symbol graph.
  SG->recordNode(Symbol(SG, VD, nullptr));

  return true;
}
