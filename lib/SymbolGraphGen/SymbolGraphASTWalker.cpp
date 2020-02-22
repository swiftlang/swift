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
    Graph(Options, M, None, Options.Target, Ctx) {}

/// Get a "sub" symbol graph for the parent module of a type that the main module `M` is extending.
SymbolGraph &SymbolGraphASTWalker::getExtendedModuleSymbolGraph(ModuleDecl *M) {
  auto Found = ExtendedModuleGraphs.find(M);
  if (Found != ExtendedModuleGraphs.end()) {
    return *Found->getSecond();
  }
  auto *Memory = Ctx.allocate(sizeof(SymbolGraph), alignof(SymbolGraph));  
  auto *SG = new (Memory) SymbolGraph(Options,
                                      Graph.M,
                                      Optional<ModuleDecl *>(M),
                                      Options.Target,
                                      Ctx);
  ExtendedModuleGraphs.insert({M, SG});
  return *SG;
}

bool SymbolGraphASTWalker::walkToDeclPre(Decl *D, CharSourceRange Range) {

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
      break;
    case swift::DeclKind::Extension:
      // We don't want to descend into extensions on underscored types.
      return !cast<ExtensionDecl>(D)->getExtendedNominal()->hasUnderscoredNaming();
      
    // We'll descend into everything else.
    default:
      return true;
  }

  if (!Graph.canIncludeDeclAsNode(D)) {
    return false;
  }

  auto *VD = cast<ValueDecl>(D);

  // If this symbol extends a type from another module, record it in that
  // module's symbol graph, which will be emitted separately.
  if (const auto *Extension
          = dyn_cast_or_null<ExtensionDecl>(VD->getInnermostDeclContext())) {
    if (const auto *ExtendedNominal = Extension->getExtendedNominal()) {
      auto ExtendedModule = ExtendedNominal->getModuleContext();
      if (ExtendedModule != &M) {
        auto &SG = getExtendedModuleSymbolGraph(ExtendedModule);
        SG.recordNode(Symbol(&Graph, VD, nullptr));
        return true;
      }
    }
  }

  // Otherwise, record this in the main module `M`'s symbol graph.
  Graph.recordNode(Symbol(&Graph, VD, nullptr));

  return true;
}
