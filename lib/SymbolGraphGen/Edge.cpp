//===--- Edge.cpp - Symbol Graph Edge -------------------------------------===//
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

#include "swift/AST/Module.h"
#include "Edge.h"
#include "Symbol.h"
#include "SymbolGraphASTWalker.h"

#include <queue>

using namespace swift;
using namespace symbolgraphgen;

namespace {
const ValueDecl *getForeignProtocolRequirement(const ValueDecl *VD, const ModuleDecl *M) {
  std::queue<const ValueDecl *> requirements;
  while (true) {
    for (auto *req : VD->getSatisfiedProtocolRequirements()) {
      if (req->getModuleContext()->getNameStr() != M->getNameStr())
        return req;
      else
        requirements.push(req);
    }
    if (requirements.empty())
      return nullptr;
    VD = requirements.front();
    requirements.pop();
  }
}
} // end anonymous namespace

void Edge::serialize(llvm::json::OStream &OS) const {
  OS.object([&](){
    OS.attribute("kind", Kind.Name);
    SmallString<256> SourceUSR, TargetUSR;

    Source.getUSR(SourceUSR);
    OS.attribute("source", SourceUSR.str());

    Target.getUSR(TargetUSR);
    OS.attribute("target", TargetUSR.str());

    // In case a dependent module isn't available, serialize a fallback name.
    auto TargetModuleName = Target.getSymbolDecl()
        ->getModuleContext()->getName().str();

    if (TargetModuleName != Graph->M.getName().str()) {
      SmallString<128> Scratch(TargetModuleName);
      llvm::raw_svector_ostream PathOS(Scratch);
      PathOS << '.';
      Target.printPath(PathOS);
      OS.attribute("targetFallback", Scratch.str());
    }

    if (ConformanceExtension) {
      SmallVector<Requirement, 4> FilteredRequirements;
      filterGenericRequirements(
          ConformanceExtension->getGenericRequirements(),
          ConformanceExtension->getExtendedNominal()
              ->getDeclContext()->getSelfNominalTypeDecl(),
          FilteredRequirements);
      if (!FilteredRequirements.empty()) {
        OS.attributeArray("swiftConstraints", [&](){
          for (const auto &Req :
               ConformanceExtension->getGenericRequirements()) {
            ::serialize(Req, OS);
          }
        });
      }
    }
    
    const ValueDecl *InheritingDecl = nullptr;
    if (const auto *ID = Source.getDeclInheritingDocs()) {
      if (Target.getSymbolDecl() == ID || Source.getSynthesizedBaseTypeDecl())
        InheritingDecl = ID;
    }

    if (!InheritingDecl && Source.getSynthesizedBaseTypeDecl())
      InheritingDecl = Source.getSymbolDecl();

    if (!InheritingDecl) {
      if (const auto *ID = getForeignProtocolRequirement(Source.getSymbolDecl(), &Graph->M))
        InheritingDecl = ID;
    }
    
    // If our source symbol is a inheriting decl, write in information about
    // where it's inheriting docs from.
    if (InheritingDecl) {
      Symbol inheritedSym(Graph, InheritingDecl, nullptr);
      SmallString<256> USR, Display;
      llvm::raw_svector_ostream DisplayOS(Display);
      
      inheritedSym.getUSR(USR);
      inheritedSym.printPath(DisplayOS);
      
      OS.attributeObject("sourceOrigin", [&](){
        OS.attribute("identifier", USR.str());
        OS.attribute("displayName", Display.str());
      });
    }
  });
}
