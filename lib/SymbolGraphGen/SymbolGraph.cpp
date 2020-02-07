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

#include "swift/AST/Module.h"
#include "swift/Basic/Version.h"

#include "FormatVersion.h"
#include "SymbolGraph.h"

using namespace swift;
using namespace symbolgraphgen;

SymbolGraph::SymbolGraph(ModuleDecl &M, llvm::Triple Target,
                         Optional<llvm::VersionTuple> ModuleVersion)
: M(M), Target(Target), ModuleVersion(ModuleVersion) {}

void SymbolGraph::serialize(SymbolGraphASTWalker &Walker,
                            llvm::json::OStream &OS) const {
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
      symbolgraphgen::serialize(Target, OS);
    });

    if (ModuleVersion) {
      AttributeRAII MV("moduleVersion", OS);
      symbolgraphgen::serialize(*ModuleVersion, OS);
    }

    OS.attributeArray("symbols", [&](){
      for (const auto *VD: Nodes) {
        Symbol S { VD };
        S.serialize(Walker, OS);
      }
    });

    OS.attributeArray("relationships", [&](){
      for (const auto Relationship : Edges) {
        Relationship.serialize(OS);
      }
    });

  });
}
