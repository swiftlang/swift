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
#include "SymbolGraphASTWalker.h"

using namespace swift;
using namespace symbolgraphgen;

void Edge::serialize(llvm::json::OStream &OS) const {
  OS.object([&](){
    OS.attribute("kind", Kind.Name);
    OS.attribute("source", Walker->getUSR(Source));
    OS.attribute("target", Walker->getUSR(Target));

    // In case a dependent module isn't available, serialize a fallback name.
    auto TargetModuleName = Target->getModuleContext()->getName().str();
    if (TargetModuleName != Walker->M.getName().str()) {
      auto TargetSymbolIdentifier = Walker->getSymbolIdentifier(Target);
      auto TargetComponents = TargetSymbolIdentifier.SimpleComponents;
      SmallString<128> Scratch(TargetModuleName);
      for (auto it = TargetComponents.begin();
           it != TargetComponents.end(); ++it) {
        Scratch.push_back('.');
        Scratch.append(*it);
      }
      OS.attribute("targetFallback", Scratch.str());
    }
  });
}
