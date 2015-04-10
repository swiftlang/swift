//===--- ExternalDefinitionsToDeclarations.cpp - external defs to decls ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

namespace {

class ExternalDefsToDecls : public SILModuleTransform {
  virtual ~ExternalDefsToDecls() {}

  void run() override {
    for (auto &F : *getModule()) {
      SILLinkage linkage = F.getLinkage();
      if (isAvailableExternally(linkage) && F.isDefinition() &&
          !hasSharedVisibility(linkage)) {
        F.convertToDeclaration();
        invalidateAnalysis(&F, SILAnalysis::PreserveKind::Nothing);
      }
    }
  }

  StringRef getName() override { return "External Defs To Decls"; }
};

} // end anonymous namespace


SILTransform *swift::createExternalDefsToDecls() {
  return new ExternalDefsToDecls();
}
