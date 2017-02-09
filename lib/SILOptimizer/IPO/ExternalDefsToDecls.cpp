//===--- ExternalDefsToDecls.cpp - external defs to decls -----------------===//
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

#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

namespace {

class ExternalDefsToDecls : public SILModuleTransform {
  ~ExternalDefsToDecls() override {}

  void run() override {
    for (auto &F : *getModule()) {
      SILLinkage linkage = F.getLinkage();
      if (isAvailableExternally(linkage) && F.isDefinition() &&
          !hasSharedVisibility(linkage) && !F.isTransparent()) {
        F.convertToDeclaration();
        invalidateAnalysis(&F, SILAnalysis::InvalidationKind::FunctionBody);
      }
    }
  }

  StringRef getName() override { return "External Defs To Decls"; }
};

} // end anonymous namespace


SILTransform *swift::createExternalDefsToDecls() {
  return new ExternalDefsToDecls();
}
