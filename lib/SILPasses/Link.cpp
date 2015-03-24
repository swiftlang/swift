//===--- Link.cpp - Link in transparent SILFunctions from module ----------===//
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
#include "swift/SIL/SILModule.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILLinking(SILModule *M, bool LinkAll) {
  auto LinkMode = LinkAll? SILModule::LinkingMode::LinkAll :
    SILModule::LinkingMode::LinkNormal;
  for (auto &Fn : *M)
    M->linkFunction(&Fn, LinkMode);

  if (!LinkAll)
    return;

  M->linkAllWitnessTables();
  M->linkAllVTables();
}


namespace {

class SILLinker : public SILModuleTransform {

  /// The entry point to the transformation.
  void run() override {
    // Copies code from the standard library into the user program to enable
    // optimizations.
    SILModule &M = *getModule();
    for (auto &Fn : M)
      if (M.linkFunction(&Fn, SILModule::LinkingMode::LinkAll))
          invalidateAnalysis(&Fn, SILAnalysis::PreserveKind::Nothing);
  }

  StringRef getName() override { return "SIL Linker"; }
};
} // end anonymous namespace


SILTransform *swift::createSILLinker() {
  return new SILLinker();
}
