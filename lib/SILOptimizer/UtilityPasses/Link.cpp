//===--- Link.cpp - Link in transparent SILFunctions from module ----------===//
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
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/SerializedSILLoader.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

namespace {

/// Copies code from the standard library into the user program to enable
/// optimizations.
class SILLinker : public SILModuleTransform {
  SILModule::LinkingMode LinkMode;

public:
  explicit SILLinker(SILModule::LinkingMode LinkMode) : LinkMode(LinkMode) {}

  void run() override {
    SILModule &M = *getModule();
    for (auto &Fn : M)
      if (M.linkFunction(&Fn, LinkMode))
        invalidateAnalysis(&Fn, SILAnalysis::InvalidationKind::Everything);

    // In embedded Swift, the stdlib contains all the runtime functions needed
    // (swift_retain, etc.). Link them in so they can be referenced in IRGen.
    if (M.getOptions().EmbeddedSwift) {
      linkEmbeddedRuntimeFromStdlib();
    }
  }

  void linkEmbeddedRuntimeFromStdlib() {
    #define FUNCTION(ID, NAME, CC, AVAILABILITY, RETURNS, ARGS, ATTRS, EFFECT) \
      linkEmbeddedRuntimeFunctionByName(#NAME);

    #define RETURNS(...)
    #define ARGS(...)
    #define NO_ARGS
    #define ATTRS(...)
    #define NO_ATTRS
    #define EFFECT(...)

    #include "swift/Runtime/RuntimeFunctions.def"
  }

  void linkEmbeddedRuntimeFunctionByName(StringRef name) {
    SILModule &M = *getModule();

    // Bail if runtime function is already loaded.
    if (M.lookUpFunction(name)) return;

    SILFunction *Fn =
        M.getSILLoader()->lookupSILFunction(name, SILLinkage::PublicExternal);
    if (!Fn) return;

    if (M.linkFunction(Fn, LinkMode))
      invalidateAnalysis(Fn, SILAnalysis::InvalidationKind::Everything);
  }
};
} // end anonymous namespace


SILTransform *swift::createMandatorySILLinker() {
  return new SILLinker(SILModule::LinkingMode::LinkNormal);
}

SILTransform *swift::createPerformanceSILLinker() {
  return new SILLinker(SILModule::LinkingMode::LinkAll);
}
