//===--- SILOptFunctionBuilder.h --------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_SILOPTFUNCTIONBUILDER_H
#define SWIFT_SILOPTIMIZER_UTILS_SILOPTFUNCTIONBUILDER_H

#include "swift/SIL/SILFunctionBuilder.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

namespace swift {

class SILOptFunctionBuilder {
  SILTransform &transform;
  SILFunctionBuilder builder;
  SILModule &module;

public:
  SILOptFunctionBuilder(SILTransform &transform, SILModule &module)
      : transform(transform), builder(module), module(module) {}
  SILOptFunctionBuilder(SILModuleTransform &transform)
      : SILOptFunctionBuilder(transform, *transform.getModule()) {}
  SILOptFunctionBuilder(SILFunctionTransform &transform)
      : SILOptFunctionBuilder(transform, transform.getModule()) {}

  template <class... ArgTys>
  SILFunction *getOrCreateSharedFunction(ArgTys &&... args) {
    SILFunction *f =
      builder.getOrCreateSharedFunction(std::forward<ArgTys>(args)...);
    notifyAddFunction(f);
    return f;
  }

  template <class... ArgTys>
  SILFunction *getOrCreateFunction(ArgTys &&... args) {
    SILFunction *f = builder.getOrCreateFunction(std::forward<ArgTys>(args)...);
    notifyAddFunction(f);
    return f;
  }

  template <class... ArgTys> SILFunction *createFunction(ArgTys &&... args) {
    SILFunction *f = builder.createFunction(std::forward<ArgTys>(args)...);
    notifyAddFunction(f);
    return f;
  }

  void eraseFunction(SILFunction *f) {
    auto &pm = getPassManager();
    pm.notifyWillDeleteFunction(f);
    getModule().eraseFunction(f);
  }

  SILModule &getModule() const { return module; }

private:
  SILPassManager &getPassManager() const {
    return *transform.getPassManager();
  }

  void notifyAddFunction(SILFunction *f) {
    auto &pm = getPassManager();
    pm.notifyOfNewFunction(f, &transform);
    pm.notifyAnalysisOfFunction(f);
  }
};

} // namespace swift

#endif
