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

namespace swift {

class SILOptFunctionBuilder {
  SILFunctionBuilder builder;

public:
  SILOptFunctionBuilder(SILPassManager &passManager)
      : builder(*passManager.getModule()) {}

  template <class... ArgTys>
  SILFunction *getOrCreateSharedFunction(ArgTys &&... args) {
    return builder.getOrCreateSharedFunction(std::forward<ArgTys>(args)...);
  }

  template <class... ArgTys>
  SILFunction *getOrCreateFunction(ArgTys &&... args) {
    return builder.getOrCreateFunction(std::forward<ArgTys>(args)...);
  }

  template <class... ArgTys> SILFunction *createFunction(ArgTys &&... args) {
    return builder.createFunction(std::forward<ArgTys>(args)...);
  }
};

} // namespace swift

#endif
