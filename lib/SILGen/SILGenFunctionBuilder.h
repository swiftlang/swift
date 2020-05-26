//===--- SILGenFunctionBuilder.h ------------------------------------------===//
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

#ifndef SWIFT_SILGEN_SILGENFUNCTIONBUILDER_H
#define SWIFT_SILGEN_SILGENFUNCTIONBUILDER_H

#include "swift/SIL/SILFunctionBuilder.h"

namespace swift {
namespace Lowering {

class LLVM_LIBRARY_VISIBILITY SILGenFunctionBuilder {
  SILFunctionBuilder builder;

public:
  SILGenFunctionBuilder(SILGenModule &SGM) : builder(SGM.M) {}
  SILGenFunctionBuilder(SILGenFunction &SGF) : builder(SGF.SGM.M) {}

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

} // namespace Lowering
} // namespace swift

#endif
