//===--- SILParserFunctionBuilder.h ---------------------------------------===//
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

#ifndef SWIFT_PARSESIL_SILPARSERFUNCTIONBUILDER_H
#define SWIFT_PARSESIL_SILPARSERFUNCTIONBUILDER_H

#include "swift/SIL/SILFunctionBuilder.h"

namespace swift {

class LLVM_LIBRARY_VISIBILITY SILParserFunctionBuilder {
  SILFunctionBuilder builder;

public:
  SILParserFunctionBuilder(SILModule &mod) : builder(mod) {}

  SILFunction *createFunctionForForwardReference(StringRef name,
                                                 CanSILFunctionType ty,
                                                 SILLocation loc) {
    auto *result = builder.createFunction(
        SILLinkage::Private, name, ty, nullptr, loc, IsNotBare,
        IsNotTransparent, IsNotSerialized, IsNotDynamic, IsNotDistributed,
        IsNotRuntimeAccessible);
    result->setDebugScope(new (builder.mod) SILDebugScope(loc, result));

    // If we did not have a declcontext set, as a fallback set the parent module
    // of our SILFunction to the parent module of our SILModule.
    //
    // DISCUSSION: This ensures that we can perform protocol conformance checks.
    if (!result->getDeclContext()) {
      result->setParentModule(result->getModule().getSwiftModule());
    }

    return result;
  }
};

} // namespace swift

#endif
