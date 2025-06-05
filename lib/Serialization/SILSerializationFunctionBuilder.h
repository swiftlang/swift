//===--- SILSerializationFunctionBuilder.h --------------------------------===//
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

#ifndef SWIFT_SERIALIZATION_SERIALIZATIONFUNCTIONBUILDER_H
#define SWIFT_SERIALIZATION_SERIALIZATIONFUNCTIONBUILDER_H

#include "swift/SIL/SILFunctionBuilder.h"

namespace swift {

class LLVM_LIBRARY_VISIBILITY SILSerializationFunctionBuilder {
  SILFunctionBuilder builder;

public:
  SILSerializationFunctionBuilder(SILModule &mod) : builder(mod) {}

  /// Create a SILFunction declaration for use either as a forward reference or
  /// for the eventual deserialization of a function body.
  SILFunction *createDeclaration(StringRef name, SILType type,
                                 SILLocation loc) {
    return builder.createFunction(
        SILLinkage::Private, name, type.getAs<SILFunctionType>(), nullptr,
        loc, IsNotBare, IsNotTransparent,
        IsNotSerialized, IsNotDynamic, IsNotDistributed, IsNotRuntimeAccessible,
        ProfileCounter(), IsNotThunk, SubclassScope::NotApplicable);
  }

  void setHasOwnership(SILFunction *f, bool newValue) {
    builder.setHasOwnership(f, newValue);
  }
};

} // namespace swift

#endif
