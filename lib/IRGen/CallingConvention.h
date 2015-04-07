//===--- CallingConvention.h - Calling conventions --------------*- C++ -*-===//
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
//
// This file declares the interfaces for working with abstract and
// phsyical calling conventions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CALLINGCONVENTION_H
#define SWIFT_IRGEN_CALLINGCONVENTION_H

#include "llvm/IR/CallingConv.h"

namespace llvm {
  class AttributeSet;
  class Value;
}

namespace swift {
  class ValueDecl;
  enum class SILFunctionTypeRepresentation : uint8_t;

namespace irgen {
  class IRGenModule;

/// Expand an abstract SIL function type representation into a physical
/// convention.
llvm::CallingConv::ID expandCallingConv(IRGenModule &IGM,
                                     SILFunctionTypeRepresentation convention);

} // end namespace irgen
} // end namespace swift

#endif
