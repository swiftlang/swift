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
  struct AttributeWithIndex;
  class Value;
}

namespace swift {
class ValueDecl;

namespace irgen {
class IRGenModule;

/// An abstract calling convention.
enum class AbstractCC : unsigned char {
  /// The C calling convention.
  C,

  /// The calling convention used for calling a normal function.
  Freestanding,

  /// The calling convention used for calling an instance method.
  Method
};

AbstractCC getAbstractCC(ValueDecl *fn);

/// Expand an abstract calling convention into a physical convention
/// and a set of attributes.
llvm::CallingConv::ID expandAbstractCC(IRGenModule &IGM, AbstractCC convention,
                                       bool hasAggResult,
                       llvm::SmallVectorImpl<llvm::AttributeWithIndex> &attrs);

} // end namespace irgen
} // end namespace swift

#endif
