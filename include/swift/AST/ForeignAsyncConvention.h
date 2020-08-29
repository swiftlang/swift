//===--- ForeignAsyncConvention.h - Async conventions -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the ForeignAsyncConvention structure, which
// describes the rules for how to detect that a foreign API is asynchronous.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FOREIGN_ASYNC_CONVENTION_H
#define SWIFT_FOREIGN_ASYNC_CONVENTION_H

#include "swift/AST/Type.h"

namespace swift {

/// A small structure describing the async convention of a foreign declaration.
class ForeignAsyncConvention {
  /// The index of the completion handler parameters.
  unsigned CompletionHandlerParamIndex;

  /// When non-zero, indicates which parameter to the completion handler is the
  /// Error? parameter (minus one) that makes this async function also throwing.
  unsigned CompletionHandlerErrorParamIndex;
public:
  ForeignAsyncConvention()
      : CompletionHandlerParamIndex(0), CompletionHandlerErrorParamIndex(0) { }

  ForeignAsyncConvention(unsigned completionHandlerParamIndex,
                         Optional<unsigned> completionHandlerErrorParamIndex)
      : CompletionHandlerParamIndex(completionHandlerParamIndex),
        CompletionHandlerErrorParamIndex(
          completionHandlerErrorParamIndex
            ? *completionHandlerErrorParamIndex + 1
            : 0) {}

  /// Retrieve the index of the completion handler parameter, which will be
  /// erased from the Swift signature of the imported async function.
  unsigned completionHandlerParamIndex() const {
    return CompletionHandlerParamIndex;
  }

  /// Retrieve the index of the \c Error? parameter in the completion handler's
  /// parameter list. When argument passed to this parameter is non-null, the
  /// provided error will be thrown by the async function.
  Optional<unsigned> completionHandlerErrorParamIndex() const {
    if (CompletionHandlerErrorParamIndex == 0)
      return None;

    return CompletionHandlerErrorParamIndex - 1;
  }

  /// Whether the async function is throwing due to the completion handler
  /// having an \c Error? parameter.
  ///
  /// Equivalent to \c static_cast<bool>(completionHandlerErrorParamIndex()).
  bool isThrowing() const {
    return CompletionHandlerErrorParamIndex != 0;
  }

  bool operator==(ForeignAsyncConvention other) const {
    return CompletionHandlerParamIndex == other.CompletionHandlerParamIndex
      && CompletionHandlerErrorParamIndex ==
        other.CompletionHandlerErrorParamIndex;
  }
  bool operator!=(ForeignAsyncConvention other) const {
    return !(*this == other);
  }
};

}

#endif
