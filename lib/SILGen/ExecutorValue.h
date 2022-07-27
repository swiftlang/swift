//===--- ExecutorValue.h - smart executor values for SILGen ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILGEN_EXECUTORVALUE_H
#define SWIFT_SILGEN_EXECUTORVALUE_H

#include "swift/SIL/SILValue.h"

namespace swift {
namespace Lowering {

class SILGenBuilder;

/// A thin wrapper around a raw SILValue that represents an executor.
struct ExecutorValue {
  SILValue rawValue;

  ExecutorValue() : rawValue() {}
  ExecutorValue(SILValue val) : rawValue(val) {}
  ExecutorValue(SILValue const& val) : rawValue(val) {}
  ExecutorValue& operator=(SILValue const& val) {
    rawValue = val;
    return *this;
  }

  operator bool() const { return rawValue; }

  void emitHopToExecutor(SILLocation, SILGenBuilder, bool isMandatory);

  /// \returns a value of type Builtin.Executor
  SILValue emitGetCurrentExecutor(SILLocation, SILGenBuilder);
};

} // Lowering
} // swift

#endif // SWIFT_SILGEN_EXECUTORVALUE_H
