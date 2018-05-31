//===--- AutoDiff.h - Runtime support for autodiff --------------*- c++ -*-===//
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
//
// SWIFT_ENABLE_TENSORFLOW
//
//===----------------------------------------------------------------------===//

#include <stack>
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Metadata.h"

namespace swift {

/// Automatic differentiation tape, used to handle control flow.
struct AutoDiffTape {
  /// Element type of the tape.
  Metadata *type;
  std::vector<OpaqueValue *> elements;

  AutoDiffTape(Metadata *type) : type(type) {}
};

/// Create a new tape.
SWIFT_RUNTIME_EXPORT
OpaqueValue *swift_autodiffCreateTape(Metadata *);

/// Destroy a tape.
SWIFT_RUNTIME_EXPORT
void swift_autodiffDestroyTape(OpaqueValue *tape);

/// Push a value onto a tape.
SWIFT_RUNTIME_EXPORT
void swift_autodiffPushToTape(OpaqueValue *tape, OpaqueValue *value);

/// Pop a value from a tape.
SWIFT_RUNTIME_EXPORT
OpaqueValue *swift_autodiffPopFromTape(OpaqueValue *tape);

} // end namespace swift
