//===--- ErrorObjectTestSupport.h - Support for Instruments.app -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift runtime support for tests involving errors.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_ERROROBJECT_TEST_SUPPORT_H
#define SWIFT_RUNTIME_ERROROBJECT_TEST_SUPPORT_H

#include <atomic>

namespace swift {

#if defined(__cplusplus)
SWIFT_RUNTIME_EXPORT std::atomic<void (*)(SwiftError *error)> _swift_willThrow;
SWIFT_RUNTIME_EXPORT std::atomic<void (*)(
  OpaqueValue *value,
  const Metadata *type,
  const WitnessTable *errorConformance
)> _swift_willThrowTypedImpl;
#endif

/// Set the value of @c _swift_willThrow atomically.
///
/// This function is present for use by the standard library's test suite only.
SWIFT_CC(swift)
SWIFT_RUNTIME_STDLIB_SPI
void _swift_setWillThrowHandler(void (* handler)(SwiftError *error));
}

#endif
