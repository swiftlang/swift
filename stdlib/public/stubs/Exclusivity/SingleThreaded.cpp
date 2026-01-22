//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Single-threaded implementation of dynamic exclusivity checking. This file
// should be linked in to single-threaded Embedded Swift applications that
// require dynamic exclusivity checking.
//===----------------------------------------------------------------------===//

#include "swift/shims/Visibility.h"

extern "C" {
void * _Nullable _swift_exclusivity_single_threaded;

SWIFT_RUNTIME_STDLIB_INTERNAL
void * _Nullable _swift_getExclusivityTLS() {
  return _swift_exclusivity_single_threaded;
}

SWIFT_RUNTIME_STDLIB_INTERNAL
void _swift_setExclusivityTLS(void * _Nullable newValue) {
  _swift_exclusivity_single_threaded = newValue;
}
}
