//===----------------------------------------------------------------------===//
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

#include <dispatch/dispatch.h>

__attribute__((visibility("hidden")))
extern "C" dispatch_queue_attr_t 
_swift_dispatch_queue_concurrent(void) {
  return DISPATCH_QUEUE_CONCURRENT;
}

__attribute__((visibility("hidden")))
extern "C" dispatch_data_t
_swift_dispatch_data_empty(void) {
  return dispatch_data_empty;
}

#define SOURCE(t)                               \
  __attribute__((visibility("hidden")))         \
  extern "C" dispatch_source_type_t             \
  _swift_dispatch_source_type_##t(void) {       \
    return DISPATCH_SOURCE_TYPE_##t;            \
  }

SOURCE(DATA_ADD)
SOURCE(DATA_OR)
SOURCE(MACH_SEND)
SOURCE(MACH_RECV)
SOURCE(MEMORYPRESSURE)
SOURCE(PROC)
SOURCE(READ)
SOURCE(SIGNAL)
SOURCE(TIMER)
SOURCE(VNODE)
SOURCE(WRITE)
