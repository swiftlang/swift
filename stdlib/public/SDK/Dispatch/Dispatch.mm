//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <dispatch/dispatch.h>
#include <objc/runtime.h>
#include <stdio.h>

#define DISPATCH_RUNTIME_STDLIB_INTERFACE __attribute__((__visibility__("default")))

@protocol OS_dispatch_source;
@protocol OS_dispatch_source_mach_send;
@protocol OS_dispatch_source_mach_recv;
@protocol OS_dispatch_source_memorypressure;
@protocol OS_dispatch_source_proc;
@protocol OS_dispatch_source_read;
@protocol OS_dispatch_source_signal;
@protocol OS_dispatch_source_timer;
@protocol OS_dispatch_source_data_add;
@protocol OS_dispatch_source_data_or;
@protocol OS_dispatch_source_vnode;
@protocol OS_dispatch_source_write;

// #include <dispatch/private.h>
__attribute__((constructor))
static void _dispatch_overlay_constructor() {
  Class source = objc_lookUpClass("OS_dispatch_source");
  if (source) {
    class_addProtocol(source, @protocol(OS_dispatch_source));
    class_addProtocol(source, @protocol(OS_dispatch_source_mach_send));
    class_addProtocol(source, @protocol(OS_dispatch_source_mach_recv));
    class_addProtocol(source, @protocol(OS_dispatch_source_memorypressure));
    class_addProtocol(source, @protocol(OS_dispatch_source_proc));
    class_addProtocol(source, @protocol(OS_dispatch_source_read));
    class_addProtocol(source, @protocol(OS_dispatch_source_signal));
    class_addProtocol(source, @protocol(OS_dispatch_source_timer));
    class_addProtocol(source, @protocol(OS_dispatch_source_data_add));
    class_addProtocol(source, @protocol(OS_dispatch_source_data_or));
    class_addProtocol(source, @protocol(OS_dispatch_source_vnode));
    class_addProtocol(source, @protocol(OS_dispatch_source_write));
  }
}

#include "swift/Runtime/Config.h"

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE 
extern "C" dispatch_queue_attr_t
_swift_dispatch_queue_concurrent(void) {
  return DISPATCH_QUEUE_CONCURRENT;
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" dispatch_queue_t
_swift_dispatch_get_main_queue(void) {
  return dispatch_get_main_queue();
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" dispatch_data_t
_swift_dispatch_data_empty(void) {
  return dispatch_data_empty;
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" dispatch_block_t
_swift_dispatch_data_destructor_default(void) {
  return DISPATCH_DATA_DESTRUCTOR_DEFAULT;
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" dispatch_block_t
_swift_dispatch_data_destructor_free(void) {
  return _dispatch_data_destructor_free;
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" dispatch_block_t
_swift_dispatch_data_destructor_munmap(void) {
  return _dispatch_data_destructor_munmap;
}

#define SOURCE(t)                                                              \
  SWIFT_CC(swift)                                                              \
  DISPATCH_RUNTIME_STDLIB_INTERFACE extern "C" dispatch_source_type_t  \
  _swift_dispatch_source_type_##t(void) {                                      \
    return DISPATCH_SOURCE_TYPE_##t;                                           \
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
