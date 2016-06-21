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
extern "C" void
_swift_dispatch_apply_current(size_t iterations, __attribute__((__noescape__)) void (^block)(size_t)) {
  dispatch_apply(iterations, (dispatch_queue_t _Nonnull)0, block);
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

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" dispatch_block_t
_swift_dispatch_block_create_with_qos_class(dispatch_block_flags_t flags, qos_class_t qos, int relative_priority, dispatch_block_t block) {
  return dispatch_block_create_with_qos_class(flags, qos, relative_priority, block);
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" dispatch_block_t
_swift_dispatch_block_create_noescape(dispatch_block_flags_t flags, dispatch_block_t block) {
  return dispatch_block_create(flags, block);
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" void
_swift_dispatch_block_cancel(dispatch_block_t block) {
  dispatch_block_cancel(block);
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" long
_swift_dispatch_block_wait(dispatch_block_t block, dispatch_time_t timeout) {
  return dispatch_block_wait(block, timeout);
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" void
_swift_dispatch_block_notify(dispatch_block_t block, dispatch_queue_t queue, dispatch_block_t notification_block) {
  dispatch_block_notify(block, queue, notification_block);
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" long
_swift_dispatch_block_testcancel(dispatch_block_t block) {
  return dispatch_block_testcancel(block);
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" bool
_swift_dispatch_data_apply(dispatch_data_t data, bool (^applier)(dispatch_data_t, size_t, const void *, size_t)) {
  return dispatch_data_apply(data, applier);
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" void
_swift_dispatch_async(dispatch_queue_t queue, dispatch_block_t block) {
  dispatch_async(queue, block);
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" void
_swift_dispatch_group_async(dispatch_group_t group, dispatch_queue_t queue, dispatch_block_t block) {
  dispatch_group_async(group, queue, block);
}

SWIFT_CC(swift) DISPATCH_RUNTIME_STDLIB_INTERFACE
extern "C" void
_swift_dispatch_sync(dispatch_queue_t queue, dispatch_block_t block) {
  dispatch_sync(queue, block);
}

// DISPATCH_RUNTIME_STDLIB_INTERFACE
// extern "C" dispatch_queue_t
// _swift_apply_current_root_queue() {
//   return DISPATCH_APPLY_CURRENT_ROOT_QUEUE;
// }

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
