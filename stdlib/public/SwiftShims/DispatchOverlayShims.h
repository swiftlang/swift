//===--- DispatchOverlayShims.h - Compatibility decls -----------*- C++ -*-===//
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

#ifndef SWIFT_STDLIB_SHIMS_DISPATCHSHIMS_H
#define SWIFT_STDLIB_SHIMS_DISPATCHSHIMS_H

@import Dispatch;

#define SWIFT_DISPATCH_RETURNS_RETAINED __attribute__((__ns_returns_retained__))
#define SWIFT_DISPATCH_NOESCAPE __attribute__((__noescape__))

#pragma clang assume_nonnull begin

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

typedef void (^__swift_shims_dispatch_block_t)(void);
typedef id __swift_shims_dispatch_data_t;


static inline dispatch_queue_attr_t
_swift_dispatch_queue_concurrent(void) {
  return DISPATCH_QUEUE_CONCURRENT;
}

static inline dispatch_queue_t
_swift_dispatch_get_main_queue(void) {
  return dispatch_get_main_queue();
}

static inline dispatch_data_t
_swift_dispatch_data_empty(void) {
  return dispatch_data_empty;
}

static inline __swift_shims_dispatch_block_t _Nullable
_swift_dispatch_data_destructor_default(void) {
  return DISPATCH_DATA_DESTRUCTOR_DEFAULT;
}

static inline __swift_shims_dispatch_block_t
_swift_dispatch_data_destructor_free(void) {
  return _dispatch_data_destructor_free;
}

static inline __swift_shims_dispatch_block_t
_swift_dispatch_data_destructor_munmap(void) {
  return _dispatch_data_destructor_munmap;
}

#define SWIFT_DISPATCH_SOURCE_TYPE(t)                                          \
  static inline dispatch_source_type_t _swift_dispatch_source_type_##t(void) { \
    return DISPATCH_SOURCE_TYPE_##t;                                           \
  }

SWIFT_DISPATCH_SOURCE_TYPE(DATA_ADD)
SWIFT_DISPATCH_SOURCE_TYPE(DATA_OR)
SWIFT_DISPATCH_SOURCE_TYPE(MACH_SEND)
SWIFT_DISPATCH_SOURCE_TYPE(MACH_RECV)
SWIFT_DISPATCH_SOURCE_TYPE(MEMORYPRESSURE)
SWIFT_DISPATCH_SOURCE_TYPE(PROC)
SWIFT_DISPATCH_SOURCE_TYPE(READ)
SWIFT_DISPATCH_SOURCE_TYPE(SIGNAL)
SWIFT_DISPATCH_SOURCE_TYPE(TIMER)
SWIFT_DISPATCH_SOURCE_TYPE(VNODE)
SWIFT_DISPATCH_SOURCE_TYPE(WRITE)


static inline __swift_shims_dispatch_block_t
_swift_dispatch_block_create_with_qos_class(
    dispatch_block_flags_t flags,
    qos_class_t qos,
    int relative_priority,
    __swift_shims_dispatch_block_t _Nonnull block) {
  return dispatch_block_create_with_qos_class(
      flags, qos, relative_priority, block);
}

static inline __swift_shims_dispatch_block_t
_swift_dispatch_block_create_noescape(
    dispatch_block_flags_t flags,
    __swift_shims_dispatch_block_t SWIFT_DISPATCH_NOESCAPE block) {
  return dispatch_block_create(flags, block);
}

static inline int _swift_dispatch_block_wait(
    __swift_shims_dispatch_block_t block,
    unsigned long long timeout) {
  return dispatch_block_wait(block, timeout);
}

static inline void _swift_dispatch_block_notify(
    __swift_shims_dispatch_block_t block,
    dispatch_queue_t queue,
    __swift_shims_dispatch_block_t notifier) {
  dispatch_block_notify(block, queue, notifier);
}

static inline void _swift_dispatch_block_cancel(
    __swift_shims_dispatch_block_t block) {
  dispatch_block_cancel(block);
}

static inline int _swift_dispatch_block_testcancel(
    __swift_shims_dispatch_block_t block) {
  return dispatch_block_testcancel(block);
}

static inline void _swift_dispatch_async(
    dispatch_queue_t queue,
    __swift_shims_dispatch_block_t block) {
  dispatch_async(queue, block);
}

static inline void _swift_dispatch_sync(
    dispatch_queue_t queue,
    __swift_shims_dispatch_block_t block) {
  dispatch_sync(queue, block);
}

static inline void _swift_dispatch_barrier_async(
    dispatch_queue_t queue,
    __swift_shims_dispatch_block_t block) {
  dispatch_barrier_async(queue, block);
}

static inline void _swift_dispatch_group_async(
    dispatch_group_t group,
    dispatch_queue_t queue,
    __swift_shims_dispatch_block_t block) {
  dispatch_group_async((dispatch_group_t)group, queue, block);
}

static inline void _swift_dispatch_group_notify(
    dispatch_group_t group,
    dispatch_queue_t queue,
    __swift_shims_dispatch_block_t block) {
  dispatch_group_notify(group, queue, block);
}

static inline void _swift_dispatch_after(
    dispatch_time_t when,
    dispatch_queue_t queue,
    __swift_shims_dispatch_block_t block) {
  dispatch_after(when, queue, block);
}


static inline void _swift_dispatch_apply_current(
    unsigned int iterations,
    void SWIFT_DISPATCH_NOESCAPE (^block)(long)) {
  dispatch_apply(iterations, (dispatch_queue_t _Nonnull)0, ^(size_t i){
    block((long)i);
  });
}

SWIFT_DISPATCH_RETURNS_RETAINED
__swift_shims_dispatch_data_t 
_swift_dispatch_data_create(
    const void *buffer,
    size_t size,
    dispatch_queue_t _Nullable queue,
    __swift_shims_dispatch_block_t _Nullable destructor) {
  return dispatch_data_create(buffer, size, queue, destructor);
}

typedef unsigned int (^__swift_shims_dispatch_data_applier)(__swift_shims_dispatch_data_t, size_t, const void *, size_t);

unsigned int
_swift_dispatch_data_apply(
    __swift_shims_dispatch_data_t data,
    __swift_shims_dispatch_data_applier SWIFT_DISPATCH_NOESCAPE applier) {
  return dispatch_data_apply(data, ^bool(dispatch_data_t data, size_t off, const void *loc, size_t size){
    return applier(data, off, loc, size);
  });
}

void _swift_dispatch_source_set_event_handler(
    dispatch_source_t source,
    __swift_shims_dispatch_block_t _Nullable block) {
  dispatch_source_set_event_handler(source, block);
}

void _swift_dispatch_source_set_cancel_handler(
    dispatch_source_t source,
    __swift_shims_dispatch_block_t _Nullable block) {
  dispatch_source_set_cancel_handler(source, block);
}

void _swift_dispatch_source_set_registration_handler(
    dispatch_source_t source,
    __swift_shims_dispatch_block_t _Nullable block) {
  dispatch_source_set_registration_handler(source, block);
}

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#pragma clang assume_nonnull end

#endif // SWIFT_STDLIB_SHIMS_DISPATCHSHIMS_H

