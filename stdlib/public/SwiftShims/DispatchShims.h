//===--- DispatchShims.h - Compatibility decls. for Dispatch ----*- C++ -*-===//
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
//
//  Using the Darwin (or Glibc) module in the core stdlib would create a
//  circular dependency, so instead we import these declarations as part of
//  SwiftShims.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_DISPATCHSHIMS_H
#define SWIFT_STDLIB_SHIMS_DISPATCHSHIMS_H

#ifdef __OBJC2__

#include "SwiftStdint.h"
#include "SwiftStddef.h"
#include "Visibility.h"

#define SWIFT_DISPATCH_RETURNS_RETAINED __attribute__((__ns_returns_retained__))
#define SWIFT_DISPATCH_NOESCAPE __attribute__((__noescape__))
#define SWIFT_DISPATCH_NONNULL _Nonnull
#define SWIFT_DISPATCH_NULLABLE _Nullable
#define SWIFT_DISPATCH_ASSUME_NONNULL_BEGIN _Pragma("clang assume_nonnull begin")
#define SWIFT_DISPATCH_ASSUME_NONNULL_END _Pragma("clang assume_nonnull end")

SWIFT_DISPATCH_ASSUME_NONNULL_BEGIN

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

typedef unsigned long __swift_shims_dispatch_block_flags_t;
typedef unsigned int __swift_shims_qos_class_t;
typedef __swift_uint64_t __swift_shims_dispatch_time_t;
typedef void (^__swift_shims_dispatch_block_t)(void);
typedef id __swift_shims_dispatch_queue_t;
typedef id __swift_shims_dispatch_group_t;
typedef id __swift_shims_dispatch_data_t;
typedef id __swift_shims_dispatch_source_t;

SWIFT_RUNTIME_STDLIB_INTERFACE
SWIFT_DISPATCH_RETURNS_RETAINED
__swift_shims_dispatch_block_t
_swift_dispatch_block_create_with_qos_class(
		__swift_shims_dispatch_block_flags_t flags, 
		__swift_shims_qos_class_t qos,
		int relative_priority, 
		__swift_shims_dispatch_block_t SWIFT_DISPATCH_NONNULL block);

SWIFT_RUNTIME_STDLIB_INTERFACE
SWIFT_DISPATCH_RETURNS_RETAINED
__swift_shims_dispatch_block_t
_swift_dispatch_block_create_noescape(
		__swift_shims_dispatch_block_flags_t flags,
		__swift_shims_dispatch_block_t SWIFT_DISPATCH_NOESCAPE block);

SWIFT_RUNTIME_STDLIB_INTERFACE
int _swift_dispatch_block_wait(
		__swift_shims_dispatch_block_t block,
		unsigned long long timeout);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_block_notify(
		__swift_shims_dispatch_block_t block,
		__swift_shims_dispatch_queue_t queue,
		__swift_shims_dispatch_block_t notifier);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_block_cancel(
		__swift_shims_dispatch_block_t block);

SWIFT_RUNTIME_STDLIB_INTERFACE
int _swift_dispatch_block_testcancel(
		__swift_shims_dispatch_block_t block);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_async(
		__swift_shims_dispatch_queue_t queue,
		__swift_shims_dispatch_block_t block);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_sync(
		__swift_shims_dispatch_queue_t queue,
		__swift_shims_dispatch_block_t block);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_barrier_async(
		__swift_shims_dispatch_queue_t queue,
		__swift_shims_dispatch_block_t block);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_group_async(
		__swift_shims_dispatch_group_t group,
		__swift_shims_dispatch_queue_t queue,
		__swift_shims_dispatch_block_t block);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_group_notify(
		__swift_shims_dispatch_group_t group,
		__swift_shims_dispatch_queue_t queue,
		__swift_shims_dispatch_block_t block);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_after(
		__swift_shims_dispatch_time_t when,
		__swift_shims_dispatch_queue_t queue,
		__swift_shims_dispatch_block_t block);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_apply_current(
		unsigned int iterations,
		void SWIFT_DISPATCH_NOESCAPE (^block)(long));

SWIFT_RUNTIME_STDLIB_INTERFACE
SWIFT_DISPATCH_RETURNS_RETAINED
__swift_shims_dispatch_data_t 
_swift_dispatch_data_create(
		const void *buffer,
		__swift_size_t size,
		__swift_shims_dispatch_queue_t SWIFT_DISPATCH_NULLABLE queue,
		__swift_shims_dispatch_block_t SWIFT_DISPATCH_NULLABLE destructor);

typedef unsigned int (^__swift_shims_dispatch_data_applier)(__swift_shims_dispatch_data_t, __swift_size_t, const void *, __swift_size_t);

SWIFT_RUNTIME_STDLIB_INTERFACE
unsigned int
_swift_dispatch_data_apply(
		__swift_shims_dispatch_data_t data, 
		__swift_shims_dispatch_data_applier SWIFT_DISPATCH_NOESCAPE applier);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_source_set_event_handler(
		__swift_shims_dispatch_source_t source,
		__swift_shims_dispatch_block_t SWIFT_DISPATCH_NULLABLE block);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_source_set_cancel_handler(
		__swift_shims_dispatch_source_t source,
		__swift_shims_dispatch_block_t SWIFT_DISPATCH_NULLABLE block);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_source_set_registration_handler(
		__swift_shims_dispatch_source_t source,
		__swift_shims_dispatch_block_t SWIFT_DISPATCH_NULLABLE block);

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

SWIFT_DISPATCH_ASSUME_NONNULL_END

#endif // __OBJC2__

#endif // SWIFT_STDLIB_SHIMS_DISPATCHSHIMS_H

