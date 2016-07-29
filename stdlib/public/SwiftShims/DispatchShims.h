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

#define SWIFT_DISPATCH_RETURNS_RETAINED_BLOCK __attribute__((__ns_returns_retained__))
#define SWIFT_DISPATCH_NOESCAPE __attribute__((__noescape__))

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

typedef unsigned long __swift_shims_dispatch_block_flags_t;
typedef unsigned int __swift_shims_qos_class_t;
typedef void (^__swift_shims_dispatch_block_t)(void);
typedef id __swift_shims_dispatch_queue_t;
typedef id __swift_shims_dispatch_group_t;
typedef id __swift_shims_dispatch_data_t;

SWIFT_RUNTIME_STDLIB_INTERFACE
SWIFT_DISPATCH_RETURNS_RETAINED_BLOCK 
__swift_shims_dispatch_block_t
_swift_dispatch_block_create_with_qos_class(
		__swift_shims_dispatch_block_flags_t flags, 
		__swift_shims_qos_class_t qos,
		int relative_priority, 
		__swift_shims_dispatch_block_t block);

SWIFT_RUNTIME_STDLIB_INTERFACE
SWIFT_DISPATCH_RETURNS_RETAINED_BLOCK
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
void _swift_dispatch_group_async(
		__swift_shims_dispatch_group_t group,
		__swift_shims_dispatch_queue_t queue,
		__swift_shims_dispatch_block_t block);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_dispatch_apply_current(
		unsigned int iterations,
		void SWIFT_DISPATCH_NOESCAPE (^block)(long));

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_shims_dispatch_data_t 
_swift_dispatch_data_create(
		const void *buffer,
		__swift_size_t size,
		__swift_shims_dispatch_queue_t queue,
		__swift_shims_dispatch_block_t destructor);

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif // __OBJC2__

#endif // SWIFT_STDLIB_SHIMS_DISPATCHSHIMS_H

