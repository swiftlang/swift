//===--- DispatchShims.mm - Dispatch helper shims -------------------------===//
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
// This file contains shims to refer to framework functions required by the
// standard library. The stdlib cannot directly import these modules without
// introducing circular dependencies.
//
//===----------------------------------------------------------------------===//

#import <dispatch/dispatch.h>
#include "../SwiftShims/DispatchShims.h"

using namespace swift;

template <class FromTy> struct DestType;

#define BRIDGE_TYPE(FROM, TO) \
template <> struct DestType<FROM> { using type = TO; }

BRIDGE_TYPE(__swift_shims_dispatch_block_flags_t, dispatch_block_flags_t);
BRIDGE_TYPE(__swift_shims_qos_class_t, qos_class_t);
BRIDGE_TYPE(__swift_shims_dispatch_block_t, dispatch_block_t);
BRIDGE_TYPE(__swift_shims_dispatch_queue_t, dispatch_queue_t);

template <class FromTy>
static typename DestType<FromTy>::type cast(FromTy value) {
  return (typename DestType<FromTy>::type) value;
}

__swift_shims_dispatch_block_t
swift::_swift_dispatch_block_create_with_qos_class(
		__swift_shims_dispatch_block_flags_t flags, 
		__swift_shims_qos_class_t qos,
		int relative_priority, 
		__swift_shims_dispatch_block_t block)
{
	return dispatch_block_create_with_qos_class(
			cast(flags), cast(qos), relative_priority, cast(block));
}

__swift_shims_dispatch_block_t
swift::_swift_dispatch_block_create_noescape(
		__swift_shims_dispatch_block_flags_t flags,
		__swift_shims_dispatch_block_t block)
{
	return dispatch_block_create(cast(flags), cast(block));
}

int
swift::_swift_dispatch_block_wait(
		__swift_shims_dispatch_block_t block,
		unsigned long long timeout)
{
	return dispatch_block_wait(block, timeout);
}

void
swift::_swift_dispatch_block_notify(
		__swift_shims_dispatch_block_t block,
		__swift_shims_dispatch_queue_t queue,
		__swift_shims_dispatch_block_t notifier)
{
	dispatch_block_notify(cast(block), cast(queue), cast(notifier));
}

void 
swift::_swift_dispatch_block_cancel(
		__swift_shims_dispatch_block_t block)
{
	dispatch_block_cancel(cast(block));
}

int
swift::_swift_dispatch_block_testcancel(
		__swift_shims_dispatch_block_t block)
{
	return dispatch_block_testcancel(cast(block));
}

void
swift::_swift_dispatch_async(
	__swift_shims_dispatch_queue_t queue,
	__swift_shims_dispatch_block_t block)
{
	dispatch_async(cast(queue), cast(block));
}

void
swift::_swift_dispatch_barrier_async(
	      __swift_shims_dispatch_queue_t queue,
	      __swift_shims_dispatch_block_t block)
{
	dispatch_barrier_async(cast(queue), cast(block));
}

void
swift::_swift_dispatch_group_async(
	__swift_shims_dispatch_group_t group,
	__swift_shims_dispatch_queue_t queue,
	__swift_shims_dispatch_block_t block)
{
	dispatch_group_async((dispatch_group_t)group, cast(queue), cast(block));
}

void
swift::_swift_dispatch_group_notify(
	__swift_shims_dispatch_group_t group,
	__swift_shims_dispatch_queue_t queue,
	__swift_shims_dispatch_block_t block)
{
	dispatch_group_notify((dispatch_group_t)group, cast(queue), cast(block));
}

void
swift::_swift_dispatch_sync(
	__swift_shims_dispatch_queue_t queue,
	__swift_shims_dispatch_block_t block)
{
	dispatch_sync(cast(queue), cast(block));
}

void
swift::_swift_dispatch_after(
	__swift_shims_dispatch_time_t when,
	__swift_shims_dispatch_queue_t queue,
	__swift_shims_dispatch_block_t block)
{
	dispatch_after((dispatch_time_t)when, cast(queue), cast(block));
}

void
swift::_swift_dispatch_apply_current(
		unsigned int iterations,
		void SWIFT_DISPATCH_NOESCAPE (^block)(long))
{
	dispatch_apply(iterations, (dispatch_queue_t _Nonnull)0, ^(size_t i){
		block((long)i);
	});
}

__swift_shims_dispatch_data_t
swift::_swift_dispatch_data_create(
		const void *buffer,
		__swift_size_t size,
		__swift_shims_dispatch_queue_t queue,
		__swift_shims_dispatch_block_t destructor)
{
	return dispatch_data_create(buffer, size, cast(queue), cast(destructor));
}

unsigned int
swift::_swift_dispatch_data_apply(
		__swift_shims_dispatch_data_t data, 
		__swift_shims_dispatch_data_applier SWIFT_DISPATCH_NOESCAPE applier)
{
	return dispatch_data_apply(data, ^bool(dispatch_data_t data, size_t off, const void *loc, size_t size){
		return applier(data, off, loc, size);
	});
}

void
swift::_swift_dispatch_source_set_event_handler(
	__swift_shims_dispatch_source_t source,
	__swift_shims_dispatch_block_t block)
{
	dispatch_source_set_event_handler((dispatch_source_t)source, cast(block));
}

void
swift::_swift_dispatch_source_set_cancel_handler(
	__swift_shims_dispatch_source_t source,
	__swift_shims_dispatch_block_t block)
{
	dispatch_source_set_cancel_handler((dispatch_source_t)source, cast(block));
}

void
swift::_swift_dispatch_source_set_registration_handler(
	__swift_shims_dispatch_source_t source,
	__swift_shims_dispatch_block_t block)
{
	dispatch_source_set_registration_handler((dispatch_source_t)source, cast(block));
}
