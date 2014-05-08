// RUN: %target-swift-frontend -verify -parse %s

import Dispatch

// dispatch/io.h
dispatch_io_create(DISPATCH_IO_STREAM, 0, nil, nil)
dispatch_io_close(nil, DISPATCH_IO_STOP)
dispatch_io_set_interval(nil, 0, DISPATCH_IO_STRICT_INTERVAL)

// dispatch/queue.h
dispatch_queue_create("", DISPATCH_QUEUE_SERIAL)
// FIXME: dispatch_queue_create("", DISPATCH_QUEUE_CONCURRENT)
dispatch_set_target_queue(nil, DISPATCH_TARGET_QUEUE_DEFAULT)
dispatch_queue_get_label(DISPATCH_CURRENT_QUEUE_LABEL)
dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0)
dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0)

// dispatch/source.h
// FIXME: DISPATCH_SOURCE_TYPE_*
dispatch_source_merge_data(nil, DISPATCH_MACH_SEND_DEAD)
dispatch_source_merge_data(nil, DISPATCH_MEMORYPRESSURE_NORMAL)
dispatch_source_merge_data(nil, DISPATCH_PROC_EXIT)
dispatch_source_merge_data(nil, DISPATCH_PROC_SIGNAL)
dispatch_source_merge_data(nil, DISPATCH_VNODE_DELETE)
dispatch_source_merge_data(nil, DISPATCH_TIMER_STRICT)

// dispatch/time.h
dispatch_time(DISPATCH_TIME_FOREVER, 0)
