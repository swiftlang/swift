// RUN: %target-swift-frontend -verify -parse %s

// REQUIRES: objc_interop

import Dispatch

func getAnyValue<T>(opt: T?) -> T { return opt! }

// dispatch/io.h
let _ = dispatch_io_create(DISPATCH_IO_STREAM, 0, getAnyValue(nil), getAnyValue(nil))
dispatch_io_close(getAnyValue(nil), DISPATCH_IO_STOP)
dispatch_io_set_interval(getAnyValue(nil), 0, DISPATCH_IO_STRICT_INTERVAL)

// dispatch/queue.h
let _ = dispatch_queue_create("", DISPATCH_QUEUE_SERIAL)
let _ = dispatch_queue_create("", DISPATCH_QUEUE_CONCURRENT)
dispatch_set_target_queue(getAnyValue(nil), DISPATCH_TARGET_QUEUE_DEFAULT)
let _ = dispatch_queue_get_label(DISPATCH_CURRENT_QUEUE_LABEL)
let _ = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0)
let _ = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0)

// QOS_CLASS_DEFAULT is not always available
if #available(iOS 8.0, OSX 10.10, *) {
  let _ = dispatch_get_global_queue(QOS_CLASS_DEFAULT, 0)
}

// dispatch/source.h
dispatch_source_merge_data(getAnyValue(nil), DISPATCH_MACH_SEND_DEAD)
dispatch_source_merge_data(getAnyValue(nil), DISPATCH_MEMORYPRESSURE_NORMAL)
dispatch_source_merge_data(getAnyValue(nil), DISPATCH_PROC_EXIT)
dispatch_source_merge_data(getAnyValue(nil), DISPATCH_PROC_SIGNAL)
dispatch_source_merge_data(getAnyValue(nil), DISPATCH_VNODE_DELETE)
dispatch_source_merge_data(getAnyValue(nil), DISPATCH_TIMER_STRICT)
let _ = dispatch_source_create(DISPATCH_SOURCE_TYPE_DATA_ADD, 0, 0, getAnyValue(nil))
let _ = dispatch_source_create(DISPATCH_SOURCE_TYPE_DATA_OR, 0, 0, getAnyValue(nil))
let _ = dispatch_source_create(DISPATCH_SOURCE_TYPE_MACH_SEND, 0, 0, getAnyValue(nil))
let _ = dispatch_source_create(DISPATCH_SOURCE_TYPE_MACH_RECV, 0, 0, getAnyValue(nil))
let _ = dispatch_source_create(DISPATCH_SOURCE_TYPE_MEMORYPRESSURE, 0, 0, getAnyValue(nil))
let _ = dispatch_source_create(DISPATCH_SOURCE_TYPE_PROC, 0, 0, getAnyValue(nil))
let _ = dispatch_source_create(DISPATCH_SOURCE_TYPE_READ, 0, 0, getAnyValue(nil))
let _ = dispatch_source_create(DISPATCH_SOURCE_TYPE_SIGNAL, 0, 0, getAnyValue(nil))
let _ = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0, getAnyValue(nil))
let _ = dispatch_source_create(DISPATCH_SOURCE_TYPE_VNODE, 0, 0, getAnyValue(nil))
let _ = dispatch_source_create(DISPATCH_SOURCE_TYPE_WRITE, 0, 0, getAnyValue(nil))

// dispatch/time.h
let _ = dispatch_time(DISPATCH_TIME_FOREVER, 0)
