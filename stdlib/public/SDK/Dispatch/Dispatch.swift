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


//===----------------------------------------------------------------------===//
// Macros
// FIXME: rdar://16851050 update API so these import better
//===----------------------------------------------------------------------===//

// dispatch/io.h
public let DISPATCH_IO_STREAM = dispatch_io_type_t(0)
public let DISPATCH_IO_RANDOM = dispatch_io_type_t(1)
public let DISPATCH_IO_STOP = dispatch_io_close_flags_t(1)
public let DISPATCH_IO_STRICT_INTERVAL = dispatch_io_interval_flags_t(1)

// dispatch/queue.h
public let DISPATCH_QUEUE_SERIAL : dispatch_queue_attr_t! = nil
public let DISPATCH_CURRENT_QUEUE_LABEL : dispatch_queue_t! = nil
public let DISPATCH_TARGET_QUEUE_DEFAULT : dispatch_queue_t! = nil
public let DISPATCH_QUEUE_PRIORITY_HIGH       = dispatch_queue_priority_t(2)
public let DISPATCH_QUEUE_PRIORITY_DEFAULT    = dispatch_queue_priority_t(0)
public let DISPATCH_QUEUE_PRIORITY_LOW        = dispatch_queue_priority_t(-2)
public let DISPATCH_QUEUE_PRIORITY_BACKGROUND = dispatch_queue_priority_t(-32768)

public func dispatch_get_global_queue(identifier: qos_class_t,
                                      _ flags: UInt) -> dispatch_queue_t {
  return dispatch_get_global_queue(Int(identifier.rawValue), flags);
}

public var DISPATCH_QUEUE_CONCURRENT : dispatch_queue_attr_t {
  get { return _swift_dispatch_queue_concurrent() } 
}

@asmname("_swift_dispatch_queue_concurrent") 
func _swift_dispatch_queue_concurrent() -> dispatch_queue_attr_t

// dispatch/data.h
public var dispatch_data_empty : dispatch_data_t {
get { return _swift_dispatch_data_empty() }
}

@asmname("_swift_dispatch_data_empty")
func _swift_dispatch_data_empty() -> dispatch_data_t

// dispatch/source.h
// FIXME: DISPATCH_SOURCE_TYPE_*
public let DISPATCH_MACH_SEND_DEAD = dispatch_source_mach_send_flags_t(1)
public let DISPATCH_MEMORYPRESSURE_NORMAL   = dispatch_source_memorypressure_flags_t(1)
public let DISPATCH_MEMORYPRESSURE_WARN     = dispatch_source_memorypressure_flags_t(2)
public let DISPATCH_MEMORYPRESSURE_CRITICAL = dispatch_source_memorypressure_flags_t(4)
public let DISPATCH_PROC_EXIT: dispatch_source_proc_flags_t = 0x80000000
public let DISPATCH_PROC_FORK   = dispatch_source_proc_flags_t(0x40000000)
public let DISPATCH_PROC_EXEC   = dispatch_source_proc_flags_t(0x20000000)
public let DISPATCH_PROC_SIGNAL = dispatch_source_proc_flags_t(0x08000000)
public let DISPATCH_VNODE_DELETE = dispatch_source_vnode_flags_t(0x1)
public let DISPATCH_VNODE_WRITE  = dispatch_source_vnode_flags_t(0x2)
public let DISPATCH_VNODE_EXTEND = dispatch_source_vnode_flags_t(0x4)
public let DISPATCH_VNODE_ATTRIB = dispatch_source_vnode_flags_t(0x8)
public let DISPATCH_VNODE_LINK   = dispatch_source_vnode_flags_t(0x10)
public let DISPATCH_VNODE_RENAME = dispatch_source_vnode_flags_t(0x20)
public let DISPATCH_VNODE_REVOKE = dispatch_source_vnode_flags_t(0x40)
public let DISPATCH_TIMER_STRICT = dispatch_source_timer_flags_t(1)

public var DISPATCH_SOURCE_TYPE_DATA_ADD : dispatch_source_type_t { 
  get { return _swift_dispatch_source_type_data_add() } 
}
public var DISPATCH_SOURCE_TYPE_DATA_OR : dispatch_source_type_t { 
  get { return _swift_dispatch_source_type_data_or() } 
}
public var DISPATCH_SOURCE_TYPE_MACH_SEND : dispatch_source_type_t { 
  get { return _swift_dispatch_source_type_mach_send() } 
}
public var DISPATCH_SOURCE_TYPE_MACH_RECV : dispatch_source_type_t { 
  get { return _swift_dispatch_source_type_mach_recv() } 
}
public var DISPATCH_SOURCE_TYPE_MEMORYPRESSURE : dispatch_source_type_t { 
  get { return _swift_dispatch_source_type_memorypressure() } 
}
public var DISPATCH_SOURCE_TYPE_READ : dispatch_source_type_t { 
  get { return _swift_dispatch_source_type_read() } 
}
public var DISPATCH_SOURCE_TYPE_PROC : dispatch_source_type_t { 
  get { return _swift_dispatch_source_type_proc() } 
}
public var DISPATCH_SOURCE_TYPE_SIGNAL : dispatch_source_type_t { 
  get { return _swift_dispatch_source_type_signal() } 
}
public var DISPATCH_SOURCE_TYPE_TIMER : dispatch_source_type_t { 
  get { return _swift_dispatch_source_type_timer() } 
}
public var DISPATCH_SOURCE_TYPE_VNODE : dispatch_source_type_t { 
  get { return _swift_dispatch_source_type_vnode() } 
}
public var DISPATCH_SOURCE_TYPE_WRITE : dispatch_source_type_t { 
  get { return _swift_dispatch_source_type_write() } 
}

@asmname("_swift_dispatch_source_type_DATA_ADD") 
func _swift_dispatch_source_type_data_add() -> dispatch_source_type_t
@asmname("_swift_dispatch_source_type_DATA_OR") 
func _swift_dispatch_source_type_data_or() -> dispatch_source_type_t
@asmname("_swift_dispatch_source_type_MACH_SEND") 
func _swift_dispatch_source_type_mach_send() -> dispatch_source_type_t
@asmname("_swift_dispatch_source_type_MACH_RECV") 
func _swift_dispatch_source_type_mach_recv() -> dispatch_source_type_t
@asmname("_swift_dispatch_source_type_MEMORYPRESSURE") 
func _swift_dispatch_source_type_memorypressure() -> dispatch_source_type_t
@asmname("_swift_dispatch_source_type_PROC") 
func _swift_dispatch_source_type_proc() -> dispatch_source_type_t
@asmname("_swift_dispatch_source_type_READ") 
func _swift_dispatch_source_type_read() -> dispatch_source_type_t
@asmname("_swift_dispatch_source_type_SIGNAL") 
func _swift_dispatch_source_type_signal() -> dispatch_source_type_t
@asmname("_swift_dispatch_source_type_TIMER") 
func _swift_dispatch_source_type_timer() -> dispatch_source_type_t
@asmname("_swift_dispatch_source_type_VNODE") 
func _swift_dispatch_source_type_vnode() -> dispatch_source_type_t
@asmname("_swift_dispatch_source_type_WRITE") 
func _swift_dispatch_source_type_write() -> dispatch_source_type_t

// dispatch/time.h
// DISPATCH_TIME_NOW: ok
// DISPATCH_TIME_FOREVER: ok
