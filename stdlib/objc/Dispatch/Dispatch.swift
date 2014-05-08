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

@exported import Dispatch

//===----------------------------------------------------------------------===//
// Macros
// FIXME: rdar://16851050 update API so these import better
//===----------------------------------------------------------------------===//

// dispatch/io.h
let DISPATCH_IO_STREAM = dispatch_io_type_t(0)
let DISPATCH_IO_RANDOM = dispatch_io_type_t(1)
let DISPATCH_IO_STOP = dispatch_io_close_flags_t(1)
let DISPATCH_IO_STRICT_INTERVAL = dispatch_io_interval_flags_t(1)

// dispatch/queue.h
// DISPATCH_QUEUE_SERIAL: ok
// DISPATCH_CURRENT_QUEUE_LABEL: ok
// DISPATCH_TARGET_QUEUE_DEFAULT: ok
let DISPATCH_QUEUE_PRIORITY_HIGH       = dispatch_queue_priority_t(2)
let DISPATCH_QUEUE_PRIORITY_DEFAULT    = dispatch_queue_priority_t(0)
let DISPATCH_QUEUE_PRIORITY_LOW        = dispatch_queue_priority_t(-2)
let DISPATCH_QUEUE_PRIORITY_BACKGROUND = dispatch_queue_priority_t(-32768)
// FIXME: DISPATCH_QUEUE_CONCURRENT

// dispatch/source.h
// FIXME: DISPATCH_SOURCE_TYPE_*
let DISPATCH_MACH_SEND_DEAD = dispatch_source_mach_send_flags_t(1)
let DISPATCH_MEMORYPRESSURE_NORMAL   = dispatch_source_memorypressure_flags_t(1)
let DISPATCH_MEMORYPRESSURE_WARN     = dispatch_source_memorypressure_flags_t(2)
let DISPATCH_MEMORYPRESSURE_CRITICAL = dispatch_source_memorypressure_flags_t(4)
let DISPATCH_PROC_EXIT: dispatch_source_proc_flags_t = 0x80000000
let DISPATCH_PROC_FORK   = dispatch_source_proc_flags_t(0x40000000)
let DISPATCH_PROC_EXEC   = dispatch_source_proc_flags_t(0x20000000)
let DISPATCH_PROC_SIGNAL = dispatch_source_proc_flags_t(0x08000000)
let DISPATCH_VNODE_DELETE = dispatch_source_vnode_flags_t(0x1)
let DISPATCH_VNODE_WRITE  = dispatch_source_vnode_flags_t(0x2)
let DISPATCH_VNODE_EXTEND = dispatch_source_vnode_flags_t(0x4)
let DISPATCH_VNODE_ATTRIB = dispatch_source_vnode_flags_t(0x8)
let DISPATCH_VNODE_LINK   = dispatch_source_vnode_flags_t(0x10)
let DISPATCH_VNODE_RENAME = dispatch_source_vnode_flags_t(0x20)
let DISPATCH_VNODE_REVOKE = dispatch_source_vnode_flags_t(0x40)
let DISPATCH_TIMER_STRICT = dispatch_source_timer_flags_t(1)

// dispatch/time.h
// DISPATCH_TIME_NOW: ok
// DISPATCH_TIME_FOREVER: ok
