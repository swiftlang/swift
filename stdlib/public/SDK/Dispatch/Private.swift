//===----------------------------------------------------------------------===//
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

// Redeclarations of all SwiftPrivate functions with appropriate markup.

@available(*, unavailable, renamed:"DispatchQueue.init(label:qos:attributes:autoreleaseFrequency:target:)")
public func dispatch_queue_create(_ label: UnsafePointer<Int8>?, _ attr: __OS_dispatch_queue_attr?) -> DispatchQueue
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.init(label:qos:attributes:autoreleaseFrequency:target:)")
public func dispatch_queue_create_with_target(_ label: UnsafePointer<Int8>?, _ attr: __OS_dispatch_queue_attr?, _ queue: DispatchQueue?) -> DispatchQueue
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchIO.init(type:fileDescriptor:queue:cleanupHandler:)")
public func dispatch_io_create(_ type: UInt, _ fd: Int32, _ queue: DispatchQueue, _ cleanup_handler: (Int32) -> Void) -> DispatchIO
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchIO.init(type:path:oflag:mode:queue:cleanupHandler:)")
public func dispatch_io_create_with_path(_ type: UInt, _ path: UnsafePointer<Int8>, _ oflag: Int32, _ mode: mode_t, _ queue: DispatchQueue, _ cleanup_handler: (Int32) -> Void) -> DispatchIO
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchIO.init(type:io:queue:cleanupHandler:)")
public func dispatch_io_create_with_io(_ type: UInt, _ io: DispatchIO, _ queue: DispatchQueue, _ cleanup_handler: (Int32) -> Void) -> DispatchIO
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchIO.read(fileDescriptor:length:queue:handler:)")
public func dispatch_read(_ fd: Int32, _ length: Int, _ queue: DispatchQueue, _ handler: (__DispatchData, Int32) -> Void) 
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchIO.read(self:offset:length:queue:ioHandler:)")
func dispatch_io_read(_ channel: DispatchIO, _ offset: off_t, _ length: Int, _ queue: DispatchQueue, _ io_handler: (Bool, __DispatchData?, Int32) -> Void) 
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchIO.write(self:offset:data:queue:ioHandler:)")
func dispatch_io_write(_ channel: DispatchIO, _ offset: off_t, _ data: __DispatchData, _ queue: DispatchQueue, _ io_handler: (Bool, __DispatchData?, Int32) -> Void)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchIO.write(fileDescriptor:data:queue:handler:)")
func dispatch_write(_ fd: Int32, _ data: __DispatchData, _ queue: DispatchQueue, _ handler: (__DispatchData?, Int32) -> Void)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchData.init(bytes:)")
public func dispatch_data_create(_ buffer: UnsafeRawPointer, _ size: Int, _ queue: DispatchQueue?, _ destructor: (() -> Void)?) -> __DispatchData
{
	fatalError()
}

@available(*, unavailable, renamed:"getter:DispatchData.count(self:)")
public func dispatch_data_get_size(_ data: __DispatchData) -> Int
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchData.withUnsafeBytes(self:body:)")
public func dispatch_data_create_map(_ data: __DispatchData, _ buffer_ptr: UnsafeMutablePointer<UnsafeRawPointer?>?, _ size_ptr: UnsafeMutablePointer<Int>?) -> __DispatchData
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchData.append(self:_:)")
public func dispatch_data_create_concat(_ data1: __DispatchData, _ data2: __DispatchData) -> __DispatchData
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchData.subdata(self:in:)")
public func dispatch_data_create_subrange(_ data: __DispatchData, _ offset: Int, _ length: Int) -> __DispatchData
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchData.enumerateBytes(self:block:)")
public func dispatch_data_apply(_ data: __DispatchData, _ applier: (__DispatchData, Int, UnsafeRawPointer, Int) -> Bool) -> Bool
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchData.region(self:location:)")
public func dispatch_data_copy_region(_ data: __DispatchData, _ location: Int, _ offset_ptr: UnsafeMutablePointer<Int>) -> __DispatchData
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.async(self:group:qos:flags:execute:)")
public func dispatch_group_async(_ group: DispatchGroup, _ queue: DispatchQueue, _ block: () -> Void)
{
	fatalError()
}

@available(*, unavailable, renamed: "DispatchGroup.notify(self:qos:flags:queue:execute:)")
public func dispatch_group_notify(_ group: DispatchGroup, _ queue: DispatchQueue, _ block: () -> Void) 
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchGroup.wait(self:timeout:)")
public func dispatch_group_wait(_ group: DispatchGroup, _ timeout: dispatch_time_t) -> Int
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchIO.close(self:flags:)")
public func dispatch_io_close(_ channel: DispatchIO, _ flags: UInt)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchIO.setInterval(self:interval:flags:)")
public func dispatch_io_set_interval(_ channel: DispatchIO, _ interval: UInt64, _ flags: UInt)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.apply(attributes:iterations:execute:)")
public func dispatch_apply(_ iterations: Int, _ queue: DispatchQueue, _ block: (Int) -> Void) 
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.async(self:execute:)")
public func dispatch_async(_ queue: DispatchQueue, _ block: () -> Void)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.global(attributes:)")
public func dispatch_get_global_queue(_ identifier: Int, _ flags: UInt) -> DispatchQueue
{
	fatalError()
}

@available(*, unavailable, renamed: "getter:DispatchQueue.main()")
public func dispatch_get_main_queue() -> DispatchQueue 
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.Attributes.initiallyInactive")
public func dispatch_queue_attr_make_initially_inactive(_ attr: __OS_dispatch_queue_attr?) -> __OS_dispatch_queue_attr
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.AutoreleaseFrequency.workItem")
public func dispatch_queue_attr_make_with_autorelease_frequency(_ attr: __OS_dispatch_queue_attr?, _ frequency: __dispatch_autorelease_frequency_t) -> __OS_dispatch_queue_attr
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQoS")
public func dispatch_queue_attr_make_with_qos_class(_ attr: __OS_dispatch_queue_attr?, _ qos_class: qos_class_t, _ relative_priority: Int32) -> __OS_dispatch_queue_attr
{
	fatalError()
}

@available(*, unavailable, renamed:"getter:DispatchQueue.label(self:)")
public func dispatch_queue_get_label(_ queue: DispatchQueue?) -> UnsafePointer<Int8>
{
	fatalError()
}

@available(*, unavailable, renamed:"getter:DispatchQueue.qos(self:)")
public func dispatch_queue_get_qos_class(_ queue: DispatchQueue, _ relative_priority_ptr: UnsafeMutablePointer<Int32>?) -> qos_class_t
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.asyncAfter(self:deadline:qos:flags:execute:)")
public func dispatch_after(_ when: dispatch_time_t, _ queue: DispatchQueue, _ block: () -> Void)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.async(self:group:qos:flags:execute:)")
public func dispatch_barrier_async(_ queue: DispatchQueue, _ block: () -> Void)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.sync(self:flags:execute:)")
public func dispatch_barrier_sync(_ queue: DispatchQueue, _ block: () -> Void)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.setSpecific(self:key:value:)")
public func dispatch_queue_set_specific(_ queue: DispatchQueue, _ key: UnsafeRawPointer, _ context: UnsafeMutableRawPointer?, _ destructor: (@convention(c) (UnsafeMutableRawPointer?) -> Void)?)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.getSpecific(self:key:)")
public func dispatch_queue_get_specific(_ queue: DispatchQueue, _ key: UnsafeRawPointer) -> UnsafeMutableRawPointer?
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchQueue.getSpecific(key:)")
public func dispatch_get_specific(_ key: UnsafeRawPointer) -> UnsafeMutableRawPointer?
{
	fatalError()
}

@available(*, unavailable, renamed:"dispatchPrecondition(_:)")
public func dispatch_assert_queue(_ queue: DispatchQueue)
{
	fatalError()
}

@available(*, unavailable, renamed:"dispatchPrecondition(_:)")
public func dispatch_assert_queue_barrier(_ queue: DispatchQueue)
{
	fatalError()
}

@available(*, unavailable, renamed:"dispatchPrecondition(_:)")
public func dispatch_assert_queue_not(_ queue: DispatchQueue)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchSemaphore.wait(self:timeout:)")
public func dispatch_semaphore_wait(_ dsema: DispatchSemaphore, _ timeout: dispatch_time_t) -> Int
{
	fatalError()
}

@available(*, unavailable, renamed: "DispatchSemaphore.signal(self:)")
public func dispatch_semaphore_signal(_ dsema: DispatchSemaphore) -> Int
{
	fatalError()
}

@available(*, unavailable, message:"Use DispatchSource class methods")
public func dispatch_source_create(_ type: __dispatch_source_type_t, _ handle: UInt, _ mask: UInt, _ queue: DispatchQueue?) -> DispatchSource
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchSource.setEventHandler(self:handler:)")
public func dispatch_source_set_event_handler(_ source: DispatchSource, _ handler: (() -> Void)?)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchSource.setCancelHandler(self:handler:)")
public func dispatch_source_set_cancel_handler(_ source: DispatchSource, _ handler: (() -> Void)?)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchSource.cancel(self:)")
public func dispatch_source_cancel(_ source: DispatchSource)
{
	fatalError()
}

@available(*, unavailable, renamed:"getter:DispatchSource.isCancelled(self:)")
public func dispatch_source_testcancel(_ source: DispatchSource) -> Int
{
	fatalError()
}

@available(*, unavailable, renamed:"getter:DispatchSource.handle(self:)")
public func dispatch_source_get_handle(_ source: DispatchSource) -> UInt
{
	fatalError()
}

@available(*, unavailable, renamed:"getter:DispatchSource.mask(self:)")
public func dispatch_source_get_mask(_ source: DispatchSource) -> UInt
{
	fatalError()
}

@available(*, unavailable, renamed:"getter:DispatchSource.data(self:)")
public func dispatch_source_get_data(_ source: DispatchSource) -> UInt
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchUserDataAdd.mergeData(self:value:)")
public func dispatch_source_merge_data(_ source: DispatchSource, _ value: UInt)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchTimerSource.setTimer(self:start:interval:leeway:)")
public func dispatch_source_set_timer(_ source: DispatchSource, _ start: dispatch_time_t, _ interval: UInt64, _ leeway: UInt64)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchSource.setRegistrationHandler(self:handler:)")
public func dispatch_source_set_registration_handler(_ source: DispatchSource, _ handler: (() -> Void)?)
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchTime.now()")
public func dispatch_time(_ when: dispatch_time_t, _ delta: Int64) -> dispatch_time_t
{
	fatalError()
}

@available(*, unavailable, renamed:"DispatchWalltime.init(time:)")
public func dispatch_walltime(_ when: UnsafePointer<timespec>?, _ delta: Int64) -> dispatch_time_t
{
	fatalError()
}

@available(*, unavailable, renamed: "DispatchQueue.GlobalQueuePriority.high")
public var DISPATCH_QUEUE_PRIORITY_HIGH: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchQueue.GlobalQueuePriority.default")
public var DISPATCH_QUEUE_PRIORITY_DEFAULT: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchQueue.GlobalQueuePriority.low")
public var DISPATCH_QUEUE_PRIORITY_LOW: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchQueue.GlobalQueuePriority.background")
public var DISPATCH_QUEUE_PRIORITY_BACKGROUND: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchIO.StreamType.stream")
public var DISPATCH_IO_STREAM: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchIO.StreamType.random")
public var DISPATCH_IO_RANDOM: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchIO.CloseFlags.stop")
public var DISPATCH_IO_STOP: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchIO.IntervalFlags.strictInterval")
public var DISPATCH_IO_STRICT_INTERVAL: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.MachSendEvent.dead")
public var DISPATCH_MACH_SEND_DEAD: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.MemoryPressureEvent.normal")
public var DISPATCH_MEMORYPRESSURE_NORMAL: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.MemoryPressureEvent.warning")
public var DISPATCH_MEMORYPRESSURE_WARN: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.MemoryPressureEvent.critical")
public var DISPATCH_MEMORYPRESSURE_CRITICAL: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.ProcessEvent.exit")
public var DISPATCH_PROC_EXIT: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.ProcessEvent.fork")
public var DISPATCH_PROC_FORK: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.ProcessEvent.exec")
public var DISPATCH_PROC_EXEC: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.ProcessEvent.signal")
public var DISPATCH_PROC_SIGNAL: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.TimerFlags.strict")
public var DISPATCH_TIMER_STRICT: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.FileSystemEvent.delete")
public var DISPATCH_VNODE_DELETE: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.FileSystemEvent.write")
public var DISPATCH_VNODE_WRITE: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.FileSystemEvent.extend")
public var DISPATCH_VNODE_EXTEND: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.FileSystemEvent.attrib")
public var DISPATCH_VNODE_ATTRIB: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.FileSystemEvent.link")
public var DISPATCH_VNODE_LINK: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.FileSystemEvent.rename")
public var DISPATCH_VNODE_RENAME: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.FileSystemEvent.revoke")
public var DISPATCH_VNODE_REVOKE: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchSource.FileSystemEvent.funlock")
public var DISPATCH_VNODE_FUNLOCK: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchTime.now()")
public var DISPATCH_TIME_NOW: Int {
  fatalError()
}

@available(*, unavailable, renamed: "DispatchTime.distantFuture")
public var DISPATCH_TIME_FOREVER: Int {
  fatalError()
}
