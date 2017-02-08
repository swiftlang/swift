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

@_exported import IOKit

/// General error
public var kIOReturnError: IOReturn            { return iokit_common_err(0x2bc) }
/// Can't allocate memory
public var kIOReturnNoMemory: IOReturn         { return iokit_common_err(0x2bd) }
/// Resource shortage
public var kIOReturnNoResources: IOReturn      { return iokit_common_err(0x2be) }
/// Error during IPC
public var kIOReturnIPCError: IOReturn         { return iokit_common_err(0x2bf) }
/// No such device
public var kIOReturnNoDevice: IOReturn         { return iokit_common_err(0x2c0) }
/// Privilege violation
public var kIOReturnNotPrivileged: IOReturn    { return iokit_common_err(0x2c1) }
/// Invalid argument
public var kIOReturnBadArgument: IOReturn      { return iokit_common_err(0x2c2) }
/// Device read locked
public var kIOReturnLockedRead: IOReturn       { return iokit_common_err(0x2c3) }
/// Device write locked
public var kIOReturnLockedWrite: IOReturn      { return iokit_common_err(0x2c4) }
/// Exclusive access and device already open
public var kIOReturnExclusiveAccess: IOReturn  { return iokit_common_err(0x2c5) }
/// Sent/received messages had different msg_id
public var kIOReturnBadMessageID: IOReturn     { return iokit_common_err(0x2c6) }
/// Unsupported function
public var kIOReturnUnsupported: IOReturn      { return iokit_common_err(0x2c7) }
/// Misc. VM failure
public var kIOReturnVMError: IOReturn          { return iokit_common_err(0x2c8) }
/// Internal error
public var kIOReturnInternalError: IOReturn    { return iokit_common_err(0x2c9) }
/// General I/O error
public var kIOReturnIOError: IOReturn          { return iokit_common_err(0x2ca) }
/// Can't acquire lock
public var kIOReturnCannotLock: IOReturn       { return iokit_common_err(0x2cc) }
/// Device not open
public var kIOReturnNotOpen: IOReturn          { return iokit_common_err(0x2cd) }
/// Read not supported
public var kIOReturnNotReadable: IOReturn      { return iokit_common_err(0x2ce) }
/// Write not supported
public var kIOReturnNotWritable: IOReturn      { return iokit_common_err(0x2cf) }
/// Alignment error
public var kIOReturnNotAligned: IOReturn       { return iokit_common_err(0x2d0) }
/// Media error
public var kIOReturnBadMedia: IOReturn         { return iokit_common_err(0x2d1) }
/// Device(s) still open
public var kIOReturnStillOpen: IOReturn        { return iokit_common_err(0x2d2) }
/// RLD failure
public var kIOReturnRLDError: IOReturn         { return iokit_common_err(0x2d3) }
/// DMA failure
public var kIOReturnDMAError: IOReturn         { return iokit_common_err(0x2d4) }
/// Device busy
public var kIOReturnBusy: IOReturn             { return iokit_common_err(0x2d5) }
/// I/O timeout
public var kIOReturnTimeout: IOReturn          { return iokit_common_err(0x2d6) }
/// Device offline
public var kIOReturnOffline: IOReturn          { return iokit_common_err(0x2d7) }
/// Not ready
public var kIOReturnNotReady: IOReturn         { return iokit_common_err(0x2d8) }
/// Device not attached
public var kIOReturnNotAttached: IOReturn      { return iokit_common_err(0x2d9) }
/// No DMA channels left
public var kIOReturnNoChannels: IOReturn       { return iokit_common_err(0x2da) }
/// No space for data
public var kIOReturnNoSpace: IOReturn          { return iokit_common_err(0x2db) }
/// Port already exists
public var kIOReturnPortExists: IOReturn       { return iokit_common_err(0x2dd) }
/// Can't wire down physical memory
public var kIOReturnCannotWire: IOReturn       { return iokit_common_err(0x2de) }
/// No interrupt attached
public var kIOReturnNoInterrupt: IOReturn      { return iokit_common_err(0x2df) }
/// No DMA frames enqueued
public var kIOReturnNoFrames: IOReturn         { return iokit_common_err(0x2e0) }
/// Oversized msg received on interrupt port
public var kIOReturnMessageTooLarge: IOReturn  { return iokit_common_err(0x2e1) }
/// Not permitted
public var kIOReturnNotPermitted: IOReturn     { return iokit_common_err(0x2e2) }
/// No power to device
public var kIOReturnNoPower: IOReturn          { return iokit_common_err(0x2e3) }
/// Media not present
public var kIOReturnNoMedia: IOReturn          { return iokit_common_err(0x2e4) }
/// media not formatted
public var kIOReturnUnformattedMedia: IOReturn { return iokit_common_err(0x2e5) }
/// No such mode
public var kIOReturnUnsupportedMode: IOReturn  { return iokit_common_err(0x2e6) }
/// Data underrun
public var kIOReturnUnderrun: IOReturn         { return iokit_common_err(0x2e7) }
/// Data overrun
public var kIOReturnOverrun: IOReturn          { return iokit_common_err(0x2e8) }
/// The device is not working properly
public var kIOReturnDeviceError: IOReturn      { return iokit_common_err(0x2e9) }
/// A completion routine is required
public var kIOReturnNoCompletion: IOReturn     { return iokit_common_err(0x2ea) }
/// Operation aborted
public var kIOReturnAborted: IOReturn          { return iokit_common_err(0x2eb) }
/// Bus bandwidth would be exceeded
public var kIOReturnNoBandwidth: IOReturn      { return iokit_common_err(0x2ec) }
/// Device not responding
public var kIOReturnNotResponding: IOReturn    { return iokit_common_err(0x2ed) }
/// Isochronous I/O request for distant past
public var kIOReturnIsoTooOld: IOReturn        { return iokit_common_err(0x2ee) }
/// Isochronous I/O request for distant future
public var kIOReturnIsoTooNew: IOReturn        { return iokit_common_err(0x2ef) }
/// Data was not found
public var kIOReturnNotFound: IOReturn         { return iokit_common_err(0x2f0) }
/// Should never be seen
public var kIOReturnInvalid: IOReturn          { return iokit_common_err(0x1) }

internal let SYS_IOKIT        = UInt32((0x38 & 0x3f) << 26)
internal let SUB_IOKIT_COMMON = UInt32((0 & 0xfff) << 14)

internal func iokit_common_err(_ value: UInt32) -> IOReturn {
  return IOReturn(bitPattern: SYS_IOKIT | SUB_IOKIT_COMMON | value)
}
