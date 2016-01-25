//===----------------------------------------------------------------------===//
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

#if os(OSX)

/// General error
public let kIOReturnError           = iokit_common_err(0x2bc)
/// Can't allocate memory
public let kIOReturnNoMemory        = iokit_common_err(0x2bd)
/// Resource shortage
public let kIOReturnNoResources     = iokit_common_err(0x2be)
/// Error during IPC
public let kIOReturnIPCError        = iokit_common_err(0x2bf)
/// No such device
public let kIOReturnNoDevice        = iokit_common_err(0x2c0)
/// Privilege violation
public let kIOReturnNotPrivileged   = iokit_common_err(0x2c1)
/// Invalid argument
public let kIOReturnBadArgument     = iokit_common_err(0x2c2)
/// Device read locked
public let kIOReturnLockedRead      = iokit_common_err(0x2c3)
/// Device write locked
public let kIOReturnLockedWrite     = iokit_common_err(0x2c4)
/// Exclusive access and device already open
public let kIOReturnExclusiveAccess = iokit_common_err(0x2c5)
/// Sent/received messages had different msg_id
public let kIOReturnBadMessageID    = iokit_common_err(0x2c6)
/// Unsupported function
public let kIOReturnUnsupported     = iokit_common_err(0x2c7)
/// Misc. VM failure
public let kIOReturnVMError         = iokit_common_err(0x2c8)
/// Internal error
public let kIOReturnInternalError   = iokit_common_err(0x2c9)
/// General I/O error
public let kIOReturnIOError         = iokit_common_err(0x2ca)
/// Can't acquire lock
public let kIOReturnCannotLock      = iokit_common_err(0x2cc)
/// Device not open
public let kIOReturnNotOpen         = iokit_common_err(0x2cd)
/// Read not supported
public let kIOReturnNotReadable     = iokit_common_err(0x2ce)
/// Write not supported
public let kIOReturnNotWritable     = iokit_common_err(0x2cf)
/// Alignment error
public let kIOReturnNotAligned      = iokit_common_err(0x2d0)
/// Media error
public let kIOReturnBadMedia        = iokit_common_err(0x2d1)
/// Device(s) still open
public let kIOReturnStillOpen       = iokit_common_err(0x2d2)
/// RLD failure
public let kIOReturnRLDError        = iokit_common_err(0x2d3)
/// DMA failure
public let kIOReturnDMAError        = iokit_common_err(0x2d4)
/// Device busy
public let kIOReturnBusy            = iokit_common_err(0x2d5)
/// I/O timeout
public let kIOReturnTimeout         = iokit_common_err(0x2d6)
/// Device offline
public let kIOReturnOffline         = iokit_common_err(0x2d7)
/// Not ready
public let kIOReturnNotReady        = iokit_common_err(0x2d8)
/// Device not attached
public let kIOReturnNotAttached     = iokit_common_err(0x2d9)
/// No DMA channels left
public let kIOReturnNoChannels      = iokit_common_err(0x2da)
/// No space for data
public let kIOReturnNoSpace         = iokit_common_err(0x2db)
/// Port already exists
public let kIOReturnPortExists      = iokit_common_err(0x2dd)
/// Can't wire down physical memory
public let kIOReturnCannotWire      = iokit_common_err(0x2de)
/// No interrupt attached
public let kIOReturnNoInterrupt     = iokit_common_err(0x2df)
/// No DMA frames enqueued
public let kIOReturnNoFrames        = iokit_common_err(0x2e0)
/// Oversized msg received on interrupt port
public let kIOReturnMessageTooLarge = iokit_common_err(0x2e1)
/// Not permitted
public let kIOReturnNotPermitted    = iokit_common_err(0x2e2)
/// No power to device
public let kIOReturnNoPower         = iokit_common_err(0x2e3)
/// Media not present
public let kIOReturnNoMedia         = iokit_common_err(0x2e4)
/// media not formatted
public let kIOReturnUnformattedMedia = iokit_common_err(0x2e5)
/// No such mode
public let kIOReturnUnsupportedMode = iokit_common_err(0x2e6)
/// Data underrun
public let kIOReturnUnderrun        = iokit_common_err(0x2e7)
/// Data overrun
public let kIOReturnOverrun         = iokit_common_err(0x2e8)
/// The device is not working properly
public let kIOReturnDeviceError     = iokit_common_err(0x2e9)
/// A completion routine is required
public let kIOReturnNoCompletion    = iokit_common_err(0x2ea)
/// Operation aborted
public let kIOReturnAborted         = iokit_common_err(0x2eb)
/// Bus bandwidth would be exceeded
public let kIOReturnNoBandwidth     = iokit_common_err(0x2ec)
/// Device not responding
public let kIOReturnNotResponding   = iokit_common_err(0x2ed)
/// Isochronous I/O request for distant past
public let kIOReturnIsoTooOld       = iokit_common_err(0x2ee)
/// Isochronous I/O request for distant future
public let kIOReturnIsoTooNew       = iokit_common_err(0x2ef)
/// Data was not found
public let kIOReturnNotFound        = iokit_common_err(0x2f0)
/// Should never be seen
public let kIOReturnInvalid         = iokit_common_err(0x1)

private let SYS_IOKIT        = UInt32((0x38 & 0x3f) << 26)
private let SUB_IOKIT_COMMON = UInt32((0 & 0xfff) << 14)

private func iokit_common_err(value: UInt32) -> Int32 {
    return Int32(bitPattern: SYS_IOKIT | SUB_IOKIT_COMMON | value)
}

#endif
