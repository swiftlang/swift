//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Dynamic exclusivity checking.
///
//===----------------------------------------------------------------------===//

import SwiftShims

fileprivate let ValueBufferSize = unsafe 3 * MemoryLayout<UnsafeRawPointer>.stride

fileprivate let TrackingFlag: UInt = 0x20
fileprivate let ActionMask: UInt = 0x1

typealias AccessPointer = UnsafeMutablePointer<Access>
@unsafe struct Access {

  enum Action: UInt {
    case read
    case modify
  }

  struct NextAndAction {
    private var rawValue: UInt

    var action: Action {
      get {
        Action(rawValue: rawValue & ActionMask)!
      }
      set {
        rawValue = (rawValue & ~ActionMask) | newValue.rawValue
      }
    }

    var next: AccessPointer? {
      get { unsafe UnsafeMutablePointer(bitPattern: rawValue & ~ActionMask) }
      set { rawValue = UInt(bitPattern: newValue) | (rawValue & ActionMask) }
    }
  }

  var location: UnsafeRawPointer?
  var pc: UnsafeRawPointer?
  var nextAndAction: NextAndAction

  var action: Action {
    get { unsafe nextAndAction.action }
    set { unsafe nextAndAction.action = newValue }
  }

  var next: AccessPointer? {
    get { unsafe nextAndAction.next }
    set { unsafe nextAndAction.next = newValue }
  }

  static func from(rawPointer: UnsafeMutableRawPointer?) -> AccessPointer? {
    guard let rawPointer = unsafe rawPointer else { return nil }
    return unsafe rawPointer.assumingMemoryBound(to: Access.self)
  }

  @inline(__always)
  static func search(
    buffer: UnsafeMutableRawPointer,
    location: UnsafeRawPointer,
    pc: UnsafeRawPointer?,
    action: Action,
    inserting: Bool,
    head: inout AccessPointer?
  ) {
    var cursor = unsafe head
    while let nextPtr = unsafe cursor {
      if unsafe nextPtr.pointee.location == location {
        if unsafe nextPtr.pointee.action == .modify || action == .modify {
          #if !$Embedded
          unsafe _swift_reportExclusivityConflict(
            nextPtr.pointee.action.rawValue,
            nextPtr.pointee.pc,
            action.rawValue,
            pc,
            location)
          #else
          unsafe _embeddedReportExclusivityViolation(
            oldAction: nextPtr.pointee.action,
            oldPC: nextPtr.pointee.pc,
            newAction: action,
            newPC: pc,
            pointer: location)
          #endif
        }
      }
      unsafe cursor = nextPtr.pointee.next
    }

    if inserting {
      guard let access = unsafe Access.from(rawPointer: buffer) else {
        nullAccessBuffer()
      }

      unsafe access.pointee.location = location
      unsafe access.pointee.pc = pc
      unsafe access.pointee.action = action

      unsafe access.pointee.next = head
      unsafe head = access
    }
  }

  @inline(__always)
  static func remove(access: AccessPointer, head: inout AccessPointer?) {
    var cursor = unsafe head
    var previous: AccessPointer? = nil
    while let nextPtr = unsafe cursor {
      if unsafe nextPtr == access {
        if let previous = unsafe previous {
          unsafe previous.pointee.next = access.pointee.next
        } else {
          unsafe head = access.pointee.next
        }
        return
      }
      unsafe previous = nextPtr
      unsafe cursor = nextPtr.pointee.next
    }

    unsafe accessNotFound(access)
  }

  @inline(__always)
  static func findParent(
    access: AccessPointer,
    child: AccessPointer?
  ) -> AccessPointer? {
    var cursor = unsafe access
    while let next = unsafe cursor.pointee.next {
      if unsafe next == child {
        return unsafe cursor
      }
      unsafe cursor = next
    }

    // If we were searching for nil, then we found it.
    if unsafe child == nil {
      return unsafe cursor
    }

    // If we were searching for a non-nil node, we didn't find it.
    return nil
  }

  static func forEach(_ head: AccessPointer?, _ action: (Access) -> Void) {
    var cursor = unsafe head
    while let nextPtr = unsafe cursor {
      unsafe action(nextPtr.pointee)
      unsafe cursor = nextPtr.pointee.next
    }
  }
}

fileprivate var accessHead: AccessPointer? {
  get { unsafe Access.from(rawPointer: _swift_getExclusivityTLS()) }
  set { unsafe _swift_setExclusivityTLS(newValue) }
}

@c(swift_beginAccess)
@usableFromInline
@unsafe
internal func swift_beginAccess(
  pointer: UnsafeRawPointer,
  buffer: UnsafeMutableRawPointer,
  flags: UInt,
  pc: UnsafeRawPointer?
) {
  // Make sure that Access fits in the value buffer. This should be a static
  // assert, but we'll settle for a precondition for now. The optimizer should
  // eliminate the check when it's true.
  precondition(unsafe MemoryLayout<Access>.size <= ValueBufferSize)

  #if !$Embedded
  // If exclusivity checking is disabled, mark this access as untracked by
  // putting nil into the access buffer's pointer field, and return.
  if _swift_disableExclusivityChecking {
    unsafe Access.from(rawPointer: buffer)?.pointee.location = nil
    return
  }
  #endif

  guard let action = Access.Action(rawValue: flags & ActionMask) else {
    invalidFlags(flags)
  }

  let isTracking = (flags & TrackingFlag) != 0

  unsafe Access.search(
    buffer: buffer,
    location: pointer,
    pc: pc ?? _swift_stdlib_get_return_address(),
    action: action,
    inserting: isTracking,
    head: &accessHead)
}

@c(swift_endAccess)
@usableFromInline
@unsafe
internal func swift_endAccess(buffer: UnsafeMutableRawPointer) {
  guard let access = unsafe Access.from(rawPointer: buffer) else {
    nullAccessBuffer()
  }

  // If the pointer field is nil then we're not tracking this access.
  if unsafe access.pointee.location == nil {
    return
  }
  unsafe Access.remove(access: access, head: &accessHead)
}

@c(_swift_exclusivityAccessSetNext)
@usableFromInline
@unsafe
internal func _swift_exclusivityAccessSetNext(
  access: UnsafeMutableRawPointer,
  next: UnsafeMutableRawPointer
) {
  let access = unsafe Access.from(rawPointer: access)
  let next = unsafe Access.from(rawPointer: next)
  unsafe access?.pointee.next = next
}

#if INTERNAL_CHECKS_ENABLED
@c(swift_dumpTrackedAccesses)
@usableFromInline
@unsafe
internal func swift_dumpTrackedAccesses() {
  if let head = unsafe accessHead {
    unsafe Access.forEach(head) {
      unsafe _swift_stdlib_fputs_stderr("        Access. " +
          "Pointer: \($0.location, default: "<null>")." +
          "PC: \($0.pc, default: "<null>"). " +
          "AccessAction: \($0.action)\n")
    }
  } else {
    unsafe _swift_stdlib_fputs_stderr("        No Accesses.\n")
  }
}
#endif

/// Starting from `access`, find the access that is the parent node of `child`.
/// If `child` is `nil`, find the last access in the list.
@c(_swift_exclusivityAccessGetParent)
@usableFromInline
@unsafe
internal func _swift_exclusivityAccessGetParent(
  access: UnsafeMutableRawPointer?,
  child: UnsafeMutableRawPointer?
) -> UnsafeMutableRawPointer? {
    if let access = unsafe Access.from(rawPointer: access) {
      let result = unsafe Access.findParent(
        access: access, child: Access.from(rawPointer: child))
      return UnsafeMutableRawPointer(result)
    }
    return nil
}

@inline(never)
fileprivate func invalidFlags(_ flags: UInt) -> Never {
  reportExclusivityError("Internal exclusivity error",
                         "unable to construct action from flags \(flags)")
}

@inline(never)
fileprivate func accessNotFound(_ access: AccessPointer) -> Never {
  unsafe reportExclusivityError("Internal exclusivity error",
                                "didn't find exclusive access buffer \(access)")
}

@inline(never)
fileprivate func nullAccessBuffer() -> Never {
  reportExclusivityError("Internal exclusivity error", "NULL access buffer")
}

fileprivate func reportExclusivityError(
  _ prefix: StaticString, _ message: String
) -> Never {
  prefix.withUTF8Buffer { prefixBuffer in
    var message = message
    message.withUTF8 { messageBuffer in
      #if !$Embedded
      unsafe _swift_stdlib_reportFatalError(
          prefixBuffer.baseAddress!, CInt(prefixBuffer.count),
          messageBuffer.baseAddress!, CInt(messageBuffer.count),
          0)
      #else
      unsafe _embeddedReportFatalError(
          prefix: prefix,
          message: messageBuffer)
      #endif
    }
  }
  Builtin.int_trap()
}
