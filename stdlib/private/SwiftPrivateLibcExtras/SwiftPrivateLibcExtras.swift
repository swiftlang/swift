//===--- SwiftPrivateLibcExtras.swift -------------------------------------===//
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

import SwiftPrivate
#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(Android)
import Glibc
#endif

public func _stdlib_mkstemps(_ template: inout String, _ suffixlen: CInt) -> CInt {
#if os(Android)
  preconditionFailure("mkstemps doesn't work on Android")
#else
  var utf8 = template.nulTerminatedUTF8
  let (fd, fileName) = utf8.withUnsafeMutableBufferPointer {
    (utf8) -> (CInt, String) in
    let fd = mkstemps(UnsafeMutablePointer(utf8.baseAddress!), suffixlen)
    let fileName = String(cString: UnsafePointer(utf8.baseAddress!))
    return (fd, fileName)
  }
  template = fileName
  return fd
#endif
}

public var _stdlib_FD_SETSIZE: CInt {
  return 1024
}

public struct _stdlib_fd_set {
  var _data: [UInt32]
  static var _wordBits: Int {
    return sizeof(UInt32) * 8
  }

  public init() {
    _data = [UInt32](
      repeating: 0,
      count: Int(_stdlib_FD_SETSIZE) / _stdlib_fd_set._wordBits)
  }

  public func isset(_ fd: CInt) -> Bool {
    let fdInt = Int(fd)
    return (
        _data[fdInt / _stdlib_fd_set._wordBits] &
          UInt32(1 << (fdInt % _stdlib_fd_set._wordBits))
      ) != 0
  }

  public mutating func set(_ fd: CInt) {
    let fdInt = Int(fd)
    _data[fdInt / _stdlib_fd_set._wordBits] |=
      UInt32(1 << (fdInt % _stdlib_fd_set._wordBits))
  }

  public mutating func clear(_ fd: CInt) {
    let fdInt = Int(fd)
    _data[fdInt / _stdlib_fd_set._wordBits] &=
      ~UInt32(1 << (fdInt % _stdlib_fd_set._wordBits))
  }

  public mutating func zero() {
    let count = _data.count
    return _data.withUnsafeMutableBufferPointer {
      (_data) in
      for i in 0..<count {
        _data[i] = 0
      }
      return
    }
  }
}

public func _stdlib_select(
  _ readfds: inout _stdlib_fd_set, _ writefds: inout _stdlib_fd_set,
  _ errorfds: inout _stdlib_fd_set, _ timeout: UnsafeMutablePointer<timeval>?
) -> CInt {
  return readfds._data.withUnsafeMutableBufferPointer {
    (readfds) in
    writefds._data.withUnsafeMutableBufferPointer {
      (writefds) in
      errorfds._data.withUnsafeMutableBufferPointer {
        (errorfds) in
        let readAddr = readfds.baseAddress
        let writeAddr = writefds.baseAddress
        let errorAddr = errorfds.baseAddress
        return select(
          _stdlib_FD_SETSIZE,
          UnsafeMutablePointer(readAddr),
          UnsafeMutablePointer(writeAddr),
          UnsafeMutablePointer(errorAddr),
          timeout)
      }
    }
  }
}

//
// Functions missing in `Darwin` module.
//
public func _WSTATUS(_ status: CInt) -> CInt {
  return status & 0x7f
}

public var _WSTOPPED: CInt {
  return 0x7f
}

public func WIFEXITED(_ status: CInt) -> Bool {
  return _WSTATUS(status) == 0
}

public func WIFSIGNALED(_ status: CInt) -> Bool {
  return _WSTATUS(status) != _WSTOPPED && _WSTATUS(status) != 0
}

public func WEXITSTATUS(_ status: CInt) -> CInt {
  return (status >> 8) & 0xff
}

public func WTERMSIG(_ status: CInt) -> CInt {
  return _WSTATUS(status)
}
