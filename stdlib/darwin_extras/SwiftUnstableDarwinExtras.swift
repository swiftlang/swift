//===--- DarwinExtras.swift -----------------------------------------------===//
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

import SwiftUnstable
import Darwin

public func _stdlib_mkstemps(inout template: String, suffixlen: CInt) -> CInt {
  var utf8 = template.nulTerminatedUTF8
  let (fd, fileName) = utf8.withUnsafeMutableBufferPointer {
    (utf8) -> (CInt, String) in
    let fd = mkstemps(UnsafeMutablePointer(utf8.baseAddress), suffixlen)
    let fileName = String.fromCString(UnsafePointer(utf8.baseAddress))!
    return (fd, fileName)
  }
  template = fileName
  return fd
}

public var _stdlib_FD_SETSIZE: CInt {
  return 1024
}

public struct _stdlib_fd_set {
  var _data: _UnitTestArray<UInt32>
  static var _wordBits: Int {
    return sizeof(UInt32) * 8
  }

  public init() {
    _data = _UnitTestArray<UInt32>(
      count: Int(_stdlib_FD_SETSIZE) / _stdlib_fd_set._wordBits,
      repeatedValue: 0)
  }

  public func isset(fd: CInt) -> Bool {
    let fdInt = Int(fd)
    return (
        _data[fdInt / _stdlib_fd_set._wordBits] &
          UInt32(1 << (fdInt % _stdlib_fd_set._wordBits))
      ) != 0
  }

  public mutating func set(fd: CInt) {
    let fdInt = Int(fd)
    _data[fdInt / _stdlib_fd_set._wordBits] |=
      UInt32(1 << (fdInt % _stdlib_fd_set._wordBits))
  }

  public mutating func clear(fd: CInt) {
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
  inout readfds: _stdlib_fd_set, inout writefds: _stdlib_fd_set,
  inout errorfds: _stdlib_fd_set, timeout: UnsafeMutablePointer<timeval>
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
public func _WSTATUS(status: CInt) -> CInt {
  return status & 0x7f
}

public var _WSTOPPED: CInt {
  return 0x7f
}

public func WIFEXITED(status: CInt) -> Bool {
  return _WSTATUS(status) == 0
}

public func WIFSIGNALED(status: CInt) -> Bool {
  return _WSTATUS(status) != _WSTOPPED && _WSTATUS(status) != 0
}

public func WEXITSTATUS(status: CInt) -> CInt {
  return (status >> 8) & 0xff
}

public func WTERMSIG(status: CInt) -> CInt {
  return _WSTATUS(status)
}

