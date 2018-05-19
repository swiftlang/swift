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

import SwiftShims

public struct _FDInputStream {
  public let fd: CInt
  public var isClosed: Bool = false
  public var isEOF: Bool = false
  internal var _buffer = [UInt8](repeating: 0, count: 256)
  internal var _bufferUsed: Int = 0

  public init(fd: CInt) {
    self.fd = fd
  }

  public mutating func getline() -> String? {
    if let newlineIndex =
      _buffer[0..<_bufferUsed].index(of: UInt8(Unicode.Scalar("\n").value)) {
      let result = String(decoding: _buffer[0..<newlineIndex], as: UTF8.self)
      _buffer.removeSubrange(0...newlineIndex)
      _bufferUsed -= newlineIndex + 1
      return result
    }
    if isEOF && _bufferUsed > 0 {
      let result = String(decoding: _buffer[0..<_bufferUsed], as: UTF8.self)
      _buffer.removeAll()
      _bufferUsed = 0
      return result
    }
    return nil
  }

  public mutating func read() {
    let minFree = 128
    var bufferFree = _buffer.count - _bufferUsed
    if bufferFree < minFree {
      _buffer.reserveCapacity(minFree - bufferFree)
      while bufferFree < minFree {
        _buffer.append(0)
        bufferFree += 1
      }
    }
    let fd = self.fd
    let readResult: __swift_ssize_t = _buffer.withUnsafeMutableBufferPointer {
      (_buffer) in
      let addr = _buffer.baseAddress! + self._bufferUsed
      let size = bufferFree
      return _stdlib_read(fd, addr, size)
    }
    if readResult == 0 {
      isEOF = true
      return
    }
    if readResult < 0 {
      fatalError("read() returned error")
    }
    _bufferUsed += readResult
  }

  public mutating func close() {
    if isClosed {
      return
    }
    let result = _stdlib_close(fd)
    if result < 0 {
      fatalError("close() returned an error")
    }
    isClosed = true
  }
}

public struct _Stderr : TextOutputStream {
  public init() {}

  public mutating func write(_ string: String) {
    for c in string.utf8 {
      _swift_stdlib_putc_stderr(CInt(c))
    }
  }
}

public struct _FDOutputStream : TextOutputStream {
  public let fd: CInt
  public var isClosed: Bool = false

  public init(fd: CInt) {
    self.fd = fd
  }

  public mutating func write(_ string: String) {
    let utf8CStr = string.utf8CString
    utf8CStr.withUnsafeBufferPointer {
      (utf8CStr) -> Void in
      var writtenBytes = 0
      let bufferSize = utf8CStr.count - 1
      while writtenBytes != bufferSize {
        let result = _stdlib_write(
          self.fd, UnsafeRawPointer(utf8CStr.baseAddress! + Int(writtenBytes)),
          bufferSize - writtenBytes)
        if result < 0 {
          fatalError("write() returned an error")
        }
        writtenBytes += result
      }
    }
  }

  public mutating func close() {
    if isClosed {
      return
    }
    let result = _stdlib_close(fd)
    if result < 0 {
      fatalError("close() returned an error")
    }
    isClosed = true
  }
}
