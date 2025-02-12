//===--- MemoryReader.swift -----------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Provides the ability to read memory, both in the current process and
//  remotely.
//
//===----------------------------------------------------------------------===//

import Swift

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
internal import Darwin
#elseif os(Windows)
internal import ucrt
#elseif canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif

#if os(macOS)
internal import BacktracingImpl.OS.Darwin
#endif

@_spi(MemoryReaders) public protocol MemoryReader {
  typealias Address = UInt64
  typealias Size = UInt64

  /// Fill the specified buffer with data from the specified location in
  /// the source.
  func fetch(from address: Address,
             into buffer: UnsafeMutableRawBufferPointer) throws

  /// Fill the specified buffer with data from the specified location in
  /// the source.
  func fetch<T>(from address: Address,
                into buffer: UnsafeMutableBufferPointer<T>) throws

  /// Write data from the specified location in the source through a pointer
  func fetch<T>(from addr: Address,
                into pointer: UnsafeMutablePointer<T>) throws

  /// Fetch an array of Ts from the specified location in the source
  func fetch<T>(from addr: Address, count: Int, as: T.Type) throws -> [T]

  /// Fetch a T from the specified location in the source
  func fetch<T>(from addr: Address, as: T.Type) throws -> T

  /// Fetch a NUL terminated string from the specified location in the source
  func fetchString(from addr: Address) throws -> String?

  /// Fetch a fixed-length string from the specified location in the source
  func fetchString(from addr: Address, length: Int) throws -> String?
}

extension MemoryReader {

  public func fetch<T>(from address: Address,
                       into buffer: UnsafeMutableBufferPointer<T>) throws {
    try fetch(from: address, into: UnsafeMutableRawBufferPointer(buffer))
  }

  public func fetch<T>(from addr: Address,
                       into pointer: UnsafeMutablePointer<T>) throws {
    try fetch(from: addr,
              into: UnsafeMutableBufferPointer(start: pointer, count: 1))
  }

  public func fetch<T>(from addr: Address, count: Int, as: T.Type) throws -> [T] {
    let array = try Array<T>(unsafeUninitializedCapacity: count){
      buffer, initializedCount in

      try fetch(from: addr, into: buffer)

      initializedCount = count
    }

    return array
  }

  public func fetch<T>(from addr: Address, as: T.Type) throws -> T {
    return try withUnsafeTemporaryAllocation(of: T.self, capacity: 1) { buf in
      try fetch(from: addr, into: buf)
      return buf[0]
    }
  }

  public func fetchString(from addr: Address) throws -> String? {
    var bytes: [UInt8] = []
    var ptr = addr
    while true {
      let ch = try fetch(from: ptr, as: UInt8.self)
      if ch == 0 {
        break
      }
      bytes.append(ch)
      ptr += 1
    }

    return String(decoding: bytes, as: UTF8.self)
  }

  public func fetchString(from addr: Address, length: Int) throws -> String? {
    let bytes = try fetch(from: addr, count: length, as: UInt8.self)
    return String(decoding: bytes, as: UTF8.self)
  }
}

@_spi(MemoryReaders) public struct UnsafeLocalMemoryReader: MemoryReader {
  public init() {}

  public func fetch(from address: Address,
                    into buffer: UnsafeMutableRawBufferPointer) throws {
    buffer.baseAddress!.copyMemory(
      from: UnsafeRawPointer(bitPattern: UInt(address))!,
      byteCount: buffer.count
    )
  }

  public func fetch<T>(from address: Address, as type: T.Type) throws -> T {
    let ptr = UnsafeRawPointer(bitPattern: UInt(address))!
    return ptr.loadUnaligned(fromByteOffset: 0, as: type)
  }

  public func fetchString(from address: Address) throws -> String? {
    let ptr = UnsafeRawPointer(bitPattern: UInt(address))!
    return String(validatingUTF8: ptr.assumingMemoryBound(to: CChar.self))
  }
}

#if os(macOS)
@_spi(MemoryReaders) public struct MachError: Error {
  var result: kern_return_t
}

@_spi(MemoryReaders)
public struct UncachedRemoteMemoryReader: MemoryReader {
  private var task: task_t

  // Sadly we can't expose the type of this argument
  public init(task: Any) {
    self.task = task as! task_t
  }

  public func fetch(from address: Address,
                    into buffer: UnsafeMutableRawBufferPointer) throws {
    let size = buffer.count
    var sizeOut = UInt64(0)
    let result = mach_vm_read_overwrite(task,
                                        UInt64(address),
                                        UInt64(size),
                                        mach_vm_address_t(
                                          Int(bitPattern: buffer.baseAddress)
                                        ),
                                        &sizeOut)

    if result != KERN_SUCCESS {
      throw MachError(result: result)
    }
  }
}

@_spi(MemoryReaders)
public struct UncachedLocalMemoryReader: MemoryReader {
  public typealias Address = UInt64
  public typealias Size = UInt64

  public func fetch(from address: Address,
                    into buffer: UnsafeMutableRawBufferPointer) throws {
    let reader = UncachedRemoteMemoryReader(task: mach_task_self())
    return try reader.fetch(from: address, into: buffer)
  }
}
#endif

#if os(Linux)
@_spi(MemoryReaders) public struct POSIXError: Error {
  var errno: CInt
}

@_spi(MemoryReaders) public struct MemserverError: Error {
  var message: String
}

@_spi(MemoryReaders)
public struct UncachedMemserverMemoryReader: MemoryReader {
  private var fd: CInt

  public init(fd: CInt) {
    self.fd = fd
  }

  private func safeRead(_ fd: CInt, _ buffer: UnsafeMutableRawBufferPointer) throws -> Int {
    var done = 0
    while done < buffer.count {
      var ret: ssize_t = 0
      repeat {
        ret = read(fd, buffer.baseAddress! + done, buffer.count - done)
      } while ret < 0 && _swift_get_errno() == EINTR
      if ret < 0 {
        throw POSIXError(errno: _swift_get_errno())
      }
      if ret == 0 {
        break
      }
      done += Int(ret)
    }

    return done
  }

  private func safeWrite(_ fd: CInt, _ buffer: UnsafeRawBufferPointer) throws -> Int {
    var done = 0
    while done < buffer.count {
      var ret: ssize_t = 0
      repeat {
        ret = write(fd, buffer.baseAddress! + done, buffer.count - done)
      } while ret < 0 && _swift_get_errno() == EINTR
      if ret < 0 {
        throw POSIXError(errno: _swift_get_errno())
      }
      if ret == 0 {
        break
      }
      done += Int(ret)
    }

    return done
  }

  private func sendRequest(for bytes: Size, from addr: Address) throws {
    var request = memserver_req(addr: addr, len: bytes)
    try withUnsafeBytes(of: &request){ ptr in
      let ret = try safeWrite(fd, ptr)
      if ret != ptr.count {
        throw MemserverError(message: "Channel closed prematurely")
      }
    }
  }

  private func receiveReply() throws -> memserver_resp {
    var response = memserver_resp(addr: 0, len: 0)
    try withUnsafeMutableBytes(of: &response){ ptr in
      let ret = try safeRead(fd, ptr)
      if ret != ptr.count {
        throw MemserverError(message: "Channel closed prematurely")
      }
    }
    return response
  }

  public func fetch(from addr: Address,
                    into buffer: UnsafeMutableRawBufferPointer) throws {
    try sendRequest(for: Size(buffer.count), from: addr)

    var done = 0
    while done < buffer.count {
      let reply = try receiveReply()

      if reply.len < 0 {
        throw MemserverError(message: "Unreadable at \(hex(addr))")
      }

      if buffer.count - done < Int(reply.len) {
        throw MemserverError(message: "Overrun at \(hex(addr)) trying to read \(buffer.count) bytes")
      }

      let ret = try safeRead(fd,
                             UnsafeMutableRawBufferPointer(
                               rebasing: buffer[done..<done+Int(reply.len)]))

      if ret != reply.len {
        throw MemserverError(message: "Channel closed prematurely")
      }

      done += Int(reply.len)
    }
  }
}

@_spi(MemoryReaders)
public struct UncachedRemoteMemoryReader: MemoryReader {
  private var pid: pid_t

  public init(pid: Any) {
    self.pid = pid as! pid_t
  }

  public func fetch(from address: Address,
                    into buffer: UnsafeMutableRawBufferPointer) throws {
    let size = buffer.count
    var fromIOVec = iovec(iov_base: UnsafeMutableRawPointer(
                            bitPattern: UInt(address)),
                          iov_len: size)
    var toIOVec = iovec(iov_base: buffer.baseAddress, iov_len: size)
    let result = process_vm_readv(pid, &toIOVec, 1, &fromIOVec, 1, 0)
    if result != size {
      throw POSIXError(errno: _swift_get_errno())
    }
  }
}

@_spi(MemoryReaders)
public struct UncachedLocalMemoryReader: MemoryReader {
  private var reader: RemoteMemoryReader

  init() {
    reader = RemoteMemoryReader(pid: getpid())
  }

  public func fetch(from address: Address,
                    into buffer: UnsafeMutableRawBufferPointer) throws {
    return try reader.fetch(from: address, into: buffer)
  }
}
#endif
