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

@_implementationOnly import _SwiftBacktracingShims

@_spi(MemoryReaders) public protocol MemoryReader {
  associatedtype Address: FixedWidthInteger

  func fetch<T>(from address: Address,
                into buffer: UnsafeMutableBufferPointer<T>) throws

  func fetch<T>(from addr: Address,
                into pointer: UnsafeMutablePointer<T>) throws

  func fetch<T>(from addr: Address, count: Int, as: T.Type) throws -> [T]

  func fetch<T>(from addr: Address, as: T.Type) throws -> T
}

extension MemoryReader {

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

}

@_spi(MemoryReaders) public struct UnsafeLocalMemoryReader: MemoryReader {
  public typealias Address = UInt

  public func fetch<T>(from address: Address,
                       into buffer: UnsafeMutableBufferPointer<T>) throws {
    buffer.baseAddress!.update(from: UnsafePointer<T>(bitPattern: address)!,
                               count: buffer.count)
  }
}

#if os(macOS)
@_spi(MemoryReaders) public struct MachError: Error {
  var result: __swift_kern_return_t
}

@_spi(MemoryReaders) public struct RemoteMemoryReader: MemoryReader {
  public typealias Address = UInt64

  private var task: __swift_task_t

  // Sadly we can't expose the type of this argument
  public init(task: Any) {
    self.task = task as! __swift_task_t
  }

  public func fetch<T>(from address: Address,
                       into buffer: UnsafeMutableBufferPointer<T>) throws {
    let size = UInt64(MemoryLayout<T>.stride * buffer.count)
    var sizeOut = UInt64(0)
    let result = _swift_backtrace_vm_read(task,
                                          UInt64(address),
                                          UInt64(size),
                                          buffer.baseAddress,
                                          &sizeOut)

    if result != _SWIFT_KERN_SUCCESS {
      throw MachError(result: result)
    }
  }
}

@_spi(MemoryReaders) public struct LocalMemoryReader: MemoryReader {
  public typealias Address = UInt64

  public func fetch<T>(from address: Address,
                       into buffer: UnsafeMutableBufferPointer<T>) throws {
    let reader = RemoteMemoryReader(task: _swift_backtrace_task_self())
    return try reader.fetch(from: address, into: buffer)
  }
}
#endif
