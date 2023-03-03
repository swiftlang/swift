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
@_implementationOnly import Darwin.Mach

@_spi(MemoryReaders) public struct MachError: Error {
  var result: kern_return_t
}

@_spi(MemoryReaders) public struct RemoteMemoryReader: MemoryReader {
  public typealias Address = UInt64

  private var task: task_t

  // Sadly we can't expose the type of this argument
  public init(task: Any) {
    self.task = task as! task_t
  }

  public func fetch<T>(from address: Address,
                       into buffer: UnsafeMutableBufferPointer<T>) throws {
    let size = mach_vm_size_t(MemoryLayout<T>.stride * buffer.count)
    var sizeOut = mach_vm_size_t(0)
    let kr = mach_vm_read_overwrite(task,
                                    mach_vm_address_t(address),
                                    mach_vm_size_t(size),
                                    unsafeBitCast(buffer.baseAddress,
                                                  to: mach_vm_address_t.self),
                                    &sizeOut)

    if kr != KERN_SUCCESS {
      throw MachError(result: kr)
    }
  }
}

@_spi(MemoryReaders) public struct LocalMemoryReader: MemoryReader {
  public typealias Address = UInt64

  public func fetch<T>(from address: Address,
                       into buffer: UnsafeMutableBufferPointer<T>) throws {
    let reader = RemoteMemoryReader(task: mach_task_self_)
    return try reader.fetch(from: address, into: buffer)
  }
}
#endif
