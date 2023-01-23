//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 5.9, *)
@frozen
public struct ValueWitnessTable: PublicLayout {
  public typealias Layout = UnsafePointer<(
    initializeBufferWithCopyOfBuffer: InitializeBufferWithCopyOfBuffer,
    destroy: Destroy,
    initializeWithCopy: InitializeWithCopy,
    assignWithCopy: AssignWithCopy,
    initializeWithTake: InitializeWithTake,
    assignWithTake: AssignWithTake,
    getEnumTagSinglePayload: GetEnumTagSinglePayload,
    storeEnumTagSinglePayload: StoreEnumTagSinglePayload,
    size: Int,
    stride: Int,
    flags: Flags,
    extraInhabitantCount: UInt32
  )>
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

//===----------------------------------------------------------------------===//
// Function Typealiases
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension ValueWitnessTable {
  public typealias InitializeBufferWithCopyOfBuffer = @convention(c) (
    // Destination
    UnsafeMutableRawPointer,
    // Source
    UnsafeRawPointer,
    // Metadata
    UnsafeRawPointer
  ) -> UnsafeMutableRawPointer
  
  public typealias Destroy = @convention(c) (
    // Source
    UnsafeMutableRawPointer,
    // Metadata
    UnsafeRawPointer
  ) -> ()
  
  public typealias InitializeWithCopy = @convention(c) (
    // Destination
    UnsafeMutableRawPointer,
    // Source
    UnsafeRawPointer,
    // Metadata
    UnsafeRawPointer
  ) -> UnsafeMutableRawPointer
  
  public typealias AssignWithCopy = @convention(c) (
    // Destination
    UnsafeMutableRawPointer,
    // Source
    UnsafeRawPointer,
    // Metadata
    UnsafeRawPointer
  ) -> UnsafeMutableRawPointer
  
  public typealias InitializeWithTake = @convention(c) (
    // Destination
    UnsafeMutableRawPointer,
    // Source
    UnsafeMutableRawPointer,
    // Metadata
    UnsafeRawPointer
  ) -> UnsafeMutableRawPointer
  
  public typealias AssignWithTake = @convention(c) (
    // Destination
    UnsafeMutableRawPointer,
    // Source
    UnsafeMutableRawPointer,
    // Metadata
    UnsafeRawPointer
  ) -> UnsafeMutableRawPointer
  
  public typealias GetEnumTagSinglePayload = @convention(c) (
    // Enum instance
    UnsafeRawPointer,
    // Number of empty cases
    UInt32,
    // Metadata
    UnsafeRawPointer
  ) -> UInt32
  
  public typealias StoreEnumTagSinglePayload = @convention(c) (
    // Enum instance
    UnsafeMutableRawPointer,
    // Case tag
    UInt32,
    // Number of empty cases
    UInt32,
    // Metadata
    UnsafeRawPointer
  ) -> ()
}

//===----------------------------------------------------------------------===//
// Function API
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension ValueWitnessTable {
  @inlinable
  @discardableResult
  public func initializeBufferWithCopyOfBuffer(
    _ dest: UnsafeMutableRawPointer,
    _ src: UnsafeRawPointer
  ) -> UnsafeMutableRawPointer {
    return address(
      for: \.initializeBufferWithCopyOfBuffer
    ).signedVWTInitializeBufferWithCopyOfBuffer(
      dest,
      src,
      trailing
    )
  }
  
  @inlinable
  public func destroy(_ src: UnsafeMutableRawPointer) {
    address(for: \.destroy).signedVWTDestroy(src, trailing)
  }
  
  @inlinable
  @discardableResult
  public func initializeWithCopy(
    _ dest: UnsafeMutableRawPointer,
    _ src: UnsafeRawPointer
  ) -> UnsafeMutableRawPointer {
    return address(
      for: \.initializeWithCopy
    ).signedVWTInitializeWithCopy(dest, src, trailing)
  }
  
  @inlinable
  @discardableResult
  public func assignWithCopy(
    _ dest: UnsafeMutableRawPointer,
    _ src: UnsafeRawPointer
  ) -> UnsafeMutableRawPointer {
    return address(
      for: \.assignWithCopy
    ).signedVWTAssignWithCopy(dest, src, trailing)
  }
  
  @inlinable
  @discardableResult
  public func initializeWithTake(
    _ dest: UnsafeMutableRawPointer,
    _ src: UnsafeMutableRawPointer
  ) -> UnsafeMutableRawPointer {
    return address(
      for: \.initializeWithTake
    ).signedVWTInitializeWithTake(dest, src, trailing)
  }
  
  @inlinable
  @discardableResult
  public func assignWithTake(
    _ dest: UnsafeMutableRawPointer,
    _ src: UnsafeMutableRawPointer
  ) -> UnsafeMutableRawPointer {
    return address(
      for: \.assignWithTake
    ).signedVWTAssignWithTake(dest, src, trailing)
  }
  
  @inlinable
  @discardableResult
  public func getEnumTagSinglePayload(
    _ src: UnsafeRawPointer,
    _ numberOfEmptyCases: UInt32
  ) -> UInt32 {
    return address(
      for: \.getEnumTagSinglePayload
    ).signedVWTGetEnumTagSinglePayload(
      src,
      numberOfEmptyCases,
      trailing
    )
  }
  
  @inlinable
  public func storeEnumTagSinglePayload(
    _ src: UnsafeMutableRawPointer,
    _ tag: UInt32,
    _ numberOfEmptyCases: UInt32
  ) -> () {
    return address(
      for: \.storeEnumTagSinglePayload
    ).signedVWTStoreEnumTagSinglePayload(
      src,
      tag,
      numberOfEmptyCases,
      trailing
    )
  }
}

//===----------------------------------------------------------------------===//
// Property API
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension ValueWitnessTable {
  @inlinable
  public var size: Int {
    layout.pointee.size
  }
  
  @inlinable
  public var stride: Int {
    layout.pointee.stride
  }
  
  @inlinable
  public var flags: Flags {
    layout.pointee.flags
  }
  
  @inlinable
  public var extraInhabitantCount: Int {
    Int(truncatingIfNeeded: layout.pointee.extraInhabitantCount)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension ValueWitnessTable: Equatable {
  @inlinable
  public static func ==(
    lhs: ValueWitnessTable,
    rhs: ValueWitnessTable
  ) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension ValueWitnessTable: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
