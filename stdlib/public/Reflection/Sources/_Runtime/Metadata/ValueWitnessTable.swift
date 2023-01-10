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

@available(SwiftStdlib 9999, *)
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

@available(SwiftStdlib 9999, *)
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

@available(SwiftStdlib 9999, *)
extension ValueWitnessTable {
  @inlinable
  @discardableResult
  public func initializeBufferWithCopyOfBuffer(
    _ dest: UnsafeMutableRawPointer,
    _ src: UnsafeRawPointer
  ) -> UnsafeMutableRawPointer {
    let address = layout.raw

    return address.signedVWTInitializeBufferWithCopyOfBuffer(
      dest,
      src,
      trailing
    )
  }
  
  @inlinable
  public func destroy(_ src: UnsafeMutableRawPointer) {
    // rdar://103834325
    // FIXME: There's currently a compiler bug preventing me from doing:
    // 'address(of: \.destroy)'
    // or even
    // 'MemoryLayout<ValueWitnessTable.Layout.Pointee>.offset(of: \.destroy)'
    //
    // The same goes for everything else in this file
    let address = layout.raw
                  + MemoryLayout<InitializeBufferWithCopyOfBuffer>.size
    
    address.signedVWTDestroy(src, trailing)
  }
  
  @inlinable
  @discardableResult
  public func initializeWithCopy(
    _ dest: UnsafeMutableRawPointer,
    _ src: UnsafeRawPointer
  ) -> UnsafeMutableRawPointer {
    let address = layout.raw
                  + MemoryLayout<InitializeBufferWithCopyOfBuffer>.size
                  + MemoryLayout<Destroy>.size
    
    return address.signedVWTInitializeWithCopy(dest, src, trailing)
  }
  
  @inlinable
  @discardableResult
  public func assignWithCopy(
    _ dest: UnsafeMutableRawPointer,
    _ src: UnsafeRawPointer
  ) -> UnsafeMutableRawPointer {
    let address = layout.raw
                  + MemoryLayout<InitializeBufferWithCopyOfBuffer>.size
                  + MemoryLayout<Destroy>.size
                  + MemoryLayout<InitializeWithCopy>.size
    
    return address.signedVWTAssignWithCopy(dest, src, trailing)
  }
  
  @inlinable
  @discardableResult
  public func initializeWithTake(
    _ dest: UnsafeMutableRawPointer,
    _ src: UnsafeMutableRawPointer
  ) -> UnsafeMutableRawPointer {
    let address = layout.raw
                  + MemoryLayout<InitializeBufferWithCopyOfBuffer>.size
                  + MemoryLayout<Destroy>.size
                  + MemoryLayout<InitializeWithCopy>.size
                  + MemoryLayout<AssignWithCopy>.size
    
    return address.signedVWTInitializeWithTake(dest, src, trailing)
  }
  
  @inlinable
  @discardableResult
  public func assignWithTake(
    _ dest: UnsafeMutableRawPointer,
    _ src: UnsafeMutableRawPointer
  ) -> UnsafeMutableRawPointer {
    let address = layout.raw
                  + MemoryLayout<InitializeBufferWithCopyOfBuffer>.size
                  + MemoryLayout<Destroy>.size
                  + MemoryLayout<InitializeWithCopy>.size
                  + MemoryLayout<AssignWithCopy>.size
                  + MemoryLayout<InitializeWithTake>.size
    
    return address.signedVWTAssignWithTake(dest, src, trailing)
  }
  
  @inlinable
  @discardableResult
  public func getEnumTagSinglePayload(
    _ src: UnsafeRawPointer,
    _ numberOfEmptyCases: UInt32
  ) -> UInt32 {
    let address = layout.raw
                  + MemoryLayout<InitializeBufferWithCopyOfBuffer>.size
                  + MemoryLayout<Destroy>.size
                  + MemoryLayout<InitializeWithCopy>.size
                  + MemoryLayout<AssignWithCopy>.size
                  + MemoryLayout<InitializeWithTake>.size
                  + MemoryLayout<AssignWithTake>.size
    
    return address.signedVWTGetEnumTagSinglePayload(
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
    let address = layout.raw
                  + MemoryLayout<InitializeBufferWithCopyOfBuffer>.size
                  + MemoryLayout<Destroy>.size
                  + MemoryLayout<InitializeWithCopy>.size
                  + MemoryLayout<AssignWithCopy>.size
                  + MemoryLayout<InitializeWithTake>.size
                  + MemoryLayout<AssignWithTake>.size
                  + MemoryLayout<GetEnumTagSinglePayload>.size
    
    return address.signedVWTStoreEnumTagSinglePayload(
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

@available(SwiftStdlib 9999, *)
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

@available(SwiftStdlib 9999, *)
extension ValueWitnessTable: Equatable {
  @inlinable
  public static func ==(
    lhs: ValueWitnessTable,
    rhs: ValueWitnessTable
  ) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 9999, *)
extension ValueWitnessTable: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
