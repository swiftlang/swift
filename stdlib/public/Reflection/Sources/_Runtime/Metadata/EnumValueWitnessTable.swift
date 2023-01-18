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
public struct EnumValueWitnessTable: PublicLayout {
  public typealias Layout = UnsafePointer<(
    base: ValueWitnessTable.Layout.Pointee,
    getEnumTag: @convention(c) (
      UnsafeRawPointer,
      UnsafeRawPointer
    ) -> UInt32,
    destructiveProjectEnumData: @convention(c) (
      UnsafeMutableRawPointer,
      UnsafeRawPointer
    ) -> (),
    destructiveInjectEnumTag: @convention(c) (
      UnsafeRawPointer,
      UInt32,
      UnsafeRawPointer
    ) -> ()
  )>
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension EnumValueWitnessTable {
  @inlinable
  public func getEnumTag(_ value: UnsafeRawPointer) -> UInt32 {
    layout.pointee.getEnumTag(value, trailing)
  }
  
  @inlinable
  public func destructiveProjectEnumData(_ value: UnsafeMutableRawPointer) {
    layout.pointee.destructiveProjectEnumData(value, trailing)
  }
  
  @inlinable
  public func destructiveInjectEnumTag(
    _ tag: UInt32,
    into value: UnsafeMutableRawPointer
  ) {
    layout.pointee.destructiveInjectEnumTag(value, tag, trailing)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension EnumValueWitnessTable: Equatable {
  @inlinable
  public static func ==(
    lhs: EnumValueWitnessTable,
    rhs: EnumValueWitnessTable
  ) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension EnumValueWitnessTable: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
