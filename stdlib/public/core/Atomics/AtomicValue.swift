//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Atomics open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
public protocol AtomicValue {
  associatedtype AtomicRepresentation

  static func encodeAtomicRepresentation(
    _ value: consuming Self
  ) -> AtomicRepresentation

  static func decodeAtomicRepresentation(
    _ storage: consuming AtomicRepresentation
  ) -> Self
}

@available(SwiftStdlib 5.10, *)
extension RawRepresentable
where
  Self: AtomicValue,
  RawValue: AtomicValue
{
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicRepresentation = RawValue.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming Self
  ) -> RawValue.AtomicRepresentation {
    RawValue.encodeAtomicRepresentation(value.rawValue)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming RawValue.AtomicRepresentation
  ) -> Self {
    Self(rawValue: RawValue.decodeAtomicRepresentation(representation))!
  }
}

@available(SwiftStdlib 5.10, *)
extension Never: AtomicValue {
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicRepresentation = Never

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming Never
  ) -> Never {}

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming Never
  ) -> Never {}
}
