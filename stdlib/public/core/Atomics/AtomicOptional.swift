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
public protocol AtomicOptionalWrappable: AtomicValue {
  associatedtype AtomicOptionalRepresentation

  static func encodeAtomicOptionalRepresentation(
    _ value: consuming Self?
  ) -> AtomicOptionalRepresentation

  static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> Self?
}

@available(SwiftStdlib 5.10, *)
extension AtomicOptionalWrappable
where
  AtomicRepresentation == AtomicOptionalRepresentation
{
  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming Self
  ) -> AtomicRepresentation {
    Self.encodeAtomicOptionalRepresentation(value)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> Self {
    Self.decodeAtomicOptionalRepresentation(representation)!
  }
}

@available(SwiftStdlib 5.10, *)
extension Optional: AtomicValue where Wrapped: AtomicOptionalWrappable {
  public typealias AtomicRepresentation = Wrapped.AtomicOptionalRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming Wrapped?
  ) -> AtomicRepresentation {
    Wrapped.encodeAtomicOptionalRepresentation(value)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> Wrapped? {
    Wrapped.decodeAtomicOptionalRepresentation(representation)
  }
}
