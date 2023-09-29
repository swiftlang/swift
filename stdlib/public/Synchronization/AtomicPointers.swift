//===----------------------------------------------------------------------===//
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

import Builtin

//===----------------------------------------------------------------------===//
// UnsafePointer AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension UnsafePointer: AtomicOptionalWrappable {
  public typealias AtomicOptionalRepresentation = Int.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicOptionalRepresentation(
    _ value: consuming UnsafePointer<Pointee>?
  ) -> AtomicOptionalRepresentation {
    Int.AtomicRepresentation(Int(bitPattern: value)._value)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> UnsafePointer<Pointee>? {
    UnsafePointer<Pointee>(bitPattern: Int(representation.storage))
  }
}

//===----------------------------------------------------------------------===//
// UnsafeMutablePointer AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension UnsafeMutablePointer: AtomicOptionalWrappable {
  public typealias AtomicOptionalRepresentation = Int.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicOptionalRepresentation(
    _ value: consuming UnsafeMutablePointer<Pointee>?
  ) -> AtomicOptionalRepresentation {
    Int.AtomicRepresentation(Int(bitPattern: value)._value)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> UnsafeMutablePointer<Pointee>? {
    UnsafeMutablePointer<Pointee>(bitPattern: Int(representation.storage))
  }
}

//===----------------------------------------------------------------------===//
// UnsafeRawPointer AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension UnsafeRawPointer: AtomicOptionalWrappable {
  public typealias AtomicOptionalRepresentation = Int.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicOptionalRepresentation(
    _ value: consuming UnsafeRawPointer?
  ) -> AtomicOptionalRepresentation {
    Int.AtomicRepresentation(Int(bitPattern: value)._value)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> UnsafeRawPointer? {
    UnsafeRawPointer(bitPattern: Int(representation.storage))
  }
}

//===----------------------------------------------------------------------===//
// UnsafeMutableRawPointer AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension UnsafeMutableRawPointer: AtomicOptionalWrappable {
  public typealias AtomicOptionalRepresentation = Int.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicOptionalRepresentation(
    _ value: consuming UnsafeMutableRawPointer?
  ) -> AtomicOptionalRepresentation {
    Int.AtomicRepresentation(Int(bitPattern: value)._value)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> UnsafeMutableRawPointer? {
    UnsafeMutableRawPointer(bitPattern: Int(representation.storage))
  }
}

//===----------------------------------------------------------------------===//
// Unmanaged AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension Unmanaged: AtomicOptionalWrappable {
  public typealias AtomicOptionalRepresentation = Int.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicOptionalRepresentation(
    _ value: consuming Unmanaged<Instance>?
  ) -> AtomicOptionalRepresentation {
    // The following leads to a compiler crash at the moment.
    // Int.AtomicRepresentation(Int(bitPattern: value?.toOpaque())._value)

    if let unmanaged = value {
      return Int.AtomicRepresentation(
        Int(bitPattern: unmanaged.toOpaque())._value
      )
    }

    return Int.AtomicRepresentation(0._value)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> Unmanaged<Instance>? {
    UnsafeRawPointer(bitPattern: Int(representation.storage)).map {
      Unmanaged.fromOpaque($0)
    }
  }
}
