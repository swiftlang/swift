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

#if !((os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64))

@available(SwiftStdlib 5.10, *)
extension Float16: AtomicValue {
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicRepresentation = UInt16.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming Float16
  ) -> AtomicRepresentation {
    UInt16.encodeAtomicRepresentation(value.bitPattern)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> Float16 {
    Float16(bitPattern: UInt16.decodeAtomicRepresentation(representation))
  }
}

#endif

@available(SwiftStdlib 5.10, *)
extension Float: AtomicValue {
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicRepresentation = UInt32.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming Float
  ) -> AtomicRepresentation {
    UInt32.encodeAtomicRepresentation(value.bitPattern)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> Float {
    Float(bitPattern: UInt32.decodeAtomicRepresentation(representation))
  }
}

#if (_pointerBitWidth(_32) && _hasAtomicBitWidth(_64)) || _pointerBitWidth(_64)

@available(SwiftStdlib 5.10, *)
extension Double: AtomicValue {
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicRepresentation = UInt64.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming Double
  ) -> AtomicRepresentation {
    UInt64.encodeAtomicRepresentation(value.bitPattern)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> Double {
    Double(bitPattern: UInt64.decodeAtomicRepresentation(representation))
  }
}

#endif
