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

//===----------------------------------------------------------------------===//
// UnsafePointer AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension UnsafePointer: AtomicOptionalWrappable {
  // Pointers' optional representation is the same as their base atomic
  // representation.
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicOptionalRepresentation = Int.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicOptionalRepresentation(
    _ value: consuming UnsafePointer<Pointee>?
  ) -> AtomicOptionalRepresentation {
    Int.encodeAtomicRepresentation(
      Int(bitPattern: value)
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> UnsafePointer<Pointee>? {
    UnsafePointer<Pointee>(
      bitPattern: Int.decodeAtomicRepresentation(representation)
    )
  }
}

//===----------------------------------------------------------------------===//
// UnsafeMutablePointer AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension UnsafeMutablePointer: AtomicOptionalWrappable {
  // Pointers' optional representation is the same as their base atomic
  // representation.
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicOptionalRepresentation = Int.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicOptionalRepresentation(
    _ value: consuming UnsafeMutablePointer<Pointee>?
  ) -> AtomicOptionalRepresentation {
    Int.encodeAtomicRepresentation(
      Int(bitPattern: value)
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> UnsafeMutablePointer<Pointee>? {
    UnsafeMutablePointer<Pointee>(
      bitPattern: Int.decodeAtomicRepresentation(representation)
    )
  }
}

//===----------------------------------------------------------------------===//
// UnsafeRawPointer AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension UnsafeRawPointer: AtomicOptionalWrappable {
  // Pointers' optional representation is the same as their base atomic
  // representation.
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicOptionalRepresentation = Int.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicOptionalRepresentation(
    _ value: consuming UnsafeRawPointer?
  ) -> AtomicOptionalRepresentation {
    Int.encodeAtomicRepresentation(
      Int(bitPattern: value)
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> UnsafeRawPointer? {
    UnsafeRawPointer(
      bitPattern: Int.decodeAtomicRepresentation(representation)
    )
  }
}

//===----------------------------------------------------------------------===//
// UnsafeMutableRawPointer AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension UnsafeMutableRawPointer: AtomicOptionalWrappable {
  // Pointers' optional representation is the same as their base atomic
  // representation.
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicOptionalRepresentation = Int.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicOptionalRepresentation(
    _ value: consuming UnsafeMutableRawPointer?
  ) -> AtomicOptionalRepresentation {
    Int.encodeAtomicRepresentation(
      Int(bitPattern: value)
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> UnsafeMutableRawPointer? {
    UnsafeMutableRawPointer(
      bitPattern: Int.decodeAtomicRepresentation(representation)
    )
  }
}

//===----------------------------------------------------------------------===//
// Unmanaged AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension Unmanaged: AtomicOptionalWrappable {
  // Pointers' optional representation is the same as their base atomic
  // representation.
  @available(SwiftStdlib 5.10, *)
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
      return Int.encodeAtomicRepresentation(
        Int(bitPattern: unmanaged.toOpaque())
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
    UnsafeRawPointer.decodeAtomicOptionalRepresentation(representation).map {
      Unmanaged.fromOpaque($0)
    }
  }
}

//===----------------------------------------------------------------------===//
// OpaquePointer AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension OpaquePointer: AtomicOptionalWrappable {
  // Pointers' optional representation is the same as their base atomic
  // representation.
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicOptionalRepresentation = Int.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicOptionalRepresentation(
    _ value: consuming OpaquePointer?
  ) -> AtomicOptionalRepresentation {
    Int.encodeAtomicRepresentation(
      Int(bitPattern: value)
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> OpaquePointer? {
    OpaquePointer(
      bitPattern: Int.decodeAtomicRepresentation(representation)
    )
  }
}

//===----------------------------------------------------------------------===//
// ObjectIdentifier AtomicOptionalWrappable conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension ObjectIdentifier: AtomicOptionalWrappable {
  // Pointers' optional representation is the same as their base atomic
  // representation.
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicOptionalRepresentation = Int.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicOptionalRepresentation(
    _ value: consuming ObjectIdentifier?
  ) -> AtomicOptionalRepresentation {
    Int.encodeAtomicRepresentation(
      // {U}Int have bitPattern inits for ObjectIdentifier, but not optional
      // ObjectIdentifier :sad:
      unsafeBitCast(value, to: Int.self)
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicOptionalRepresentation(
    _ representation: consuming AtomicOptionalRepresentation
  ) -> ObjectIdentifier? {
    // ObjectIdentifier doesn't have a bitPattern init..?
    unsafeBitCast(
      Int.decodeAtomicRepresentation(representation),
      to: ObjectIdentifier?.self
    )
  }
}

//===----------------------------------------------------------------------===//
// UnsafeBufferPointer AtomicValue conformance
//===----------------------------------------------------------------------===//

#if (_pointerBitWidth(_32) && _hasAtomicBitWidth(_64)) || (_pointerBitWidth(_64) && _hasAtomicBitWidth(_128))

@available(SwiftStdlib 5.10, *)
extension UnsafeBufferPointer: AtomicValue {
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicRepresentation = WordPair.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming UnsafeBufferPointer<Element>
  ) -> AtomicRepresentation {
    let valueCopy = value

    return WordPair.encodeAtomicRepresentation(
      WordPair(
        highWord: UInt(bitPattern: valueCopy.baseAddress),
        lowWord: UInt(truncatingIfNeeded: valueCopy.count)
      )
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> UnsafeBufferPointer<Element> {
    let wp = WordPair.decodeAtomicRepresentation(representation)

    return UnsafeBufferPointer<Element>(
      start: UnsafePointer<Element>(bitPattern: wp.highWord),
      count: Int(truncatingIfNeeded: wp.lowWord)
    )
  }
}

#endif

//===----------------------------------------------------------------------===//
// UnsafeMutableBufferPointer AtomicValue conformance
//===----------------------------------------------------------------------===//

#if (_pointerBitWidth(_32) && _hasAtomicBitWidth(_64)) || (_pointerBitWidth(_64) && _hasAtomicBitWidth(_128))

@available(SwiftStdlib 5.10, *)
extension UnsafeMutableBufferPointer: AtomicValue {
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicRepresentation = WordPair.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming UnsafeMutableBufferPointer<Element>
  ) -> AtomicRepresentation {
    let valueCopy = value

    return WordPair.encodeAtomicRepresentation(
      WordPair(
        highWord: UInt(bitPattern: valueCopy.baseAddress),
        lowWord: UInt(truncatingIfNeeded: valueCopy.count)
      )
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> UnsafeMutableBufferPointer<Element> {
    let wp = WordPair.decodeAtomicRepresentation(representation)

    return UnsafeMutableBufferPointer<Element>(
      start: UnsafeMutablePointer<Element>(bitPattern: wp.highWord),
      count: Int(truncatingIfNeeded: wp.lowWord)
    )
  }
}

#endif

//===----------------------------------------------------------------------===//
// UnsafeRawBufferPointer AtomicValue conformance
//===----------------------------------------------------------------------===//

#if (_pointerBitWidth(_32) && _hasAtomicBitWidth(_64)) || (_pointerBitWidth(_64) && _hasAtomicBitWidth(_128))

@available(SwiftStdlib 5.10, *)
extension UnsafeRawBufferPointer: AtomicValue {
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicRepresentation = WordPair.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming UnsafeRawBufferPointer
  ) -> AtomicRepresentation {
    let valueCopy = value

    return WordPair.encodeAtomicRepresentation(
      WordPair(
        highWord: UInt(bitPattern: valueCopy.baseAddress),
        lowWord: UInt(truncatingIfNeeded: valueCopy.count)
      )
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> UnsafeRawBufferPointer {
    let wp = WordPair.decodeAtomicRepresentation(representation)

    return UnsafeRawBufferPointer(
      start: UnsafeRawPointer(bitPattern: wp.highWord),
      count: Int(truncatingIfNeeded: wp.lowWord)
    )
  }
}

#endif

//===----------------------------------------------------------------------===//
// UnsafeMutableRawBufferPointer AtomicValue conformance
//===----------------------------------------------------------------------===//

#if (_pointerBitWidth(_32) && _hasAtomicBitWidth(_64)) || (_pointerBitWidth(_64) && _hasAtomicBitWidth(_128))

@available(SwiftStdlib 5.10, *)
extension UnsafeMutableRawBufferPointer: AtomicValue {
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicRepresentation = WordPair.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming UnsafeMutableRawBufferPointer
  ) -> AtomicRepresentation {
    let valueCopy = value

    return WordPair.encodeAtomicRepresentation(
      WordPair(
        highWord: UInt(bitPattern: valueCopy.baseAddress),
        lowWord: UInt(truncatingIfNeeded: valueCopy.count)
      )
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> UnsafeMutableRawBufferPointer {
    let wp = WordPair.decodeAtomicRepresentation(representation)

    return UnsafeMutableRawBufferPointer(
      start: UnsafeMutableRawPointer(bitPattern: wp.highWord),
      count: Int(truncatingIfNeeded: wp.lowWord)
    )
  }
}

#endif
