//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A type that can be hashed into a `Hasher` to produce an integer hash value.
///
/// You can use any type that conforms to the `Hashable` protocol in a set or
/// as a dictionary key. Many types in the standard library conform to
/// `Hashable`: Strings, integers, floating-point and Boolean values, and even
/// sets are hashable by default.
public protocol Hashable: Equatable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// Implement this method to conform to the `Hashable` protocol. The
  /// components used for hashing must be the same as the components compared
  /// in your type's `==` operator implementation. Call `hasher.combine(_:)`
  /// with each of these components.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  func hash(into hasher: inout Hasher)

  /// The hash value.
  ///
  /// Hash values are not guaranteed to be equal across different executions of
  /// your program. Do not save hash values to use during a future execution.
  var hashValue: Int { get }
}

extension Hashable {
  /// The hash value.
  public var hashValue: Int {
    var hasher = Hasher()
    hash(into: &hasher)
    return hasher.finalize()
  }
}

/// The universal hash function used by `Set` and `Dictionary`.
@frozen
public struct Hasher {
  @usableFromInline
  internal var _state: _HasherState

  /// Initialize a new hasher.
  @inlinable
  public init() {
    _state = _HasherState()
  }

  /// Adds the given value to this hasher, mixing its essential parts into the
  /// hasher state.
  @inlinable
  public mutating func combine<H>(_ value: H) where H : Hashable {
    value.hash(into: &self)
  }

  /// Adds the given value to this hasher.
  @inlinable
  public mutating func combine(_ value: Int) {
    _state.combine(UInt(bitPattern: value))
  }

  /// Adds the given value to this hasher.
  @inlinable
  public mutating func combine(_ value: UInt) {
    _state.combine(value)
  }

  /// Adds the given value to this hasher.
  @inlinable
  public mutating func combine(_ value: UInt8) {
    _state.combine(UInt(value))
  }

  /// Finalizes the hasher and returns the hash value.
  @inlinable
  public __consuming func finalize() -> Int {
    return _state.finalize()
  }
}

// MARK: - Internal hasher state

@usableFromInline
internal struct _HasherState {
  @usableFromInline
  internal var _buffer: [UInt8] = []

  @inlinable
  internal init() {}

  @inlinable
  internal mutating func combine(_ value: UInt) {
    withUnsafeBytes(of: value) { bytes in
      _buffer.append(contentsOf: bytes)
    }
  }

  @inlinable
  internal func finalize() -> Int {
    // Simple hash implementation (djb2 algorithm)
    var hash = 5381
    for byte in _buffer {
      hash = ((hash << 5) &+ hash) &+ Int(byte)
    }
    return hash
  }
}