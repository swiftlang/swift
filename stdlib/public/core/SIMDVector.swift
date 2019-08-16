//===--- SIMDVector.swift -------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

infix operator .==: ComparisonPrecedence
infix operator .!=: ComparisonPrecedence
infix operator .<: ComparisonPrecedence
infix operator .<=: ComparisonPrecedence
infix operator .>: ComparisonPrecedence
infix operator .>=: ComparisonPrecedence

infix operator .&: LogicalConjunctionPrecedence
infix operator .^: LogicalDisjunctionPrecedence
infix operator .|: LogicalDisjunctionPrecedence
infix operator .&=: AssignmentPrecedence
infix operator .^=: AssignmentPrecedence
infix operator .|=: AssignmentPrecedence
prefix operator .!

// A `Never`-like type for default implementations and associated types.
@frozen
public enum _SIMDNever: Hashable {}

extension _SIMDNever: Codable {
  public func encode(to encoder: Encoder) throws {
    switch self {}
  }

  public init(from decoder: Decoder) throws {
    fatalError("\(Self.self) cannot be instantiated")
  }
}

// A `Never`-like type for default implementations that provides a `SIMDScalar`
// for use in `where` requirements.
//
// This is necessary for the following code to properly compile:
//
//     extension _SIMDGenericNever: SIMDStorage {
//       public typealias _InnerStorage = _SIMDGenericNever<Scalar>
//       ...
//     }
//
// Given that we want `_InnerStorage.Scalar` to equal specific `Scalar` values
// to appease the type checker. If one does not need this functionality, just
// pass `_SIMDNever` as the generic parameter since it is a `SIMDScalar`.
@frozen
public enum _SIMDGenericNever<Scalar: Codable & Hashable>: Hashable {}

extension _SIMDGenericNever: Codable {
  public func encode(to encoder: Encoder) throws {
    switch self {}
  }

  public init(from decoder: Decoder) throws {
    fatalError("\(Self.self) cannot be instantiated")
  }
}

/// A type that can function as storage for a SIMD vector type.
///
/// The `SIMDStorage` protocol defines a storage layout and provides
/// elementwise accesses. Computational operations are defined on the `SIMD`
/// protocol, which refines this protocol, and on the concrete types that
/// conform to `SIMD`.
public protocol SIMDStorage {
  /// The type of scalars in the vector space.
  associatedtype Scalar: Codable, Hashable
  
  associatedtype _InnerStorage: SIMDStorage = _SIMDGenericNever<Self.Scalar> where _InnerStorage.Scalar == Self.Scalar
  
  // Indicates whether `InnerStorage` is represented by a vector that can be
  // passed to LLVM via a polymorphic builtin.
  static var _hasVectorRepresentation: Bool { get }
  
  var _innerStorage: _InnerStorage { get set }
  
  /// The number of scalars, or elements, in the vector.
  var scalarCount: Int { get }
  
  static func _add(_ lhs: Self, _ rhs: Self) -> Self
  static func _and(_ lhs: Self, _ rhs: Self) -> Self
  static func _or(_ lhs: Self, _ rhs: Self) -> Self
  static func _mul(_ lhs: Self, _ rhs: Self) -> Self
  static func _div(_ lhs: Self, _ rhs: Self) -> Self
  // TODO: Consider adding interface for `Builtin.generic_divExact`
  // static func _divExact(_ lhs: Self, _ rhs: Self) -> Self
  static func _rem(_ lhs: Self, _ rhs: Self) -> Self
  static func _shl(_ lhs: Self, _ rhs: Self) -> Self
  static func _shr(_ lhs: Self, _ rhs: Self) -> Self
  static func _sub(_ lhs: Self, _ rhs: Self) -> Self
  static func _xor(_ lhs: Self, _ rhs: Self) -> Self
  
  /// Creates a vector with zero in all lanes.
  init()
  
  init(_innerStorage: _InnerStorage)
  
  /// Accesses the element at the specified index.
  ///
  /// - Parameter index: The index of the element to access. `index` must be in
  ///   the range `0..<scalarCount`.
  subscript(index: Int) -> Scalar { get set }
}

extension SIMDStorage {
  public static var _hasVectorRepresentation: Bool {
    @_transparent
    get { return false }
  }

  public static var _hasVectorOperations: Bool {
    @_transparent 
    get {
      return _hasVectorRepresentation && _isConcrete(Self.self)
    }
  }
  
  /// The number of scalars, or elements, in a vector of this type.
  @_alwaysEmitIntoClient
  public static var scalarCount: Int {
    // Wouldn't it make more sense to define the instance var in terms of the
    // static var? Yes, probably, but by doing it this way we make the static
    // var backdeployable.
    return Self().scalarCount
  }
  
  public var _innerStorage: _InnerStorage {
    get {
      fatalError("In default SIMDStorage impl for \(Self.self)")
    }
    set {
      fatalError("In default SIMDStorage impl for \(Self.self)")
    }
  }
  
  public init(_innerStorage: _InnerStorage) {
    self.init()
    self._innerStorage = _innerStorage
  }
}

extension SIMDStorage {
  public static func _add(_ lhs: Self, _ rhs: Self) -> Self {
    fatalError("In default SIMDStorage impl for \(Self.self)")
  }
  
  public static func _and(_ lhs: Self, _ rhs: Self) -> Self {
    fatalError("In default SIMDStorage impl for \(Self.self)")
  }
  
  public static func _or(_ lhs: Self, _ rhs: Self) -> Self {
    fatalError("In default SIMDStorageWithOps impl for \(Self.self)")
  }
  
  public static func _mul(_ lhs: Self, _ rhs: Self) -> Self {
    fatalError("In default SIMDStorageWithOps impl for \(Self.self)")
  }
  
  public static func _div(_ lhs: Self, _ rhs: Self) -> Self {
    fatalError("In default SIMDStorageWithOps impl for \(Self.self)")
  }
  
  public static func _rem(_ lhs: Self, _ rhs: Self) -> Self {
    fatalError("In default SIMDStorageWithOps impl for \(Self.self)")
  }
  
  public static func _shl(_ lhs: Self, _ rhs: Self) -> Self {
    fatalError("In default SIMDStorageWithOps impl for \(Self.self)")
  }
  
  public static func _shr(_ lhs: Self, _ rhs: Self) -> Self {
    fatalError("In default SIMDStorageWithOps impl for \(Self.self)")
  }
  
  public static func _sub(_ lhs: Self, _ rhs: Self) -> Self {
    fatalError("In default SIMDStorageWithOps impl for \(Self.self)")
  }
  
  public static func _xor(_ lhs: Self, _ rhs: Self) -> Self {
    fatalError("In default SIMDStorageWithOps impl for \(Self.self)")
  }
}

extension _SIMDNever: SIMDStorage {
  public typealias Scalar = _SIMDNever
  public typealias _InnerStorage = _SIMDGenericNever<Self.Scalar>

  public static var scalarCount: Int {
    @_transparent
    get { switch Self() {} }
  }

  public var scalarCount: Int {
    @_transparent
    get { switch self {} }
  }

  public subscript(index: Int) -> Scalar {
    @_transparent
    get { switch self {} }
    set {}
  }

  @_transparent
  public init() {
    fatalError("\(Self.self) cannot be instantiated")
  }
}

extension _SIMDGenericNever: SIMDStorage {
  public typealias _InnerStorage = _SIMDGenericNever<Scalar>

  public static var scalarCount: Int {
    @_transparent
    get { switch Self() {} }
  }

  public var scalarCount: Int {
    @_transparent
    get { switch self {} }
  }

  public subscript(index: Int) -> Scalar {
    @_transparent
    get { switch self {} }
    set {}
  }

  @_transparent
  public init() {
    fatalError("\(Self.self) cannot be instantiated")
  }
}

public protocol _SIMDVectorStorage: SIMDStorage {
  // Must be a builtin type by IRGen time.
  associatedtype _Vector

  var _vector: _Vector { get }

  init(_vector: _Vector)
}

extension _SIMDVectorStorage where Scalar: SignedInteger {
  @_transparent
  public static func _add(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_add(lhs._vector, rhs._vector))
  }

  @_transparent
  public static func _div(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_sdiv(lhs._vector, rhs._vector))
  }

  @_transparent
  public static func _rem(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_srem(lhs._vector, rhs._vector))
  }
  
  @_transparent
  public static func _shr(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_ashr(lhs._vector, rhs._vector))
  }
}

extension _SIMDVectorStorage where Scalar: UnsignedInteger {
  @_transparent
  public static func _add(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_add(lhs._vector, rhs._vector))
  }

  @_transparent
  public static func _div(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_udiv(lhs._vector, rhs._vector))
  }

  @_transparent
  public static func _rem(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_urem(lhs._vector, rhs._vector))
  }
  
  @_transparent
  public static func _shr(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_lshr(lhs._vector, rhs._vector))
  }
}

extension _SIMDVectorStorage where Scalar: BinaryFloatingPoint {
  @_transparent
  public static func _add(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_fadd(lhs._vector, rhs._vector))
  }

  @_transparent
  public static func _div(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_fdiv(lhs._vector, rhs._vector))
  }

  @_transparent
  public static func _rem(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_frem(lhs._vector, rhs._vector))
  }
}

extension _SIMDVectorStorage {
  @_transparent
  public static func _and(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_and(lhs._vector, rhs._vector))
  }

  @_transparent
  public static func _or(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_or(lhs._vector, rhs._vector))
  }

  @_transparent
  public static func _mul(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_mul(lhs._vector, rhs._vector))
  }

  @_transparent
  public static func _shl(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_shl(lhs._vector, rhs._vector))
  }

  @_transparent
  public static func _sub(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_sub(lhs._vector, rhs._vector))
  }

  @_transparent
  public static func _xor(_ lhs: Self, _ rhs: Self) -> Self {
    return Self(_vector: Builtin.generic_xor(lhs._vector, rhs._vector))
  }
}

/// A type that can be used as an element in a SIMD vector.
public protocol SIMDScalar {
  associatedtype SIMDMaskScalar: SIMDScalar & FixedWidthInteger & SignedInteger
  associatedtype SIMD2Storage: SIMDStorage where SIMD2Storage.Scalar == Self
  associatedtype SIMD4Storage: SIMDStorage where SIMD4Storage.Scalar == Self
  associatedtype SIMD8Storage: SIMDStorage where SIMD8Storage.Scalar == Self
  associatedtype SIMD16Storage: SIMDStorage where SIMD16Storage.Scalar == Self
  associatedtype SIMD32Storage: SIMDStorage where SIMD32Storage.Scalar == Self
  associatedtype SIMD64Storage: SIMDStorage where SIMD64Storage.Scalar == Self
}

extension _SIMDNever: SIMDScalar {
  public typealias SIMDMaskScalar = Int8
  public typealias SIMD2Storage = _SIMDNever
  public typealias SIMD4Storage = _SIMDNever
  public typealias SIMD8Storage = _SIMDNever
  public typealias SIMD16Storage = _SIMDNever
  public typealias SIMD32Storage = _SIMDNever
  public typealias SIMD64Storage = _SIMDNever
}

extension _SIMDGenericNever: SIMDScalar {
  public typealias SIMDMaskScalar = Int8
  public typealias SIMD2Storage = _SIMDGenericNever<_SIMDGenericNever<Scalar>>
  public typealias SIMD4Storage = _SIMDGenericNever<_SIMDGenericNever<Scalar>>
  public typealias SIMD8Storage = _SIMDGenericNever<_SIMDGenericNever<Scalar>>
  public typealias SIMD16Storage = _SIMDGenericNever<_SIMDGenericNever<Scalar>>
  public typealias SIMD32Storage = _SIMDGenericNever<_SIMDGenericNever<Scalar>>
  public typealias SIMD64Storage = _SIMDGenericNever<_SIMDGenericNever<Scalar>>
}

/// A SIMD vector of a fixed number of elements.
public protocol SIMD: SIMDStorage,
                      Codable,
                      Hashable,
                      CustomStringConvertible,
                      ExpressibleByArrayLiteral {
  /// The mask type resulting from pointwise comparisons of this vector type.
  associatedtype MaskStorage: SIMD
    where MaskStorage.Scalar: FixedWidthInteger & SignedInteger
}

extension SIMD {
  public static var _hasVectorRepresentation: Bool {
    @_transparent
    get {
      return Self._InnerStorage._hasVectorRepresentation
    }
  }

  /// The valid indices for subscripting the vector.
  @_transparent
  public var indices: Range<Int> {
    return 0 ..< scalarCount
  }
  
  /// A vector with the specified value in all lanes.
  @_transparent
  public init(repeating value: Scalar) {
    self.init()
    for i in indices { self[i] = value }
  }
  
  /// Returns a Boolean value indicating whether two vectors are equal.
  @_transparent
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    var result = true
    for i in lhs.indices { result = result && lhs[i] == rhs[i] }
    return result
  }
  
  /// Hashes the elements of the vector using the given hasher.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    for i in indices { hasher.combine(self[i]) }
  }
  
  /// Encodes the scalars of this vector into the given encoder in an unkeyed
  /// container.
  ///
  /// This function throws an error if any values are invalid for the given
  /// encoder's format.
  ///
  /// - Parameter encoder: The encoder to write data to.
  public func encode(to encoder: Encoder) throws {
    var container = encoder.unkeyedContainer()
    for i in indices {
      try container.encode(self[i])
    }
  }
  
  /// Creates a new vector by decoding scalars from the given decoder.
  ///
  /// This initializer throws an error if reading from the decoder fails, or
  /// if the data read is corrupted or otherwise invalid.
  ///
  /// - Parameter decoder: The decoder to read data from.
  public init(from decoder: Decoder) throws {
    self.init()
    var container = try decoder.unkeyedContainer()
    guard container.count == scalarCount else {
      throw DecodingError.dataCorrupted(
        DecodingError.Context(
          codingPath: decoder.codingPath,
          debugDescription: "Expected vector with exactly \(scalarCount) elements."
        )
      )
    }
    for i in indices {
      self[i] = try container.decode(Scalar.self)
    }
  }
  
  /// A textual description of the vector.
  public var description: String {
    get {
      return "\(Self.self)(" + indices.map({"\(self[$0])"}).joined(separator: ", ") + ")"
    }
  }
  
  /// Returns a vector mask with the result of a pointwise equality comparison.
  @_transparent
  public static func .==(lhs: Self, rhs: Self) -> SIMDMask<MaskStorage> {
    var result = SIMDMask<MaskStorage>()
    for i in result.indices { result[i] = lhs[i] == rhs[i] }
    return result
  }
  
  /// Returns a vector mask with the result of a pointwise inequality
  /// comparison.
  @_transparent
  public static func .!=(lhs: Self, rhs: Self) -> SIMDMask<MaskStorage> {
    var result = SIMDMask<MaskStorage>()
    for i in result.indices { result[i] = lhs[i] != rhs[i] }
    return result
  }
  
  /// Replaces elements of this vector with elements of `other` in the lanes
  /// where `mask` is `true`.
  @_transparent
  public mutating func replace(with other: Self, where mask: SIMDMask<MaskStorage>) {
    for i in indices { self[i] = mask[i] ? other[i] : self[i] }
  }
  
  /// Creates a vector from the specified elements.
  ///
  /// - Parameter scalars: The elements to use in the vector. `scalars` must
  ///   have the same number of elements as the vector type.
  @inlinable
  public init(arrayLiteral scalars: Scalar...) {
    self.init(scalars)
  }
  
  /// Creates a vector from the given sequence.
  ///
  /// - Precondition: `scalars` must have the same number of elements as the
  ///   vector type.
  ///
  /// - Parameter scalars: The elements to use in the vector.
  @inlinable
  public init<S: Sequence>(_ scalars: S) where S.Element == Scalar {
    self.init()
    var index = 0
    for scalar in scalars {
      if index == scalarCount {
        _preconditionFailure("Too many elements in sequence.")
      }
      self[index] = scalar
      index += 1
    }
    if index < scalarCount {
      _preconditionFailure("Not enough elements in sequence.")
    }
  }
  
  /// Extracts the scalars at specified indices to form a SIMD2.
  ///
  /// The elements of the index vector are wrapped modulo the count of elements
  /// in this vector. Because of this, the index is always in-range and no trap
  /// can occur.
  @_alwaysEmitIntoClient
  public subscript<Index>(index: SIMD2<Index>) -> SIMD2<Scalar>
  where Index: FixedWidthInteger {
    var result = SIMD2<Scalar>()
    for i in result.indices {
      result[i] = self[Int(index[i]) % scalarCount]
    }
    return result
  }
  
  /// Extracts the scalars at specified indices to form a SIMD3.
  ///
  /// The elements of the index vector are wrapped modulo the count of elements
  /// in this vector. Because of this, the index is always in-range and no trap
  /// can occur.
  @_alwaysEmitIntoClient
  public subscript<Index>(index: SIMD3<Index>) -> SIMD3<Scalar>
  where Index: FixedWidthInteger {
    var result = SIMD3<Scalar>()
    for i in result.indices {
      result[i] = self[Int(index[i]) % scalarCount]
    }
    return result
  }
  
  /// Extracts the scalars at specified indices to form a SIMD4.
  ///
  /// The elements of the index vector are wrapped modulo the count of elements
  /// in this vector. Because of this, the index is always in-range and no trap
  /// can occur.
  @_alwaysEmitIntoClient
  public subscript<Index>(index: SIMD4<Index>) -> SIMD4<Scalar>
  where Index: FixedWidthInteger {
    var result = SIMD4<Scalar>()
    for i in result.indices {
      result[i] = self[Int(index[i]) % scalarCount]
    }
    return result
  }
  
  /// Extracts the scalars at specified indices to form a SIMD8.
  ///
  /// The elements of the index vector are wrapped modulo the count of elements
  /// in this vector. Because of this, the index is always in-range and no trap
  /// can occur.
  @_alwaysEmitIntoClient
  public subscript<Index>(index: SIMD8<Index>) -> SIMD8<Scalar>
  where Index: FixedWidthInteger {
    var result = SIMD8<Scalar>()
    for i in result.indices {
      result[i] = self[Int(index[i]) % scalarCount]
    }
    return result
  }
  
  /// Extracts the scalars at specified indices to form a SIMD16.
  ///
  /// The elements of the index vector are wrapped modulo the count of elements
  /// in this vector. Because of this, the index is always in-range and no trap
  /// can occur.
  @_alwaysEmitIntoClient
  public subscript<Index>(index: SIMD16<Index>) -> SIMD16<Scalar>
  where Index: FixedWidthInteger {
    var result = SIMD16<Scalar>()
    for i in result.indices {
      result[i] = self[Int(index[i]) % scalarCount]
    }
    return result
  }
  
  /// Extracts the scalars at specified indices to form a SIMD32.
  ///
  /// The elements of the index vector are wrapped modulo the count of elements
  /// in this vector. Because of this, the index is always in-range and no trap
  /// can occur.
  @_alwaysEmitIntoClient
  public subscript<Index>(index: SIMD32<Index>) -> SIMD32<Scalar>
  where Index: FixedWidthInteger {
    var result = SIMD32<Scalar>()
    for i in result.indices {
      result[i] = self[Int(index[i]) % scalarCount]
    }
    return result
  }
  
  /// Extracts the scalars at specified indices to form a SIMD64.
  ///
  /// The elements of the index vector are wrapped modulo the count of elements
  /// in this vector. Because of this, the index is always in-range and no trap
  /// can occur.
  @_alwaysEmitIntoClient
  public subscript<Index>(index: SIMD64<Index>) -> SIMD64<Scalar>
  where Index: FixedWidthInteger {
    var result = SIMD64<Scalar>()
    for i in result.indices {
      result[i] = self[Int(index[i]) % scalarCount]
    }
    return result
  }
}

//  Implementations of comparison operations. These should eventually all
//  be replaced with @_semantics to lower directly to vector IR nodes.
extension SIMD where Scalar: Comparable {
  /// Returns a vector mask with the result of a pointwise less than
  /// comparison.
  @_transparent
  public static func .<(lhs: Self, rhs: Self) -> SIMDMask<MaskStorage> {
    var result = SIMDMask<MaskStorage>()
    for i in result.indices { result[i] = lhs[i] < rhs[i] }
    return result
  }
  
  /// Returns a vector mask with the result of a pointwise less than or equal
  /// comparison.
  @_transparent
  public static func .<=(lhs: Self, rhs: Self) -> SIMDMask<MaskStorage> {
    var result = SIMDMask<MaskStorage>()
    for i in result.indices { result[i] = lhs[i] <= rhs[i] }
    return result
  }
  
  /// The least element in the vector.
  @_alwaysEmitIntoClient
  public func min() -> Scalar {
    return indices.reduce(into: self[0]) { $0 = Swift.min($0, self[$1]) }
  }
  
  /// The greatest element in the vector.
  @_alwaysEmitIntoClient
  public func max() -> Scalar {
    return indices.reduce(into: self[0]) { $0 = Swift.max($0, self[$1]) }
  }
}

//  These operations should never need @_semantics; they should be trivial
//  wrappers around the core operations defined above.
extension SIMD {
  /// Returns a vector mask with the result of a pointwise equality comparison.
  @_transparent
  public static func .==(lhs: Scalar, rhs: Self) -> SIMDMask<MaskStorage> {
    return Self(repeating: lhs) .== rhs
  }

  /// Returns a vector mask with the result of a pointwise inequality comparison.
  @_transparent
  public static func .!=(lhs: Scalar, rhs: Self) -> SIMDMask<MaskStorage> {
    return Self(repeating: lhs) .!= rhs
  }

  /// Returns a vector mask with the result of a pointwise equality comparison.
  @_transparent
  public static func .==(lhs: Self, rhs: Scalar) -> SIMDMask<MaskStorage> {
    return lhs .== Self(repeating: rhs)
  }

  /// Returns a vector mask with the result of a pointwise inequality comparison.
  @_transparent
  public static func .!=(lhs: Self, rhs: Scalar) -> SIMDMask<MaskStorage> {
    return lhs .!= Self(repeating: rhs)
  }
  
  /// Replaces elements of this vector with `other` in the lanes where `mask`
  /// is `true`.
  @_transparent
  public mutating func replace(with other: Scalar, where mask: SIMDMask<MaskStorage>) {
    replace(with: Self(repeating: other), where: mask)
  }
  
  /// Returns a copy of this vector, with elements replaced by elements of
  /// `other` in the lanes where `mask` is `true`.
  @_transparent
  public func replacing(with other: Self, where mask: SIMDMask<MaskStorage>) -> Self {
    var result = self
    result.replace(with: other, where: mask)
    return result
  }
  
  /// Returns a copy of this vector, with elements `other` in the lanes where
  /// `mask` is `true`.
  @_transparent
  public func replacing(with other: Scalar, where mask: SIMDMask<MaskStorage>) -> Self {
    return replacing(with: Self(repeating: other), where: mask)
  }
}

extension SIMD where Scalar: Comparable {
  /// Returns a vector mask with the result of a pointwise greater than or
  /// equal comparison.
  @_transparent
  public static func .>=(lhs: Self, rhs: Self) -> SIMDMask<MaskStorage> {
    return rhs .<= lhs
  }

  /// Returns a vector mask with the result of a pointwise greater than
  /// comparison.
  @_transparent
  public static func .>(lhs: Self, rhs: Self) -> SIMDMask<MaskStorage> {
    return rhs .< lhs
  }

  /// Returns a vector mask with the result of a pointwise less than comparison.
  @_transparent
  public static func .<(lhs: Scalar, rhs: Self) -> SIMDMask<MaskStorage> {
    return Self(repeating: lhs) .< rhs
  }

  /// Returns a vector mask with the result of a pointwise less than or equal
  /// comparison.
  @_transparent
  public static func .<=(lhs: Scalar, rhs: Self) -> SIMDMask<MaskStorage> {
    return Self(repeating: lhs) .<= rhs
  }

  /// Returns a vector mask with the result of a pointwise greater than or
  /// equal comparison.
  @_transparent
  public static func .>=(lhs: Scalar, rhs: Self) -> SIMDMask<MaskStorage> {
    return Self(repeating: lhs) .>= rhs
  }

  /// Returns a vector mask with the result of a pointwise greater than
  /// comparison.
  @_transparent
  public static func .>(lhs: Scalar, rhs: Self) -> SIMDMask<MaskStorage> {
    return Self(repeating: lhs) .> rhs
  }

  /// Returns a vector mask with the result of a pointwise less than comparison.
  @_transparent
  public static func .<(lhs: Self, rhs: Scalar) -> SIMDMask<MaskStorage> {
    return lhs .< Self(repeating: rhs)
  }

  /// Returns a vector mask with the result of a pointwise less than or equal
  /// comparison.
  @_transparent
  public static func .<=(lhs: Self, rhs: Scalar) -> SIMDMask<MaskStorage> {
    return lhs .<= Self(repeating: rhs)
  }
  
  /// Returns a vector mask with the result of a pointwise greater than or
  /// equal comparison.
  @_transparent
  public static func .>=(lhs: Self, rhs: Scalar) -> SIMDMask<MaskStorage> {
    return lhs .>= Self(repeating: rhs)
  }
  
  /// Returns a vector mask with the result of a pointwise greater than
  /// comparison.
  @_transparent
  public static func .>(lhs: Self, rhs: Scalar) -> SIMDMask<MaskStorage> {
    return lhs .> Self(repeating: rhs)
  }
  
  @_alwaysEmitIntoClient
  public mutating func clamp(lowerBound: Self, upperBound: Self) {
    self = self.clamped(lowerBound: lowerBound, upperBound: upperBound)
  }

  @_alwaysEmitIntoClient
  public func clamped(lowerBound: Self, upperBound: Self) -> Self {
    return pointwiseMin(upperBound, pointwiseMax(lowerBound, self))
  }
}

extension SIMD where Scalar: FixedWidthInteger {
  /// A vector with zero in all lanes.
  @_transparent
  public static var zero: Self {
    return Self()
  }
  
  /// A vector with one in all lanes.
  @_alwaysEmitIntoClient
  public static var one: Self {
    return Self(repeating: 1)
  }
  
  /// Returns a vector with random values from within the specified range in
  /// all lanes, using the given generator as a source for randomness.
  @inlinable
  public static func random<T: RandomNumberGenerator>(
    in range: Range<Scalar>,
    using generator: inout T
  ) -> Self {
    var result = Self()
    for i in result.indices {
      result[i] = Scalar.random(in: range, using: &generator)
    }
    return result
  }
  
  /// Returns a vector with random values from within the specified range in
  /// all lanes.
  @inlinable
  public static func random(in range: Range<Scalar>) -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(in: range, using: &g)
  }
  
  /// Returns a vector with random values from within the specified range in
  /// all lanes, using the given generator as a source for randomness.
  @inlinable
  public static func random<T: RandomNumberGenerator>(
    in range: ClosedRange<Scalar>,
    using generator: inout T
  ) -> Self {
    var result = Self()
    for i in result.indices {
      result[i] = Scalar.random(in: range, using: &generator)
    }
    return result
  }
  
  /// Returns a vector with random values from within the specified range in
  /// all lanes.
  @inlinable
  public static func random(in range: ClosedRange<Scalar>) -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(in: range, using: &g)
  }
}

extension SIMD where Scalar: FloatingPoint {
  /// A vector with zero in all lanes.
  @_transparent
  public static var zero: Self {
    return Self()
  }
  
  /// A vector with one in all lanes.
  @_alwaysEmitIntoClient
  public static var one: Self {
    return Self(repeating: 1)
  }
  
  @_alwaysEmitIntoClient
  public mutating func clamp(lowerBound: Self, upperBound: Self) {
    self = self.clamped(lowerBound: lowerBound, upperBound: upperBound)
  }

  @_alwaysEmitIntoClient
  public func clamped(lowerBound: Self, upperBound: Self) -> Self {
    return pointwiseMin(upperBound, pointwiseMax(lowerBound, self))
  }
}

extension SIMD
where Scalar: BinaryFloatingPoint, Scalar.RawSignificand: FixedWidthInteger {
  /// Returns a vector with random values from within the specified range in
  /// all lanes, using the given generator as a source for randomness.
  @inlinable
  public static func random<T: RandomNumberGenerator>(
    in range: Range<Scalar>,
    using generator: inout T
  ) -> Self {
    var result = Self()
    for i in result.indices {
      result[i] = Scalar.random(in: range, using: &generator)
    }
    return result
  }
  
  /// Returns a vector with random values from within the specified range in
  /// all lanes.
  @inlinable
  public static func random(in range: Range<Scalar>) -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(in: range, using: &g)
  }
  
  /// Returns a vector with random values from within the specified range in
  /// all lanes, using the given generator as a source for randomness.
  @inlinable
  public static func random<T: RandomNumberGenerator>(
    in range: ClosedRange<Scalar>,
    using generator: inout T
  ) -> Self {
    var result = Self()
    for i in result.indices {
      result[i] = Scalar.random(in: range, using: &generator)
    }
    return result
  }
  
  /// Returns a vector with random values from within the specified range in
  /// all lanes.
  @inlinable
  public static func random(in range: ClosedRange<Scalar>) -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(in: range, using: &g)
  }
}

@frozen
public struct SIMDMask<Storage>: SIMD
                  where Storage: SIMD,
                 Storage.Scalar: FixedWidthInteger & SignedInteger {
  
  public var _storage: Storage
  
  public typealias MaskStorage = Storage
  
  public typealias Scalar = Bool
  
  @_transparent
  public var scalarCount: Int {
    return _storage.scalarCount
  }
  
  @_transparent
  public init() {
    _storage = Storage()
  }
  
  @_transparent
  public init(_ _storage: Storage) {
    self._storage = _storage
  }
  
  public subscript(index: Int) -> Bool {
    @_transparent
    get {
      _precondition(indices.contains(index))
      return _storage[index] < 0
    }
    @_transparent
    set {
      _precondition(indices.contains(index))
      _storage[index] = newValue ? -1 : 0
    }
  }
}

extension SIMDMask {
  /// Returns a vector mask with `true` or `false` randomly assigned in each
  /// lane, using the given generator as a source for randomness.
  @inlinable
  public static func random<T: RandomNumberGenerator>(using generator: inout T) -> SIMDMask {
    var result = SIMDMask()
    for i in result.indices { result[i] = Bool.random(using: &generator) }
    return result
  }
  
  /// Returns a vector mask with `true` or `false` randomly assigned in each
  /// lane.
  @inlinable
  public static func random() -> SIMDMask {
    var g = SystemRandomNumberGenerator()
    return SIMDMask.random(using: &g)
  }
}

//  Implementations of integer operations. These should eventually all
//  be replaced with @_semantics to lower directly to vector IR nodes.
extension SIMD where Scalar: FixedWidthInteger {
  @_transparent
  public var leadingZeroBitCount: Self {
    var result = Self()
    for i in indices { result[i] = Scalar(self[i].leadingZeroBitCount) }
    return result
  }
  
  @_transparent
  public var trailingZeroBitCount: Self {
    var result = Self()
    for i in indices { result[i] = Scalar(self[i].trailingZeroBitCount) }
    return result
  }
  
  @_transparent
  public var nonzeroBitCount: Self {
    var result = Self()
    for i in indices { result[i] = Scalar(self[i].nonzeroBitCount) }
    return result
  }
  
  @_transparent
  public static prefix func ~(rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = ~rhs[i] }
    return result
  }
  
  /// Returns the sum of the scalars in the vector, computed with wrapping
  /// addition.
  ///
  /// Equivalent to indices.reduce(into: 0) { $0 &+= self[$1] }.
  @_alwaysEmitIntoClient
  public func wrappedSum() -> Scalar {
    return indices.reduce(into: 0) { $0 &+= self[$1] }
  }
}

// Binary integer operations; (T, T) -> T
//
// These functions will defer to the operations defined on `_InnerStorage` if
// `_hasVectorOperations == true`. They will then call the relevant polymorphic
// builtin functions.
//
// The `_fastPath` hint is used because these functions will be most often
// called on stdlib SIMD types rather than externally-defined types.
extension SIMD where Scalar: FixedWidthInteger {
  @_transparent
  public static func &(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self.init(_innerStorage: Self._InnerStorage._and(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] & rhs[i] }
    return result
  }
  
  @_transparent
  public static func ^(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self.init(_innerStorage: Self._InnerStorage._xor(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] ^ rhs[i] }
    return result
  }
  
  @_transparent
  public static func |(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self.init(_innerStorage: Self._InnerStorage._or(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] | rhs[i] }
    return result
  }
  
  @_transparent
  public static func &<<(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self.init(_innerStorage: Self._InnerStorage._shl(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &<< rhs[i] }
    return result
  }
  
  @_transparent
  public static func &>>(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self.init(_innerStorage: Self._InnerStorage._shr(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &>> rhs[i] }
    return result
  }
  
  @_transparent
  public static func &+(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self(_innerStorage: Self._InnerStorage._add(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &+ rhs[i] }
    return result
  }
  
  @_transparent
  public static func &-(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self(_innerStorage: Self._InnerStorage._sub(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &- rhs[i] }
    return result
  }
  
  @_transparent
  public static func &*(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self(_innerStorage: Self._InnerStorage._mul(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &* rhs[i] }
    return result
  }
  
  @_transparent
  public static func /(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self(_innerStorage: Self._InnerStorage._div(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] / rhs[i] }
    return result
  }
  
  @_transparent
  public static func %(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self(_innerStorage: Self._InnerStorage._rem(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] % rhs[i] }
    return result
  }
}

//  Implementations of floating-point operations. These should eventually all
//  be replaced with @_semantics to lower directly to vector IR nodes.
extension SIMD where Scalar: FloatingPoint {
  @_transparent
  public func addingProduct(_ lhs: Self, _ rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = self[i].addingProduct(lhs[i], rhs[i]) }
    return result
  }
  
  @_transparent
  public func squareRoot() -> Self {
    var result = Self()
    for i in result.indices { result[i] = self[i].squareRoot() }
    return result
  }
  
  @_transparent
  public func rounded(_ rule: FloatingPointRoundingRule) -> Self {
    var result = Self()
    for i in result.indices { result[i] = self[i].rounded(rule) }
    return result
  }
  
  /// Returns the least scalar in the vector.
  @_alwaysEmitIntoClient
  public func min() -> Scalar {
    return indices.reduce(into: self[0]) { $0 = Scalar.minimum($0, self[$1]) }
  }
  
  /// Returns the greatest scalar in the vector.
  @_alwaysEmitIntoClient
  public func max() -> Scalar {
    return indices.reduce(into: self[0]) { $0 = Scalar.maximum($0, self[$1]) }
  }
  
  /// Returns the sum of the scalars in the vector.
  @_alwaysEmitIntoClient
  public func sum() -> Scalar {
    // Implementation note: this eventually be defined to lower to either
    // llvm.experimental.vector.reduce.fadd or an explicit tree-sum. Open-
    // coding the tree sum is problematic, we probably need to define a
    // Swift Builtin to support it.
    return indices.reduce(into: 0) { $0 += self[$1] }
  }
}

// Binary floating-point operations; (T, T) -> T
//
// These functions will defer to the operations defined on `_InnerStorage` if
// `_hasVectorOperations == true`. They will then call the relevant polymorphic
// builtin functions.
//
// The `_fastPath` hint is used because these functions will be most often
// called on stdlib SIMD types rather than externally-defined types.
extension SIMD where Scalar: FloatingPoint {
  @_transparent
  public static func +(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self(_innerStorage: Self._InnerStorage._add(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] + rhs[i] }
    return result
  }
  
  @_transparent
  public static func -(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self(_innerStorage: Self._InnerStorage._sub(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] - rhs[i] }
    return result
  }
  
  @_transparent
  public static func *(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self(_innerStorage: Self._InnerStorage._mul(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] * rhs[i] }
    return result
  }
  
  @_transparent
  public static func /(lhs: Self, rhs: Self) -> Self {
    if _fastPath(Self._hasVectorOperations) {
      return Self(_innerStorage: Self._InnerStorage._div(
        lhs._innerStorage,
        rhs._innerStorage
      ))
    }
    var result = Self()
    for i in result.indices { result[i] = lhs[i] / rhs[i] }
    return result
  }
}

extension SIMDMask {
  @_transparent
  public static prefix func .!(rhs: SIMDMask) -> SIMDMask {
    return SIMDMask(~rhs._storage)
  }
  
  @_transparent
  public static func .&(lhs: SIMDMask, rhs: SIMDMask) -> SIMDMask {
    return SIMDMask(lhs._storage & rhs._storage)
  }
  
  @_transparent
  public static func .^(lhs: SIMDMask, rhs: SIMDMask) -> SIMDMask {
    return SIMDMask(lhs._storage ^ rhs._storage)
  }
  
  @_transparent
  public static func .|(lhs: SIMDMask, rhs: SIMDMask) -> SIMDMask {
    return SIMDMask(lhs._storage | rhs._storage)
  }
}

//  These operations should never need @_semantics; they should be trivial
//  wrappers around the core operations defined above.
extension SIMD where Scalar: FixedWidthInteger {
  
  @_transparent
  public static func &(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) & rhs
  }
  
  @_transparent
  public static func ^(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) ^ rhs
  }
  
  @_transparent
  public static func |(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) | rhs
  }
  
  @_transparent
  public static func &<<(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) &<< rhs
  }
  
  @_transparent
  public static func &>>(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) &>> rhs
  }
  
  @_transparent
  public static func &+(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) &+ rhs
  }
  
  @_transparent
  public static func &-(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) &- rhs
  }
  
  @_transparent
  public static func &*(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) &* rhs
  }
  
  @_transparent
  public static func /(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) / rhs
  }
  
  @_transparent
  public static func %(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) % rhs
  }
  
  @_transparent
  public static func &(lhs: Self, rhs: Scalar) -> Self {
    return lhs & Self(repeating: rhs)
  }
  
  @_transparent
  public static func ^(lhs: Self, rhs: Scalar) -> Self {
    return lhs ^ Self(repeating: rhs)
  }
  
  @_transparent
  public static func |(lhs: Self, rhs: Scalar) -> Self {
    return lhs | Self(repeating: rhs)
  }
  
  @_transparent
  public static func &<<(lhs: Self, rhs: Scalar) -> Self {
    return lhs &<< Self(repeating: rhs)
  }
  
  @_transparent
  public static func &>>(lhs: Self, rhs: Scalar) -> Self {
    return lhs &>> Self(repeating: rhs)
  }
  
  @_transparent
  public static func &+(lhs: Self, rhs: Scalar) -> Self {
    return lhs &+ Self(repeating: rhs)
  }
  
  @_transparent
  public static func &-(lhs: Self, rhs: Scalar) -> Self {
    return lhs &- Self(repeating: rhs)
  }
  
  @_transparent
  public static func &*(lhs: Self, rhs: Scalar) -> Self {
    return lhs &* Self(repeating: rhs)
  }
  
  @_transparent
  public static func /(lhs: Self, rhs: Scalar) -> Self {
    return lhs / Self(repeating: rhs)
  }
  
  @_transparent
  public static func %(lhs: Self, rhs: Scalar) -> Self {
    return lhs % Self(repeating: rhs)
  }
  
  @_transparent
  public static func &=(lhs: inout Self, rhs: Self) {
    lhs = lhs & rhs
  }
  
  @_transparent
  public static func ^=(lhs: inout Self, rhs: Self) {
    lhs = lhs ^ rhs
  }
  
  @_transparent
  public static func |=(lhs: inout Self, rhs: Self) {
    lhs = lhs | rhs
  }
  
  @_transparent
  public static func &<<=(lhs: inout Self, rhs: Self) {
    lhs = lhs &<< rhs
  }
  
  @_transparent
  public static func &>>=(lhs: inout Self, rhs: Self) {
    lhs = lhs &>> rhs
  }
  
  @_transparent
  public static func &+=(lhs: inout Self, rhs: Self) {
    lhs = lhs &+ rhs
  }
  
  @_transparent
  public static func &-=(lhs: inout Self, rhs: Self) {
    lhs = lhs &- rhs
  }
  
  @_transparent
  public static func &*=(lhs: inout Self, rhs: Self) {
    lhs = lhs &* rhs
  }
  
  @_transparent
  public static func /=(lhs: inout Self, rhs: Self) {
    lhs = lhs / rhs
  }
  
  @_transparent
  public static func %=(lhs: inout Self, rhs: Self) {
    lhs = lhs % rhs
  }
  
  @_transparent
  public static func &=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs & rhs
  }
  
  @_transparent
  public static func ^=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs ^ rhs
  }
  
  @_transparent
  public static func |=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs | rhs
  }
  
  @_transparent
  public static func &<<=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs &<< rhs
  }
  
  @_transparent
  public static func &>>=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs &>> rhs
  }
  
  @_transparent
  public static func &+=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs &+ rhs
  }
  
  @_transparent
  public static func &-=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs &- rhs
  }
  
  @_transparent
  public static func &*=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs &* rhs
  }
  
  @_transparent
  public static func /=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs / rhs
  }
  
  @_transparent
  public static func %=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs % rhs
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&+' instead")
  public static func +(lhs: Self, rhs: Self) -> Self {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&-' instead")
  public static func -(lhs: Self, rhs: Self) -> Self {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&*' instead")
  public static func *(lhs: Self, rhs: Self) -> Self {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&+' instead")
  public static func +(lhs: Self, rhs: Scalar) -> Self {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&-' instead")
  public static func -(lhs: Self, rhs: Scalar) -> Self {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&*' instead")
  public static func *(lhs: Self, rhs: Scalar) -> Self {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&+' instead")
  public static func +(lhs: Scalar, rhs: Self) -> Self {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&-' instead")
  public static func -(lhs: Scalar, rhs: Self) -> Self {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&*' instead")
  public static func *(lhs: Scalar, rhs: Self) -> Self {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&+=' instead")
  public static func +=(lhs: inout Self, rhs: Self) {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&-=' instead")
  public static func -=(lhs: inout Self, rhs: Self) {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&*=' instead")
  public static func *=(lhs: inout Self, rhs: Self) {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&+=' instead")
  public static func +=(lhs: inout Self, rhs: Scalar) {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&-=' instead")
  public static func -=(lhs: inout Self, rhs: Scalar) {
    fatalError()
  }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&*=' instead")
  public static func *=(lhs: inout Self, rhs: Scalar) {
    fatalError()
  }
}

extension SIMD where Scalar: FloatingPoint {
  
  @_transparent
  public static prefix func -(rhs: Self) -> Self {
    return 0 - rhs
  }
  
  @_transparent
  public static func +(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) + rhs
  }
  
  @_transparent
  public static func -(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) - rhs
  }
  
  @_transparent
  public static func *(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) * rhs
  }
  
  @_transparent
  public static func /(lhs: Scalar, rhs: Self) -> Self {
    return Self(repeating: lhs) / rhs
  }
  
  @_transparent
  public static func +(lhs: Self, rhs: Scalar) -> Self {
    return lhs + Self(repeating: rhs)
  }
  
  @_transparent
  public static func -(lhs: Self, rhs: Scalar) -> Self {
    return lhs - Self(repeating: rhs)
  }
  
  @_transparent
  public static func *(lhs: Self, rhs: Scalar) -> Self {
    return lhs * Self(repeating: rhs)
  }
  
  @_transparent
  public static func /(lhs: Self, rhs: Scalar) -> Self {
    return lhs / Self(repeating: rhs)
  }
  
  @_transparent
  public static func +=(lhs: inout Self, rhs: Self) {
    lhs = lhs + rhs
  }
  
  @_transparent
  public static func -=(lhs: inout Self, rhs: Self) {
    lhs = lhs - rhs
  }
  
  @_transparent
  public static func *=(lhs: inout Self, rhs: Self) {
    lhs = lhs * rhs
  }
  
  @_transparent
  public static func /=(lhs: inout Self, rhs: Self) {
    lhs = lhs / rhs
  }
  
  @_transparent
  public static func +=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs + rhs
  }
  
  @_transparent
  public static func -=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs - rhs
  }
  
  @_transparent
  public static func *=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs * rhs
  }
  
  @_transparent
  public static func /=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs / rhs
  }
  
  @_transparent
  public func addingProduct(_ lhs: Scalar, _ rhs: Self) -> Self {
    return self.addingProduct(Self(repeating: lhs), rhs)
  }
  
  @_transparent
  public func addingProduct(_ lhs: Self, _ rhs: Scalar) -> Self {
    return self.addingProduct(lhs, Self(repeating: rhs))
  }
  
  @_transparent
  public mutating func addProduct(_ lhs: Self, _ rhs: Self) {
    self = self.addingProduct(lhs, rhs)
  }
  
  @_transparent
  public mutating func addProduct(_ lhs: Scalar, _ rhs: Self) {
    self = self.addingProduct(lhs, rhs)
  }
  
  @_transparent
  public mutating func addProduct(_ lhs: Self, _ rhs: Scalar) {
    self = self.addingProduct(lhs, rhs)
  }
  
  @_transparent
  public mutating func formSquareRoot( ) {
    self = self.squareRoot()
  }
  
  @_transparent
  public mutating func round(_ rule: FloatingPointRoundingRule) {
    self = self.rounded(rule)
  }
}

extension SIMDMask {
  @_transparent
  public static func .&(lhs: Bool, rhs: SIMDMask) -> SIMDMask {
    return SIMDMask(repeating: lhs) .& rhs
  }
  
  @_transparent
  public static func .^(lhs: Bool, rhs: SIMDMask) -> SIMDMask {
    return SIMDMask(repeating: lhs) .^ rhs
  }
  
  @_transparent
  public static func .|(lhs: Bool, rhs: SIMDMask) -> SIMDMask {
    return SIMDMask(repeating: lhs) .| rhs
  }
  
  @_transparent
  public static func .&(lhs: SIMDMask, rhs: Bool) -> SIMDMask {
    return lhs .& SIMDMask(repeating: rhs)
  }
  
  @_transparent
  public static func .^(lhs: SIMDMask, rhs: Bool) -> SIMDMask {
    return lhs .^ SIMDMask(repeating: rhs)
  }
  
  @_transparent
  public static func .|(lhs: SIMDMask, rhs: Bool) -> SIMDMask {
    return lhs .| SIMDMask(repeating: rhs)
  }
  
  @_transparent
  public static func .&=(lhs: inout SIMDMask, rhs: SIMDMask) {
    lhs = lhs .& rhs
  }
  
  @_transparent
  public static func .^=(lhs: inout SIMDMask, rhs: SIMDMask) {
    lhs = lhs .^ rhs
  }
  
  @_transparent
  public static func .|=(lhs: inout SIMDMask, rhs: SIMDMask) {
    lhs = lhs .| rhs
  }
  
  @_transparent
  public static func .&=(lhs: inout SIMDMask, rhs: Bool) {
    lhs = lhs .& rhs
  }
  
  @_transparent
  public static func .^=(lhs: inout SIMDMask, rhs: Bool) {
    lhs = lhs .^ rhs
  }
  
  @_transparent
  public static func .|=(lhs: inout SIMDMask, rhs: Bool) {
    lhs = lhs .| rhs
  }
}

/// True if any lane of mask is true.
@_alwaysEmitIntoClient
public func any<Storage>(_ mask: SIMDMask<Storage>) -> Bool {
  return mask._storage.min() < 0
}

/// True if every lane of mask is true.
@_alwaysEmitIntoClient
public func all<Storage>(_ mask: SIMDMask<Storage>) -> Bool {
  return mask._storage.max() < 0
}

/// The lanewise minimum of two vectors.
///
/// Each element of the result is the minimum of the corresponding elements
/// of the inputs.
@_alwaysEmitIntoClient
public func pointwiseMin<T>(_ a: T, _ b: T) -> T
where T: SIMD, T.Scalar: Comparable {
  var result = T()
  for i in result.indices {
    result[i] = min(a[i], b[i])
  }
  return result
}

/// The lanewise maximum of two vectors.
///
/// Each element of the result is the minimum of the corresponding elements
/// of the inputs.
@_alwaysEmitIntoClient
public func pointwiseMax<T>(_ a: T, _ b: T) -> T
where T: SIMD, T.Scalar: Comparable {
  var result = T()
  for i in result.indices {
    result[i] = max(a[i], b[i])
  }
  return result
}


/// The lanewise minimum of two vectors.
///
/// Each element of the result is the minimum of the corresponding elements
/// of the inputs.
@_alwaysEmitIntoClient
public func pointwiseMin<T>(_ a: T, _ b: T) -> T
where T: SIMD, T.Scalar: FloatingPoint {
  var result = T()
  for i in result.indices {
    result[i] = T.Scalar.minimum(a[i], b[i])
  }
  return result
}

/// The lanewise maximum of two vectors.
///
/// Each element of the result is the maximum of the corresponding elements
/// of the inputs.
@_alwaysEmitIntoClient
public func pointwiseMax<T>(_ a: T, _ b: T) -> T
where T: SIMD, T.Scalar: FloatingPoint {
  var result = T()
  for i in result.indices {
    result[i] = T.Scalar.maximum(a[i], b[i])
  }
  return result
}
