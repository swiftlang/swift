infix operator .== : ComparisonPrecedence
infix operator .!= : ComparisonPrecedence
infix operator .< : ComparisonPrecedence
infix operator .<= : ComparisonPrecedence
infix operator .> : ComparisonPrecedence
infix operator .>= : ComparisonPrecedence
infix operator .& : LogicalConjunctionPrecedence
infix operator .^ : LogicalDisjunctionPrecedence
infix operator .| : LogicalDisjunctionPrecedence
infix operator .&= : AssignmentPrecedence
infix operator .^= : AssignmentPrecedence
infix operator .|= : AssignmentPrecedence
prefix operator .!

/// A SIMD vector type that may not have any computational operations.
///
/// This protocol only defines a storage layout and provides elementwise
/// accesses. Computational operations are defined on SIMDVector, which
/// refines this protocol, or on the concrete types that conform.
public protocol SIMDVectorStorage {
  /// The type of scalars in the vector space.
  associatedtype Scalar : Hashable
  
  /// The number of elements in the vector.
  var elementCount: Int { get }
  
  /// A vector with zero in all lanes.
  init()
  
  /// Element access to the vector.
  subscript(index: Int) -> Scalar { get set }
}

public protocol SIMDVector : SIMDVectorStorage, Hashable, CustomStringConvertible {
  /// The mask type resulting from pointwise comparisons of this vector type.
  associatedtype Mask : SIMDMaskVector
}

public protocol SIMDMaskVector : SIMDVector where Scalar == Bool, Mask == Self {
}

public extension SIMDVector {
  /// The valid indices for subscripting the vector.
  @_transparent
  var indices: Range<Int> { return 0 ..< elementCount }
  
  /// A vector with value in all lanes.
  @_transparent
  init(repeating value: Scalar) {
    self.init()
    for i in indices { self[i] = value }
  }
  
  /// Conformance to Equatable
  @_transparent
  static func ==(lhs: Self, rhs: Self) -> Bool {
    var result = true
    for i in lhs.indices { result = result && lhs[i] == rhs[i] }
    return result
  }
  
  /// Conformance to Hashable
  @inlinable
  func hash(into hasher: inout Hasher) {
    for i in indices { hasher.combine(self[i]) }
  }
  
  /// Conformance to CustomStringConvertible
  var description: String {
    get {
      return "\(Self.self)(" + indices.map({"\(self[$0])"}).joined(separator: ", ") + ")"
    }
  }
  
  /// Pointwise equality
  @_transparent
  static func .==(lhs: Self, rhs: Self) -> Mask {
    var result = Mask()
    for i in result.indices { result[i] = lhs[i] == rhs[i] }
    return result
  }
  
  /// Replaces elements of this vector with `other` in the lanes where
  /// `mask` is `true`.
  @_transparent
  mutating func replace(with other: Self, where mask: Mask) {
    for i in indices { self[i] = mask[i] ? other[i] : self[i] }
  }
}

//  Implementations of comparison operations. These should eventually all
//  be replaced with @_semantics to lower directly to vector IR nodes.
public extension SIMDVector where Scalar : Comparable {
  /// Pointwise less than
  @_transparent
  static func .<(lhs: Self, rhs: Self) -> Mask {
    var result = Mask()
    for i in result.indices { result[i] = lhs[i] < rhs[i] }
    return result
  }
  
  /// Pointwise less than or equal to
  @_transparent
  static func .<=(lhs: Self, rhs: Self) -> Mask {
    var result = Mask()
    for i in result.indices { result[i] = lhs[i] <= rhs[i] }
    return result
  }
}

//  Implementations of pointwise boolean operations. These should eventually
//  all be replaced with @_semantics to lower directly to vector IR nodes.
public extension SIMDMaskVector {
  @_transparent
  static func .&(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] && rhs[i] }
    return result
  }
  
  @_transparent
  static func .^(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] != rhs[i] }
    return result
  }
  
  @_transparent
  static func .|(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] || rhs[i] }
    return result
  }
}

//  Implementations of integer operations. These should eventually all
//  be replaced with @_semantics to lower directly to vector IR nodes.
public extension SIMDVector where Scalar : FixedWidthInteger {
  @_transparent
  var leadingZeroBitCount: Self {
    var result = Self()
    for i in indices { result[i] = Scalar(self[i].leadingZeroBitCount) }
    return result
  }
  
  @_transparent
  var trailingZeroBitCount: Self {
    var result = Self()
    for i in indices { result[i] = Scalar(self[i].trailingZeroBitCount) }
    return result
  }
  
  @_transparent
  var nonzeroBitCount: Self {
    var result = Self()
    for i in indices { result[i] = Scalar(self[i].nonzeroBitCount) }
    return result
  }
  
  @_transparent
  static prefix func ~(rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = ~rhs[i] }
    return result
  }
  
  @_transparent
  static func &(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] & rhs[i] }
    return result
  }
  
  @_transparent
  static func ^(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] ^ rhs[i] }
    return result
  }
  
  @_transparent
  static func |(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] | rhs[i] }
    return result
  }
  
  @_transparent
  static func &<<(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &<< rhs[i] }
    return result
  }
  
  @_transparent
  static func &>>(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &>> rhs[i] }
    return result
  }
  
  @_transparent
  static func &+(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &+ rhs[i] }
    return result
  }
  
  @_transparent
  static func &-(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &- rhs[i] }
    return result
  }
  
  @_transparent
  static func &*(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &* rhs[i] }
    return result
  }
  
  @_transparent
  static func /(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] / rhs[i] }
    return result
  }
  
  @_transparent
  static func %(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] % rhs[i] }
    return result
  }
}

//  Implementations of floating-point operations. These should eventually all
//  be replaced with @_semantics to lower directly to vector IR nodes.
public extension SIMDVector where Scalar : FloatingPoint {
  @_transparent
  static func +(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] + rhs[i] }
    return result
  }
  
  @_transparent
  static func -(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] - rhs[i] }
    return result
  }
  
  @_transparent
  static func *(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] * rhs[i] }
    return result
  }
  
  @_transparent
  static func /(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] / rhs[i] }
    return result
  }
  
  @_transparent
  func addingProduct(_ lhs: Self, _ rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = self[i].addingProduct(lhs[i], rhs[i]) }
    return result
  }
  
  @_transparent
  func squareRoot( ) -> Self {
    var result = Self()
    for i in result.indices { result[i] = self[i].squareRoot() }
    return result
  }
  
  @_transparent
  func rounded(_ rule: FloatingPointRoundingRule) -> Self {
    var result = Self()
    for i in result.indices { result[i] = self[i].rounded(rule) }
    return result
  }
}

//  These operations should never need @_semantics; they should be trivial
//  wrappers around the core operations defined above.
public extension SIMDVector {
  @_transparent static func .!=(lhs: Self, rhs: Self) -> Mask { return .!(lhs .== rhs) }
  @_transparent static func .==(lhs: Scalar, rhs: Self) -> Mask { return Self(repeating: lhs) .== rhs }
  @_transparent static func .!=(lhs: Scalar, rhs: Self) -> Mask { return Self(repeating: lhs) .!= rhs }
  @_transparent static func .==(lhs: Self, rhs: Scalar) -> Mask { return lhs .== Self(repeating: rhs) }
  @_transparent static func .!=(lhs: Self, rhs: Scalar) -> Mask { return lhs .!= Self(repeating: rhs) }
  
  @_transparent
  mutating func replace(with other: Scalar, where mask: Mask) {
    replace(with: Self(repeating: other), where: mask)
  }
  
  @_transparent
  func replacing(with other: Self, where mask: Mask) -> Self {
    var result = self
    result.replace(with: other, where: mask)
    return result
  }
  
  @_transparent
  func replacing(with other: Scalar, where mask: Mask) -> Self {
    return replacing(with: Self(repeating: other), where: mask)
  }
}

public extension SIMDMaskVector {
  
  @_transparent static prefix func .!(rhs: Self) -> Self { return false .== rhs }
  
  @_transparent static func .&(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) .& rhs }
  @_transparent static func .^(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) .^ rhs }
  @_transparent static func .|(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) .| rhs }
  
  @_transparent static func .&(lhs: Self, rhs: Scalar) -> Self { return lhs .& Self(repeating: rhs) }
  @_transparent static func .^(lhs: Self, rhs: Scalar) -> Self { return lhs .^ Self(repeating: rhs) }
  @_transparent static func .|(lhs: Self, rhs: Scalar) -> Self { return lhs .| Self(repeating: rhs) }
  
  @_transparent static func .&=(lhs: inout Self, rhs: Self) { lhs = lhs .& rhs }
  @_transparent static func .^=(lhs: inout Self, rhs: Self) { lhs = lhs .^ rhs }
  @_transparent static func .|=(lhs: inout Self, rhs: Self) { lhs = lhs .| rhs }
  
  @_transparent static func .&=(lhs: inout Self, rhs: Scalar) { lhs = lhs .& rhs }
  @_transparent static func .^=(lhs: inout Self, rhs: Scalar) { lhs = lhs .^ rhs }
  @_transparent static func .|=(lhs: inout Self, rhs: Scalar) { lhs = lhs .| rhs }
  
  @inlinable
  static func random<T: RandomNumberGenerator>(using generator: inout T) -> Self {
    var result = Self()
    for i in result.indices { result[i] = Bool.random(using: &generator) }
    return result
  }
  
  @inlinable
  static func random() -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(using: &g)
  }
}

public extension SIMDVector where Scalar : Comparable {
  @_transparent static func .>=(lhs: Self, rhs: Self) -> Mask { return rhs .<= lhs }
  @_transparent static func .>(lhs: Self, rhs: Self) -> Mask { return rhs .< lhs }
  @_transparent static func .<(lhs: Scalar, rhs: Self) -> Mask { return Self(repeating: lhs) .< rhs }
  @_transparent static func .<=(lhs: Scalar, rhs: Self) -> Mask { return Self(repeating: lhs) .<= rhs }
  @_transparent static func .>=(lhs: Scalar, rhs: Self) -> Mask { return Self(repeating: lhs) .>= rhs }
  @_transparent static func .>(lhs: Scalar, rhs: Self) -> Mask { return Self(repeating: lhs) .> rhs }
  @_transparent static func .<(lhs: Self, rhs: Scalar) -> Mask { return lhs .< Self(repeating: rhs) }
  @_transparent static func .<=(lhs: Self, rhs: Scalar) -> Mask { return lhs .<= Self(repeating: rhs) }
  @_transparent static func .>=(lhs: Self, rhs: Scalar) -> Mask { return lhs .>= Self(repeating: rhs) }
  @_transparent static func .>(lhs: Self, rhs: Scalar) -> Mask { return lhs .> Self(repeating: rhs) }
}

public extension SIMDVector where Scalar : FixedWidthInteger {
  @_transparent static var zero: Self { return Self() }
  
  @_transparent static func &(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) & rhs }
  @_transparent static func ^(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) ^ rhs }
  @_transparent static func |(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) | rhs }
  @_transparent static func &<<(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) &<< rhs }
  @_transparent static func &>>(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) &>> rhs }
  @_transparent static func &+(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) &+ rhs }
  @_transparent static func &-(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) &- rhs }
  @_transparent static func &*(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) &* rhs }
  @_transparent static func /(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) / rhs }
  @_transparent static func %(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) % rhs }
  
  @_transparent static func &(lhs: Self, rhs: Scalar) -> Self { return lhs & Self(repeating: rhs) }
  @_transparent static func ^(lhs: Self, rhs: Scalar) -> Self { return lhs ^ Self(repeating: rhs) }
  @_transparent static func |(lhs: Self, rhs: Scalar) -> Self { return lhs | Self(repeating: rhs) }
  @_transparent static func &<<(lhs: Self, rhs: Scalar) -> Self { return lhs &<< Self(repeating: rhs) }
  @_transparent static func &>>(lhs: Self, rhs: Scalar) -> Self { return lhs &>> Self(repeating: rhs) }
  @_transparent static func &+(lhs: Self, rhs: Scalar) -> Self { return lhs &+ Self(repeating: rhs) }
  @_transparent static func &-(lhs: Self, rhs: Scalar) -> Self { return lhs &- Self(repeating: rhs) }
  @_transparent static func &*(lhs: Self, rhs: Scalar) -> Self { return lhs &* Self(repeating: rhs) }
  @_transparent static func /(lhs: Self, rhs: Scalar) -> Self { return lhs / Self(repeating: rhs) }
  @_transparent static func %(lhs: Self, rhs: Scalar) -> Self { return lhs % Self(repeating: rhs) }
  
  @_transparent static func &=(lhs: inout Self, rhs: Self) { lhs = lhs & rhs }
  @_transparent static func ^=(lhs: inout Self, rhs: Self) { lhs = lhs ^ rhs }
  @_transparent static func |=(lhs: inout Self, rhs: Self) { lhs = lhs | rhs }
  @_transparent static func &<<=(lhs: inout Self, rhs: Self) { lhs = lhs &<< rhs }
  @_transparent static func &>>=(lhs: inout Self, rhs: Self) { lhs = lhs &>> rhs }
  @_transparent static func &+=(lhs: inout Self, rhs: Self) { lhs = lhs &+ rhs }
  @_transparent static func &-=(lhs: inout Self, rhs: Self) { lhs = lhs &- rhs }
  @_transparent static func &*=(lhs: inout Self, rhs: Self) { lhs = lhs &* rhs }
  @_transparent static func /=(lhs: inout Self, rhs: Self) { lhs = lhs / rhs }
  @_transparent static func %=(lhs: inout Self, rhs: Self) { lhs = lhs % rhs }
  
  @_transparent static func &=(lhs: inout Self, rhs: Scalar) { lhs = lhs & rhs }
  @_transparent static func ^=(lhs: inout Self, rhs: Scalar) { lhs = lhs ^ rhs }
  @_transparent static func |=(lhs: inout Self, rhs: Scalar) { lhs = lhs | rhs }
  @_transparent static func &<<=(lhs: inout Self, rhs: Scalar) { lhs = lhs &<< rhs }
  @_transparent static func &>>=(lhs: inout Self, rhs: Scalar) { lhs = lhs &>> rhs }
  @_transparent static func &+=(lhs: inout Self, rhs: Scalar) { lhs = lhs &+ rhs }
  @_transparent static func &-=(lhs: inout Self, rhs: Scalar) { lhs = lhs &- rhs }
  @_transparent static func &*=(lhs: inout Self, rhs: Scalar) { lhs = lhs &* rhs }
  @_transparent static func /=(lhs: inout Self, rhs: Scalar) { lhs = lhs / rhs }
  @_transparent static func %=(lhs: inout Self, rhs: Scalar) { lhs = lhs % rhs }
  
  @inlinable
  static func random<T: RandomNumberGenerator>(
    in range: Range<Scalar>,
    using generator: inout T
  ) -> Self {
    var result = Self()
    for i in result.indices {
      result[i] = Scalar.random(in: range, using: &generator)
    }
    return result
  }
  
  @inlinable
  static func random(in range: Range<Scalar>) -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(in: range, using: &g)
  }
  
  @inlinable
  static func random<T: RandomNumberGenerator>(
    in range: ClosedRange<Scalar>,
    using generator: inout T
  ) -> Self {
    var result = Self()
    for i in result.indices {
      result[i] = Scalar.random(in: range, using: &generator)
    }
    return result
  }
  
  @inlinable
  static func random(in range: ClosedRange<Scalar>) -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(in: range, using: &g)
  }
}

public extension SIMDVector where Scalar : FloatingPoint {
  @_transparent static var zero: Self { return Self() }
  
  @_transparent static func +(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) + rhs }
  @_transparent static func -(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) - rhs }
  @_transparent static func *(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) * rhs }
  @_transparent static func /(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) / rhs }
  
  @_transparent static func +(lhs: Self, rhs: Scalar) -> Self { return lhs + Self(repeating: rhs) }
  @_transparent static func -(lhs: Self, rhs: Scalar) -> Self { return lhs - Self(repeating: rhs) }
  @_transparent static func *(lhs: Self, rhs: Scalar) -> Self { return lhs * Self(repeating: rhs) }
  @_transparent static func /(lhs: Self, rhs: Scalar) -> Self { return lhs / Self(repeating: rhs) }
  
  @_transparent static func +=(lhs: inout Self, rhs: Self) { lhs = lhs + rhs }
  @_transparent static func -=(lhs: inout Self, rhs: Self) { lhs = lhs - rhs }
  @_transparent static func *=(lhs: inout Self, rhs: Self) { lhs = lhs * rhs }
  @_transparent static func /=(lhs: inout Self, rhs: Self) { lhs = lhs / rhs }
  
  @_transparent static func +=(lhs: inout Self, rhs: Scalar) { lhs = lhs + rhs }
  @_transparent static func -=(lhs: inout Self, rhs: Scalar) { lhs = lhs - rhs }
  @_transparent static func *=(lhs: inout Self, rhs: Scalar) { lhs = lhs * rhs }
  @_transparent static func /=(lhs: inout Self, rhs: Scalar) { lhs = lhs / rhs }
  
  @_transparent func addingProduct(_ lhs: Scalar, _ rhs: Self) -> Self {
    return self.addingProduct(Self(repeating: lhs), rhs)
  }
  @_transparent func addingProduct(_ lhs: Self, _ rhs: Scalar) -> Self {
    return self.addingProduct(lhs, Self(repeating: rhs))
  }
  @_transparent mutating func addProduct(_ lhs: Self, _ rhs: Self) {
    self = self.addingProduct(lhs, rhs)
  }
  @_transparent mutating func addProduct(_ lhs: Scalar, _ rhs: Self) {
    self = self.addingProduct(lhs, rhs)
  }
  @_transparent mutating func addProduct(_ lhs: Self, _ rhs: Scalar) {
    self = self.addingProduct(lhs, rhs)
  }
  
  @_transparent mutating func formSquareRoot( ) {
    self = self.squareRoot()
  }
  
  @_transparent mutating func round(_ rule: FloatingPointRoundingRule) {
    self = self.rounded(rule)
  }
}

public extension SIMDVector
where Scalar : BinaryFloatingPoint, Scalar.RawSignificand : FixedWidthInteger {
  @inlinable
  static func random<T: RandomNumberGenerator>(
    in range: Range<Scalar>,
    using generator: inout T
  ) -> Self {
    var result = Self()
    for i in result.indices {
      result[i] = Scalar.random(in: range, using: &generator)
    }
    return result
  }
  
  @inlinable
  static func random(in range: Range<Scalar>) -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(in: range, using: &g)
  }
  
  @inlinable
  static func random<T: RandomNumberGenerator>(
    in range: ClosedRange<Scalar>,
    using generator: inout T
  ) -> Self {
    var result = Self()
    for i in result.indices {
      result[i] = Scalar.random(in: range, using: &generator)
    }
    return result
  }
  
  @inlinable
  static func random(in range: ClosedRange<Scalar>) -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(in: range, using: &g)
  }
}

public protocol SIMDVectorizable {
  associatedtype _MaskElement : SIMDVectorizable & FixedWidthInteger & SignedInteger
  associatedtype _Vector2 : SIMDVectorStorage where _Vector2.Scalar == Self
  associatedtype _Vector4 : SIMDVectorStorage where _Vector4.Scalar == Self
  associatedtype _Vector8 : SIMDVectorStorage where _Vector8.Scalar == Self
  associatedtype _Vector16 : SIMDVectorStorage where _Vector16.Scalar == Self
  associatedtype _Vector32 : SIMDVectorStorage where _Vector32.Scalar == Self
  associatedtype _Vector64 : SIMDVectorStorage where _Vector64.Scalar == Self
}

@_fixed_layout
public struct SIMDMask<Storage> : SIMDMaskVector
  where Storage : SIMDVector,
        Storage.Scalar : FixedWidthInteger & SignedInteger {
  
  public var _storage : Storage
  
  public typealias Mask = SIMDMask<Storage>
  
  public typealias Scalar = Bool
  
  @_transparent
  public var elementCount: Int {
    return _storage.elementCount
  }
  
  @_transparent
  public init() {
    _storage = Storage()
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
