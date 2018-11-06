infix operator .== : ComparisonPrecedence
infix operator .!= : ComparisonPrecedence
infix operator .< : ComparisonPrecedence
infix operator .<= : ComparisonPrecedence
infix operator .> : ComparisonPrecedence
infix operator .>= : ComparisonPrecedence
infix operator .& : LogicalConjunctionPrecedence
infix operator .^ : LogicalDisjunctionPrecedence
infix operator .| : LogicalDisjunctionPrecedence
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
  ///
  /// Precondition: `index` must be in `0 ..< elementCount`.
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

//  These operations should never need @_semantics; they should be trivial
//  wrappers around the core operations defined above.
public extension SIMDVector {
  @_transparent static func .!=(lhs: Self, rhs: Self) -> Mask { return .!(lhs .== rhs) }
  @_transparent static func .==(lhs: Scalar, rhs: Self) -> Mask { return Self(repeating: lhs) .== rhs }
  @_transparent static func .!=(lhs: Scalar, rhs: Self) -> Mask { return Self(repeating: lhs) .!= rhs }
  @_transparent static func .==(lhs: Self, rhs: Scalar) -> Mask { return lhs .== Self(repeating: rhs) }
  @_transparent static func .!=(lhs: Self, rhs: Scalar) -> Mask { return lhs .!= Self(repeating: rhs) }
}

public extension SIMDMaskVector {
  @_transparent static prefix func .!(rhs: Self) -> Self { return false .== rhs }
  @_transparent static func .&(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) .& rhs }
  @_transparent static func .^(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) .^ rhs }
  @_transparent static func .|(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) .| rhs }
  @_transparent static func .&(lhs: Self, rhs: Scalar) -> Self { return lhs .& Self(repeating: rhs) }
  @_transparent static func .^(lhs: Self, rhs: Scalar) -> Self { return lhs .^ Self(repeating: rhs) }
  @_transparent static func .|(lhs: Self, rhs: Scalar) -> Self { return lhs .| Self(repeating: rhs) }
  @_transparent static func .&(lhs: inout Self, rhs: Self) { lhs = lhs .& rhs }
  @_transparent static func .^(lhs: inout Self, rhs: Self) { lhs = lhs .^ rhs }
  @_transparent static func .|(lhs: inout Self, rhs: Self) { lhs = lhs .| rhs }
  @_transparent static func .&(lhs: inout Self, rhs: Scalar) { lhs = lhs .& rhs }
  @_transparent static func .^(lhs: inout Self, rhs: Scalar) { lhs = lhs .^ rhs }
  @_transparent static func .|(lhs: inout Self, rhs: Scalar) { lhs = lhs .| rhs }
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
public struct SIMDMask<Storage> : SIMDMaskVector where Storage : SIMDVector, Storage.Scalar : FixedWidthInteger & SignedInteger {
  public var _storage : Storage
  public typealias Mask = SIMDMask<Storage>
  public typealias Scalar = Bool
  @_transparent public var elementCount: Int { return _storage.elementCount }
  @_transparent public init() { _storage = Storage() }
  public subscript(index: Int) -> Bool {
    @inlinable get {
      _precondition(indices.contains(index))
      return _storage[index] < 0
    }
    @inlinable set {
      _precondition(indices.contains(index))
      _storage[index] = newValue ? -1 : 0
    }
  }
}
