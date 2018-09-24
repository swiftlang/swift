/// A computational vector type.
public protocol SIMDVector : Hashable,
                             CustomStringConvertible,
                             ExpressibleByArrayLiteral {
  
  associatedtype Element : Hashable
  
  /// A vector with zero in all lanes.
  init()
  
  /// A vector with value in all lanes.
  ///
  /// A default implementation is provided by SIMDVectorN.
  init(repeating value: Element)
  
  /// A vector constructed from the contents of `array`.
  ///
  /// `array` must have the correct number of elements for the vector type.
  /// If it does not, a runtime error occurs.
  ///
  /// A default implementation is provided by SIMDVectorN.
  init(fromArray array: [Element])
  
  /// The number of elements in the vector.
  var count: Int { get }
  
  /// Element access to the vector.
  ///
  /// Precondition: `index` must be in `0 ..< count`.
  subscript(index: Int) -> Element { get set }
  
  /// A type representing the result of lanewise comparison.
  ///
  /// Most SIMD comparison operators are *lanewise*, meaning that a comparison
  /// of two 4-element vectors produces a vector of 4 comparison results. E.g:
  ///
  ///   let vec = Float.Vector4( 1, 2, 3, 4)
  ///   let mask = vec < 3
  ///   // mask = Int32.Vector4(-1,-1, 0, 0), because the condition `< 3` is
  ///   // true in the first two lanes and false in the second two lanes.
  ///
  /// This vector of comparison results is itself a vector with the same
  /// number of elements as the vectors being compared.
  associatedtype Mask : SIMDMask
  
  /// Elementwise equality test.
  ///
  /// The result is a mask vector where each lane is `true` if and only
  /// if the corresponding lane of the vector `lhs` is equal to `rhs`.
  ///
  /// There are two `==` and `!=` operators defined on SIMD vectors; the
  /// "normal" equality operator required by `Equatable`, which returns
  /// `Bool`, and this operator which returns a vector of `Bool`, called
  /// `Mask`.
  // Note: the corresponding `!=` operator and the `Bool` operators are defined
  // in terms of this operation in extensions. SIMD types should only define
  // this operator, and use the default implementations of the others.
  static func ==(lhs: Self, rhs: Self) -> Mask
  
  /// A vector formed from the corresponding lane of this vector where
  /// prediate is false, and from the corresponding lane of other where
  /// mask is true.
  ///
  /// See Also: replacing(with: Element, where: Mask), and the in-place
  /// operations replace(with:,where:).
  func replacing(with other: Self, where mask: Mask) -> Self
}

//  Non-customizable operations on SIMDVector
public extension SIMDVector {
  
  /// A mask vector where each lane is `true` if and only if the
  /// corresponding lane of the vector `rhs` is equal to `lhs`.
  @_transparent
  static func ==(lhs: Element, rhs: Self) -> Mask {
    return Self(repeating: lhs) == rhs
  }
  
  /// A mask vector where each lane is `true` if and only if the
  /// corresponding lane of the vector `lhs` is equal to `rhs`.
  @_transparent
  static func ==(lhs: Self, rhs: Element) -> Mask {
    return rhs == lhs
  }
  
  /// A mask vector where each lane is `true` if and only if the
  /// corresponding lanes of the two arguments are not equal.
  @_transparent
  static func !=(lhs: Self, rhs: Self) -> Mask {
    return !(lhs == rhs)
  }
  
  /// A mask vector where each lane is `true` if and only if the
  /// corresponding lane of the vector `rhs` is not equal to `lhs`.
  @_transparent
  static func !=(lhs: Element, rhs: Self) -> Mask {
    return !(lhs == rhs)
  }
  
  /// A mask vector where each lane is `true` if and only if the
  /// corresponding lane of the vector `lhs` is not equal to `rhs`.
  @_transparent
  static func !=(lhs: Self, rhs: Element) -> Mask {
    return !(lhs == rhs)
  }
  
  /// Replaces elements of this vector with elements of `other` in the lanes
  /// where `mask` is `true`.
  @inlinable
  mutating func replace(with other: Self, where mask: Mask) {
    self = self.replacing(with: other, where: mask)
  }
  
  /// A vector formed by making a copy of this vector, and replacing lanes
  /// where `mask` is true with `other`.
  @inlinable
  func replacing(with other: Element, where mask: Mask) -> Self {
    return self.replacing(with: Self(repeating: other), where: mask)
  }
  
  /// Replaces elements of this vector with `other` in the lanes where
  /// `mask` is `true`.
  @inlinable
  mutating func replace(with other: Element, where mask: Mask) {
    self = self.replacing(with: Self(repeating: other), where: mask)
  }
}

//  Defaulted conformance to Equatable.
public extension SIMDVector {
  @_transparent
  static func ==(lhs: Self, rhs: Self) -> Bool {
    return all(lhs == rhs)
  }
}

//  Defaulted conformance to Hashable.
public extension SIMDVector {
  //  We don't get an implementation automatically created for us because these
  //  structs wrap Builtin.Vectors, which are not themselves hashable.
  func hash(into hasher: inout Hasher) {
    for i in 0 ..< count {
      hasher.combine(self[i])
    }
  }
}

//  Defaulted conformance to CustomStringConvertible
public extension SIMDVector {
  @inlinable
  var description: String {
    get {
      return "\(Self.self)(" + (0 ..< count).map({
        "\(self[$0])"
      }).joined(separator: ", ") + ")"
    }
  }
}

