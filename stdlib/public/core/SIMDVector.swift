/// A computational vector type.
public protocol SIMDVector : MutableCollection,
                             CustomStringConvertible,
                             ExpressibleByArrayLiteral,
                             Equatable
                       where Index == Int {
  
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
  associatedtype Predicate : SIMDPredicate
  
  static func ==(lhs: Self, rhs: Self) -> Predicate
  
  // func replacing(with other: Self, where predicate: Predicate) -> Self
}

//  Non-customizable operations on SIMDVector
public extension SIMDVector {

  @_transparent
  static func !=(lhs: Self, rhs: Self) -> Predicate {
    return !(lhs == rhs)
  }
  /*
  @inlinable
  mutating func replace(with other: Self, where predicate: Predicate) {
    self = self.replacing(with: other, where: predicate)
  }
  */
}

//  Defaulted conformance to Collection. endIndex is defined on SIMDVectorN.
public extension SIMDVector {
  @inlinable
  var startIndex: Int {
    return 0
  }
  
  @inlinable
  func index(after i: Int) -> Int {
    return i + 1
  }
}

//  Defaulted conformance to Equatable.
public extension SIMDVector {
  @_transparent
  static func ==(lhs: Self, rhs: Self) -> Bool {
    return all(lhs == rhs)
  }
}

//  Defaulted conformance to CustomStringConvertible
public extension SIMDVector {
  @inlinable
  var description: String {
    get {
      return "\(Self.self)(" + self.map({"\($0)"}).joined(separator: ", ") + ")"
    }
  }
}

