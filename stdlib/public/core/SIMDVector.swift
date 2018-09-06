/*
public protocol SIMDVector : MutableCollection,
                             ExpressibleByArrayLiteral,
                             CustomStringConvertible,
                             Equatable
                       where Index == Int {
  
  /// A vector with zero in all lanes.
  init()
  
  /// A vector with value in all lanes.
  init(repeating value: Element)
  
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
  
  static func !=(lhs: Self, rhs: Self) -> Predicate
  
  func replacing(with other: Self, where predicate: Predicate) -> Self
}

public extension SIMDVector {
  
  @_transparent
  var startIndex: Int { return 0 }
  
  @_transparent
  func index(after i: Int) -> Int { return i + 1 }
  
  @_transparent
  static func !=(lhs: Self, rhs: Self) -> Predicate { return !(lhs == rhs) }
  
  @_transparent
  mutating func replace(with other: Self, where predicate: Predicate) {
    self = self.replacing(with: other, where: predicate)
  }
  
  @_transparent
  var description: String {
    get {
      return "\(Self.self)(" + self.map({"\($0)"}).joined(separator: ", ") + ")"
    }
  }
  
  @_transparent
  static func ==(lhs: Self, rhs: Self) -> Bool {
    return all(lhs == rhs)
  }
}
*/
