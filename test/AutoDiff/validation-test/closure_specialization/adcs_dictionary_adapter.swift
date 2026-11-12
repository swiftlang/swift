// REQUIRES: executable_test
// RUN: %target-build-swift %s -Onone -o %t.none
// RUN: %target-run %t.none
// RUN: %target-build-swift %s -O -o %t.opt
// RUN: %target-run %t.opt

import _Differentiation

extension Dictionary: Differentiable where Value: Differentiable {
  public typealias TangentVector = [Key: Value.TangentVector]
  public mutating func move(by direction: TangentVector) {
    for (key, component) in direction { self[key]!.move(by: component) }
  }
}

extension Dictionary: AdditiveArithmetic where Value: AdditiveArithmetic {
  public static var zero: Self { [:] }
  public static func + (_ lhs: Self, _ rhs: Self) -> Self {
    lhs.merging(rhs, uniquingKeysWith: +)
  }
  public static func - (_ lhs: Self, _ rhs: Self) -> Self {
    lhs.merging(rhs.mapValues { .zero - $0 }, uniquingKeysWith: +)
  }
}

extension Dictionary where Value: Differentiable {
  @usableFromInline
  @derivative(of: subscript(_:))
  func vjpSubscriptGet(key: Key)
    -> (value: Value?, pullback: (Optional<Value>.TangentVector) -> TangentVector) {
    (self[key], { tangent in
      if let value = tangent.value { return [key: value] }
      return .zero
    })
  }
}

// Generic specialization adapts the indirect dictionary pullback with an
// ordinary ABI thunk. ADCS preserves that adapter's captured callback rather
// than assuming every thunk is a transparent reabstraction.
@differentiable(reverse)
public func lookup(_ values: [String: Double], _ key: String) -> Double? {
  if values.keys.contains(key) { return values[key] }
  return nil
}

@differentiable(reverse)
public func lookupFirst(_ values: [String: Double]) -> Double {
  lookup(values, "first")!
}

#if !DEFINITIONS_ONLY
let result = valueWithPullback(at: ["first": 3.0], of: lookupFirst)
precondition(result.value == 3)
precondition(result.pullback(2) == ["first": 2])
precondition(result.pullback(5) == ["first": 5])
#endif
