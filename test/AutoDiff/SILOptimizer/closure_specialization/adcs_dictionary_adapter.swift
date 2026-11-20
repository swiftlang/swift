// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name dictionary_adapter %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// CHECK:       enum _AD__{{.*}}lookup{{.*}}_spec_bb1_1{{.*}} {
// CHECK:       case bb1((predecessor: {{.*}}, (_: @callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>)))
// CHECK-LABEL: {{^}}// reverse-mode derivative of lookup(_:_:)
// CHECK:         // function_ref specialized pullback of lookup(_:_:)
// CHECK-LABEL: {{^}}// specialized pullback of lookup(_:_:)
// CHECK:         %[[#CAPTURES:]] = tuple_extract %[[#]], 1
// CHECK:         %[[#CALLBACK:]] = tuple_extract %[[#CAPTURES]], 0
// CHECK:         apply %[[#CALLBACK]]({{.*}}) : $@callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>

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

@inline(never)
private func hasEntries(_ values: [String: Double]) -> Bool {
  !values.isEmpty
}

@inline(never)
private func hasKey(_ values: [String: Double], _ key: String) -> Bool {
  values.keys.contains(key)
}

// Inlining merges the two false paths, which forward an empty tape payload.
@inline(__always)
private func containsKey(_ values: [String: Double], _ key: String) -> Bool {
  guard hasEntries(values) else { return false }
  return hasKey(values, key)
}

// ADCS must keep the callback captured by the ABI thunk from generic specialization.
@differentiable(reverse)
public func lookup(_ values: [String: Double], _ key: String) -> Double? {
  if containsKey(values, key) { return values[key] }
  return nil
}

@differentiable(reverse)
public func lookupFirst(_ values: [String: Double]) -> Double {
  lookup(values, "first")!
}
