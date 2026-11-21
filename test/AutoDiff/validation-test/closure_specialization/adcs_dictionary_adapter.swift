// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_dictionary_adapter %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

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

// Generic specialization adds an ABI thunk that captures the dictionary pullback.
@differentiable(reverse)
public func lookup(_ values: [String: Double], _ key: String) -> Double? {
  if values.keys.contains(key) { return values[key] }
  return nil
}

// CHECK:       enum [[TAPE:_AD__[^ ]+lookup[^ ]+_spec_bb1_1[^ ]+]] {
// CHECK-NOT:   }
// CHECK:         case bb1((predecessor: {{[^,]+}}, (_: @callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>)))

// CHECK-LABEL: {{^}}// reverse-mode derivative of lookup(_:_:)
// CHECK:         // function_ref specialized pullback of lookup(_:_:)
// CHECK-NEXT:    %[[#PB:]] = function_ref @[[PULLBACK:[^ ]+]] : $@convention(thin) (Optional<Double>.TangentVector, @owned [[TAPE]]) -> @owned Dictionary<String, Double>
// CHECK-NEXT:    %[[#]] = partial_apply [callee_guaranteed] %[[#PB]]
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of lookup(_:_:)
// CHECK:       sil private @[[PULLBACK]]
// CHECK:         %[[#CAPTURES:]] = tuple_extract %[[#]], 1
// CHECK:         %[[#CALLBACK:]] = tuple_extract %[[#CAPTURES]], 0
// CHECK:         apply %[[#CALLBACK]]({{.*}}) : $@callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>
// CHECK:       } // end sil function

@differentiable(reverse)
public func lookupFirst(_ values: [String: Double]) -> Double {
  lookup(values, "first")!
}

let result = valueWithPullback(at: ["first": 3.0], of: lookupFirst)
precondition(result.value == 3)
precondition(result.pullback(2) == ["first": 2])
precondition(result.pullback(5) == ["first": 5])
