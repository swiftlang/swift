// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

infix operator +++ : AdditionPrecedence
protocol P {
  associatedtype T
  static func +++(lhs: Self, rhs: Self) -> Self
}

extension P {
  static func +++(lhs: Self, rhs: Self) -> Self {
    print("<default>")
    return lhs
  }
}

extension P where T == Int {
  static func +++(lhs: Self, rhs: Self) -> Self {
    print("<int>")
    return rhs
  }
}

struct A<U> : P {
  typealias T = U
}

struct B : P {
  typealias T = Int
}

var a = A<Int>()
// CHECK: apply %21<A<Int>>(%8, %14, %19, %10) : $@convention(method) <τ_0_0 where τ_0_0 : P, τ_0_0.T == Int> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> @out τ_0_0
let _ = a +++ a

var b = B()
// CHECK: apply %46<B>(%33, %39, %44, %35) : $@convention(method) <τ_0_0 where τ_0_0 : P, τ_0_0.T == Int> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> @out τ_0_0
let _ = b +++ b
