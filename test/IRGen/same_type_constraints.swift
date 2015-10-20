// RUN: %target-swift-frontend -emit-ir -primary-file %s -disable-objc-attr-requires-foundation-module | FileCheck %s

// <rdar://problem/21665983> IRGen crash with protocol extension involving same-type constraint to X<T>
public struct DefaultFoo<T> {
  var t: T?
}

public protocol P {
  typealias Foo
}

public extension P where Foo == DefaultFoo<Self> {
  public func foo() -> DefaultFoo<Self> {
    return DefaultFoo()
  }
}

// CHECK: define void @_TFe21same_type_constraintsRq_S_1Pzqq_S0_3FooGVS_10DefaultFooq___S0_3foouRq_S0_zqq_S0_3FooGS1_q___fq_FT_GS1_q__
