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

// CHECK: define void @_TFe21same_type_constraintsR_S_1Pw_3FoozGVS_10DefaultFooq__rS0_3foouR_S0_w_S1_zGS2_q__rfq_FT_GS2_q__
