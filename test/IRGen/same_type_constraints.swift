// RUN: %target-swift-frontend -emit-ir -primary-file %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

// <rdar://problem/21665983> IRGen crash with protocol extension involving same-type constraint to X<T>
public struct DefaultFoo<T> {
  var t: T?
}

public protocol P {
  associatedtype Foo
}

public extension P where Foo == DefaultFoo<Self> {
  public func foo() -> DefaultFoo<Self> {
    return DefaultFoo()
  }
}

// CHECK: define{{( protected)?}} void @_TFe21same_type_constraintsRxS_1Pwx3FoozGVS_10DefaultFoox_rS0_3foofT_GS2_x_

// <rdar://26873036> IRGen crash with derived class declaring same-type constraint on constrained associatedtype.
public class C1<T: Equatable> { }
public class C2<T: Equatable, U: P where T == U.Foo>: C1<T> {}

// CHECK: define{{( protected)?}} void @_TFC21same_type_constraints2C1D

public protocol DataType {}

public protocol E {
  associatedtype Data: DataType
}

public class GenericKlazz<T: DataType, R: E> : E where R.Data == T
{
  public typealias Data = T
}
