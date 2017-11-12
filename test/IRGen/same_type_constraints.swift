// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s -disable-objc-attr-requires-foundation-module | %FileCheck %s
// RUN: %target-swift-frontend -Osize -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s -disable-objc-attr-requires-foundation-module | %FileCheck %s --check-prefix=OSIZE

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

// CHECK: define{{( protected)?}} swiftcc void @_T021same_type_constraints1PPA2A10DefaultFooVyxG0E0RtzrlE3fooAFyF

// <rdar://26873036> IRGen crash with derived class declaring same-type constraint on constrained associatedtype.
public class C1<T: Equatable> { }
public class C2<T: Equatable, U: P>: C1<T> where T == U.Foo {}

// CHECK: define{{( protected)?}} swiftcc void @_T021same_type_constraints2C1CfD

public protocol MyHashable {}
public protocol DataType : MyHashable {}

public protocol E {
  associatedtype Data: DataType
}

struct Dict<V : MyHashable, K> {}
struct Val {}

public class GenericKlazz<T: DataType, R: E> : E where R.Data == T
{
  public typealias Data = T

  var d: Dict<T, Val>
  init() {
     d = Dict()
  }
}

// This used to hit an infinite loop - <rdar://problem/27018457>
public protocol CodingType {
    associatedtype ValueType
}

public protocol ValueCoding {
    associatedtype Coder: CodingType
}

func foo<Self>(s: Self)
where Self : CodingType,
      Self.ValueType: ValueCoding,
      Self.ValueType.Coder == Self {
  print(Self.ValueType.self)
}

// OSIZE: define internal i8** @_T021same_type_constraints12GenericKlazzCyxq_GAA1EAA4Data_AA0F4TypePWT(%swift.type* %"GenericKlazz<T, R>.Data", %swift.type* nocapture readonly %"GenericKlazz<T, R>", i8** nocapture readnone %"GenericKlazz<T, R>.E") [[ATTRS:#[0-9]+]] {
// OSIZE: [[ATTRS]] = {{{.*}}noinline
