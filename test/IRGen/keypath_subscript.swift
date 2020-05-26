// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// rdar://problem/46632723
public struct Foo<T>: Hashable { }

public struct Bar<U, V> {
  public subscript<W> (foo: W) -> Int {
    return 0
  }

  // CHECK-LABEL: define {{.*}} @"$s17keypath_subscript3FooVyqd__Gr0__lTh"
  // CHECK: call swiftcc %swift.metadata_response @"$s17keypath_subscript3FooVMa"
  public func blah<W>(_: W) -> AnyKeyPath {
    return \Bar<U, V>.[Foo<W>()] as AnyKeyPath
  }
}

