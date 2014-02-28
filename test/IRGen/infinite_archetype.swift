// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir | FileCheck %s

protocol Fooable {
  typealias Foo
}

// CHECK: define void @_TF18infinite_archetype3fooUS_7Fooable__FT1xQ__Q_(%swift.opaque* noalias sret, %swift.opaque* noalias, %swift.type* %T, i8** %T.Fooable)
func foo<T: Fooable where T == T.Foo>(x: T) -> T { return x }
