// RUN: %swift -target x86_64-apple-macosx10.9 -primary-file %s -emit-ir | FileCheck %s
// XFAIL: linux

protocol Fooable {
  typealias Foo
}

// CHECK: define hidden void @_TF18infinite_archetype3foo{{.*}}(%swift.opaque* noalias sret, %swift.opaque* noalias, %swift.type* %T, i8** %T.Fooable)
func foo<T: Fooable where T == T.Foo>(x: T) -> T { return x }
