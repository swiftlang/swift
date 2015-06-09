// RUN: %target-swift-frontend -primary-file %s -emit-ir | FileCheck %s

// REQUIRES: CPU=i386_or_x86_64

protocol Fooable {
  typealias Foo
}

// CHECK: define hidden void @_TF18infinite_archetype3foo{{.*}}(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.Fooable)
func foo<T: Fooable where T == T.Foo>(x: T) -> T { return x }
