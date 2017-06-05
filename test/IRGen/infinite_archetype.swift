// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=i386_or_x86_64

protocol Fooable {
  associatedtype Foo
}

// CHECK: define hidden swiftcc void @_T018infinite_archetype3foo{{[_0-9a-zA-Z]*}}F(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.Fooable)
func foo<T: Fooable where T == T.Foo>(x: T) -> T { return x }
