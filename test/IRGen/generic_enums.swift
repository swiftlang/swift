// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir

struct Bar<A1, A2> {
  var a: A1
  var b: Foo<A1>
}

enum Foo<A> {}
