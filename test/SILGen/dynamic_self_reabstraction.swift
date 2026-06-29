// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen %s -verify

func foo<T>(_ f: () -> T) -> T { return f() }

class Foo<U> {
  func bar() -> Self {
    return foo { self }
  }
}
