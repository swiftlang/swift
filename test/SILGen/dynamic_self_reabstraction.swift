// RUN: %target-swift-emit-silgen %s -verify

func foo<T>(_ f: () -> T) -> T { return f() }

class Foo<U> {
  func bar() -> Self {
    return foo { self }
  }
}
REQUIRES: updating_for_owned_noescape
