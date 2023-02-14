// RUN: %target-swift-emit-silgen %s

public class C<Key, Value> {
  public func method() -> Value { fatalError() }
}

public func foo<T>(_: () -> T) {}

public func bar() {
  foo { C<Int, Int>().method() }
}
REQUIRES: updating_for_owned_noescape
