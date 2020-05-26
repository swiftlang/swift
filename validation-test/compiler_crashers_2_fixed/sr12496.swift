// RUN: %target-swift-emit-silgen %s

class Foo {
  init(f: @escaping () -> String = String.init) {
  }
}
