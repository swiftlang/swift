// RUN: %target-swift-emit-silgen %s

// https://github.com/apple/swift/issues/54938

class Foo {
  init(f: @escaping () -> String = String.init) {
  }
}
